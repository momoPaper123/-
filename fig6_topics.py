# -*- coding: utf-8 -*-
"""
第 5 步：为 Figure 6（我们聊了什么：话题的版图）提取特征，输出汇总 CSV 给 R 绘图

方法：
  6a/6b 话题：用话题词典给每条消息打标签（一条消息可同时属于多个话题），按月、按小时汇总
  6c 足迹：jieba 词性标注提取地名（ns），按同一条消息的上下文粗分为“想去”“去过”“其他”
  6d 约定：匹配“下次/以后/改天……一起去/吃/看……”这类句子，提取约定对象；
         在之后 365 天内，若出现同时提到该对象且带有“吃了/去过/好吃/打卡”等完成表达的消息，视为兑现
  6e 吃货档案：食物词典计数，以及“吃什么”这个问题每月被问了多少次
  以上均为词典与规则的粗略统计，适合看整体规律，个别消息可能误判

输入：data/messages_clean.csv
输出（都在 data 文件夹，文件名以 fig6_ 开头）：
  fig6_topic_monthly.csv      每月各话题的命中消息数（6a）
  fig6_topic_month_total.csv  每月文字消息总数、命中任一话题的消息数（6a 覆盖率）
  fig6_topic_hour.csv         各话题在 24 小时的命中消息数（6b）
  fig6_topic_sender.csv       各话题双方的命中消息数
  fig6_topic_keywords.csv     各话题里每个关键词的命中次数（供你检查、调整话题词典）
  fig6_places.csv             地名排行：双方次数、“想去/去过/其他”次数、首次提到日期（6c）
  fig6_location_share.csv     聊天中发送过的位置（微信“位置”消息）
  fig6_promises.csv           “下次一起……”约定明细与是否兑现（6d，含原句，仅供你本人核对，不要外传）
  fig6_food_top.csv           食物排行（6e）
  fig6_food_monthly.csv       各食物每月被提到的消息数
  fig6_whateat_monthly.csv    “吃什么”每月被问的次数（6e）
  fig6_summary.txt            摘要（不含聊天正文，可发给 Claude 核对）

运行：python -X utf8 fig6_topics.py
"""
import bisect
import csv
import os
import re
import time
from collections import Counter, defaultdict
from datetime import date

import jieba
import jieba.posseg as pseg

# ============ 路径 ============
BASE = r"C:\Users\Administrator\Desktop\LP可视化"
DATA = os.path.join(BASE, "data")

# ============ 可自行修改的词典与参数 ============
# 话题词典：一条消息包含某话题任一关键词即计入该话题（运行后看 fig6_topic_keywords.csv 调整）
TOPICS = {
    "吃饭美食": ["吃饭", "饿了", "好饿", "外卖", "好吃", "餐厅", "饭店", "做饭", "做菜", "买菜", "炒菜", "早饭", "早餐",
             "午饭", "午餐", "晚饭", "晚餐", "夜宵", "宵夜", "零食", "点餐", "火锅", "烧烤", "奶茶", "咖啡", "水果",
             "甜品", "食堂"],
    "工作学习": ["上班", "下班", "加班", "工作", "老板", "领导", "同事", "开会", "会议", "项目", "公司", "单位", "论文",
             "实验", "考试", "学习", "作业", "汇报", "报告", "课题", "导师", "上课", "写代码", "数据", "绩效"],
    "出行旅行": ["旅游", "旅行", "出发", "机票", "高铁", "火车", "飞机", "航班", "酒店", "民宿", "景点", "出差", "打车",
             "地铁", "开车", "堵车", "到站", "行李", "自驾", "车票", "动车", "登机"],
    "休闲娱乐": ["游戏", "电影", "电视剧", "综艺", "追剧", "看剧", "抖音", "小红书", "B站", "王者", "开黑", "上分", "动漫",
             "演唱会", "唱歌", "KTV", "剧本杀", "密室", "小说", "直播"],
    "购物消费": ["淘宝", "京东", "拼多多", "快递", "下单", "包裹", "买了", "想买", "购物", "便宜", "打折", "优惠", "价格",
             "退货", "网购", "衣服", "鞋子", "化妆品", "口红", "双十一"],
    "家人朋友": ["爸爸", "妈妈", "我爸", "我妈", "你爸", "你妈", "爷爷", "奶奶", "外公", "外婆", "姥姥", "姥爷", "哥哥",
             "姐姐", "弟弟", "妹妹", "家里人", "亲戚", "朋友", "同学", "闺蜜", "兄弟", "室友", "舍友"],
    "身体健康": ["生病", "感冒", "发烧", "头疼", "头痛", "肚子疼", "医院", "吃药", "看病", "不舒服", "咳嗽", "体检", "医生",
             "失眠", "睡不着", "运动", "健身", "跑步", "减肥", "体重", "痛经", "姨妈", "过敏", "打针"],
    "作息日常": ["睡觉", "起床", "醒了", "好困", "困了", "睡了", "洗澡", "早安", "晚安", "午睡", "熬夜", "闹钟", "到家",
             "回家", "出门", "洗漱", "躺下", "上床"],
    "情感表达": ["想你", "爱你", "喜欢你", "抱抱", "亲亲", "想见", "见面", "陪你", "陪我", "一辈子", "永远", "在一起",
             "吵架", "生气", "哄", "原谅", "委屈", "心疼"],
    "成家大事": ["婚礼", "婚纱", "领证", "结婚", "彩礼", "酒席", "婚宴", "婚庆", "戒指", "喜糖", "请柬", "伴娘", "伴郎",
             "婚房", "装修", "看房", "买房", "房贷", "家具", "家电", "搬家", "见家长", "订婚"],
    "天气": ["天气", "下雨", "下雪", "降温", "升温", "台风", "带伞", "好热", "好冷", "热死", "冷死", "太阳", "雾霾", "暴雨",
           "温度"],
}
# 食物词典（6e）
FOODS = ["火锅", "烧烤", "奶茶", "咖啡", "蛋糕", "面包", "披萨", "汉堡", "炸鸡", "薯条", "寿司", "拉面", "米线", "螺蛳粉",
         "麻辣烫", "串串", "冒菜", "饺子", "包子", "馄饨", "面条", "米饭", "小龙虾", "烤鱼", "烤肉", "牛排", "酸菜鱼",
         "麻辣香锅", "黄焖鸡", "沙拉", "西瓜", "草莓", "芒果", "榴莲", "葡萄", "车厘子", "冰淇淋", "雪糕", "甜品", "巧克力",
         "零食", "薯片", "可乐", "啤酒", "肯德基", "麦当劳", "必胜客", "海底捞", "星巴克", "瑞幸", "喜茶", "蜜雪冰城",
         "牛肉面", "臭豆腐", "卤味", "鸭脖", "炒饭", "盖饭", "排骨", "鸡腿", "鸡翅", "螃蟹", "羊肉串", "烤串", "煎饼",
         "肠粉", "凉皮", "肉夹馍", "鸡蛋", "牛奶", "酸奶", "水饺", "汤圆", "粽子", "月饼"]
# 地名中需要排除的泛指词
PLACE_STOP = set("中国 国外 国内 全国 外地 本地 当地 老家 家乡 市区 郊区 附近 南方 北方 东北 西北 西南 东南 华北 华南 "
                 "海边 山上 楼下 楼上 门口 路上 这边 那边 里面 外面 宿舍 公司 学校 世界 地球".split())
PLACE_MIN = 5            # 地名总提及次数下限
PLACE_TOP = 100          # 输出的地名数量
PROMISE_WINDOW = 365     # 约定兑现的查找窗口（天）
PROMISE_DEDUP = 30       # 同一对象在多少天内重复约定只算一次

# ============ 正则 ============
INLINE_EMOJI_RE = re.compile(r"\[[\u4e00-\u9fa5A-Za-z]{1,8}\]")
URL_RE = re.compile(r"https?://\S+")
HAS_CJK = re.compile(r"[\u4e00-\u9fa5]")
# 话题、食物：按词长从长到短拼成正则，保证“奶茶”优先于更短的词被匹配
TOPIC_RE = {tp: re.compile("|".join(map(re.escape, sorted(kws, key=len, reverse=True)))) for tp, kws in TOPICS.items()}
FOOD_RE = re.compile("|".join(map(re.escape, sorted(FOODS, key=len, reverse=True))))
# 地名上下文：想去 / 去过
WISH_RE = re.compile(r"想去|下次|以后|改天|有空|打算|计划|要不要去|一起去|带你去|想带|有机会")
VISIT_RE = re.compile(r"去了|去过|到了|来了|回来|玩了|逛了|打卡|刚到|已经到|在这|这边")
# 约定：“下次/以后/改天……一起 + 动词 + 对象”
PROMISE_RE = re.compile(r"(?:下次|以后|改天|有空|有机会|过几天|哪天|到时候)[^，。！？,.!?\s]{0,8}?一起"
                        r"(去|来|吃|喝|看|玩|逛|买|做|拍|爬|泡|旅游|旅行)([^，。！？,.!?\s~～]{0,10})")
VERB2_RE = re.compile(r"^(吃|喝|看|玩|逛|买|做|拍|爬|泡)")
TAIL_RE = re.compile(r"(好不好|好吗|好不|可以吗|行不行|吧|呀|啊|呢|嘛|哦|吗|哈|呗|啦|了|鸭)+$")
COMP_RE = re.compile(r"[去来吃喝看玩逛买做拍爬泡][了过完]|好吃|好喝|好看|好玩|到了|回来了|打卡|终于")
OBJ_STOP = {"一下", "一起", "时候", "东西", "地方", "我们", "你们", "咱们", "这个", "那个", "一次", "一趟", "好吃", "好玩", "好看"}
# “吃什么”类问题
WHATEAT_RE = re.compile(r"吃(?:点)?(?:什么|啥)|去哪(?:儿|里)?吃|吃哪(?:家|个)")

# ============ 读取 ============
t0 = time.time()
print("读取 messages_clean.csv ...")
with open(os.path.join(DATA, "messages_clean.csv"), encoding="utf-8-sig", newline="") as f:
    rows = list(csv.DictReader(f))
print(f"共 {len(rows)} 条消息")

docs = []        # (sender, month, date, hour, text)，按时间顺序
loc_rows = []    # (sender, date, location_name)
for r in rows:
    t = r["text"] if r["type"] == "文字" else (r["voice_text"] if r["type"] == "语音" else "")
    if t:
        # 去掉链接和小黄脸表情代码（如 [亲亲]、[太阳]），避免被误计入话题、食物
        t = INLINE_EMOJI_RE.sub(" ", URL_RE.sub(" ", t))
        docs.append((r["sender"], r["month"], date.fromisoformat(r["date"]), int(r["hour"]), t))
    if r["type"] == "位置" and r.get("location_name"):
        loc_rows.append((r["sender"], r["date"], r["location_name"]))
last_date = docs[-1][2]
print(f"文本消息（含语音转写）: {len(docs)} 条；位置消息: {len(loc_rows)} 条")

# ============ 6a/6b 话题 ============
print("话题打标签 ...")
topic_month, topic_hour, topic_sender, topic_kw = Counter(), Counter(), Counter(), Counter()
month_total, month_tagged = Counter(), Counter()
for s, m, d, h, t in docs:
    month_total[m] += 1
    hit_any = False
    for tp, rx in TOPIC_RE.items():
        hits = rx.findall(t)
        if hits:
            hit_any = True
            topic_month[(m, tp)] += 1
            topic_hour[(tp, h)] += 1
            topic_sender[(tp, s)] += 1
            for k in set(hits):
                topic_kw[(tp, k)] += 1
    if hit_any:
        month_tagged[m] += 1

with open(os.path.join(DATA, "fig6_topic_monthly.csv"), "w", encoding="utf-8-sig", newline="") as f:
    w = csv.writer(f)
    w.writerow(["month", "topic", "n"])
    for (m, tp), n in sorted(topic_month.items()):
        w.writerow([m, tp, n])
with open(os.path.join(DATA, "fig6_topic_month_total.csv"), "w", encoding="utf-8-sig", newline="") as f:
    w = csv.writer(f)
    w.writerow(["month", "n_text", "n_tagged"])
    for m in sorted(month_total):
        w.writerow([m, month_total[m], month_tagged[m]])
with open(os.path.join(DATA, "fig6_topic_hour.csv"), "w", encoding="utf-8-sig", newline="") as f:
    w = csv.writer(f)
    w.writerow(["topic", "hour", "n"])
    for tp in TOPICS:
        for h in range(24):
            w.writerow([tp, h, topic_hour[(tp, h)]])
with open(os.path.join(DATA, "fig6_topic_sender.csv"), "w", encoding="utf-8-sig", newline="") as f:
    w = csv.writer(f)
    w.writerow(["topic", "sender", "n"])
    for tp in TOPICS:
        for s in ("老婆", "我"):
            w.writerow([tp, s, topic_sender[(tp, s)]])
with open(os.path.join(DATA, "fig6_topic_keywords.csv"), "w", encoding="utf-8-sig", newline="") as f:
    w = csv.writer(f)
    w.writerow(["topic", "keyword", "n"])
    for tp, kws in TOPICS.items():
        for k in sorted(kws, key=lambda x: -topic_kw[(tp, x)]):
            w.writerow([tp, k, topic_kw[(tp, k)]])

# ============ 6e 食物与“吃什么” ============
print("统计食物 ...")
food_cnt, food_by, food_month, food_first = Counter(), Counter(), Counter(), {}
whateat = Counter()
for s, m, d, h, t in docs:
    for fd in set(FOOD_RE.findall(t)):
        food_cnt[fd] += 1
        food_by[(fd, s)] += 1
        food_month[(m, fd)] += 1
        if fd not in food_first or d < food_first[fd]:
            food_first[fd] = d
    if WHATEAT_RE.search(t):
        whateat[(m, s)] += 1
with open(os.path.join(DATA, "fig6_food_top.csv"), "w", encoding="utf-8-sig", newline="") as f:
    w = csv.writer(f)
    w.writerow(["food", "n", "n_her", "n_me", "first_date"])
    for fd, n in food_cnt.most_common():
        w.writerow([fd, n, food_by[(fd, "老婆")], food_by[(fd, "我")], food_first[fd].isoformat()])
with open(os.path.join(DATA, "fig6_food_monthly.csv"), "w", encoding="utf-8-sig", newline="") as f:
    w = csv.writer(f)
    w.writerow(["month", "food", "n"])
    for (m, fd), n in sorted(food_month.items()):
        w.writerow([m, fd, n])
months = sorted(month_total)
with open(os.path.join(DATA, "fig6_whateat_monthly.csv"), "w", encoding="utf-8-sig", newline="") as f:
    w = csv.writer(f)
    w.writerow(["month", "sender", "n"])
    for m in months:
        for s in ("老婆", "我"):
            w.writerow([m, s, whateat[(m, s)]])

# ============ 6c 地名 ============
print("提取地名（词性标注，约 3–10 分钟）...")
jieba.setLogLevel(60)
for kws in TOPICS.values():
    for k in kws:
        jieba.add_word(k, freq=50000, tag="n")
for fd in FOODS:
    jieba.add_word(fd, freq=50000, tag="n")
place_cnt, place_by, place_ctx, place_first = Counter(), Counter(), Counter(), {}
t1 = time.time()
for i, (s, m, d, h, t) in enumerate(docs):
    t2 = INLINE_EMOJI_RE.sub(" ", t)
    if HAS_CJK.search(t2):
        ps = {wd for wd, fl in pseg.lcut(t2) if fl == "ns" and len(wd) >= 2 and wd not in PLACE_STOP}
        if ps:
            ctx = "想去" if WISH_RE.search(t2) else ("去过" if VISIT_RE.search(t2) else "其他")
            for p in ps:
                place_cnt[p] += 1
                place_by[(p, s)] += 1
                place_ctx[(p, ctx)] += 1
                if p not in place_first or d < place_first[p]:
                    place_first[p] = d
    if (i + 1) % 50000 == 0:
        el = time.time() - t1
        print(f"  已处理 {i + 1}/{len(docs)} 条，预计还需 {el / (i + 1) * (len(docs) - i - 1) / 60:.1f} 分钟")
place_top = [(p, n) for p, n in place_cnt.most_common() if n >= PLACE_MIN][:PLACE_TOP]
with open(os.path.join(DATA, "fig6_places.csv"), "w", encoding="utf-8-sig", newline="") as f:
    w = csv.writer(f)
    w.writerow(["place", "n", "n_her", "n_me", "n_wish", "n_visited", "n_other", "first_date"])
    for p, n in place_top:
        w.writerow([p, n, place_by[(p, "老婆")], place_by[(p, "我")], place_ctx[(p, "想去")], place_ctx[(p, "去过")],
                    place_ctx[(p, "其他")], place_first[p].isoformat()])
loc_cnt, loc_first = Counter(), {}
for s, d, nm in loc_rows:
    loc_cnt[nm] += 1
    if nm not in loc_first or d < loc_first[nm]:
        loc_first[nm] = d
with open(os.path.join(DATA, "fig6_location_share.csv"), "w", encoding="utf-8-sig", newline="") as f:
    w = csv.writer(f)
    w.writerow(["location_name", "n", "first_date"])
    for nm, n in loc_cnt.most_common():
        w.writerow([nm, n, loc_first[nm]])

# ============ 6d 约定与兑现 ============
print("提取“下次一起……”约定 ...")
promises = []
last_key = {}
for s, m, d, h, t in docs:
    for mt in PROMISE_RE.finditer(t):
        verb, obj = mt.group(1), mt.group(2)
        v2 = VERB2_RE.match(obj)
        if verb in ("去", "来") and v2:
            verb, obj = v2.group(1), obj[1:]
        obj = TAIL_RE.sub("", obj)
        toks = [wd for wd in jieba.lcut(obj) if len(wd) >= 2 and wd not in OBJ_STOP and HAS_CJK.search(wd)]
        key = toks[-1] if toks else (obj if len(obj) >= 2 and obj not in OBJ_STOP else "")
        # 同一对象在 PROMISE_DEDUP 天内重复约定只算一次
        if key and key in last_key and (d - last_key[key]).days < PROMISE_DEDUP:
            continue
        if key:
            last_key[key] = d
        promises.append({"date": d, "sender": s, "verb": verb, "obj": key, "text": t[:60]})
# 只在带有完成表达的消息里查找兑现，提高速度
comp = [(d, t) for s, m, d, h, t in docs if COMP_RE.search(t)]
comp_dates = [d for d, _ in comp]
for p in promises:
    p["fulfilled"], p["fulfill_date"], p["days"] = 0, "", ""
    p["observed_days"] = min(PROMISE_WINDOW, (last_date - p["date"]).days)
    if not p["obj"]:
        continue
    lo_i = bisect.bisect_right(comp_dates, p["date"])
    for j in range(lo_i, len(comp)):
        dj, tj = comp[j]
        if (dj - p["date"]).days > PROMISE_WINDOW:
            break
        if p["obj"] in tj:
            p["fulfilled"], p["fulfill_date"], p["days"] = 1, dj.isoformat(), (dj - p["date"]).days
            break
with open(os.path.join(DATA, "fig6_promises.csv"), "w", encoding="utf-8-sig", newline="") as f:
    w = csv.writer(f)
    w.writerow(["date", "sender", "verb", "obj", "fulfilled", "fulfill_date", "days", "observed_days", "text"])
    for p in promises:
        w.writerow([p["date"].isoformat(), p["sender"], p["verb"], p["obj"], p["fulfilled"], p["fulfill_date"],
                    p["days"], p["observed_days"], p["text"]])

# ============ 摘要（不含聊天正文） ============
lines = []
P = lines.append
P(f"文本消息: {len(docs)} 条；命中任一话题: {sum(month_tagged.values())} 条"
  f"（覆盖率 {sum(month_tagged.values()) / len(docs) * 100:.1f}%）")
tp_tot = Counter()
for (m, tp), n in topic_month.items():
    tp_tot[tp] += n
P("话题命中消息数: " + ", ".join(f"{tp}:{n}" for tp, n in tp_tot.most_common()))
P("各话题命中最多的 3 个关键词:")
for tp, kws in TOPICS.items():
    top3 = sorted(kws, key=lambda x: -topic_kw[(tp, x)])[:3]
    P(f"  {tp}: " + ", ".join(f"{k}({topic_kw[(tp, k)]})" for k in top3))
P(f"\n地名（≥{PLACE_MIN}次）: {len(place_top)} 个；前 20: " + ", ".join(f"{p}({n})" for p, n in place_top[:20]))
P(f"位置消息: {len(loc_rows)} 条，不同地点 {len(loc_cnt)} 个")
obs = [p for p in promises if p["obj"] and p["observed_days"] >= 90]
P(f"\n约定: {len(promises)} 条（有明确对象 {sum(1 for p in promises if p['obj'])} 条）；"
  f"观察期≥90天的 {len(obs)} 条中兑现 {sum(p['fulfilled'] for p in obs)} 条"
  f"（{(sum(p['fulfilled'] for p in obs) / len(obs) * 100) if obs else 0:.1f}%）")
P("约定对象前 15: " + ", ".join(f"{k}({n})" for k, n in Counter(p["obj"] for p in promises if p["obj"]).most_common(15)))
P(f"约定动词: {Counter(p['verb'] for p in promises).most_common()}")
P(f"\n食物前 15: " + ", ".join(f"{fd}({n})" for fd, n in food_cnt.most_common(15)))
P(f"“吃什么”被问: 老婆 {sum(v for (m, s), v in whateat.items() if s == '老婆')} 次，"
  f"我 {sum(v for (m, s), v in whateat.items() if s == '我')} 次")
P(f"\n总用时 {(time.time() - t0) / 60:.1f} 分钟")
with open(os.path.join(DATA, "fig6_summary.txt"), "w", encoding="utf-8") as f:
    f.write("\n".join(lines))
print("\n========== 摘要 ==========")
print("\n".join(lines))
print(f"\n完成！文件已保存到: {DATA}")
