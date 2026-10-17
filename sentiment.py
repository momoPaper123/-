# -*- coding: utf-8 -*-
"""
第 4 步：为 Figure 5（情绪与重要时刻）计算每条消息的情绪分数，输出汇总 CSV 给 R 绘图

方法：SnowNLP 情感概率（0~1，换算为 -1~1）与情绪词典打分各占一半，合成为 -1~1 的情绪分数
      > 0 偏积极，< 0 偏消极；适合看月度、季度的大趋势，不适合解读单条消息

输入：data/messages_clean.csv
输出（都在 data 文件夹）：
  senti_daily.csv      每个聊天日、每人的平均情绪分数
  senti_monthly.csv    每月、每人的平均情绪分数（5a）
  senti_rolling.csv    双方合并的 30 天滑动平均情绪（5b）
  senti_extremes.csv   情绪最高和最低的几段时间（5b 标注用）
  senti_extreme_words.csv  这几段时间里的特征词（帮你回忆当时发生了什么，不放进图里）

运行前安装：pip install snownlp jieba
运行：python -X utf8 sentiment.py
"""
import csv
import os
import re
import time
from collections import Counter, defaultdict
from datetime import date, timedelta

import jieba
from snownlp import SnowNLP

BASE = r"C:\Users\Administrator\Desktop\LP可视化"
DATA = os.path.join(BASE, "data")

# ============ 参数 ============
ROLL_DAYS = 30           # 滑动平均窗口（天）
MIN_DAY_MSGS = 20        # 少于这个消息数的日子不参与滑动平均
N_EXTREME = 3            # 高峰、低谷各取几段
MIN_GAP_DAYS = 90        # 两段高峰（或低谷）之间至少相隔的天数

# 情绪词典：聊天口语中常见的积极 / 消极表达（可自行增删）
POS_WORDS = ["爱你", "喜欢", "想你", "开心", "高兴", "快乐", "幸福", "哈哈", "嘿嘿", "嘻嘻", "亲亲", "抱抱",
             "么么", "宝贝", "宝宝", "谢谢", "好棒", "厉害", "可爱", "漂亮", "好看", "好吃", "舒服", "期待",
             "放心", "加油", "辛苦了", "晚安", "早安", "甜", "暖", "棒", "爽", "哇", "耶", "好耶", "完美"]
NEG_WORDS = ["难过", "伤心", "生气", "气死", "烦", "讨厌", "委屈", "哭", "累", "困死", "痛", "疼", "难受",
             "不开心", "郁闷", "崩溃", "无语", "失望", "害怕", "担心", "焦虑", "吵架", "滚", "算了",
             "不想理", "别烦", "后悔", "对不起", "抱歉", "糟糕", "倒霉", "惨", "死了", "垃圾", "恶心"]
POS_EMOJI = ["[微笑]", "[愉快]", "[憨笑]", "[偷笑]", "[呲牙]", "[爱心]", "[亲亲]", "[拥抱]", "[玫瑰]",
             "[色]", "[可爱]", "[得意]", "[强]", "[耶]", "[庆祝]", "[嘿哈]", "[奸笑]", "[机智]"]
NEG_EMOJI = ["[流泪]", "[大哭]", "[难过]", "[委屈]", "[发怒]", "[抓狂]", "[心碎]", "[叹气]", "[苦涩]",
             "[衰]", "[撇嘴]", "[白眼]", "[鄙视]", "[快哭了]", "[生病]", "[吐]"]
NEGATORS = ["不", "没", "别", "不是", "没有"]   # 否定词出现在积极词前时，视为消极

INLINE_EMOJI_RE = re.compile(r"\[[\u4e00-\u9fa5A-Za-z]{1,8}\]")
URL_RE = re.compile(r"https?://\S+")
HAS_CJK = re.compile(r"[\u4e00-\u9fa5]")


def lexicon_score(t):
    """词典打分：积极词 +1、消极词 -1，按命中数归一化到 -1~1；没有命中返回 None"""
    pos = neg = 0
    for w in POS_WORDS:
        i = t.find(w)
        while i != -1:
            prev = t[max(0, i - 2):i]
            if any(prev.endswith(n) for n in NEGATORS):
                neg += 1
            else:
                pos += 1
            i = t.find(w, i + len(w))
    for w in NEG_WORDS:
        neg += t.count(w)
    pos += sum(t.count(e) for e in POS_EMOJI)
    neg += sum(t.count(e) for e in NEG_EMOJI)
    if pos + neg == 0:
        return None
    return (pos - neg) / (pos + neg)


def snow_score(t):
    """SnowNLP 情感概率换算到 -1~1；没有中文的消息返回 None"""
    t = INLINE_EMOJI_RE.sub("", URL_RE.sub("", t)).strip()
    if not HAS_CJK.search(t):
        return None
    try:
        return SnowNLP(t).sentiments * 2 - 1
    except Exception:
        return None


def combine(s, l):
    if s is None and l is None:
        return None
    if s is None:
        return l
    if l is None:
        return s
    return 0.5 * s + 0.5 * l


# ============ 读取 ============
print("读取 messages_clean.csv ...")
with open(os.path.join(DATA, "messages_clean.csv"), encoding="utf-8-sig", newline="") as f:
    rows = list(csv.DictReader(f))
docs = []
for r in rows:
    t = r["text"] if r["type"] == "文字" else (r["voice_text"] if r["type"] == "语音" else "")
    if t:
        docs.append((r["chat_day"], r["month"], r["sender"], t))
print(f"待打分消息（文字 + 语音转写）: {len(docs)} 条")

# ============ 逐条打分 ============
print("情绪打分中（约 5–15 分钟，请耐心等待）...")
t0 = time.time()
scored = []                      # (chat_day, month, sender, score)
cache = {}                       # 相同文本只算一次（“哈哈”“好的”之类重复很多）
for i, (d, m, s, t) in enumerate(docs):
    if t in cache:
        sc = cache[t]
    else:
        sc = combine(snow_score(t), lexicon_score(t))
        cache[t] = sc
    if sc is not None:
        scored.append((d, m, s, sc))
    if (i + 1) % 20000 == 0:
        el = time.time() - t0
        print(f"  已处理 {i + 1}/{len(docs)} 条，用时 {el / 60:.1f} 分钟，预计还需 {el / (i + 1) * (len(docs) - i - 1) / 60:.1f} 分钟")
print(f"有效打分: {len(scored)} 条（无中文且无表情词的消息未打分）")


def summarize(groups):
    out = []
    for key, v in sorted(groups.items()):
        n = len(v)
        mean = sum(v) / n
        sd = (sum((x - mean) ** 2 for x in v) / (n - 1)) ** 0.5 if n > 1 else 0
        out.append(key + (n, round(mean, 4), round(sd / n ** 0.5, 4) if n > 1 else 0,
                          round(sum(x > 0.2 for x in v) / n, 4), round(sum(x < -0.2 for x in v) / n, 4)))
    return out


# ============ 每日、每月汇总 ============
day_g, mon_g, day_all = defaultdict(list), defaultdict(list), defaultdict(list)
for d, m, s, sc in scored:
    day_g[(d, s)].append(sc)
    mon_g[(m, s)].append(sc)
    day_all[d].append(sc)
head = ["n", "mean", "se", "pos_share", "neg_share"]
with open(os.path.join(DATA, "senti_daily.csv"), "w", encoding="utf-8-sig", newline="") as f:
    w = csv.writer(f)
    w.writerow(["chat_day", "sender"] + head)
    w.writerows(summarize(day_g))
with open(os.path.join(DATA, "senti_monthly.csv"), "w", encoding="utf-8-sig", newline="") as f:
    w = csv.writer(f)
    w.writerow(["month", "sender"] + head)
    w.writerows(summarize(mon_g))

# ============ 双方合并的 30 天滑动平均（按消息数加权） ============
days = sorted(day_all)
d0, d1 = date.fromisoformat(days[0]), date.fromisoformat(days[-1])
all_days = [d0 + timedelta(i) for i in range((d1 - d0).days + 1)]
sums = {d: (sum(v), len(v)) for d, v in day_all.items() if len(v) >= MIN_DAY_MSGS}
half = ROLL_DAYS // 2
roll = []
for dd in all_days:
    tot = cnt = 0
    for k in range(-half, half + 1):
        key = (dd + timedelta(k)).isoformat()
        if key in sums:
            tot += sums[key][0]
            cnt += sums[key][1]
    roll.append((dd.isoformat(), round(tot / cnt, 4) if cnt >= MIN_DAY_MSGS * 5 else ""))
with open(os.path.join(DATA, "senti_rolling.csv"), "w", encoding="utf-8-sig", newline="") as f:
    w = csv.writer(f)
    w.writerow(["date", "roll"])
    w.writerows(roll)

# ============ 高峰与低谷：贪心选取，彼此至少相隔 MIN_GAP_DAYS 天 ============
valid = [(date.fromisoformat(d), v) for d, v in roll if v != ""]
def pick(desc):
    chosen = []
    for dd, v in sorted(valid, key=lambda x: -x[1] if desc else x[1]):
        if all(abs((dd - c).days) >= MIN_GAP_DAYS for c, _ in chosen):
            chosen.append((dd, v))
        if len(chosen) == N_EXTREME:
            break
    return chosen
extremes = [("高峰", i + 1, dd, v) for i, (dd, v) in enumerate(pick(True))] + \
           [("低谷", i + 1, dd, v) for i, (dd, v) in enumerate(pick(False))]
with open(os.path.join(DATA, "senti_extremes.csv"), "w", encoding="utf-8-sig", newline="") as f:
    w = csv.writer(f)
    w.writerow(["kind", "rank", "center", "start", "end", "roll"])
    for k, r, dd, v in extremes:
        w.writerow([k, r, dd.isoformat(), (dd - timedelta(half)).isoformat(), (dd + timedelta(half)).isoformat(), v])

# ============ 高峰低谷期间的特征词（与全时段相比出现得格外多的词） ============
print("提取高峰、低谷期间的特征词 ...")
jieba.setLogLevel(60)
STOP = set("的 了 是 我 你 他 她 这 那 就 都 也 还 在 有 没 不 吗 呢 吧 啊 呀 哦 嗯 哈 啦 嘛 什么 怎么 一个 一下 可以 现在 今天 明天 就是 然后 还是 已经 知道 觉得 感觉 没有 不是 这个 那个 我们 你们".split())
win_words = defaultdict(Counter)
overall = Counter()
win_of = []
for k, r, dd, v in extremes:
    win_of.append((f"{k}{r}", dd - timedelta(half), dd + timedelta(half)))
for d, _, _, t in docs:
    dd = date.fromisoformat(d)
    ws = [x for x in jieba.lcut(INLINE_EMOJI_RE.sub(" ", URL_RE.sub(" ", t)))
          if len(x) >= 2 and x not in STOP and HAS_CJK.search(x)]
    overall.update(ws)
    for name, a, b in win_of:
        if a <= dd <= b:
            win_words[name].update(ws)
n_all = sum(overall.values())
with open(os.path.join(DATA, "senti_extreme_words.csv"), "w", encoding="utf-8-sig", newline="") as f:
    w = csv.writer(f)
    w.writerow(["period", "word", "n_window", "ratio"])
    for name, _, _ in win_of:
        c = win_words[name]
        n_w = sum(c.values()) or 1
        ranked = sorted(((wd, cw, (cw / n_w) / ((overall[wd] + 1) / n_all)) for wd, cw in c.items() if cw >= 5),
                        key=lambda x: -x[2])[:10]
        for wd, cw, ra in ranked:
            w.writerow([name, wd, cw, round(ra, 2)])

# ============ 摘要（不含聊天正文） ============
print("\n========== 摘要 ==========")
for s in ("老婆", "我"):
    v = [sc for _, _, ss, sc in scored if ss == s]
    print(f"{s}: 打分 {len(v)} 条，平均情绪 {sum(v) / len(v):.3f}，"
          f"积极(>0.2) {sum(x > 0.2 for x in v) / len(v) * 100:.1f}%，消极(<-0.2) {sum(x < -0.2 for x in v) / len(v) * 100:.1f}%")
print("高峰与低谷（30天滑动平均中心日期）:")
for k, r, dd, v in extremes:
    print(f"  {k}{r}: {dd}  情绪 {v:.3f}")
print(f"总用时 {(time.time() - t0) / 60:.1f} 分钟")
print(f"\n完成！文件已保存到: {DATA}")
