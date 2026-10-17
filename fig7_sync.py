# -*- coding: utf-8 -*-
"""
第 6 步：为 Figure 7（越来越像：同步与相互影响）提取特征，输出汇总 CSV 给 R 绘图

方法：
  7a 用词趋同：每季度双方词频向量的余弦相似度；另将该季度每条消息的发送人标签随机打乱 10 次，
     得到“两人说话没有差别”时的参照值，观测值越接近参照值，说明两人用词越像
  7b 口头禅传染：某个词（或小黄脸表情）先被一方稳定使用（累计第 3 次），至少 60 天后另一方才开始稳定使用，
     且之后另一方用了不少于 20 次，视为一次“传染”，记录谁传给谁、隔了多久
  7c 语言风格匹配（LSM）：按月计算双方在 8 类虚词上的使用比例，
     每类 LSM = 1 − |p老婆 − p我| / (p老婆 + p我 + 0.0001)，8 类取平均，越接近 1 说话风格越像
  7d/7e 情绪回应：以“回合”（同一人连续发的消息）为单位，用与 Figure 5 相同的方法（SnowNLP + 情绪词典）打情绪分；
     比较对方回合为消极（< −0.2）与非消极时，回应的回复间隔和回应长度
  7f 见面推断：每个聊天日的消息数与前后 15 天中位数之比（越低越可能在一起），以及“到楼下/来接你/到了吗”等见面用语
  以上均为基于词典与规则的统计，适合看整体规律

输入：data/messages_clean.csv
输出（都在 data 文件夹，文件名以 fig7_ 开头）：
  fig7_similarity_quarterly.csv  每季度用词相似度与随机参照值（7a）
  fig7_contagion.csv             口头禅传染明细（7b）
  fig7_lsm_monthly.csv           每月各类虚词比例与 LSM（7c）
  fig7_turn_pairs.csv            每一对“回合—回应”的情绪类别、回复间隔、回应长度（7d、7e，不含正文）
  fig7_daily.csv                 每个聊天日的消息数、滑动中位数、比值、见面用语次数（7f）
  fig7_summary.txt               摘要（不含聊天正文，可发给 Claude 核对）

运行：python -X utf8 fig7_sync.py
"""
import csv
import math
import os
import random
import re
import statistics
import time
from collections import Counter, defaultdict
from datetime import date, timedelta

import jieba
from snownlp import SnowNLP

# ============ 路径 ============
BASE = r"C:\Users\Administrator\Desktop\LP可视化"
DATA = os.path.join(BASE, "data")
random.seed(2025)

# ============ 参数 ============
N_SHUFFLE = 10                 # 7a 随机打乱次数
SIM_MIN_COUNT = 3              # 7a 当季总次数不少于该值的词才进入词表
STABLE_N = 3                   # 7b 累计第几次使用算“开始稳定使用”
CONTAGION_LAG = 60             # 7b 两人开始稳定使用的间隔下限（天）
CONTAGION_EDGE = 60            # 7b 传出方开始使用的日期需晚于聊天开始多少天（排除认识前就有的口头禅）
ORIGIN_BEFORE = 5              # 7b 被传染前，传出方至少已用过的次数
ADOPT_AFTER = 20               # 7b 被传染后，接收方至少使用的次数
LSM_MIN_TOK = 500              # 7c 某月某人词数少于该值不计算
NEG_CUT, POS_CUT = -0.2, 0.2   # 7d/7e 消极、积极的阈值（与 Figure 5 一致）
ROLL_HALF = 15                 # 7f 滑动中位数的半窗口（天）
ME, HER = "我", "老婆"

# ============ 词表 ============
# 停用词（与 text_features.py 一致）
STOPWORDS = set("""
的 了 是 我 你 他 她 它 我们 你们 他们 她们 这 那 这个 那个 这样 那样 就 都 也 还 又 在 有 没 没有 不 不是
和 跟 与 及 把 被 给 让 对 向 从 到 为 为了 以 于 而 但 但是 可是 所以 因为 如果 然后 还是 或者 而且
吗 呢 吧 啊 呀 哦 噢 嗯 恩 哈 啦 嘛 哇 呐 诶 欸 唉 哎 嘿 喔 咯 哟 呗 么 滴 哒 呃 额
什么 怎么 怎么样 为什么 哪 哪里 哪个 多少 几 谁 一个 一下 一点 一些 有点 一样 一直 一起 已经 现在 今天 明天 昨天
可以 可能 应该 需要 觉得 知道 感觉 好像 就是 不过 只是 还有 这么 那么 多 很 太 真 好 挺 最 更 比较 非常
要 会 能 想 去 来 说 看 做 弄 搞 吃 走 用 拿 叫 等 下 上 里 中 后 前 时候 东西 问题 事情 自己 人
个 们 些 点 次 条 张 下次 刚刚 刚才 马上 先 再 才 呗 呀呀 嗯嗯 哦哦 好的 好吧 对 对呀 是的 没事
""".split())
# 7c 八类虚词（每个词只归入一类）
FUNC = {
    "人称代词": "我 你 他 她 它 我们 你们 他们 她们 咱 咱们 自己 人家",
    "语气词": "啊 呀 呢 吧 嘛 哦 噢 哈 啦 呗 咯 哇 嗯 诶 欸 哎 唉 嘿 喔 哟 呐 滴 哒",
    "否定词": "不 没 没有 别 不是 不要 不用 甭",
    "连词": "和 跟 但 但是 可是 所以 因为 然后 而且 如果 或者 还是 不过 虽然 要是 于是",
    "程度副词": "很 太 非常 特别 超 超级 挺 有点 比较 更 最 真 巨 好多",
    "疑问词": "吗 什么 怎么 为什么 哪 哪里 谁 几 多少 咋 啥 怎么样",
    "时态词": "了 过 着 在 正在 已经 刚 刚刚 马上 就 才 还",
    "介词": "把 被 给 对 向 从 到 为 让 比 往 替",
}
FUNC_OF = {}
for cat, ws in FUNC.items():
    for wd in ws.split():
        FUNC_OF.setdefault(wd, cat)
# 情绪词典（与 sentiment.py 完全一致）
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
NEGATORS = ["不", "没", "别", "不是", "没有"]
# 7f 见面用语
CUE_RE = re.compile(r"到楼下|在楼下|下楼|来接|接你|接我|等你|见面|马上到|快到了|到了吗|我到了|到门口|在门口|开门")

INLINE_EMOJI_RE = re.compile(r"\[[\u4e00-\u9fa5A-Za-z]{1,8}\]")
URL_RE = re.compile(r"https?://\S+")
HAS_CJK = re.compile(r"[\u4e00-\u9fa5]")
STOP_RE = re.compile(r"^[\d\W_a-zA-Z]+$")
PUNCT_RE = re.compile(r"^[\s\W_]+$")


def lexicon_score(t):
    """词典打分（与 sentiment.py 一致）"""
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
    """SnowNLP 情感概率换算到 -1~1（与 sentiment.py 一致）"""
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


def quarter_of(month):
    y, m = month.split("-")
    return f"{y}Q{(int(m) - 1) // 3 + 1}"


def cosine(a, b, vocab):
    dot = sum(a[w] * b[w] for w in vocab)
    na = math.sqrt(sum(a[w] ** 2 for w in vocab))
    nb = math.sqrt(sum(b[w] ** 2 for w in vocab))
    return dot / (na * nb) if na and nb else float("nan")


# ============ 读取 ============
t0 = time.time()
print("读取 messages_clean.csv ...")
with open(os.path.join(DATA, "messages_clean.csv"), encoding="utf-8-sig", newline="") as f:
    rows = list(csv.DictReader(f))
print(f"共 {len(rows)} 条消息")
start_day = date.fromisoformat(rows[0]["date"])

# ============ 分词 ============
print("中文分词中（约 1–3 分钟）...")
jieba.setLogLevel(60)
for r in rows:
    t = r["text"] if r["type"] == "文字" else (r["voice_text"] if r["type"] == "语音" else "")
    r["_t"] = t
    r["_content"], r["_emo"], r["_toks"] = [], [], []
    if t:
        clean = URL_RE.sub(" ", t)
        r["_emo"] = INLINE_EMOJI_RE.findall(clean)
        body = INLINE_EMOJI_RE.sub(" ", clean)
        toks = [w.strip() for w in jieba.lcut(body) if w.strip() and not PUNCT_RE.match(w.strip())]
        r["_toks"] = toks
        r["_content"] = [w for w in toks if len(w) >= 2 and w not in STOPWORDS and not STOP_RE.match(w)]

# ============ 7a 用词趋同 ============
print("计算每季度用词相似度 ...")
q_msgs = defaultdict(list)
for r in rows:
    if r["_content"]:
        q_msgs[quarter_of(r["month"])].append((r["sender"], Counter(r["_content"])))
sim_rows = []
for q in sorted(q_msgs):
    lst = q_msgs[q]
    tot = Counter()
    by = {HER: Counter(), ME: Counter()}
    for s, c in lst:
        tot.update(c)
        by[s].update(c)
    vocab = [w for w, n in tot.items() if n >= SIM_MIN_COUNT]
    obs = cosine(by[HER], by[ME], vocab)
    labels = [s for s, _ in lst]
    shuf = []
    for _ in range(N_SHUFFLE):
        random.shuffle(labels)
        a, b = Counter(), Counter()
        for lab, (_, c) in zip(labels, lst):
            (a if lab == HER else b).update(c)
        shuf.append(cosine(a, b, vocab))
    sim_rows.append((q, sum(by[HER].values()), sum(by[ME].values()), len(vocab), round(obs, 4),
                     round(statistics.mean(shuf), 4), round(statistics.pstdev(shuf), 4)))
with open(os.path.join(DATA, "fig7_similarity_quarterly.csv"), "w", encoding="utf-8-sig", newline="") as f:
    w = csv.writer(f)
    w.writerow(["quarter", "n_tok_her", "n_tok_me", "n_vocab", "cosine", "cosine_shuffled", "cosine_shuffled_sd"])
    w.writerows(sim_rows)

# ============ 7b 口头禅传染 ============
print("识别口头禅传染 ...")
uses = defaultdict(lambda: {HER: [], ME: []})
for r in rows:
    d = date.fromisoformat(r["date"])
    for tok in set(r["_content"]) | set(r["_emo"]):
        uses[tok][r["sender"]].append(d)
cont = []
for tok, u in uses.items():
    if len(u[HER]) < STABLE_N or len(u[ME]) < STABLE_N:
        continue
    s_her, s_me = u[HER][STABLE_N - 1], u[ME][STABLE_N - 1]
    if s_her <= s_me:
        origin, adopter, d_o, d_a = HER, ME, s_her, s_me
    else:
        origin, adopter, d_o, d_a = ME, HER, s_me, s_her
    if (d_a - d_o).days < CONTAGION_LAG or (d_o - start_day).days < CONTAGION_EDGE:
        continue
    n_before = sum(1 for x in u[origin] if x < d_a)
    n_after = sum(1 for x in u[adopter] if x >= d_a)
    if n_before < ORIGIN_BEFORE or n_after < ADOPT_AFTER:
        continue
    cont.append((tok, "表情" if tok.startswith("[") else "词语", origin, adopter, d_o.isoformat(), d_a.isoformat(),
                 (d_a - d_o).days, n_before, n_after, len(u[HER]) + len(u[ME])))
cont.sort(key=lambda x: -x[8])
with open(os.path.join(DATA, "fig7_contagion.csv"), "w", encoding="utf-8-sig", newline="") as f:
    w = csv.writer(f)
    w.writerow(["token", "kind", "origin", "adopter", "origin_date", "adopt_date", "lag_days", "n_origin_before",
                "n_adopter_after", "n_total"])
    w.writerows(cont)

# ============ 7c 语言风格匹配 ============
print("计算语言风格匹配 ...")
m_tot = Counter()
m_cat = Counter()
for r in rows:
    if r["_toks"]:
        m_tot[(r["month"], r["sender"])] += len(r["_toks"])
        for tok in r["_toks"]:
            cat = FUNC_OF.get(tok)
            if cat:
                m_cat[(r["month"], r["sender"], cat)] += 1
lsm_rows = []
for m in sorted({k[0] for k in m_tot}):
    if m_tot[(m, HER)] < LSM_MIN_TOK or m_tot[(m, ME)] < LSM_MIN_TOK:
        continue
    vals = []
    for cat in FUNC:
        ph = m_cat[(m, HER, cat)] / m_tot[(m, HER)]
        pm = m_cat[(m, ME, cat)] / m_tot[(m, ME)]
        lsm = 1 - abs(ph - pm) / (ph + pm + 0.0001)
        vals.append(lsm)
        lsm_rows.append((m, cat, round(ph * 100, 3), round(pm * 100, 3), round(lsm, 4)))
    lsm_rows.append((m, "总体", "", "", round(sum(vals) / len(vals), 4)))
with open(os.path.join(DATA, "fig7_lsm_monthly.csv"), "w", encoding="utf-8-sig", newline="") as f:
    w = csv.writer(f)
    w.writerow(["month", "category", "pct_her", "pct_me", "lsm"])
    w.writerows(lsm_rows)

# ============ 7d/7e 情绪回应 ============
print("回合情绪打分中（约 5–15 分钟，请耐心等待）...")
cache = {}
t1 = time.time()
n_txt = sum(1 for r in rows if r["_t"])
k = 0
for r in rows:
    r["_s"] = None
    if r["_t"]:
        t = r["_t"]
        if t not in cache:
            cache[t] = combine(snow_score(t), lexicon_score(t))
        r["_s"] = cache[t]
        k += 1
        if k % 20000 == 0:
            el = time.time() - t1
            print(f"  已处理 {k}/{n_txt} 条，预计还需 {el / k * (n_txt - k) / 60:.1f} 分钟")
# 汇总为回合
turns = {}
order = []
for r in rows:
    tid = int(r["turn_id"])
    if tid not in turns:
        turns[tid] = {"sender": r["sender"], "date": r["date"], "reply": r["reply_sec"], "n": 0, "chars": 0, "s": []}
        order.append(tid)
    tu = turns[tid]
    tu["n"] += 1
    if r["type"] == "文字" and r["char_len"]:
        tu["chars"] += int(float(r["char_len"]))
    if r["_s"] is not None:
        tu["s"].append(r["_s"])


def cls(v):
    if v is None:
        return ""
    return "消极" if v < NEG_CUT else ("积极" if v > POS_CUT else "中性")


pairs = []
for a, b in zip(order[:-1], order[1:]):
    ta, tb = turns[a], turns[b]
    if tb["reply"] == "" or not ta["s"]:
        continue
    sa = sum(ta["s"]) / len(ta["s"])
    sb = sum(tb["s"]) / len(tb["s"]) if tb["s"] else None
    pairs.append((tb["date"], tb["sender"], cls(sa), round(sa, 4), int(float(tb["reply"])), tb["n"], tb["chars"],
                  "" if sb is None else round(sb, 4), cls(sb)))
with open(os.path.join(DATA, "fig7_turn_pairs.csv"), "w", encoding="utf-8-sig", newline="") as f:
    w = csv.writer(f)
    w.writerow(["date", "responder", "trigger_class", "trigger_senti", "reply_sec", "resp_msgs", "resp_chars",
                "resp_senti", "resp_class"])
    w.writerows(pairs)

# ============ 7f 见面推断 ============
print("统计每日消息量与见面用语 ...")
day_n, day_cue = Counter(), Counter()
for r in rows:
    day_n[r["chat_day"]] += 1
    if r["_t"] and CUE_RE.search(r["_t"]):
        day_cue[r["chat_day"]] += 1
d0, d1 = date.fromisoformat(min(day_n)), date.fromisoformat(max(day_n))
all_days = [d0 + timedelta(i) for i in range((d1 - d0).days + 1)]
ns = [day_n[d.isoformat()] for d in all_days]
daily = []
for i, d in enumerate(all_days):
    win = ns[max(0, i - ROLL_HALF):i + ROLL_HALF + 1]
    med = statistics.median(win)
    daily.append((d.isoformat(), ns[i], day_cue[d.isoformat()], med, round(ns[i] / med, 4) if med > 0 else ""))
with open(os.path.join(DATA, "fig7_daily.csv"), "w", encoding="utf-8-sig", newline="") as f:
    w = csv.writer(f)
    w.writerow(["chat_day", "n", "cue_n", "roll_median", "ratio"])
    w.writerows(daily)

# ============ 摘要（不含聊天正文） ============
lines = []
P = lines.append
P("7a 每季度用词余弦相似度（观测 / 随机参照）:")
for q, nh, nm, nv, obs, sh, sd in sim_rows:
    P(f"  {q}: {obs:.3f} / {sh:.3f}  （词数 老婆 {nh}，我 {nm}）")
dir_cnt = Counter((c[2], c[3]) for c in cont)
P(f"\n7b 口头禅传染: 共 {len(cont)} 个；老婆→我 {dir_cnt[(HER, ME)]} 个，我→老婆 {dir_cnt[(ME, HER)]} 个")
P("  被传染后使用最多的前 25 个: " + ", ".join(f"{c[0]}({c[2]}→{c[3]},{c[6]}天,{c[8]}次)" for c in cont[:25]))
lsm_all = [(m, v) for m, c, _, _, v in lsm_rows if c == "总体"]
if lsm_all:
    P(f"\n7c 月度 LSM: {len(lsm_all)} 个月，范围 {min(v for _, v in lsm_all):.3f}–{max(v for _, v in lsm_all):.3f}，"
      f"首月 {lsm_all[0][1]:.3f}，末月 {lsm_all[-1][1]:.3f}")
P(f"\n7d/7e 回合—回应配对: {len(pairs)} 对")
for who in (ME, HER):
    for c in ("消极", "中性", "积极"):
        sub = [p for p in pairs if p[1] == who and p[2] == c]
        if sub:
            P(f"  {who}回应对方{c}回合: {len(sub)} 对，回复间隔中位数 {statistics.median(p[4] for p in sub):.0f} 秒，"
              f"回应字数中位数 {statistics.median(p[6] for p in sub):.0f}")
low = [x for x in daily if x[4] != "" and x[4] < 0.3]
P(f"\n7f 聊天日 {len(daily)} 天；消息数低于前后 15 天中位数 30% 的日子 {len(low)} 天")
for y in sorted({x[0][:4] for x in daily}):
    yd = [x for x in daily if x[0][:4] == y]
    P(f"  {y}: 低消息日 {sum(1 for x in yd if x[4] != '' and x[4] < 0.3)}/{len(yd)} 天，"
      f"见面用语 {sum(x[2] for x in yd)} 次")
P(f"\n总用时 {(time.time() - t0) / 60:.1f} 分钟")
with open(os.path.join(DATA, "fig7_summary.txt"), "w", encoding="utf-8") as f:
    f.write("\n".join(lines))
print("\n========== 摘要 ==========")
print("\n".join(lines))
print(f"\n完成！文件已保存到: {DATA}")
