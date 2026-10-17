# -*- coding: utf-8 -*-
"""
第 3 步：为 Figure 4（我们的语言）提取文本特征，输出汇总 CSV 给 R 绘图

输入：data/messages_clean.csv（clean_messages.py 生成）、LYQ/media（表情包图片）
输出（都在 data 文件夹）：
  text_wordfreq.csv        4a 词云：双方各自高频词
  text_logodds.csv         4b 专属词：你更常说 vs 她更常说
  text_emoji_inline.csv    4c 小黄脸表情排行（如 [捂脸]）
  text_sticker_top.csv     4d 表情包排行（附图片路径）
  text_sticker_img/        4d 表情包图片（统一转为 PNG）
  text_nickname.csv        4e 称呼的月度使用次数
  text_nickname_candidates.csv  可能的称呼候选词（供你检查、补充称呼清单）
  text_keywords.csv        4f 关键词的月度使用次数
  text_month_totals.csv    每月双方文字消息总数（用于把次数换算成“每千条”）
  text_newwords.csv        4g 每个季度新出现的高频词

运行前安装：pip install jieba pillow
运行：python -X utf8 text_features.py
"""
import csv
import math
import os
import re
import shutil
from collections import Counter, defaultdict

import jieba

# ============ 路径 ============
BASE = r"C:\Users\Administrator\Desktop\LP可视化"
DATA = os.path.join(BASE, "data")
MEDIA = os.path.join(BASE, "LYQ", "media")
IMG_OUT = os.path.join(DATA, "text_sticker_img")

# ============ 可自行修改的清单 ============
# 称呼清单：可以把你们专属的叫法加进来（运行后看 text_nickname_candidates.csv 找灵感）
NICKNAMES = ["宝宝", "宝贝", "宝子", "老婆", "老公", "媳妇", "亲爱的",
             "猪猪", "小猪", "乖乖", "崽崽", "傻瓜", "笨蛋"]
# 关键词清单（已按你的要求去掉“对不起”等负面词）
KEYWORDS = ["早安", "晚安", "想你", "爱你", "抱抱", "亲亲", "谢谢", "哈哈"]
TOP_WORDS = 300          # 4a 每人保留的高频词数
TOP_LOGODDS = 25         # 4b 每人保留的专属词数
MIN_WORD_TOTAL = 30      # 4b 词语总出现次数下限
TOP_STICKERS = 12        # 4d 表情包排行数量
NEW_MIN_COUNT = 10       # 4g 新词在当季至少出现的次数
NEW_MAX_BEFORE = 1       # 4g 新词在此前所有季度最多出现的次数
NEW_TOP = 5              # 4g 每季度保留的新词数

# 停用词：常见虚词、语气词和聊天高频但无信息量的词
STOPWORDS = set("""
的 了 是 我 你 他 她 它 我们 你们 他们 她们 这 那 这个 那个 这样 那样 就 都 也 还 又 在 有 没 没有 不 不是
和 跟 与 及 把 被 给 让 对 向 从 到 为 为了 以 于 而 但 但是 可是 所以 因为 如果 然后 还是 或者 而且
吗 呢 吧 啊 呀 哦 噢 嗯 恩 哈 啦 嘛 哇 呐 诶 欸 唉 哎 嘿 喔 咯 哟 呗 么 滴 哒 呃 额
什么 怎么 怎么样 为什么 哪 哪里 哪个 多少 几 谁 一个 一下 一点 一些 有点 一样 一直 一起 已经 现在 今天 明天 昨天
可以 可能 应该 需要 觉得 知道 感觉 好像 就是 不过 只是 还有 这么 那么 多 很 太 真 好 挺 最 更 比较 非常
要 会 能 想 去 来 说 看 做 弄 搞 吃 走 用 拿 叫 等 下 上 里 中 后 前 时候 东西 问题 事情 自己 人
个 们 些 点 次 条 张 下次 刚刚 刚才 马上 先 再 才 呗 呀呀 嗯嗯 哦哦 好的 好吧 对 对呀 是的 没事
""".split())
STOP_RE = re.compile(r"^[\d\W_a-zA-Z]+$")             # 纯数字、标点、英文
INLINE_EMOJI_RE = re.compile(r"\[([\u4e00-\u9fa5A-Za-z]{1,8})\]")
URL_RE = re.compile(r"https?://\S+")
MD5_RE = re.compile(r"[0-9a-f]{32}")


def quarter_of(month):
    """'2024-05' -> '2024Q2'"""
    y, m = month.split("-")
    return f"{y}Q{(int(m) - 1) // 3 + 1}"


# ============ 读取 ============
print("读取 messages_clean.csv ...")
with open(os.path.join(DATA, "messages_clean.csv"), encoding="utf-8-sig", newline="") as f:
    rows = list(csv.DictReader(f))
print(f"共 {len(rows)} 条消息")

# 文本语料：文字消息 + 语音转写
docs = []           # (sender, month, date, text)
stickers = []       # (sender, month, date, md5)
for r in rows:
    if r["type"] == "文字" and r["text"]:
        docs.append((r["sender"], r["month"], r["date"], r["text"]))
    elif r["type"] == "语音" and r["voice_text"]:
        docs.append((r["sender"], r["month"], r["date"], r["voice_text"]))
    elif r["type"] == "表情包" and r.get("emoji_md5"):
        stickers.append((r["sender"], r["month"], r["date"], r["emoji_md5"].lower()))
print(f"文本消息（含语音转写）: {len(docs)} 条；表情包: {len(stickers)} 条")

# ============ 每月文字消息总数（用于归一化） ============
month_tot = Counter((s, m) for s, m, _, _ in docs)
with open(os.path.join(DATA, "text_month_totals.csv"), "w", encoding="utf-8-sig", newline="") as f:
    w = csv.writer(f)
    w.writerow(["month", "sender", "n_text"])
    for (s, m), n in sorted(month_tot.items(), key=lambda x: (x[0][1], x[0][0])):
        w.writerow([m, s, n])

# ============ 4c 小黄脸表情 ============
emoji_cnt = Counter()
for s, _, _, t in docs:
    for e in INLINE_EMOJI_RE.findall(t):
        emoji_cnt[(e, s)] += 1
with open(os.path.join(DATA, "text_emoji_inline.csv"), "w", encoding="utf-8-sig", newline="") as f:
    w = csv.writer(f)
    w.writerow(["emoji", "sender", "n"])
    for (e, s), n in emoji_cnt.most_common():
        w.writerow([f"[{e}]", s, n])

# ============ 4e 称呼、4f 关键词（按“包含该词的消息数”统计） ============
def count_terms(terms, out_name):
    cnt = Counter()
    first = {}
    for s, m, d, t in docs:
        for k in terms:
            if k in t:
                cnt[(k, s, m)] += 1
                if k not in first or d < first[k]:
                    first[k] = d
    with open(os.path.join(DATA, out_name), "w", encoding="utf-8-sig", newline="") as f:
        w = csv.writer(f)
        w.writerow(["term", "sender", "month", "n", "first_date"])
        for (k, s, m), n in sorted(cnt.items(), key=lambda x: (x[0][0], x[0][2], x[0][1])):
            w.writerow([k, s, m, n, first[k]])
    return cnt

nick_cnt = count_terms(NICKNAMES, "text_nickname.csv")
kw_cnt = count_terms(KEYWORDS, "text_keywords.csv")

# ============ 分词 ============
print("中文分词中（约 1–3 分钟）...")
for k in NICKNAMES + KEYWORDS:
    jieba.add_word(k, freq=100000)
jieba.setLogLevel(60)

word_cnt = {"老婆": Counter(), "我": Counter()}
word_q = defaultdict(Counter)          # 季度 -> 词频（双方合并）
for i, (s, m, _, t) in enumerate(docs):
    t = URL_RE.sub(" ", t)
    t = INLINE_EMOJI_RE.sub(" ", t)
    q = quarter_of(m)
    for wd in jieba.lcut(t):
        wd = wd.strip()
        if len(wd) < 2 or wd in STOPWORDS or STOP_RE.match(wd):
            continue
        word_cnt[s][wd] += 1
        word_q[q][wd] += 1
    if (i + 1) % 50000 == 0:
        print(f"  已处理 {i + 1} 条")

# ---------- 4a 双方高频词 ----------
with open(os.path.join(DATA, "text_wordfreq.csv"), "w", encoding="utf-8-sig", newline="") as f:
    w = csv.writer(f)
    w.writerow(["word", "sender", "n"])
    for s in ("老婆", "我"):
        for wd, n in word_cnt[s].most_common(TOP_WORDS):
            w.writerow([wd, s, n])

# ---------- 4b 加权对数优势比（Monroe 等 2008，以双方合并词频为先验） ----------
her, me = word_cnt["老婆"], word_cnt["我"]
n_her, n_me = sum(her.values()), sum(me.values())
comb = her + me
PRIOR_SCALE = 0.1
alpha = {wd: c * PRIOR_SCALE for wd, c in comb.items()}
a0 = sum(alpha.values())
lo = []
for wd, c in comb.items():
    if c < MIN_WORD_TOTAL:
        continue
    a = alpha[wd]
    yh, ym = her[wd], me[wd]
    d = (math.log((yh + a) / (n_her + a0 - yh - a))
         - math.log((ym + a) / (n_me + a0 - ym - a)))
    z = d / math.sqrt(1 / (yh + a) + 1 / (ym + a))
    lo.append((wd, yh, ym, z))
lo.sort(key=lambda x: x[3])
pick = lo if len(lo) <= 2 * TOP_LOGODDS else lo[:TOP_LOGODDS] + lo[-TOP_LOGODDS:]   # 最负 = 我更常说；最正 = 她更常说
with open(os.path.join(DATA, "text_logodds.csv"), "w", encoding="utf-8-sig", newline="") as f:
    w = csv.writer(f)
    w.writerow(["word", "n_her", "n_me", "z", "side"])
    for wd, yh, ym, z in pick:
        w.writerow([wd, yh, ym, round(z, 3), "老婆" if z > 0 else "我"])

# ---------- 称呼候选词（供检查） ----------
nick_hint = re.compile(r"宝|猪|崽|乖|亲爱|老公|老婆|媳妇|呆|傻|笨|哥|妹|喵|狗|兔|熊|仔|公主|小可爱")
cand = [(wd, c, her[wd], me[wd]) for wd, c in comb.most_common(5000) if nick_hint.search(wd)][:60]
with open(os.path.join(DATA, "text_nickname_candidates.csv"), "w", encoding="utf-8-sig", newline="") as f:
    w = csv.writer(f)
    w.writerow(["word", "n_total", "n_her", "n_me"])
    w.writerows(cand)

# ---------- 4g 每季度新出现的高频词 ----------
quarters = sorted(word_q)
seen = Counter()
new_rows = []
for qi, q in enumerate(quarters):
    if qi >= 2:              # 前两个季度（含只有 1 天的 2023Q1）几乎所有词都是“新词”，跳过
        cands = [(wd, c) for wd, c in word_q[q].items()
                 if c >= NEW_MIN_COUNT and seen[wd] <= NEW_MAX_BEFORE]
        cands.sort(key=lambda x: -x[1])
        for rank, (wd, c) in enumerate(cands[:NEW_TOP], 1):
            total_after = sum(word_q[qq][wd] for qq in quarters[qi:])
            new_rows.append((q, rank, wd, c, total_after))
    seen.update(word_q[q])
with open(os.path.join(DATA, "text_newwords.csv"), "w", encoding="utf-8-sig", newline="") as f:
    w = csv.writer(f)
    w.writerow(["quarter", "rank", "word", "n_quarter", "n_since"])
    w.writerows(new_rows)

# ============ 4d 表情包排行 + 图片 ============
print("整理表情包图片 ...")
st_total = Counter(md5 for _, _, _, md5 in stickers)
st_by = Counter((md5, s) for s, _, _, md5 in stickers)
st_first = {}
for _, _, d, md5 in stickers:
    if md5 not in st_first or d < st_first[md5]:
        st_first[md5] = d

# 在 media 文件夹里按文件名中的 MD5 建立索引
md5_path = {}
if os.path.isdir(MEDIA):
    for root, _, files in os.walk(MEDIA):
        for fn in files:
            for h in MD5_RE.findall(fn.lower()):
                md5_path.setdefault(h, os.path.join(root, fn))

try:
    from PIL import Image
except ImportError:
    Image = None
    print("  未安装 pillow，表情包图片将不会被转换（pip install pillow）")

if os.path.isdir(IMG_OUT):
    shutil.rmtree(IMG_OUT)
os.makedirs(IMG_OUT, exist_ok=True)
st_rows = []
for rank, (md5, n) in enumerate(st_total.most_common(TOP_STICKERS), 1):
    img_rel, status = "", "图片缺失"
    src = md5_path.get(md5)
    if src and Image is not None:
        try:
            im = Image.open(src)
            im.seek(0)                                   # 动图取第一帧
            im = im.convert("RGBA")
            im.thumbnail((200, 200))
            out = os.path.join(IMG_OUT, f"{rank:02d}_{md5[:8]}.png")
            im.save(out)
            img_rel, status = os.path.relpath(out, DATA).replace("\\", "/"), "已找到"
        except Exception as e:
            status = f"无法读取({type(e).__name__})"
    st_rows.append((rank, md5, n, st_by[(md5, "老婆")], st_by[(md5, "我")], st_first[md5], img_rel, status))
with open(os.path.join(DATA, "text_sticker_top.csv"), "w", encoding="utf-8-sig", newline="") as f:
    w = csv.writer(f)
    w.writerow(["rank", "md5", "n", "n_her", "n_me", "first_date", "img", "status"])
    w.writerows(st_rows)

# ============ 摘要（不含聊天正文） ============
print("\n========== 摘要 ==========")
print(f"分词后有效词数: 老婆 {n_her}，我 {n_me}；不同词语 {len(comb)} 个")
print(f"小黄脸表情种类: {len(set(e for e, _ in emoji_cnt))}")
print(f"表情包种类: {len(st_total)}；前 {TOP_STICKERS} 名图片情况: {Counter(r[7] for r in st_rows)}")
print(f"media 中可识别 MD5 的文件数: {len(md5_path)}")
print("称呼命中次数: " + ", ".join(f"{k}:{sum(v for (kk, _, _), v in nick_cnt.items() if kk == k)}" for k in NICKNAMES))
print("关键词命中次数: " + ", ".join(f"{k}:{sum(v for (kk, _, _), v in kw_cnt.items() if kk == k)}" for k in KEYWORDS))
print(f"新词表: {len(new_rows)} 行，覆盖 {len(set(r[0] for r in new_rows))} 个季度")
print(f"\n完成！文件已保存到: {DATA}")
