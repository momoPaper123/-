#Figure 1：三年半的全景：我们聊了多少###############
#加载R包
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(RColorBrewer)
library(MetBrewer)
library(patchwork)
library(extrafont)
library(openxlsx)
#注册Windows系统字体
loadfonts(device = "win", quiet = TRUE)
windowsFonts(Arial = windowsFont("Arial"), SimSun = windowsFont("SimSun"))
if (requireNamespace("showtext", quietly = TRUE)) showtext::showtext_auto(FALSE)
#设置路径
base_dir <- "C:/Users/Administrator/Desktop/LP可视化"
data_dir <- file.path(base_dir, "data")
fig_dir <- file.path(base_dir, "Figure1")
dir.create(fig_dir, showWarnings = FALSE)
#读取清洗后的消息数据，先全部按字符读入再转换类型
msg <- readr::read_csv(file.path(data_dir, "messages_clean.csv"), col_types = readr::cols(.default = "c"))
#检查读取行数与解析问题数（应为267563行、0个问题）
cat("读取行数:", nrow(msg), "  解析问题数:", nrow(readr::problems(msg)), "\n")
msg <- msg %>% dplyr::mutate(ts = as.numeric(ts), date = as.Date(date), chat_day = as.Date(chat_day), year = as.integer(year), hour = as.integer(hour), weekday = as.integer(weekday), char_len = as.numeric(char_len))
#读取关键日期，并确保婚礼日期为2025-10-07
events <- readr::read_csv(file.path(data_dir, "events.csv"), col_types = readr::cols(.default = "c")) %>% dplyr::mutate(date = as.Date(date))
events$date[events$label == "婚礼"] <- as.Date("2025-10-07")
#配色：老婆用玫红色，我用蓝色，总体用紫色（MetBrewer Klimt）
kl <- as.character(MetBrewer::met.brewer("Klimt"))
col_sender <- c("老婆" = kl[2], "我" = kl[5])
col_total <- kl[6]
#统一绘图主题：L型边框、无网格线、宋体、标题居中
theme_lp <- function() {
  theme_classic(base_size = 9) +
    theme(text = element_text(family = "SimSun", colour = "black"),
          plot.title = element_text(hjust = 0.5, size = 10),
          axis.line = element_line(colour = "black", linewidth = 0.4),
          axis.ticks = element_line(colour = "black", linewidth = 0.4),
          axis.text = element_text(colour = "black"),
          panel.grid = element_blank(),
          legend.background = element_blank(),
          legend.key = element_blank(),
          legend.title = element_blank())
}
##1a 累计消息曲线####
#每日消息量并计算累计值（单位：万条）
daily <- msg %>% dplyr::count(date, name = "n") %>% tidyr::complete(date = seq(min(date), max(date), by = "day"), fill = list(n = 0)) %>% dplyr::arrange(date) %>% dplyr::mutate(cum = cumsum(n) / 1e4)
#里程碑日期：相识、领证、婚礼（领证与婚礼相隔较近，标签分别放在竖线左右两侧）
ms <- events %>% dplyr::filter(kind == "milestone") %>% dplyr::arrange(date)
ms_hjust <- c(0, 1, 0)
ms_nudge <- c(8, -8, 8)
#第10万条和第20万条消息的达成日期
k_marks <- data.frame(k = c(10, 20))
k_marks$date <- as.Date(sapply(k_marks$k, function(k) as.character(daily$date[which(daily$cum >= k)[1]])))
#绘制累计曲线
y_top <- max(daily$cum) * 1.15
p1a <- ggplot(daily, aes(x = date, y = cum)) +
  geom_area(fill = col_total, alpha = 0.15) +
  geom_line(colour = col_total, linewidth = 0.8) +
  scale_x_date(date_breaks = "6 months", date_labels = "%Y-%m", expand = expansion(mult = c(0.01, 0.01))) +
  scale_y_continuous(limits = c(0, y_top), expand = expansion(mult = c(0, 0))) +
  labs(title = "累计消息数", x = NULL, y = "累计消息数（万条）") +
  theme_lp()
#循环添加里程碑竖线和标签（使用annotate避免重影）
for (i in seq_len(nrow(ms))) {
  p1a <- p1a +
    annotate("segment", x = ms$date[i], xend = ms$date[i], y = 0, yend = y_top * 0.86, linetype = "dashed", colour = "grey40", linewidth = 0.4) +
    annotate("text", x = ms$date[i] + ms_nudge[i], y = y_top, label = paste0(ms$label[i], "\n", format(ms$date[i], "%Y.%m.%d")), family = "SimSun", size = 2.8, hjust = ms_hjust[i], vjust = 1, lineheight = 0.9)
}
#循环添加10万、20万条节点
for (i in seq_len(nrow(k_marks))) {
  p1a <- p1a +
    annotate("point", x = k_marks$date[i], y = k_marks$k[i], colour = col_total, size = 1.8) +
    annotate("text", x = k_marks$date[i], y = k_marks$k[i], label = paste0("第", k_marks$k[i], "万条  ", format(k_marks$date[i], "%Y.%m.%d")), family = "SimSun", size = 2.6, hjust = -0.08, vjust = 1.4)
}
##1b 每日消息日历热力图####
#按聊天日统计（凌晨5点前算前一天）
first_day <- min(msg$chat_day)
last_day <- max(msg$chat_day)
cal <- msg %>% dplyr::count(chat_day, name = "n") %>% tidyr::complete(chat_day = seq(first_day, last_day, by = "day"), fill = list(n = 0)) %>% dplyr::mutate(year = as.integer(format(chat_day, "%Y")), week = as.integer(format(chat_day, "%W")), wd = as.integer(format(chat_day, "%u")), n_plot = ifelse(n == 0, NA, n))
#横轴月份刻度：取每月1日所在周
month_breaks <- as.integer(format(as.Date(paste0("2025-", 1:12, "-01")), "%W"))
#绘制日历热力图（没聊天的日子为浅灰色）
p1b <- ggplot(cal, aes(x = week, y = wd, fill = n_plot)) +
  geom_tile(colour = "white", linewidth = 0.25) +
  facet_wrap(~year, ncol = 1, strip.position = "left") +
  scale_x_continuous(breaks = month_breaks, labels = paste0(1:12, "月"), limits = c(-0.5, 53.5), expand = c(0, 0)) +
  scale_y_reverse(breaks = c(1, 3, 5, 7), labels = c("周一", "周三", "周五", "周日"), expand = c(0, 0)) +
  scale_fill_gradientn(colours = RColorBrewer::brewer.pal(9, "RdPu")[2:9], trans = "log10", na.value = "#EDEDED", breaks = c(1, 10, 100, 1000), labels = c("1", "10", "100", "1000"), name = "每日消息数") +
  coord_fixed() +
  labs(title = "每日消息日历", x = NULL, y = NULL) +
  theme_lp() +
  theme(axis.line = element_blank(),
        axis.ticks = element_blank(),
        strip.background = element_blank(),
        strip.placement = "outside",
        strip.text.y.left = element_text(angle = 0, family = "SimSun", size = 9),
        legend.title = element_text(family = "SimSun", size = 8),
        legend.key.height = unit(3, "mm"),
        legend.key.width = unit(10, "mm"),
        legend.position = "bottom")
##1c 月度消息量（按发送人堆叠）####
monthly <- msg %>% dplyr::mutate(month = as.Date(paste0(format(date, "%Y-%m"), "-01"))) %>% dplyr::count(month, sender, name = "n")
monthly$sender <- factor(monthly$sender, levels = c("老婆", "我"))
p1c <- ggplot(monthly, aes(x = month, y = n / 1000, fill = sender)) +
  geom_col(width = 25, colour = NA) +
  scale_fill_manual(values = col_sender) +
  scale_x_date(date_breaks = "6 months", date_labels = "%Y-%m", expand = expansion(mult = c(0.01, 0.01))) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(title = "月度消息量", x = NULL, y = "消息数（千条）") +
  theme_lp() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
        legend.position = "inside",
        legend.position.inside = c(0.88, 0.85),
        legend.key.size = unit(3, "mm"))
##1d 非文字消息类型构成（按季度）####
type_levels <- c("图片", "表情包", "语音", "视频", "通话", "链接", "红包转账", "其他")
type_q <- msg %>% dplyr::filter(type != "文字") %>% dplyr::mutate(type2 = dplyr::case_when(type %in% c("红包", "转账") ~ "红包转账", type %in% type_levels ~ type, TRUE ~ "其他"), quarter = paste0(format(date, "%y"), "Q", (as.integer(format(date, "%m")) - 1) %/% 3 + 1)) %>% dplyr::count(quarter, type2, name = "n") %>% dplyr::group_by(quarter) %>% dplyr::mutate(pct = n / sum(n) * 100) %>% dplyr::ungroup()
type_q$type2 <- factor(type_q$type2, levels = rev(type_levels))
type_q$quarter <- factor(type_q$quarter, levels = sort(unique(type_q$quarter)))
#类型配色（MetBrewer Cross），“其他”用灰色
cr <- as.character(MetBrewer::met.brewer("Cross"))
col_type <- setNames(c(cr[2], cr[4], cr[5], cr[6], cr[7], cr[8], cr[1], "#BDBDBD"), type_levels)
p1d <- ggplot(type_q, aes(x = quarter, y = pct, fill = type2)) +
  geom_col(width = 0.8, colour = NA) +
  scale_fill_manual(values = col_type, breaks = type_levels) +
  scale_y_continuous(expand = expansion(mult = c(0, 0))) +
  labs(title = "非文字消息类型构成", x = NULL, y = "占比（%）") +
  theme_lp() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5),
        legend.key.size = unit(3, "mm"),
        legend.position = "right")
##1e 关键数字面板####
#每个聊天日的消息数与单日最高
day_n <- msg %>% dplyr::count(chat_day, name = "n")
top_day <- day_n %>% dplyr::slice_max(n, n = 1, with_ties = FALSE)
#最长连续聊天天数
chat_days <- sort(unique(msg$chat_day))
runs <- data.frame(d = chat_days, id = cumsum(c(1, diff(chat_days) != 1))) %>% dplyr::group_by(id) %>% dplyr::summarise(start = min(d), end = max(d), len = dplyr::n(), .groups = "drop") %>% dplyr::slice_max(len, n = 1, with_ties = FALSE)
#时间跨度、第一条消息、总字数
span_days <- as.numeric(last_day - first_day) + 1
first_msg <- msg %>% dplyr::arrange(ts) %>% dplyr::slice(1)
total_chars <- sum(msg$char_len, na.rm = TRUE)
#整理关键数字
kpi <- data.frame(
  metric = c("总消息数", "聊天天数", "最长连续聊天", "单日最高", "文字总量", "第一条消息"),
  value = c(format(nrow(msg), big.mark = ","), paste0(length(chat_days), " 天"), paste0(runs$len, " 天"), paste0(format(top_day$n, big.mark = ","), " 条"), paste0(round(total_chars / 1e4, 1), " 万字"), substr(first_msg$datetime, 12, 16)),
  note = c(paste0("老婆 ", format(sum(msg$sender == "老婆"), big.mark = ","), " · 我 ", format(sum(msg$sender == "我"), big.mark = ",")), paste0("占全部 ", span_days, " 天的 ", round(length(chat_days) / span_days * 100, 1), "%"), paste0(format(runs$start, "%Y.%m.%d"), "–", format(runs$end, "%Y.%m.%d")), format(top_day$chat_day, "%Y.%m.%d"), paste0("文字消息占 ", round(mean(msg$type == "文字") * 100, 1), "%"), format(first_msg$date, "%Y.%m.%d"))
)
kpi$x <- rep(c(1, 2), 3)
kpi$y <- rep(c(3, 2, 1), each = 2)
#绘制数字面板
p1e <- ggplot(kpi) +
  geom_tile(aes(x = x, y = y), width = 0.94, height = 0.9, fill = "#F7F2F5", colour = NA) +
  geom_text(aes(x = x, y = y + 0.22, label = metric), family = "SimSun", size = 2.8) +
  geom_text(aes(x = x, y = y, label = value), family = "SimSun", size = 4.2, colour = col_total, fontface = "bold") +
  geom_text(aes(x = x, y = y - 0.25, label = note), family = "SimSun", size = 2.3, colour = "grey30") +
  scale_x_continuous(limits = c(0.5, 2.5), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0.5, 3.5), expand = c(0, 0)) +
  labs(title = "关键数字") +
  theme_void(base_size = 9) +
  theme(text = element_text(family = "SimSun"),
        plot.title = element_text(hjust = 0.5, size = 10))

##Figure 1 拼图与导出
#1b取消正方形格子限制，让日历随拼图宽度拉伸，色条改为竖向放右侧以节省高度
p1b <- p1b +
  coord_cartesian(expand = FALSE) +
  theme(axis.text.y = element_text(size = 6),
        legend.position = "right",
        legend.key.height = unit(10, "mm"),
        legend.key.width = unit(3, "mm"))
#1d缩小季度标签字号，避免重叠
p1d <- p1d + theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6.5))
#1e改为一行6个数字的横向面板，按时间叙事顺序排列：第一条消息、聊天天数、最长连续聊天、总消息数、单日最高、文字总量
kpi_plot <- kpi[c(6, 2, 3, 1, 4, 5), ]
#第一条消息：大字显示日期，小字显示时间
kpi_plot$value[kpi_plot$metric == "第一条消息"] <- format(first_msg$date, "%Y.%m.%d")
kpi_plot$note[kpi_plot$metric == "第一条消息"] <- paste0(substr(first_msg$datetime, 12, 16), " 加为好友")
#总消息数：双方条数分两行显示，避免溢出方块
kpi_plot$note[kpi_plot$metric == "总消息数"] <- paste0("老婆 ", format(sum(msg$sender == "老婆"), big.mark = ","), "我 ", format(sum(msg$sender == "我"), big.mark = ","))
kpi_plot$x <- 1:6
kpi_plot$y <- 1
p1e <- ggplot(kpi_plot) +
  geom_tile(aes(x = x, y = y), width = 0.94, height = 0.9, fill = "#F7F2F5", colour = NA) +
  geom_text(aes(x = x, y = y + 0.28, label = metric), family = "SimSun", size = 2.8) +
  geom_text(aes(x = x, y = y + 0.04, label = value), family = "SimSun", size = 3.8, colour = col_total, fontface = "bold") +
  geom_text(aes(x = x, y = y - 0.16, label = note), family = "SimSun", size = 2.1, colour = "grey30", vjust = 1, lineheight = 0.9) +
  scale_x_continuous(limits = c(0.5, 6.5), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0.5, 1.5), expand = c(0, 0)) +
  labs(title = "关键数字") +
  theme_void(base_size = 9) +
  theme(text = element_text(family = "SimSun"),
        plot.title = element_text(hjust = 0.5, size = 10))
#拼图布局：A通栏、B通栏、C与D等宽并排、E通栏
design <- "AAAAAA
BBBBBB
CCCDDD
EEEEEE"
#拼图布局：A通栏、B通栏、C与D等宽并排、E通栏；free()使B日历不参与左边缘对齐，避免A、C纵轴标题被推开
fig1 <- p1a + free(p1b) + p1c + p1d + p1e +
  plot_layout(design = design, heights = c(1, 1.3, 1, 0.45)) +
  plot_annotation(tag_levels = "A") &
  theme(plot.tag = element_text(family = "Arial", face = "bold", size = 12))
ggsave(file.path(fig_dir, "Figure1.pdf"), fig1, width = 18, height = 18, units = "cm", device = cairo_pdf, family = "SimSun")
ggsave(file.path(fig_dir, "Figure1.png"), fig1, width = 18, height = 18, units = "cm", dpi = 300)
ggsave(file.path(fig_dir, "Figure1.tif"), fig1, width = 18, height = 18, units = "cm", dpi = 300, compression = "lzw")

##Figure 1 写作数据导出xlsx
#月度消息量与累计（1a、1c）
monthly_w <- monthly %>% tidyr::pivot_wider(names_from = sender, values_from = n, values_fill = 0) %>% dplyr::arrange(month) %>% dplyr::mutate(合计 = 老婆 + 我, 累计 = cumsum(合计), 月份 = format(month, "%Y-%m")) %>% dplyr::select(月份, 老婆, 我, 合计, 累计)
#年度聊天日汇总（1b）
year_sum <- day_n %>% dplyr::mutate(年份 = as.integer(format(chat_day, "%Y"))) %>% dplyr::group_by(年份) %>% dplyr::summarise(聊天天数 = dplyr::n(), 消息总数 = sum(n), 日均消息 = round(mean(n), 1), 日消息中位数 = median(n), 单日最高 = max(n), .groups = "drop")
#关键数字与里程碑（1a、1e）
kpi_out <- kpi %>% dplyr::transmute(指标 = metric, 数值 = value, 说明 = note)
ms_out <- rbind(data.frame(节点 = ms$label, 日期 = format(ms$date, "%Y-%m-%d")), data.frame(节点 = paste0("第", k_marks$k, "万条消息"), 日期 = format(k_marks$date, "%Y-%m-%d")))
#非文字消息季度构成（1d）与全部类型总体占比
type_out <- type_q %>% dplyr::mutate(pct = round(pct, 1)) %>% dplyr::select(quarter, type2, pct) %>% tidyr::pivot_wider(names_from = type2, values_from = pct, values_fill = 0) %>% dplyr::rename(季度 = quarter)
type_all <- msg %>% dplyr::count(type, name = "条数") %>% dplyr::arrange(dplyr::desc(条数)) %>% dplyr::mutate(占比 = round(条数 / sum(条数) * 100, 2)) %>% dplyr::rename(消息类型 = type)
#写入xlsx（3个sheet）
wb <- openxlsx::createWorkbook()
openxlsx::addWorksheet(wb, "1a_1c_月度消息")
openxlsx::writeData(wb, "1a_1c_月度消息", monthly_w)
openxlsx::addWorksheet(wb, "1a_1b_1e_年度与关键数字")
openxlsx::writeData(wb, "1a_1b_1e_年度与关键数字", year_sum, startRow = 1)
openxlsx::writeData(wb, "1a_1b_1e_年度与关键数字", kpi_out, startRow = nrow(year_sum) + 4)
openxlsx::writeData(wb, "1a_1b_1e_年度与关键数字", ms_out, startRow = nrow(year_sum) + nrow(kpi_out) + 7)
openxlsx::addWorksheet(wb, "1d_消息类型构成")
openxlsx::writeData(wb, "1d_消息类型构成", type_out, startRow = 1)
openxlsx::writeData(wb, "1d_消息类型构成", type_all, startRow = nrow(type_out) + 4)
openxlsx::saveWorkbook(wb, file.path(fig_dir, "Figure1_data.xlsx"), overwrite = TRUE)
cat("Figure 1 已导出到:", fig_dir, "\n")

#Figure 2：聊天的作息节律###############
#加载R包
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(RColorBrewer)
library(MetBrewer)
library(patchwork)
library(ggridges)
library(extrafont)
library(openxlsx)
#注册Windows系统字体
loadfonts(device = "win", quiet = TRUE)
windowsFonts(Arial = windowsFont("Arial"), SimSun = windowsFont("SimSun"))
if (requireNamespace("showtext", quietly = TRUE)) showtext::showtext_auto(FALSE)
#设置路径
base_dir <- "C:/Users/Administrator/Desktop/LP可视化"
data_dir <- file.path(base_dir, "data")
fig_dir <- file.path(base_dir, "Figure2")
dir.create(fig_dir, showWarnings = FALSE)
#读取清洗后的消息数据
msg <- readr::read_csv(file.path(data_dir, "messages_clean.csv"), col_types = readr::cols(.default = "c"))
cat("读取行数:", nrow(msg), "  解析问题数:", nrow(readr::problems(msg)), "\n")
#类型转换，并计算以早上5点为起点的时刻（5~29）和季度
msg <- msg %>% dplyr::mutate(date = as.Date(date), hour = as.integer(hour), minute = as.integer(minute), weekday = as.integer(weekday), hx = (hour + minute / 60 - 5) %% 24 + 5, quarter = paste0(format(date, "%Y"), "Q", (as.integer(format(date, "%m")) - 1) %/% 3 + 1))
#读取关键日期，并确保婚礼日期为2025-10-07
events <- readr::read_csv(file.path(data_dir, "events.csv"), col_types = readr::cols(.default = "c")) %>% dplyr::mutate(date = as.Date(date))
events$date[events$label == "婚礼"] <- as.Date("2025-10-07")
#配色：老婆玫红色、我蓝色（MetBrewer Klimt）；工作日蓝、周末绿、节假日橙红（MetBrewer Egypt）
kl <- as.character(MetBrewer::met.brewer("Klimt"))
col_sender <- c("老婆" = kl[2], "我" = kl[5])
eg <- as.character(MetBrewer::met.brewer("Egypt"))
col_day <- c("工作日" = eg[2], "周末" = eg[3], "节假日" = eg[1])
#统一绘图主题：L型边框、无网格线、宋体、标题居中
theme_lp <- function() {
  theme_classic(base_size = 9) +
    theme(text = element_text(family = "SimSun", colour = "black"),
          plot.title = element_text(hjust = 0.5, size = 10),
          axis.line = element_line(colour = "black", linewidth = 0.4),
          axis.ticks = element_line(colour = "black", linewidth = 0.4),
          axis.text = element_text(colour = "black"),
          panel.grid = element_blank(),
          legend.background = element_blank(),
          legend.key = element_blank(),
          legend.title = element_blank())
}
#P值标签格式：P<0.0001标"P < 0.0001"，0.0001≤P<0.001保留四位，其余保留三位
fmt_p <- function(p) {
  if (p < 0.0001) {
    'paste(italic(P), " < 0.0001")'
  } else if (p < 0.001) {
    paste0('paste(italic(P), " = ', sprintf("%.4f", p), '")')
  } else {
    paste0('paste(italic(P), " = ', sprintf("%.3f", p), '")')
  }
}
#全部日期范围
span_dates <- seq(min(msg$date), max(msg$date), by = "day")
wd_lab <- c("周一", "周二", "周三", "周四", "周五", "周六", "周日")
##2a 星期×小时热力图####
#每个星期几在时间范围内出现的天数，用于计算平均值
n_wd <- data.frame(weekday = as.integer(format(span_dates, "%u"))) %>% dplyr::count(weekday, name = "n_days")
wh <- msg %>% dplyr::count(weekday, hour, name = "n") %>% tidyr::complete(weekday = 1:7, hour = 0:23, fill = list(n = 0)) %>% dplyr::left_join(n_wd, by = "weekday") %>% dplyr::mutate(avg = n / n_days)
p2a <- ggplot(wh, aes(x = hour, y = factor(weekday, levels = 7:1, labels = rev(wd_lab)), fill = avg)) +
  geom_tile(colour = "white", linewidth = 0.3) +
  scale_fill_gradientn(colours = RColorBrewer::brewer.pal(9, "RdPu"), name = "平均每小时\n消息数") +
  scale_x_continuous(breaks = seq(0, 21, 3), labels = paste0(seq(0, 21, 3), "时"), expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(title = "一周各时段消息热度", x = NULL, y = NULL) +
  theme_lp() +
  theme(legend.title = element_text(family = "SimSun", size = 8),
        legend.key.width = unit(3, "mm"),
        legend.key.height = unit(8, "mm"))
##2b 24小时活跃时钟（双方对比）####
#各自在每个小时发送消息占本人全部消息的比例
hs <- msg %>% dplyr::count(sender, hour, name = "n") %>% tidyr::complete(sender, hour = 0:23, fill = list(n = 0)) %>% dplyr::group_by(sender) %>% dplyr::mutate(pct = n / sum(n) * 100) %>% dplyr::ungroup()
hs$sender <- factor(hs$sender, levels = c("老婆", "我"))
#极坐标起点偏移半小时，使0时位于正上方
p2b <- ggplot(hs, aes(x = hour, y = pct, fill = sender)) +
  geom_col(position = position_dodge(width = 0.85), width = 0.85) +
  coord_polar(start = -pi / 24) +
  scale_x_continuous(limits = c(-0.5, 23.5), breaks = seq(0, 21, 3), labels = paste0(seq(0, 21, 3), "时")) +
  scale_y_continuous(limits = c(0, NA)) +
  scale_fill_manual(values = col_sender) +
  labs(title = "24小时活跃时钟", x = NULL, y = NULL) +
  theme_lp() +
  theme(axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "bottom",
        legend.key.size = unit(3, "mm"))
##2c 作息的季度演变（山脊图）####
#剔除只有1天数据的2023Q1，横轴从早上5点到次日5点
q_levels <- sort(setdiff(unique(msg$quarter), "2023Q1"))
ridge <- msg %>% dplyr::filter(quarter %in% q_levels) %>% dplyr::select(quarter, hx) %>% dplyr::mutate(quarter = factor(quarter, levels = rev(q_levels)))
#越近的季度颜色越深
q_col <- colorRampPalette(RColorBrewer::brewer.pal(9, "PuRd")[3:8])(length(q_levels))
p2c <- ggplot(ridge, aes(x = hx, y = quarter, fill = quarter)) +
  ggridges::geom_density_ridges(scale = 2.2, rel_min_height = 0.005, colour = "grey15", linewidth = 0.25, alpha = 0.9) +
  scale_fill_manual(values = rev(q_col), guide = "none") +
  scale_x_continuous(limits = c(5, 29), breaks = seq(5, 29, 4), labels = paste0(c(5, 9, 13, 17, 21, 1, 5), "时"), expand = c(0, 0)) +
  scale_y_discrete(expand = expansion(add = c(0.2, 2.4))) +
  labs(title = "作息的季度演变", x = NULL, y = NULL) +
  theme_lp()
##2d 深夜（0–5时）消息占比####
#按月、按发送人计算深夜消息占比，剔除只有1天的2023年3月
night <- msg %>% dplyr::mutate(month = as.Date(paste0(format(date, "%Y-%m"), "-01"))) %>% dplyr::filter(month > as.Date("2023-03-01")) %>% dplyr::group_by(month, sender) %>% dplyr::summarise(night_pct = mean(hour < 5) * 100, n = dplyr::n(), .groups = "drop")
night$sender <- factor(night$sender, levels = c("老婆", "我"))
y_top_d <- max(night$night_pct) * 1.25
p2d <- ggplot(night, aes(x = month, y = night_pct, colour = sender)) +
  geom_line(linewidth = 0.6) +
  geom_point(size = 0.9) +
  scale_colour_manual(values = col_sender) +
  scale_x_date(date_breaks = "6 months", date_labels = "%Y-%m", expand = expansion(mult = c(0.01, 0.02))) +
  scale_y_continuous(limits = c(0, y_top_d), expand = c(0, 0)) +
  labs(title = "深夜（0–5时）消息占比", x = NULL, y = "占当月消息比例（%）") +
  theme_lp() +
  theme(legend.position = "inside",
        legend.position.inside = c(0.92, 0.72),
        legend.key.size = unit(3, "mm"))
#循环添加领证、婚礼竖线和标签（使用annotate避免重影）
ms2 <- events %>% dplyr::filter(label %in% c("领证", "婚礼")) %>% dplyr::arrange(date)
ms2_hjust <- c(1, 0)
ms2_nudge <- c(-8, 8)
for (i in seq_len(nrow(ms2))) {
  p2d <- p2d +
    annotate("segment", x = ms2$date[i], xend = ms2$date[i], y = 0, yend = y_top_d * 0.88, linetype = "dashed", colour = "grey40", linewidth = 0.4) +
    annotate("text", x = ms2$date[i] + ms2_nudge[i], y = y_top_d, label = ms2$label[i], family = "SimSun", size = 2.8, hjust = ms2_hjust[i], vjust = 1)
}
##2e 工作日、周末、节假日的分时消息量####
#法定节假日（2023–2026，含2023年末开始的2024年元旦）
hol <- data.frame(
  start = as.Date(c("2023-01-21", "2023-04-05", "2023-04-29", "2023-06-22", "2023-09-29",
                    "2023-12-30", "2024-02-10", "2024-04-04", "2024-05-01", "2024-06-08", "2024-09-15", "2024-10-01",
                    "2025-01-01", "2025-01-28", "2025-04-04", "2025-05-01", "2025-05-31", "2025-10-01",
                    "2026-01-01", "2026-02-15", "2026-04-04", "2026-05-01", "2026-06-19", "2026-09-25", "2026-10-01")),
  end = as.Date(c("2023-01-27", "2023-04-05", "2023-05-03", "2023-06-24", "2023-10-06",
                  "2024-01-01", "2024-02-17", "2024-04-06", "2024-05-05", "2024-06-10", "2024-09-17", "2024-10-07",
                  "2025-01-01", "2025-02-04", "2025-04-06", "2025-05-05", "2025-06-02", "2025-10-08",
                  "2026-01-03", "2026-02-23", "2026-04-06", "2026-05-05", "2026-06-21", "2026-09-27", "2026-10-07"))
)
hol_dates <- do.call(c, lapply(seq_len(nrow(hol)), function(i) seq(hol$start[i], hol$end[i], by = "day")))
#调休上班日（周末但需要上班，算作工作日）
adj_work <- as.Date(c("2023-04-23", "2023-05-06", "2023-06-25", "2023-10-07", "2023-10-08",
                      "2024-02-04", "2024-02-18", "2024-04-07", "2024-04-28", "2024-05-11", "2024-09-14", "2024-09-29", "2024-10-12",
                      "2025-01-26", "2025-02-08", "2025-04-27", "2025-09-28", "2025-10-11",
                      "2026-01-04", "2026-02-14", "2026-02-28", "2026-05-09", "2026-09-20", "2026-10-10"))
#给每一天标注日类型
day_tab <- data.frame(date = span_dates) %>% dplyr::mutate(wd = as.integer(format(date, "%u")), day_type = dplyr::case_when(date %in% hol_dates ~ "节假日", date %in% adj_work ~ "工作日", wd >= 6 ~ "周末", TRUE ~ "工作日"))
day_tab$day_type <- factor(day_tab$day_type, levels = c("工作日", "周末", "节假日"))
n_type <- day_tab %>% dplyr::count(day_type, name = "n_days")
#各日类型每小时平均消息数
prof <- msg %>% dplyr::left_join(day_tab %>% dplyr::select(date, day_type), by = "date") %>% dplyr::count(day_type, hour, name = "n") %>% tidyr::complete(day_type, hour = 0:23, fill = list(n = 0)) %>% dplyr::left_join(n_type, by = "day_type") %>% dplyr::mutate(avg = n / n_days)
p2e <- ggplot(prof, aes(x = hour, y = avg, colour = day_type)) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 1) +
  scale_colour_manual(values = col_day) +
  scale_x_continuous(breaks = seq(0, 21, 3), labels = paste0(seq(0, 21, 3), "时"), expand = expansion(mult = c(0.02, 0.02))) +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
  labs(title = "不同日子的分时消息量", x = NULL, y = "平均每小时消息数") +
  theme_lp() +
  theme(legend.position = "top",
        legend.key.size = unit(3, "mm"))
##2f 工作日、周末、节假日的每日消息量对比####
#每天的消息总数（只含有消息的日子）
daily_type <- msg %>% dplyr::count(date, name = "n") %>% dplyr::left_join(day_tab %>% dplyr::select(date, day_type), by = "date")
#两两比较：Wilcoxon秩和检验，Bonferroni校正
cmp <- list(c("工作日", "周末"), c("周末", "节假日"), c("工作日", "节假日"))
p_raw <- sapply(cmp, function(g) wilcox.test(daily_type$n[daily_type$day_type == g[1]], daily_type$n[daily_type$day_type == g[2]])$p.value)
p_adj <- p.adjust(p_raw, method = "bonferroni")
#P值横线位置（对数坐标）
y_max_f <- max(daily_type$n)
br_y <- y_max_f * c(1.6, 2.6, 4.2)
br_x <- list(c(1, 2), c(2, 3), c(1, 3))
set.seed(2025)
p2f <- ggplot(daily_type, aes(x = day_type, y = n, fill = day_type)) +
  geom_jitter(width = 0.15, size = 0.3, alpha = 0.25, colour = "grey30") +
  geom_boxplot(width = 0.55, outlier.shape = NA, colour = "black", linewidth = 0.4, alpha = 0.75) +
  scale_fill_manual(values = col_day, guide = "none") +
  scale_y_log10(limits = c(1, y_max_f * 6), breaks = c(1, 10, 100, 1000), labels = c("1", "10", "100", "1000"), expand = c(0, 0)) +
  labs(title = "每日消息量对比", x = NULL, y = "每日消息数") +
  theme_lp()
#循环添加P值横线和标签（使用annotate避免重影）
for (i in seq_along(cmp)) {
  p2f <- p2f +
    annotate("segment", x = br_x[[i]][1], xend = br_x[[i]][2], y = br_y[i], yend = br_y[i], linewidth = 0.4) +
    annotate("text", x = mean(br_x[[i]]), y = br_y[i] * 1.12, label = fmt_p(p_adj[i]), parse = TRUE, family = "Arial", size = 2.6, vjust = 0)
}
##Figure 2 拼图与导出
#布局：A热力图与B时钟并排；C山脊图占左侧两行；D、E、F排在右侧；free()使B、C不参与边缘对齐
design <- "AAAABB
CCDDDD
CCEEFF"
fig2 <- p2a + free(p2b) + free(p2c) + p2d + p2e + p2f +
  plot_layout(design = design, heights = c(1, 1, 1.1)) +
  plot_annotation(tag_levels = "A") &
  theme(plot.tag = element_text(family = "Arial", face = "bold", size = 12))
ggsave(file.path(fig_dir, "Figure2.pdf"), fig2, width = 210, height = 210, units = "mm", device = cairo_pdf, family = "SimSun")
ggsave(file.path(fig_dir, "Figure2.png"), fig2, width = 210, height = 210, units = "mm", dpi = 300)
ggsave(file.path(fig_dir, "Figure2.tif"), fig2, width = 210, height = 210, units = "mm", dpi = 300, compression = "lzw")
##Figure 2 写作数据导出xlsx
#星期×小时平均消息数（2a）
wh_out <- wh %>% dplyr::mutate(星期 = factor(wd_lab[weekday], levels = wd_lab), 小时 = paste0(hour, "时"), 平均消息数 = round(avg, 1)) %>% dplyr::select(星期, 小时, 平均消息数) %>% tidyr::pivot_wider(names_from = 小时, values_from = 平均消息数) %>% dplyr::arrange(星期)
#双方各小时消息占比（2b）
hs_out <- hs %>% dplyr::mutate(pct = round(pct, 2)) %>% dplyr::select(hour, sender, pct) %>% tidyr::pivot_wider(names_from = sender, values_from = pct) %>% dplyr::rename(小时 = hour)
#各季度中位时刻与深夜占比（2c）
q_out <- msg %>% dplyr::filter(quarter %in% q_levels) %>% dplyr::group_by(quarter) %>% dplyr::summarise(消息数 = dplyr::n(), med = median(hx), 深夜占比 = round(mean(hour < 5) * 100, 1), .groups = "drop") %>% dplyr::mutate(中位时刻 = sprintf("%02d:%02d", floor(med %% 24), floor((med %% 1) * 60))) %>% dplyr::select(季度 = quarter, 消息数, 中位时刻, 深夜占比)
#每月双方深夜消息占比（2d）
night_out <- night %>% dplyr::mutate(月份 = format(month, "%Y-%m"), night_pct = round(night_pct, 2)) %>% dplyr::select(月份, sender, night_pct) %>% tidyr::pivot_wider(names_from = sender, values_from = night_pct)
#各日类型每小时平均消息数（2e）
prof_out <- prof %>% dplyr::mutate(avg = round(avg, 1)) %>% dplyr::select(hour, day_type, avg) %>% tidyr::pivot_wider(names_from = day_type, values_from = avg) %>% dplyr::rename(小时 = hour)
#各日类型每日消息量汇总与P值（2f）
type_sum <- daily_type %>% dplyr::group_by(day_type) %>% dplyr::summarise(天数 = dplyr::n(), 中位数 = median(n), 下四分位 = quantile(n, 0.25), 上四分位 = quantile(n, 0.75), 均值 = round(mean(n), 1), .groups = "drop") %>% dplyr::rename(日类型 = day_type)
p_out <- data.frame(比较 = sapply(cmp, paste, collapse = " vs "), 原始P值 = signif(p_raw, 4), Bonferroni校正P值 = signif(p_adj, 4))
#写入xlsx（3个sheet）
wb <- openxlsx::createWorkbook()
openxlsx::addWorksheet(wb, "2a_星期小时热度")
openxlsx::writeData(wb, "2a_星期小时热度", wh_out)
openxlsx::addWorksheet(wb, "2b_2c_2d_时段与深夜")
openxlsx::writeData(wb, "2b_2c_2d_时段与深夜", hs_out, startRow = 1)
openxlsx::writeData(wb, "2b_2c_2d_时段与深夜", q_out, startRow = nrow(hs_out) + 4)
openxlsx::writeData(wb, "2b_2c_2d_时段与深夜", night_out, startRow = nrow(hs_out) + nrow(q_out) + 7)
openxlsx::addWorksheet(wb, "2e_2f_日类型对比")
openxlsx::writeData(wb, "2e_2f_日类型对比", prof_out, startRow = 1)
openxlsx::writeData(wb, "2e_2f_日类型对比", type_sum, startRow = nrow(prof_out) + 4)
openxlsx::writeData(wb, "2e_2f_日类型对比", p_out, startRow = nrow(prof_out) + nrow(type_sum) + 7)
openxlsx::saveWorkbook(wb, file.path(fig_dir, "Figure2_data.xlsx"), overwrite = TRUE)
cat("Figure 2 已导出到:", fig_dir, "\n")

#Figure 3：互动模式：谁先开口，谁回得快###############
#加载R包
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(RColorBrewer)
library(MetBrewer)
library(patchwork)
library(extrafont)
library(openxlsx)
#注册Windows系统字体
loadfonts(device = "win", quiet = TRUE)
windowsFonts(Arial = windowsFont("Arial"), SimSun = windowsFont("SimSun"))
if (requireNamespace("showtext", quietly = TRUE)) showtext::showtext_auto(FALSE)
#设置路径
base_dir <- "C:/Users/Administrator/Desktop/LP可视化"
data_dir <- file.path(base_dir, "data")
fig_dir <- file.path(base_dir, "Figure3")
dir.create(fig_dir, showWarnings = FALSE)
#读取清洗后的消息数据
msg <- readr::read_csv(file.path(data_dir, "messages_clean.csv"), col_types = readr::cols(.default = "c"))
cat("读取行数:", nrow(msg), "  解析问题数:", nrow(readr::problems(msg)), "\n")
#类型转换，按时间排序，计算以早上5点为起点的时刻（5~29）和季度
msg <- msg %>% dplyr::mutate(idx = as.numeric(idx), ts = as.numeric(ts), date = as.Date(date), chat_day = as.Date(chat_day), hour = as.integer(hour), minute = as.integer(minute), char_len = as.numeric(char_len), reply_sec = as.numeric(reply_sec), session_id = as.integer(session_id), session_start = as.integer(session_start), turn_id = as.integer(turn_id), hx = (hour + minute / 60 - 5) %% 24 + 5, quarter = paste0(format(date, "%Y"), "Q", (as.integer(format(date, "%m")) - 1) %/% 3 + 1)) %>% dplyr::arrange(ts, idx)
#读取关键日期，并确保婚礼日期为2025-10-07
events <- readr::read_csv(file.path(data_dir, "events.csv"), col_types = readr::cols(.default = "c")) %>% dplyr::mutate(date = as.Date(date))
events$date[events$label == "婚礼"] <- as.Date("2025-10-07")
ms2 <- events %>% dplyr::filter(label %in% c("领证", "婚礼")) %>% dplyr::arrange(date)
#配色：老婆玫红色、我蓝色（MetBrewer Klimt）
kl <- as.character(MetBrewer::met.brewer("Klimt"))
col_sender <- c("老婆" = kl[2], "我" = kl[5])
#统一绘图主题：L型边框、无网格线、宋体、标题居中
theme_lp <- function() {
  theme_classic(base_size = 9) +
    theme(text = element_text(family = "SimSun", colour = "black"),
          plot.title = element_text(hjust = 0.5, size = 10),
          axis.line = element_line(colour = "black", linewidth = 0.4),
          axis.ticks = element_line(colour = "black", linewidth = 0.4),
          axis.text = element_text(colour = "black"),
          panel.grid = element_blank(),
          legend.background = element_blank(),
          legend.key = element_blank(),
          legend.title = element_blank())
}
#P值标签格式：P<0.0001标"P < 0.0001"，0.0001≤P<0.001保留四位，其余保留三位
fmt_p <- function(p) {
  if (p < 0.0001) {
    'paste(italic(P), " < 0.0001")'
  } else if (p < 0.001) {
    paste0('paste(italic(P), " = ', sprintf("%.4f", p), '")')
  } else {
    paste0('paste(italic(P), " = ', sprintf("%.3f", p), '")')
  }
}
#把5~29的时刻转换为“时:分”
hx2clock <- function(h) sprintf("%02d:%02d", floor(h %% 24), floor((h %% 1) * 60))
##3a 谁先发起对话（按季度）####
#每次对话（沉默超过3小时后）第一条消息的发送人即为发起人
sess <- msg %>% dplyr::group_by(session_id) %>% dplyr::summarise(initiator = sender[1], start = min(ts), end = max(ts), n_msg = dplyr::n(), n_turn = dplyr::n_distinct(turn_id), quarter = quarter[1], .groups = "drop") %>% dplyr::mutate(dur_min = (end - start) / 60)
#剔除只有1天数据的2023Q1
init_q <- sess %>% dplyr::filter(quarter != "2023Q1") %>% dplyr::count(quarter, initiator, name = "n") %>% dplyr::group_by(quarter) %>% dplyr::mutate(pct = n / sum(n) * 100) %>% dplyr::ungroup()
init_q$initiator <- factor(init_q$initiator, levels = c("我", "老婆"))
p3a <- ggplot(init_q, aes(x = quarter, y = pct, fill = initiator)) +
  geom_col(width = 0.8, colour = NA) +
  geom_hline(yintercept = 50, linetype = "dashed", colour = "grey20", linewidth = 0.4) +
  scale_fill_manual(values = col_sender, breaks = c("老婆", "我")) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(title = "谁先开口", x = NULL, y = "发起对话占比（%）") +
  theme_lp() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
        legend.position = "top",
        legend.key.size = unit(3, "mm"))
##3b 回复间隔分布（双方对比）####
#同一次对话内、发送人切换时的间隔即为回复间隔；加1秒避免0秒无法取对数
rep <- msg %>% dplyr::filter(!is.na(reply_sec)) %>% dplyr::mutate(reply1 = reply_sec + 1)
rep$sender <- factor(rep$sender, levels = c("老婆", "我"))
p_reply <- wilcox.test(reply_sec ~ sender, data = rep)$p.value
y_br_b <- 3 * 3600 * 2.2
p3b <- ggplot(rep, aes(x = sender, y = reply1, fill = sender)) +
  geom_violin(colour = NA, alpha = 0.6, scale = "width", width = 0.8) +
  geom_boxplot(width = 0.15, outlier.shape = NA, colour = "black", linewidth = 0.4, fill = "white") +
  scale_fill_manual(values = col_sender, guide = "none") +
  scale_y_log10(limits = c(1, y_br_b * 3), breaks = c(1, 10, 60, 600, 3600, 10800), labels = c("1秒", "10秒", "1分", "10分", "1时", "3时"), expand = c(0, 0)) +
  labs(title = "回复间隔分布", x = NULL, y = "回复间隔") +
  theme_lp() +
  annotate("segment", x = 1, xend = 2, y = y_br_b, yend = y_br_b, linewidth = 0.4) +
  annotate("text", x = 1.5, y = y_br_b * 1.12, label = fmt_p(p_reply), parse = TRUE, family = "Arial", size = 2.6, vjust = 0)
##3c 回复间隔中位数的月度变化####
#剔除只有1天的2023年3月
rep_m <- rep %>% dplyr::mutate(month = as.Date(paste0(format(date, "%Y-%m"), "-01"))) %>% dplyr::filter(month > as.Date("2023-03-01")) %>% dplyr::group_by(month, sender) %>% dplyr::summarise(med = median(reply_sec), n = dplyr::n(), .groups = "drop")
y_top_c <- max(rep_m$med) * 1.25
p3c <- ggplot(rep_m, aes(x = month, y = med, colour = sender)) +
  geom_line(linewidth = 0.6) +
  geom_point(size = 0.9) +
  scale_colour_manual(values = col_sender) +
  scale_x_date(date_breaks = "6 months", date_labels = "%Y-%m", expand = expansion(mult = c(0.01, 0.02))) +
  scale_y_continuous(limits = c(0, y_top_c), expand = c(0, 0)) +
  labs(title = "回复速度的变化", x = NULL, y = "回复间隔中位数（秒）") +
  theme_lp() +
  theme(legend.position = "inside",
        legend.position.inside = c(0.06, 0.8),
        legend.key.size = unit(3, "mm"))
#循环添加领证、婚礼竖线和标签（使用annotate避免重影）
ms2_hjust <- c(1, 0)
ms2_nudge <- c(-8, 8)
for (i in seq_len(nrow(ms2))) {
  p3c <- p3c +
    annotate("segment", x = ms2$date[i], xend = ms2$date[i], y = 0, yend = y_top_c * 0.88, linetype = "dashed", colour = "grey40", linewidth = 0.4) +
    annotate("text", x = ms2$date[i] + ms2_nudge[i], y = y_top_c, label = ms2$label[i], family = "SimSun", size = 2.8, hjust = ms2_hjust[i], vjust = 1)
}
##3d 文字消息长度分布####
len_lab <- c("1字", "2字", "3–5字", "6–10字", "11–20字", "21–50字", ">50字")
txt <- msg %>% dplyr::filter(type == "文字", !is.na(char_len), char_len > 0) %>% dplyr::mutate(len_bin = cut(char_len, breaks = c(0, 1, 2, 5, 10, 20, 50, Inf), labels = len_lab))
len_d <- txt %>% dplyr::count(sender, len_bin, name = "n") %>% tidyr::complete(sender, len_bin, fill = list(n = 0)) %>% dplyr::group_by(sender) %>% dplyr::mutate(pct = n / sum(n) * 100) %>% dplyr::ungroup()
len_d$sender <- factor(len_d$sender, levels = c("老婆", "我"))
#逐区间比较双方占比：两比例卡方检验，7个区间Bonferroni校正
len_tot <- txt %>% dplyr::count(sender, name = "tot")
tot_w <- len_tot$tot[len_tot$sender == "老婆"]
tot_m <- len_tot$tot[len_tot$sender == "我"]
len_p <- len_d %>% dplyr::select(sender, len_bin, n) %>% tidyr::pivot_wider(names_from = sender, values_from = n) %>% dplyr::mutate(p_raw = mapply(function(a, b) prop.test(c(a, b), c(tot_w, tot_m))$p.value, 老婆, 我), p_adj = p.adjust(p_raw, method = "bonferroni"), x = as.integer(len_bin))
#P值横线高度：奇数区间放低排、偶数区间放高排，交错避免相邻标签重叠
y_max_d <- max(len_d$pct)
len_p$y <- ifelse(len_p$x %% 2 == 1, y_max_d * 1.1, y_max_d * 1.3)
p3d <- ggplot(len_d, aes(x = len_bin, y = pct, fill = sender)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.75, colour = NA) +
  scale_fill_manual(values = col_sender) +
  scale_y_continuous(limits = c(0, y_max_d * 1.5), breaks = seq(0, 40, 10), expand = c(0, 0)) +
  labs(title = "消息长度分布", x = "每条文字消息字数", y = "占本人文字消息比例（%）") +
  theme_lp() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
        legend.position = "inside",
        legend.position.inside = c(0.88, 0.5),
        legend.key.size = unit(3, "mm"))
#循环添加每对柱子上方的P值横线和标签（使用annotate避免重影）
for (i in seq_len(nrow(len_p))) {
  p3d <- p3d +
    annotate("segment", x = len_p$x[i] - 0.2, xend = len_p$x[i] + 0.2, y = len_p$y[i], yend = len_p$y[i], linewidth = 0.4) +
    annotate("text", x = len_p$x[i], y = len_p$y[i] + y_max_d * 0.03, label = fmt_p(len_p$p_adj[i]), parse = TRUE, family = "Arial", size = 2.2, vjust = 0)
}
##3e 每次对话的时长与来回轮数####
#时长加1分钟避免0取对数；颜色表示发起人
sess$initiator <- factor(sess$initiator, levels = c("老婆", "我"))
p3e <- ggplot(sess, aes(x = dur_min + 1, y = n_turn, colour = initiator)) +
  geom_point(size = 0.7, alpha = 0.45) +
  scale_colour_manual(values = col_sender) +
  scale_x_log10(breaks = c(1, 10, 60, 600, 1440), labels = c("1分", "10分", "1时", "10时", "24时")) +
  scale_y_log10(breaks = c(1, 10, 100, 1000)) +
  labs(title = "每次对话的时长与轮数", x = "对话持续时长", y = "来回轮数") +
  theme_lp() +
  theme(legend.position = "inside",
        legend.position.inside = c(0.12, 0.85),
        legend.key.size = unit(3, "mm")) +
  guides(colour = guide_legend(override.aes = list(size = 2, alpha = 1)))
##3f 连发行为：一个回合内连续发送的消息条数####
burst_lab <- c("1条", "2条", "3条", "4条", "5条", "6–10条", ">10条")
turns <- msg %>% dplyr::group_by(turn_id) %>% dplyr::summarise(sender = sender[1], n = dplyr::n(), .groups = "drop") %>% dplyr::mutate(burst_bin = cut(n, breaks = c(0, 1, 2, 3, 4, 5, 10, Inf), labels = burst_lab))
burst_d <- turns %>% dplyr::count(sender, burst_bin, name = "n") %>% tidyr::complete(sender, burst_bin, fill = list(n = 0)) %>% dplyr::group_by(sender) %>% dplyr::mutate(pct = n / sum(n) * 100) %>% dplyr::ungroup()
burst_d$sender <- factor(burst_d$sender, levels = c("老婆", "我"))
#逐区间比较双方占比：两比例卡方检验，7个区间Bonferroni校正
turn_tot <- turns %>% dplyr::count(sender, name = "tot")
ttot_w <- turn_tot$tot[turn_tot$sender == "老婆"]
ttot_m <- turn_tot$tot[turn_tot$sender == "我"]
burst_p <- burst_d %>% dplyr::select(sender, burst_bin, n) %>% tidyr::pivot_wider(names_from = sender, values_from = n) %>% dplyr::mutate(p_raw = mapply(function(a, b) prop.test(c(a, b), c(ttot_w, ttot_m))$p.value, 老婆, 我), p_adj = p.adjust(p_raw, method = "bonferroni"), x = as.integer(burst_bin))
#P值横线高度：奇数区间放低排、偶数区间放高排，交错避免相邻标签重叠
y_max_f <- max(burst_d$pct)
burst_p$y <- ifelse(burst_p$x %% 2 == 1, y_max_f * 1.1, y_max_f * 1.3)
p3f <- ggplot(burst_d, aes(x = burst_bin, y = pct, fill = sender)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.75, colour = NA) +
  scale_fill_manual(values = col_sender) +
  scale_y_continuous(limits = c(0, y_max_f * 1.5), expand = c(0, 0)) +
  labs(title = "连发行为", x = "一个回合内连续发送的消息条数", y = "占本人回合数比例（%）") +
  theme_lp() +
  theme(legend.position = "inside",
        legend.position.inside = c(0.88, 0.5),
        legend.key.size = unit(3, "mm"))
#循环添加每对柱子上方的P值横线和标签（使用annotate避免重影）
for (i in seq_len(nrow(burst_p))) {
  p3f <- p3f +
    annotate("segment", x = burst_p$x[i] - 0.2, xend = burst_p$x[i] + 0.2, y = burst_p$y[i], yend = burst_p$y[i], linewidth = 0.4) +
    annotate("text", x = burst_p$x[i], y = burst_p$y[i] + y_max_f * 0.03, label = fmt_p(burst_p$p_adj[i]), parse = TRUE, family = "Arial", size = 2.2, vjust = 0)
}
##3g 谁说晚安：每个聊天日最后一条消息####
last_msg <- msg %>% dplyr::group_by(chat_day) %>% dplyr::slice_max(ts, n = 1, with_ties = FALSE) %>% dplyr::ungroup()
last_share <- last_msg %>% dplyr::count(sender, name = "n") %>% dplyr::mutate(pct = round(n / sum(n) * 100, 1))
last_lab <- setNames(paste0(last_share$sender, "（", last_share$pct, "%）"), last_share$sender)
last_msg$sender <- factor(last_msg$sender, levels = c("老婆", "我"))
p3g <- ggplot(last_msg, aes(x = hx, fill = sender, colour = sender)) +
  geom_density(alpha = 0.35, linewidth = 0.5, adjust = 0.8) +
  scale_fill_manual(values = col_sender, labels = last_lab[c("老婆", "我")]) +
  scale_colour_manual(values = col_sender, labels = last_lab[c("老婆", "我")]) +
  scale_x_continuous(limits = c(5, 29), breaks = seq(5, 29, 4), labels = paste0(c(5, 9, 13, 17, 21, 1, 5), "时"), expand = c(0, 0)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(title = "谁说晚安", x = "当天最后一条消息的时间", y = "密度") +
  theme_lp() +
  theme(legend.position = "inside",
        legend.position.inside = c(0.2, 0.85),
        legend.key.size = unit(3, "mm"))
##Figure 3 拼图与导出
#布局：A与B并排；C通栏；D与E并排；F与G并排
design <- "AAABBB
CCCCCC
DDDEEE
FFFGGG"
fig3 <- p3a + p3b + p3c + p3d + p3e + p3f + p3g +
  plot_layout(design = design, heights = c(1, 0.85, 1, 1)) +
  plot_annotation(tag_levels = "A") &
  theme(plot.tag = element_text(family = "Arial", face = "bold", size = 12))
ggsave(file.path(fig_dir, "Figure3.pdf"), fig3, width = 180, height = 180, units = "mm", device = cairo_pdf, family = "SimSun")
ggsave(file.path(fig_dir, "Figure3.png"), fig3, width = 180, height = 180, units = "mm", dpi = 300)
ggsave(file.path(fig_dir, "Figure3.tif"), fig3, width = 180, height = 180, units = "mm", dpi = 300, compression = "lzw")
##Figure 3 写作数据导出xlsx
#对话发起：按季度（3a）与总体二项检验（检验发起比例是否偏离50%）
init_out <- init_q %>% dplyr::mutate(pct = round(pct, 1)) %>% dplyr::select(quarter, initiator, pct) %>% tidyr::pivot_wider(names_from = initiator, values_from = pct) %>% dplyr::rename(季度 = quarter)
init_all <- sess %>% dplyr::count(initiator, name = "对话数") %>% dplyr::mutate(占比 = round(对话数 / sum(对话数) * 100, 1)) %>% dplyr::rename(发起人 = initiator)
p_init <- binom.test(sum(sess$initiator == "老婆"), nrow(sess), p = 0.5)$p.value
init_all$二项检验P值 <- c(signif(p_init, 4), NA)
#对话时长与轮数（3e）
sess_out <- sess %>% dplyr::group_by(initiator) %>% dplyr::summarise(对话数 = dplyr::n(), 时长中位数_分钟 = round(median(dur_min), 1), 轮数中位数 = median(n_turn), 消息数中位数 = median(n_msg), .groups = "drop") %>% dplyr::rename(发起人 = initiator)
#回复间隔汇总（3b）与月度中位数（3c）
rep_out <- rep %>% dplyr::group_by(sender) %>% dplyr::summarise(回复次数 = dplyr::n(), 中位数_秒 = median(reply_sec), 下四分位_秒 = quantile(reply_sec, 0.25), 上四分位_秒 = quantile(reply_sec, 0.75), 一分钟内回复占比 = round(mean(reply_sec <= 60) * 100, 1), .groups = "drop") %>% dplyr::rename(发送人 = sender)
rep_out$Wilcoxon检验P值 <- c(signif(p_reply, 4), NA)
rep_m_out <- rep_m %>% dplyr::mutate(月份 = format(month, "%Y-%m")) %>% dplyr::select(月份, sender, med) %>% tidyr::pivot_wider(names_from = sender, values_from = med)
#消息长度（3d）：各区间占比与逐区间P值
len_out <- len_d %>% dplyr::mutate(pct = round(pct, 2)) %>% dplyr::select(len_bin, sender, pct) %>% tidyr::pivot_wider(names_from = sender, values_from = pct) %>% dplyr::rename(字数区间 = len_bin)
len_out <- len_out %>% dplyr::left_join(len_p %>% dplyr::transmute(字数区间 = len_bin, 原始P值 = signif(p_raw, 4), Bonferroni校正P值 = signif(p_adj, 4)), by = "字数区间")
#消息长度（3d）：双方字数汇总与整体分布比较（Wilcoxon秩和检验）
p_len <- wilcox.test(char_len ~ sender, data = txt)$p.value
len_sum <- txt %>% dplyr::group_by(sender) %>% dplyr::summarise(文字消息数 = dplyr::n(), 字数中位数 = median(char_len), 字数均值 = round(mean(char_len), 2), 总字数 = sum(char_len), .groups = "drop") %>% dplyr::rename(发送人 = sender)
len_sum$Wilcoxon检验P值 <- c(signif(p_len, 4), NA)
#连发行为（3f）：各区间占比与逐区间P值
burst_out <- burst_d %>% dplyr::mutate(pct = round(pct, 2)) %>% dplyr::select(burst_bin, sender, pct) %>% tidyr::pivot_wider(names_from = sender, values_from = pct) %>% dplyr::rename(连发条数 = burst_bin)
burst_out <- burst_out %>% dplyr::left_join(burst_p %>% dplyr::transmute(连发条数 = burst_bin, 原始P值 = signif(p_raw, 4), Bonferroni校正P值 = signif(p_adj, 4)), by = "连发条数")
#连发行为（3f）：双方回合汇总与整体分布比较（Wilcoxon秩和检验）
p_burst <- wilcox.test(n ~ sender, data = turns)$p.value
burst_sum <- turns %>% dplyr::group_by(sender) %>% dplyr::summarise(回合数 = dplyr::n(), 平均每回合条数 = round(mean(n), 2), 最多连发条数 = max(n), .groups = "drop") %>% dplyr::rename(发送人 = sender)
burst_sum$Wilcoxon检验P值 <- c(signif(p_burst, 4), NA)
#谁说晚安（3g）
last_out <- last_msg %>% dplyr::group_by(sender) %>% dplyr::summarise(天数 = dplyr::n(), 中位时刻 = hx2clock(median(hx)), .groups = "drop") %>% dplyr::mutate(占比 = round(天数 / sum(天数) * 100, 1)) %>% dplyr::rename(发送人 = sender)
p_last <- binom.test(sum(last_msg$sender == "老婆"), nrow(last_msg), p = 0.5)$p.value
last_out$二项检验P值 <- c(signif(p_last, 4), NA)
#写入xlsx（3个sheet）
wb <- openxlsx::createWorkbook()
openxlsx::addWorksheet(wb, "3a_3e_对话发起与会话")
openxlsx::writeData(wb, "3a_3e_对话发起与会话", init_out, startRow = 1)
openxlsx::writeData(wb, "3a_3e_对话发起与会话", init_all, startRow = nrow(init_out) + 4)
openxlsx::writeData(wb, "3a_3e_对话发起与会话", sess_out, startRow = nrow(init_out) + nrow(init_all) + 7)
openxlsx::addWorksheet(wb, "3b_3c_回复间隔")
openxlsx::writeData(wb, "3b_3c_回复间隔", rep_out, startRow = 1)
openxlsx::writeData(wb, "3b_3c_回复间隔", rep_m_out, startRow = nrow(rep_out) + 4)
openxlsx::addWorksheet(wb, "3d_3f_3g_长度连发晚安")
openxlsx::writeData(wb, "3d_3f_3g_长度连发晚安", len_out, startRow = 1)
openxlsx::writeData(wb, "3d_3f_3g_长度连发晚安", len_sum, startRow = nrow(len_out) + 4)
openxlsx::writeData(wb, "3d_3f_3g_长度连发晚安", burst_out, startRow = nrow(len_out) + nrow(len_sum) + 7)
openxlsx::writeData(wb, "3d_3f_3g_长度连发晚安", burst_sum, startRow = nrow(len_out) + nrow(len_sum) + nrow(burst_out) + 10)
openxlsx::writeData(wb, "3d_3f_3g_长度连发晚安", last_out, startRow = nrow(len_out) + nrow(len_sum) + nrow(burst_out) + nrow(burst_sum) + 13)
openxlsx::saveWorkbook(wb, file.path(fig_dir, "Figure3_data.xlsx"), overwrite = TRUE)
cat("Figure 3 已导出到:", fig_dir, "\n")

#Figure 4：我们的语言###############
#加载R包
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(RColorBrewer)
library(MetBrewer)
library(patchwork)
library(ggwordcloud)
library(png)
library(extrafont)
library(openxlsx)
#注册Windows系统字体
loadfonts(device = "win", quiet = TRUE)
windowsFonts(Arial = windowsFont("Arial"), SimSun = windowsFont("SimSun"))
if (requireNamespace("showtext", quietly = TRUE)) showtext::showtext_auto(FALSE)
#设置路径
base_dir <- "C:/Users/Administrator/Desktop/LP可视化"
data_dir <- file.path(base_dir, "data")
fig_dir <- file.path(base_dir, "Figure4")
dir.create(fig_dir, showWarnings = FALSE)
#读取Python提取的文本特征（全部按字符读入再转换）
rd <- function(f) readr::read_csv(file.path(data_dir, f), col_types = readr::cols(.default = "c"))
wf <- rd("text_wordfreq.csv") %>% dplyr::mutate(n = as.numeric(n))
lo <- rd("text_logodds.csv") %>% dplyr::mutate(n_her = as.numeric(n_her), n_me = as.numeric(n_me), z = as.numeric(z))
emo <- rd("text_emoji_inline.csv") %>% dplyr::mutate(n = as.numeric(n))
stk <- rd("text_sticker_top.csv") %>% dplyr::mutate(rank = as.integer(rank), n = as.numeric(n), n_her = as.numeric(n_her), n_me = as.numeric(n_me))
nick <- rd("text_nickname.csv") %>% dplyr::mutate(n = as.numeric(n))
kw <- rd("text_keywords.csv") %>% dplyr::mutate(n = as.numeric(n))
mt <- rd("text_month_totals.csv") %>% dplyr::mutate(n_text = as.numeric(n_text))
nw <- rd("text_newwords.csv") %>% dplyr::mutate(rank = as.integer(rank), n_quarter = as.numeric(n_quarter), n_since = as.numeric(n_since))
#配色：老婆玫红色、我蓝色、总体紫色（MetBrewer Klimt）
kl <- as.character(MetBrewer::met.brewer("Klimt"))
col_sender <- c("老婆" = kl[2], "我" = kl[5])
col_total <- kl[6]
#统一绘图主题：L型边框、无网格线、宋体、标题居中
theme_lp <- function() {
  theme_classic(base_size = 9) +
    theme(text = element_text(family = "SimSun", colour = "black"),
          plot.title = element_text(hjust = 0.5, size = 10),
          axis.line = element_line(colour = "black", linewidth = 0.4),
          axis.ticks = element_line(colour = "black", linewidth = 0.4),
          axis.text = element_text(colour = "black"),
          panel.grid = element_blank(),
          legend.background = element_blank(),
          legend.key = element_blank(),
          legend.title = element_blank())
}
#无坐标轴面板（词云、表情包图墙）使用的主题
theme_blank_lp <- function() {
  theme_void(base_size = 9) +
    theme(text = element_text(family = "SimSun", colour = "black"),
          plot.title = element_text(hjust = 0.5, size = 10),
          legend.position = "none")
}
#月份转季度、季度转季度首日
to_q <- function(month) paste0(substr(month, 1, 4), "Q", (as.integer(substr(month, 6, 7)) - 1) %/% 3 + 1)
q_date <- function(q) as.Date(paste0(substr(q, 1, 4), "-", sprintf("%02d", (as.integer(substr(q, 6, 6)) - 1) * 3 + 1), "-01"))
#季度范围：剔除只有1天数据的2023Q1
q_keep <- setdiff(sort(unique(to_q(mt$month))), "2023Q1")
##4a 双方高频词词云####
wc <- wf %>% dplyr::group_by(sender) %>% dplyr::slice_max(n, n = 80, with_ties = FALSE) %>% dplyr::ungroup()
wc$sender <- factor(wc$sender, levels = c("老婆", "我"))
set.seed(2025)
p4a <- ggplot(wc, aes(label = word, size = n, colour = sender)) +
  ggwordcloud::geom_text_wordcloud_area(family = "SimSun", rm_outside = TRUE, eccentricity = 0.8) +
  scale_size_area(max_size = 8) +
  scale_colour_manual(values = col_sender) +
  facet_wrap(~sender, nrow = 1) +
  labs(title = "双方高频词") +
  theme_blank_lp() +
  theme(strip.text = element_text(family = "SimSun", size = 9))
##4b 专属词对比（加权对数优势比）####
#每方取z值绝对值最大的15个词，正值为老婆更常说，负值为我更常说
lo_p <- lo %>% dplyr::group_by(side) %>% dplyr::slice_max(abs(z), n = 15, with_ties = FALSE) %>% dplyr::ungroup() %>% dplyr::mutate(word = reorder(word, z))
p4b <- ggplot(lo_p, aes(x = z, y = word, fill = side)) +
  geom_col(width = 0.75) +
  geom_vline(xintercept = 0, colour = "black", linewidth = 0.4) +
  scale_fill_manual(values = col_sender, breaks = c("老婆", "我"), labels = c("老婆更常说", "我更常说")) +
  scale_x_continuous(expand = expansion(mult = c(0.05, 0.05))) +
  labs(title = "专属词对比", x = "加权对数优势比（z值）", y = NULL) +
  theme_lp() +
  theme(legend.position = "top",
        legend.key.size = unit(3, "mm"),
        axis.text.y = element_text(size = 7))
##4c 小黄脸表情排行####
#取双方合计使用最多的15个表情
emo_top <- emo %>% dplyr::group_by(emoji) %>% dplyr::summarise(tot = sum(n), .groups = "drop") %>% dplyr::slice_max(tot, n = 15, with_ties = FALSE) %>% dplyr::arrange(dplyr::desc(tot))
emo_p <- emo %>% dplyr::filter(emoji %in% emo_top$emoji) %>% tidyr::complete(emoji, sender, fill = list(n = 0)) %>% dplyr::mutate(emoji = factor(emoji, levels = rev(emo_top$emoji)), sender = factor(sender, levels = c("我", "老婆")))
p4c <- ggplot(emo_p, aes(x = n, y = emoji, fill = sender)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.75) +
  scale_fill_manual(values = col_sender, breaks = c("老婆", "我")) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(title = "小黄脸表情排行", x = "使用次数", y = NULL) +
  theme_lp() +
  theme(legend.position = "inside",
        legend.position.inside = c(0.78, 0.15),
        legend.key.size = unit(3, "mm"),
        axis.text.y = element_text(size = 7))
##4d 最常用的表情包（图墙）####
#4列×3行排列，有图片则贴图，缺失则显示灰色方块
p4d <- ggplot(data.frame(x = c(0.5, 4.5), y = c(0.45, 3.5)), aes(x = x, y = y)) +
  geom_blank() +
  scale_x_continuous(limits = c(0.5, 4.5), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0.45, 3.5), expand = c(0, 0)) +
  coord_fixed() +
  labs(title = "最常用的表情包") +
  theme_blank_lp()
#循环贴图并添加排名和次数（使用annotate与annotation_raster避免重影）
for (i in seq_len(nrow(stk))) {
  cx <- (stk$rank[i] - 1) %% 4 + 1
  cy <- 3 - (stk$rank[i] - 1) %/% 4
  f <- ifelse(is.na(stk$img[i]) || stk$img[i] == "", "", file.path(data_dir, stk$img[i]))
  if (f != "" && file.exists(f)) {
    img <- png::readPNG(f)
    ar <- dim(img)[1] / dim(img)[2]
    hw <- ifelse(ar > 1, 0.36 / ar, 0.36)
    hh <- ifelse(ar > 1, 0.36, 0.36 * ar)
    p4d <- p4d + annotation_raster(img, xmin = cx - hw, xmax = cx + hw, ymin = cy + 0.08 - hh, ymax = cy + 0.08 + hh)
  } else {
    p4d <- p4d +
      annotate("rect", xmin = cx - 0.36, xmax = cx + 0.36, ymin = cy - 0.28, ymax = cy + 0.44, fill = "#EDEDED") +
      annotate("text", x = cx, y = cy + 0.08, label = "图片缺失", family = "SimSun", size = 2.4, colour = "grey45")
  }
  p4d <- p4d + annotate("text", x = cx, y = cy - 0.4, label = paste0("#", stk$rank[i], " · ", format(stk$n[i], big.mark = ","), "次"), family = "SimSun", size = 2.4)
}
##4e 称呼的演变（每千条文字消息中的出现次数）####
#每季度双方文字消息总数
qtot <- mt %>% dplyr::mutate(quarter = to_q(month)) %>% dplyr::group_by(quarter) %>% dplyr::summarise(n_text = sum(n_text), .groups = "drop")
#保留总出现次数≥10的称呼，按总次数排序
nick_tot <- nick %>% dplyr::group_by(term) %>% dplyr::summarise(tot = sum(n), her = sum(n[sender == "老婆"]), me = sum(n[sender == "我"]), first_date = first_date[1], .groups = "drop") %>% dplyr::filter(tot >= 10) %>% dplyr::arrange(dplyr::desc(tot))
nick_q <- nick %>% dplyr::filter(term %in% nick_tot$term) %>% dplyr::mutate(quarter = to_q(month)) %>% dplyr::group_by(term, quarter) %>% dplyr::summarise(n = sum(n), .groups = "drop") %>% tidyr::complete(term = nick_tot$term, quarter = q_keep, fill = list(n = 0)) %>% dplyr::filter(quarter %in% q_keep) %>% dplyr::left_join(qtot, by = "quarter") %>% dplyr::mutate(rate = ifelse(n == 0, NA, n / n_text * 1000), term = factor(term, levels = rev(nick_tot$term)), quarter = factor(quarter, levels = q_keep))
p4e <- ggplot(nick_q, aes(x = quarter, y = term, fill = rate)) +
  geom_tile(colour = "white", linewidth = 0.3) +
  scale_fill_gradientn(colours = RColorBrewer::brewer.pal(9, "RdPu")[2:9], trans = "log10", na.value = "#EDEDED", name = "每千条\n消息") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(title = "称呼的演变", x = NULL, y = NULL) +
  theme_lp() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6.5),
        legend.title = element_text(family = "SimSun", size = 8),
        legend.key.width = unit(3, "mm"),
        legend.key.height = unit(6, "mm"))
##4f 关键词时间线（每千条文字消息中的出现次数）####
kw_levels <- c("早安", "晚安", "想你", "爱你", "抱抱", "亲亲", "谢谢", "哈哈")
qtot_s <- mt %>% dplyr::mutate(quarter = to_q(month)) %>% dplyr::group_by(quarter, sender) %>% dplyr::summarise(n_text = sum(n_text), .groups = "drop")
kw_q <- kw %>% dplyr::mutate(quarter = to_q(month)) %>% dplyr::group_by(term, sender, quarter) %>% dplyr::summarise(n = sum(n), .groups = "drop") %>% tidyr::complete(term = kw_levels, sender = c("老婆", "我"), quarter = q_keep, fill = list(n = 0)) %>% dplyr::filter(quarter %in% q_keep, term %in% kw_levels) %>% dplyr::left_join(qtot_s, by = c("quarter", "sender")) %>% dplyr::mutate(rate = n / n_text * 1000, qd = q_date(quarter), term = factor(term, levels = kw_levels), sender = factor(sender, levels = c("老婆", "我")))
p4f <- ggplot(kw_q, aes(x = qd, y = rate, colour = sender)) +
  geom_line(linewidth = 0.5) +
  geom_point(size = 0.6) +
  facet_wrap(~term, nrow = 2, scales = "free_y") +
  scale_colour_manual(values = col_sender) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.1))) +
  labs(title = "关键词时间线", x = NULL, y = "每千条消息中的次数") +
  theme_lp() +
  theme(strip.background = element_blank(),
        strip.text = element_text(family = "SimSun", size = 8),
        axis.text.x = element_text(size = 6.5),
        legend.position = "top",
        legend.key.size = unit(3, "mm"))
##4g 每季度新出现的高频词####
#字号表示该词在当季的使用次数
nw_p <- nw %>% dplyr::mutate(quarter = factor(quarter, levels = sort(unique(quarter))))
p4g <- ggplot(nw_p, aes(x = quarter, y = -rank, label = word, size = n_quarter)) +
  geom_text(family = "SimSun", colour = col_total) +
  scale_size(range = c(2, 3.2), guide = "none") +
  scale_y_continuous(limits = c(-5.5, -0.5), breaks = NULL) +
  labs(title = "每季度新出现的高频词", x = NULL, y = NULL) +
  theme_lp() +
  theme(axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 7))
##Figure 4 拼图与导出
#布局：A词云与B专属词并排（B占两行）；C表情与D表情包并排；E称呼与F关键词并排；G通栏；free()使B、D不参与边缘对齐
design <- "AAAABB
CCDDBB
EEEFFF
GGGGGG"
fig4 <- p4a + free(p4b) + p4c + free(p4d) + p4e + p4f + p4g +
  plot_layout(design = design, heights = c(1.1, 1, 1.1, 0.75)) +
  plot_annotation(tag_levels = "A") &
  theme(plot.tag = element_text(family = "Arial", face = "bold", size = 12))
ggsave(file.path(fig_dir, "Figure4.pdf"), fig4, width = 200, height = 220, units = "mm", device = cairo_pdf, family = "SimSun")
ggsave(file.path(fig_dir, "Figure4.png"), fig4, width = 200, height = 220, units = "mm", dpi = 300)
ggsave(file.path(fig_dir, "Figure4.tif"), fig4, width = 200, height = 220, units = "mm", dpi = 300, compression = "lzw")
##Figure 4 写作数据导出xlsx
#双方前30高频词（4a）与专属词（4b）
top_her <- wf %>% dplyr::filter(sender == "老婆") %>% dplyr::arrange(dplyr::desc(n)) %>% dplyr::slice(1:30)
top_me <- wf %>% dplyr::filter(sender == "我") %>% dplyr::arrange(dplyr::desc(n)) %>% dplyr::slice(1:30)
word_out <- data.frame(排名 = 1:30, 老婆高频词 = top_her$word, 老婆次数 = top_her$n, 我高频词 = top_me$word, 我次数 = top_me$n)
lo_out <- lo %>% dplyr::arrange(dplyr::desc(z)) %>% dplyr::transmute(词语 = word, 老婆次数 = n_her, 我次数 = n_me, z值 = z, 更常说的一方 = side)
#小黄脸表情前30（4c）与表情包排行（4d）
emo_out <- emo %>% tidyr::pivot_wider(names_from = sender, values_from = n, values_fill = 0) %>% dplyr::mutate(合计 = 老婆 + 我) %>% dplyr::arrange(dplyr::desc(合计)) %>% dplyr::slice(1:30) %>% dplyr::rename(表情 = emoji)
stk_out <- stk %>% dplyr::transmute(排名 = rank, 使用次数 = n, 老婆发送 = n_her, 我发送 = n_me, 首次使用 = first_date, 图片状态 = status, 表情包编号 = md5)
#称呼（4e）、关键词（4f）与新词（4g）
nick_out <- nick_tot %>% dplyr::transmute(称呼 = term, 合计 = tot, 老婆说 = her, 我说 = me, 首次出现 = first_date)
kw_out <- kw %>% dplyr::group_by(term) %>% dplyr::summarise(合计 = sum(n), 老婆说 = sum(n[sender == "老婆"]), 我说 = sum(n[sender == "我"]), 首次出现 = first_date[1], .groups = "drop") %>% dplyr::rename(关键词 = term) %>% dplyr::arrange(dplyr::desc(合计))
nw_out <- nw %>% dplyr::transmute(季度 = quarter, 排名 = rank, 新词 = word, 当季次数 = n_quarter, 此后累计次数 = n_since)
#写入xlsx（3个sheet）
wb <- openxlsx::createWorkbook()
openxlsx::addWorksheet(wb, "4a_4b_高频词与专属词")
openxlsx::writeData(wb, "4a_4b_高频词与专属词", word_out, startRow = 1)
openxlsx::writeData(wb, "4a_4b_高频词与专属词", lo_out, startRow = nrow(word_out) + 4)
openxlsx::addWorksheet(wb, "4c_4d_表情")
openxlsx::writeData(wb, "4c_4d_表情", emo_out, startRow = 1)
openxlsx::writeData(wb, "4c_4d_表情", stk_out, startRow = nrow(emo_out) + 4)
openxlsx::addWorksheet(wb, "4e_4f_4g_称呼关键词新词")
openxlsx::writeData(wb, "4e_4f_4g_称呼关键词新词", nick_out, startRow = 1)
openxlsx::writeData(wb, "4e_4f_4g_称呼关键词新词", kw_out, startRow = nrow(nick_out) + 4)
openxlsx::writeData(wb, "4e_4f_4g_称呼关键词新词", nw_out, startRow = nrow(nick_out) + nrow(kw_out) + 7)
openxlsx::saveWorkbook(wb, file.path(fig_dir, "Figure4_data.xlsx"), overwrite = TRUE)
cat("Figure 4 已导出到:", fig_dir, "\n")

#Figure 5：情绪与重要时刻###############
#加载R包
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(RColorBrewer)
library(MetBrewer)
library(patchwork)
library(extrafont)
library(openxlsx)
#注册Windows系统字体
loadfonts(device = "win", quiet = TRUE)
windowsFonts(Arial = windowsFont("Arial"), SimSun = windowsFont("SimSun"))
if (requireNamespace("showtext", quietly = TRUE)) showtext::showtext_auto(FALSE)
#设置路径
base_dir <- "C:/Users/Administrator/Desktop/LP可视化"
data_dir <- file.path(base_dir, "data")
fig_dir <- file.path(base_dir, "Figure5")
dir.create(fig_dir, showWarnings = FALSE)
#读取情绪分析结果
rd <- function(f) readr::read_csv(file.path(data_dir, f), col_types = readr::cols(.default = "c"))
sm <- rd("senti_monthly.csv") %>% dplyr::mutate(n = as.numeric(n), mean = as.numeric(mean), se = as.numeric(se), pos_share = as.numeric(pos_share), neg_share = as.numeric(neg_share), mdate = as.Date(paste0(month, "-01")))
sr <- rd("senti_rolling.csv") %>% dplyr::mutate(date = as.Date(date), roll = as.numeric(roll))
se_ext <- rd("senti_extremes.csv") %>% dplyr::mutate(rank = as.integer(rank), center = as.Date(center), start = as.Date(start), end = as.Date(end), roll = as.numeric(roll))
se_words <- rd("senti_extreme_words.csv")
#读取清洗后的消息数据（用于红包转账、通话、特殊日子）
msg <- readr::read_csv(file.path(data_dir, "messages_clean.csv"), col_types = readr::cols(.default = "c"))
cat("读取行数:", nrow(msg), "  解析问题数:", nrow(readr::problems(msg)), "\n")
msg <- msg %>% dplyr::mutate(date = as.Date(date), chat_day = as.Date(chat_day), amount = as.numeric(amount), call_sec = as.numeric(call_sec), quarter = paste0(format(date, "%Y"), "Q", (as.integer(format(date, "%m")) - 1) %/% 3 + 1))
#读取关键日期，并确保婚礼日期为2025-10-07
events <- readr::read_csv(file.path(data_dir, "events.csv"), col_types = readr::cols(.default = "c")) %>% dplyr::mutate(date = as.Date(date))
events$date[events$label == "婚礼"] <- as.Date("2025-10-07")
ms2 <- events %>% dplyr::filter(label %in% c("领证", "婚礼")) %>% dplyr::arrange(date)
ms2_hjust <- c(1, 0)
ms2_nudge <- c(-8, 8)
#配色：老婆玫红色、我蓝色（MetBrewer Klimt）
kl <- as.character(MetBrewer::met.brewer("Klimt"))
col_sender <- c("老婆" = kl[2], "我" = kl[5])
col_total <- kl[6]
#统一绘图主题：L型边框、无网格线、宋体、标题居中
theme_lp <- function() {
  theme_classic(base_size = 9) +
    theme(text = element_text(family = "SimSun", colour = "black"),
          plot.title = element_text(hjust = 0.5, size = 10),
          axis.line = element_line(colour = "black", linewidth = 0.4),
          axis.ticks = element_line(colour = "black", linewidth = 0.4),
          axis.text = element_text(colour = "black"),
          panel.grid = element_blank(),
          legend.background = element_blank(),
          legend.key = element_blank(),
          legend.title = element_blank())
}
#给时间序列图添加领证、婚礼竖线和标签（使用annotate避免重影）
add_ms <- function(p, y_top) {
  for (i in seq_len(nrow(ms2))) {
    p <- p +
      annotate("segment", x = ms2$date[i], xend = ms2$date[i], y = -Inf, yend = y_top * 0.9 + (1 - 0.9) * 0, linetype = "dashed", colour = "grey40", linewidth = 0.4) +
      annotate("text", x = ms2$date[i] + ms2_nudge[i], y = y_top, label = ms2$label[i], family = "SimSun", size = 2.8, hjust = ms2_hjust[i], vjust = 1)
  }
  p
}
##5a 情绪曲线（按月，双方分开）####
#剔除只有1天的2023年3月，阴影为均值±标准误
sm_p <- sm %>% dplyr::filter(mdate > as.Date("2023-03-01"))
sm_p$sender <- factor(sm_p$sender, levels = c("老婆", "我"))
y_lo_a <- min(0, min(sm_p$mean - sm_p$se)) - 0.02
y_top_a <- max(sm_p$mean + sm_p$se) * 1.25
p5a <- ggplot(sm_p, aes(x = mdate, y = mean, colour = sender, fill = sender)) +
  geom_hline(yintercept = 0, colour = "grey50", linewidth = 0.3) +
  geom_ribbon(aes(ymin = mean - se, ymax = mean + se), alpha = 0.2, colour = NA) +
  geom_line(linewidth = 0.6) +
  geom_point(size = 0.8) +
  scale_colour_manual(values = col_sender) +
  scale_fill_manual(values = col_sender) +
  scale_x_date(date_breaks = "6 months", date_labels = "%Y-%m", expand = expansion(mult = c(0.01, 0.02))) +
  scale_y_continuous(limits = c(y_lo_a, y_top_a), expand = c(0, 0)) +
  labs(title = "情绪曲线", x = NULL, y = "月平均情绪分数") +
  theme_lp() +
  theme(legend.position = "inside",
        legend.position.inside = c(0.95, 0.75),
        legend.key.size = unit(3, "mm"))
p5a <- add_ms(p5a, y_top_a)
##5b 情绪的高点与低点（双方合并30天滑动平均）####
sr_p <- sr %>% dplyr::filter(!is.na(roll))
y_lo_b <- min(0, min(sr_p$roll)) - 0.02
y_top_b <- max(sr_p$roll) * 1.25
se_ext$lab <- paste0(ifelse(se_ext$kind == "高峰", "高点", "低点"), se_ext$rank)
p5b <- ggplot(sr_p, aes(x = date, y = roll)) +
  geom_hline(yintercept = 0, colour = "grey50", linewidth = 0.3)
#循环添加高点（暖色）与低点（冷色）区间阴影和标签（使用annotate避免重影）
for (i in seq_len(nrow(se_ext))) {
  p5b <- p5b +
    annotate("rect", xmin = se_ext$start[i], xmax = se_ext$end[i], ymin = -Inf, ymax = Inf, fill = ifelse(se_ext$kind[i] == "高峰", "#F4A582", "#92C5DE"), alpha = 0.45) +
    annotate("text", x = se_ext$center[i], y = y_lo_b + 0.012, label = se_ext$lab[i], family = "SimSun", size = 2.5, vjust = 0)
}
p5b <- p5b +
  geom_line(colour = col_total, linewidth = 0.7) +
  scale_x_date(date_breaks = "6 months", date_labels = "%Y-%m", expand = expansion(mult = c(0.01, 0.02))) +
  scale_y_continuous(limits = c(y_lo_b, y_top_b), expand = c(0, 0)) +
  labs(title = "情绪的高点与低点", x = NULL, y = "30天滑动平均情绪分数") +
  theme_lp()
p5b <- add_ms(p5b, y_top_b)
##5c 红包与转账次数（按季度）####
#转账只计“发起转账”，避免与“确认收款”重复计算
money <- msg %>% dplyr::filter(type == "红包" | (type == "转账" & pay_kind == "发起转账"), quarter != "2023Q1") %>% dplyr::mutate(kind = ifelse(type == "红包", "红包", "转账"))
money_q <- money %>% dplyr::count(quarter, kind, name = "n") %>% tidyr::complete(quarter = setdiff(sort(unique(msg$quarter)), "2023Q1"), kind = c("红包", "转账"), fill = list(n = 0))
ju <- as.character(MetBrewer::met.brewer("Juarez"))
col_money <- c("红包" = ju[1], "转账" = ju[3])
p5c <- ggplot(money_q, aes(x = quarter, y = n, fill = kind)) +
  geom_col(width = 0.75, colour = NA) +
  scale_fill_manual(values = col_money) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.08))) +
  labs(title = "红包与转账", x = NULL, y = "次数") +
  theme_lp() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
        legend.position = "inside",
        legend.position.inside = c(0.88, 0.85),
        legend.key.size = unit(3, "mm"))
##5d 每年转账金额（按转出方）####
amt_y <- money %>% dplyr::filter(kind == "转账", !is.na(amount)) %>% dplyr::mutate(year = format(date, "%Y")) %>% dplyr::group_by(year, sender) %>% dplyr::summarise(amt = sum(amount), n = dplyr::n(), .groups = "drop") %>% tidyr::complete(year, sender = c("老婆", "我"), fill = list(amt = 0, n = 0))
amt_y$sender <- factor(amt_y$sender, levels = c("老婆", "我"))
p5d <- ggplot(amt_y, aes(x = year, y = amt, fill = sender)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.75) +
  geom_text(aes(label = ifelse(amt > 0, format(round(amt), big.mark = ","), "")), position = position_dodge(width = 0.8), vjust = -0.4, family = "SimSun", size = 2.3) +
  scale_fill_manual(values = col_sender, labels = c("老婆转给我", "我转给老婆")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(title = "每年转账金额", x = NULL, y = "转账金额（元）") +
  theme_lp() +
  theme(legend.position = "top",
        legend.key.size = unit(3, "mm"))
##5e 通话记录（按月）####
#“其他设备接听”为在手机上接听，电脑端无通话时长；取消、拒绝、无应答合并为“未接通”
call <- msg %>% dplyr::filter(type == "通话") %>% dplyr::mutate(status3 = dplyr::case_when(call_status == "接通" ~ "接通", call_status == "其他设备接听" ~ "其他设备接听", TRUE ~ "未接通"), month = as.Date(paste0(format(date, "%Y-%m"), "-01")))
call_m <- call %>% dplyr::count(month, status3, name = "n")
call_m$status3 <- factor(call_m$status3, levels = c("未接通", "其他设备接听", "接通"))
eg <- as.character(MetBrewer::met.brewer("Egypt"))
col_call <- c("接通" = eg[3], "其他设备接听" = eg[2], "未接通" = "#BDBDBD")
y_top_e <- max(call_m %>% dplyr::group_by(month) %>% dplyr::summarise(t = sum(n)) %>% dplyr::pull(t)) * 1.3
p5e <- ggplot(call_m, aes(x = month, y = n, fill = status3)) +
  geom_col(width = 25, colour = NA) +
  scale_fill_manual(values = col_call, breaks = c("接通", "其他设备接听", "未接通")) +
  scale_x_date(date_breaks = "6 months", date_labels = "%Y-%m", expand = expansion(mult = c(0.01, 0.02))) +
  scale_y_continuous(limits = c(0, y_top_e), expand = c(0, 0)) +
  labs(title = "通话记录", x = NULL, y = "通话次数") +
  theme_lp() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "inside",
        legend.position.inside = c(0.5, 0.82),
        legend.direction = "horizontal",
        legend.key.size = unit(3, "mm"))
p5e <- add_ms(p5e, y_top_e)
##5f 特殊日子与平时的消息量对比####
#特殊日子：生日、节日、相识纪念日、领证、婚礼；只保留已经过去的日子
last_day <- max(msg$chat_day)
first_day <- min(msg$chat_day)
sp <- events %>% dplyr::filter(kind %in% c("birthday", "holiday") | label %in% c("领证", "婚礼")) %>% dplyr::select(date, label)
sp <- rbind(sp, data.frame(date = as.Date(c("2024-03-31", "2025-03-31", "2026-03-31")), label = "相识纪念日"), data.frame(date = as.Date("2026-06-07"), label = "领证纪念日"))
sp <- sp %>% dplyr::filter(date > first_day + 14, date <= last_day) %>% dplyr::mutate(label = dplyr::recode(label, "老婆生日(阳历)" = "生日（阳历）", "老婆生日(农历九月初六)" = "生日（农历）"))
#每个聊天日的消息数（含没聊天的日子）
day_n <- msg %>% dplyr::count(chat_day, name = "n") %>% tidyr::complete(chat_day = seq(first_day, last_day, by = "day"), fill = list(n = 0))
#相对消息量 = 当天消息数 ÷ 前后各14天（不含其他特殊日子）的中位数
sp$n_day <- sapply(sp$date, function(d) day_n$n[day_n$chat_day == d])
sp$baseline <- sapply(sp$date, function(d) median(day_n$n[day_n$chat_day >= d - 14 & day_n$chat_day <= d + 14 & !(day_n$chat_day %in% sp$date)]))
sp <- sp %>% dplyr::filter(baseline > 0) %>% dplyr::mutate(ratio = pmax(n_day, 1) / baseline, year = format(date, "%Y"))
sp_levels <- c("生日（阳历）", "生日（农历）", "相识纪念日", "领证纪念日", "情人节", "520", "七夕", "中秋", "除夕", "领证", "婚礼")
sp$label <- factor(sp$label, levels = rev(sp_levels[sp_levels %in% sp$label]))
vg <- as.character(MetBrewer::met.brewer("VanGogh2"))
col_year <- setNames(vg[c(1, 3, 5, 8)], c("2023", "2024", "2025", "2026"))
p5f <- ggplot(sp, aes(x = ratio, y = label, colour = year)) +
  geom_vline(xintercept = 1, linetype = "dashed", colour = "grey40", linewidth = 0.4) +
  geom_point(size = 2, alpha = 0.9) +
  scale_colour_manual(values = col_year) +
  scale_x_log10(breaks = c(0.25, 0.5, 1, 2, 4), labels = c("0.25", "0.5", "1", "2", "4")) +
  labs(title = "特殊日子的消息量", x = "相对平时的消息量（倍）", y = NULL) +
  theme_lp() +
  theme(legend.position = "right",
        legend.key.size = unit(3, "mm"))
##Figure 5 拼图与导出
#布局：A通栏、B通栏、C与D并排、E与F并排
design <- "AAAAAA
BBBBBB
CCCDDD
EEEFFF"
fig5 <- p5a + p5b + p5c + p5d + p5e + p5f +
  plot_layout(design = design, heights = c(1, 1, 1, 1.1)) +
  plot_annotation(tag_levels = "A") &
  theme(plot.tag = element_text(family = "Arial", face = "bold", size = 12))
ggsave(file.path(fig_dir, "Figure5.pdf"), fig5, width = 180, height = 180, units = "mm", device = cairo_pdf, family = "SimSun")
ggsave(file.path(fig_dir, "Figure5.png"), fig5, width = 180, height = 180, units = "mm", dpi = 300)
ggsave(file.path(fig_dir, "Figure5.tif"), fig5, width = 180, height = 180, units = "mm", dpi = 300, compression = "lzw")
##Figure 5 写作数据导出xlsx
#情绪：月度（5a）、高点低点（5b）及其特征词
sm_out <- sm_p %>% dplyr::mutate(月份 = format(mdate, "%Y-%m")) %>% dplyr::select(月份, sender, mean, se) %>% tidyr::pivot_wider(names_from = sender, values_from = c(mean, se)) %>% dplyr::rename_with(~ gsub("mean_", "均值_", gsub("se_", "标准误_", .x)))
ext_out <- se_ext %>% dplyr::transmute(时期 = lab, 中心日期 = format(center), 开始 = format(start), 结束 = format(end), 滑动平均情绪 = roll)
words_out <- se_words %>% dplyr::mutate(period = gsub("高峰", "高点", gsub("低谷", "低点", period))) %>% dplyr::rename(时期 = period, 特征词 = word, 期间次数 = n_window, 相对全时段倍数 = ratio)
#红包转账（5c、5d）与通话（5e）
money_out <- money_q %>% tidyr::pivot_wider(names_from = kind, values_from = n) %>% dplyr::rename(季度 = quarter)
amt_out <- amt_y %>% dplyr::transmute(年份 = year, 转出方 = sender, 转账次数 = n, 转账金额 = round(amt, 2))
call_out <- call %>% dplyr::group_by(status3) %>% dplyr::summarise(次数 = dplyr::n(), 有时长的次数 = sum(!is.na(call_sec)), 时长中位数_分钟 = round(median(call_sec, na.rm = TRUE) / 60, 1), 总时长_小时 = round(sum(call_sec, na.rm = TRUE) / 3600, 1), .groups = "drop") %>% dplyr::rename(通话状态 = status3)
call_m_out <- call_m %>% dplyr::mutate(月份 = format(month, "%Y-%m")) %>% dplyr::select(月份, status3, n) %>% tidyr::pivot_wider(names_from = status3, values_from = n, values_fill = 0)
#特殊日子（5f）
sp_out <- sp %>% dplyr::arrange(date) %>% dplyr::transmute(日期 = format(date), 特殊日子 = as.character(label), 当天消息数 = n_day, 前后14天中位数 = baseline, 相对倍数 = round(ratio, 2))
#写入xlsx（3个sheet）
wb <- openxlsx::createWorkbook()
openxlsx::addWorksheet(wb, "5a_5b_情绪")
openxlsx::writeData(wb, "5a_5b_情绪", sm_out, startRow = 1)
openxlsx::writeData(wb, "5a_5b_情绪", ext_out, startRow = nrow(sm_out) + 4)
openxlsx::writeData(wb, "5a_5b_情绪", words_out, startRow = nrow(sm_out) + nrow(ext_out) + 7)
openxlsx::addWorksheet(wb, "5c_5d_5e_红包转账通话")
openxlsx::writeData(wb, "5c_5d_5e_红包转账通话", money_out, startRow = 1)
openxlsx::writeData(wb, "5c_5d_5e_红包转账通话", amt_out, startRow = nrow(money_out) + 4)
openxlsx::writeData(wb, "5c_5d_5e_红包转账通话", call_out, startRow = nrow(money_out) + nrow(amt_out) + 7)
openxlsx::writeData(wb, "5c_5d_5e_红包转账通话", call_m_out, startRow = nrow(money_out) + nrow(amt_out) + nrow(call_out) + 10)
openxlsx::addWorksheet(wb, "5f_特殊日子")
openxlsx::writeData(wb, "5f_特殊日子", sp_out)
openxlsx::saveWorkbook(wb, file.path(fig_dir, "Figure5_data.xlsx"), overwrite = TRUE)
cat("Figure 5 已导出到:", fig_dir, "\n")

#Figure 6：我们聊了什么：话题的版图###############
#加载R包
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(RColorBrewer)
library(MetBrewer)
library(patchwork)
library(ggrepel)
library(maps)
library(extrafont)
library(openxlsx)
#注册Windows系统字体
loadfonts(device = "win", quiet = TRUE)
windowsFonts(Arial = windowsFont("Arial"), SimSun = windowsFont("SimSun"))
if (requireNamespace("showtext", quietly = TRUE)) showtext::showtext_auto(FALSE)
#设置路径
base_dir <- "C:/Users/Administrator/Desktop/LP可视化"
data_dir <- file.path(base_dir, "data")
fig_dir <- file.path(base_dir, "Figure6")
dir.create(fig_dir, showWarnings = FALSE)
#读取Python提取的话题与食物特征（全部按字符读入再转换）
rd <- function(f) readr::read_csv(file.path(data_dir, f), col_types = readr::cols(.default = "c"))
tm <- rd("fig6_topic_monthly.csv") %>% dplyr::mutate(n = as.numeric(n), mdate = as.Date(paste0(month, "-01")))
tt <- rd("fig6_topic_month_total.csv") %>% dplyr::mutate(n_text = as.numeric(n_text), n_tagged = as.numeric(n_tagged))
th <- rd("fig6_topic_hour.csv") %>% dplyr::mutate(hour = as.integer(hour), n = as.numeric(n))
tsd <- rd("fig6_topic_sender.csv") %>% dplyr::mutate(n = as.numeric(n))
food <- rd("fig6_food_top.csv") %>% dplyr::mutate(n = as.numeric(n), n_her = as.numeric(n_her), n_me = as.numeric(n_me))
we <- rd("fig6_whateat_monthly.csv") %>% dplyr::mutate(n = as.numeric(n))
#读取消息文字（用于统计各旅行地点在聊天中被提到的次数）
msg <- readr::read_csv(file.path(data_dir, "messages_clean.csv"), col_types = readr::cols(.default = "c"), col_select = c(date, type, text, voice_text))
cat("读取行数:", nrow(msg), "  解析问题数:", nrow(readr::problems(msg)), "\n")
tx <- msg %>% dplyr::mutate(date = as.Date(date), t = dplyr::case_when(type == "文字" ~ text, type == "语音" ~ voice_text, TRUE ~ NA_character_)) %>% dplyr::filter(!is.na(t), t != "") %>% dplyr::select(date, t)
#读取关键日期，并确保婚礼日期为2025-10-07
events <- readr::read_csv(file.path(data_dir, "events.csv"), col_types = readr::cols(.default = "c")) %>% dplyr::mutate(date = as.Date(date))
events$date[events$label == "婚礼"] <- as.Date("2025-10-07")
ms2 <- events %>% dplyr::filter(label %in% c("领证", "婚礼")) %>% dplyr::arrange(date)
ms2_hjust <- c(1, 0)
ms2_nudge <- c(-8, 8)
#配色：老婆玫红色、我蓝色（MetBrewer Klimt）；旅行年份与Figure 5保持一致（MetBrewer VanGogh2）
kl <- as.character(MetBrewer::met.brewer("Klimt"))
col_sender <- c("老婆" = kl[2], "我" = kl[5])
vg <- as.character(MetBrewer::met.brewer("VanGogh2"))
col_year <- c("2024" = vg[3], "2025" = vg[5], "2026" = vg[8])
#统一绘图主题：L型边框、无网格线、宋体、标题居中
theme_lp <- function() {
  theme_classic(base_size = 9) +
    theme(text = element_text(family = "SimSun", colour = "black"),
          plot.title = element_text(hjust = 0.5, size = 10),
          axis.line = element_line(colour = "black", linewidth = 0.4),
          axis.ticks = element_line(colour = "black", linewidth = 0.4),
          axis.text = element_text(colour = "black"),
          panel.grid = element_blank(),
          legend.background = element_blank(),
          legend.key = element_blank(),
          legend.title = element_blank())
}
#月份转季度
to_q <- function(month) paste0(substr(month, 1, 4), "Q", (as.integer(substr(month, 6, 7)) - 1) %/% 3 + 1)
#把5~29的时刻转换为“时:分”
hx2clock <- function(h) sprintf("%02d:%02d", floor(h %% 24), floor((h %% 1) * 60))
##6a 话题的河流（按月话题占比）####
#剔除只有1天的2023年3月；占比 = 某话题命中消息数 ÷ 当月各话题命中次数之和
tp_order <- tm %>% dplyr::group_by(topic) %>% dplyr::summarise(tot = sum(n), .groups = "drop") %>% dplyr::arrange(dplyr::desc(tot)) %>% dplyr::pull(topic)
tm_p <- tm %>% dplyr::filter(mdate > as.Date("2023-03-01")) %>% dplyr::select(mdate, topic, n) %>% tidyr::complete(mdate, topic = tp_order, fill = list(n = 0)) %>% dplyr::group_by(mdate) %>% dplyr::mutate(pct = n / sum(n) * 100) %>% dplyr::ungroup() %>% dplyr::mutate(topic = factor(topic, levels = tp_order))
#11个话题配色：MetBrewer Cross 9色 + Thomas 淡紫色、草绿色
cr <- as.character(MetBrewer::met.brewer("Cross"))
tho <- as.character(MetBrewer::met.brewer("Thomas"))
col_topic <- setNames(c(cr, tho[c(5, 8)])[seq_along(tp_order)], tp_order)
y_top_a <- 112
p6a <- ggplot(tm_p, aes(x = mdate, y = pct, fill = topic)) +
  geom_area(colour = "white", linewidth = 0.15, alpha = 0.95) +
  scale_fill_manual(values = col_topic) +
  scale_x_date(date_breaks = "6 months", date_labels = "%Y-%m", expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, y_top_a), breaks = seq(0, 100, 25), expand = c(0, 0)) +
  labs(title = "话题的河流", x = NULL, y = "话题占比（%）") +
  theme_lp() +
  theme(legend.position = "right",
        legend.key.size = unit(3, "mm")) +
  guides(fill = guide_legend(ncol = 1))
#循环添加领证、婚礼竖线和标签（使用annotate避免重影）
for (i in seq_len(nrow(ms2))) {
  p6a <- p6a +
    annotate("segment", x = ms2$date[i], xend = ms2$date[i], y = 0, yend = 103, linetype = "dashed", colour = "grey20", linewidth = 0.4) +
    annotate("text", x = ms2$date[i] + ms2_nudge[i], y = y_top_a, label = ms2$label[i], family = "SimSun", size = 2.8, hjust = ms2_hjust[i], vjust = 1)
}
##6b 话题的时间表（话题×小时热力图）####
#每个话题在各小时的消息占该话题全部命中消息的比例；话题按平均时刻（以早上5点为起点）由早到晚自上而下排列
th_p <- th %>% dplyr::group_by(topic) %>% dplyr::mutate(pct = n / sum(n) * 100, hx = (hour - 5) %% 24 + 5) %>% dplyr::ungroup()
tp_hour <- th_p %>% dplyr::group_by(topic) %>% dplyr::summarise(mh = sum(hx * n) / sum(n), .groups = "drop") %>% dplyr::arrange(dplyr::desc(mh))
th_p$topic <- factor(th_p$topic, levels = tp_hour$topic)
p6b <- ggplot(th_p, aes(x = hour, y = topic, fill = pct)) +
  geom_tile(colour = "white", linewidth = 0.3) +
  scale_fill_gradientn(colours = RColorBrewer::brewer.pal(9, "RdPu"), name = "占该话题\n比例（%）") +
  scale_x_continuous(breaks = seq(0, 21, 3), labels = paste0(seq(0, 21, 3), "时"), expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(title = "话题的时间表", x = NULL, y = NULL) +
  theme_lp() +
  theme(legend.title = element_text(family = "SimSun", size = 8),
        legend.key.width = unit(3, "mm"),
        legend.key.height = unit(7, "mm"))
##6c 旅行足迹地图####
#旅行地点、时间、所属行程与大致经纬度（景区级位置，精度约0.1度）；kw为在聊天中检索该地点的关键词
spots <- data.frame(
  name = c("洛带金龙长城", "四姑娘山", "丽江古镇", "玉龙雪山", "云杉坪", "蓝月谷", "听花谷", "香格里拉", "虎跳峡", "泸沽湖", "三亚", "折多山", "木雅圣地", "稻城亚丁", "鹧鸪山", "金川梨花节", "雅拉雪山", "墨石公园", "塔公草原", "折多山", "松潘", "红星岩海子", "贵阳", "黔灵山公园", "安顺古镇", "千里江山图", "江油中华洞天", "青海湖", "茶卡盐湖", "托素湖", "大柴旦翡翠湖", "察尔汗盐湖", "水上雅丹", "黑独山胭脂山", "敦煌莫高窟", "鸣沙山月牙泉", "玉门关", "嘉峪关", "大地之子", "无界", "张掖七彩丹霞"),
  kw = c("金龙长城|洛带", "四姑娘", "丽江", "玉龙雪山", "云杉坪", "蓝月谷", "听花谷", "香格里拉", "虎跳峡", "泸沽湖", "三亚", "折多山", "木雅", "稻城|亚丁", "鹧鸪山", "金川|梨花", "雅拉", "墨石", "塔公", "折多山", "松潘", "红星岩", "贵阳", "黔灵山", "安顺", "千里江山", "中华洞天|江油", "青海湖", "茶卡", "托素", "翡翠湖|大柴旦", "察尔汗", "雅丹", "黑独山|胭脂山", "莫高窟", "鸣沙山|月牙泉", "玉门关", "嘉峪关", "大地之子", "无界", "丹霞|张掖"),
  ym = c("2024-05", "2024-05", "2024-05", "2024-06", "2024-06", "2024-06", "2024-06", "2024-06", "2024-06", "2024-06", "2024-06", "2024-09", "2024-09", "2024-09", "2025-03", "2025-03", "2025-03", "2025-03", "2025-03", "2025-03", "2025-08", "2026-01", "2026-05", "2026-05", "2026-05", "2026-05", "2026-07", rep("2026-09", 14)),
  trip = c("2024.05 成都洛带", "2024.05 四姑娘山", rep("2024.05–06 滇西北", 8), "2024.06 三亚", rep("2024.09 川西南", 3), rep("2025.03 川西北", 6), "2025.08 松潘", "2026.01 红星岩", rep("2026.05 贵州", 4), "2026 江油", rep("2026.09 青甘大环线", 14)),
  lon = c(104.33, 102.88, 100.23, 100.18, 100.21, 100.23, 100.24, 99.70, 100.10, 100.78, 109.51, 101.80, 101.58, 100.32, 102.69, 102.07, 101.56, 101.40, 101.53, 101.80, 103.60, 103.50, 106.63, 106.69, 105.95, 105.90, 104.95, 100.18, 99.08, 96.95, 95.20, 95.05, 93.65, 93.30, 94.81, 94.67, 93.86, 98.22, 96.10, 96.15, 100.08),
  lat = c(30.63, 31.05, 26.87, 27.10, 27.14, 27.12, 27.04, 27.83, 27.18, 27.72, 18.25, 30.07, 30.15, 28.42, 31.88, 31.48, 30.38, 30.47, 30.32, 30.07, 32.64, 32.90, 26.65, 26.60, 26.25, 26.10, 31.95, 36.88, 36.70, 37.20, 37.80, 36.80, 38.18, 38.55, 40.04, 40.09, 40.35, 39.80, 40.40, 40.45, 38.97)
)
spots <- spots %>% dplyr::mutate(ord = dplyr::row_number(), year = substr(ym, 1, 4), lab = ifelse(duplicated(name), "", name))
#每个地点在聊天中被提到的消息数与首次提到日期
spots$mention <- sapply(spots$kw, function(k) sum(grepl(k, tx$t, perl = TRUE)))
spots$first_mention <- sapply(spots$kw, function(k) {
  hit <- tx$date[grepl(k, tx$t, perl = TRUE)]
  ifelse(length(hit) == 0, NA, format(min(hit)))
})
#川西放大区域（D图范围），A图中用虚线框标出
box <- c(99.8, 105.4, 28.0, 33.3)
spots$in_box <- spots$lon >= box[1] & spots$lon <= box[2] & spots$lat >= box[3] & spots$lat <= box[4]
#底图：只画陆地、不画国界（陆地与边界线同色），海洋为浅蓝色
world <- ggplot2::map_data("world")
land_fill <- "#EFE9E1"
sea_fill <- "#E3EDF2"
size_lim <- c(0, max(spots$mention))
p6c <- ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group), fill = land_fill, colour = land_fill, linewidth = 0.1) +
  annotate("rect", xmin = box[1], xmax = box[2], ymin = box[3], ymax = box[4], fill = NA, colour = "grey30", linetype = "dashed", linewidth = 0.35) +
  geom_path(data = spots, aes(x = lon, y = lat, group = trip, colour = year), linewidth = 0.4, alpha = 0.7) +
  geom_point(data = spots, aes(x = lon, y = lat, colour = year, size = mention), alpha = 0.9) +
  ggrepel::geom_text_repel(data = spots %>% dplyr::filter(!in_box, lab != ""), aes(x = lon, y = lat, label = lab), family = "SimSun", size = 1.9, segment.size = 0.2, segment.colour = "grey40", min.segment.length = 0, max.overlaps = Inf, box.padding = 0.15, point.padding = 0.1, seed = 2025) +
  scale_colour_manual(values = col_year, name = "旅行年份") +
  scale_size(range = c(1, 4), limits = size_lim, name = "聊天提及次数") +
  scale_x_continuous(breaks = seq(95, 110, 5), labels = paste0(seq(95, 110, 5), "°E")) +
  scale_y_continuous(breaks = seq(20, 40, 5), labels = paste0(seq(20, 40, 5), "°N")) +
  coord_quickmap(xlim = c(91.5, 111.5), ylim = c(17, 41.8), expand = FALSE) +
  labs(title = "旅行足迹", x = NULL, y = NULL) +
  theme_lp() +
  theme(panel.background = element_rect(fill = sea_fill, colour = NA),
        legend.position = "bottom",
        legend.box = "vertical",
        legend.title = element_text(family = "SimSun", size = 8),
        legend.key.size = unit(3, "mm")) +
  guides(colour = guide_legend(order = 1, override.aes = list(size = 2)), size = guide_legend(order = 2))
##6d 川西放大####
p6d <- ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group), fill = land_fill, colour = land_fill, linewidth = 0.1) +
  geom_path(data = spots, aes(x = lon, y = lat, group = trip, colour = year), linewidth = 0.4, alpha = 0.7) +
  geom_point(data = spots, aes(x = lon, y = lat, colour = year, size = mention), alpha = 0.9) +
  ggrepel::geom_text_repel(data = spots %>% dplyr::filter(in_box, lab != ""), aes(x = lon, y = lat, label = lab), family = "SimSun", size = 2.2, segment.size = 0.2, segment.colour = "grey40", min.segment.length = 0, max.overlaps = Inf, box.padding = 0.2, point.padding = 0.1, seed = 2025) +
  scale_colour_manual(values = col_year) +
  scale_size(range = c(1, 4), limits = size_lim) +
  scale_x_continuous(breaks = seq(100, 105, 1), labels = paste0(seq(100, 105, 1), "°E")) +
  scale_y_continuous(breaks = seq(28, 33, 1), labels = paste0(seq(28, 33, 1), "°N")) +
  coord_quickmap(xlim = box[1:2], ylim = box[3:4], expand = FALSE) +
  labs(title = "川西放大", x = NULL, y = NULL) +
  theme_lp() +
  theme(panel.background = element_rect(fill = land_fill, colour = NA),
        axis.text = element_text(size = 6.5),
        legend.position = "none")
##6e 吃货档案：提到最多的食物####
food_top <- food %>% dplyr::slice_max(n, n = 15, with_ties = FALSE)
food_p <- food_top %>% dplyr::select(food, n_her, n_me) %>% tidyr::pivot_longer(c(n_her, n_me), names_to = "sender", values_to = "cnt") %>% dplyr::mutate(sender = factor(ifelse(sender == "n_her", "老婆", "我"), levels = c("我", "老婆")), food = factor(food, levels = rev(food_top$food)))
p6e <- ggplot(food_p, aes(x = cnt, y = food, fill = sender)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.75) +
  scale_fill_manual(values = col_sender, breaks = c("老婆", "我")) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(title = "吃货档案", x = "提到的消息数", y = NULL) +
  theme_lp() +
  theme(legend.position = "inside",
        legend.position.inside = c(0.8, 0.15),
        legend.key.size = unit(3, "mm"),
        axis.text.y = element_text(size = 7))
##6f “吃什么”每季度被问的次数####
we_q <- we %>% dplyr::mutate(quarter = to_q(month)) %>% dplyr::filter(quarter != "2023Q1") %>% dplyr::group_by(quarter, sender) %>% dplyr::summarise(n = sum(n), .groups = "drop")
we_q$sender <- factor(we_q$sender, levels = c("老婆", "我"))
p6f <- ggplot(we_q, aes(x = quarter, y = n, fill = sender)) +
  geom_col(width = 0.75, colour = NA) +
  scale_fill_manual(values = col_sender) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.08))) +
  labs(title = "“吃什么”被问了多少次", x = NULL, y = "次数") +
  theme_lp() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
        legend.position = "inside",
        legend.position.inside = c(0.12, 0.85),
        legend.key.size = unit(3, "mm"))
##Figure 6 拼图与导出
#布局：A通栏；B与D在左、C地图在右占两行；E与F并排；free()使地图不参与边缘对齐
design <- "AAAAAA
BBBCCC
DDDCCC
EEEFFF"
fig6 <- p6a + p6b + free(p6c) + free(p6d) + p6e + p6f +
  plot_layout(design = design, heights = c(0.9, 1, 1.1, 0.95)) +
  plot_annotation(tag_levels = "A") &
  theme(plot.tag = element_text(family = "Arial", face = "bold", size = 12))
ggsave(file.path(fig_dir, "Figure6.pdf"), fig6, width = 200, height = 200, units = "mm", device = cairo_pdf, family = "SimSun")
ggsave(file.path(fig_dir, "Figure6.png"), fig6, width = 200, height = 200, units = "mm", dpi = 300)
ggsave(file.path(fig_dir, "Figure6.tif"), fig6, width = 200, height = 200, units = "mm", dpi = 300, compression = "lzw")
##Figure 6 写作数据导出xlsx
#话题：每月占比（6a）、每月覆盖率、各话题汇总与时段（6b）
tm_out <- tm_p %>% dplyr::mutate(月份 = format(mdate, "%Y-%m"), pct = round(pct, 1)) %>% dplyr::select(月份, topic, pct) %>% tidyr::pivot_wider(names_from = topic, values_from = pct)
cov_out <- tt %>% dplyr::mutate(覆盖率 = round(n_tagged / n_text * 100, 1)) %>% dplyr::rename(月份 = month, 文字消息数 = n_text, 命中话题消息数 = n_tagged)
peak <- th %>% dplyr::group_by(topic) %>% dplyr::slice_max(n, n = 1, with_ties = FALSE) %>% dplyr::ungroup() %>% dplyr::transmute(topic, 高峰时段 = paste0(hour, "时"))
tp_out <- tm %>% dplyr::group_by(topic) %>% dplyr::summarise(命中消息数 = sum(n), .groups = "drop") %>% dplyr::mutate(占全部命中比例 = round(命中消息数 / sum(命中消息数) * 100, 1)) %>% dplyr::left_join(tsd %>% tidyr::pivot_wider(names_from = sender, values_from = n), by = "topic") %>% dplyr::left_join(peak, by = "topic") %>% dplyr::left_join(tp_hour %>% dplyr::transmute(topic, 平均时刻 = hx2clock(mh)), by = "topic") %>% dplyr::arrange(dplyr::desc(命中消息数)) %>% dplyr::rename(话题 = topic)
#旅行足迹（6c、6d）
spot_out <- spots %>% dplyr::transmute(序号 = ord, 行程 = trip, 年月 = ym, 地点 = name, 经度 = lon, 纬度 = lat, 检索关键词 = kw, 聊天提及次数 = mention, 首次提及日期 = first_mention)
#食物前30（6e）与“吃什么”（6f）
food_out <- food %>% dplyr::slice_max(n, n = 30, with_ties = FALSE) %>% dplyr::rename(食物 = food, 合计 = n, 老婆提到 = n_her, 我提到 = n_me, 首次提到 = first_date)
we_out <- we_q %>% tidyr::pivot_wider(names_from = sender, values_from = n, values_fill = 0) %>% dplyr::rename(季度 = quarter)
we_tot <- we %>% dplyr::group_by(sender) %>% dplyr::summarise(合计 = sum(n), .groups = "drop") %>% dplyr::rename(提问人 = sender)
#写入xlsx（3个sheet）
wb <- openxlsx::createWorkbook()
openxlsx::addWorksheet(wb, "6a_6b_话题")
openxlsx::writeData(wb, "6a_6b_话题", tp_out, startRow = 1)
openxlsx::writeData(wb, "6a_6b_话题", tm_out, startRow = nrow(tp_out) + 4)
openxlsx::writeData(wb, "6a_6b_话题", cov_out, startRow = nrow(tp_out) + nrow(tm_out) + 7)
openxlsx::addWorksheet(wb, "6c_6d_旅行足迹")
openxlsx::writeData(wb, "6c_6d_旅行足迹", spot_out)
openxlsx::addWorksheet(wb, "6e_6f_吃")
openxlsx::writeData(wb, "6e_6f_吃", food_out, startRow = 1)
openxlsx::writeData(wb, "6e_6f_吃", we_out, startRow = nrow(food_out) + 4)
openxlsx::writeData(wb, "6e_6f_吃", we_tot, startRow = nrow(food_out) + nrow(we_out) + 7)
openxlsx::saveWorkbook(wb, file.path(fig_dir, "Figure6_data.xlsx"), overwrite = TRUE)
cat("Figure 6 已导出到:", fig_dir, "\n")

#Figure 7：同步与相互影响###############
#加载R包
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(RColorBrewer)
library(MetBrewer)
library(patchwork)
library(extrafont)
library(openxlsx)
#注册Windows系统字体
loadfonts(device = "win", quiet = TRUE)
windowsFonts(Arial = windowsFont("Arial"), SimSun = windowsFont("SimSun"))
if (requireNamespace("showtext", quietly = TRUE)) showtext::showtext_auto(FALSE)
#设置路径
base_dir <- "C:/Users/Administrator/Desktop/LP可视化"
data_dir <- file.path(base_dir, "data")
fig_dir <- file.path(base_dir, "Figure7")
dir.create(fig_dir, showWarnings = FALSE)
#读取Python提取的特征（全部按字符读入再转换）
rd <- function(f) readr::read_csv(file.path(data_dir, f), col_types = readr::cols(.default = "c"))
sim <- rd("fig7_similarity_quarterly.csv") %>% dplyr::mutate(n_tok_her = as.numeric(n_tok_her), n_tok_me = as.numeric(n_tok_me), cosine = as.numeric(cosine), cosine_shuffled = as.numeric(cosine_shuffled), cosine_shuffled_sd = as.numeric(cosine_shuffled_sd))
cont <- rd("fig7_contagion.csv") %>% dplyr::mutate(origin_date = as.Date(origin_date), adopt_date = as.Date(adopt_date), lag_days = as.numeric(lag_days), n_origin_before = as.numeric(n_origin_before), n_adopter_after = as.numeric(n_adopter_after), n_total = as.numeric(n_total))
lsm <- rd("fig7_lsm_monthly.csv") %>% dplyr::mutate(pct_her = as.numeric(pct_her), pct_me = as.numeric(pct_me), lsm = as.numeric(lsm), mdate = as.Date(paste0(month, "-01")))
tp <- rd("fig7_turn_pairs.csv") %>% dplyr::mutate(reply_sec = as.numeric(reply_sec), resp_msgs = as.numeric(resp_msgs), resp_chars = as.numeric(resp_chars), trigger_senti = as.numeric(trigger_senti), resp_senti = as.numeric(resp_senti))
#读取关键日期，并确保婚礼日期为2025-10-07
events <- readr::read_csv(file.path(data_dir, "events.csv"), col_types = readr::cols(.default = "c")) %>% dplyr::mutate(date = as.Date(date))
events$date[events$label == "婚礼"] <- as.Date("2025-10-07")
ms2 <- events %>% dplyr::filter(label %in% c("领证", "婚礼")) %>% dplyr::arrange(date)
ms2_hjust <- c(1, 0)
ms2_nudge <- c(-8, 8)
#配色：老婆玫红色、我蓝色、总体紫色（MetBrewer Klimt）；情绪：消极蓝、中性灰、积极橙红（RColorBrewer RdYlBu）
kl <- as.character(MetBrewer::met.brewer("Klimt"))
col_sender <- c("老婆" = kl[2], "我" = kl[5])
col_total <- kl[6]
ryb <- RColorBrewer::brewer.pal(11, "RdYlBu")
col_trig <- c("对方消极" = ryb[10], "对方非消极" = ryb[4])
col_emo <- c("积极" = ryb[3], "中性" = "#D9D9D9", "消极" = ryb[10])
#统一绘图主题：L型边框、无网格线、宋体、标题居中
theme_lp <- function() {
  theme_classic(base_size = 9) +
    theme(text = element_text(family = "SimSun", colour = "black"),
          plot.title = element_text(hjust = 0.5, size = 10),
          axis.line = element_line(colour = "black", linewidth = 0.4),
          axis.ticks = element_line(colour = "black", linewidth = 0.4),
          axis.text = element_text(colour = "black"),
          panel.grid = element_blank(),
          legend.background = element_blank(),
          legend.key = element_blank(),
          legend.title = element_blank())
}
#P值标签格式：P<0.0001标"P < 0.0001"，0.0001≤P<0.001保留四位，其余保留三位
fmt_p <- function(p) {
  if (p < 0.0001) {
    'paste(italic(P), " < 0.0001")'
  } else if (p < 0.001) {
    paste0('paste(italic(P), " = ', sprintf("%.4f", p), '")')
  } else {
    paste0('paste(italic(P), " = ', sprintf("%.3f", p), '")')
  }
}
#季度转季度首日
q_date <- function(q) as.Date(paste0(substr(q, 1, 4), "-", sprintf("%02d", (as.integer(substr(q, 6, 6)) - 1) * 3 + 1), "-01"))
#给时间序列图添加领证、婚礼竖线和标签（使用annotate避免重影）
add_ms <- function(p, y_lo, y_top) {
  for (i in seq_len(nrow(ms2))) {
    p <- p +
      annotate("segment", x = ms2$date[i], xend = ms2$date[i], y = y_lo, yend = y_lo + (y_top - y_lo) * 0.9, linetype = "dashed", colour = "grey40", linewidth = 0.4) +
      annotate("text", x = ms2$date[i] + ms2_nudge[i], y = y_top, label = ms2$label[i], family = "SimSun", size = 2.8, hjust = ms2_hjust[i], vjust = 1)
  }
  p
}
##7a 用词相似度（每季度）####
#剔除双方词数均不足2000的季度（2023Q1只有1天、2026Q4只有半个月）；参照线为随机打乱发送人后的相似度（阴影为±1.96倍标准差）
sim_p <- sim %>% dplyr::filter(n_tok_her >= 2000, n_tok_me >= 2000) %>% dplyr::mutate(qd = q_date(quarter))
sim_l <- rbind(data.frame(qd = sim_p$qd, v = sim_p$cosine, grp = "两人实际"), data.frame(qd = sim_p$qd, v = sim_p$cosine_shuffled, grp = "随机打乱参照"))
sim_l$grp <- factor(sim_l$grp, levels = c("两人实际", "随机打乱参照"))
y_lo_a <- floor(min(sim_p$cosine) * 10) / 10 - 0.05
y_top_a <- 1.08
p7a <- ggplot(sim_l, aes(x = qd, y = v, colour = grp, linetype = grp)) +
  geom_ribbon(data = sim_p, aes(x = qd, ymin = cosine_shuffled - 1.96 * cosine_shuffled_sd, ymax = cosine_shuffled + 1.96 * cosine_shuffled_sd), inherit.aes = FALSE, fill = "grey70", alpha = 0.4) +
  geom_line(linewidth = 0.6) +
  geom_point(size = 1) +
  scale_colour_manual(values = c("两人实际" = col_total, "随机打乱参照" = "grey45")) +
  scale_linetype_manual(values = c("两人实际" = "solid", "随机打乱参照" = "dashed")) +
  scale_x_date(date_breaks = "6 months", date_labels = "%Y-%m", expand = expansion(mult = c(0.02, 0.04))) +
  scale_y_continuous(limits = c(y_lo_a, y_top_a), breaks = seq(0.4, 1, 0.1), expand = c(0, 0)) +
  labs(title = "用词相似度", x = NULL, y = "余弦相似度") +
  theme_lp() +
  theme(legend.position = "inside",
        legend.position.inside = c(0.2, 0.2),
        legend.key.width = unit(6, "mm"),
        legend.key.height = unit(3, "mm"))
p7a <- add_ms(p7a, y_lo_a, y_top_a)
##7b 口头禅传染####
#排除因话题出现而非口头禅的词（工作、理财、食物、出行等），可根据Figure7_data.xlsx继续补充
topic_exclude <- c("收益", "黄金", "银行", "单子", "车子", "支付", "生意", "客户", "资金", "预计", "米线", "高速", "股票", "拉肚子", "周一", "接收", "最低", "亏了")
cont_f <- cont %>% dplyr::filter(!token %in% topic_exclude)
cont_top <- cont_f %>% dplyr::slice_max(n_adopter_after, n = 20, with_ties = FALSE) %>% dplyr::arrange(dplyr::desc(origin_date))
cont_top$token <- factor(cont_top$token, levels = cont_top$token)
cont_pt <- rbind(data.frame(token = cont_top$token, date = cont_top$origin_date, who = cont_top$origin, role = "传出方开始使用"), data.frame(token = cont_top$token, date = cont_top$adopt_date, who = cont_top$adopter, role = "接收方开始使用"))
cont_pt$who <- factor(cont_pt$who, levels = c("老婆", "我"))
cont_pt$role <- factor(cont_pt$role, levels = c("传出方开始使用", "接收方开始使用"))
x_max_b <- max(cont_top$adopt_date) + 150
p7b <- ggplot() +
  geom_segment(data = cont_top, aes(x = origin_date, xend = adopt_date, y = token, yend = token), colour = "grey60", linewidth = 0.5) +
  geom_point(data = cont_pt, aes(x = date, y = token, colour = who, shape = role), size = 1.8) +
  geom_text(data = cont_top, aes(x = adopt_date, y = token, label = paste0(n_adopter_after, "次")), family = "SimSun", size = 2.2, hjust = -0.35) +
  scale_colour_manual(values = col_sender) +
  scale_shape_manual(values = c("传出方开始使用" = 16, "接收方开始使用" = 17)) +
  scale_x_date(date_breaks = "6 months", date_labels = "%Y-%m", limits = c(min(cont_top$origin_date) - 30, x_max_b)) +
  labs(title = "口头禅传染", x = NULL, y = NULL) +
  theme_lp() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5),
        axis.text.y = element_text(size = 7),
        legend.position = "top",
        legend.box = "vertical",
        legend.spacing.y = unit(0, "mm"),
        legend.key.size = unit(3, "mm")) +
  guides(colour = guide_legend(order = 1), shape = guide_legend(order = 2))
##7c 语言风格匹配（LSM，按月）####
lsm_o <- lsm %>% dplyr::filter(category == "总体", mdate > as.Date("2023-03-01"))
y_lo_c <- floor(min(lsm_o$lsm) * 20) / 20 - 0.02
y_top_c <- 1.0
p7c <- ggplot(lsm_o, aes(x = mdate, y = lsm)) +
  geom_line(colour = col_total, linewidth = 0.6) +
  geom_point(colour = col_total, size = 1) +
  scale_x_date(date_breaks = "6 months", date_labels = "%Y-%m", expand = expansion(mult = c(0.02, 0.04))) +
  scale_y_continuous(limits = c(y_lo_c, y_top_c), expand = c(0, 0)) +
  labs(title = "语言风格匹配", x = NULL, y = "LSM") +
  theme_lp()
p7c <- add_ms(p7c, y_lo_c, y_top_c)
##7d 对方情绪消极时的回复间隔####
#以回合为单位：对方回合情绪分<−0.2为消极，其余为非消极；回复间隔加1秒避免0秒无法取对数
tp2 <- tp %>% dplyr::filter(trigger_class != "") %>% dplyr::mutate(trig2 = factor(ifelse(trigger_class == "消极", "对方消极", "对方非消极"), levels = c("对方消极", "对方非消极")), resp_lab = factor(ifelse(responder == "我", "我回应老婆", "老婆回应我"), levels = c("我回应老婆", "老婆回应我")), reply1 = reply_sec + 1)
#每位回应者内部比较：Wilcoxon秩和检验，2次比较Bonferroni校正
resp_levels <- levels(tp2$resp_lab)
p_d_raw <- sapply(resp_levels, function(r) wilcox.test(tp2$reply_sec[tp2$resp_lab == r & tp2$trig2 == "对方消极"], tp2$reply_sec[tp2$resp_lab == r & tp2$trig2 == "对方非消极"])$p.value)
p_d <- p.adjust(p_d_raw, method = "bonferroni")
y_br_d <- 10800 * 2.2
p7d <- ggplot(tp2, aes(x = resp_lab, y = reply1, fill = trig2)) +
  geom_violin(position = position_dodge(width = 0.8), scale = "width", width = 0.75, colour = NA, alpha = 0.6) +
  geom_boxplot(aes(group = interaction(resp_lab, trig2)), position = position_dodge(width = 0.8), width = 0.15, outlier.shape = NA, colour = "black", fill = "white", linewidth = 0.4) +
  scale_fill_manual(values = col_trig) +
  scale_y_log10(limits = c(1, y_br_d * 3), breaks = c(1, 10, 60, 600, 3600, 10800), labels = c("1秒", "10秒", "1分", "10分", "1时", "3时"), expand = c(0, 0)) +
  labs(title = "对方情绪低落时的回复速度", x = NULL, y = "回复间隔") +
  theme_lp() +
  theme(legend.position = "top",
        legend.key.size = unit(3, "mm"))
#循环添加P值横线和标签（使用annotate避免重影）
for (i in seq_along(resp_levels)) {
  p7d <- p7d +
    annotate("segment", x = i - 0.2, xend = i + 0.2, y = y_br_d, yend = y_br_d, linewidth = 0.4) +
    annotate("text", x = i, y = y_br_d * 1.12, label = fmt_p(p_d[i]), parse = TRUE, family = "Arial", size = 2.6, vjust = 0)
}
##7e 对方情绪不同时，回应的情绪构成####
tp3 <- tp2 %>% dplyr::filter(resp_class != "") %>% dplyr::mutate(trigger_class = factor(trigger_class, levels = c("消极", "中性", "积极")), resp_class = factor(resp_class, levels = c("积极", "中性", "消极")))
emo_d <- tp3 %>% dplyr::count(resp_lab, trigger_class, resp_class, name = "n") %>% dplyr::group_by(resp_lab, trigger_class) %>% dplyr::mutate(pct = n / sum(n) * 100) %>% dplyr::ungroup()
#每位回应者：对方情绪类别×回应情绪类别的卡方检验，2次比较Bonferroni校正
p_e_raw <- sapply(resp_levels, function(r) chisq.test(table(tp3$trigger_class[tp3$resp_lab == r], tp3$resp_class[tp3$resp_lab == r]))$p.value)
p_e <- p.adjust(p_e_raw, method = "bonferroni")
p_df_e <- data.frame(resp_lab = factor(resp_levels, levels = resp_levels), x = 1, xend = 3, y = 104, lab = sapply(p_e, fmt_p))
p7e <- ggplot(emo_d, aes(x = trigger_class, y = pct, fill = resp_class)) +
  geom_col(width = 0.7, colour = NA) +
  geom_segment(data = p_df_e, aes(x = x, xend = xend, y = y, yend = y), inherit.aes = FALSE, linewidth = 0.4) +
  geom_text(data = p_df_e, aes(x = 2, y = y + 2, label = lab), inherit.aes = FALSE, parse = TRUE, family = "Arial", size = 2.6, vjust = 0) +
  facet_wrap(~resp_lab, nrow = 1) +
  scale_fill_manual(values = col_emo, labels = c("回应积极", "回应中性", "回应消极")) +
  scale_y_continuous(limits = c(0, 115), breaks = seq(0, 100, 25), expand = c(0, 0)) +
  labs(title = "回应的情绪", x = "对方回合的情绪", y = "占比（%）") +
  theme_lp() +
  theme(strip.background = element_blank(),
        strip.text = element_text(family = "SimSun", size = 9),
        legend.position = "right",
        legend.key.size = unit(3, "mm"))
##Figure 7 拼图与导出
#布局：A与C在左、B在右占两行；D与E并排
design <- "AAABBB
CCCBBB
DDDEEE"
fig7 <- p7a + p7b + p7c + p7d + p7e +
  plot_layout(design = design, heights = c(1, 1, 1.1)) +
  plot_annotation(tag_levels = "A") &
  theme(plot.tag = element_text(family = "Arial", face = "bold", size = 12))
ggsave(file.path(fig_dir, "Figure7.pdf"), fig7, width = 200, height = 190, units = "mm", device = cairo_pdf, family = "SimSun")
ggsave(file.path(fig_dir, "Figure7.png"), fig7, width = 200, height = 190, units = "mm", dpi = 300)
ggsave(file.path(fig_dir, "Figure7.tif"), fig7, width = 200, height = 190, units = "mm", dpi = 300, compression = "lzw")
##Figure 7 写作数据导出xlsx
#用词相似度（7a）与LSM（7c），并用Spearman秩相关检验随时间的趋势
p_sim <- cor.test(seq_len(nrow(sim_p)), sim_p$cosine, method = "spearman", exact = FALSE)
p_lsm <- cor.test(seq_len(nrow(lsm_o)), lsm_o$lsm, method = "spearman", exact = FALSE)
sim_out <- sim_p %>% dplyr::transmute(季度 = quarter, 老婆词数 = n_tok_her, 我词数 = n_tok_me, 实际相似度 = cosine, 随机参照 = cosine_shuffled, 参照标准差 = cosine_shuffled_sd)
lsm_out <- lsm %>% dplyr::filter(mdate > as.Date("2023-03-01")) %>% dplyr::select(month, category, lsm) %>% tidyr::pivot_wider(names_from = category, values_from = lsm) %>% dplyr::rename(月份 = month)
trend_out <- data.frame(指标 = c("用词相似度（季度）", "LSM（月度）"), Spearman_rho = round(c(p_sim$estimate, p_lsm$estimate), 3), P值 = signif(c(p_sim$p.value, p_lsm$p.value), 4))
#口头禅传染（7b）：保留排除后的全部结果，另附方向统计与被排除的话题词
cont_out <- cont_f %>% dplyr::transmute(词语 = token, 类型 = kind, 传出方 = origin, 接收方 = adopter, 传出方开始 = format(origin_date), 接收方开始 = format(adopt_date), 间隔天数 = lag_days, 传染前传出方次数 = n_origin_before, 传染后接收方次数 = n_adopter_after)
dir_out <- cont_f %>% dplyr::count(origin, adopter, name = "个数") %>% dplyr::mutate(方向 = paste0(origin, "→", adopter)) %>% dplyr::select(方向, 个数)
excl_out <- data.frame(已排除的话题词 = topic_exclude)
#情绪回应（7d、7e）
d_out <- tp2 %>% dplyr::group_by(resp_lab, trig2) %>% dplyr::summarise(配对数 = dplyr::n(), 回复间隔中位数_秒 = median(reply_sec), 下四分位_秒 = quantile(reply_sec, 0.25), 上四分位_秒 = quantile(reply_sec, 0.75), 回应字数中位数 = median(resp_chars), 回应条数均值 = round(mean(resp_msgs), 2), .groups = "drop") %>% dplyr::rename(回应方向 = resp_lab, 对方情绪 = trig2)
d_out$Wilcoxon校正P值 <- rep(signif(p_d, 4), each = 2)
e_out <- emo_d %>% dplyr::mutate(pct = round(pct, 1)) %>% dplyr::select(resp_lab, trigger_class, resp_class, pct) %>% tidyr::pivot_wider(names_from = resp_class, values_from = pct) %>% dplyr::rename(回应方向 = resp_lab, 对方情绪 = trigger_class)
e_out$卡方校正P值 <- rep(signif(p_e, 4), each = 3)
#写入xlsx（3个sheet）
wb <- openxlsx::createWorkbook()
openxlsx::addWorksheet(wb, "7a_7c_用词与风格")
openxlsx::writeData(wb, "7a_7c_用词与风格", sim_out, startRow = 1)
openxlsx::writeData(wb, "7a_7c_用词与风格", trend_out, startRow = nrow(sim_out) + 4)
openxlsx::writeData(wb, "7a_7c_用词与风格", lsm_out, startRow = nrow(sim_out) + nrow(trend_out) + 7)
openxlsx::addWorksheet(wb, "7b_口头禅传染")
openxlsx::writeData(wb, "7b_口头禅传染", dir_out, startRow = 1)
openxlsx::writeData(wb, "7b_口头禅传染", cont_out, startRow = nrow(dir_out) + 4)
openxlsx::writeData(wb, "7b_口头禅传染", excl_out, startRow = nrow(dir_out) + nrow(cont_out) + 7)
openxlsx::addWorksheet(wb, "7d_7e_情绪回应")
openxlsx::writeData(wb, "7d_7e_情绪回应", d_out, startRow = 1)
openxlsx::writeData(wb, "7d_7e_情绪回应", e_out, startRow = nrow(d_out) + 4)
openxlsx::saveWorkbook(wb, file.path(fig_dir, "Figure7_data.xlsx"), overwrite = TRUE)
cat("Figure 7 已导出到:", fig_dir, "\n")
