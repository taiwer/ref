library(tidyverse)

df <- data.frame(
  name = c("Tom", "Jerry", "Mickey", "Donald", "Goofy"),
  height = c("170", "172", "183", "185", "177"),
  weight = c("73", "78", "85", "92", "70")
)


mean(df$height)

# across(c(height, weight), as.numeric) 把height, weight转换为数值型
df <- df %>%
  mutate(across(c(height, weight), as.numeric))

mean(df$height)


library(ggplot2)
library(dplyr)

# 准备数据：按频数降序排列
df <- mpg %>%
  count(class) %>%
  arrange(desc(n)) %>%
  mutate(pct = round(100 * n / sum(n), 0),
         label = paste0(class, "\n", pct, "%"),
         class = factor(class, levels = class) # 按频数降序设置因子顺序
  )

colors <- colorRampPalette(c("lightblue", "blue", "darkblue"))(nrow(df))

# 绘制饼图（极坐标 + 降序排列）
ggplot(df, aes(x = "", y = n, fill = class)) +
  geom_bar(stat = "identity", width = 1) +
  scale_fill_manual(values = colors) +
  coord_polar(theta = "y", direction = -1) +  # 逆时针方向排列
  geom_text(aes(label = label), 
            col = "white",
            position = position_stack(vjust = 0.5), size = 5) +
  theme_void() +
  theme(legend.position = "none")



# merge small cases -------------------------------------------------------

df <- mpg %>%
  mutate(class = fct_lump_n(class, 5)) %>% 
  count(class) %>%
  arrange(desc(n)) %>%
  mutate(pct = round(100 * n / sum(n), 0),
         label = paste0(class, "\n", pct, "%"),
         class = factor(class, levels = class) # 按频数降序设置因子顺序
  )

colors <- colorRampPalette(c("lightblue", "blue", "darkblue"))(nrow(df))

# 绘制饼图（极坐标 + 降序排列）
ggplot(df, aes(x = "", y = n, fill = class)) +
  geom_bar(stat = "identity", width = 1) +
  scale_fill_manual(values = colors) +
  coord_polar(theta = "y", direction = -1) +  # 逆时针方向排列
  geom_text(aes(label = label), 
            col = "white",
            position = position_stack(vjust = 0.5), size = 5) +
  theme_void() +
  theme(legend.position = "none")



# arrange boxplot ---------------------------------------------------------

mpg %>%
  mutate(class = fct_lump_n(class, 5)) %>% 
  ggplot(aes(reorder(class,displ),displ,
             col = class))+
  geom_boxplot()+
  guides(x = guide_axis(n.dodge = 3)) +
  theme(legend.position = "none")

# histogram

mpg %>% 
  ggplot(aes(cty))+
  geom_histogram(breaks = seq(4, 36, 2),
                 fill = "cyan",
                 color = "black")+
  stat_bin(aes(label =ifelse(..count.. == 0, "", ..count..)),
           breaks = seq(4, 36, 2),
           geom = "text",
           vjust = -0.5,
           size = 3)+
  scale_x_continuous(breaks = seq(4, 36, 2))+
  scale_y_continuous(limits = c(0,50))

# chisq.test -------------------------------------------------------

chisq.test(mpg$class, mpg$drv)

chisq.test(mpg$class, mpg$drv)

df <- mpg %>% 
  mutate(class = fct_lump_n(class, 3),
         drv = fct_lump_n(drv, 1))

chisq.test(df$class, df$drv)$expected

# 计算行的百分比
table(df$class, df$drv) %>% 
  prop.table(margin = 1)%>% 
  round(2)*100 

# 计算行的百分比
table(df$class, df$drv) %>% 
  prop.table(margin = 2)%>% 
  round(2)*100 



# ggstatsbetween ----------------------------------------------------------

library(ggstatsplot)

# 筛选两组数据
df <- mpg %>%
  filter(drv %in% c("f", "4")) %>%
  mutate(drv = factor(drv, levels = c("f", "4"),
                      labels = c("前驱", "四驱")))

# 两个总体均值的t检验
ggbetweenstats(
  data = df,
  x = drv,
  y = displ,
  type = "parametric",        
  palette = "Accent",        
  messages = FALSE,
  violin.args = list(width = 0),
  title = "前驱和四驱的排量比较",
  xlab = "驱动方式",
  ylab = "发动机排量",
  bf.message = FALSE,
  digits = 3L
)


# 按中位数对 class 重新排序（中位数越高排越前）
df <- mpg %>%
  mutate(
    class = fct_lump(class, n = 4),
    class = fct_reorder(class, displ, 
                        .fun = median, .desc = TRUE)
  )

# 单因素方差分析
ggbetweenstats(
  data = df,
  x = class,
  y = displ,
  type = "parametric",        
  palette = "Accent", 
  violin.args = list(width = 0),
  title = "Suv, Midsize, Compact, Subcompact, Other五个组别的排量比较",
  xlab = "Class",
  ylab = "发动机排量",
  bf.message = FALSE,
  digits = 3L
)


