library(tidyverse)
library(ggsankey)
library(glue)
library(patchwork)

# 1. 数据准备 ----
df0 <- read_csv("juece2025.csv") %>% 
  select(number, Con, Dy, RS) %>% 
  distinct() %>%                      
  mutate(across(everything(), fct_infreq))

# 2. 添加诊断正确性标记 ----
df0 <- df0 %>% 
  mutate(
    Con_correct = if_else(Con == RS, "✓", "✕"),
    Dy_correct = if_else(Dy == RS, "✓", "✕")
  )

# 3. 准备Sankey数据(修正版) ----
prepare_sankey_data <- function(data, prefix) {
  correct_col <- paste0(prefix, "_correct")
  
  # 创建节点和流线数据(不丢失number信息)
  node_data <- data %>% 
    select(number, node = !!sym(prefix), correct = !!sym(correct_col))
  
  # 创建连接数据
  link_data <- data %>% 
    count(!!sym(prefix), RS, !!sym(correct_col)) %>% 
    rename(source = !!sym(prefix), target = RS, correct = !!sym(correct_col))
  
  # 创建ggsankey需要的格式
  list(
    nodes = node_data,
    links = link_data,
    track = glue("{prefix} → RS")
  )
}

# 4. 生成Sankey数据 ----
sankey_con <- prepare_sankey_data(df0, "Con")
sankey_dy <- prepare_sankey_data(df0, "Dy")

# 5. 可视化设置 ----
node_colors <- c(
  "LE"        = "#ffcc99",
  "TME"       = "#b3e2b0",
  "SCRT/CRT"  = "#d9b8d4",
  "TNT"       = "#a6cee3",
  "Con"       = "#fbb4ae",
  "Dy"        = "#b3cde3",
  "RS"        = "#ccebc5"
)

# 6. 绘图函数(修正版) ----
plot_sankey <- function(sankey_data, title) {
  # 准备节点位置
  nodes <- sankey_data$links %>% 
    distinct(source) %>% 
    rename(node = source) %>% 
    bind_rows(tibble(node = "RS")) %>% 
    mutate(x = case_when(
      node %in% c("Con", "Dy") ~ 1,
      node == "RS" ~ 3,
      TRUE ~ 2
    ))
  
  # 创建绘图数据
  plot_data <- sankey_data$links %>% 
    left_join(nodes, by = c("source" = "node")) %>% 
    rename(x_from = x) %>% 
    left_join(nodes, by = c("target" = "node")) %>% 
    rename(x_to = x)
  
  ggplot() +
    # 绘制流线(使用geom_curve模拟桑基图)
    geom_curve(
      data = plot_data,
      aes(x = x_from, xend = x_to,
          y = 0, yend = 0,
          size = n, color = source),
      curvature = 0.2,
      alpha = 0.7
    ) +
    # 添加诊断标记
    geom_text(
      data = plot_data,
      aes(x = (x_from + x_to)/2, y = 0, label = correct),
      size = 4,
      vjust = -1
    ) +
    # 添加节点标签
    geom_label(
      data = nodes,
      aes(x = x, y = 0, label = node, fill = node),
      size = 3.5,
      color = "black"
    ) +
    scale_size_continuous(range = c(1, 10)) +
    scale_color_manual(values = node_colors) +
    scale_fill_manual(values = node_colors) +
    labs(title = title) +
    theme_void() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
      legend.position = "none"
    ) +
    xlim(0.5, 3.5)
}

# 7. 分别绘制两个图 ----
plot_con <- plot_sankey(sankey_con, "Con诊断路径与金标准对比")
plot_dy <- plot_sankey(sankey_dy, "Dy诊断路径与金标准对比")

# 8. 合并图形 ----
combined_plot <- plot_con / plot_dy +
  plot_annotation(
    title = "诊断路径与金标准对比",
    subtitle = "✓表示诊断正确，✕表示诊断错误",
    theme = theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
      plot.subtitle = element_text(hjust = 0.5, color = "gray40", size = 14)
    )
  )

print(combined_plot)
