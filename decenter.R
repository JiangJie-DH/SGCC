p_no_center <- ggraph(g_no_center, layout = "fr") +
  geom_edge_link(
    aes(width = weight),
    color = "gray60",
    alpha = 0.3,
    show.legend = FALSE
  ) +
  geom_node_point(
    aes(
      size  = degree_network,   # 仍用于视觉
      color = betweenness
    ),
    alpha = 0.8
  ) +
  geom_node_text(
    aes(label = id_Pinyin),     # ← 改这里：统一使用 id_Pinyin
    repel = TRUE,
    size = 3,
    max.overlaps = 20
  ) +
  scale_edge_width(range = c(0.5, 3)) +
  
  # ⚠️ 关键：size 图例关闭
  scale_size_continuous(
    range = c(2, 12),
    guide = "none"
  ) +
  
  scale_color_gradient(
    low = "lightblue",
    high = "darkred"
  ) +
  
  theme_graph() +
  labs(
    title = '去除“上海总商会”后的组织网络',
    subtitle = 'Edge weight > 8',
    color = 'Betweenness'
  ) +
  
  guides(
    size = "none"   # 再加一道保险，确保 degree_network 不进图例
  )

print(p_no_center)

