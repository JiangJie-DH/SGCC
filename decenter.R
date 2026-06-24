set.seed(123)

p_no_center <- ggraph(g_no_center, layout = "fr") +

  # —— 边 ——
  geom_edge_link(
    aes(width = weight),
    color = "gray65",
    alpha = 0.3,
    show.legend = FALSE
  ) +
  scale_edge_width(range = c(0.4, 2.5)) +

  # —— 节点 ——
  geom_node_point(
    aes(
      size  = degree_network,
      color = betweenness
    ),
    alpha = 0.9
  ) +

  # —— 标签 ——
  geom_node_text(
    aes(label = id_Pinyin),
    repel = TRUE,
    size = 2.6,
    max.overlaps = 15
  ) +

  # —— 节点大小（隐藏图例） ——
  scale_size_continuous(
    range = c(2.5, 12),
    guide = "none"
  ) +

  # —— Betweenness颜色 ——
  scale_color_gradient(
    low  = "lightblue",
    high = "darkred",
    name = "Betweenness\n(Bridge Role)"
  ) +

  # —— 主题 ——
  theme_graph(base_family = "serif") +

  labs(
    title = "Organizational Co-occurrence Network after Removing Shanghai General Chamber of Commerce",

    subtitle =
      "Node color represents betweenness centrality; darker nodes indicate stronger brokerage roles",

    caption =
      "Removing the dominant hub reveals alternative bridging organizations and latent structural dependencies within the organizational network"
  ) +

  guides(
    size = "none"
  )

print(p_no_center)

ggsave(
  filename = "~/Desktop/p_no_center.png",
  plot = p_no_center,
  dpi = 500,
  width = 10,
  height = 8,
  units = "in"
)
