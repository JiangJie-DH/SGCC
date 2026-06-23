
g_person <- tbl_graph(nodes = nodes_filtered_person,
                      edges = edges_filtered_person,
                      directed = FALSE) %>%
  activate(nodes) %>%
  mutate(
    id = ifelse(id == "Yu Xiaqing", "Yu Qiaqing", id),
    degree_network = centrality_degree(),
    betweenness = centrality_betweenness(),
    closeness = centrality_closeness()
  )


g_person_kcore <- g_person_kcore %>%
  activate(nodes) %>%
  mutate(id_Pinyin = ifelse(
    id_Pinyin == "Yu Xiaqing",
    "Yu Qiaqing",
    id_Pinyin
  ))

set.seed(123)

p_person_kcore <- ggraph(g_person_kcore, layout = "fr") +
  
  # —— k-core 圈层凸包 —— 
  geom_mark_hull(
    aes(x, y, fill = k_layer, group = k_layer),
    concavity = 6,
    expand = unit(4, "mm"),
    alpha = 0.15,
    color = NA,
    show.legend = TRUE
  ) +
  
  # —— 边 —— 
  geom_edge_link(
    aes(width = weight),
    color = "gray65",
    alpha = 0.3,
    show.legend = FALSE
  ) +
  scale_edge_width(range = c(0.4, 2.5)) +
  
  # —— 节点：颜色=社区，形状=k-core —— 
  geom_node_point(
    aes(
      size  = degree_network,
      color = community,
      shape = k_layer
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
  
  # —— 样式控制 —— 
  scale_size_continuous(range = c(2.5, 12), guide = "none") +
  
  scale_color_brewer(
    palette = "Set2",
    name = "Community"
  ) +
  
  scale_shape_manual(
    values = c(
      "Core"       = 19,
      "Middle"     = 17,
      "Peripheral" = 1
    ),
    name = "k-core Layer"
  ) +
  
  scale_fill_manual(
    values = c(
      "Core"       = alpha("red",    0.18),
      "Middle"     = alpha("orange", 0.18),
      "Peripheral" = alpha("gray80", 0.18)
    ),
    name = "k-core Layer"
  ) +
  
  theme_graph(base_family = "serif") +
  labs(
    title    = "Core–Periphery Structure of the Person Co-occurrence Network",
    subtitle = "Node color indicates narrative communities; node shape indicates k-core embeddedness",
    caption  = "k-core captures stable institutional embeddedness beyond mere co-occurrence frequency"
  )

print(p_person_kcore)

# 保存为 500 DPI 到桌面
ggsave(
  filename = "~/Desktop/p_person_kcore.png",
  plot = p_person_kcore,
  dpi = 500,
  width = 10,
  height = 8,
  units = "in"
)

