# 上海商业精英论文 - 申报总商会系统网络分析（精简版）
# 基于申报对上海总商会系统的网络分析
# 2026年1月2日 

# ============================================================================
# 环境设置
# ============================================================================

# 加载必要的包
library(histtext)
library(lubridate)
library(dplyr)
library(stringi)
library(readr)
library(igraph)
library(ggplot2)
library(ggraph)
library(purrr)
library(tidyr)
library(stringr)
library(showtext)
library(tidygraph)
library(ggraph)
library(ggforce)
library(concaveman)

# 设定工作文件夹
setwd("~/Downloads/R_documents/SCE/精简版") # 根据电脑不同，酌情修改文件夹位置

# ============================================================================
# 一、数据检索与基础处理
# ============================================================================

# 检索包含总商会系统的报道
zsh_chamber_all <- search_documents(
  '"上海商業會議公所"|"商業會議公所"|"上海商務公所"|"商務公所"|"上海商務總會"|"商務總會"|"上海總商會"|"總商會"',
  "shunpao-revised"
)
zsh_chamber_all <- unique(zsh_chamber_all)  # 去重，得30432条

# 获取报道全文
zsh_chamber_ft <- get_documents(zsh_chamber_all, "shunpao-revised") # 得30432篇

# 导出数据
write_csv(zsh_chamber_all, "zsh_chamber_all.csv")
write_csv(zsh_chamber_ft, "zsh_chamber_ft.csv")

# 生成文章长度列
zsh_chamber_ft <- zsh_chamber_ft %>% mutate(Length = nchar(Text))

# 文章长度分类统计图
zsh_chamber_ft %>%
  mutate(Length_Category = cut(Length, 
                               breaks = c(0, 500, 2000, 5000, Inf),
                               labels = c("Short(<500)", "Medium(500-2000)", 
                                          "Long(2000-5000)", "Very Long(>5000)"))) %>%
  count(Length_Category) %>%
  ggplot(aes(x = Length_Category, y = n, fill = Length_Category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = n), vjust = -0.5) +
  theme_minimal() +
  labs(x = "Article Type", y = "Count", title = "Distribution of Article Length Categories")

# 提取年代分布
zsh_chamber_temp <- zsh_chamber_ft %>%
  mutate(Year = as.integer(str_sub(Date, 1, 4))) %>%
  mutate(N = 1) %>%
  group_by(Year) %>%
  summarise(N = sum(N))

# 年代分布柱状图（1902-1929）
ggplot(zsh_chamber_temp, aes(x = Year, y = N)) +
  geom_col(fill = "blue") +
  labs(title = "ZSH in Shenbao", subtitle = "Number of mentions",
       x = "Year", y = "Number of articles") +
  xlim(1902, 1929) +
  theme(panel.background = element_rect(fill = "lightgrey"))

# ============================================================================
# 二、共现词检索与清洗
# ============================================================================

# 搜索总商会系统的共现词表（context_size = 100）
zsh_sbconc <- search_concordance(
  '"上海商業會議公所"|"商業會議公所"|"上海商務公所"|"商務公所"|"上海商務總會"|"商務總會"|"上海總商會"|"總商會"',
  corpus = "shunpao-revised", 
  context_size = 100
) # 得56231条

# 合并concordance列
zsh_sbconc <- zsh_sbconc %>% 
  mutate(Text = paste(Before, Matched, After)) %>%
  relocate(Text, .before = "Source")

write.csv(zsh_sbconc, "zsh_sbconc.csv") 

# 提取1902-1929年数据
# 先生成Year列，再进行筛选
zsh_sbconc_use <- zsh_sbconc %>% 
  mutate(Year = as.numeric(str_sub(DocId, 5, 8))) %>%
  filter(between(Year, 1902, 1929)) # 53982条

# 数据清洗：删除其他地区总商会
zsh_names <- c("上海商業會議公所", "上海商務公所", "上海商務總會", "上海總商會")

# 第一步：提取Matched为总商会名称的行
zsh_sbconc_1 <- zsh_sbconc_use %>% filter(Matched %in% zsh_names)

# 第二步：提取Before列最右侧两个字符并过滤
zsh_sbconc_temp <- zsh_sbconc_use %>%
  filter(!Matched %in% zsh_names) %>%
  mutate(Before_temp = str_sub(Before, -2, -1))

# 提取Before列最右侧两个字符为空的所有行
# 等同于总商会系统
zsh_sbconc_2 <- zsh_sbconc_temp %>% 
  filter(Before_temp == "" | is.na(Before_temp))

# 提取Before列最右侧两个字符不为空的所有行
# 用于继续清洗
zsh_sbconc_temp_2 <- zsh_sbconc_temp %>% 
  filter(Before_temp != "" & !is.na(Before_temp))  # 40910条

# 统计Before_temp
Statis_before <- zsh_sbconc_temp_2 %>%
  count(Before_temp, name = "n") %>%
  arrange(desc(n))

# 删除省市简称
search_pattern <- "蘇|杭|寧|甯|京|平|粵|漢|廈"
Statis_before_1 <- Statis_before %>%
  filter(str_detect(Before_temp, search_pattern))

zsh_sbconc_temp_3 <- zsh_sbconc_temp_2 %>%
  anti_join(Statis_before_1, by = "Before_temp")  # 35343条

# 删除历史省份
historical_province_pattern <- "北京|天津|河北|山西|蒙古|遼寧|吉林|龍江|上海|江蘇|浙江|安徽|福建|江西|山東|河南|湖北|湖南|廣東|廣西|海南|重慶|四川|貴州|雲南|西藏|陝西|甘肅|青海|寧夏|新疆|香港|澳門|臺灣|熱河|察哈爾|綏遠|西康"

Statis_before_2 <- zsh_sbconc_temp_3 %>%
  count(Before_temp, name = "n") %>%
  arrange(desc(n))

Statis_before_3 <- Statis_before_2 %>%
  filter(str_detect(Before_temp, historical_province_pattern))

zsh_sbconc_temp_4 <- zsh_sbconc_temp_3 %>%
  anti_join(Statis_before_3, by = "Before_temp")  # 34355条

# 删除其他地区标识
search_pattern_2 <- "中華|廣|津|蕪|寗|全國|江寗|省"
Statis_before_4 <- zsh_sbconc_temp_4 %>%
  count(Before_temp, name = "n") %>%
  arrange(desc(n))

Statis_before_5 <- Statis_before_4 %>%
  filter(str_detect(Before_temp, search_pattern_2))

zsh_sbconc_temp_5 <- zsh_sbconc_temp_4 %>%
  anti_join(Statis_before_5, by = "Before_temp")  # 29888条

write.csv(zsh_sbconc_temp_5, "zsh_sbconc_temp_5.csv")

# 导入人工清洗后的数据
zsh_sbconc_temp_5_Ed <- read_csv("zsh_sbconc_temp_5_Ed.csv")  


# 合并清洗后的数据
zsh_sbconc_all <- bind_rows(
  zsh_sbconc_1 %>% mutate(Date = as.Date(Date)),
  zsh_sbconc_2 %>% select(-Before_temp) %>% mutate(Date = as.Date(Date)),
  zsh_sbconc_temp_5_Ed %>% select(-Before_temp) %>% mutate(Date = as.Date(Date))
)  

write.csv(zsh_sbconc_all, "zsh_sbconc_all.csv")


# ============================================================================
# 三、命名实体识别（NER）
# ============================================================================

# NER处理
zsh_sbconc_ner_use <- histtext::ner_on_corpus(
  zsh_sbconc_use, 
  corpus = "shunpao-revised", 
  only_precomputed = TRUE
)  # 13775722条

write_csv(zsh_sbconc_ner_use, "zsh_sbconc_ner_use.csv")


# 提取实体对应年代
zsh_sbconc_ner_use <- zsh_sbconc_ner_use  %>% 
  mutate(Year = as.numeric(str_sub(DocId, 5, 8))) # 经检测，均为1902-1929之间数据

# 提取PERSON、ORG和EVENT实体
zsh_sbconc_ner_2 <- zsh_sbconc_ner_use %>%
  filter(str_detect(Type, "PERSON|ORG|EVENT"))  # 5072365条

# 删除非汉字、空格，清理数据
zsh_sbconc_ner_2 <- zsh_sbconc_ner_2 %>%
  mutate(Text = str_replace_all(Text, "[^[\\p{Han}]]", " ")) %>%
  mutate(Text = str_squish(Text)) %>%
  filter(Text != "") %>%
  filter(nchar(Text) > 1) %>%
  filter(Confidence >= 0.6)  # 4383601条

write_csv(zsh_sbconc_ner_2, "zsh_sbconc_ner_2.csv")

# 处理Type冲突
conflicting_rows <- zsh_sbconc_ner_2 %>%
  group_by(DocId, Text) %>%
  filter(n_distinct(Type) > 1) %>%
  arrange(DocId, Text, Type) %>%
  ungroup()  # 16994条

write.csv(conflicting_rows, "conflicting_rows.csv")

# 删除冲突行，导入人工清洗后的数据
zsh_sbconc_ner_3 <- zsh_sbconc_ner_2 %>%
  anti_join(conflicting_rows, by = c("DocId", "Text"))  # 4286923条

conflicting_rows_Ed <- read_csv("conflicting_rows_Ed.csv")

zsh_sbconc_ner_4 <- bind_rows(zsh_sbconc_ner_3, conflicting_rows_Ed) %>%
  distinct()  # 1848246条

write.csv(zsh_sbconc_ner_4, "zsh_sbconc_ner_4.csv")

# 继续清洗：删除空数据、敬语
zsh_sbconc_ner_5 <- zsh_sbconc_ner_4 %>%
  filter(!is.na(Text)) %>%
  filter(Text != "") %>%
  filter(!Text %in% c("云云", "鈞鑒"))

# 分离合并的实体
zsh_sbconc_ner_6 <- zsh_sbconc_ner_5 %>%
  separate_rows(Text, sep = "\\s+") %>%
  filter(Text != "") %>%
  mutate(Length = nchar(Text)) %>%
  filter(Length > 1)  # 1852050条

write_csv(zsh_sbconc_ner_6, "zsh_sbconc_ner_6.csv")


# 统计年代分布
yearly_counts <- zsh_sbconc_ner_6 %>%
  filter(between(Year, 1902, 1929)) %>%
  distinct(DocId, Year) %>%
  count(Year, name = "Articles")

total_articles <- sum(yearly_counts$Articles)

ggplot(yearly_counts, aes(x = Year, y = Articles)) +
  geom_col(fill = "#3B7FB6", width = 0.8) +
  geom_text(
    aes(label = Articles),
    vjust = -0.3,
    size = 3
  ) +
  scale_x_continuous(
    breaks = seq(1902, 1929, by = 1),
    limits = c(1901.5, 1929.5)
  ) +
  labs(
    title = "Number of Articles by Year",
    subtitle = paste0("Total articles: ", total_articles),
    x = "Year",
    y = "Number of Articles"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold", size = 18),
    plot.subtitle = element_text(size = 13)
  )

# 出图
ggsave("yearly_counts.png", width = 12, height = 8, dpi = 300)


# 统计频次分布
text_counts_entity <- zsh_sbconc_ner_6 %>%
  group_by(Text) %>%
  summarise(Freq = n()) %>%
  arrange(desc(Freq)) %>%
  ungroup()

# 频次分布图
freq_distribution <- text_counts_entity %>%
  mutate(freq_range = case_when(
    Freq >= 1 & Freq <= 4 ~ "1-4",
    Freq == 5 ~ "5",
    Freq >= 6 & Freq <= 10 ~ "6-10",
    Freq >= 11 & Freq <= 20 ~ "11-20",
    Freq >= 21 & Freq <= 50 ~ "21-50",
    Freq >= 51 & Freq <= 100 ~ "51-100",
    Freq >= 101 & Freq <= 500 ~ "101-500",
    Freq > 500 ~ "500+",
    TRUE ~ as.character(Freq)
  )) %>%
  group_by(freq_range) %>%
  summarise(entity_count = n()) %>%
  ungroup()

freq_distribution$freq_range <- factor(
  freq_distribution$freq_range,
  levels = c("1-4", "5", "6-10", "11-20", "21-50", "51-100", "101-500", "500+")
)

# 出图
ggplot(freq_distribution, aes(x = freq_range, y = entity_count)) +
  geom_bar(stat = "identity", fill = "#667eea", alpha = 0.8) +
  geom_text(aes(label = entity_count), vjust = -0.5, size = 4) +
  labs(
    # title = "Organization Frequency Distribution - Long Tail Effect",
    # subtitle = "Following Long Tail Theory: Few high-frequency entities, many low-frequency entities",
    x = "Frequency Range", 
    y = "Entity Count"
  ) +
  theme_minimal(base_size = 14)

# ============================================================================
# 四、O2O网络（机构-机构网络）
# ============================================================================

# 提取ORG实体并限制在总商会15字符范围内
zsh_OrgStrt <- zsh_sbconc_ner_6 %>% 
  filter(Type == "ORG") %>%
  select(-7) %>%
  unique()  # 810342条

# 提取总商会关键词
zsh_zsh <- zsh_OrgStrt %>%
  filter(str_detect(Text, '上海商業會議公所|商業會議公所|上海商務公所|商務公所|上海商務總會|商務總會|上海總商會|總商會'))

zsh_OrgStrt <- bind_rows(zsh_OrgStrt, zsh_zsh) %>% unique()

write_csv(zsh_OrgStrt, "zsh_OrgStrt.csv")

# 提取总商会附近15字符的ORG实体
zsh_data <- zsh_OrgStrt %>%
  filter(Text %in% c("上海商業會議公所", "上海商務公所", "上海商務總會", "上海總商會",
                     "商業會議公所", "商務公所", "商務總會", "總商會"))

org_data <- zsh_OrgStrt[zsh_OrgStrt$Type == "ORG", ]

results <- list()
for (i in 1:nrow(zsh_data)) {
  current_entry <- zsh_data[i, ]
  same_doc_orgs <- org_data[org_data$DocId == current_entry$DocId, ]
  close_orgs <- same_doc_orgs[
    abs(same_doc_orgs$Start - current_entry$End) <= 15 |
    abs(same_doc_orgs$End - current_entry$Start) <= 15, 
  ]
  if (nrow(close_orgs) > 0) {
    results[[length(results) + 1]] <- close_orgs
  }
}

final_results <- do.call(rbind, results) %>% unique() # 得43839条

# 删除频次<5的实体
text_counts_Org2 <- final_results %>%
  group_by(Text) %>%
  summarise(Freq = n()) %>%
  arrange(desc(Freq))

entities_to_keep <- text_counts_Org2 %>%
  filter(Freq >= 5) %>%
  pull(Text)

final_results <- final_results %>% filter(Text %in% entities_to_keep) # 得34229条
write.csv(final_results, "final_results.csv")

# 机构名标准化
final_results_standardized <- final_results %>%
  mutate(Text_Standard = case_when(
    Text %in% c("上海總商會", "總商會") ~ "上海總商會",
    Text %in% c("上海商務總會", "商務總會") ~ "上海商務總會",
    Text %in% c("上海縣敎育會", "縣敎育會") ~ "上海縣敎育會",
    Text %in% c("農商部", "農工商部") ~ "農商部",
    Text %in% c("全國商聯會", "全國商會聯合會", "全國商會聨合會", 
                "全國商會聯合會總事務所", "商聯會") ~ "全國商會聯合會",
    Text %in% c("縣商會", "上海縣商會", "南市縣商會", "南巿縣商會") ~ "上海縣商會",
    Text %in% c("銀行公會", "上海銀行公會") ~ "上海銀行公會",
    Text %in% c("錢業公會", "上海錢業公會", "銀行公會錢業公會") ~ "上海錢業公會",
    Text %in% c("閘北商會", "上寳閘北商會", "上寶閘北商會") ~ "閘北商會",
    Text %in% c("江蘇省敎育會", "江蘇省教育會", "省敎育會") ~ "江蘇省教育會",
    Text %in% c("國民政府", "南京國民政府", "國府") ~ "國民政府",
    Text %in% c("農商部", "北京農商部") ~ "農商部",
    Text %in% c("財政部", "財部") ~ "財政部",
    Text %in% c("外交部", "北京外交部", "外務部") ~ "外交部",
    Text %in% c("工部局", "公共租界工部局", "上海工部局") ~ "工部局",
    Text %in% c("總工會", "上海總工會") ~ "上海總工會",
    Text %in% c("國務院", "北京國務院") ~ "國務院",
    Text %in% c("縣公署", "上海縣公署") ~ "上海縣公署",
    Text %in% c("上海特别市黨部", "市黨部") ~ "市黨部",
    Text %in% c("交涉公署", "江蘇交涉公署", "交涉署") ~ "江蘇交涉公署",
    Text %in% c("中華國貨維持會", "國貨維持會") ~ "中華國貨維持會",
    TRUE ~ Text
  ))

# 统计统一后的名称
Statis_final_results_standardized<- final_results_standardized %>%
  count(Text_Standard, name = "n") %>%
  arrange(desc(n)) # 得549条
write.csv(Statis_final_results_standardized, "Statis_final_results_standardized.csv")


# 生成O2O边和节点
edges_org <- final_results_standardized %>%
  group_by(DocId) %>%
  distinct(Text_Standard) %>%
  filter(n() >= 2) %>%
  summarise(combinations = list(combn(Text_Standard, 2, simplify = FALSE)),
            .groups = 'drop') %>%
  unnest(combinations) %>%
  mutate(source = map_chr(combinations, 1),
         target = map_chr(combinations, 2)) %>%
  select(source, target, DocId)

edges_weighted_org <- edges_org %>%
  group_by(source, target) %>%
  summarise(weight = n(), doc_ids = list(DocId), .groups = 'drop')

nodes_org <- final_results_standardized %>%
  distinct(Text_Standard) %>%
  rename(id = Text_Standard) %>%
  mutate(degree = map_int(id, ~sum(final_results_standardized$Text_Standard == .x)),
         docs = map(id, ~unique(final_results_standardized$DocId[final_results_standardized$Text_Standard == .x])))

# 过滤权重>40的边
edges_filtered <- edges_weighted_org %>% filter(weight > 40)

nodes_in_edges <- unique(c(edges_filtered$source, edges_filtered$target))
nodes_filtered <- nodes_org %>% filter(id %in% nodes_in_edges)


# 找出哪些列是列表类型
list_columns <- sapply(nodes_filtered, is.list)

# 只导出非列表列
write.csv(nodes_filtered[, !list_columns], 
          "nodes_filtered_no_lists.csv", 
          row.names = FALSE)

# 导入处理后的表格nodes_filtered_Ed.csv
nodes_filtered_Ed <- read_csv("nodes_filtered_Ed.csv")


# 基于处理后的节点，从原始边数据重新过滤
edges_consistent <- edges_weighted_org %>%
  filter(weight > 40) %>%
  filter(source %in% nodes_filtered_Ed$id & 
           target %in% nodes_filtered_Ed$id)

# 确保没有孤立节点
nodes_in_edges <- unique(c(edges_consistent$source, edges_consistent$target))
nodes_final <- nodes_filtered_Ed %>% filter(id %in% nodes_in_edges)

# 构建图
# 重新构建图并确保计算度中心性
g_tidy <- tbl_graph(
  nodes = nodes_final,
  edges = edges_consistent %>% rename(from = source, to = target),
  directed = FALSE
) %>%
  activate(nodes) %>%
  mutate(
    degree = centrality_degree(),  # 计算度中心性
    betweenness = centrality_betweenness(),
    closeness = centrality_closeness()
  ) %>%
  activate(edges) %>%
  mutate(weight = weight)  # 确保权重列存在



# 创建静态图
p_static_enhanced <- ggraph(g_tidy, layout = 'fr') +
  geom_edge_link(aes(width = weight), alpha = 0.3, color = 'gray60') +
  geom_node_point(aes(size = degree, color = Attribute_En), alpha = 0.8) +
  geom_node_text(aes(label = id_Pinyin), repel = TRUE, size = 4, max.overlaps = 30) +  # 增大到4
  scale_edge_width(range = c(0.5, 3)) +
  scale_size_continuous(range = c(3, 15)) +  # 增大节点大小范围
  scale_color_brewer(palette = "Set1") +
  theme_graph(base_size = 12) +  # 增加基础字体大小
  labs(
    size = 'Degree Centrality',
    color = 'Institution Type',
    # edge_width = 'Co-occurrence Frequency'
  ) +
  theme(
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 11, face = "bold"),
    plot.margin = margin(10, 10, 10, 10)  # 增加边距
  )

print(p_static_enhanced)



# 保存高清大图
ggsave("simplified_network.png", p_static_enhanced, 
       width = 14,    # 增加宽度
       height = 10,   # 增加高度
       dpi = 300,     # 合适的DPI
       bg = "white")  # 白色背景


# ===============================
# k-core 圈层结构 + 组织属性（Attribute）
# 论文主图版本
# 成功
# ===============================

# -------------------------------
# 1. tidygraph → igraph
# -------------------------------
g_igraph <- as.igraph(g_tidy)

# -------------------------------
# 2. k-core coreness（非加权）
# -------------------------------
core <- igraph::coreness(g_igraph)

# -------------------------------
# 3. 回写 coreness，并划分圈层
# -------------------------------
g_tidy_kcore <- g_tidy %>%
  activate(nodes) %>%
  mutate(
    coreness  = as.numeric(core),
    node_size = coreness,
    layer = case_when(
      coreness >= quantile(coreness, 0.8, na.rm = TRUE) ~ "Core Layer",
      coreness >= quantile(coreness, 0.5, na.rm = TRUE) ~ "Middle Layer",
      TRUE ~ "Peripheral Layer"
    )
  )

# -------------------------------
# 4. 绘图
# -------------------------------
p_kcore <- ggraph(g_tidy_kcore, layout = "fr") +
  
  # 圈层凸包（仍然按 layer）
  geom_mark_hull(
    aes(x, y, fill = layer),
    concavity = 4,
    expand = unit(4, "mm"),
    alpha = 0.12,
    show.legend = FALSE
  ) +
  
  # 边
  geom_edge_link(
    aes(width = weight),
    color = "gray65",
    alpha = 0.4,
    show.legend = FALSE
  ) +
  scale_edge_width(range = c(0.3, 2.2), guide = "none") +
  
  # 节点：颜色 = Attribute，形状 = layer
  geom_node_point(
    aes(
      size  = degree,
      color = `Attribute_En`,
      shape = layer
    ),
    alpha = 0.9,
    stroke = 1.1
  )+
  scale_size_continuous(range = c(3, 14), guide = "none") +
  
  # 标签
  geom_node_text(
    aes(label = id_Pinyin),
    repel = TRUE,
    size = 2.6,
    alpha = 0.9
  ) +
  
  # 组织属性颜色（你可按史学含义微调）
  scale_color_brewer(
    name = "Organization Type",
    palette = "Set1"
  ) +
  
  # 圈层填充
  scale_fill_manual(
    values = c(
      "Core Layer" = alpha("red", 0.15),
      "Middle Layer" = alpha("orange", 0.15),
      "Peripheral Layer" = alpha("gray80", 0.15)
    )
  ) +
  
  # 圈层形状
  scale_shape_manual(
    name = "Network Layer",
    values = c(
      "Core Layer" = 19,      # 实心圆
      "Middle Layer" = 17,    # 三角
      "Peripheral Layer" = 1  # 空心圆
    )
  ) +
  
  theme_graph() +
  labs(
    title = "The Organization–Organization Network Structure",
    subtitle = "Layered Configuration Based on k-core Decomposition"
  ) +
  
  guides(
    fill  = "none",
    size  = "none",
    color = guide_legend(order = 1),
    shape = guide_legend(order = 2)
  )

print(p_kcore)

# 保存高清大图
ggsave("he Organization–Organization Network Structure.png", p_kcore, 
       width = 14,    # 增加宽度
       height = 10,   # 增加高度
       dpi = 500,     # 合适的DPI
       bg = "white")  # 白色背景



# ===============================
# 1. 去除“上海总商会”
# ===============================
final_results_no_center <- final_results_standardized %>%
  filter(Text_Standard != "上海總商會")

# ===============================
# 2. 构建共现边
# ===============================
edges_weighted_no_center <- final_results_no_center %>%
  group_by(DocId) %>%
  distinct(Text_Standard) %>%
  filter(n() >= 2) %>%
  summarise(
    pairs = list(combn(Text_Standard, 2, simplify = FALSE)),
    .groups = "drop"
  ) %>%
  unnest(pairs) %>%
  mutate(
    source = map_chr(pairs, 1),
    target = map_chr(pairs, 2)
  ) %>%
  group_by(source, target) %>%
  summarise(
    weight = n(),
    doc_ids = list(DocId),
    .groups = "drop"
  )

# ===============================
# 3. 初始节点表（仅用于导出人工清洗）
# ===============================
nodes_filtered_no_center <- final_results_no_center %>%
  group_by(Text_Standard) %>%
  summarise(
    n_docs = n_distinct(DocId),
    .groups = "drop"
  ) %>%
  rename(id = Text_Standard)

# 只导出非 list 列
list_columns <- sapply(nodes_filtered_no_center, is.list)

write.csv(
  nodes_filtered_no_center[, !list_columns],
  "nodes_filtered_no_center_list.csv",
  row.names = FALSE
)

# ===============================
# 4. 读回人工清洗后的节点表（核心节点集）
# ===============================
nodes_filtered_no_center_Ed <- read_csv("nodes_filtered_no_center_Ed.csv")

# ===============================
# 5. 边表过滤（与节点表对齐 + 权重阈值）
# ===============================
edges_filtered_no_center <- edges_weighted_no_center %>%
  filter(
    source %in% nodes_filtered_no_center_Ed$id,
    target %in% nodes_filtered_no_center_Ed$id,
    weight > 8
  )

# 再保险：去掉孤立节点
nodes_in_edges <- unique(c(
  edges_filtered_no_center$source,
  edges_filtered_no_center$target
))

nodes_final_no_center <- nodes_filtered_no_center_Ed %>%
  filter(id %in% nodes_in_edges)

# ===============================
# 6. 输出最终网络数据（供复现 / 存档）
# ===============================
write_csv(edges_filtered_no_center, "edges_final_no_center.csv")
write_csv(nodes_final_no_center,    "nodes_final_no_center.csv")

# ===============================
# 7. 构建网络 + 中心性
# ===============================
g_no_center <- tbl_graph(
  nodes = nodes_final_no_center,
  edges = edges_filtered_no_center,
  directed = FALSE
) %>%
  activate(nodes) %>%
  mutate(
    degree_network = centrality_degree(),
    betweenness    = centrality_betweenness(),
    closeness      = centrality_closeness()
  )

# ===============================
# 8. 可视化
# ===============================
p_no_center <- ggraph(g_no_center, layout = "fr",layout = "stress") +
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
    title = 'The Organizational Network after De-centering',
    subtitle = 'Edge weight > 8',
    color = 'Betweenness'
  ) +
  
  guides(
    size = "none"   # 再加一道保险，确保 degree_network 不进图例
  )

print(p_no_center)


# ============================================================================
# 五、P2P网络（人物-人物网络）
# ============================================================================

# 提取PERSON实体
zsh_PerStrt <- zsh_sbconc_ner_6 %>% 
  filter(Type == "PERSON") %>%
  unique()  # 1041037条

write_csv(zsh_PerStrt, "zsh_PerStrt.csv")

# 提取总商会附近15字符的PERSON实体
person_data <- zsh_PerStrt[zsh_PerStrt$Type == "PERSON", ]

results <- list()
for (i in 1:nrow(zsh_data)) {
  current_entry <- zsh_data[i, ]
  same_doc_persons <- person_data[person_data$DocId == current_entry$DocId, ]
  close_persons <- same_doc_persons[
    abs(same_doc_persons$Start - current_entry$End) <= 15 |
    abs(same_doc_persons$End - current_entry$Start) <= 15, 
  ]
  if (nrow(close_persons) > 0) {
    results[[length(results) + 1]] <- close_persons
  }
}

final_results_person <- do.call(rbind, results) %>% 
  unique() %>% 
  select(-7)  # 11639条

# 删除敬语
honorifics_to_remove <- c("云敬", "云頃", "文云", "諸公", "公鑒", 
                          "奉鈞", "鑒元", "台鑒", "均鑒")

final_results_person_2 <- final_results_person %>%
  filter(!Text %in% honorifics_to_remove) %>%
  mutate(Text = str_replace(Text, "君$", "")) %>%
  filter(str_length(Text) > 1)  # 11241条

# 人名标准化
final_results_person_clean <- final_results_person_2 %>%
  mutate(Text_clean = Text %>%
           str_replace_all("\\s+", " ") %>%
           str_trim() %>%
           str_replace_all("\u3000", " "))

dict <- tibble::tibble(
  variant = c(
    "虞和德", "虞和德啓", "虞洽", "虞洽節", "虞洽老", "虞洽卿", "虞洽卿", "虞卿", "虞治卿",
    "馮少山", "馮培熹", "馮培熺",
    "方椒伯", "方椒伯二", "方椒", "方積蕃", "方伯", "方栩伯",
    "朱葆三", "朱葆", "朱葆珊", "朱佩珍", "朱公葆三", "朱偑珍",
    "林康侯", "林康候", "林康",
    "沈聯芳", "沈聨芳",
    "趙晋卿", "趙晉卿", "趙錫恩",
    "葉惠鈞", "葉惠釣",
    "孫傳芳", "孫停芳", "孫聨帥", "孫聯帥", "孫馨帥", "孫馨師",
    "王曉籟", "王孝賚",
    "宋漢章", "宋漢", "宋漢傘",
    "周金箴", "周金旗",
    "聞蘭亭", "聞闌亭",
    "聶雲台", "聶雲臺", "聶君雲", "聶其杰",
    "穆藕初", "穆抒齋", "穆藉初", "穆杼齋", "穆藕",
    "傅筱庵", "傅筱菴", "傅宗耀", "傅筱董",
    "盧永祥", "盧護軍"
  ),
  standard = c(
    rep("虞治卿", 9), rep("馮少山", 3), rep("方椒伯", 6),
    rep("朱葆三", 6), rep("林康侯", 3), rep("沈聯芳", 2),
    rep("趙晋卿", 3), rep("葉惠鈞", 2), rep("孫傳芳", 6),
    rep("王曉籟", 2), rep("宋漢章", 3), rep("周金箴", 2),
    rep("聞蘭亭", 2), rep("聶雲台", 4), rep("穆藕初", 5),
    rep("傅筱庵", 4), rep("盧永祥", 2)
  )
)

dict <- dict %>%
  mutate(variant_clean = variant %>%
           str_replace_all("\\s+", " ") %>%
           str_trim() %>%
           str_replace_all("\u3000", " "))

final_results_person_standardized <- final_results_person_clean %>%
  left_join(dict, by = c("Text_clean" = "variant_clean")) %>%
  mutate(Text_Standard = dplyr::coalesce(standard, Text)) %>%
  select(-8, -9, -10) %>%
  filter(str_length(Text) > 1)

write.csv(final_results_person_standardized, "final_results_person_standardized_2.csv")

# 生成P2P边和节点
edges_person <- final_results_person_standardized %>%
  group_by(DocId) %>%
  distinct(Text_Standard) %>%
  filter(n() >= 2) %>%
  summarise(combinations = list(combn(Text_Standard, 2, simplify = FALSE)),
            .groups = 'drop') %>%
  unnest(combinations) %>%
  mutate(source = map_chr(combinations, 1),
         target = map_chr(combinations, 2)) %>%
  select(source, target, DocId)

edges_weighted_person <- edges_person %>%
  group_by(source, target) %>%
  summarise(weight = n(), doc_ids = list(DocId), .groups = 'drop')

nodes_person <- final_results_person_standardized %>%
  distinct(Text_Standard) %>%
  rename(id = Text_Standard) %>%
  mutate(degree = map_int(id, ~sum(final_results_person_standardized$Text_Standard == .x)),
         docs = map(id, ~unique(final_results_person_standardized$DocId[final_results_person_standardized$Text_Standard == .x])))

# 权重阈值分析
weight_threshold <- 4
edges_filtered_person <- edges_weighted_person %>% filter(weight > weight_threshold)

nodes_in_edges_person <- unique(c(edges_filtered_person$source, 
                                   edges_filtered_person$target))
nodes_filtered_person <- nodes_person %>% filter(id %in% nodes_in_edges_person)

# 创建P2P网络
g_person <- tbl_graph(nodes = nodes_filtered_person,
                      edges = edges_filtered_person,
                      directed = FALSE) %>%
  activate(nodes) %>%
  mutate(degree_network = centrality_degree(),
         betweenness = centrality_betweenness(),
         closeness = centrality_closeness())

# 可视化P2P网络
set.seed(123)

p_person_basic <- ggraph(g_person, layout = 'fr') +
  geom_edge_link(aes(width = weight), alpha = 0.3, color = 'gray60') +
  geom_node_point(aes(size = degree_network), color = 'steelblue', alpha = 0.8) +
  geom_node_text(aes(label = id), repel = TRUE, size = 3, max.overlaps = 20) +
  scale_edge_width(range = c(0.5, 3)) +
  scale_size_continuous(range = c(2, 12)) +
  theme_graph() +
  labs(title = '人物共现网络 (P2P Network)',
       subtitle = paste('Weight >', weight_threshold),
       size = 'Network Degree')

print(p_person_basic)
ggsave("P2P_basic.png", p_person_basic, width = 12, height = 8, dpi = 500)

# 介数中心性分析
p_person_betweenness <- ggraph(g_person, layout = 'fr') +
  geom_edge_link(aes(width = weight), alpha = 0.2, color = 'gray70') +
  geom_node_point(aes(size = degree_network, color = betweenness), alpha = 0.8) +
  geom_node_text(aes(label = id), repel = TRUE, size = 3, max.overlaps = 20) +
  scale_edge_width(range = c(0.5, 3)) +
  scale_size_continuous(range = c(2, 12)) +
  scale_color_gradient(low = 'lightblue', high = 'red', name = 'Betweenness\n(桥梁作用)') +
  theme_graph() +
  labs(title = '人物网络 - Betweenness分析',
       subtitle = '红色 = 关键桥梁人物',
       size = 'Degree')

print(p_person_betweenness)
ggsave("P2P_betweenness.png", p_person_betweenness, width = 12, height = 8, dpi = 300)

# 社区检测
communities_person <- cluster_louvain(as.igraph(g_person))

g_person_community <- g_person %>%
  activate(nodes) %>%
  mutate(community = as.factor(membership(communities_person)))

p_person_community <- ggraph(g_person_community, layout = 'fr') +
  geom_edge_link(aes(width = weight), alpha = 0.2, color = 'gray70') +
  geom_node_point(aes(size = degree_network, color = community), alpha = 0.8) +
  geom_node_text(aes(label = id), repel = TRUE, size = 2.5, max.overlaps = 15) +
  scale_edge_width(range = c(0.5, 3)) +
  scale_size_continuous(range = c(2, 12)) +
  theme_graph() +
  labs(title = '人物网络社区结构',
       subtitle = paste('检测到', length(unique(membership(communities_person))), '个社区'),
       color = 'Community', size = 'Degree')

print(p_person_community)
ggsave("P2P_community.png", p_person_community, width = 12, height = 8, dpi = 500)

# 导出P2P结果
write.csv(edges_filtered_person, 'edges_person_filtered.csv', row.names = FALSE)
write.csv(g_person %>% activate(nodes) %>% as_tibble(), 
          'nodes_person_filtered.csv', row.names = FALSE)
write.csv(g_person_community %>% activate(nodes) %>% as_tibble() %>%
            select(id, degree, degree_network, betweenness, community) %>%
            arrange(community, desc(degree_network)),
          'person_community_info.csv', row.names = FALSE)

# ============================================================================
# 六、E2E网络（事件-事件网络）
# ============================================================================

# 提取EVENT实体
zsh_EveStrt <- zsh_sbconc_ner_6 %>% 
  filter(Type == "EVENT") %>%
  select(-7) %>%
  unique()  # 40761条

write_csv(zsh_EveStrt, "zsh_EveStrt.csv")

event_data <- zsh_EveStrt[zsh_EveStrt$Type == "EVENT", ] %>% unique()

# 事件标准化
final_results_event_1 <- event_data %>%
  mutate(Text_Standard = case_when(
    str_detect(Text, "五卅") ~ "五卅运动",
    str_detect(Text, "第三次全國代表大會|三全大會") ~ "第三次全國代表大會",
    str_detect(Text, "濟南慘案|濟案|五三慘案") ~ "濟南慘案",
    str_detect(Text, "南京路慘案|上海慘案|滬案|滬慘案|上海慘殺案") ~ "五卅运动",
    str_detect(Text, "五四") ~ "五四运动",
    str_detect(Text, "六三運動|六三") ~ "五四运动",
    str_detect(Text, "江浙戰事|江浙戰|江浙戰爭|江浙之戰|東南戰事|浙戰|第二次江浙戰|東南戰") ~ "江浙戰爭",
    str_detect(Text, "國民革命|北伐勝利大會|北伐勝利|北伐將士大會|中國國民革命|北伐") ~ "北伐",
    str_detect(Text, "遠東運動會|遠東運動大會|萬國運動會") ~ "遠東運動會",
    str_detect(Text, "中華國貨展覽會|國貨運動大會|國貨展覽會") ~ "國貨運動",
    str_detect(Text, "歐洲大戰|世界大戰|歐戰|第一次世界大戰") ~ "第一次世界大戰",
    str_detect(Text, "華府會議|華盛頓會議|太平洋會議|平洋會議|華會") ~ "華盛頓會議",
    str_detect(Text, "庚申赭亂|申赭亂") ~ "庚申赭乱",
    str_detect(Text, "臨城刦案|臨城案|臨城事件|臨城刼案|臨城匪案") ~ "臨城大刼案",
    str_detect(Text, "九六|九六公債") ~ "九六公債",
    str_detect(Text, "漢案|漢口慘案") ~ "漢口慘案",
    str_detect(Text, "國恥紀念會|五九國恥|國恥|五九") ~ "國恥紀念會",
    str_detect(Text, "中華民國八團體國是會議|上海國是會議") ~ "上海國是會議",
    TRUE ~ Text
  )) %>%
  add_count(Text_Standard, name = "freq") %>%
  arrange(desc(freq), Text_Standard)

write.csv(final_results_event_1, "final_results_event_1.csv")

# 生成E2E边和节点
edges_event <- final_results_event_1 %>%
  group_by(DocId) %>%
  distinct(Text_Standard) %>%
  filter(n() >= 2) %>%
  summarise(combinations = list(combn(Text_Standard, 2, simplify = FALSE)),
            .groups = 'drop') %>%
  unnest(combinations) %>%
  mutate(source = map_chr(combinations, 1),
         target = map_chr(combinations, 2)) %>%
  select(source, target, DocId)

edges_weighted_event <- edges_event %>%
  group_by(source, target) %>%
  summarise(weight = n(), doc_ids = list(DocId), .groups = 'drop')

nodes_event <- final_results_event_1 %>%
  distinct(Text_Standard) %>%
  rename(id = Text_Standard) %>%
  mutate(degree = map_int(id, ~sum(final_results_event_1$Text_Standard == .x)),
         docs = map(id, ~unique(final_results_event_1$DocId[final_results_event_1$Text_Standard == .x])))

# 权重阈值分析
weight_threshold <- 10
edges_filtered_event <- edges_weighted_event %>% filter(weight > weight_threshold)

nodes_in_edges_event <- unique(c(edges_filtered_event$source, 
                                  edges_filtered_event$target))
nodes_filtered_event <- nodes_event %>% filter(id %in% nodes_in_edges_event)


# 只导出非 list 列
list_columns <- sapply(nodes_filtered_event, is.list)

write.csv(
  nodes_filtered_event[, !list_columns],
  "nodes_filtered_event.csv",
  row.names = FALSE
)

# ===============================
# 读回人工清洗后的节点表（核心节点集）
# ===============================
nodes_filtered_event_Ed<- read_csv("nodes_filtered_event_Ed.csv")

# 边数据对齐
edges_filtered_event <- edges_weighted_event %>%
  filter(
    source %in% nodes_filtered_event_Ed$id,
    target %in% nodes_filtered_event_Ed$id,
    weight > weight_threshold
  )

nodes_in_edges_event <- unique(c(
  edges_filtered_event$source,
  edges_filtered_event$target
))

nodes_final_event <- nodes_filtered_event_Ed %>%
  filter(id %in% nodes_in_edges_event)



# ==========================================================================



# 创建E2E网络
g_event <- tbl_graph(nodes = nodes_filtered_event_Ed,
                     edges = edges_filtered_event,
                     directed = FALSE) %>%
  activate(nodes) %>%
  mutate(degree_network = centrality_degree(),
         betweenness = centrality_betweenness(),
         closeness = centrality_closeness())

# 可视化E2E网络
set.seed(123)

p_event_basic <- ggraph(g_event, layout = 'kk') +
  geom_edge_link(aes(width = weight), alpha = 0.3, color = 'gray60') +
  geom_node_point(aes(size = degree_network), color = 'forestgreen', alpha = 0.8) +
  geom_node_text(aes(label = id), repel = TRUE, size = 3, max.overlaps = 20) +
  scale_edge_width(range = c(0.5, 3)) +
  scale_size_continuous(range = c(2, 12)) +
  theme_graph() +
  labs(title = '事件共现网络 (E2E Network)',
       subtitle = paste('Weight >', weight_threshold),
       size = 'Network Degree')

print(p_event_basic)
ggsave("E2E_basic.png", p_event_basic, width = 12, height = 8, dpi = 500)

# 介数中心性分析
p_event_betweenness <- ggraph(g_event, layout = 'fr') +
  geom_edge_link(aes(width = weight), alpha = 0.2, color = 'gray70') +
  geom_node_point(aes(size = degree_network, color = betweenness), alpha = 0.8) +
  geom_node_text(aes(label = id), repel = TRUE, size = 3, max.overlaps = 20) +
  scale_edge_width(range = c(0.5, 3)) +
  scale_size_continuous(range = c(2, 12)) +
  scale_color_gradient(low = 'lightgreen', high = 'darkred', name = 'Betweenness\n(桥梁作用)') +
  theme_graph() +
  labs(title = '事件网络 - Betweenness分析',
       subtitle = '红色 = 连接不同事件群的关键事件',
       size = 'Degree')

print(p_event_betweenness)
ggsave("E2E_betweenness.png", p_event_betweenness, width = 12, height = 8, dpi = 500)

library(dplyr)
library(tidygraph)
library(igraph)
library(ggraph)
library(ggplot2)
library(ggforce)

# ===============================
# 1. Leiden 社区检测
# ===============================
communities_event <- cluster_leiden(
  as.igraph(g_event),
  resolution_parameter = 0.5
)

# ===============================
# 2. k-core
# ===============================
core_vals <- coreness(as.igraph(g_event))

# ===============================
# 3. 写回 tidygraph
# ===============================
g_event_community <- g_event %>%
  activate(nodes) %>%
  mutate(
    degree_network = centrality_degree(),
    community = as.factor(membership(communities_event)),
    coreness  = core_vals,
    k_layer = case_when(
      coreness >= quantile(coreness, 0.75, na.rm = TRUE) ~ "Core",
      coreness >= quantile(coreness, 0.40, na.rm = TRUE) ~ "Middle",
      TRUE                                              ~ "Peripheral"
    )
  )

# ===============================
# 4. 可视化：Leiden × k-core（双凸包）
# ===============================
p_event_community <- ggraph(g_event_community, layout = "stress") +
  
  # —— Leiden 社区凸包（叙事结构）——
  geom_mark_hull(
    aes(x, y, fill = community, group = community),
    concavity = 6,
    expand = unit(4, "mm"),
    alpha = 0.20,
    color = NA,
    show.legend = TRUE
  ) +
  
  # —— k-core 圈层凸包（结构层级）——
  geom_mark_hull(
    aes(x, y, group = k_layer),
    concavity = 8,
    expand = unit(6, "mm"),
    fill = NA,
    color = "black",
    linetype = "dashed",
    linewidth = 0.6,
    show.legend = FALSE
  ) +
  
  # —— 边（不显示 weight 图例）——
  geom_edge_link(
    aes(width = weight),
    alpha = 0.25,
    color = "gray65",
    show.legend = FALSE
  ) +
  scale_edge_width(range = c(0.4, 2.8)) +
  
  # —— 节点：大小 = degree，颜色 = 社区，形状 = k-core —— 
  geom_node_point(
    aes(
      size  = degree_network,
      color = community,
      shape = k_layer
    ),
    alpha = 0.95
  ) +
  
  # —— 关闭 size 图例（degree 不显示）——
  scale_size_continuous(range = c(2.5, 11), guide = "none") +
  
  # —— 标签 —— 
  geom_node_text(
    aes(label = id_Pinyin),
    repel = TRUE,
    size = 2.6,
    max.overlaps = 15
  ) +
  
  # —— 图例：只保留社区 & k-core —— 
  scale_color_brewer(
    palette = "Set2",
    name = "Leiden Community"
  ) +
  scale_fill_brewer(
    palette = "Set2",
    name = "Leiden Community"
  ) +
  scale_shape_manual(
    values = c(
      "Core"       = 19,
      "Middle"     = 17,
      "Peripheral" = 1
    ),
    name = "k-core Layer"
  ) +
  
  guides(
    size = "none",
    edge_width = "none"
  ) +
  
  theme_graph(base_family = "serif") +
  labs(
    # title = "Event Network Structure in Shenbao",
    # subtitle = "Narrative communities (Leiden) and structural layers (k-core)",
    caption = "Colored hulls indicate narrative communities; dashed hulls indicate k-core structural layers"
  )

print(p_event_community)


ggsave("E2E_community.png", p_event_community, width = 12, height = 8, dpi = 300)

# 导出E2E结果
write.csv(edges_filtered_event, 'edges_event_filtered.csv', row.names = FALSE)
write.csv(g_event %>% activate(nodes) %>% as_tibble(), 
          'nodes_event_filtered.csv', row.names = FALSE)
write.csv(g_event_community %>% activate(nodes) %>% as_tibble() %>%
            select(id, degree, degree_network, betweenness, community) %>%
            arrange(community, desc(degree_network)),
          'event_community_info.csv', row.names = FALSE)

# ============================================================================
# 保存工作空间
# ============================================================================

save.image('zsh_chamber.RData')

cat("\n========== 分析完成 ==========\n")
cat("生成的主要文件:\n")
cat("  O2O网络: O2O.png, network_no_center.png\n")
cat("  P2P网络: P2P_basic.png, P2P_betweenness.png, P2P_community.png\n")
cat("  E2E网络: E2E_basic.png, E2E_betweenness.png, E2E_community.png\n")
cat("  数据文件: 各类CSV和RData文件\n")
