library(tidyverse)
library(tidygraph)
library(igraph)
library(ggplot2) # 
library(ggraph)
library(sysfonts)
library(showtextdb)
library(showtext)
library(readxl)

# 

# data <- read_excel("D:/学习/实习内容/微博实习/知识图谱图/", range = "A1:C42")
# import the data
data <-  readxl::read_excel("../hotspots&IP.xls",range="A1:C74")
# define the data_name
data <- data %>% rename(topic = `打标`, 
                        code = `在榜时长`,
                        pkg_name = `热搜话题词`)
data<-data%>%mutate(code=as.numeric(as.character(code)))
# define the nodes
popular_topics <- c(
# add your topic names
  '熬夜场景','怀旧热点','疾病预防','健康科普议题','美妆产品种草','年龄情绪议题','年轻人养生热点','品牌商业话题','时事热点','特殊节日节点','饮食健康','影视综艺热点','娱乐明星热点')

number_of_pkgs <-dim(data)[1]

# find largest packages written in popular languages
top_packages <- data %>%
  filter(topic %in% popular_topics) %>%
  group_by(pkg_name) %>%
  summarize(total_code = sum(code)) %>%
  #arrange(desc(total_code)) %>%
  head(number_of_pkgs) %>%
  select(pkg_name, total_code)

# all popular languages per package
top_topics_per_pkg <- data %>%
  filter(
    pkg_name %in% top_packages$pkg_name,
    language %in% popular_topics
  ) %>%
  arrange(pkg_name, desc(code)) %>%
  group_by(pkg_name) %>%
  mutate(
    main = row_number() == 1, # main language of package should be opaque
    total_code = sum(code)
  ) %>%
  ungroup() %>%
  select(topic, pkg_name, code, total_code, main)

# only following languages found in given packages
(top_topics <- top_topics_per_pkg %>%
    pull(topic) %>%
    unique %>%
    sort)

top_topic_colors <- c(
  '#A2A273',
  '#8B8B8B',
  '#BEA564',
  '#768CA2',
  '#8A9A8A',
  '#A17777',
  '#8B809F',
  '#939391',
  '#c1cbd7',
  '#c5b8a5'
)

top_topic_colors <- c(
  '#FF1B1B', 
  '#C00000',
  '#FFBFBF',
  '#C2C2C2',
  '#620000'# ,
  # '#7F7F7F',
  # '#0C0C0C'
)

top_topic_colors <- c(
  '#00008B', 
  '#3c4a82',
  '#5865c4',
  '#969eda',
  '#A6B3F2',
  '#9FCCF9',
  '#1254B6',
  '#86AEDA',
  '#557FCB',
  '#8BB6DD',
  '#99CCFF',
  '#198CFF',
  '#3797D9'
  # '#7F7F7F',
  # '#0C0C0C'
)


# 热点+联名
names(top_topic_colors) <- c(
  # add the topics name
  '熬夜场景','怀旧热点','疾病预防','健康科普议题','美妆产品种草','年龄情绪议题','年轻人养生热点','品牌商业话题','时事热点','特殊节日节点','饮食健康','影视综艺热点','娱乐明星热点')


# draw the plot
edges1 <- top_topics_per_pkg %>%
  transmute(from = topic, to = pkg_name, total_code = code, main)

edges2 <- top_topics_per_pkg %>%
  count(topic, wt = code, name = 'total_code') %>%
  transmute(
    from = '',
    to = language,
    total_code,
    main = TRUE
  )

vertices1 <- top_topics_per_pkg %>%
  filter(main) %>%
  transmute(
    node = pkg_name, topic, total_code, level = 1
  )

vertices2 <- edges2 %>%
  transmute(
    node = to, topic = to, total_code, level = 2
  )


vertices <- bind_rows(vertices1, vertices2)%>%
  mutate(
    radius = total_code**(1), # scaling circles
    topic = factor(topic, names(top_topic_colors))
  ) %>%
  arrange(level, topic, node)

graph <- igraph::graph_from_data_frame(edges1, vertices = vertices)

# create custom layout by updating existing circle layout
layout <- ggraph::create_layout(graph, layout = 'circle')


outer_circle <- layout %>%
  filter(level == 1) %>%
  mutate(topic = factor(topic, names(top_topic_colors))) %>%
  arrange(topic, desc(name)) %>%
  mutate(
    x = cos((row_number() - 1) / number_of_pkgs * 2 * pi),
    y = sin((row_number() - 1) / number_of_pkgs * 2 * pi)
  )

# positioning circle centers manually by specifying polar coords
# if need more circles, add more angles and radii
angles <- c(16, 160, 179, 200,260, 127, 285, 210, 342, 355)
radii <- c(0.18, 0.75, 0.33,0.16, 0.44, 0.35, 0.35, 0.6, 0.56, 0.8)


# hotspots & IP
# Version 1
angles <- c(30, 237, 102, 315, 328)
radii <- c(0.14, 0.31, -0.21, -0.45, 0.20)
# corresponding labels.
angles <- c(0, 16, 200,127,120, 179, 210, 225, 250, 0,328,335,0)
radii <- c(0.8, 0.18, 0.16,0.35, 0.75, 0.33, 0.6, 0.35, 0.35, 0,0.20,0.56,0.30)

centers <- tibble(
  x = radii * cos(angles / 180 * pi),
  y = radii * sin(angles / 180 * pi)
)

inner_circle <- bind_cols(centers, select(filter(layout, level != 1), -x, -y))

layout[] <- bind_rows(outer_circle, inner_circle) %>%
  arrange(.ggraph.index)


ggraph(layout) + 
  geom_edge_diagonal(
    aes(edge_color = node1.topic, edge_alpha = as.factor(main)),
    edge_width = 1.5, show.legend = FALSE
  ) +
  geom_node_point(
    aes(
      size = radius ** 1.2, 
      # size = ifelse(radius >= 100000, radius**1.03, radius),
      # size = ifelse(level == 2, radius-20000, radius/2),
      color = topic),
    alpha = 0.6, show.legend = FALSE
  ) +
  geom_node_text(aes(x = x, y = y, label = name,filter=name%in%top_topics), size = 0, hjust = 'outward',
  #repel = TRUE
  ) +
  scale_edge_color_manual(values = top_topic_colors) +
  scale_color_manual(values = top_topic_colors) +
  scale_size_area(max_size =80) +
  scale_edge_alpha_manual(values = c(0.5, 1)) +
  coord_fixed(clip = 'off') +
  theme_void()+
  geom_node_text(aes(
    x = 1.035* x,
    y = 1.05* y, 
    label = name, 
# fill in your topics
    filter = !(name %in% c('新年来接健康好彩头',
                           '18个年轻人关心的健康常识',
                           '数九寒天健康日历',
                           '曝百位大咖抗老真相',
                           '一个人老了的标志是什么',
                           '所有女生的抗老宝藏',
                           '秋冬抗老护肤榜',
                           '医生给出开学健康锦囊',
                           '健康过暑假',
                           '雪天8大健康提醒',
                           '不同年龄阶段怎么抗老',
                           '垮脸问题一网打尽',
                           top_topics)
    )), 
    size = 6, hjust = 'outward')+
  geom_node_text(aes(
    x = 1.035* x,
    y = 1.077* y, 
    label = name, 
    filter = (name %in% c('健康过暑假',
                          '雪天8大健康提醒',
                          '不同年龄阶段怎么抗老',
                          '垮脸问题一网打尽'))), 
    size = 6, hjust = 'outward')+
  geom_node_text(aes(
    x = 1.035* x,
    y = 1.14* y, 
    label = name, 
    filter = (name %in% c('新年来接健康好彩头',
                          '18个年轻人关心的健康常识',
                          '数九寒天健康日历',
                          '曝百位大咖抗老真相'))), 
    size = 6, hjust = 'outward')+
  geom_node_text(aes(
    x = 1.035* x,
    y = 1.1* y, 
    label = name, 
    filter = (name %in% c('时一个人老了的标志是什么'))), 
    size = 6, hjust = 'outward')+
  geom_node_text(aes(
    x = 1.035* x,
    y = 1.102* y, 
    label = name, 
    filter = (name %in% c('汪苏泷感受到了伍佰的快乐',
                          '蔡依林恭喜马龙夺冠',
                          '许昕解说 好笑'))), 
    size = 6, hjust = 'outward')+
  geom_node_text(aes(
    x = 1.035* x,
    y = 1.096* y, 
    label = name, 
    filter = (name %in% c('一个人老了的标志是什么',
                          '所有女生的抗老宝藏',
                          '秋冬抗老护肤榜',
                          '医生给出开学健康锦囊'))), 
    size = 6, hjust = 'outward')+
  geom_node_text(aes(
    x = 1.035* x,
    y = 1.08* y, 
    label = name, 
    filter = (name %in% c('8090课间十分钟能有多快乐'))),
    size = 6, hjust = 'outward')
  

# save the plot
ggsave(
  'D:/学习/实习内容/微博实习/知识图谱图/梦幻-微博热搜/dream_15.png',width= 40, height= 20,dpi=300)


