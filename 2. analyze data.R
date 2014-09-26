library(plyr)
library(ggplot2)
library(googleVis)
library(scales)
options(stringsAsFactors = F)
setwd('/Users/Runze/Google Drive/Weibo')

#weibo = read.csv('weibo_tweets.csv', header = T)
load('weibo_tweets.RData')

#analyze tweets per user
ids = ddply(weibo, .(id), summarise, count = length(id))
ids_hist =
  ggplot(ids, aes(x = count, y = ..density..)) +
  geom_histogram(fill = '#6baed6', col = '#bdd7e7') +
  geom_density() +
  ggtitle('Number of tweets per user')
ggsave(file = 'ids_hist.jpeg', ids_hist)

#analyze tweets per region
regions = ddply(weibo, .(region), summarise, count = length(region))
regions_hist =
  ggplot(regions, aes(x = count, y = ..density..)) +
  geom_histogram(binwidth = 3000, fill = '#6baed6', col = '#bdd7e7') +
  geom_density() +
  ggtitle('Number of tweets per region')
ggsave(file = 'regions_hist.jpeg', regions_hist)

#create a new version of the dataset by eliminating regions with tweets less than the 25th percentile
large_regions = regions$region[regions$count >= quantile(regions$count)[2]]
large_regions_en = c('Yunnan', 'Inner Mongolia', 'Taiwan', 'Jilin',
                   'Tianjing', 'Ningxia', 'Anhui', 'Shanxi', 'Guangxi',
                   'Xinjiang', 'Jiangxi', 'Hebei', 'Hainan',
                   'Hunan', 'Macao', 'Gansu', 'Tibet', 'Guizhou',
                   'Chongqing', 'Shaanxi', 'Qinghai', 'Heilongjiang')
large_regions_df = data.frame(cbind(large_regions, large_regions_en))

weibo_region = subset(weibo, region %in% large_regions)
weibo_region = merge(weibo_region, large_regions_df, by.x = 'region', by.y = 'large_regions')
names(weibo_region)[4] = 'region_en'

#define topics
topics = list()
topics[[1]] = c('books', '读书|看书|好书|阅读')
topics[[2]] = c('art', '艺术|音乐|绘画|油画|水彩|歌剧|百老汇|交响')
topics[[3]] = c('money', '金钱|赚钱|有钱|多钱|没钱|钞票|大款|富人')
topics[[4]] = c('love', '爱情|爱你|爱人|爱他|爱她')
topics[[5]] = c('marriage', '婚|嫁|娶')
topics[[6]] = c('politics', '政治|政府')
topics[[7]] = c('charity', '慈善|捐款|募捐')
topics[[8]] = c('academic', '教育|学习|念书|在学|攻读|学位|读研|考研|读博|申博|论文|毕业|研究')
topics[[9]] = c('tech/\ninnovation', '科技|创新|创业')
topics[[10]] = c('career/\nambitions', '事业|成就|有成|理想|野心|拼搏|奋斗')
topics[[11]] = c('news', '新闻|时事')
topics[[12]] = c('food', '美食|好吃|好喝|美味|名吃')
topics[[13]] = c('celebrity', '名星|影星|歌星|男星|女星|演员|好莱坞|hollywood')
topics[[14]] = c('tv/\nmovie', '电视|剧|电影|大片|新片')
topics[[15]] = c('horoscope', '星座')
topics[[16]] = c('weight\nloss', '减肥|瘦')

topics[[17]] = c('iphone', 'iphone|苹果手机|苹果店|果粉')
topics[[18]] = c('xiaomi', 'xiaomi|小米手机')
topics[[19]] = c('samsung', 'samsung|galaxy|三星手机')

topics[[20]] = c('dog', '狗|犬|汪星人')
topics[[21]] = c('cat', '猫|喵星人')

#create html table for topics and keywords
topic = sapply(topics, function(t) t[1])
kw = sapply(topics, function(t) t[2])
topic_kw = data.frame(cbind(gsub('\n', '', gsub('/', ', ', topic)),
                            gsub('\\|', ', ', kw)))
names(topic_kw) = c('topic', 'keyword')
t = gvisTable(topic_kw)
plot(t)

#function to calculate the % of mentions
mention = function(topic, text) {
  m = length(grep(topic, text, ignore.case = T))
  return(m / length(text))
}

#overall mentions
mentions = sapply(topics, function(t) mention(t[2], weibo$tweet))

plot_mentions = function(slice, title, width) {
  m = data.frame(cbind(topic[slice], mentions[slice]))
  names(m) = c('topic', 'mentions')
  m$mentions = as.numeric(m$mentions)
  m = m[order(m$mentions, decreasing = T), ]
  m$topic = factor(m$topic, level = m$topic)
  
  m_plot =
    ggplot(m, aes(x = topic, y = mentions)) +
    geom_bar(stat = 'identity', fill = '#3182bd') +
    scale_y_continuous(labels = percent) +
    ggtitle(sprintf('Mentions of %s topics', title))
  ggsave(file = sprintf('mention_%s.jpeg', title), m_plot, width = width, height = 5)
}

plot_mentions(1:16, 'broad', 10)
plot_mentions(17:19, 'phone', 5)
plot_mentions(20:21, 'dog vs. cat', 5)

#mentions per region
mention_region = function(topic) {
  daply(weibo_region, .(region_en), function(x) mention(topic, x$tweet))
}

mentions_region = lapply(topics, function(t) mention_region(t[2]))

#find the topics with mentions varying the most across regions
mentions_region_sd = sapply(mentions_region, sd)
mentions_region = mentions_region[order(mentions_region_sd, decreasing = T)]
mentions_region_var = mentions_region[1:3]
topic_region = topic[order(mentions_region_sd, decreasing = T)]
topic_region_var = topic_region[1:3]

#chart
for (i in 1:3) {
  m = data.frame(cbind(names(mentions_region_var[[i]]), mentions_region_var[[i]]))
  names(m) = c('region', 'mention')
  
  #find the top 10 regions
  m = m[order(m$mention, decreasing = T), ]
  m = m[1:10, ]
  m$mention = as.numeric(m$mention)
  m$region = factor(m$region, level = m$region)
  
  m_plot =
    ggplot(m, aes(x = region, y = mention)) +
    geom_bar(stat = 'identity', fill = '#807dba') +
    scale_y_continuous(labels = percent) +
    ggtitle(sprintf('Mentions of %s by region', gsub('\n', '', topic_region_var[i])))
  ggsave(file = sprintf('mention_region_%s.jpeg', i), m_plot, width = 10, height = 5)
}