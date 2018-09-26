rm(list = ls())
gc()
setwd("C:/工作/可视化/depression")
library(ggmap)
library(data.table)
library(ggplot2)
library(ggthemes)
library(maps)
library(tibble)
library(lubridate)
library(gganimate)
library(animation)
library(dplyr)
library(stringr)
# library(easyGgplot2)
# install.packages("gridExtra")
library(gridExtra)
library(ggthemr)

#fun
####alter zangnan location
alter_worldmap_zangnan <- function(){
  world_map <- map_data("world")
  setDT(world_map)
  China <- unique(world_map[region == "China" & group == 430, c(1, 2)])
  India <- unique(world_map[region == "India" & group == 839, c(1, 2)])
  interact_area <- rbind(China, India)[, .(.N), by = .(long, lat)][N == 2]
  interact_area <- interact_area[long >= 90]
  # plot(interact_area[, 1:2])
  
  library(OpenImageR)
  zangnan <- readImage("zangnan.png")
  # zangnan <- rgb_2gray(zangnan)
  zangnan <- zangnan[, , 3]
  zangnan[zangnan > 0.5] <- 1
  border_pos <- which(zangnan != 1, arr.ind = T)
  # imageShow(zangnan)
  border_pos <- as.data.table(border_pos)
  # plot(border_pos[, .(col, 100 - row)])
  border_pos <- border_pos[, .(col, 100 - row)]
  # col V2
  # 1:  29 26
  # col V2
  # 1: 170 75
  # 26.854853, 92.114564
  # 28.535393, 97.580710
  border_pos[V2 == 26 & col == 29]
  border_pos[V2 == max(V2)]
  border_pos <- border_pos[, .(long = (col - 29) * (97.580710 - 92.114564)/(170 - 29) + 92.11456, 
                               lat = (V2 - 26) * (28.535393 - 26.854853)/(75 - 26) + 26.854853)]
  
  start <- rowSums((border_pos - data.table(seq = 1 : dim(border_pos)[1], 
                                            data.table::last(interact_area[, c(1,2)])[1,1],
                                            data.table::last(interact_area[, c(1,2)])[1,2])[, .(long, lat)])^2) %>% sqrt() %>% which.min()
  finish <- rowSums((border_pos - data.table(seq = 1 : dim(border_pos)[1], 
                                             data.table::first(interact_area[, c(1,2)])[1,1],
                                             data.table::first(interact_area[, c(1,2)])[1,2])[, .(long, lat)])^2) %>% sqrt() %>% which.min()
  # border_pos[start :finish] %>% plot()
  new_interact_border <- border_pos[start :finish]
  new_interact_border[, "id"] <- paste(new_interact_border$long, new_interact_border$lat, sep = "_")
  
  China <- world_map[region == "China" & group == 430]
  India <- world_map[region == "India" & group == 839]
  
  China[, "id"] <- paste(China$long, China$lat, sep = "_")
  interact_area[, "id"] <- paste(interact_area$long, interact_area$lat, sep = "_")
  loworder <- min(China[id %in% interact_area$id, order])
  uporder <- max(China[id %in% interact_area$id, order])
  China <- China[ !id %in% interact_area$id]
  China <- rbind(China, new_interact_border[, .(long, lat, group = 430, 
                                                order = seq(uporder - 0.1, loworder + 0.1, length.out = dim(new_interact_border)[1]),
                                                region = "China", subregion = NA, id)])[order(order)]
  
  # China <- China[, .(long, lat, group = 430, order = 1, region = "China", subregion = "China")]
  
  India[, "id"] <- paste(India$long, India$lat, sep = "_")
  interact_area[, "id"] <- paste(interact_area$long, interact_area$lat, sep = "_")
  loworder <- min(India[id %in% interact_area$id, order])
  uporder <- max(India[id %in% interact_area$id, order])
  India <- India[ !id %in% interact_area$id]
  India <- rbind(India, new_interact_border[, .(long, lat, group = 839, 
                                                order = seq(loworder + 0.1, uporder - 0.1, length.out = dim(new_interact_border)[1]),
                                                region = "India", subregion = NA, id)])[order(order)]
  
  India <- India[ !(between(long, 90 ,93) & lat > 26.98)]
  # India <- India[, .(long, lat, group = 839, order = 2, region = "India", subregion = "India")]
  
  world_map <- world_map[!((region == "China" & group == 430) | (region == "India" & group == 839))]
  world_map <- rbind(world_map, China[, -"id"], India[, -"id"])
  return(world_map)
}
plotdata <- fread("C:/工作/可视化/depression/share-with-depression.csv") 
colnames(plotdata)[c(1, 4)] <- c("region", "share")
plotdata[, "region"] <- gsub("United States", "USA", plotdata$region)
plotdata[, "region"] <- gsub("Cote d'Ivoire", "Ivory Coast", plotdata$region)
plotdata[, "region"] <- gsub("United Kingdom", "UK", plotdata$region)
plotdata[region == "Taiwan", "share"] <- plotdata[region == "China", share]
plotdata[, "fan"] <- cut(plotdata$share, breaks = seq(2, 7, by = 0.5))
labels <- rep(str_split("2%
2.5%
3%
3.5%
4%
4.5%
5%
5.5%
6%
>6.5%", "\n")[[1]], each = 2)[-c(1,20)] %>% matrix(ncol=2, byrow=T)
labels <- c(paste(labels[, 1], "~", labels[, 2]) %>% str_replace_all(">", ""), "> 6.5%")

labelsdata <- data.table(labels_n = sort(as.numeric(unique(plotdata$fan))), 
                         label = labels)
plotdata[, "labels_n"] <- as.numeric(plotdata$fan)
plotdata <- merge(plotdata, labelsdata, all.x = TRUE)

# world_map <- map_data("world")
world_map <- alter_worldmap_zangnan()
world_map[, "region"] <- gsub("Democratic Republic of the Congo|Republic of Congo", "Congo", world_map$region)
# world_map <- world_map[region != "Antarctica"]
# world_map[world_map$group == 1425 & world_map$group == 1425, "region"] <- "Trinidad and Tobago"
setDT(world_map)
setkey(world_map, region)
setkey(plotdata, region)
world_map1 <- world_map
datalist <- list()
years <- unique(plotdata$Year)
for(i in 1 : length(years)){
  world_map1[, "Year"] <- years[i]
  datalist[[i]] <- merge(world_map1, plotdata[Year == years[i], .(region, label)], all.x = TRUE)
}
world_map <- rbindlist(datalist)

# world_map <- merge(world_map, plotdata[, .(region, Year, label)], all.x = TRUE, allow.cartesian = T)
world_map[is.na(label), "label"] <- "No data"


colorpanel = c("No data"   = "#F5F5F5",
               "2% ~ 2.5%" = "#D5E0F5",
               "2.5% ~ 3%" = "#C0D0F0",
               "3% ~ 3.5%" = "#ACC0EB",
               "3.5% ~ 4%" = "#97B0E6",
               "4% ~ 4.5%" = "#82A1E1",
               "4.5% ~ 5%" = "#6482C8",
               "5% ~ 5.5%" = "#4665AF",
               "5.5% ~ 6%" = "#254A97",
               "6% ~ 6.5%" = "#1E3E7E",
               "> 6.5%"    = "#183265")
titlecolor <- rep(colorpanel %>% tail(9), each = 3)
datepoint <- na.omit(unique(world_map$Year))
#save several plots
windowsFonts(A = windowsFont('等线'), B = windowsFont('微软雅黑'))
for (i in 1 : length(datepoint)) { 
  nian <- datepoint[i]
  plotname <- paste0("plot", nian, ".png")
  plotdata1 <- world_map[Year == nian]
  g2 <- ggplot(plotdata1) +
    geom_polygon(aes(x = long, y = lat, group = group, fill = label), colour = "grey60", size = .2) +
    guides(fill = guide_legend(reverse=TRUE)) +
    scale_fill_manual(values = colorpanel, na.value = "white",
                      limits = names(colorpanel),
                      # aesthetics = c("colour", "fill"), 
                      breaks = names(colorpanel)) +
    theme(panel.background = element_rect(fill = "transparent", colour = NA),
          plot.background = element_rect(fill = "transparent", colour = NA),
          plot.title = element_text(colour = titlecolor[i], family = "B", size = 50, hjust = 0.1, vjust = 1),
          legend.position = c(0.13, 0.39),
          legend.key = element_blank(),
          legend.text = element_text(family = "B"),
          legend.title = element_text(family = "B"),axis.line=element_blank(),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          plot.margin = margin(25, -50, -25, -50),#top,right,below,left
          axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank()) + xlim(-180, 200) +
    labs(fill = "") +
    ggtitle(nian)
  # ggsave(plotname, g2, width = 1500/100, height = 776/100)
  ggsave(plotname, g2, width = 1920 * 0.7 / 130, height = 1080/ 130 )
  print(nian)
}

#rgb to hex function
# x <- c("213 224 245",
#         "192 208 240",
#         "172 192 235",
#         "151 176 230",
#         "130 161 225",
#         "100 130 200",
#         "70 101 175",
#         "37 74 151",
#         "30 62 126",
#         "24 50 101")
# sapply(strsplit(x, " "), function(x)
#   rgb(x[1], x[2], x[3], maxColorValue=255))
##########################
#############每年人数条形图
plotdata <- fread("C:/工作/可视化/depression/number-with-depression-by-country.csv") 
colnames(plotdata)[c(1, 4)] <- c("region", "amount")
plotdata[, "region"] <- gsub("United States", "USA", plotdata$region)
plotdata[, "region"] <- gsub("Cote d'Ivoire", "Ivory Coast", plotdata$region)
plotdata[, "region"] <- gsub("United Kingdom", "UK", plotdata$region)
plotdata[region == "China", "amount"] <- plotdata[region == "China", amount] + plotdata[region == "Taiwan", amount]
world_map <- alter_worldmap_zangnan()
world_map[, "region"] <- gsub("Democratic Republic of the Congo|Republic of Congo", "Congo", world_map$region)
world_map <- world_map[region != "Antarctica"]
# world_map[world_map$group == 1425 & world_map$group == 1425, "region"] <- "Trinidad and Tobago"
plotdata <- plotdata[region %in% unique(world_map$region)]
plotdata[, "id"] <- paste0(plotdata$region, plotdata$Year)
setkey(plotdata, id)
# plotdata <- plotdata[order(Year, -amount)][, .SD[1:10], by = Year]
# plotdata <- plotdata[region %in% c("China", "India", "USA", "Russia", "Japan")]

###
plotdata1 <- fread("C:/工作/可视化/depression/prevalence-of-depression-males-vs-females.csv") 
colnames(plotdata1)[c(1, 4, 5)] <- c("region", "males", "females")
plotdata1[, "region"] <- gsub("United States", "USA", plotdata1$region)
plotdata1[, "region"] <- gsub("Cote d'Ivoire", "Ivory Coast", plotdata1$region)
plotdata1[, "region"] <- gsub("United Kingdom", "UK", plotdata1$region)
# plotdata1[region == "China", "amount"] <- plotdata1[region == "China", amount] + plotdata1[region == "Taiwan", amount]
plotdata1[, c("males", "females")] <- plotdata1[, c("males", "females")]/rowSums(plotdata1[, c("males", "females")])
plotdata1[, "id"] <- paste0(plotdata1$region, plotdata1$Year)
setkey(plotdata1, id)
plotdata_gender <- merge(plotdata, plotdata1[, .(id, males, females)], all.x = T)
plotdata_gender[, c("males", "females")] <- plotdata_gender[, c("males", "females")] * plotdata_gender[, amount]
plotdata_gender <- melt(plotdata_gender[, -c("id", "Code", "amount")], 
                        id = c("region","Year"),
                        variable.name = "gender",
                        value.name = "amount") 
plotdata_gender[gender == "males", "amount"] <- plotdata_gender[gender == "males", "amount"] * (- 1)

countries <- plotdata_gender[Year == 2016][order(-amount), region] %>% unique()
plotdata_gender[, "region"] <- factor(plotdata_gender$region, levels = c(countries))

#
colorpanel = c("No data"   = "#F5F5F5",
               "2% ~ 2.5%" = "#D5E0F5",
               "2.5% ~ 3%" = "#C0D0F0",
               "3% ~ 3.5%" = "#ACC0EB",
               "3.5% ~ 4%" = "#97B0E6",
               "4% ~ 4.5%" = "#82A1E1",
               "4.5% ~ 5%" = "#6482C8",
               "5% ~ 5.5%" = "#4665AF",
               "5.5% ~ 6%" = "#254A97",
               "6% ~ 6.5%" = "#1E3E7E",
               "> 6.5%"    = "#183265")
# datepoint <- na.omit(unique(world_map$Year))
#save several plots
windowsFonts(A = windowsFont('等线'), B = windowsFont('微软雅黑'))
#
country_fanwei <- head(countries, 10)

plotdata_gender1 <- plotdata_gender[region %in% country_fanwei]
# plotdata_gender1 <- plotdata_gender[Year == 2016 & region %in% country_fanwei]
# setorder(plotdata_gender1, gender)
n1 <- ggplot(plotdata_gender1, aes(x = region, y = amount, fill = gender)) + 
  geom_bar(stat = "identity") + 
  # geom_bar(subset = .(gender == "Male"), stat = "identity") +
  scale_fill_manual(values = c(females = "#E41A1C", males = "#377EB8"), name="") +
  scale_y_continuous(breaks = seq(-20000000, 30000000, 5000000), 
                     limits = c(-22000000, 35000000),
                     labels = paste0(as.character(c(seq(2000, 0, -500), seq(500, 3000, 500))), c(rep("万", 4), "", rep("万", 4)))) + 
  scale_x_discrete(labels = c("中\n国", "印\n度", "美\n国", "巴\n西", "印\n度\n尼\n西\n亚", "俄\n罗\n斯", 
                              "孟\n加\n拉\n国", "巴\n基\n斯\n坦", "尼\n日\n利\n亚", "日\n本")) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(family = "B", face = "bold", size = 16, color = "black"),
        axis.text.x = element_text(family = "B", face = "bold", size = 16, vjust = 1, color = "black"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.position = "none") +
  # coord_flip()# +
  # guides(fill = guide_legend(direction = "horizontal"))
  # geom_text(x=15, y=-1230, label="Scatter plot")
  annotate("text", x = 6.5, y = 20000000, label = '♀', size = 30, color = "#E41A1C", fontface = "bold") + 
  annotate("text", x = 6.5, y = -14000000, label = "♂", size = 30, color = "#377EB8")
# n1
flash <- n1 + transition_time(Year) + ease_aes('linear')
animate(flash, width = 1920 * 0.3, height = 1080, nframes = 2300, fps = 50)
################
##中国年龄分布
china_age_distribution <- fread("C:/工作/可视化/depression/china_depression_age_distribution.csv")
china_age_distribution[, "age"] <- factor(china_age_distribution[, c(age)], 
                                          levels = china_age_distribution[, unique(age)])
qplot(x = age, y = 1, data = china_age_distribution, size = percentage * 10, color = age) +
  scale_y_continuous(breaks = 1, limits = c(1, 1)) +
  scale_size_area(max_size=30,guide=FALSE) +
  transition_time(Year)
  #设置size_area大小
  scale_size_area(max_size=30,guide=FALSE)+
  scale_y_log10(limits=c(136000,136000000))+
  scale_x_log10(limits=c(5, 2000))+
  scale_colour_brewer(palette="Set1",
                      name="corporate name",
                      breaks=c("AAPL","ADBE","CSCO","IBM","MSFT","ORCL","SNE"),
                      labels=c("Apple","Adobe","Cisco","IBM","Microsoft","Oracle","SONY"))+
  annotate("text", x=280, y=100000000, label = as.character(Date[a+1]),size=5,color="grey")
#用paste来黏贴字符串，使其能够循环输出图像,并选择cairo-png输出，来抗锯齿生成高质量图片
ggsave(TEMP,file=paste((i+1000),".png",sep=""),width=5,height=4,type="cairo-png")
################
# saveVideo({
#   for (i in 1 : length(datepoint)) { 
#     nian <- datepoint[i]
#     plotname <- paste0("plot", nian, ".png")
#     plotdata1 <- world_map[Year == nian]
#     g2 <- ggplot(plotdata1) +
#       geom_polygon(aes(x = long, y = lat, group = group, fill = label), colour = "grey", size = .2) +
#       guides(fill = guide_legend(reverse=TRUE)) +
#       scale_fill_manual(values = colorpanel, na.value = "white",
#                         limits = names(colorpanel),
#                         # aesthetics = c("colour", "fill"), 
#                         breaks = names(colorpanel)) +
#       theme(panel.background = element_rect(fill = "transparent", colour = NA),
#             plot.background = element_rect(fill = "transparent", colour = NA),
#             plot.title = element_text(colour = "#7F3D17", family = "B", size = 50, hjust = 0.1, vjust = 1),
#             legend.position = c(0.14, 0.36),
#             legend.key = element_blank(),
#             legend.text = element_text(family = "B"),
#             legend.title = element_text(family = "B"),axis.line=element_blank(),
#             panel.grid.major = element_blank(), 
#             panel.grid.minor = element_blank(),
#             axis.text.x=element_blank(),
#             axis.text.y=element_blank(),
#             axis.ticks=element_blank(),
#             axis.title.x=element_blank(),
#             axis.title.y=element_blank()) + xlim(-180, 200) +
#       labs(fill = "") +
#       ggtitle(nian)
#     
#     plotdata_gender1 <- plotdata_gender[Year == nian & region %in% country_fanwei]
#     # setorder(plotdata_gender1, gender)
#     n1 <- ggplot(plotdata_gender1, aes(x = region, y = amount, fill = gender)) + 
#       geom_bar(subset = .(gender == "Female"), stat = "identity") + 
#       geom_bar(subset = .(gender == "Male"), stat = "identity") +
#       scale_fill_manual(values = c(females = "#E41A1C", males = "#377EB8"), name="") +
#       scale_y_continuous(breaks = seq(-20000000, 30000000, 5000000), 
#                          limits = c(-25000000, 35000000),
#                          labels = paste0(as.character(c(seq(20, 0, -5), seq(5, 30, 5))), "m")) + 
#       # scale_x_discrete(limits = c(country_fanwei, "")) +
#       theme(axis.title.x = element_blank(),
#             axis.title.y = element_blank(),
#             axis.line.x = element_blank(),
#             axis.line.y = element_blank(),
#             panel.grid.major = element_blank(), 
#             panel.grid.minor = element_blank(),
#             legend.position = "none") +
#       # coord_flip()# +
#       # guides(fill = guide_legend(direction = "horizontal"))
#       # geom_text(x=15, y=-1230, label="Scatter plot")
#       annotate("text", x = 6.5, y = 20000000, label = '♀', size = 30, color = "#E41A1C", fontface = "bold") + 
#       annotate("text", x = 6.5, y = -14000000, label = "♂", size = 30, color = "#377EB8")
#     p_all <- grid.arrange(g2, n1 , ncol = 2, widths = c(0.8, 0.2))
#     print(p_all)
#   }
# }, video.name ='aaa.mp4',interval=1,ani.width=1200,ani.height=750)


################
# plotdata_gender1 <- plotdata_gender[Year == 2016 & region %in% country_fanwei]
# # setorder(plotdata_gender1, gender)
# n1 <- ggplot(plotdata_gender1, aes(x = region, y = amount, fill = gender)) + 
#   geom_bar(subset = .(gender == "Female"), stat = "identity") + 
#   geom_bar(subset = .(gender == "Male"), stat = "identity") +
#   scale_fill_manual(values = c(females = "#E41A1C", males = "#377EB8"), name="") +
#   scale_y_continuous(breaks = seq(-20000000, 30000000, 5000000), 
#                      limits = c(-25000000, 35000000),
#                      labels = paste0(as.character(c(seq(20, 0, -5), seq(5, 30, 5))), "m")) + 
#   # scale_x_discrete(limits = c(country_fanwei, "")) +
#   theme(axis.title.x = element_blank(),
#         axis.title.y = element_blank(),
#         axis.line.x = element_blank(),
#         axis.line.y = element_blank(),
#         panel.grid.major = element_blank(), 
#         panel.grid.minor = element_blank(),
#         legend.position = "none") +
#   # coord_flip()# +
#   # guides(fill = guide_legend(direction = "horizontal"))
#   # geom_text(x=15, y=-1230, label="Scatter plot")
#   annotate("text", x = 6.5, y = 20000000, label = '♀', size = 30, color = "#E41A1C", fontface = "bold") + 
#   annotate("text", x = 6.5, y = -14000000, label = "♂", size = 30, color = "#377EB8")
# n1
# # g3 <- ggplot(plotdata, aes(x = reorder(region, amount), y = amount)) + #fill=region自动颜色
# # geom_bar(stat='identity') +   #以数字直接作为Y轴
# # theme(axis.title.x = element_blank(),
# #       axis.title.y = element_blank()) +
# # coord_flip() +
# #   transition_time(Year) +
# #   ease_aes('linear')
# 
# colorpanel <- function(){return(c("No data"   = "#F5F5F5",
#                                 "2% ~ 2.5%" = "#D5E0F5",
#                                 "2.5% ~ 3%" = "#C0D0F0",
#                                 "3% ~ 3.5%" = "#ACC0EB",
#                                 "3.5% ~ 4%" = "#97B0E6",
#                                 "4% ~ 4.5%" = "#82A1E1",
#                                 "4.5% ~ 5%" = "#6482C8",
#                                 "5% ~ 5.5%" = "#4665AF",
#                                 "5.5% ~ 6%" = "#254A97",
#                                 "6% ~ 6.5%" = "#1E3E7E",
#                                 "> 6.5%"    = "#183265"))}
# datepoint <- na.omit(unique(world_map$Year))
# #save several plots
# windowsFonts(A = windowsFont('等线'), B = windowsFont('微软雅黑'))
# for (i in 1 : length(datepoint)) { 
#   nian <- datepoint[i]
#   plotname <- paste0("plot", nian, ".png")
#   plotdata1 <- world_map[Year == nian]
#   g2 <- ggplot(plotdata1) +
#     geom_polygon(aes(x = long, y = lat, group = group, fill = label), colour = "grey", size = .2) +
#     guides(fill = guide_legend(reverse=TRUE)) +
#     scale_fill_manual(values = colorpanel, na.value = "white",
#                       limits = names(colorpanel),
#                       # aesthetics = c("colour", "fill"), 
#                       breaks = names(colorpanel)) +
#     theme(panel.background = element_rect(fill = "transparent", colour = NA),
#           plot.background = element_rect(fill = "transparent", colour = NA),
#           legend.position = c(0.12, 0.356),
#           legend.key = element_blank(),
#           legend.text = element_text(family = "B"),
#           legend.title = element_text(family = "B")) +
#     labs(fill = "") +
#     ggtitle(nian)
#   ggsave(plotname, g2, width = 1500/100, height = 776/100)
#   print(nian)
# }

#############################
#############最后一页灰度图
library(ggmap)
library(data.table)
library(ggplot2)
library(ggthemes)
library(maps)
library(tibble)
library(lubridate)
library(gganimate)
library(animation)
library(dplyr)

plotdata <- fread("C:/工作/可视化/number-of-people-living-with-hiv.csv") 
colnames(plotdata)[c(1, 4)] <- c("region", "share")
plotdata[, "region"] <- gsub("United States", "USA", plotdata$region)
plotdata[, "region"] <- gsub("Cote d'Ivoire", "Ivory Coast", plotdata$region)
plotdata[, "region"] <- gsub("United Kingdom", "UK", plotdata$region)
plotdata[region == "Taiwan", "share"] <- plotdata[region == "China", share]
plotdata[, "fan"] <- cut(plotdata$share, breaks = c(-100, 0, 300000, 600000, 900000, 1200000,
                                                     1500000, 1800000, max(plotdata$share) + 10))
labelsdata <- data.table(labels_n = sort(as.numeric(unique(plotdata$fan))), 
                         label =c("无数据", "0~30万", "30万~60万", "60万~90万",
                                  "90万~120万", "120万~150万", "150万~180万", ">180万"))
plotdata[, "labels_n"] <- as.numeric(plotdata$fan)
plotdata <- merge(plotdata, labelsdata, all.x = TRUE)

world_map <- alter_worldmap_zangnan()
world_map[, "region"] <- gsub("Democratic Republic of the Congo|Republic of Congo", "Congo", world_map$region)
# world_map[world_map$group == 1425 & world_map$group == 1425, "region"] <- "Trinidad and Tobago"
setDT(world_map)
setkey(world_map, region)
setkey(plotdata, region)
world_map <- merge(world_map, plotdata[, .(region, Year, label)], all.x = TRUE, allow.cartesian = T)
world_map[is.na(label), "label"] <- "无数据"
colorpanel = c("无数据"="white",
               "0~30万"="white",
               "30万~60万"="#eeeeee",
               "60万~90万"="#e5e5e5",
               "90万~120万"="#dcdcdc",
               "120万~150万"="#d2d2d2",
               "150万~180万"="#c9c9c9",
               ">180万"="#bfbfbf")

datepoint <- na.omit(unique(world_map$Year))
setwd("C:/工作/可视化")
windowsFonts(A = windowsFont('等线'), B = windowsFont('微软雅黑'))
nian <- datepoint[27]
plotname <- paste0("plottail", nian, ".png")
plotdata1 <- world_map[Year == nian]
g2 <- ggplot(plotdata1) +
  geom_polygon(aes(x = long, y = lat, group = group, fill = label), colour = "grey", size = .2) +
  guides(fill = guide_legend(reverse=TRUE)) +
  scale_fill_manual(values = colorpanel, na.value = "white",
                    limits = c("无数据", "0~30万", "30万~60万", "60万~90万", 
                               "90万~120万", "120万~150万", "150万~180万", ">180万"),
                    # aesthetics = c("colour", "fill"), 
                    breaks = c("无数据", "0~30万", "30万~60万", "60万~90万", 
                               "90万~120万", "120万~150万", "150万~180万", ">180万")) +
  theme(panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA),
        legend.position = c(0.12, 0.356),
        legend.key = element_blank(),
        legend.text = element_text(family = "B"),
        legend.title = element_text(family = "B")) +
  labs(fill = "") +
  ggtitle(nian)
ggsave(plotname, g2, width = 1500/100, height = 776/100)

