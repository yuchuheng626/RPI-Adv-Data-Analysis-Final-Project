rm(list = ls())
#install.packages("fastDummies")

#load packages
library(tidyverse)
library(quantreg)
library(wooldridge)
library(plm)
library(lmtest)
library(dplyr)
library(fastDummies)


#import data
youtube_usa <- read_csv("/Users/chuhengyu/Desktop/Project/archive/USvideos.csv")
youtube_britain <- read_csv("/Users/chuhengyu/Desktop/Project/archive/GBvideos.csv")
youtube_germany <- read_csv("/Users/chuhengyu/Desktop/Project/archive/DEvideos.csv")
youtube_canada <- read_csv("/Users/chuhengyu/Desktop/Project/archive/CAvideos.csv")
youtube_france <- read_csv("/Users/chuhengyu/Desktop/Project/archive/FRvideos.csv")
youtube_russia <- read_csv("/Users/chuhengyu/Desktop/Project/archive/RUvideos.csv")
youtube_mexico <- read_csv("/Users/chuhengyu/Desktop/Project/archive/MXvideos.csv")
youtube_south_korea <- read_csv("/Users/chuhengyu/Desktop/Project/archive/KRvideos.csv")
youtube_japan <- read_csv("/Users/chuhengyu/Desktop/Project/archive/JPvideos.csv")
youtube_india <- read_csv("/Users/chuhengyu/Desktop/Project/archive/INvideos.csv")

#add number of words in the title
youtube_usa$title_count <- sapply(strsplit(youtube_usa$title, " "), length)
youtube_britain$title_count <- sapply(strsplit(youtube_britain$title, " "), length)
youtube_germany$title_count <- sapply(strsplit(youtube_germany$title, " "), length)
youtube_canada$title_count <- sapply(strsplit(youtube_canada$title, " "), length)
youtube_france$title_count <- sapply(strsplit(youtube_france$title, " "), length)
youtube_russia$title_count <- sapply(strsplit(youtube_russia$title, " "), length)
youtube_mexico$title_count <- sapply(strsplit(youtube_mexico$title, " "), length)
youtube_south_korea$title_count <- sapply(strsplit(youtube_south_korea$title, " "), length)
youtube_japan$title_count <- sapply(strsplit(youtube_japan$title, " "), length)
youtube_india$title_count <- sapply(strsplit(youtube_india$title, " "), length)

#create a country column
youtube_usa$country <- c("USA")
youtube_britain$country <- c("Great Britain")
youtube_germany$country <- c("Germany")
youtube_canada$country <- c("Canada")
youtube_france$country <- c("France")
youtube_russia$country <- c("Russia")
youtube_mexico$country <- c("Mexico")
youtube_south_korea$country <- c("South Korea")
youtube_japan$country <- c("Japan")
youtube_india$country <- c("India")

#merge every column into one
youtube_combined <- bind_rows(youtube_usa, youtube_britain , youtube_germany, youtube_canada, youtube_france, youtube_russia, youtube_britain, youtube_mexico, youtube_south_korea, youtube_japan, youtube_india)
youtube_combined[is.na(youtube_combined) | youtube_combined == "Inf"] <- NA
#drop un-useful column
youtube_combined <- subset(youtube_combined, select = -c(thumbnail_link, video_error_or_removed, ratings_disabled))

#add binary value for on_chart(1 on trending chart, 0 if not)
youtube_combined$on_chart <- 1

#tag count for each video
youtube_combined$tag_count <- sapply(strsplit(youtube_combined$tags, "|", fixed=TRUE), length)

#graph relationship between likes and views for all countries
ggplot(youtube_combined, aes(x=views, y=likes, group=country))+
  geom_point(aes(color=country))


quant_youtube <- subset(youtube_combined, select = -c(title, channel_title, publish_time, 
                                                      tags, description, comments_disabled, video_id, country))

by_category_time <- aggregate(. ~trending_date+category_id, quant_youtube, sum)

#bar graph for title_count
ggplot(youtube_combined, aes(x=title_count))+ 
  geom_bar(fill="dark blue", alpha=0.5)

#likes vs category
ggplot(by_category_time, aes(x=reorder(factor(category_id), -views), y=views))+ 
  geom_bar(stat="identity", fill="dark blue", alpha=0.5)+
  labs(x = "Category ID", y = "Views")

#Views% by country

quant_youtube <- subset(youtube_combined, select = -c(title, channel_title, trending_date, publish_time, 
                                                      tags, description, comments_disabled, video_id))
by_country <- aggregate(. ~country, quant_youtube, sum)

by_country$likes_percent <- by_country$likes/by_country$views
by_country$comment_percent <- by_country$comment_count/by_country$views
by_country$dislikes_percent <- by_country$dislikes/by_country$views

#likes% by country
ggplot(by_country, aes(x=reorder(factor(country), -likes_percent), y=likes_percent))+ 
  geom_bar(stat="identity", fill="dark blue", alpha=0.5)+
  theme(axis.text.x = element_text(angle = 45,hjust = 1))+
  labs(x = "Country", y = "Likes Percentage")

#Comments% by country
ggplot(by_country, aes(x=reorder(factor(country), -comment_percent), y=comment_percent))+ 
  geom_bar(stat="identity", fill="dark blue", alpha=0.5)+
  theme(axis.text.x = element_text(angle = 45,hjust = 1))+
  labs(x = "Country", y = "Comments Percentage")

#Dislikes% by country
ggplot(by_country, aes(x=reorder(factor(country), -dislikes_percent), y=dislikes_percent))+ 
  geom_bar(stat="identity", fill="dark blue", alpha=0.5)+
  theme(axis.text.x = element_text(angle = 45,hjust = 1))+
  labs(x = "Country", y = "Dislikes Percentage")

#create dummy variables & comments views ratio
results <- fastDummies::dummy_cols(by_category_time, select_columns = "category_id")

#comment_count <- categoty_id+views+likes+dislikes+title_count+tag_count

row_sub = apply(results, 1, function(row) all(row !=0 ))
results[row_sub,]

results[is.na(results) | results=="Inf"] = NA

results <- filter(results, comment_count > 0, dislikes > 0, likes > 0)


fit_ols <- lm(log(comment_count)~log(views)+log(likes)+log(dislikes)+title_count^2+tag_count^2+
                category_id_2+category_id_10+category_id_15+category_id_17+category_id_19+
                category_id_20+category_id_22+category_id_23+category_id_24+category_id_25+category_id_26+
                category_id_27+category_id_28+category_id_29+category_id_30+category_id_43, data=results)
summary(fit_ols)
coeftest(fit_ols, vcovHC(fit_ols, cluster="group", type="HC0"))


fit_fe <- plm(log(comment_count)~log(views)+log(likes)+log(dislikes)+title_count^2+tag_count^2+log(views)*category_id_2+
                log(views)*category_id_10+log(views)*category_id_15+log(views)*category_id_17+log(views)*category_id_19+
                log(views)*category_id_20+log(views)*category_id_22+log(views)*category_id_23+log(views)*category_id_24+
                log(views)*category_id_25+log(views)*category_id_26+log(views)*category_id_27+log(views)*category_id_28+
                log(views)*category_id_29+
                log(views)*category_id_30+log(views)*category_id_43, data=results, index=c("category_id", "trending_date"), model="within")
summary(fit_fe)
coeftest(fit_fe, vcovHC(fit_fe, cluster="group", type="HC0"))


#title length vs views
ggplot(youtube_combined, aes(x=title_count, y=views))+ 
  geom_point()

#tag# vs views
ggplot(youtube_combined, aes(x=tag_count, y=views))+ 
  geom_point()

#views vs comments
ggplot(youtube_combined, aes(x=views, y=comment_count, group=country))+
  geom_point(aes(color=country))
                                                 
ggplot(youtube_combined["views"=max(views),"comments"=max(comment_count),by=title],aes(views,comment_count,colour=comment_count,size=comment_count))+
  geom_jitter()+guides(fill="none")+
  theme(legend.position = "none")

#comment ratio by category
quant_youtube1 <- subset(quant_youtube, select = -c(country))
by_category <- aggregate(. ~category_id, quant_youtube1, sum)
by_category$comment_ratio <- by_category$comment_count/by_category$views

ggplot(by_category, aes(x=reorder(factor(category_id), -comment_ratio), y=comment_ratio))+ 
  geom_bar(stat="identity", fill="dark blue", alpha=0.5)+
  theme(axis.text.x = element_text(angle = 45,hjust = 1))+
  labs(x = "Category", y = "Comment Views Ratio")

#title_length vs comment_count
ggplot(youtube_combined, aes(x=title_count, y=comment_cout))+ 
  geom_point()

#tag# vs comment_count
ggplot(youtube_combined, aes(x=tag_count, y=comment_count))+ 
  geom_point()
