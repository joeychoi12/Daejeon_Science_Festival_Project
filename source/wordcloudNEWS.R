setwd("C:/Users/709-000/Desktop/대전사이언스 페스티벌의 효율적 추진 방안/source/")
##### Word cloud using Naver Blogs##### 
library(wordcloud2)
library(KoNLP)
library(dplyr)
library(stringr)
library(xlsx)

trim <- function (x) gsub("^\\s+|\\s+$", "", x)
trim2 <- function(x) gsub("\\n","",x)
trim3 <- function(x) gsub("\\t","",x)
trim4 <- function(x) gsub("\\r","",x)

news <- read.xlsx("C:/Users/709-000/Desktop/대전사이언스 페스티벌의 효율적 추진 방안/Data/뉴스 크롤링 4분기/2000-2004년 네이버 뉴스 기사 크롤링.xlsx", sheetIndex = 1, fileEncoding = 'EUC-KR')

news1 <- read.csv("C:/Users/709-000/Desktop/대전사이언스 페스티벌의 효율적 추진 방안/Data/뉴스 크롤링 4분기/2000-2004년 네이버 뉴스 기사 크롤링.csv")
news2 <- read.csv("C:/Users/709-000/Desktop/대전사이언스 페스티벌의 효율적 추진 방안/Data/뉴스 크롤링 4분기/2005-2009년 네이버 뉴스 기사 크롤링.csv")
news3 <- read.csv("C:/Users/709-000/Desktop/대전사이언스 페스티벌의 효율적 추진 방안/Data/뉴스 크롤링 4분기/2010-2014년 네이버 뉴스 기사 크롤링.csv")
news4 <- read.csv("C:/Users/709-000/Desktop/대전사이언스 페스티벌의 효율적 추진 방안/Data/뉴스 크롤링 4분기/2015-2018년 네이버 뉴스 기사 크롤링.csv")
head(news)
title <- as.character(news3$contents)

head(title)
class(title)
write(title, "content_News_ScienceFestival.txt")
data <- readLines('content_News_ScienceFestival.txt')
data1 <- sapply(data, extractNoun, USE.NAMES = F)
data3 <- unlist(data1)
data3 <- gsub("\\d+","",data3) ## 숫자 없애기 
data3 <- Filter(function(x) {nchar(x) >= 2}, data3) #2글자 이상 필터
data4 <- str_replace_all(data3, "[^[:alpha:]]","") #한글, 영어이외는 삭제
data4 <- gsub("대전","",data4)
data4 <- gsub("페스티벌","",data4)
data4 <- gsub("사이언스","",data4)
data4 <- gsub("과학","",data4)
data4 <- gsub("축제","",data4)
data4 <- gsub("공원","",data4)
data4 <- gsub("행사","",data4)
data4 <- gsub("개최","",data4)


write(data4,"ScienceFestival_Words.txt")
data6 <- read.table("ScienceFestival_Words.txt")
nrow(data6)
wordcount <- table(data6)
wordcloud <-sort(wordcount,decreasing = T)
head(wordcloud,600)



wordcloud2(wordcloud)


##### Save data to compare for different years 
#wordcloud2000 <- wordcloud
#wordcloud2005 <- wordcloud
#wordcloud2010 <- wordcloud
#wordcloud2015 <- wordcloud

wordcloud2000_2 <- wordcloud
wordcloud2005_2 <- wordcloud
wordcloud2010_2 <- wordcloud
wordcloud2015_2 <- wordcloud

wordcloud2000_3 <- wordcloud
wordcloud2005_3 <- wordcloud
wordcloud2010_3 <- wordcloud
wordcloud2015_3 <- wordcloud

head(wordcloud2000_2,15)
head(wordcloud2005_2,15)
head(wordcloud2010_2,15)
head(wordcloud2015_2,15)


write(passage, "passage.txt")
pdata <- readLines('passage.txt')
pdata1 <- sapply(pdata, extractNoun, USE.NAMES = F)
pdata3 <- unlist(pdata1)
pdata3 <- gsub("\\d+","",pdata3) ## 숫자 없애기 
pdata3 <- Filter(function(x) {nchar(x) >= 2}, pdata3) #2글자 이상 필터
pdata4 <- str_replace_all(pdata3, "[^[:alpha:]]","") #한글, 영어이외는 삭제

write(pdata4,"titlelist.txt")
data6 <- read.table("titlelist.txt")
nrow(data6)
wordcount <- table(data6)
wordcloud <-sort(wordcount,decreasing = T)
head(wordcloud,300)
