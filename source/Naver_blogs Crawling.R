##### Naver Blog Crawling using Rvest
setwd("C:/Users/709-000/Desktop/FINAL_PROJECT/")
# import rvest library for html reading
library(rvest)

# set list variables in order to create dataframe from crawled data
title <- c()
passage <- c()
date <- c()

#perform for loop in order to go through all the pages 
for (i in 1:1001){
  if (i %% 10 == 1){
    url <- paste0("https://search.naver.com/search.naver?date_from=20101001&date_option=8&date_to=20191011&dup_remove=1&nso=p%3Afrom20101001to20191011&post_blogurl=&post_blogurl_without=&query=대전%20관광&sm=tab_pge&srchby=all&st=sim&where=post&start=", i) 
    html <- read_html(url) 
    html %>% 
      html_node('div.blog') %>%
      html_nodes("li") -> lis
  
    for (li in lis) {
      title <- c(title, html_node(li, '.sh_blog_title') %>% html_text('p'))
      passage <- c(passage, html_nodes(li, '.sh_blog_passage') %>% html_text('p'))
      date <- c(date, html_nodes(li, '.txt_inline') %>% html_text('p'))
        
    }
    
  }
  
  
 i <- i + 10
}

View(title)

naverblogs <- data.frame(title = title, passage = passage, date = date)
View(naverblogs)


##### 네이버 크롤링 통합 with different time frame 
title <- c()
passage <- c()
date <- c()
#'https://search.naver.com/search.naver?where=post&query=대전%20관광&st=sim&sm=tab_opt&date_from=20190915&date_to=20191015&date_option=8&srchby=all&dup_remove=1&post_blogurl=&post_blogurl_without=&nso=so%3Ar%2Ca%3Aall%2Cp%3Afrom20190915to20191015'

url1 <- 'https://search.naver.com/search.naver?date_from=' #+timestart
url1 <- 'https://search.naver.com/search.naver?date_from=' #+timestart

url2 <- '&date_option=8&date_to=' #+timeend

url3 <- '&dup_remove=1&nso=p%3A1m&post_blogurl=&post_blogurl_without=&query=%EB%8C%80%EC%A0%84%20%EA%B4%80%EA%B4%91&sm=tab_pge&srchby=all&st=sim&where=post&start=' #+ i
url3 <- '&dup_remove=1&nso=p%3Afrom20170808to20170908&post_blogurl=&post_blogurl_without=&query=대전%20관광&sm=tab_pge&srchby=all&st=sim&where=post&start='
timeend <- c(20191013,20181013,20171013,20161013)
timestart <- c(20181014,20171014,20161014,20151014)
t1 <- c()
t2 <- c()
for (i in 5:8){
  for (j in 1:8){
    t1 <- c(t1,paste0("201",i,"0",j,'15'))
    t2 <- c(t2,paste0("201",i,"0",j+1,'14'))
        
  }
  for (k in 10:11){
    t1 <- c(t1, paste0("201",i,k,"15"))
    t2 <- c(t2, paste0("201",i,k+1,"14"))
  }
  
    
}

for (l in 1:8){
  t1 <- c(t1, paste0('20190',l,"15"))
  t2 <- c(t2, paste0('20190',l+1,"14"))
}



url_all

t1
t2
url_all
timestart[1]
i
for (i in 1:48){
  for (j in 1:991){
    if (j %% 10 == 1){
      url_all <- paste0(url1,t1[i],url2,t2[i],url3,j)
      html <- read_html(url_all)
    
      html %>% 
        html_node('div.blog') %>%
        html_nodes("li") -> lis
    
      for (li in lis) {
        title <- c(title, html_node(li, '.sh_blog_title') %>% html_text('p'))
        passage <- c(passage, html_nodes(li, '.sh_blog_passage') %>% html_text('p'))
        date <- c(date, html_nodes(li, '.txt_inline') %>% html_text('p'))
      
      }
    }
    j <- j + 10
  }
  print(i)
}

View(title)
naverblogs2 <- data.frame(title = title, passage = passage, date = date)
View(naverblogs2)

sum(title != '')
head(sort(table(title),descending = T))
url_all
# export data to csv
write.csv(naverblogs2,"naver_blogs.csv")


##### Word cloud using Naver Blogs##### 
library(wordcloud2)
library(KoNLP)
library(dplyr)
library(stringr)

trim <- function (x) gsub("^\\s+|\\s+$", "", x)
trim2 <- function(x) gsub("\\n","",x)
trim3 <- function(x) gsub("\\t","",x)
trim4 <- function(x) gsub("\\r","",x)

write(title, "title.txt")
data <- readLines('title.txt')
data1 <- sapply(data, extractNoun, USE.NAMES = F)
data3 <- unlist(data1)
data3 <- gsub("\\d+","",data3) ## 숫자 없애기 
data3 <- Filter(function(x) {nchar(x) >= 2}, data3) #2글자 이상 필터
data4 <- str_replace_all(data3, "[^[:alpha:]]","") #한글, 영어이외는 삭제

write(data4,"titlelist.txt")
data6 <- read.table("titlelist.txt")
nrow(data6)
wordcount <- table(data6)
wordcloud <-sort(wordcount,decreasing = T)
head(wordcloud,30)




##### 네이버 블로그 사이트에서 크롤링 ##### 
for (i in 1:1001){
  if (i %% 10 == 1){
    url <- paste0("https://section.blog.naver.com/Search/Post.nhn?pageNo=",i,"&rangeType=ALL&orderBy=sim&keyword=%EB%8C%80%EC%A0%84%20%EA%B4%80%EA%B4%91") 
    html <- read_html(url)
    html %>% 
      html_node('area_list_search') %>%
      html_nodes("li") -> lis
    
    for (li in lis) {
      title <- c(title, html_node(li, '.title') %>% html_text('p'))
      passage <- c(passage, html_nodes(li, '.sh_blog_passage') %>% html_text('p'))
      date <- c(date, html_nodes(li, '.txt_inline') %>% html_text('p'))
      
    }
    
  }
  
  
  i <- i + 10
}



url <-'https://search.naver.com/search.naver?date_from=20101001&date_option=8&date_to=20191011&dup_remove=1&nso=p%3Afrom20101001to20191011&post_blogurl=&post_blogurl_without=&query=대전%20관광&sm=tab_pge&srchby=all&st=sim&where=post&start=1'
html <- read_html(url)

##### 네이버 블로그 내용 크롤링 #####
html %>% 
  html_node('div.blog') %>%
  html_nodes('li') %>% 
  html_nodes('.sh_blog_passage') %>%
  html_text('p')

 # failed 
##### 네이버 블로그 제목 크롤링 ##### # test
html %>% 
  html_node('div.blog') %>%
  html_nodes('li') %>% 
  html_nodes('.sh_blog_title') %>%
  html_text('p')  


##### 네이버 블로그 날짜 크롤링 ##### 
html %>% 
  html_node('div.blog') %>%
  html_nodes('li') %>% 
  html_nodes('.txt_inline') %>%
  html_text('p')  

url  <- "http://section.blog.naver.com/Search/Post.nhn?pageNo=1&rangeType=ALL&orderBy=sim&keyword=%EB%8C%80%EC%A0%84%20%EA%B4%80%EA%B4%91"
html <- read_html(url)

html_node(html,'title')

html %>% 
  html_node('area_list_search')
  
View(html)


View(title)
