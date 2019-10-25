##### Naver Blog Crawling using Rvest

# import rvest library for html reading
library(rvest)

# set list variables in order to create dataframe from crawled data
title <- c()
passage <- c()
date <- c()

#perform for loop in order to go through all the pages 
for (i in 1:1001){
  if (i %% 10 == 1){
    url <- paste0("https://search.naver.com/search.naver?&where=news&query=%EB%8C%80%EC%A0%84%20%EA%B4%80%EA%B4%91&sm=tab_pge&sort=0&photo=0&field=0&reporter_article=&pd=0&ds=&de=&docid=&nso=so:r,p:all,a:all&mynews=0&cluster_rank=46&start=", i) 
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

url <- "https://search.naver.com/search.naver?&where=news&query=%EB%8C%80%EC%A0%84%20%EA%B4%80%EA%B4%91&sm=tab_pge&sort=0&photo=0&field=0&reporter_article=&pd=0&ds=&de=&docid=&nso=so:r,p:all,a:all&mynews=0&cluster_rank=46&start=1"
html1 <- read_html(url)
html1 %>%
  html_node('div.news') %>%
  html_nodes('._sp_each_url') %>%
  html_text('p')


html1 <- read_html(url)
html1 %>%
  html_node('div.news') %>%
  html_nodes('.txt_inline') %>%
  html_node('._sp_each_source') %>%
  html_text('p')

#time
html1 %>%
  html_node('div.news') %>%
  html_nodes('.txt_inline') %>%
  html_text()




html1 %>% 
  html_node('div.news') %>%
  html_nodes('.')
  
 
