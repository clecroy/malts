library(tidyverse)
library(lubridate)
library(rvest)
library(xml2)

# read html of malt results
kl <- read_html("https://www.klwines.com/Products?&filters=sv2_206!27&limit=500&offset=0&orderBy=60%20asc,search.score()%20desc&searchText=")

# store full product names
prod_name <- kl %>% html_nodes('.tf-product-header') %>% html_text()

# store TRUNCATED product descriptions from extended search results page
prod_desc <- kl %>% html_nodes('.tf-product-description') %>% html_text()

# get results block from body node, store all children nodes
res_blk <- kl %>% 
  html_nodes('body') %>% 
  xml_find_all("//div[contains(@class, 'results-block clearfix')]")%>% 
  html_children()

# extract product ids as text from results block children nodes, remove names in vector
ids <- res_blk[seq(1,length(res_blk),3)] %>% xml_attrs() %>% unlist()
names(ids) <- NULL

xml_try <- kl %>% 
  html_nodes('body') %>% 
  xml_find_all("//div[contains(@class, 'content')]")
xml_try[[2]]

xml_try2 <- kl %>% 
  html_nodes('body-con') %>% html_children()



%>% html_text()
head(xml_try2,50)
