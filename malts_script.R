library(tidyverse)
library(lubridate)
library(rvest)
library(xml2)
library(microbenchmark)
library(parallel)
library(textreuse)

### ADD COMMENTS

#### EXPORT PACKAGES AND FUNCTIONS TO CLUSTERS

##### TEST PARALELL RUN OF read_pages


  # Read html of 500 variety == "malt" results
kl <- read_html("https://www.klwines.com/Products?&filters=sv2_206!27&limit=500&offset=0&orderBy=60%20asc,search.score()%20desc&searchText=")

  # Extract product names
prod_name <- kl %>% 
  html_nodes('.tf-product-header') %>% 
  html_text()

  # Get variety == "malt" results block from body node, store all children nodes
res_blk <- kl %>% 
  html_nodes('body') %>% 
  xml_find_all("//div[contains(@class, 'results-block clearfix')]")%>% 
  html_children()

  # Extract product ids as text from results block children nodes, remove names in vector
ids <- res_blk[seq(1,length(res_blk),3)] %>% xml_attrs() %>% unlist()
names(ids) <- NULL

# generate list of urls or each malt
url_list <- paste0("https://www.klwines.com/p/i?i=",ids)

# function to store html of each product page; delay each fetch by 5 seconds

prod <- list_along(1:length(url_list))

read_pages <- function(url_element) {
  prod_html <- read_html(url_element)
  
  prod_desc <- prod_html %>% 
    html_nodes('p') %>% 
    html_text() %>% 
    paste(collapse="|")
  
  prod_revs <- prod_html %>% 
    html_nodes('.ReviewTextDetail') %>% 
    html_text() %>% 
    paste(collapse="|")
  
  Sys.sleep(5)
  
  return(c(prod_desc, prod_revs))
}


##### Call function
#### NOTE this takes ~30 min to run ####

malts_raw <- lapply(url_list, read_pages)

# organize and store results as dataframe: id, name, desc, reviews
malts <- data.frame("id" = as.numeric(ids))
malts$name <- as.character(prod_name)
malts$desc <- unlist(lapply(malts_raw, function(x) x[[1]]))
malts$reviews <- unlist(lapply(malts_raw, function(x) x[[2]]))

# save output
write_csv(as.data.frame(malts), "C:/Users/clecroy/malts.csv")

# detectCores()
# 
# cl <- makeCluster(6)
# 
# three_prod <- microbenchmark(parLapply(cl, url_list[1:6], read_pages))
# 
# stopCluster(cl)

malts <- read_csv("C:/Users/clecroy/whisky/malts.csv")

malts_c <- malts %>% unite("tot_desc", desc, reviews, sep=" ")
malts_c$name[1:3]
apply(malts_c$tot_desc, ant_join, )