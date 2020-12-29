library(tidyverse)
library(lubridate)
library(rvest)
library(xml2)
library(microbenchmark)
library(parallel)
library(textreuse)
library(polite)
library(robotstxt)

# session <- bow(malts_url)
# session

## Get product names and ids

  # Read html of spirits where variety == "malt", results >= 500
malts_url <- "https://www.klwines.com/Products?&filters=sv2_206!27&limit=500&offset=0&orderBy=60%20asc,search.score()%20desc&searchText="
kl <- read_html(url)

  # Extract product names
prod_name <- kl %>% 
  html_nodes('.tf-product-header') %>% 
  html_text()

  # Extract results block from body node, store all children nodes
res_blk <- kl %>% 
  html_nodes('body') %>% 
  xml_find_all("//div[contains(@class, 'results-block clearfix')]") %>% 
  html_children()

  # Extract product ids as text from results block children nodes, remove names in vector
ids <- res_blk[seq(1,length(res_blk),3)] %>% 
  xml_attrs() %>% 
  unlist()
names(ids) <- NULL

## Get product descriptions and reviews
  # Generate list of urls for each malt

url_list <- paste0("https://www.klwines.com/p/i?i=",ids)

  # Function to store html of each product page; delay each fetch by 5 seconds
  
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


## Organize and store results as dataframe: id, name, desc, reviews

malts <- data.frame("id" = as.numeric(ids))
malts$name <- str_trim(as.character(prod_name))
malts$desc <- str_trim(unlist(lapply(malts_raw, function(x) x[[1]])))
malts$reviews <- str_trim(unlist(lapply(malts_raw, function(x) x[[2]])))

## Save output
write_csv(as.data.frame(malts), "C:/malts/malts.csv")

##################################################################
##################################################################
##################################################################

## Read output to restart analysis
malts <- read_csv("C:/malts/malts.csv")

## Remove useless text related to special order only, preorder items, 
## other notification text.
malts_test <- malts[1:25, ]
# regex to use for substring removal, in THIS ORDER:

# REMOVE substrings in $desc beginning with "|The item you..."
desc_trim <- str_trim(malts$desc)

replace_1 <- str_replace(desc_trim, 
                         "\\|The\\sitem\\syou\\s.*",
                         "")

# REMOVE all substrings beginning and ending with "*"
replace_2 <- str_replace_all(replace_1, 
                         "\\*[^*]+\\*",
                         "")

# REMOVE all substrings beginning with "(" and ending with ")"
replace_3 <- str_replace_all(replace_2, 
                             "\\(.*\\)",
                             "")

# REMOVE all substrings of "\r\n"
replace_4 <- str_replace_all(replace_3, 
                             "\\r\\n",
                             "")  


# REMOVE all substrings of "      |      "
final_desc <- str_trim(str_replace_all(replace_4, 
                                      "\\s*\\|\\s*",
                                      ""))

malts_clean <- malts
malts_clean[ ,3] <- final_desc

## Save output
write_csv(as.data.frame(malts_clean), "C:/malts/malts_clean.csv")


malts_c <- malts %>% unite("tot_desc", desc, reviews, sep=" ")
malts_c$name[1:3]
apply(malts_c$tot_desc, ant_join, )
