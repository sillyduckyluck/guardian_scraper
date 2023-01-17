# INSTRUCTIONS:
# https://ladal.edu.au/webcrawling.html

# load packages
library(tidyverse)
library(rvest)
library(readtext)
library(flextable)
library(webdriver)
library(dplyr)

#this should only need to be installed once. Uncomment if your machine requires
#the packages klippy and remote

#install.packages("remotes")
#remotes::install_github("rlesur/klippy")
# activate klippy for copy-to-clipboard button
klippy::klippy()

page_numbers <- 1:472

# uncomment for testing
# page_numbers <- 1:5

home_url <- "https://obamawhitehouse.archives.gov/"
base_url <- "https://obamawhitehouse.archives.gov/briefing-room/speeches-and-remarks?term_node_tid_depth=31&page="
paging_urls <- paste0(base_url, page_numbers)


all_links <- NULL
for (url in paging_urls) {
  # download and parse single overview page
  html_document <- read_html(url)
  # extract links to articles
  links <- html_document %>%
    html_nodes(xpath = "//h3[contains(@class, 'field-content')]/a") %>%
    html_attr(name = "href")
  
  
  
  # append links to vector of all links
  all_links <- c(all_links, links)
}

scrape_guardian_article <- function(url) {
  url <- paste0(home_url, url)
  html_document <- read_html(url)
  # extract title
  title <- html_document %>%
    rvest::html_node("h1") %>%
    rvest::html_text(trim = T)
  # extract text
  text <- html_document %>%
    rvest::html_node("p") %>%
    rvest::html_text(trim = T)
  # extract date
  date <- html_document %>%
    rvest::html_element("#press_article_date_created") %>%
    rvest::html_text()
  # date <- url %>%
    # stringr::str_replace_all(".*([0-9]{4,4}/[a-z]{3,4}/[0-9]{1,2}).*", "\\1")
  
  content <- html_document %>%
    rvest::html_element("#content-start") %>%
    rvest::html_text()
  
  # generate data frame from results
  article <- data.frame(
    url = url,
    date = date,
    title = title,
    body = text,
    content = content
  )
  
  return(article)
  
}


#scrape all the data into one large dataframe
df = data.frame()

for (url in all_links) {
  output <-scrape_guardian_article(url)
  df = rbind(df, output)
}

#remove NA entries in content and title, ie. videos and audio
#df <- df[!(is.na(df$content) | df$content==""), ]
#df <- df[!(is.na(df$content) | df$title ==""), ]

#remove some fluffs
#err1 = "Show key events onlyPlease turn on JavaScript to use this feature"
#df[] <- lapply(df, gsub, pattern = err1, replacement = "")


#filter based on keyterms
keywords <- "Egypt"
filtered_df <- df %>% filter(grepl(keywords, content))

# save corpus
save(df, file = "cache/corpus_wh_speech.rds")

