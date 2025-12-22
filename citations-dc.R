#### Data cleaning for citation reports from WOS, Scopus
#### WOS data pulled 14 October 2025
#### Scopus data downloaded 19 December 2025

library(tidyverse)
library(parallel)
library(lubridate)
library(data.table)

# for get_author_names
source("Preprocessing.R")

# setwd(...)

# load merged dataset
merged <- read_csv("merged.csv")

#### WOS citation reports ####

# load data
df_wos <- list.files(path = "citations/WOS/", pattern = "*.csv") %>%
  paste0("citations/WOS/", .) %>%
  map(., read_csv, skip = 10, id = 'filepath')
  
# keep only filepath (id's cited paper), title, 
# authors, source publication, publication date/year, number
# of citations; convert to data frame
df_wos <- lapply(df_wos, subset,
                 select = c("filepath", "Title", "Authors", 
                            "Source Title", "Publication Date", "DOI", 
                            "Publication Year", "Total Citations")) %>%
  do.call("rbind", .)

# simplify filepath
df_wos$filepath <- df_wos$filepath %>%
  lapply(str_extract, pattern = "([^\\/]+)\\.csv") %>%
  lapply(str_remove, pattern = "_WOS\\.csv")

# reformat author name/columns - pull out surname,
# add column for list of author surnames
df_wos$authors.list <- df_wos$Authors %>% 
  mclapply(toupper, mc.cores = detectCores()) %>%
  mclapply(get_author_names, mc.cores = detectCores()) %>%
  as.character()

# convert Publication Year to numeric
df_wos$pub.year <- df_wos$`Publication Year` %>%
  mclapply(as.integer, mc.cores = detectCores())

# converting Publication Date strings to datetime
# assumes earliest possible publication date
# TODO: COMPLETE
mod_pub_date <- function(d) {
  month_str <- c('JAN', 'FEB', 'MAR', 'APR', 'MAY', 'JUNE',
                 'JUL', 'AUG', 'SEP', 'OCT', 'NOV', 'DEC')
}



### SCOPUS CITATION REPORTS ####

# load data
df_scop <- list.files(path = "citations/Scopus/", pattern = "*.csv") %>%
  paste0("citations/Scopus/", .) %>%
  map(., read_csv, id = 'filepath')

# keep only filepath (id's cited paper), title, 
# authors, source publication, publication date/year, number
# of citations; convert to data frame
df_scop <- lapply(df_scop, subset,
                 select = c("filepath", "Title", "Authors", "Source title", 
                            "Year", "DOI", "Cited by")) %>%
  do.call("rbind", .)

# simplify filepath
df_scop$filepath <- df_scop$filepath %>%
  lapply(str_extract, pattern = "([^\\/]+)\\.csv") %>%
  lapply(str_remove, pattern = "_Scopus\\.csv")

# reformat author list - pull out surnames, convert to 
# upper case only
df_scop$authors.list <- df_scop$Authors %>% 
  mclapply(toupper, mc.cores = detectCores()) %>%
  mclapply(get_author_names, mc.cores = detectCores()) %>%
  as.character()

# convert Publication Year to numeric
df_scop$pub.year <- df_scop$Year %>%
  mclapply(as.integer, mc.cores = detectCores())



#### MERGING DATASETS ####

# merge by DOI
refs_merged <- full_join(
  x = df_wos %>% filter(!is.na(DOI)),
  y = df_scop %>% filter(!is.na(DOI)),
  by = join_by(filepath, DOI)
)

# consolidate Title.x, Title.y
refs_merged$title <- ifelse(!is.na(refs_merged$Title.x),
                            refs_merged$Title.x,
                            refs_merged$Title.y)

# consolidate Authors.x, Authors.y
refs_merged$authors.list <- ifelse(!is.na(refs_merged$authors.list.x),
                              refs_merged$authors.list.x,
                              refs_merged$authors.list.y)

# consolidate Source Title (WOS) and Source title (Scopus)
refs_merged$source <- ifelse(!is.na(refs_merged$`Source Title`),
                             refs_merged$`Source Title`,
                             refs_merged$`Source title`) %>%
  toupper()

# consolidate Publication Year (WOS) and Year (Scopus)
refs_merged$year <- ifelse(!is.na(refs_merged$`Publication Year`),
                           refs_merged$`Publication Year`,
                           refs_merged$Year)

# consolidate Total Citations (WOS) and Cited by (Scopus)
refs_merged$cited.by <- ifelse(!is.na(refs_merged$`Total Citations`),
                               refs_merged$`Total Citations`,
                               refs_merged$`Cited by`)

# rename DOI column to doi, Publication Date column to 
# pub.date
colnames(refs_merged)[c(5, 6)] <- c("pub.date", "doi")

# drop duplicate columns
refs_merged <- refs_merged %>% subset(
  select = c('filepath', 'title', 'authors.list', 'source', 'pub.date', 
             'year', 'doi', 'cited.by')
)








#### FUZZY MATCHING ####

# for entries without DOI
# fuzzy merge by title, authors, publication year
refs_merged_fuzzy <- full_join(
  x = df_wos %>% filter(is.na(DOI)),
  y = df_scop %>% filter(is.na(DOI)),
  by = join_by(filepath, Title, authors.list, pub.year)
)

# consolidate Source Title columns
refs_merged_fuzzy$source <- ifelse(!is.na(refs_merged_fuzzy$`Source Title`),
                                   refs_merged_fuzzy$`Source Title`,
                                   refs_merged_fuzzy$`Source title`)

# consolidate Publication Year, Year
refs_merged_fuzzy$year <- ifelse(!is.na(refs_merged_fuzzy$`Publication Year`),
                                   refs_merged_fuzzy$`Publication Year`,
                                   refs_merged_fuzzy$Year)

# consolidate DOI columns
refs_merged_fuzzy$doi <- ifelse(!is.na(refs_merged_fuzzy$DOI.x),
                                refs_merged_fuzzy$DOI.x,
                                refs_merged_fuzzy$DOI.y)

# consolidate Total Citations (WOS) and Cited by (Scopus)
refs_merged_fuzzy$cited.by <- ifelse(!is.na(refs_merged_fuzzy$`Total Citations`),
                                     refs_merged_fuzzy$`Total Citations`,
                                     refs_merged_fuzzy$`Cited by`)

# rename Title to title, Publication Date to pub.date
colnames(refs_merged_fuzzy)[c(2, 6)] <- c("title", "pub.date")

# drop duplicate columns
refs_merged_fuzzy <- refs_merged_fuzzy %>% subset(
  select = c('filepath', 'title', 'authors.list', 'source', 'pub.date', 'year', 'doi', 'cited.by')
)

# append fuzzy matches to DOI matches
refs_merged <- bind_rows(refs_merged, refs_merged_fuzzy)

# change type for write_csv
refs_merged$filepath <- refs_merged$filepath %>% as.character()

# write to CSV
write_csv(refs_merged, "citations/refs_merged.csv")

