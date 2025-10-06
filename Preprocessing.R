library(dplyr)
library(tidyr)
library(stringr)
library(purrr)

# setwd(...) set working directory to project folder

# load icpsr, retraction watch databases
icpsr = data.frame(read.csv("icpsr/icpsr_bib.csv", na.strings = ""))
retractions = data.frame(read.csv("retraction-watch/retraction_watch.csv", na.strings = c("", "unavailable")))

# replace &quot; with \" in title columns
icpsr <- icpsr %>% 
  mutate(across(c("Title", "Study.Title.s.", "Secondary.Title") , ~ gsub('(&quot;)', '\"', .)))

retractions <- retractions %>% 
  mutate(across(c("Title") , ~ gsub('(&quot;)', '\"', .)))

# convert ;-separated list strings to vectors - icpsr
icpsr <- icpsr %>% 
  mutate(across(ends_with(".s."), ~ str_trim(.))) %>%
  mutate(across(ends_with(".s."), ~ str_squish(.))) %>%
  mutate(across(ends_with(".s."), ~ str_replace_all(., pattern = ";[[:space:]]", replacement = ";"))) %>%
  mutate(across(ends_with(".s."), ~ strsplit(., ';')))

for (col in colnames(select(icpsr, is.list))) {
  icpsr$col %>% lapply(., unlist)
}

## convert string vectors to column type - get rid of strings in numeric columns, etc
icpsr$Study.Number.s. <- lapply(icpsr$Study.Number.s., as.integer)
icpsr$Series.Number.s. <- lapply(icpsr$Series.Number.s., as.integer)

# convert ;-separated list strings to vectors - retractions
retractions <- retractions %>% 
  mutate(across(everything(), ~ str_trim(.))) %>%
  mutate(across(everything(), ~ str_squish(.))) %>%
  mutate(across(everything(), ~ str_replace_all(., pattern = ";[[:space:]]", replacement = ";"))) %>%
  mutate(across(everything(), ~ strsplit(., ';'))) %>%
  mutate(across(everything(), ~ lapply(., unlist)))

retractions$RetractionNature = map_chr(retractions$RetractionNature, unlist)
retractions$Journal = map(retractions$Journal, unlist)
retractions$ArticleType = map(retractions$ArticleType, unlist)



