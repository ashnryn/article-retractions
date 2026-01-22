### Merging ICPSR and Retraction Watch datasets
### ICPSR and RW datasets used here are modified/processed
### in Preprocessing.R

library(tidyverse)
library(ggplot2)
library(lubridate)

# setwd(...)

# load data
icpsr <- read_csv("icpsr/icpsr_final.csv")
retractions <- read_csv("retraction-watch/rw_final.csv")

# Initial brief attempt at a merge
merged <- inner_join(
  x = (icpsr %>% 
         filter(!is.na(DOI)) %>% 
         select(Title, Author.s.list, DOI, Year.Published, Date.Published) %>% 
         unnest(c(DOI))),
  y = (retractions %>% 
         filter(!is.na(OriginalPaperDOI)) %>% 
         select(Author.List, RetractionYear, RetractionDate, OriginalPaperDOI, 
                RetractionNature, Reason) %>% 
         unnest(c(OriginalPaperDOI))),
  by = join_by(DOI == OriginalPaperDOI)
)

# returns 9 observations - one duplicate
# remove duplicate row from merged
merged = merged[-c(5),]

## Fuzzy Matching

# drop all ICPSR entries with DOIs, as we already checked for matches using DOI.
icpsr_fuzz <- icpsr |> filter(is.na(DOI))

# merge based on title, publication year
merged_fuzzy <- inner_join(
  x = (icpsr_fuzz %>%
         select(Title, Author.s.list, DOI, Year.Published, Date.Published)),
  y = (retractions %>% 
         select(Title, Author.List, RetractionYear, RetractionDate, OriginalPaperYear, 
                RetractionNature, Reason) %>%
         unnest(Title)),
  by = join_by(Title == Title, Year.Published == OriginalPaperYear),
  multiple = "all"
)

# append merged_fuzzy to merged
merged <- rbind(merged, merged_fuzzy)
rm(merged_fuzzy, icpsr_fuzz)

# setwd(...)

# write csv for use in citation_data, etc
write_csv(merged, "merged.csv")







