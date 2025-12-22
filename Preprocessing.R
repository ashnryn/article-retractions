### Cleaning/reformatting ICPSR, Retraction Watch data

library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(lubridate)
library(parallel)

# setwd(...) set working directory to project folder

# for mclapply
options(mc.cores = parallel::detectCores())


## ICPSR DATA

# load icpsr data from github
icpsr = data.frame(read.csv(
  "https://raw.githubusercontent.com/ashnryn/article-retractions/refs/heads/main/icpsr/icpsr_bib.csv", 
  na.strings = ""
  ))

# keep only Ref ID, Title, Authors, Study Titles, URL, Date Published, 
# ISSN, DOI, Year Published, RIS.Type columns
icpsr <- icpsr %>% 
  select(
    "Reference.ID", 
    "Title", 
    "Author.s.", 
    "Study.Title.s.", 
    "URL", 
    "Date.Published", 
    "ISSN", 
    "DOI", 
    "Year.Published", 
    "RIS.Type"
  )

# replace any '(&quot;)' symbols with " quotation marks
icpsr <- icpsr %>% 
  mutate(across(c("Title", "Study.Title.s.") , ~ gsub('(&quot;)', '\"', .)))

# replace 'Anonymous', (author unknown)' with NA
# created new variable "author_fix" to hold modified author strings
icpsr <- icpsr %>% 
  mutate(author_fix = if_else(str_detect(str_to_lower(Author.s.), "anonymous|unknown")==T,NA, Author.s.))

# Maybes: ENDOWMENT, CORPORATION, SURVEY, MISSION, SECRETARY, LABORATORY,
# DIRECTORATE, 
org_author_flags <- c(
  "AGENCY",
  "ADMINISTRATION",
  "BUREAU",
  "CENTER",
  "COMMISSION",
  "DEPARTMENT",
  "FOUNDATION",
  "INSTITUTE",
  "OFFICE",
  "SERVICE"
)

# function to break string into vector of author surnames
# set up for [LASTNAME, FIRSTNAME, INITIAL] format
# checks for bureau flag strings
get_author_names <- function(str) {
  # if na, return str
  if (is.na(str)) { return(NA) }
  # if the author is an organization, return str
  else if (sjmisc::str_contains(str, org_author_flags, ignore.case = TRUE, logic = "OR")) {
    return(str)
  }
  # else, extract author surnames only, return as sorted vector
  else {
    vec_out <- str %>%
      str_extract_all("(^[^,]+)|(;{1} [^,]+)", simplify = TRUE) %>%
      lapply(str_extract_all, "[A-Za-z]+", simplify = TRUE) %>%
      unlist() %>%
      sort() %>%
      paste(., collapse = ';') %>%
      toString()
    return(vec_out)
  }
}

# reformat author name/columns - pull out surname,
# add column for list of author surnames
icpsr$"Author.s.list" <- icpsr$author_fix %>% 
  mclapply(get_author_names, mc.cores = detectCores()) %>%
  as.character()

# drop author_fix columns
icpsr <- subset(icpsr, select = -c(author_fix))

# write modified icpsr dataset to CSV
write_csv(icpsr, "icpsr/icpsr_final.csv")


## RETRACTION WATCH DATA

# load retraction watch data
retractions = data.frame(read.csv(
  "https://raw.githubusercontent.com/ashnryn/article-retractions/refs/heads/main/retraction-watch/retraction_watch.csv",
  na.strings = c("", "unavailable")
  ))

# keep Record.ID, Title, Journal, Publisher, Author, URLs, ArticleType,
# RetractionDate, RetractionDOI, OriginalPaperDate, OriginalPaperDOI,
# RetractionNature, Reason, Notes
retractions <- retractions %>%
  select(
    "Record.ID",
    "Title",
    "Journal",
    "Publisher",
    "Author",
    "URLS",
    "ArticleType",
    "RetractionDate",
    "RetractionDOI",
    "OriginalPaperDate",
    "OriginalPaperDOI",
    "RetractionNature",
    "Reason",
    "Notes"
  )

# replace '(&quot;)' with \" quotation marks
retractions <- retractions %>% 
  mutate(across(c("Title") , ~ gsub('(&quot;)', '\"', .)))

# function to break string into vector of author surnames
# checks for bureau flag strings
# modified for [FIRSTNAME, INITIAL, LASTNAME] format 
get_author_names_2 <- function(str) {
  # if na, return str
  if (is.na(str)) { return(str) }
  # if the author is an organization, return str
  else if (sjmisc::str_contains(str, org_author_flags, ignore.case = TRUE, logic = "OR")) {
    return(str)
  }
  # else, extract author surnames only, return as sorted semicolon-
  # separated list
  else {
    vec_out <- str %>%
      str_extract_all("([^ ;]+)(?=[;\n])", simplify = TRUE) %>%
      lapply(str_extract_all, "[A-Za-z]+", simplify = TRUE) %>%
      unlist() %>%
      sort() %>%
      paste(., collapse = ';') %>%
      toString()
    return(vec_out)
  }
}

# reformat author name/columns - pull out surname,
# add column for list of author surnames
retractions$Author.List <- retractions$Author %>% 
  mclapply(get_author_names_2, mc.cores = detectCores()) %>%
  as.character()

# convert RetractionDate, OriginalPaperDate to lubridate Date type
ret_date_numer = mclapply(retractions$RetractionDate, mdy_hm)
pub_date_numer = mclapply(retractions$OriginalPaperDate, mdy_hm)

# get retraction year, publication year; add to retractions
retractions$"RetractionYear" <- mclapply(ret_date_numer, year) %>% as.integer()
retractions$"OriginalPaperYear" <- mclapply(pub_date_numer, year) %>% as.integer()
rm(ret_date_numer, pub_date_numer)

# write to CSV
write_csv(retractions, "retraction-watch/rw_final.csv")

