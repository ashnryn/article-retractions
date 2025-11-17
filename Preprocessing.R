### Cleaning/reformatting ICPSR, Retraction Watch data

library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(lubridate)
library(parallel)

# setwd(...) set working directory to project folder

# parallelize - start
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

# replace 'Anonymous', (author unknown)' with NA. TEMP - replace w/ below once debugged
icpsr <- icpsr %>% mutate(Author.s. = ifelse(Author.s. %in% c("Anonymous","(author unknown)"), NA, Author.s.))


# tried using regex for prev. TODO: debug
# icpsr %>%
# mutate(test = if_else(str_detect(str_to_lower(icpsr$Author.s.), "anonymous|author unknown"), NA, Authors.s.))

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
# checks for bureau flag strings
get_author_names <- function(str) {
  # if na, return str
  if (is.na(str)) { return(str) }
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
      sort()
    return(vec_out)
  }
}

# reformat author name/columns - pull out surname,
# add column for list of author surnames
icpsr$"Author.s.list" <- icpsr$Author.s. %>% 
  mclapply(get_author_names, mc.cores = detectCores())

## convert study numbers, series numbers, year of publication to numeric type
icpsr$Year.Published <- lapply(icpsr$Year.Published, as.numeric)


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
  # else, extract author surnames only, return as sorted vector
  else {
    vec_out <- str %>%
      str_extract_all("([^ ;]+)(?=[;\n])", simplify = TRUE) %>%
      lapply(str_extract_all, "[A-Za-z]+", simplify = TRUE) %>%
      unlist() %>%
      sort()
    return(vec_out)
  }
}

# reformat author name/columns - pull out surname,
# add column for list of author surnames
retractions$Author.List <- retractions$Author %>% 
  mclapply(get_author_names_2, mc.cores = detectCores())

# unlist list type columns
retractions$RetractionNature = map_chr(retractions$RetractionNature, unlist)
retractions$Journal = map(retractions$Journal, unlist)
retractions$ArticleType = map(retractions$ArticleType, unlist)

# convert RetractionDate, OriginalPaperDate to lubridate Date type
ret_date_numer = mclapply(retractions$RetractionDate, mdy_hm)
pub_date_numer = mclapply(retractions$OriginalPaperDate, mdy_hm)

# get retraction year, publication year; add to retractions
retractions$"RetractionYear" <- mclapply(ret_date_numer, year)
retractions$"OriginalPaperYear" <- mclapply(pub_date_numer, year)
rm(ret_date_numer, pub_date_numer)
