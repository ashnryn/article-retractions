### Merging

# Initial brief attempt at a merge

merged <- inner_join(
  x = (icpsr %>% 
         filter(!is.na(DOI)) %>% 
         select(Reference.ID, Title, DOI) %>% 
         unnest(c(DOI))),
  y = (retractions %>% 
         filter(!is.na(OriginalPaperDOI)) %>% 
         select(Record.ID, Title, OriginalPaperDOI, RetractionDate, RetractionNature, Reason) %>% 
         unnest(c(OriginalPaperDOI))),
  by = join_by(DOI == OriginalPaperDOI)
)

# returns 9 observations - one duplicate
# remove duplicate row from merged
merged = merged[c(1:4, 6:9),]

# write csv for use in citation_data, etc
write_csv(merged, "merged.csv", )

## fuzzy matching
# drop all ICPSR entries with DOIs, as we already checked for matches using DOI.

icpsr = icpsr |> filter(is.na(DOI))

merged_fuzzy <- inner_join(
  x = (icpsr %>%
         select(Reference.ID, Title, Author.s., Year.Published)),
  y = (retractions %>% 
         select(Record.ID, Title, Author, RetractionYear, OriginalPaperYear, RetractionNature, Reason) %>%
         unnest(Title)),
  by = join_by(Title == Title, Year.Published == OriginalPaperYear),
  multiple = "all"
)

