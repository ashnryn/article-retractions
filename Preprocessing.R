library(tidyverse)
library(dplyr)
library(parallel)

# setwd(...) set working directory to project folder

# load icpsr, retraction watch databases
icpsr = read.csv("icpsr/icpsr_bib.csv")
retractions = read.csv("retraction-watch/retraction_watch.csv")

## Cleaning ICPSR data

# convert ;-separated list strings to vectors
icpsr <- icpsr |> 
  mutate(across(ends_with(".s."), ~ str_trim(.))) |>
  mutate(across(ends_with(".s."), ~ str_squish(.))) |>
  mutate(across(ends_with(".s."), ~ str_replace_all(., pattern = ";[[:space:]]", replacement = ";"))) |>
  mutate(across(ends_with(".s."), ~ strsplit(., ';')))


## Cleaning Retraction Watch data

# convert ;-separated list strings to vectors
retractions <- retractions |> 
  mutate(across(everything(), ~ str_trim(.))) |>
  mutate(across(everything(), ~ str_squish(.))) |>
  mutate(across(ends_with(".s."), ~ str_replace_all(., pattern = ";[[:space:]]", replacement = ";"))) |>
  mutate(across(everything(), ~ strsplit(., ';')))










