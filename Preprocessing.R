library(tidyverse)
library(dplyr)
library(parallel)

# setwd(...) set working directory to project folder

# load icpsr, retraction watch databases
icpsr = read.csv("icpsr/icpsr_bib.csv")
retractions = read.csv("retraction-watch/retraction_watch.csv")

# convert ;-separated list strings to vectors - icpsr
icpsr <- icpsr |> 
  mutate(across(ends_with(".s."), ~ str_trim(.))) |>
  mutate(across(ends_with(".s."), ~ str_squish(.))) |>
  mutate(across(ends_with(".s."), ~ str_replace_all(., pattern = ";[[:space:]]", replacement = ";"))) |>
  mutate(across(ends_with(".s."), ~ strsplit(., ';')))

# convert ;-separated list strings to vectors - retractions
retractions <- retractions |> 
  mutate(across(everything(), ~ str_trim(.))) |>
  mutate(across(everything(), ~ str_squish(.))) |>
  mutate(across(ends_with(".s."), ~ str_replace_all(., pattern = ";[[:space:]]", replacement = ";"))) |>
  mutate(across(everything(), ~ strsplit(., ';')))

## convert vector type to column type - get rid of strings in numeric columns, etc








