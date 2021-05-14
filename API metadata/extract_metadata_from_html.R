# extract metadata from ageis website function

library(tidyverse)

read_lines("API metadata/metadata_ar4.html")[[165]] %>%
  str_split(pattern = "title") %>%
  .[[1]] %>%
  str_subset(pattern = "key") %>%
  str_match('.*\\"(.*)\\", key\\D*(\\d+)') %>%
  as.data.frame() %>%
  select(-V1, index = V3, gas = V2) %>%
  write_csv("API metadata/gases_ar4.csv")



