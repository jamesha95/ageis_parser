# Why doens't this work? 


library(tidyverse)


a <- tribble(~sector_level, ~sector, 
             1, "cool", 
             2, "bad",
             3, "yas",
             4, "maybe",
             5, "nah")

# What we want -----------------

a %>%
  mutate(name_0_digit = if_else(sector_level == 0, sector, NA_character_),
         name_1_digit = if_else(sector_level == 1, sector, NA_character_),
         name_2_digit = if_else(sector_level == 2, sector, NA_character_),
         name_3_digit = if_else(sector_level == 3, sector, NA_character_),
         name_4_digit = if_else(sector_level == 4, sector, NA_character_),
         name_5_digit = if_else(sector_level == 5, sector, NA_character_))

# Attempt A works ----------------- 

widen_names <- function(data, j){
  data %>%
    mutate("name_{{j}}_digit" := if_else(sector_level == j, sector, NA_character_))
}

a %>%
  widen_names(0) %>%
  widen_names(1) %>%
  widen_names(2) %>%
  widen_names(3) %>%
  widen_names(4) %>%
  widen_names(5)

# Attempt B fails! ----------------- 

for (i in as.numeric(0:6)) {
  a <- a %>%
    widen_names(j = !!i)
}

a 
