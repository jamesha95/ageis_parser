# Tidying the AGEIS data
tic("Running the tidy script")
file = "ar4"

if(file == "ar4") {cleaned_data <- read_fst("data/all_ar4_data.fst")} else {
  if(file == "ar4_co2e_australia") {cleaned_data <- read_csv("data/ar4_co2e_australia_raw.csv") } else {
    cleaned_data <- read_fst("data/all_data.fst")}
}




# Starting to make a correspondence table (very slow!) --------------------------------

widen_names <- function(data, j){
  data %>%
    mutate("name_{{j}}_digit" := if_else(sector_level == j, sector, NA_character_))
}

widened_data <- cleaned_data %>%
  # This step is fairly slow (several seconds)
  widen_names(0) %>%
  widen_names(1) %>%
  widen_names(2) %>%
  widen_names(3) %>%
  widen_names(4) %>%
  widen_names(5) %>%
  widen_names(6) %>%
  widen_names(7) %>%
  widen_names(8) %>%
  widen_names(9) %>%
  widen_names(10) %>%
  # This step is excruciatingly slow (an hour or more)
  group_by(year, location, gas) %>%
  fill(name_0_digit, .direction = "down")  %>%
  group_by(year, location, gas, name_0_digit) %>%
  fill(name_1_digit, .direction = "down")  %>%
  group_by(year, location, gas, name_0_digit, name_1_digit) %>%
  fill(name_2_digit, .direction = "down")  %>%
  group_by(year, location, gas, name_0_digit, name_1_digit, name_2_digit) %>%
  fill(name_3_digit, .direction = "down")  %>%
  group_by(year, location, gas, name_0_digit, name_1_digit, name_2_digit, name_3_digit) %>%
  fill(name_4_digit, .direction = "down")  %>%
  group_by(year, location, gas, name_0_digit, name_1_digit, name_2_digit, name_3_digit, name_4_digit) %>%
  fill(name_5_digit, .direction = "down")  %>%
  group_by(year, location, gas, name_0_digit, name_1_digit, name_2_digit, name_3_digit, name_4_digit, name_5_digit) %>%
  fill(name_6_digit, .direction = "down")  %>%
  group_by(year, location, gas, name_0_digit, name_1_digit, name_2_digit, name_3_digit, name_4_digit, name_5_digit, name_6_digit) %>%
  fill(name_7_digit, .direction = "down")  %>%
  group_by(year, location, gas, name_0_digit, name_1_digit, name_2_digit, name_3_digit, name_4_digit, name_5_digit, name_6_digit, name_7_digit) %>%
  fill(name_8_digit, .direction = "down")  %>%
  group_by(year, location, gas, name_0_digit, name_1_digit, name_2_digit, name_3_digit, name_4_digit, name_5_digit, name_6_digit, name_7_digit, name_8_digit) %>%
  fill(name_9_digit, .direction = "down") 


widened_data %>% head() 

# For rows that are non-divisible totals, we want to add
#  deeper level categories.
# Ultimately the goal is to have codes at level 10 that we can sum
#  to get the national total.
# !!sym(glue("names_{level}_digit"))
# 
# 
# add_deeper_names <- function(data, level){
#    
#   var <- paste0("names_", level, "_digit")
#   var_above <- paste0("names_", level + 1, "_digit")
#   
#  
# 
#   data %>%
#     mutate( {{ var }}  := case_when(
#       !is.na( enquo(var) ) ~  enquo(var),
#       lead(sector_level, default = 11) > sector_level ~ enquo(var_above),
#       TRUE ~ NA_character_))
#   
# }
# 
# 
# widened_data %>%
#   head(20) %>% 
#   add_deeper_names(level = 3) %>%
#   glimpse()

# Filling in the relevant blanks so that unique line items each get level-10 codes that partition the space ---------
# 3 mins
tic("Creating level-10 codes for all unique line items")
filled_data <- widened_data %>%
  # First of all we drop all NA items (data not available or data confidential)
  # These don't tell us anything, and if more-aggregated categories actually do have data then
  #  that's what we want as our line items 
  ungroup() %>%
  filter(!is.na(emissions_kt)) %>%
  # testing only
  group_by(year, location, gas) %>%
  #tail(20) %>%
  # The bottom row is always 7 Other, so it does need more specific names
  mutate(
    name_1_digit = case_when(
      !is.na(name_1_digit) ~ name_1_digit,
      lead(sector_level, default = 0) <= sector_level ~ name_0_digit,
      TRUE ~ NA_character_)) %>%
  mutate(
    name_2_digit = case_when(
      !is.na(name_2_digit) ~ name_2_digit,
      lead(sector_level, default = 0) <= sector_level ~ name_1_digit,
      TRUE ~ NA_character_)) %>% 
  mutate(
    name_3_digit = case_when(
      !is.na(name_3_digit) ~ name_3_digit,
      lead(sector_level, default = 0) <= sector_level ~ name_2_digit,
      TRUE ~ NA_character_)) %>%
  mutate(
    name_4_digit = case_when(
      !is.na(name_4_digit) ~ name_4_digit,
      lead(sector_level, default = 0) <= sector_level ~ name_3_digit,
      TRUE ~ NA_character_)) %>%
  mutate(
    name_5_digit = case_when(
      !is.na(name_5_digit) ~ name_5_digit,
      lead(sector_level, default = 0) <= sector_level ~ name_4_digit,
      TRUE ~ NA_character_)) %>%
  mutate(
    name_6_digit = case_when(
      !is.na(name_6_digit) ~ name_6_digit,
      lead(sector_level, default = 0) <= sector_level ~ name_5_digit,
      TRUE ~ NA_character_)) %>%
  mutate(
    name_7_digit = case_when(
      !is.na(name_7_digit) ~ name_7_digit,
      lead(sector_level, default = 0) <= sector_level ~ name_6_digit,
      TRUE ~ NA_character_)) %>%
  mutate(
    name_8_digit = case_when(
      !is.na(name_8_digit) ~ name_8_digit,
      lead(sector_level, default = 0) <= sector_level ~ name_7_digit,
      TRUE ~ NA_character_)) %>%
  mutate(
    name_9_digit = case_when(
      !is.na(name_9_digit) ~ name_9_digit,
      lead(sector_level, default = 0) <= sector_level ~ name_8_digit,
      TRUE ~ NA_character_)) %>%
  mutate(
    name_10_digit = case_when(
      !is.na(name_10_digit) ~ name_10_digit,
      lead(sector_level, default = 0) <= sector_level ~ name_9_digit,
      TRUE ~ NA_character_),
  )  

toc()


filled_data %>%
  write_fst(glue("data/filled_data_{file}.fst"))
toc()


# Tests: we're close but not quite there --------------


# filled_data %>%
#   head(20) %>% 
#   view()
# 
# filled_data %>%
#   ungroup() %>%
#   filter(year == 2019 & gas == "carbon_dioxide_equivalent_-_ar5" & location == "Australia") %>%
#   filter(!is.na(name_10_digit)) %>%
#   summarise(total = sum(emissions_kt, na.rm = T))
# 
# filled_data %>%
#   ungroup() %>%
#   filter(year == 2019 & gas == "carbon_dioxide_equivalent_-_ar5" & location == "Australia" & sector_level == 0) %>%
#   pull(emissions_kt)

