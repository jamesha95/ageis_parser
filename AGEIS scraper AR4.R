# National Greenhouse Gas Inventory parser
# Auth: H McMillan and J Ha
# License: MIT

# This script is based off the XMLHttpRequest that is used to update the 
# National Greenhouse Gas Inventory website. The POST request returns a html
# table which can be parsed. All parts of the POST request are verbatim copies 
# of a manually executed update except for the year, location, gas type and 
# industries which are modified for as per the function call.

# NOTE: you may need to source this file instead of the usual run command as it
#       contains some strings longer than the parser allows (lines 81 and 83).

# Set up --------------------------------------------------

# Packages
library(tidyverse)
library(httr)
library(rvest)
library(pbapply)
library(snow)
library(glue)   # For working with strings
library(tictoc) # For timing how long things take
library(fst) # For saving data
library(readxl) # For Grattan categories


# Error handling
`%iferror%` <- function(a, b) tryCatch({a}, error = function(e){b})



# Metadata --------------------------------------------------


# Load gas type and location metadata
gas_table_ar4 <- read_csv("API metadata/gases_ar4.csv")
location_table <- read_csv("API metadata/locations.csv")



# I went to https://ageis.climatechange.gov.au and checked every box
# Then had a look at the underlying HTML of the website to see what it looked like
# The relevant codes were then saved to API metadata/sectorUNFCCC.txt

sectors_unfccc <- scan("API metadata/sectorUNFCCC.txt", sep = ";")


# Define POST URL and headers --------------------------------------------------
url_ar4 <- 'https://ageis.climatechange.gov.au/UNFCCC.aspx'


ageis_ar4_headers <- c(
  'Connection'       = 'keep-alive',
  'Accept-Encoding'  = 'gzip, deflate, br',
  'Host'             = 'ageis.climatechange.gov.au',
  'User-Agent'       = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_6) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/14.0.3 Safari/605.1.15',
  'Content-Type'     = 'application/x-www-form-urlencoded; charset=UTF-8',
  'Cache-Control'    = 'no-cache',
  'X-Requested-With' = 'XMLHttpRequest', 
  'X-MicrosoftAjax'  = 'Delta=true',
  'Accept'           = '*/*',
  'Origin'           = 'https://ageis.climatechange.gov.au',
  #'Content-Length'   = '5072',
  'Referer'          = 'https://ageis.climatechange.gov.au/UNFCCC.aspx',
  'Accept-Language'  = 'en-us',
  'Cookie'           = 'ASP.NET_SessionId=j5rznokai5s3e1mafjuha2fp'
)




# Key functions --------------------------------------------------

# Define function for post form options
ageis_ar4_form_options <- function(year, location, gas){
  
  location_number <- location_table$index[match(location, location_table$location)]
  gas_number <- gas_table_ar4$index[match(gas, gas_table_ar4$gas)]

  list(
    'MIME Type' = 'application/x-www-form-urlencoded; charset=utf-8',
    'ctl00$ScriptManager1' = 'ctl00$cph1$UpdatePanel1|ctl00$cph1$ButtonViewEmission',
    'ctl00$cph1$WebUserControlInventoryYearSelection1$InventoryYearDropDownID' = as.character(year),
    'ctl00$cph1$WebUserControlLocation1$LocationDropDownID' = location_number,
    'ctl00$cph1$selectionFuelUNFCCC' = '1',
    'ctl00$cph1$selectionSectorUNFCCC' = paste(sectors_unfccc, collapse = ";"),
    'ctl00$cph1$selectionGasUNFCCC' = gas_number,
    'ctl00$cph1$selectionEmissionUNFCCC' = '13',
    '__EVENTTARGET' = '',
    '__EVENTARGUMENT' = '',
    '__LASTFOCUS' = '',
    '__VIEWSTATE' = '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',
    '__VIEWSTATEGENERATOR' = '0D3740F0',
    '__EVENTVALIDATION' = 'ypfCww0mQGrYV7z5bPWl83L+OyhtAiBN7YdAlsvgUcswFrRFz5qnLBraJapZugVgmrHmCtiavzSWINVShCkD8Qk5nqchEJ5fJ2/6LHiG0TDzCVBw2CA4dFCwr7DR9qyyPsrv4ui/44xH0a8KERkwnQn2Rzkm7BFrm9E9zxn0ujeLdBiuiLkwCWT5hyM0TxEbcTMGYdMx9ekUVfxhe80ex3MNwD5iMLEI23L6eCnxljf88nPn3twYMYDiOce/H/LijRUckjzxNkbgy4BBxCcWSy//qSX2VAWNOQizEua0O7N8yiAQncJTLGViqnxc76K7UZtgoMGsU1pytrWpLtGoVBvKjTnc1lYx0qWq4CtUD408B8/NC9YU+e4L4BqcC2qim0FTfNY3pxgxo6islU0nFJ0yybn6pPnfVmjmWxZb6RPqGL29EXjIh9nqrH/p0xHWQlCP1ftV1PFyEeflGaeXqdf9b7uD9IsIxPjx+bDI2+nvzwT8MJ/DRusZ5HS3AFlufqwVZKmVruJ+11OJFGFa3ZDTYfsL3uSb6Wba+9Ni6+Ii49GbNangbVSKXfPjt4pzfi+NQAB8V5QdeA6b/nHCQusMphFiMjJPNXuwZEPmYoEEQtU2ybJnu0YTIDV36QcEvcCUxK2J0lmCMWrsZPefVZWaHlUQ9yJzEc0jf2Ou9JWxhU0CcjwAEtnQ4eZBKAtczxdiHXoDqbEAlTBdXmJkbYG77d9kCbKY/7+FPqHp3po8qLXL6V4Pwz1p8qk53BiMBJ72p44QIEsV+oH3w3akd0fbSIDcD8Bz+zysVIahkEbuwgBC7DR6aKtKmtMBpZg2GbpIhxBeyOUU7e+XdLl3jMTvCmrfyhAhrNRn+axqiKQFWSevKwCqjYOMs0KUnIUaTckeyG8GyGIM40pDk8Vy6cxVsKXtYp82jHl4EqimeNILlXA16z4Ddlswnqq7HKJo1q+vSo4iCK7um8fR6Oy9OTftWsmx6TMka1jKZv3NxVQLksScn1gRG4ppshVs+d+PlEBE8TPSjGWrDoOxc1Bb46INET6C/TrfqK4yHO/QOHWNZSOwa77hha6JTcStXvNVwEueqbCFOwMYMT+HYhUX4Wdx9kkZ7JJZWoSW+AyJ3ZQdGpixjHKYNThKfiJlUe2T',
    '__VIEWSTATEENCRYPTED' = '',
    '__ASYNCPOST' = 'true',
    'ctl00$cph1$ButtonViewEmission' = 'Click to View Emissions'
  )
}




# Define function to run POST and return dataframe
ageis_table <- function(year, location, gas){
  df <- POST(
    url = url_ar4,
    add_headers(.headers = ageis_ar4_headers),
    body = ageis_ar4_form_options(year = year, location = location, gas = gas),
    encode = "form"
    ) %>% 
    read_html() %>% 
    html_element("#ctl00_cph1_GridViewReport") %>% 
    html_table() %iferror%
    return(NULL)
  
  df$year <- year
  df$location <- location
  df$gas <- gas
  
  return(df)
}  


# Initial scrape, to get accurate levels ---------------

test_data <- POST(
  url = url_ar4,
  add_headers(.headers = ageis_ar4_headers),
  body = ageis_ar4_form_options(year = 2019, location = "Australia", gas = "Carbon Dioxide Equivalent - AR4"),
  encode = "form"
) %>% 
  read_html() # %>% 
  # html_element("#ctl00_cph1_GridViewReport") %>%
  # html_table()

table_levels <- test_data %>% 
  html_element("#ctl00_cph1_GridViewReport") %>%
  xml2::xml_contents() %>%
  as.list() %>%
  as.character() %>%
  str_subset("padding-left") %>% 
  str_match(pattern = ".*padding-left:(\\d*)px.*") %>%
  .[,2] %>%
  parse_number() 

table_levels <- table_levels/15


# Scraping --------------------------------------------

# Define all plausible combinations of year, location and gas
all_options_ar4 <- expand.grid(
  years = 1990:2019,
  locations = location_table$location,
  gases = gas_table_ar4$gas
)

parallel = FALSE

if(parallel){
  
  # Initialize clusters for multithreaded scraping
  cl <- makeCluster(parallel::detectCores(logical = T))
  clusterEvalQ(cl, {library(tidyverse); library(httr); library(rvest)})
  clusterExport(cl, c(
    "url",
    "ageis_headers",
    "ageis_form_options",
    "ageis_table",
    "all_options_ar4",
    "%iferror%",
    "gas_table_ar4",
    "location_table"
  )
  )
  
  
  
  
  # Run POST for all option combinations
  ar4_data <- pblapply(
    1:nrow(all_options_ar4),
    function(i){
      ageis_table(
        all_options_ar4$years[i],
        all_options_ar4$locations[i],
        all_options_ar4$gases[i]
      )
    },
    cl = cl
  )
  
  
  
  
  # Close cluster
  stopCluster(cl)
  
  ar4_data <- ar4_data %>%
    bind_rows()
  
}

if(!parallel){
  
  # try it with purrr?
  tic("Getting data took")
  # ar4_data <- purrr::pmap_dfr(.l = list('year' = all_options_ar4$years,
  #                                       'location' = all_options_ar4$locations,
  #                                       'gas' = all_options_ar4$gases),
  #                             .f = ageis_table)
  future::plan("multiprocess")
  ar4_data <- furrr::future_pmap_dfr(.l = list('year' = all_options_ar4$years,
                                          'location' = all_options_ar4$locations,
                                          'gas' = all_options_ar4$gases),
                                          .f = ageis_table)
  
  toc()
  
  
}


  

# Row bind and clean output data
ar4_data_first_clean <- ar4_data %>%
  rename(
    sector_code = 1,
    sector = Category,
    gigagrams = `Gg (1,000 Tonnes)`
  ) %>%
  # Now we add on the category level using depths obtained from the test run
  mutate(sector_level = rep(table_levels, nrow(ar4_data)/length(table_levels))) %>%
  # Now we tidy the other columns
  mutate(
    emissions_kt = gigagrams %>% str_remove_all(",") %>% as.numeric(),
    available = case_when(!is.na(emissions_kt) ~ "available", 
                          gigagrams == "Data is confidential" ~ "confidential", 
                          gigagrams == "Data is not available" ~ "not_available"),
    gas = gas %>% str_to_lower() %>% str_replace_all("[:blank:]", "_")
  ) %>%
  select(year, location, gas, sector_level, sector_code, sector, emissions_kt, available) 



ar4_data_first_clean %>%
  write_fst("data/all_ar4_data.fst")


ar4 <- read_fst("data/all_ar4_data.fst") 

grattan_categories <- read_excel("../../2a. Data/source categories master list.xlsx",
                                 sheet = "source categories master list",
                                 range = "A3:G1230")


ar4 %>%
  filter(year == 2019 & location == "Australia" & gas == "carbon_dioxide") %>%
  select(sector_code, sector_level, sector) %>%
  mutate(unfccc_identifier = row_number()) %>%
  write_csv("data/sectors_list.csv")
  

# Give each row unique identifier (unique_sector_code) --------------------

ar4 %>%
  group_by(year, location, gas) %>%
  mutate(unfccc_identifier = row_number()) %>%
  select(year, location, gas, sector_level, sector_code, sector, unfccc_identifier, emissions_kt) %>%
  left_join(grattan_categories %>%
              select(-sector_code), 
            by = c("unfccc_identifier", "sector", "sector_level")) %>%
  pivot_wider(names_from = gas, values_from = emissions_kt) %>%
  write_csv("data/all_ar4_data_wide.csv")




