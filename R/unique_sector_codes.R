# Trying to create meaningful, unique sector codes


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



ar4 <- read_fst("data/all_ar4_data.fst") 


# We run into the problem here that we want each row to be evaluated in order
#  but lag() is sourcing the as-yet-un-mutated sector_code 
# That means we need to resort to using accumulate(), 
#  so that as values are updated, they're fed into the calculation for the next row.

# Accumulate can't seem to handle vector inputs for arguments apart from .x
# What I'd really need is a function like accumulate2(), which can another vector input,
#  but in fact accumulate3() (to handle three vector inputs and one rolling value).

# Instead, I'll use accumulate2(), but turn out lagged_level and current_level vectors
#  into one vector, then split it inside the function. 

update_sector_code_based_on_prior <- function(lagged_code, 
                                              current_code,
                                              lagged_and_current_level){
  
  bits <- str_split(lagged_and_current_level, pattern = ";")[[1]]
  
  lagged_level <- as.numeric(bits[[1]])
  current_level<- as.numeric(bits[[2]])
  
  add_1 <- function(string){
    components <- str_match(string, "(.*\\.)(\\d+)$")
    
    new_number <- 1 + as.numeric(components[,3] )
    
    paste0(components[,2], new_number)
    
  }
  
  digits <- function(string){
    str_count(string, pattern = "\\.") + 1
  }
  
  trunc_code <- function(string, level){
    
    string %>%
      str_split(pattern = "\\.") %>%
      .[[1]] %>%
      .[1:level] %>%
      str_c(collapse = ".")
    
    
  }
  
  
  
  case_when(
    # Do nothing if the current level is 0, 1, or 2 deep (they're all fine)
    current_level < 3 ~ current_code,
    # If our current code has enough digits, do nothing
    digits(current_code) == current_level ~ current_code,
    # If our current code has too few digits...
    digits(current_code) < current_level ~
      # If we're at a new level of depth, add .1 to the code
      case_when(lagged_level < current_level ~ paste0(lagged_code, ".1"),
                # If we're still at the same depth, take the newly-updated lagged code and increase the last digit
                lagged_level == current_level ~  add_1(lagged_code),
                # If we're moving to a higher level category, but have too few digits, it's trickier
                # We need to find the previous entry at this level and add 1
                lagged_level > current_level ~ trunc_code(lagged_code,
                                                          level = current_level) %>%
                  add_1()
                
      ),
    TRUE ~ current_code)
  
  
  
}

# Edit any bugs out ----------
# There are some sector codes that erroneously end in "." rather than a number:
#   - 1.B.2.b.6.i.
#   - 3.B.1.c.
# We need to drop the final "." 

# There are also codes that are subsets of other categories yet their code does not reflect this:
#   - 4.D.1.4
#   - 4.D.1.5
# These are both, in fact, subsets of 4.D.1.3, and should really be reclassified as 4.D.1.3.vi and 4.D.1.3.vii respectively.

# Two level 5 codes have been given the "1.A.2.g.iii" code. The first of these looks like it should really be "1.A.2.g.ii"

# The codes starting "1.B.2.b.6.ii" are a mess. The simplest fix to get unique codes appears to be to assign the 
#  second "1.B.2.b.6.ii" (which we would normally mutate to be "1.B.2.b.6.ii.1") as "1.B.2.b.6.ii.0", 
#  because "1.B.2.b.6.ii.1" already exists!\
# However, the subcategories within "1.B.2.b.6.ii.0" won't start "1.B.2.b.6.ii.0", unfortunately. 
# The second instance of "2.D.3.ii.b" should be "2.D.3.ii.c" 
# The "3.A.1.b.i" codes do not need the ".i" at the end

ar4 %>%
  filter(year == '2019' & location == 'Australia' & gas == "carbon_dioxide") %>%
  filter(str_detect(sector_code, pattern = "\\.$"))


ar4_unique <- ar4 %>%
  filter(year == '2019' & location == 'Australia' & gas == "carbon_dioxide") %>%
  # Take out erroneous dots
  mutate(sector_code = str_remove(sector_code, pattern = "\\.$")) %>%
  # Fix erroneous subcategories 
  mutate(sector_code = case_when(sector_code == "4.D.1.4" ~ "4.D.1.3.vi",
                                 sector_code == "4.D.1.5" ~ "4.D.1.3.vii",
                                 sector_code == "1.A.2.g.iii" & str_starts(sector, "Mining", negate = TRUE) ~ "1.A.2.g.ii",
                                 sector_code == "1.B.2.b.6.ii" & sector == "Appliances in commercial sector" ~ "1.B.2.b.6.ii.0",
                                 sector_code == "1.B.2.b.6.ii.2" & str_starts(sector, "Commer") ~ "1.B.2.b.6.ii.0",
                                 sector_code == "2.D.3.ii.b" & sector ==	"Use of N2O for Anaesthesia" ~ "2.D.3.ii.c",
                                 sector_code == "3.A.1.b.i" ~ "3.A.1.b",
                                 sector_code %in% c("3.A.1.b.ii", "3.A.1.c.i") ~ "3.A.1.c",
                                 str_starts(sector_code, "3\\.B\\.3") & sector %in% c("Sheep", "Rams", "Wethers", "Maiden Ewes", "Breeding Ewes", "Other Ewes", "Lambs and Hoggets") ~ str_replace(sector_code, "\\.3", ".2"),
                                 TRUE ~ sector_code)) 
ar4_unique <- ar4_unique %>%
  # Combine two columns into one column for input into accumulate2()
  mutate(lagged_and_current_level = paste0(lag(sector_level, default = -1), ";", sector_level)) %>%
  # Create new sector codes
  mutate(unique_sector_code = purrr::accumulate2(.x = sector_code,
                                                 .y = lagged_and_current_level[-1],
                                                 .f = update_sector_code_based_on_prior) %>% unlist()
  )

ar4_unique$unique_sector_code %>%
  unique() %>%
  length()

ar4_unique %>%
  filter(duplicated(unique_sector_code))

ar4_unique %>%
  filter(str_starts(sector_code, "1.B.1")) %>%
  view()

ar4_unique %>%
  filter(str_starts(sector_code, "3.B.2")) %>%
  view()