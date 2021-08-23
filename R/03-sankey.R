library(readxl)
library(tidyverse)
library(grattantheme)
library(ggrepel)
library(fst)
library(tictoc)
library(glue)

`%nin%` <- Negate(`%in%`)

gpal_ol = "#f68b33"
gpal_ol1 = "#f79747"
gpal_ol2 = "#f8a25c"
gpal_ol3 = "#f9ae70"
gpal_ol4 = "#fab985"
gpal_ol5 = "#fbc599"
gpal_ol6 = "#fbd1ad"
gpal_ol7 = "#fcdcc2"
gpal_ol8 = "#fde8d6"


# Set up -------


grattan_categories <- read_excel("../../2a. Data/source categories master list.xlsx",
                                 sheet = "source categories master list",
                                 range = "A3:G1230")

tic("Reading in all the AR4 data")
ar4_2019 <- read_fst("data/AR4/filled_data_ar4.fst") %>%
  filter(year == 2019 & gas ==  "carbon_dioxide_equivalent_-_ar4" & location == "Australia") %>%
  left_join(grattan_categories %>%
              select(-sector_code, -sector), 
            by = c("unfccc_identifier", "sector_level")) %>%
  filter(!is.na(emissions_kt))
toc()

ar4_2019 %>%
  filter(!is.na(name_10_digit)) %>%
  summarise(total = sum(emissions_kt, na.rm = T)) %>%
  pull(total)

ar4_2019 %>% head() %>% glimpse()



make_sankey_code <-  function(data, .digits = 0){
  
  if(any(data$emissions_kt < 0)) warning("Negative emissions do not appear on Sankey!")
  
sankeymatic <- list()

for(i in 0:9){
  
  sankeymatic[[i + 1]] <-   
    data %>%
    filter(!is.na(name_10_digit)) %>%
    mutate(emissions_kt = round(emissions_kt, digits = .digits)) %>%
    filter(emissions_kt > 0) %>%
    # group by adjacent columns
    group_by(!!sym((glue("name_{i}_digit"))), 
             !!sym((glue("name_{i + 1}_digit")))) %>%
    # summarise to get relevant flow widths
    summarise(emissions_kt = sum(emissions_kt)) %>%
    # produce the code for sankeymatic.com
    mutate(key = str_c(!!sym(glue("name_{i}_digit")), 
                       " [", emissions_kt, "] ", 
                       !!sym(glue("name_{i + 1}_digit")))) %>%
    # remove 1:1 links
    filter(!!sym((glue("name_{i}_digit"))) != !!sym((glue("name_{i + 1}_digit")))) %>%
    # keep only the sankeymatic code
    pull(key)
}


sankeymatic %>% 
  unlist() 

}

for(j in unique(ar4_2019$allocation_2)){
  
  sectoral_data <- ar4_2019 %>%
    filter(allocation_2 == j)
  
  make_sankey_code(sectoral_data) %>%
    #cat(sep = "\n") %>%
    write_lines(path = glue("charts/sankey/{j}_code.txt"))

}

make_sankey_code(ar4_2019) %>%
  #cat(sep = "\n") %>%
  write_lines(path = glue("charts/sankey/all_code.txt"))


# industrial emissions -------------------------------------------
# 
# ar4_2019 %>%
#   filter(!is.na(name_10_digit)) %>%
#   filter(name_3_digit == "1188: Industrial Wastewater" |
#            name_3_digit == "15: Manufacturing Industries and Construction" |
#            name_4_digit == "6: Petroleum Refining" |
#            name_4_digit == "6: Manufacture of Solid Fuels and Other Energy Industries" |
#            name_5_digit == "61: Pipeline transport" |
#            name_2_digit == "111: Fugitive Emissions From Fuels" |
#            name_1_digit == "296: Industrial Processes") %>%



aggregated_data <-  ar4_2019 %>%
  filter(!is.na(name_10_digit)) %>%
  # Optional: pick only certain Grattan categories
  #filter(allocation_2 == "Ag_Land") %>%
  filter(allocation_2 == "Stationary_Industrial_Fugitives") %>%
  select(starts_with("name_"), emissions_kt)

aggregated_data %>% 
  filter(emissions_kt < 0) %>%
  glimpse()

for(digit in 9:0){
  
  upstream <- paste0("name_", digit, "_digit")
  downstream <- paste0("name_", digit + 1, "_digit") 
  
  all_names_to_group_by <- 0:10 %>%
    paste0("name_", ., "_digit") 
  
  all_downstream <- (digit + 1:10) %>%
    pmin(10) %>%
    unique() %>%
    paste0("name_", ., "_digit") 
  
  aggregated_data <- aggregated_data %>%
    group_by(!!sym(downstream)) %>%
    mutate(group_emissions_kt = sum(emissions_kt, na.rm = TRUE)) %>%
    ungroup() %>%
    # if the group is small, we assign it to the upstream group
    mutate(across(.cols = one_of(all_downstream),
                  .fns = ~if_else(group_emissions_kt > 2000, 
                                  .x, 
                                  !!sym(upstream) %>%
                                          # drop ": small sources" if it already exists
                                          str_remove(" (small sources)") %>%
                                          str_c(" (small sources)")),
                  .names = "{.col}")) %>%
    # Now some very specific manual aggregations
    mutate(across(.cols = one_of(all_downstream),
                  .fns = ~if_else( !!sym(upstream) %nin% c("281: Venting and Flaring",
                                                          "114: Underground Mines",
                                                          "407: Product uses as substitutes for Ozone Depleting Substances"),
                                  .x,
                                  !!sym(upstream)),
                  .names = "{.col}")) %>%
    group_by_at(.vars = vars(one_of(all_names_to_group_by))) %>%
    summarise(emissions_kt = sum(emissions_kt, na.rm = TRUE), .groups = 'drop')

}

aggregated_data %>%
  # Let's convert to Mt (but leave the label as kt for the function to work)
  mutate(emissions_kt = emissions_kt/1000) %>%
  make_sankey_code(.digits = 1) %>%
  str_replace("407: Product uses as substitutes for Ozone Depleting Substances",
              "407: Substitutes for Ozone Depleting Substances") %>%
  str_replace("1: Total UNFCCC",
              "Total emissions") %>%
  # can't take out the numbers; it'll mess up the sankey if there's any duplicate names (e.g. coal mining)
  # str_remove(pattern = "\\d+:\\s") %>%
  # str_remove(pattern = "\\d+:\\s") %>%
  
  # shuffle the order?
  sample() %>%
  cat(sep = "\n")




industrial_total_kt <- sum(aggregated_data$emissions_kt)
industrial_total_kt

ordered_aggregated_data <- aggregated_data %>%
  arrange(name_0_digit,
          name_1_digit,
          name_2_digit,
          name_3_digit,
          name_4_digit,
          name_5_digit,
          name_6_digit,
          name_7_digit,
          name_8_digit,
          name_9_digit,
          name_10_digit) %>%
  mutate(across(.cols = starts_with("name_"), 
                .fns = ~str_replace(.x,
                                    "1: Total UNFCCC",
                                    "1: Industrial emissions")))

rects <- data.frame()

for(digit in 1:10){
  
  this_level <- paste0("name_", digit, "_digit")
  upstream <- paste0("name_", digit - 1, "_digit")
  
  this_level_and_all_upstream <- (digit - 0:10) %>%
    pmax(0) %>%
    unique() %>%
    paste0("name_", ., "_digit")
  
  this_level_and_all_upstream_digit_only <- paste0(this_level_and_all_upstream, "_numeral")
  
  these_rects <- ordered_aggregated_data %>%
    mutate(across(.cols = starts_with("name"), .fn = parse_number, .names = "{.col}_numeral")) %>%
    group_by_at(.vars = vars(one_of(c(this_level_and_all_upstream_digit_only,
                                      this_level_and_all_upstream)))) %>%
    summarise(emissions_kt = sum(emissions_kt, na.rm = TRUE), .groups = 'drop') %>%
    mutate(#label = if_else(!!sym(this_level) == !!sym(upstream), NA_character_, !!sym(this_level)),
           label = !!sym(this_level),
           left = digit - 1,
           right = digit, 
           top = cumsum(emissions_kt)) %>%
    mutate(bottom = lag(top, default = 0)) %>% 
    select(label, left, right, top, bottom) %>%
    mutate(top = round(top, digits = 3),
           bottom = round(bottom, digits = 3))
  
  rects <- rects %>%
    bind_rows(these_rects)
  
}





tidy_rects <- rects %>%
  group_by(top, bottom, label) %>%
  summarise(left = min(left),
            right = max(right),
            .groups = 'drop') 
  
left_max <- max(tidy_rects$left)
  
# No need to keep deeper name levels if they aren't relevant
tidy_rects %>%
  mutate(label_without_number = str_to_sentence(str_remove(label, "\\d+:\\s")),
         label_without_number = case_when(label_without_number == "Fugitive emissions from fuels" ~ "Fugitive emissions\nfrom fuels",
                                          label_without_number == "Manufacturing industries and construction" ~ "Manufacturing and\nconstruction",
                                          label_without_number == "Manufacturing industries and construction (small sources)" ~ "Manufacturing and construction (small sources)",
                                          label_without_number == "Manufacture of solid fuels and other energy industries" ~ "Fossil Fuel\nextraction",
                                          label_without_number == "Product uses as substitutes for ozone depleting substances" ~ "Substitutes for ozone depleting substances",
                           TRUE ~ label_without_number)) %>%
  
  
  mutate(right = pmin(right, left_max + 1)) %>%
  # Now let's make nice labels
  mutate(mid_height = (bottom + top)/2,
         mid_width = (left + right)/2,
         end_category = if_else(right == max(right), TRUE, FALSE), 
         emissions_Mt = round((top - bottom)/1000, 1),
         full_label = paste0(label_without_number,
                             ": ", emissions_Mt)) %>%
  # Optional: Add "million tonnes" to the energy category
 # mutate(full_label = if_else(str_starts(full_label, "Energy: "), paste0(full_label, "\nmillion tonnes"), full_label)) %>%
  mutate(full_label = if_else(emissions_Mt < 2, "-", full_label)) %>%
  # mutate(full_label = if_else(!end_category,
  #                             str_wrap(full_label, width = 30),
  #                             full_label)) %>%
  mutate(fill_intensity = (1/right)^(1/10),
         right_as_factor = factor(right)) %>%
  ggplot() + 
  geom_rect(aes(xmin = left, xmax = right,
                ymin = bottom, ymax = top,
                #alpha = fill_intensity), 
                fill = right_as_factor),
            #fill = grattan_orange_f,
            colour = 'white',
            size = 0.2) + 
  geom_rect(aes(xmin = right - 0.05, xmax = right + 0.05, 
                ymin = bottom + 100, ymax = top - 100),
            size = 0, fill = grattan_orange) + 
  annotate(geom = 'rect', 
           xmin = -0.05, xmax = 0.05, 
           ymin = 0 + 100, ymax = max(tidy_rects$top) - 100,
           size = 0, fill = grattan_orange) +
  geom_text(data = . %>%
              filter(!end_category),
            aes(label = full_label, x = mid_width, y = mid_height),
            size = 10/.pt) + 
  geom_text(data = . %>%
              #filter(emissions_Mt > 1.5) %>%
                    filter(end_category),
                  aes(label = full_label, x = right + 0.08, y = mid_height),
                  size = 10/.pt,
                  hjust = 0) +
  # geom_text_repel(data = . %>%
  #             filter(end_category),
  #           aes(label = full_label, x = right, y = mid_height),
  #           size = 8/.pt, nudge_x = 0, xlim = c(6, 10), min.segment.length = 0,
  #           segment.size = 0.1,
  #           hjust = 0) +
  annotate('text', label = paste0("All industrial\nemissions:\n", 
                                  round(industrial_total_kt/1000, 0), 
                                  " million tonnes"),
           x = -0.5, y = industrial_total_kt/2,
           size = 14/.pt) + 
  scale_fill_manual(values = c(#gpal_ol,
                               #gpal_ol1,
                               #gpal_ol2,
                               gpal_ol3,
                               gpal_ol4,
                               gpal_ol5,
                               gpal_ol6,
                               gpal_ol7,
                               gpal_ol8)) +
                               
  theme_grattan() + 
  scale_y_reverse(expand = expansion(mult = c(0, 0))) +
  scale_x_continuous(limits = c(-0.5, 7.5)) + 
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.line = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank()) + 
  NULL

grattan_save(filename = 'charts/sankey/industrial_sankey_R.pdf', 
             type = 'all', 
             save_data = TRUE)

# Notes on what to do next

# make an Ag sankey (sAgkey)
# make a sink sankey (sinkey)


