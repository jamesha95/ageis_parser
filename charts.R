# Charts

library(grattantheme) 

data_to_plot <- read_fst("data/filled_data_ar4_co2e_australia.fst")

data_to_plot %>%
  mutate(emissions_Mt = emissions_kt/1000) %>%

  # Get specific data 
  filter(sector %in% c("Total UNFCCC",
                       # "Fugitive Emissions From Fuels",
                       # "Public Electricity and Heat Production",
                       # "Manufacturing Industries and Construction",
                       "Transport",
                       "Domestic aviation",
                       "Cars",
                       "Light Commercial Vehicles",
                       "Heavy-Duty Trucks and Buses",
                       "Motorcycles",
                       "Railways",
                       "Domestic Navigation"#,
                       # "Industrial Processes" ,
                       # "Agriculture" ,
                       # "Land Use, Land-Use Change and Forestry UNFCCC",
                       #"Waste"
                       )) %>%
  group_by(sector) %>% 
  mutate(emissions_index = emissions_kt/emissions_kt[year == 1990]) %>%

  ggplot(aes(x = year, y = emissions_index, group = sector)) + 
  geom_line() + 
  geom_point(data = . %>%
               filter(year %in% c(1990, 2019))) +
  facet_wrap(~sector, nrow = 2, scales = 'fixed') +
  scale_y_continuous(limits = c(0, 4)) +
  theme_grattan()

grattan_save("charts/transport_indices.png", 
             type = 'wholecolumn')

  
