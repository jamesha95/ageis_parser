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
                       "Domestic Navigation",
                       # "Industrial Processes" ,
                       # "Agriculture" ,
                       # "Land Use, Land-Use Change and Forestry UNFCCC",
                       "Waste")) %>%
  ggplot(aes(x = year, y = emissions_Mt, group = sector)) + 
  geom_line() + 
  facet_wrap(~sector, nrow = 4, scales = 'free_y') +
  scale_y_continuous() +
  theme_grattan()


  
