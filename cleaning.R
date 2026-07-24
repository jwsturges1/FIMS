library(tidyverse)

raw_data = read.csv("data/FCE_Master_FIMS_data.csv") 


clean_data = raw_data %>% 
  mutate(species_group = common_name)


clean_data = clean_data %>% 
  mutate(common_name = case_when(species_group %in% c("Tidewater Mojarra",
                                                      "Silver Jenny",
                                                      "Flagfin Mojarra",
                                                      "striped Mojarra") ~ "Mojarra sp."))

clean_data = clean_data %>% 
  mutate(site_year = paste(site, year, sep = "_"))  


commm_matrix = clean_data %>% 
  pivot_wider(names_from = site_year, values_from = count(clean_data$common_name))
