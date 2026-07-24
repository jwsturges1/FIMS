#### library(tidyverse) ----
# install.packages("tidyverse")
# raw_data = read.csv("data/FCE_Master_FIMS_data..csv") 

# library(vegan)
# install.packages("vegan")

clean_data = raw_data %>% 
  mutate(species_group = common_name)


unique_species <- clean_data %>%
  distinct(common_name) %>%
  arrange(common_name)

unique_species



clean_data = clean_data %>% 
  mutate(species_group = case_when(common_name %in% c("Tidewater Mojarra",
                                                      "Silver Jenny",
                                                      "Flagfin Mojarra",
                                                      "striped Mojarra") ~ "Mojarra spp."))
clean_data = clean_data %>% 
  mutate(species_group = case_when(common_name %in% c("Clown Goby",
                                                      "Code Goby",
                                                      "Goby spp.",
                                                      "Crested Goby") ~ "Goby spp."))

clean_data = clean_data %>% 
  mutate(species_group = case_when(common_name %in% c("Rainwater Killifish",
                                                      "Goldspotted Killifish") ~ "Killifish spp."))

clean_data = clean_data %>% 
  mutate(species_group = case_when(common_name %in% c("Inland Silverside",
                                                      "Hardhead Silverside") ~ "Silverside spp."))


library(dplyr)

clean_data <- raw_data %>%
  mutate(
    species_group = case_when(
      common_name %in% c("Tidewater Mojarra",
                         "Silver Jenny",
                         "Flagfin Mojarra",
                         "Striped Mojarra") ~ "Mojarra spp.",
      
      common_name %in% c("Clown Goby",
                         "Code Goby",
                         "Goby spp.",
                         "Crested Goby") ~ "Goby spp.",
      
      common_name %in% c("Rainwater Killifish",
                         "Goldspotted Killifish") ~ "Killifish spp.",
      
      common_name %in% c("Inland Silverside",
                         "Hardhead Silverside") ~ "Silverside spp.",
      
      TRUE ~ common_name
    )
  )


clean_data = clean_data %>% 
  mutate(site_year = paste(site, year, sep = "_"))  


commm_matrix = clean_data %>% 
  pivot_wider(names_from = site_year, values_from = count(clean_data$common_name))

unique(unique_species) <- clean_data %>%
  distinct(common_name) %>%
  arrange(common_name)



