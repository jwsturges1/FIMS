#### library(tidyverse) ----
library(tidyverse)
# install.packages("tidyverse")
raw_data = read.csv("data/FCE_Master_FIMS_data.csv") 

library(vegan)
# install.packages("vegan")

clean_data = raw_data %>% 
  mutate(species_group = common_name)


unique_species <- clean_data %>%
  distinct(common_name) %>%
  arrange(common_name)

unique_species

species_counts <- clean_data %>%
  count(common_name, sort = TRUE)

species_counts



clean_data = clean_data %>% 
  mutate(species_group = case_when(common_name %in% c("Tidewater Mojarra",
                                                      "Silver Jenny",
                                                      "Flagfin Mojarra",
                                                      "striped Mojarra") ~ "Mojarra spp."))
clean_data = clean_data %>% 
  mutate(species_group = case_when(common_name %in% c("Clown Goby",
                                                      "Code Goby",
                                                      "Goby spp.",
                                                      "Crested Goby", 
                                                      "Spottail Goby") ~ "Goby spp."))

clean_data = clean_data %>% 
  mutate(species_group = case_when(common_name %in% c("Rainwater Killifish",
                                                      "Goldspotted Killifish") ~ "Killifish spp."))

clean_data = clean_data %>% 
  mutate(species_group = case_when(common_name %in% c("Inland Silverside",
                                                      "Hardhead Silverside") ~ "Silverside spp."))

clean_data = clean_data %>% 
  mutate(species_group = case_when(common_name %in% c("Dwarf Seahorse",
                                                      "Northern Seahorse",
                                                      "Yellow Seahorse") ~ "Seahorse spp."))


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
  select(site_year,site,year,common_name)
  # pivot_wider(names_from = site_year, values_from = count(clean_data$common_name))

species_counts <- clean_data %>%
  count(common_name, sort = TRUE)

species_counts


library(dplyr)
library(tidyr)

species_site_year <- clean_data %>%
  count(common_name, site, year) %>%   # count observations
  complete(
    common_name,
    site,
    year,
    fill = list(n = 0)                 # zero-fill missing combinations
  ) %>%
  arrange(common_name, site, year)

wide_community=wide_community %>% 
  mutate(site_year=paste(site,year,sep = "_"))




# wide_community <- species_site_year %>%
#   pivot_wider(names_from = common_name,
#               values_from = n)

wide_community2 = wide_community %>% 
  column_to_rownames(var = ("site_year"))

wide_community2 = wide_community2 %>% 
  select(!c(site, year))


nmds_comm = metaMDS(wide_community2,
                    trymax =200,
                    distance = "bray",
                    autotransform = F)

tibble_nmds = as_tibble(scores(nmds_comm$points),
                        rownames=("site_year"))

wide_community = wide_community %>% 
  left_join(tibble_nmds)



colors=c("red","blue","orange","purple")

plots_nmds = ggplot(wide_community,
                    aes(MDS1,MDS2))+
  geom_point(aes(shape=as_factor(year),color=site),size=4)+
  theme_classic()+
  scale_colour_manual(values=c("red","blue","orange","purple"))

  plots_nmds
                    
