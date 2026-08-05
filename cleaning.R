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

  ####-----------------------------------------------------------
  
  #### 0. Libraries ----
  library(tidyverse)
  library(vegan)
  
  #### 1. Read + clean raw data ----
  # Update this path to point at your master data file
  raw_data <- read.csv("data/FCE_Master_FIMS_data.csv") %>%
    mutate(common_name = str_squish(common_name))  # trims stray/double whitespace
  
  # one-off typo fix
  raw_data <- raw_data %>%
    mutate(common_name = recode(common_name,
                                "Sargassum Sargassum Swimming Crab" = "Sargassum Swimming Crab"))
  
  #### 2. Rare/undesirable species exclusion + taxonomic grouping ----
  
  # species dropped entirely (decided during data review: n < 4 individuals
  # across the whole dataset, with Southern Pufferfish also dropped for now)
  species_to_drop <- c(
    "Caribbean Reef Squid", "Elegant Brittle Star", "Starfish",
    "Hermit Crab", "Ladyfish", "Striped Mullet", "Gastropod SP.",
    "Hogchoker", "Snapping Shrimp", "Spotted Dragonett", "Sargassum Fish",
    "Lizardfish", "Leather Jack", "Decorator Crab", "Spider Crab",
    "Arrow Shrimp", "Southern Pufferfish"
  )
  
  clean_data <- raw_data %>%
    filter(!common_name %in% species_to_drop) %>%
    mutate(
      species_group = case_when(
        common_name %in% c("Tidewater Mojarra", "Silver Jenny",
                           "Flagfin Mojarra", "Striped Mojarra")          ~ "Mojarra spp.",
        common_name %in% c("Clown Goby", "Code Goby", "Goby spp.",
                           "Crested Goby", "Spottail Goby")               ~ "Goby spp.",
        common_name %in% c("Rainwater Killifish", "Goldspotted Killifish") ~ "Killifish spp.",
        common_name %in% c("Inland Silverside", "Hardhead Silverside")     ~ "Silverside spp.",
        common_name %in% c("Gulf Pipefish", "Chain Pipefish", "Pipefish")  ~ "Pipefish spp.",
        common_name %in% c("Dwarf Seahorse", "Northern Seahorse",
                           "Yellow Seahorse")                             ~ "Seahorse spp.",
        common_name %in% c("Pink Shrimp", "White Shrimp")                 ~ "Penaeid Shrimp",
        common_name %in% c("Florida Blenny", "Blenny spp.")               ~ "Blenny spp.",
        common_name %in% c("Ballyhoo", "American Halfbeak")               ~ "Halfbeak spp.",
        common_name %in% c("Mangrove Snapper", "Schoolmaster Snapper")    ~ "Snapper spp.",
        common_name %in% c("Tomtate", "Bluestriped Grunt")                ~ "Grunt spp.",
        common_name == "Crab spp."                                        ~ "Portunid Crabs",
        common_name %in% c("Grass Shrimp", "Shrimp spp.")                 ~ "Palaemonid Shrimp",
        common_name %in% c("Grass Porgy", "Pinfish")                      ~ "Pinfish",
        common_name %in% c("Filefish", "Sargassum Filefish")              ~ "Sargassum Filefish",
        common_name %in% c("Scrawled Cowfish", "Boxfish")                 ~ "Ostracioid Boxfishes",
        TRUE ~ common_name
      )
    )
  
  
  #### 3. Build community matrix (pooled by site x year, across all hauls) ----
  clean_data <- clean_data %>%
    mutate(site_year = paste(site, year, sep = "_"))
  
  species_site_year <- clean_data %>%
    count(species_group, site_year, site, year, name = "n") %>%
    complete(species_group, nesting(site_year, site, year), fill = list(n = 0)) %>%
    arrange(species_group, site_year)
  
  wide_community <- species_site_year %>%
    pivot_wider(names_from = species_group, values_from = n, values_fill = 0)
  
  wide_community2 <- wide_community %>%
    column_to_rownames(var = "site_year") %>%
    select(-site, -year)
  
  #### 4. NMDS ----
  nmds_comm <- metaMDS(wide_community2,
                       trymax = 200,
                       distance = "bray",
                       autotransform = FALSE)
  
  nmds_comm$stress  # check: <0.2 acceptable, <0.1 good fit
  
  tibble_nmds <- as_tibble(nmds_comm$points, rownames = "site_year")
  
  wide_community <- wide_community %>%
    left_join(tibble_nmds, by = "site_year")
  
  #### 5. Plot ----
  plots_nmds <- ggplot(wide_community, aes(MDS1, MDS2)) +
    geom_point(aes(shape = as_factor(year), color = site), size = 4) +
    theme_classic() +
    scale_colour_manual(values = c("red", "blue", "orange", "purple")) +
    labs(shape = "Year", color = "Site")
  
  plots_nmds
  
  #### 6. Title
  
  plots_nmds <- ggplot(wide_community, aes(MDS1, MDS2)) +
    geom_point(aes(shape = as_factor(year), color = site), size = 4) +
    theme_classic() +
    scale_colour_manual(values = c("red", "blue", "orange", "purple")) +
    labs(
      title = "NMDS of Fish Community Composition, Florida Bay",
      subtitle = paste("Stress =", round(nmds_comm$stress, 3)),
      x = "NMDS1",
      y = "NMDS2",
      shape = "Year",
      color = "Site"
    ) +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      legend.title = element_text(face = "bold")
    )
  
  plots_nmds
  
  # "Standard" approach: sqrt transform + Wisconsin double standardization
  # (this is what vegan does automatically when autotransform = TRUE, the default)
  nmds_sqrt <- metaMDS(wide_community2,
                       trymax = 200,
                       distance = "bray",
                       autotransform = TRUE)
  nmds_sqrt$stress
  
  # 4th-root transform, applied manually, then Bray-Curtis with no further auto-transform
  community_4throot <- wide_community2^(1/4)
  
  nmds_4throot <- metaMDS(community_4throot,
                          trymax = 200,
                          distance = "bray",
                          autotransform = FALSE)
  nmds_4throot$stress
  
  cat("Sqrt/Wisconsin stress:", round(nmds_sqrt$stress, 4), "\n")
  cat("4th-root stress:      ", round(nmds_4throot$stress, 4), "\n")
