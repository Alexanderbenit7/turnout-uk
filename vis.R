## Extender Research Project
### Spatial Patterns of Electoral Participation in the United Kingdom
#### MSc Data Science - The University of Manchester

# Libraries:
library(rio)
library(dplyr)
library(janitor)
library(sf)
library(ggplot2)
library(tidyr)


## 1. Load data
data <-  import("/Users/alexander/Documents/MSc Data Science/erp-uom/data/candidate-level-results-general-election-04-07-2024.csv")

pcon_2024 <- st_read("/Users/alexander/Documents/MSc Data Science/erp-uom/shapefiles/2024/PCON_JULY_2024_UK_BFC.shp") # Open 2024 shp

# These are the variables of interest from the whole dataset:
# Total electorate in general election
# Total valid votes in general election
# Total invalid votes in general election
# Country name
# Country geographic code
# Constituency name
# Constituency geographic code
# Electorate
# Election valid vote count
# Election invalid vote count
# Candidate family name
# Candidate given name
# Main party name
# Main party abbreviation
# Main party MNIS ID
# Candidate is standing as independent
# Candidate vote count
# Candidate vote share
# Majority
# Candidate result position

uk24 <- data %>% select(9:11,13,14,19,20,26,30,31,34,35,40,41,42,47:49,51,52) %>% 
  janitor::clean_names() %>% select( # To correct names
    constituency_name,
    constituency_geographic_code,
    country_name,
    country_geographic_code,
    everything()
  ) # All variables of interests


## 2. Spatial Patterns of Turnout and Participation
### 2.1. Construction of indicators
turnout <- uk24 %>% select(1:10) %>% # Only turnout and participation indicators
  distinct()

# v1 <- Absenteeism (considering valid and invalid votes)
turnout$v1 <- 1- ((turnout$election_valid_vote_count + 
                     turnout$election_invalid_vote_count)/turnout$electorate)

# v2 <- Absenteeism (considering only valid votes)
turnout$v2 <- 1- (turnout$election_valid_vote_count/turnout$electorate)

pcon_2024_merged <- pcon_2024 %>% # Merge with shapefile
  left_join(turnout, by = c("PCON24CD" = "constituency_geographic_code"))

# 



### 2.2. Initial Visualisations
#### A: Comparing distributions
turnout_long <- turnout %>% select(2,11,12) %>% # Reshape
  pivot_longer(cols = c(v1, v2), names_to = "metric", values_to = "value")

dist <- ggplot(turnout_long, aes(x = value, fill = metric, colour = metric)) + # Plot
  geom_density(alpha = 0.4, linewidth = 1) +
  labs(
    title = "Density Comparison of Absenteeism Metrics",
    x = "Absenteeism Rate",
    y = "Density",
    fill = "Metric",
    colour = "Metric"
  ) +
  theme_minimal()

ggsave(
  filename = "absenteeism_density_comparison.jpeg", # Save the last plot
  plot = dist,
  path = "/Users/alexander/Documents/GitHub/turnout-uk/figs",
  width = 8,
  height = 6,
  dpi = 300
)

#### B: Spatial Patterns of Absenteeism
absenteeism_map <- ggplot(pcon_2024_merged) +
  geom_sf(aes(fill = v2), colour = NA) +  # no lines between polygons
  scale_fill_viridis_c(
    option = "A",  # high-contrast palette
    name = "Absenteeism\n(Valid Votes)"
  ) +
  labs(
    title = "UK General Election 2024",
    subtitle = "Absenteeism Rate Based on Valid Votes",
    caption = "Source: UK Parlament"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    panel.grid = element_blank(),   # remove grid
    axis.text = element_blank(),    # remove axis labels
    axis.title = element_blank()    # remove axis titles
  )

# Save it
ggsave(
  filename = "absenteeism_v2_map_contrast.jpeg",
  plot = absenteeism_map,
  path = "/Users/alexander/Documents/GitHub/turnout-uk/figs",
  width = 10,
  height = 12,
  dpi = 300
)
