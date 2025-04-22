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


## 2. Spatial Patterns of Turnout and Competitiveness
### 2.1. Absenteeism
turnout <- uk24 %>% select(1:10) %>% # Only turnout and participation indicators
  distinct()

# v1 <- Absenteeism (considering valid and invalid votes)
turnout$v1 <- 1- ((turnout$election_valid_vote_count + 
                     turnout$election_invalid_vote_count)/turnout$electorate)

# v2 <- Absenteeism (considering only valid votes)
turnout$v2 <- 1- (turnout$election_valid_vote_count/turnout$electorate)


### 2.2. Competitiveness
comp <- uk24 %>% select(1:4,14,18)
comp$main_party_abbreviation[comp$main_party_abbreviation == ""] <- "Independent"

# Indicator of competitiveness
comp <- comp %>%
  group_by(constituency_geographic_code) %>%
  arrange(desc(candidate_vote_share), .by_group = TRUE) %>%
  mutate(
    vote_share_lead = candidate_vote_share[1],
    vote_share_runnerup = candidate_vote_share[2],
    vote_share_margin = vote_share_lead - vote_share_runnerup
  ) %>%
  ungroup()

comp_margin <- comp %>%
  select(constituency_geographic_code, vote_share_margin) %>%
  distinct()

# Winners per constituency
comp_winners <- comp %>%
  group_by(constituency_geographic_code) %>%
  slice_max(candidate_vote_share, n = 1, with_ties = FALSE) %>%
  ungroup() 

comp_winners <- comp_winners %>% select(constituency_geographic_code, 
                                        main_party_abbreviation)

turnout <- merge(turnout, comp_margin, by = "constituency_geographic_code")
turnout <- merge(turnout, comp_winners, by = "constituency_geographic_code")

pcon_2024_merged <- pcon_2024 %>% # Merge with shapefile
  left_join(turnout, by = c("PCON24CD" = "constituency_geographic_code")) 



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
    name = "Absenteeism (0-1)"
  ) +
  labs(
    title = "UK General Election 2024",
    subtitle = "Absenteeism",
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

#### C: Spatial Patterns of Competitiveness
comp_map <- ggplot(pcon_2024_merged) +
  geom_sf(aes(fill = vote_share_margin), colour = NA) +  # no lines between polygons
  scale_fill_viridis_c(
    option = "A",  # high-contrast palette
    name = "Margin (0-1)"
  ) +
  labs(
    title = "UK General Election 2024",
    subtitle = "Vote Share Margin",
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
  filename = "comp_map_contrast.jpeg",
  plot = comp_map,
  path = "/Users/alexander/Documents/GitHub/turnout-uk/figs",
  width = 10,
  height = 12,
  dpi = 300
)


#### D: Results
party_colours <- c(
  "Lab" = "#E4003B",      # Labour red
  "Con" = "#0087DC",      # Conservative blue
  "LD"  = "#FAA61A",      # Lib Dem orange
  "SNP" = "#FFF95D",      # SNP yellow
  "Green" = "#6AB023",    # Green Party green
  "PC" = "#008142",       # Plaid Cymru green
  "RUK" = "#A349A4",      # Reform UK purple
  "DUP" = "#D46A4C",
  "SF" = "#326760",
  "SDLP" = "#003B71",
  "APNI" = "#003B71",
  "UUP" = "#003B71",
  "TUV" = "#003B71",
  "Independent" = "#B0B0B0"
)

party_map <- ggplot(pcon_2024_merged) +
  geom_sf(aes(fill = main_party_abbreviation), colour = "gray", size = 0.05) +
  scale_fill_manual(
    values = party_colours,
    name = "Winning Party"
  ) +
  labs(
    title = "UK General Election 2024",
    subtitle = "Winning Party by Constituency",
    caption = "Source: UK Parliament"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank()
  )

# Save it
ggsave(
  filename = "party_map.jpeg",
  plot = party_map,
  path = "/Users/alexander/Documents/GitHub/turnout-uk/figs",
  width = 10,
  height = 12,
  dpi = 300
)