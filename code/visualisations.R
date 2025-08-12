# Libraries
library(rio)
library(dplyr)
library(corrplot)
library(GGally)
library(ggplot2)
library(sf)
library(spdep)
library(tidyr)

# Open full dataset
data <- import('/Users/alexander/Documents/MSc Data Science/erp-uom/data/dataTurnoutFinal.csv')
tda <- import('/Users/alexander/Documents/MSc Data Science/erp-uom/data/constituency_low_participation_summary.csv')
ols_residuals <- import('/Users/alexander/Documents/MSc Data Science/erp-uom/data/collapsed_all.csv')
slag_residuals <- import('/Users/alexander/Documents/MSc Data Science/erp-uom/data/collapsed_all_lag.csv')
sem15_residuals <- import('/Users/alexander/Documents/MSc Data Science/erp-uom/data/sem2015_residuals_mapping.csv')
pcon_2010 <- st_read("/Users/alexander/Documents/MSc Data Science/erp-uom/shapefiles/2010/PCON_DEC_2020_UK_BFC.shp")

# Only turnout
turnout <- data %>% select(1,2,3,7)
options(scipen = 999)

# From long to wide format
df_wide <- turnout %>%
  pivot_wider(
    id_cols = c(constituency_name, constituency_geographic_code),
    names_from = election_year,
    values_from = electoralTurnout
  ) %>%
  rename_with(
    .cols = where(is.numeric),
    .fn = ~ paste0("Y", .x)
  )



### PATTERNS IN SPACE
turnout_years <- df_wide %>% select(starts_with("Y"))

plot_matrix <- ggpairs(
  turnout_years,
  lower = list(continuous = wrap("points", alpha = 0.3, size = 0.8)),
  upper = list(continuous = wrap("cor", size = 3)),
  diag = list(continuous = wrap("densityDiag", alpha = 0.5)),
  title = "Turnout Scatterplot Matrix with Pearson Correlations"
)

ggsave("turnout_scatterplot_matrix.jpg",
       plot = plot_matrix,
       path = "/Users/alexander/Documents/GitHub/turnout-uk/figs/desc",
       width = 10, height = 10, dpi = 600)



# CLUSTERS
set.seed(42)
wss <- function(k) {
  kmeans(turnout_years, centers = k, nstart = 10)$tot.withinss
}

k_values <- 1:10
wss_values <- sapply(k_values, wss)

plot(k_values, wss_values, type = "b", pch = 19,
     xlab = "Number of Clusters (k)",
     ylab = "Total Within-Cluster Sum of Squares",
     main = "Elbow Method for Choosing Optimal k")

k <- 4
kmeans_result <- kmeans(turnout_years, centers = k, nstart = 25)

df_clustered <- df_wide %>% # Add cluster to data
  filter(complete.cases(select(., starts_with("Y")))) %>%
  mutate(cluster = kmeans_result$cluster)

turnout_matrix <- df_wide %>%
  select(starts_with("Y")) %>%
  drop_na()

# Run PCA without scaling (since all are 0–1)
pca_result <- prcomp(turnout_matrix, center = TRUE, scale. = FALSE)

pca_df <- as.data.frame(pca_result$x[, 1:2]) %>%
  mutate(cluster = factor(kmeans_result$cluster))

clust_turnout <- ggplot(pca_df, aes(x = PC1, y = PC2, colour = cluster)) +
  geom_point(alpha = 0.7, size = 2) +
  labs(title = "Cluster Visualisation of Turnout Patterns (PCA)",
       x = "Principal Component 1", y = "Principal Component 2") +
  theme_minimal() +
  scale_colour_brewer(palette = "Set1")

ggsave("turnout_clusters_matrix.jpg",
       plot = clust_turnout,
       path = "/Users/alexander/Documents/GitHub/turnout-uk/figs/clust",
       width = 10, height = 10, dpi = 600)



## Cluster Map
df_clustered %>%
  group_by(cluster) %>%
  summarise(across(starts_with("Y"), mean, na.rm = TRUE))

df_clustered$cluster <- factor(df_clustered$cluster,
                               levels = 1:4,
                               labels = c("Average: 0.58", "Average: 0.64", "Average: 0.68", "Average: 0.73"),
                               ordered = TRUE)


pcon_2010 <- pcon_2010 %>% # Merge with shapefile
  left_join(df_clustered, by = c("PCON20CD" = "constituency_geographic_code")) 

colours <- c(
  "Average: 0.73" = "#00008B",
  "Average: 0.68" = "#1E90FF",
  "Average: 0.64" = "#FFD700",     
  "Average: 0.58"  = "#FF4500"
)

cluster_map <- ggplot(pcon_2010) +
  geom_sf(aes(fill = cluster), colour = "gray", size = 0.05) +
  scale_fill_manual(
    values = colours,
    name = "Cluster of Turnout"
  ) +
  labs(
    title = "Spatial Patterns of Voter Turnout",
    subtitle = "2010-2019",
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
  filename = "cluster_map.jpeg",
  plot = cluster_map,
  path = "/Users/alexander/Documents/GitHub/turnout-uk/figs/clust",
  width = 10,
  height = 12,
  dpi = 600
)


## Moran's I
# Define neighbours
nb <- poly2nb(pcon_2010)

# Step 2: Spatial weights matrix (row-standardised)
lw <- nb2listw(nb, style = "W", zero.policy = TRUE)

# Step 3: Compute Moran's I for 2019 turnout
moran_2019 <- moran.test(pcon_2010$Y2019, lw, zero.policy = TRUE)
moran_2017 <- moran.test(pcon_2010$Y2017, lw, zero.policy = TRUE)
moran_2015 <- moran.test(pcon_2010$Y2015, lw, zero.policy = TRUE)
moran_2010 <- moran.test(pcon_2010$Y2010, lw, zero.policy = TRUE)

# Create a function to extract relevant info
extract_moran_info <- function(moran_obj, year) {
  data.frame(
    Year = year,
    Moran_I = moran_obj$estimate[["Moran I statistic"]],
    Expected_I = moran_obj$estimate[["Expectation"]],
    Variance = moran_obj$estimate[["Variance"]],
    P_value = moran_obj$p.value
  )
}

# Apply to each result
moran_table <- rbind(
  extract_moran_info(moran_2010, 2010),
  extract_moran_info(moran_2015, 2015),
  extract_moran_info(moran_2017, 2017),
  extract_moran_info(moran_2019, 2019)
)

# View the table
#Year	Moran_I	Expected_I	Variance	P_value
#2010	0.4278191	-0.001550388	0.000675709	1.37E-61
#2015	0.4778102	-0.001550388	0.000675974	3.30E-76
#2017	0.3940251	-0.001550388	0.000675803	1.37E-52
#2019	0.4102217	-0.001550388	0.000676055	8.68E-57


#### Visualise LISA
# Define election years
years <- c(2010, 2015, 2017, 2019)

# Loop through each year
for (year in years) {
  
  # Extract turnout column name dynamically
  turnout_col <- paste0("Y", year)
  
  # Run local Moran's I
  local <- localmoran(pcon_2010[[turnout_col]], lw, zero.policy = TRUE)
  
  # Add results to spatial object
  pcon_2010[[paste0("local_I_", year)]] <- local[, "Ii"]
  pcon_2010[[paste0("local_p_", year)]] <- local[, "Pr(z != E(Ii))"]
  
  # Calculate spatial lag
  lag_turnout <- lag.listw(lw, pcon_2010[[turnout_col]])
  pcon_2010[[paste0("lag_Y", year)]] <- lag_turnout
  
  # Get mean turnout for that year
  mean_turnout <- mean(pcon_2010[[turnout_col]], na.rm = TRUE)
  
  # Create cluster classification
  cluster_type <- case_when(
    pcon_2010[[turnout_col]] > mean_turnout &
      lag_turnout > mean_turnout &
      local[, "Pr(z != E(Ii))"] < 0.05 ~ "High-High",
    
    pcon_2010[[turnout_col]] < mean_turnout &
      lag_turnout < mean_turnout &
      local[, "Pr(z != E(Ii))"] < 0.05 ~ "Low-Low",
    
    pcon_2010[[turnout_col]] > mean_turnout &
      lag_turnout < mean_turnout &
      local[, "Pr(z != E(Ii))"] < 0.05 ~ "High-Low",
    
    pcon_2010[[turnout_col]] < mean_turnout &
      lag_turnout > mean_turnout &
      local[, "Pr(z != E(Ii))"] < 0.05 ~ "Low-High",
    
    TRUE ~ "Not significant"
  )
  
  # Assign cluster type
  cluster_col <- paste0("cluster_type_", year)
  pcon_2010[[cluster_col]] <- cluster_type
  
  # Create and save the map
  plot <- ggplot(pcon_2010) +
    geom_sf(aes(fill = .data[[cluster_col]]), color = "gray") +
    scale_fill_manual(values = c(
      "High-High" = "#00008B",
      "Low-Low" = "#FF4500",
      "High-Low" = "#ADD8E6",
      "Low-High" = "#FFD700",
      "Not significant" = "grey80"
    )) +
    labs(
      title = paste0("Local Moran’s I (LISA) Clusters – Turnout ", year),
      fill = "Cluster Type",
      caption = "Source: UK Parliament"
    ) +
    theme_minimal() +
    theme(
      legend.position = "right",
      panel.grid = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank()
    )
  
  # Save the plot
  ggsave(
    filename = paste0("lisa_", year, ".jpeg"),
    plot = plot,
    path = "//Users/alexander/Documents/GitHub/turnout-uk/figs/lisa",
    width = 10,
    height = 12,
    dpi = 600
  )
  
  message("Saved LISA map for ", year)
}


#### Visualise TDA
# Only selected variables:
tda <- tda %>% select(1,3,4)

# Merge
pcon_2010 <- pcon_2010 %>% # Merge with shapefile
  left_join(tda, by = c("PCON20CD" = "constituency_geographic_code")) 

# Define colours for low_any_score
colours <- c(
  "0" = "gray",
  "1" = "#FFD700",   # Gold
  "2" = "#F25C1B",   # OrangeRed
  "3" = "#8F1515"
)

# Make sure low_any_score is a factor for legend control
pcon_2010$low_any_score <- as.factor(pcon_2010$low_any_score)

# Plot the map
low_any_map <- ggplot(pcon_2010) +
  geom_sf(aes(fill = low_any_score), colour = "gray80", size = 0.05) +
  scale_fill_manual(
    values = colours,
    name = "Number of elections:"
  ) +
  labs(
    title = "Number of elections",
    subtitle = "Number of elections in which constituency was in at least one low-performing ball",
    caption = "Source: UK Parliament"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank()
  )

# Save the plot
ggsave(
  filename = "low_any_score_map.jpeg",
  plot = low_any_map,
  path = "/Users/alexander/Documents/GitHub/turnout-uk/figs/tda",
  width = 10,
  height = 12,
  dpi = 600
)


## Always low performing balls:
pcon_2010$low_only_score <- as.factor(pcon_2010$low_only_score)

# Plot the map
low_only_map <- ggplot(pcon_2010) +
  geom_sf(aes(fill = low_only_score), colour = "gray80", size = 0.05) +
  scale_fill_manual(
    values = colours,
    name = "Number of elections:"
  ) +
  labs(
    title = "Number of elections",
    subtitle = "Number of elections in which constituency was only in low-performing balls",
    caption = "Source: UK Parliament"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank()
  )

# Save the plot
ggsave(
  filename = "low_only_score_map.jpeg",
  plot = low_only_map,
  path = "/Users/alexander/Documents/GitHub/turnout-uk/figs/tda",
  width = 10,
  height = 12,
  dpi = 600
)


### Residuals structure
#### Visualise OLS 
# Only selected variables:
# 2010
ols_residuals_2010 <- ols_residuals %>%
  filter(election_year == 2010) %>%
  select(1, 21) %>%
  rename(
    constituency_geographic_code = 1,
    ols_residuals_10 = 2
  )

# 2015
ols_residuals_2015 <- ols_residuals %>%
  filter(election_year == 2015) %>%
  select(1, 21) %>%
  rename(
    constituency_geographic_code = 1,
    ols_residuals_15 = 2
  )

# 2017
ols_residuals_2017 <- ols_residuals %>%
  filter(election_year == 2017) %>%
  select(1, 21) %>%
  rename(
    constituency_geographic_code = 1,
    ols_residuals_17 = 2
  )

# 2019
ols_residuals_2019 <- ols_residuals %>%
  filter(election_year == 2019) %>%
  select(1, 21) %>%
  rename(
    constituency_geographic_code = 1,
    ols_residuals_19 = 2
  )

# Merge all four datasets by constituency code
ols_residuals_all <- ols_residuals_2010 %>%
  full_join(ols_residuals_2015, by = "constituency_geographic_code") %>%
  full_join(ols_residuals_2017, by = "constituency_geographic_code") %>%
  full_join(ols_residuals_2019, by = "constituency_geographic_code")


# Merge with shapefile
pcon_2010 <- pcon_2010 %>% 
  left_join(ols_residuals_all, by = c("PCON20CD" = "constituency_geographic_code")) 

# Define colours for residuals (0 = normal, 1 = high residual / low performance)
residual_colours <- c(
  "0" = "gray",
  "1" = "#F25C1B"  # OrangeRed for low-performing
)

# Residual columns and corresponding years
residual_vars <- c("ols_residuals_10", "ols_residuals_15", "ols_residuals_17", "ols_residuals_19")
residual_years <- c(2010, 2015, 2017, 2019)

# Loop through each residual column
for (i in seq_along(residual_vars)) {
  var <- residual_vars[i]
  year <- residual_years[i]
  
  # Ensure the variable is a factor
  pcon_2010[[var]] <- as.factor(pcon_2010[[var]])
  
  # Plot
  residual_map <- ggplot(pcon_2010) +
    geom_sf(aes(fill = .data[[var]]), colour = "gray80", size = 0.05) +
    scale_fill_manual(
      values = residual_colours,
      name = "Low-performing\nBall"
    ) +
    labs(
      title = paste("Low-performing Balls Based on OLS Residuals -", year),
      subtitle = "Constituencies with above-threshold residuals in Ball Mapper analysis",
      caption = "Source: UK Parliament"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      panel.grid = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank()
    )
  
  # Save the map
  ggsave(
    filename = paste0("ols_residuals_map_", year, ".jpeg"),
    plot = residual_map,
    path = "/Users/alexander/Documents/GitHub/turnout-uk/figs/residuals/ols",
    width = 10,
    height = 12,
    dpi = 600
  )
}


#### Visualise SEM for 2015
sem15_residuals <- sem15_residuals %>% select(1,22)

# Merge with shapefile
pcon_2010 <- pcon_2010 %>% 
  left_join(sem15_residuals, by = c("PCON20CD" = "constituency_geographic_code")) 

# Make sure low_any_score is a factor for legend control
pcon_2010$low_ball_flag_sem <- as.factor(pcon_2010$low_ball_flag_sem)

# Plot the map
low_any_map <- ggplot(pcon_2010) +
  geom_sf(aes(fill = low_ball_flag_sem), colour = "gray80", size = 0.05) +
  scale_fill_manual(
    values = residual_colours,
    name = "Low-performing Balls Based on SEM (2015)"
  ) +
  labs(
    title = "Low-performing Balls Based on SEM 2015",
    subtitle = "Constituencies with above-threshold residuals in Ball Mapper analysis",
    caption = "Source: UK Parliament"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank()
  )

# Save the plot
ggsave(
  filename = "low_fit_sem_2015.jpeg",
  plot = low_any_map,
  path = "/Users/alexander/Documents/GitHub/turnout-uk/figs/residuals/sem2015",
  width = 10,
  height = 12,
  dpi = 600
)


#### Visualise SLAG models
# 2010
slag_residuals_2010 <- slag_residuals %>%
  filter(election_year == 2010) %>%
  select(1, 20) %>%
  rename(
    constituency_geographic_code = 1,
    slag_residuals_10 = 2
  )

slag_residuals_2015 <- slag_residuals %>%
  filter(election_year == 2015) %>%
  select(1, 20) %>%
  rename(
    constituency_geographic_code = 1,
    slag_residuals_15 = 2
  )

slag_residuals_2017 <- slag_residuals %>%
  filter(election_year == 2017) %>%
  select(1, 20) %>%
  rename(
    constituency_geographic_code = 1,
    slag_residuals_17 = 2
  )

slag_residuals_2019 <- slag_residuals %>%
  filter(election_year == 2019) %>%
  select(1, 20) %>%
  rename(
    constituency_geographic_code = 1,
    slag_residuals_19 = 2
  )

# Merge all
slag_residuals_all_years <- slag_residuals_2010 %>%
  full_join(slag_residuals_2015, by = "constituency_geographic_code") %>%
  full_join(slag_residuals_2017, by = "constituency_geographic_code") %>%
  full_join(slag_residuals_2019, by = "constituency_geographic_code")

# Join with spatial data
pcon_2010 <- pcon_2010 %>% 
  left_join(slag_residuals_all_years, by = c("PCON20CD" = "constituency_geographic_code")) 

# Define colours for residuals (0 = normal, 1 = high residual / low performance)
slag_residual_colours <- c(
  "0" = "gray",
  "1" = "#F25C1B"  # OrangeRed for low-performing
)

# Residual columns and corresponding years for SLAG
slag_residual_vars <- c("slag_residuals_10", "slag_residuals_15", "slag_residuals_17", "slag_residuals_19")
slag_residual_years <- c(2010, 2015, 2017, 2019)

# Loop through each SLAG residual column
for (i in seq_along(slag_residual_vars)) {
  var <- slag_residual_vars[i]
  year <- slag_residual_years[i]
  
  # Ensure the variable is a factor
  pcon_2010[[var]] <- as.factor(pcon_2010[[var]])
  
  # Plot
  slag_residual_map <- ggplot(pcon_2010) +
    geom_sf(aes(fill = .data[[var]]), colour = "gray80", size = 0.05) +
    scale_fill_manual(
      values = slag_residual_colours,
      name = "Low-performing\nBall"
    ) +
    labs(
      title = paste("Low-performing Balls Based on SLAG Residuals -", year),
      subtitle = "Constituencies with above-threshold residuals in Ball Mapper analysis",
      caption = "Source: UK Parliament"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      panel.grid = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank()
    )
  
  # Save the map
  ggsave(
    filename = paste0("slag_residuals_map_", year, ".jpeg"),
    plot = slag_residual_map,
    path = "/Users/alexander/Documents/GitHub/turnout-uk/figs/residuals/slag",
    width = 10,
    height = 12,
    dpi = 600
  )
}
 


### Descriptive statistics
# Selected features
predictors <- data %>% select(7:52)

descriptive_table <- predictors %>%
  summarise(across(
    everything(),
    list(
      n = ~ sum(!is.na(.)),
      mean = ~ mean(., na.rm = TRUE),
      sd = ~ sd(., na.rm = TRUE),
      min = ~ min(., na.rm = TRUE),
      q25 = ~ quantile(., 0.25, na.rm = TRUE),
      q50 = ~ quantile(., 0.50, na.rm = TRUE),
      q75 = ~ quantile(., 0.75, na.rm = TRUE),
      max = ~ max(., na.rm = TRUE)
    ),
    .names = "{.col}_{.fn}"
  )) %>%
  pivot_longer(
    cols = everything(),
    names_to = c("variable", ".value"),
    names_pattern = "^(.*)_(.*)$"
  ) %>%
  arrange(variable) %>%
  mutate(across(where(is.numeric), ~ round(., 2)))

# export(descriptive_table, "descr_stats.csv")


## Correlation
# Extract numeric predictors

vars_ordered <- c("electoral_density",
                  "vote_share_margin",
                  "enp_votes",
                  "total_spending",
                  "c11CarsFour",
                  "c11Unemployed",
                  "c11FulltimeStudent",
                  "c11HouseOwned",
                  "ethnic_fractionalization",
                  "religious_fractionalization",
                  "c11QualLevel4",
                  "c11QualNone",
                  "age_65_plus",
                  "male"
)

numeric_data <- data %>%
  select(all_of(vars_ordered))

# Calculate Pearson and Spearman separately
cor_pearson <- cor(numeric_data, use = "pairwise.complete.obs", method = "pearson")
cor_spearman <- cor(numeric_data, use = "pairwise.complete.obs", method = "spearman")

# Create combined matrix: Pearson in lower triangle, Spearman in upper triangle
combined_cor <- cor_pearson
combined_cor[upper.tri(combined_cor)] <- cor_spearman[upper.tri(cor_spearman)]

# Round for easier viewing
combined_cor <- round(combined_cor, 2)
combined_cor <- as.data.frame(combined_cor)

# export(combined_cor, "cor_table.csv")


## Joint distributions and patterns
# Create the customized pair plot
pairs <- ggpairs(
  numeric_data,
  lower = list(continuous = wrap("points", alpha = 0.5, size = 0.7)),
  diag = list(continuous = wrap("densityDiag")),
  upper = list(continuous = wrap("density", alpha = 0.5))
) +
  theme_minimal() +
  theme(panel.grid = element_blank())








