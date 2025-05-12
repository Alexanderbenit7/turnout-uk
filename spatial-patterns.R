# Libraries
library(rio)
library(dplyr)
library(corrplot)
library(GGally)
library(ggplot2)
library(sf)
library(spdep)
library(tidyr)

turnout <- import("/Users/alexander/Documents/MSc Data Science/erp-uom/data/ukTurnoutFULL.csv")
pcon_2024 <- st_read("/Users/alexander/Documents/MSc Data Science/erp-uom/shapefiles/2024/PCON_JULY_2024_UK_BFC.shp") # Open 2024 shp


# From long to wide format
df_wide <- turnout %>%
  pivot_wider(
    id_cols = c(gss_code, name),
    names_from = year,
    values_from = turnout_2024_constituency
  ) %>%
  rename_with(
    .cols = where(is.numeric),
    .fn = ~ paste0("Y", .x)
  )



### PATTERNS IN SPACE

turnout_years <- df_wide %>%
  select(starts_with("Y")) %>% select(1,4,3,2,5)

plot_matrix <- ggpairs(
  turnout_years,
  lower = list(continuous = wrap("points", alpha = 0.3, size = 0.8)),
  upper = list(continuous = wrap("cor", size = 3)),
  diag = list(continuous = wrap("densityDiag", alpha = 0.5)),
  title = "Turnout Scatterplot Matrix with Pearson Correlations"
)

ggsave("turnout_scatterplot_matrix.jpg",
       plot = plot_matrix,
       path = "/Users/alexander/Documents/MSc Data Science/erp-uom/figs",
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
       path = "/Users/alexander/Documents/MSc Data Science/erp-uom/figs",
       width = 10, height = 10, dpi = 600)



## Cluster Map
df_clustered %>%
  group_by(cluster) %>%
  summarise(across(starts_with("Y"), mean, na.rm = TRUE))

df_clustered <- df_clustered %>%
  mutate(cluster = recode(cluster,
                          `1` = 1,
                          `2` = 2,
                          `3` = 4,
                          `4` = 3))

df_clustered$cluster <- factor(df_clustered$cluster,
                               levels = 1:4,
                               labels = c("Low", "Medium - Low", "Medium - High", "High"),
                               ordered = TRUE)


pcon_2024_merged <- pcon_2024 %>% # Merge with shapefile
  left_join(df_clustered, by = c("PCON24CD" = "gss_code")) 

colours <- c(
  "High" = "#1A9850",
  "Medium - High" = "#91CF60",
  "Medium - Low" = "#FC8D59",     
  "Low"  = "#D73027"
)

cluster_map <- ggplot(pcon_2024_merged) +
  geom_sf(aes(fill = cluster), colour = "gray", size = 0.05) +
  scale_fill_manual(
    values = colours,
    name = "Cluster of Turnout"
  ) +
  labs(
    title = "Spatial Patterns of Voter Turnout",
    subtitle = "2010-2024",
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
  path = "/Users/alexander/Documents/MSc Data Science/erp-uom/figs",
  width = 10,
  height = 12,
  dpi = 300
)


## Moran's I
# Define neighbours
nb <- poly2nb(pcon_2024_merged)

# Step 2: Spatial weights matrix (row-standardised)
lw <- nb2listw(nb, style = "W", zero.policy = TRUE)

# Step 3: Compute Moran's I for 2019 turnout
moran_2024 <- moran.test(pcon_2024_merged$Y2024, lw, zero.policy = TRUE)
moran_2019 <- moran.test(pcon_2024_merged$Y2019, lw, zero.policy = TRUE)
moran_2017 <- moran.test(pcon_2024_merged$Y2017, lw, zero.policy = TRUE)
moran_2015 <- moran.test(pcon_2024_merged$Y2015, lw, zero.policy = TRUE)
moran_2010 <- moran.test(pcon_2024_merged$Y2010, lw, zero.policy = TRUE)

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
  extract_moran_info(moran_2019, 2019),
  extract_moran_info(moran_2024, 2024)
)

# View the table
#Year   Moran_I   Expected_I     Variance      P_value
#1 2010 0.4752404 -0.001547988 0.0006751608 1.668676e-75
#2 2015 0.5175109 -0.001547988 0.0006753746 4.725577e-89
#3 2017 0.4411630 -0.001547988 0.0006752881 2.208319e-65
#4 2019 0.4555727 -0.001547988 0.0006754816 1.513844e-69
#5 2024 0.4594251 -0.001547988 0.0006755341 1.108692e-70


#### Visualise LISA
# Define election years
years <- c(2010, 2015, 2017, 2019, 2024)

# Loop through each year
for (year in years) {
  
  # Extract turnout column name dynamically
  turnout_col <- paste0("Y", year)
  
  # Run local Moran's I
  local <- localmoran(pcon_2024_merged[[turnout_col]], lw, zero.policy = TRUE)
  
  # Add results to spatial object
  pcon_2024_merged[[paste0("local_I_", year)]] <- local[, "Ii"]
  pcon_2024_merged[[paste0("local_p_", year)]] <- local[, "Pr(z != E(Ii))"]
  
  # Calculate spatial lag
  lag_turnout <- lag.listw(lw, pcon_2024_merged[[turnout_col]])
  pcon_2024_merged[[paste0("lag_Y", year)]] <- lag_turnout
  
  # Get mean turnout for that year
  mean_turnout <- mean(pcon_2024_merged[[turnout_col]], na.rm = TRUE)
  
  # Create cluster classification
  cluster_type <- case_when(
    pcon_2024_merged[[turnout_col]] > mean_turnout &
      lag_turnout > mean_turnout &
      local[, "Pr(z != E(Ii))"] < 0.05 ~ "High-High",
    
    pcon_2024_merged[[turnout_col]] < mean_turnout &
      lag_turnout < mean_turnout &
      local[, "Pr(z != E(Ii))"] < 0.05 ~ "Low-Low",
    
    pcon_2024_merged[[turnout_col]] > mean_turnout &
      lag_turnout < mean_turnout &
      local[, "Pr(z != E(Ii))"] < 0.05 ~ "High-Low",
    
    pcon_2024_merged[[turnout_col]] < mean_turnout &
      lag_turnout > mean_turnout &
      local[, "Pr(z != E(Ii))"] < 0.05 ~ "Low-High",
    
    TRUE ~ "Not significant"
  )
  
  # Assign cluster type
  cluster_col <- paste0("cluster_type_", year)
  pcon_2024_merged[[cluster_col]] <- cluster_type
  
  # Create and save the map
  plot <- ggplot(pcon_2024_merged) +
    geom_sf(aes(fill = .data[[cluster_col]]), color = "gray") +
    scale_fill_manual(values = c(
      "High-High" = "#e41a1c",
      "Low-Low" = "#377eb8",
      "High-Low" = "#984ea3",
      "Low-High" = "#4daf4a",
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
    path = "//Users/alexander/Documents/MSc Data Science/erp-uom/figs",
    width = 10,
    height = 12,
    dpi = 300
  )
  
  message("Saved LISA map for ", year)
}



# TURNOUT TRAJECTORIES
turnout_stats <- turnout %>%
  group_by(gss_code, name) %>%
  summarise(
    sd_turnout = sd(turnout_2024_constituency),
    range_turnout = max(turnout_2024_constituency) - min(turnout_2024_constituency)
  ) %>%
  arrange(desc(sd_turnout))  %>% # or arrange(range_turnout)
  select(1,3,4)

pcon_2024_merged <- pcon_2024_merged %>% # Merge with shapefile
  left_join(turnout_stats, by = c("PCON24CD" = "gss_code")) 


# Distribution
mean_sd <- mean(turnout_stats$sd_turnout, na.rm = TRUE)
sd_sd <- sd(turnout_stats$sd_turnout, na.rm = TRUE)

# Plot with vertical lines
st_dist <- ggplot(turnout_stats, aes(x = sd_turnout)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  geom_vline(xintercept = mean_sd, linetype = "solid", color = "black", size = 1) +
  geom_vline(xintercept = mean_sd + sd_sd, linetype = "dashed", color = "red") +
  geom_vline(xintercept = mean_sd - sd_sd, linetype = "dashed", color = "red") +
  geom_vline(xintercept = mean_sd + 2*sd_sd, linetype = "dotted", color = "darkred") +
  geom_vline(xintercept = mean_sd - 2*sd_sd, linetype = "dotted", color = "darkred") +
  labs(
    title = "Distribution of Voter Turnout Variability (Within constituency)",
    x = "Standard Deviation of Turnout (%)",
    y = "Number of Constituencies"
  ) +
  theme_minimal()

# Save it
ggsave(
  filename = "st_dist.jpeg",
  plot = st_dist,
  path = "//Users/alexander/Documents/MSc Data Science/erp-uom/figs",
  width = 10,
  height = 6,
  dpi = 500
)

turnout_stats$quant_std <- ifelse(turnout_stats$sd_turnout<=0.027646,1,
                                  ifelse(turnout_stats$sd_turnout>0.027646 & turnout_stats$sd_turnout<=0.036396,2,
                                         ifelse(turnout_stats$sd_turnout>0.036396 & turnout_stats$sd_turnout<=0.047481,3,
                                                ifelse(turnout_stats$sd_turnout>0.047481,4,0))))

turnout_stats$quant_std <- factor(turnout_stats$quant_std,
                               levels = 1:4,
                               labels = c("3% or less", "Between 3% and 4%", "Between 4% and 5%", "5% or more"),
                               ordered = TRUE)


# Map
pcon_2024_merged <- pcon_2024 %>% # Merge with shapefile
  left_join(turnout_stats, by = c("PCON24CD" = "gss_code")) 

colours <- c(
  "3% or less" = "#1A9850",
  "Between 3% and 4%" = "#91CF60",
  "Between 4% and 5%" = "#FC8D59",     
  "5% or more"  = "#D73027"
)

std_map <- ggplot(pcon_2024_merged) +
  geom_sf(aes(fill = quant_std), colour = "gray", size = 0.05) +
  scale_fill_manual(
    values = colours,
    name = "% of variation"
  ) +
  labs(
    title = "Spatial Patterns of Within-Constituency Turnout Variation",
    subtitle = "2010-2024",
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
  filename = "std_map.jpeg",
  plot = std_map,
  path = "/Users/alexander/Documents/MSc Data Science/erp-uom/figs",
  width = 10,
  height = 12,
  dpi = 300
)
