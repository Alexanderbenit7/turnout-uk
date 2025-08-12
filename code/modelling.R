# Libraries
library(rio)
library(dplyr)
library(sandwich)
library(lmtest)
library(modelsummary)
library(pandoc)
library(car)
library(spdep)
library(sf)
library(flextable)
library(spatialreg)
library(broom)
library(officer)
library(ggplot2)
library(purrr)

# Open full dataset
data <- import('/Users/alexander/Documents/MSc Data Science/erp-uom/data/dataTurnoutFinal.csv')
pcon_2010 <- st_read("/Users/alexander/Documents/MSc Data Science/erp-uom/shapefiles/2010/PCON_DEC_2020_UK_BFC.shp")


# Subset of variables
vars_ordered <- c("constituency_geographic_code",
  "election_year",
  "electoralTurnout",
  "vote_share_margin",
  "enp_votes",
  "total_spending",
  "c11Unemployed",
  "c11FulltimeStudent",
  "c11HouseOwned",
  "ethnic_fractionalization",
  "religious_fractionalization",
  "c11QualLevel4",
  "age_65_plus",
  "male"
)

# Subset
modelData <- data %>%
  select(all_of(vars_ordered))
# Year
modelData$election_year <- factor(modelData$election_year)
# Create a named list of data frames split by election year
modelData_by_year <- split(modelData, modelData$election_year)



################ 1. OLS MODELS ########################
# Names of columns to exclude
excluded_vars <- c("constituency_geographic_code", "election_year", "electoralTurnout")

# Build the model formula dynamically
predictor_vars <- setdiff(names(modelData), excluded_vars)
formula <- as.formula(paste("electoralTurnout ~", paste(predictor_vars, collapse = " + ")))

# Fit OLS model for each year
ols_models <- lapply(modelData_by_year, function(df) {
  lm(formula, data = df)
})

# Name the list by year
names(ols_models) <- names(modelData_by_year)

### Results:
summary(ols_models[["2010"]])
summary(ols_models[["2015"]])
summary(ols_models[["2017"]])
summary(ols_models[["2019"]])

# Multicolinearity
vif(ols_models[["2010"]])
vif(ols_models[["2015"]])
vif(ols_models[["2017"]])
vif(ols_models[["2019"]])

# Export:
# Define output path
output_path <- "/Users/alexander/Documents/MSc Data Science/erp-uom/results/ols_models1.docx"
# Export model summaries to Word
# modelsummary(ols_models,
#             stars = TRUE,
#             output = output_path)


# Comparing predictors:
# 1. Standardise predictors in each year’s dataset
modelData_by_year_std <- lapply(modelData_by_year, function(df) {
  df[predictor_vars] <- scale(df[predictor_vars])
  return(df)
})

# 2. Re-fit the models using the standardised data
ols_models_std <- lapply(modelData_by_year_std, function(df) {
  lm(formula, data = df)
})
names(ols_models_std) <- names(modelData_by_year)

# 3. Export if needed
# modelsummary(ols_models_std,
#             stars = TRUE,
#             output = "/Users/alexander/Documents/MSc Data Science/erp-uom/results/ols_models_std.docx")


# Plot
# 1. Tidy model outputs
tidy_models <- map2(ols_models_std, names(ols_models_std), ~ broom::tidy(.x, conf.int = TRUE) %>% 
                      mutate(year = .y))

# 2. Combine all into one data frame
coeffs_df <- bind_rows(tidy_models) %>%
  filter(term != "(Intercept)")

# 4. Plot
coef <- ggplot(coeffs_df, aes(x = estimate, y = term, xmin = conf.low, xmax = conf.high, colour = year)) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_errorbarh(height = 0.2, position = position_dodge(width = 0.5)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  labs(
    title = "Standardised Coefficient Estimates (OLS)",
    x = " ",
    y = NULL,
    colour = "Election Year"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Save it
# ggsave(
#  filename = "beta_coef.jpeg",
#  plot = coef,
#  path = "/Users/alexander/Documents/MSc Data Science/erp-uom/results",
#  width = 10,
#  height = 8,
#  dpi = 600
# )


################## 2. OLS DIAGNOSTICS FOR SPATIAL DEPENDENCY ########################
# Create empty list to store diagnostics
spatial_diagnostics <- list()
# Create neighbour list based on Queen contiguity
nb_queen <- poly2nb(pcon_2010, queen = TRUE)
# Convert to spatial weights list
listw_queen <- nb2listw(nb_queen, style = "W", zero.policy = TRUE)

# 2010
# Merge model data for 2010 into the spatial object
pcon_model_2010 <- merge(pcon_2010, modelData_by_year[["2010"]],
                         by.x = "PCON20CD", by.y = "constituency_geographic_code",
                         all.x = FALSE, sort = FALSE)

resid_2010 <- residuals(ols_models[["2010"]])
moran_2010 <- moran.test(resid_2010, listw_queen, zero.policy = TRUE)
lm_tests_2010 <- lm.LMtests(ols_models[["2010"]], listw = listw_queen,
                            test = "all", zero.policy = TRUE)

# Save
spatial_diagnostics[["2010"]] <- list(
  moran_i = moran_2010,
  lm_tests = lm_tests_2010
)



# 2015
pcon_model_2015 <- merge(pcon_2010, modelData_by_year[["2015"]],
                         by.x = "PCON20CD", by.y = "constituency_geographic_code",
                         all.x = FALSE, sort = FALSE)

resid_2015 <- residuals(ols_models[["2015"]])
moran_2015 <- moran.test(resid_2015, listw_queen, zero.policy = TRUE)
lm_tests_2015 <- lm.LMtests(ols_models[["2015"]], listw = listw_queen,
                            test = "all", zero.policy = TRUE)

# Save
spatial_diagnostics[["2015"]] <- list(
  moran_i = moran_2015,
  lm_tests = lm_tests_2015
)


# 2017
pcon_model_2017 <- merge(pcon_2010, modelData_by_year[["2017"]],
                         by.x = "PCON20CD", by.y = "constituency_geographic_code",
                         all.x = FALSE, sort = FALSE)

resid_2017 <- residuals(ols_models[["2017"]])
moran_2017 <- moran.test(resid_2017, listw_queen, zero.policy = TRUE)
lm_tests_2017 <- lm.LMtests(ols_models[["2017"]], listw = listw_queen,
                            test = "all", zero.policy = TRUE)

# Save
spatial_diagnostics[["2017"]] <- list(
  moran_i = moran_2017,
  lm_tests = lm_tests_2017
)


# 2019
pcon_model_2019 <- merge(pcon_2010, modelData_by_year[["2019"]],
                         by.x = "PCON20CD", by.y = "constituency_geographic_code",
                         all.x = FALSE, sort = FALSE)

resid_2019 <- residuals(ols_models[["2019"]])
moran_2019 <- moran.test(resid_2019, listw_queen, zero.policy = TRUE)
lm_tests_2019 <- lm.LMtests(ols_models[["2019"]], listw = listw_queen,
                            test = "all", zero.policy = TRUE)

# Save
spatial_diagnostics[["2019"]] <- list(
  moran_i = moran_2019,
  lm_tests = lm_tests_2019
)

spatial_diagnostics[["2010"]]
spatial_diagnostics[["2015"]]
spatial_diagnostics[["2017"]]
spatial_diagnostics[["2019"]]



################ 3. SPATIAL ERROR 2015 ########################
# Step 1: Define predictors and formula
excluded_vars <- c("constituency_geographic_code", "election_year", "electoralTurnout")
predictors <- setdiff(names(modelData), excluded_vars)
formula_2015 <- as.formula(paste("electoralTurnout ~", paste(predictors, collapse = " + ")))

# Step 2: Fit Spatial Error Model (SEM) for 2015
sem_2015 <- errorsarlm(
  formula = formula_2015,
  data = modelData_by_year[["2015"]],
  listw = listw_queen,
  method = "eigen",        # eigen is fine for moderate-sized data
  zero.policy = TRUE       # allow regions with no neighbours
)

# STEP 1: Tidy the models
ols_summary <- tidy(ols_models[["2015"]])
sem_summary <- tidy(sem_2015)

# STEP 2: Create a new Word document
doc <- read_docx()
doc <- body_add_par(doc, "Comparison of OLS and Spatial Error Model (2015)", style = "heading 1")

# OLS results
doc <- body_add_par(doc, "OLS Model (2015)", style = "heading 2")
doc <- body_add_flextable(doc, flextable(ols_summary))

# SEM results
doc <- body_add_par(doc, "Spatial Error Model (SEM) (2015)", style = "heading 2")
doc <- body_add_flextable(doc, flextable(sem_summary))

# STEP 3: Save the document
#output_path <- "/Users/alexander/Documents/MSc Data Science/erp-uom/results/ols_vs_sem_2015.docx"
# print(doc, target = output_path)

# Extract residuals from the SEM
resids_sem <- residuals(sem_2015)

# Compute RMSE
rmse_sem <- sqrt(mean(resids_sem^2))
rmse_sem



################ 4. CHARACTERISTICS-BASED DISTANCE MODELS ########################
####  PCA
vars_for_distance <- c(
  "c11Unemployed",
  "c11FulltimeStudent",
  "c11HouseOwned",
  "ethnic_fractionalization",
  "religious_fractionalization",
  "c11QualLevel4"
)

modelData_scaled <- modelData %>%
  select(all_of(vars_for_distance)) %>%
  scale()

# Run PCA on your scaled data
pca <- prcomp(modelData_scaled, center = FALSE, scale. = FALSE)
# Get variance explained
cumulative_var <- summary(pca)$importance["Cumulative Proportion", ]

# Create the elbow plot
plot(
  x = 1:length(cumulative_var),
  y = cumulative_var,
  type = "b", pch = 19, col = "blue",
  xlab = "Principal Component",
  ylab = "Cumulative Variance Explained",
  main = "Elbow Plot (Scree Plot)"
)
abline(h = 0.8, col = "red", lty = 2)  

k <- 3 # We retain almost 90% of variance
data_pca <- pca$x[, 1:k]

# Extract first 3 principal components
pca_scores <- as.data.frame(pca$x[, 1:3])
colnames(pca_scores) <- c("PC1", "PC2", "PC3")

# Bind PCA scores to original modelData
modelData_with_pcs <- bind_cols(modelData, pca_scores)



##### CALCULATE DISTANCE
# Small constant to avoid divide-by-zero
epsilon <- 1e-6

# Store listw objects for all years
listw_characteristics <- list()

# Loop over years
for (yr in names(modelData_by_year)) {
  
  # Step 1: Subset PCA scores for that year
  data_pca <- modelData_with_pcs %>%
    filter(election_year == as.numeric(yr)) %>%
    select(PC1, PC2, PC3)
  
  # Step 2: Compute Euclidean distance matrix (between constituencies in PCA space)
  dist_mat <- as.matrix(dist(data_pca, method = "euclidean"))
  
  # Step 3: Invert distances (closer = higher weight)
  W_inv <- 1 / (dist_mat + epsilon)
  diag(W_inv) <- 0  # no self-weighting
  
  # Step 4: Row-normalise so weights sum to 1 per constituency
  W_norm <- W_inv / rowSums(W_inv)
  
  # Step 5: Convert to 'listw' object for spatial models
  listw_characteristics[[yr]] <- mat2listw(W_norm, style = "W")
}


#### Diagnostics
# Create a list to store diagnostics with characteristics-based weights
spatial_diagnostics_structural <- list()

for (yr in names(modelData_by_year)) {
  
  # Get residuals from OLS with all predictors
  resids <- residuals(ols_models[[yr]])
  
  # Moran's I using characteristics-based weights
  moran_i <- moran.test(resids, listw_characteristics[[yr]], zero.policy = TRUE)
  
  # Lagrange Multiplier tests
  lm_tests <- lm.LMtests(ols_models[[yr]], listw = listw_characteristics[[yr]],
                         test = "all", zero.policy = TRUE)
  
  # Store results
  spatial_diagnostics_structural[[yr]] <- list(
    moran_i = moran_i,
    lm_tests = lm_tests
  )
}

# For 2010
spatial_diagnostics_structural[["2010"]]$moran_i
spatial_diagnostics_structural[["2010"]]$lm_tests

# For 2015
spatial_diagnostics_structural[["2015"]]$moran_i
spatial_diagnostics_structural[["2015"]]$lm_tests

# For 2017
spatial_diagnostics_structural[["2017"]]$moran_i
spatial_diagnostics_structural[["2017"]]$lm_tests

# For 2019
spatial_diagnostics_structural[["2019"]]$moran_i
spatial_diagnostics_structural[["2019"]]$lm_tests





################ 5. CHARACTERISTICS-BASED DISTANCE NEW MODELS ########################
# Define your new set of predictors
political_vars <- c("vote_share_margin", "enp_votes", "total_spending","age_65_plus","male")

# Build formula
formula_political <- as.formula(paste("electoralTurnout ~", paste(political_vars, collapse = " + ")))

# Fit models for each year and store them
ols_models_political <- lapply(modelData_by_year, function(df) {
  lm(formula_political, data = df)
})

summary(ols_models_political[["2010"]])
summary(ols_models_political[["2015"]])
summary(ols_models_political[["2017"]])
summary(ols_models_political[["2019"]])

# Give names to models for the table
names(ols_models_political) <- c("2010", "2015", "2017", "2019")

# Set output path
output_path <- "/Users/alexander/Documents/MSc Data Science/erp-uom/results/ols_political_only.docx"

# Export all models to a Word doc
# modelsummary(
#  ols_models_political,
#  stars = TRUE,
#  output = output_path
# )


## Diagnostics
# Create an empty list to store results
spatial_diagnostics_structural_political <- list()

# Loop over each year
for (yr in names(ols_models_political)) {
  resids <- residuals(ols_models_political[[yr]])
  
  moran_i <- moran.test(resids, listw_characteristics[[yr]], zero.policy = TRUE)
  
  lm_tests <- lm.LMtests(ols_models_political[[yr]], 
                         listw = listw_characteristics[[yr]], 
                         test = "all", 
                         zero.policy = TRUE)
  
  # Store results
  spatial_diagnostics_structural_political[[yr]] <- list(
    moran_i = moran_i,
    lm_tests = lm_tests
  )
}


# For 2010
spatial_diagnostics_structural_political[["2010"]]$moran_i
spatial_diagnostics_structural_political[["2010"]]$lm_tests

# For 2015
spatial_diagnostics_structural_political[["2015"]]$moran_i
spatial_diagnostics_structural_political[["2015"]]$lm_tests

# For 2017
spatial_diagnostics_structural_political[["2017"]]$moran_i
spatial_diagnostics_structural_political[["2017"]]$lm_tests

# For 2019
spatial_diagnostics_structural_political[["2019"]]$moran_i
spatial_diagnostics_structural_political[["2019"]]$lm_tests






### SPATIAL MODELS WITH FULL SUBSET
# Store the spatial lag models
slag_models <- list()

for (yr in names(modelData_by_year)) {
  
  # Subset data for this year
  df <- modelData_by_year[[yr]]
  
  # Estimate the spatial lag model
  slag_models[[yr]] <- lagsarlm(
    formula = formula,               # your main formula
    data = df,
    listw = listw_characteristics[[yr]],
    method = "eigen",                # or "Matrix" if your dataset is large
    zero.policy = TRUE
  )
}


summary(slag_models[["2010"]])
summary(slag_models[["2015"]])
summary(slag_models[["2017"]])
summary(slag_models[["2019"]])




### SPATIAL ERROR MODELS FOR SHORT MODEL
# Store results
serror_models <- list()

# Loop over years
for (yr in names(modelData_by_year)) {
  df <- modelData_by_year[[yr]]
  
  serror_models[[yr]] <- errorsarlm(
    formula = formula_political,
    data = df,
    listw = listw_characteristics[[yr]],
    method = "eigen",          # Use eigenvalue decomposition
    zero.policy = TRUE         # Safe for isolated units
  )
}

summary(serror_models[["2010"]])
summary(serror_models[["2015"]])
summary(serror_models[["2017"]])
summary(serror_models[["2019"]])




################### EXTRACT RESIDUALS ######################
# Add residuals from OLS models to modelData
for (yr in names(ols_models)) {
  resids <- resid(ols_models[[yr]])
  modelData_by_year[[yr]]$ols_residuals <- resids
}

# Combine all years back into a single dataset
modelData <- do.call(rbind, modelData_by_year)



# Spatial Lag for 2015 geographic distance
# Create a new residuals column with NA
modelData$sem_2015_residuals <- NA
# Extract residuals from the SEM model
sem_resids <- residuals(sem_2015)
# Insert residuals only in the 2015 rows
modelData$sem_2015_residuals[modelData$election_year == 2015] <- sem_resids



# Spatial Lag for full model
# Create a column filled with NA
modelData$slag_struct_residuals <- NA
# Loop over each year and insert the residuals where year matches
for (yr in c("2010", "2015", "2017", "2019")) {
  resids <- residuals(slag_models[[yr]])
  modelData$slag_struct_residuals[modelData$election_year == as.integer(yr)] <- resids
}


# Spatial Error for short model
# Create a column for residuals from spatial error models (political vars only)
modelData$serror_struct_residuals <- NA
# Loop over the years and assign residuals
for (yr in c("2010", "2015", "2017", "2019")) {
  resids <- residuals(serror_models[[yr]])
  modelData$serror_struct_residuals[modelData$election_year == as.integer(yr)] <- resids
}
