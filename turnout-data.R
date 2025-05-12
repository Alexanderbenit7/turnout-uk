
## Libraries
library(rio)
library(dplyr)

# Load data
uk24 <- import("/Users/alexander/Documents/MSc Data Science/erp-uom/data/only-turnout/turnout-general-election-04-07-2024.csv")
uk19 <- import("/Users/alexander/Documents/MSc Data Science/erp-uom/data/only-turnout/turnout-general-election-12-12-2019.csv")
uk17 <- import("/Users/alexander/Documents/MSc Data Science/erp-uom/data/only-turnout/turnout-general-election-08-06-2017.csv")
uk15 <- import("/Users/alexander/Documents/MSc Data Science/erp-uom/data/only-turnout/turnout-general-election-07-05-2015.csv")
uk10 <- import("/Users/alexander/Documents/MSc Data Science/erp-uom/data/only-turnout/turnout-general-election-06-05-2010.csv")
overlap <- import("/Users/alexander/Documents/MSc Data Science/erp-uom/data/overlap/PARL25_PARL10_combo_overlap.csv") %>%
  rename(PARL25 = PARL25, PARL10 = PARL10)
const <- import("/Users/alexander/Documents/MSc Data Science/erp-uom/data/overlap/parl_constituencies_2025.csv")

# Select variables of interest
uk24 <- uk24[, c(2, 3, 16, 17, 18)] |> mutate(year = 2024)
uk19 <- uk19[, c(2, 3, 16, 17, 18)] |> mutate(year = 2019)
uk17 <- uk17[, c(2, 3, 16, 17, 18)] |> mutate(year = 2017)
uk15 <- uk15[, c(2, 3, 16, 17, 18)] |> mutate(year = 2015)
uk10 <- uk10[, c(2, 3, 16, 17, 18)] |> mutate(year = 2010)

# Combine all into one
uk_all <- bind_rows(uk24, uk19, uk17, uk15, uk10)



### SPATIAL OVERLAY
uk_past <- uk_all %>%
  filter(year %in% c(2010, 2015, 2017, 2019)) %>%
  rename(PARL10 = `Constituency geographic code`)

merged <- uk_past %>%
  inner_join(overlap, by = "PARL10") %>%
  mutate(weighted_turnout = Turnout * percentage_overlap_pop)

turnout_PARL25 <- merged %>%
  group_by(year, PARL25) %>%
  summarise(turnout_2024_constituency = sum(weighted_turnout, na.rm = TRUE)) %>%
  ungroup()

# Including full names
const <- const %>% select(2,3,5)

# Merge
spatial_overlay <- merge(turnout_PARL25, const, by.x = "PARL25", by.y = "short_code", all.x = TRUE)

spatial_overlay <- spatial_overlay %>% select(5,4,2,3)
uk24 <- uk24 %>% select(2,1,6,5)
colnames(uk24) <- colnames(spatial_overlay)

# Final data: 
uk_turnoutFULL <- rbind(spatial_overlay, uk24)


