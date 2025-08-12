library(rio)
library(sf)
library(dplyr)
library(purrr)
library(tidyr)
library(stringr)

# Electoral data
results2010 <- import('/Users/alexander/Documents/MSc Data Science/erp-uom/data/raw/political_data/electoral data/candidate-level-results-general-election-06-05-2010.csv')
results2015 <- import('/Users/alexander/Documents/MSc Data Science/erp-uom/data/raw/political_data/electoral data/candidate-level-results-general-election-07-05-2015.csv')
results2017 <- import('/Users/alexander/Documents/MSc Data Science/erp-uom/data/raw/political_data/electoral data/candidate-level-results-general-election-08-06-2017.csv')
results2019 <- import('/Users/alexander/Documents/MSc Data Science/erp-uom/data/raw/political_data/electoral data/candidate-level-results-general-election-12-12-2019.csv')
results2024 <- import('/Users/alexander/Documents/MSc Data Science/erp-uom/data/raw/political_data/electoral data/candidate-level-results-general-election-04-07-2024.csv')

# Party spending
spending10 <- import('/Users/alexander/Documents/MSc Data Science/erp-uom/data/raw/political_data/electoral data/2010-UK-Parliament-spending-data-Excel.xls', sheet = 3)
spending15 <- import('/Users/alexander/Documents/MSc Data Science/erp-uom/data/raw/political_data/electoral data/2015-UK-Parliament-spending-data.xlsx')
spending17_raw <- import('/Users/alexander/Documents/MSc Data Science/erp-uom/data/raw/political_data/electoral data/2017-UK-Parliament-candidate-spending-publication-version.xlsx')
colnames(spending17_raw) <- as.character(unlist(spending17_raw[1, ]))
spending17 <- spending17_raw[-1, ]  # remove first row
spending19 <- import('/Users/alexander/Documents/MSc Data Science/erp-uom/data/raw/political_data/electoral data/2019 UKPGE candidate spending spreadsheet.xlsx')
spending24 <- import('/Users/alexander/Documents/MSc Data Science/erp-uom/data/raw/political_data/electoral data/2024 UKPGE candidate spending spreadsheet.xlsx', sheet = 2)

# Census 2011
pipa <- import('/Users/alexander/Documents/MSc Data Science/erp-uom/data/raw/census/UK GE 2010_2019 V1.9 (inc Brexit EU Ref vote and 2011 Census).dta')
sex11ni <- import('/Users/alexander/Documents/MSc Data Science/erp-uom/data/raw/census/census-2011-ks101ni.xlsx', sheet = 11)
age11ni <- import('/Users/alexander/Documents/MSc Data Science/erp-uom/data/raw/census/census-2011-usually-resident-population-by-single-year-of-age-table.xlsx', sheet = 7)
car11ni <- import('/Users/alexander/Documents/MSc Data Science/erp-uom/data/raw/census/census-2011-ks405ni.xlsx', sheet = 11)
employment11ni <- import('/Users/alexander/Documents/MSc Data Science/erp-uom/data/raw/census/census-2011-ks601ni.xlsx', sheet = 11)
tenure11ni <- import('/Users/alexander/Documents/MSc Data Science/erp-uom/data/raw/census/census-2011-ks402ni.xlsx', sheet = 11)
edu11ni <- import('/Users/alexander/Documents/MSc Data Science/erp-uom/data/raw/census/census-2011-ks501ni.xlsx', sheet = 11)
ethni11ni <- import('/Users/alexander/Documents/MSc Data Science/erp-uom/data/raw/census/census-2011-ks201ni.xlsx', sheet = 9)
reli11ni <- import('/Users/alexander/Documents/MSc Data Science/erp-uom/data/raw/census/census-2011-ks211ni.xlsx', sheet = 9)

# Census 2021
field <- import('/Users/alexander/Documents/MSc Data Science/erp-uom/data/raw/census/BES-2024-General-Election-results-file-v1.0.dta')
hcni <- import('/Users/alexander/Documents/MSc Data Science/erp-uom/data/raw/census/Demographic-data-for-new-parliamentary-constituencies-May-2024.xlsx', sheet = 5)


# Shapefiles
pcon_2024 <- st_read('/Users/alexander/Documents/MSc Data Science/erp-uom/shapefiles/2024/PCON_JULY_2024_UK_BFC.shp')
pcon_2010 <- st_read('/Users/alexander/Documents/MSc Data Science/erp-uom/shapefiles/2010/PCON_DEC_2020_UK_BFC.shp')


##### POLITICAL INDICATORS #######
# Function to convert IDate columns to Date
convert_idate_to_date <- function(df) {
  df[] <- lapply(df, function(col) {
    if (inherits(col, "IDate")) as.Date(col) else col
  })
  return(df)
}

# Apply to each dataset
results2010 <- results2010 %>% convert_idate_to_date() %>% mutate(election_year = 2010)
results2015 <- results2015 %>% convert_idate_to_date() %>% mutate(election_year = 2015)
results2017 <- results2017 %>% convert_idate_to_date() %>% mutate(election_year = 2017)
results2019 <- results2019 %>% convert_idate_to_date() %>% mutate(election_year = 2019)
results2024 <- results2024 %>% convert_idate_to_date() %>% mutate(election_year = 2024)

# Now safely combine them. We leave 2024 out because of different configuration
results_all <- bind_rows(results2010, results2015, results2017, results2019, results2024)

# Variables of interest
electoralData <- results_all %>% select(9:11,13,14,19,20,26,30,31,34,35,40,41,42,47:49,51:53) %>% 
  janitor::clean_names() %>% select( # To correct names
    constituency_name,
    constituency_geographic_code,
    country_name,
    country_geographic_code,
    everything()
  ) 


### Electoral Participation
# Only turnout and participation indicators
turnout <- electoralData %>% select(1:10,21) %>% distinct() %>%
  mutate(electoralTurnout = election_valid_vote_count / electorate) %>%
  select(1:4,8,12,11)


### Electoral closeness
comp <- electoralData %>%
  select(1:4, 14, 18, 21) %>%
  mutate(main_party_abbreviation = if_else(main_party_abbreviation == "", "Independent", main_party_abbreviation))

# Calculation:
comp <- comp %>%
  group_by(constituency_geographic_code, election_year) %>%
  arrange(desc(candidate_vote_share), .by_group = TRUE) %>%
  slice_head(n = 2) %>%  # Keep only top 2 candidates per race
  summarise(
    vote_share_lead = candidate_vote_share[1],
    vote_share_runnerup = candidate_vote_share[2],
    vote_share_margin = vote_share_lead - vote_share_runnerup,
    .groups = "drop"
  ) %>% select(1,2,5)

# Back to first object:
turnout <- turnout %>%
  left_join(comp, by = c("constituency_geographic_code", "election_year")) %>%
  select(election_year, everything())


### Electoral Fragmentation
enp_data <- electoralData %>%
  group_by(constituency_geographic_code, election_year) %>%
  summarise(
    enp_votes = 1 / sum((candidate_vote_share)^2, na.rm = TRUE),
    .groups = "drop"
  )

# Back to first object:
turnout <- turnout %>%
  left_join(enp_data, by = c("constituency_geographic_code", "election_year"))


### Electoral Density
# Subset for every election year
t2010 <- turnout %>%
  filter(election_year == 2010) %>%
  select(PCON20CD = constituency_geographic_code, electorate2010 = electorate)

t2015 <- turnout %>%
  filter(election_year == 2015) %>%
  select(PCON20CD = constituency_geographic_code, electorate2015 = electorate)

t2017 <- turnout %>%
  filter(election_year == 2017) %>%
  select(PCON20CD = constituency_geographic_code, electorate2017 = electorate)

t2019 <- turnout %>%
  filter(election_year == 2019) %>%
  select(PCON20CD = constituency_geographic_code, electorate2019 = electorate)

t2024 <- turnout %>%
  filter(election_year == 2024) %>%
  select(PCON24CD = constituency_geographic_code, electorate2024 = electorate)

# Combine all elections with same constituency configuration
electorates_2010s <- reduce(list(t2010, t2015, t2017, t2019), 
                            full_join, by = "PCON20CD")

# Merge with shapefile
pcon_2010 <- pcon_2010 %>%
  left_join(electorates_2010s, by = c("PCON20CD" = "PCON20CD"))

pcon_2024 <- pcon_2024 %>%
  left_join(t2024, by = c("PCON24CD" = "PCON24CD"))

# Calculate area in square kilometres
pcon_2010 <- pcon_2010 %>%
  mutate(
    area_km2 = as.numeric(st_area(geometry)) / 1e6,  # area in km²
    density2010 = electorate2010 / area_km2,
    density2015 = electorate2015 / area_km2,
    density2017 = electorate2017 / area_km2,
    density2019 = electorate2019 / area_km2
  )

pcon_2024 <- pcon_2024 %>%
  mutate(
    area_km2 = as.numeric(st_area(geometry)) / 1e6,
    density2024 = electorate2024 / area_km2
  )

# Extract the values
t2010 <- pcon_2010 %>%
  select(
    constituency_geographic_code = PCON20CD,
    electoral_density = density2010,
    geometry
  ) %>%
  mutate(election_year = 2010)

t2015 <- pcon_2010 %>%
  select(
    constituency_geographic_code = PCON20CD,
    electoral_density = density2015,
    geometry
  ) %>%
  mutate(election_year = 2015)

t2017 <- pcon_2010 %>%
  select(
    constituency_geographic_code = PCON20CD,
    electoral_density = density2017,
    geometry
  ) %>%
  mutate(election_year = 2017)

t2019 <- pcon_2010 %>%
  select(
    constituency_geographic_code = PCON20CD,
    electoral_density = density2019,
    geometry
  ) %>%
  mutate(election_year = 2019)

# For 2024 using pcon_2024
t2024 <- pcon_2024 %>%
  select(
    constituency_geographic_code = PCON24CD,
    electoral_density = density2024,
    geometry
  ) %>%
  mutate(election_year = 2024)

# Bind all values
density_all_years <- bind_rows(t2010, t2015, t2017, t2019, t2024)
density_all_years <- st_drop_geometry(density_all_years)

# Merge
turnout <- turnout %>%
  left_join(density_all_years, by = c("constituency_geographic_code", "election_year"))


### Party performance
# Step 1: Clean empty party names
electoralData_clean <- electoralData %>%
  mutate(
    main_party_abbreviation = if_else(
      main_party_abbreviation == "" | is.na(main_party_abbreviation),
      "Independent",
      main_party_abbreviation
    )
  )

# Step 2: Identify the top 5 parties by total votes
top_parties <- electoralData_clean %>%
  group_by(main_party_abbreviation) %>%
  summarise(total_votes = sum(candidate_vote_count, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(total_votes)) %>%
  slice_head(n = 5) %>%
  pull(main_party_abbreviation)

# Step 3: Relabel non-top parties as "Other"
electoralData_clean <- electoralData_clean %>%
  mutate(
    party_grouped = if_else(main_party_abbreviation %in% top_parties, 
                            main_party_abbreviation, 
                            "Other")
  )

# Step 4: Calculate vote share by party_grouped
party_shares <- electoralData_clean %>%
  select(
    election_year,
    constituency_geographic_code,
    party_grouped,
    candidate_vote_share
  ) %>%
  group_by(election_year, constituency_geographic_code, party_grouped) %>%
  summarise(party_vote_share = sum(candidate_vote_share, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    names_from = party_grouped,
    values_from = party_vote_share,
    values_fill = 0
  )

# Merge
turnout <- turnout %>%
  left_join(party_shares, by = c("constituency_geographic_code", "election_year"))


### Party spending
## 2010
# Ensure column names are cleaned (optional but recommended)
spending10 <- spending10 %>%
  rename_with(~ gsub("\\s+", "_", .))  # Replace spaces with underscores

# Calculate total spending (Long + Short) per candidate
spending10 <- spending10 %>%
  mutate(
    long_spend = as.numeric(Long_Total_Spend),
    short_spend = as.numeric(Short_Total_Spend),
    total_spend = long_spend + short_spend
  ) # NO missing values in 2010

# Sum up total spending per constituency
spending10 <- spending10 %>%
  group_by(ConstituencyId) %>%
  summarise(
    total_spending = sum(total_spend, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(constituency_geographic_code = ConstituencyId) %>%
  mutate(election_year = 2010)

## 2015
spending15 <- spending15 %>%
  rename_with(~ gsub("\\s+", "_", .))

# Calculate total spending (Long + Short) per candidate
spending15 <- spending15 %>%
  mutate(
    long_spend = as.numeric(Long_Total_Spend),
    short_spend = as.numeric(Short_Total_Spend),
    total_spend = long_spend + short_spend
  ) # NO missing values in 2015

# Sum up total spending per constituency
spending15 <- spending15 %>%
  group_by(ConstituencyId) %>%
  summarise(
    total_spending = sum(total_spend, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(constituency_geographic_code = ConstituencyId) %>%
  mutate(election_year = 2015)

## 2017
spending17 <- spending17 %>%
  setNames(make.names(names(.), unique = TRUE)) %>%  # Fix NA or duplicate names
  rename_with(~ gsub("\\.+", "_", .)) 

# Calculate total spending (Long + Short) per candidate
spending17 <- spending17 %>%
  mutate(total_spend = as.numeric(Total_Reported_Spending)) # 57 missing values in 2017

# Sum up total spending per constituency
spending17 <- spending17 %>%
  group_by(Constituency_ID) %>%
  summarise(
    total_spending = sum(total_spend, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(constituency_geographic_code = Constituency_ID) %>%
  mutate(election_year = 2017)


## 2019
spending19 <- spending19 %>%
  setNames(make.names(names(.), unique = TRUE)) %>%  # Fix NA or duplicate names
  rename_with(~ gsub("\\.+", "_", .)) 

# Calculate total spending (Long + Short) per candidate
spending19 <- spending19 %>%
  mutate(total_spend = as.numeric(Total_Reported_Spending)) # 99 missing values in 2019

# Sum up total spending per constituency
spending19 <- spending19 %>%
  group_by(Constituency_ID) %>%
  summarise(
    total_spending = sum(total_spend, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(constituency_geographic_code = Constituency_ID) %>%
  mutate(election_year = 2019)


## 2024
spending24 <- spending24 %>%
  setNames(make.names(names(.), unique = TRUE)) %>%  # Fix NA or duplicate names
  rename_with(~ gsub("\\.+", "_", .)) 

# Calculate total spending (Long + Short) per candidate
spending24 <- spending24 %>%
  mutate(total_spend = as.numeric(Total_reported_spending)) # 176 missing values in 2024

# Sum up total spending per constituency
spending24 <- spending24 %>%
  group_by(Constituency_ID) %>%
  summarise(
    total_spending = sum(total_spend, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(constituency_geographic_code = Constituency_ID) %>%
  mutate(election_year = 2024)

# Bind all:
spending_all_years <- bind_rows(
  spending10,
  spending15,
  spending17,
  spending19,
  spending24
)

# Merge
turnout <- turnout %>%
  left_join(spending_all_years, by = c("constituency_geographic_code", "election_year")) 




##### SOCIODEMOGRAPHIC INDICATORS #######

## For 2011: No data for Northern Ireland
census2011 <- pipa %>% select(2,162:344)

# Sex, age, income, education and population homogeneity
sex11 <- census2011 %>% select(1:3)
age11 <- census2011 %>% select(1,6:21) # Reduce bands
car11 <- census2011 %>% select(1,54:58)
employment11 <- census2011 %>% select(1,128:145)
tenure11 <- census2011 %>% select(1,22:32)
edu11 <- census2011 %>% select(1,164:171)
edu11 <- edu11 %>% select(1:4,6,7)
ethni11 <- census2011 %>% select(1,59:63)
reli11 <- census2011 %>% select(1,109:117)

## Add information for Northern Ireland
colnames(sex11ni) <- as.character(unlist(sex11ni[4, ]))
sex11ni <- sex11ni[-(1:4), ]
rownames(sex11ni) <- NULL
sex11ni <- sex11ni %>%
  select(2, 8, 9) %>%
  rename(
    ons_id = 1,
    c11Male = 2,
    c11Female = 3
  )

# Fill missing c11Male and c11Female in sex11 using values from sex11ni
sex11 <- sex11 %>%
  left_join(sex11ni, by = "ons_id", suffix = c("", "_ni")) %>%
  mutate(
    c11Male = ifelse(is.na(c11Male), c11Male_ni, c11Male),
    c11Female = ifelse(is.na(c11Female), c11Female_ni, c11Female)
  ) %>%
  select(-c11Male_ni, -c11Female_ni)  # Remove temporary cols

# Rename columns
colnames(sex11) <- c("constituency_geographic_code", "male", "female")

# Convert columns to numeric, then round
sex11 <- sex11 %>%
  mutate(
    male = round(as.numeric(male), 2),
    female = round(as.numeric(female), 2)
  )


## Age
age11 <- age11 %>%
  rename(constituency_geographic_code = ons_id) %>%
  mutate(
    age_0_15    = as.numeric(c11Age0to4) + as.numeric(c11Age5to7) + as.numeric(c11Age8to9) +
      as.numeric(c11Age10to14) + as.numeric(c11Age15),
    age_16_24   = as.numeric(c11Age16to17) + as.numeric(c11Age18to19) + as.numeric(c11Age20to24),
    age_25_44   = as.numeric(c11Age25to29) + as.numeric(c11Age30to44),
    age_45_64   = as.numeric(c11Age45to59) + as.numeric(c11Age60to64),
    age_65_plus = as.numeric(c11Age65to74) + as.numeric(c11Age75to84) +
      as.numeric(c11Age85to89) + as.numeric(c11Age90plus)
  ) %>%
  select(constituency_geographic_code, age_0_15, age_16_24, age_25_44, age_45_64, age_65_plus)

# Fix NI age
colnames(age11ni) <- as.character(unlist(age11ni[5, ]))
age11ni <- age11ni[-(1:5), -1]
rownames(age11ni) <- NULL

# Clean column names
colnames(age11ni) <- str_replace_all(colnames(age11ni), "\\s+", "_")
colnames(age11ni)[1] <- "constituency_geographic_code"  # Rename AA Code
colnames(age11ni)[2] <- "total_population"

# Convert age columns to numeric percentages
age_cols <- grep("^Persons:_\\d+|^Persons:_100\\+_years$", colnames(age11ni), value = TRUE)
age11ni[age_cols] <- lapply(age11ni[age_cols], function(x) as.numeric(x) / as.numeric(age11ni$total_population)) 

# Build bands by selecting age columns explicitly
age11ni <- age11ni %>%
  mutate(
    age_0_15 = rowSums(select(., matches("Persons:_([0-9]|1[0-5])_years"))),
    age_16_24 = rowSums(select(., matches("Persons:_(1[6-9]|2[0-4])_years"))),
    age_25_44 = rowSums(select(., matches("Persons:_(2[5-9]|3[0-9]|4[0-4])_years"))),
    age_45_64 = rowSums(select(., matches("Persons:_(4[5-9]|5[0-9]|6[0-4])_years"))),
    age_65_plus = rowSums(select(., matches("Persons:_(6[5-9]|[7-9][0-9]|100\\+)_years")))
  ) %>%
  select(constituency_geographic_code, age_0_15, age_16_24, age_25_44, age_45_64, age_65_plus)

# Same scale
age11ni[, 2:ncol(age11ni)] <- round(age11ni[, 2:ncol(age11ni)] * 100, 2)

# Join to fill only missing values in age11 from age11ni
age11 <- age11 %>%
  left_join(age11ni, by = "constituency_geographic_code", suffix = c("", ".ni")) %>%
  mutate(
    age_0_15 = round(ifelse(is.na(age_0_15), age_0_15.ni, age_0_15), 2),
    age_16_24 = round(ifelse(is.na(age_16_24), age_16_24.ni, age_16_24), 2),
    age_25_44 = round(ifelse(is.na(age_25_44), age_25_44.ni, age_25_44), 2),
    age_45_64 = round(ifelse(is.na(age_45_64), age_45_64.ni, age_45_64), 2),
    age_65_plus = round(ifelse(is.na(age_65_plus), age_65_plus.ni, age_65_plus), 2)
  ) %>%
  select(-ends_with(".ni"))


## Car
car11 <- car11 %>%
  rename(constituency_geographic_code = ons_id)

# Fix NI age
colnames(car11ni) <- as.character(unlist(car11ni[5, ]))
car11ni <- car11ni[-(1:5), -(c(1, 3:9))]
rownames(car11ni) <- NULL
colnames(car11ni) <- colnames(car11)

# Replace
car11 <- car11 %>%
  left_join(car11ni, by = "constituency_geographic_code", suffix = c("", ".ni")) %>%
  mutate(
    c11CarsNone  = round(ifelse(is.na(as.numeric(c11CarsNone)), as.numeric(c11CarsNone.ni), as.numeric(c11CarsNone)), 2),
    c11CarsOne   = round(ifelse(is.na(as.numeric(c11CarsOne)), as.numeric(c11CarsOne.ni), as.numeric(c11CarsOne)), 2),
    c11CarsTwo   = round(ifelse(is.na(as.numeric(c11CarsTwo)), as.numeric(c11CarsTwo.ni), as.numeric(c11CarsTwo)), 2),
    c11CarsThree = round(ifelse(is.na(as.numeric(c11CarsThree)), as.numeric(c11CarsThree.ni), as.numeric(c11CarsThree)), 2),
    c11CarsFour  = round(ifelse(is.na(as.numeric(c11CarsFour)), as.numeric(c11CarsFour.ni), as.numeric(c11CarsFour)), 2)
  ) %>%
  select(-ends_with(".ni"))


## Employment
employment11 <- employment11 %>%
  rename(constituency_geographic_code = ons_id) %>%
  select(
    constituency_geographic_code,
    c11Unemployed,
    c11EmployedFullTime,
    c11Retired,
    c11FulltimeStudent
  )

# Fix NI employment
colnames(employment11ni) <- as.character(unlist(employment11ni[5, ]))
employment11ni <- employment11ni[-(1:5), -(c(1, 3:18))]
rownames(employment11ni) <- NULL

# Just relevant cols
employment11ni <- employment11ni %>%
  select(
    constituency_geographic_code = `AA Code`,
    c11Unemployed = `Economically active: Unemployed: Aged 16-74 years (%)`,
    c11EmployedFullTime = `Economically active: Employee: Full-time: Aged 16-74 years (%)`,
    c11Retired = `Economically inactive: Retired: Aged 16-74 years (%)`,
    c11FulltimeStudent = `Economically active: Full-time student: Aged 16-74 years (%)`
  )

# Convert NI indicators to numeric and round
employment11ni <- employment11ni %>%
  mutate(across(-constituency_geographic_code, ~ round(as.numeric(.), 2)))

# Convert existing employment11 indicators to numeric and round
employment11 <- employment11 %>%
  mutate(across(-constituency_geographic_code, ~ round(as.numeric(.), 2)))

# Merge NI data into existing employment11, filling missing values
employment11 <- employment11 %>%
  left_join(employment11ni, by = "constituency_geographic_code", suffix = c("", ".ni")) %>%
  mutate(
    c11Unemployed = ifelse(is.na(c11Unemployed), c11Unemployed.ni, c11Unemployed),
    c11EmployedFullTime = ifelse(is.na(c11EmployedFullTime), c11EmployedFullTime.ni, c11EmployedFullTime),
    c11Retired = ifelse(is.na(c11Retired), c11Retired.ni, c11Retired),
    c11FulltimeStudent = ifelse(is.na(c11FulltimeStudent), c11FulltimeStudent.ni, c11FulltimeStudent)
  ) %>%
  select(-ends_with(".ni"))


## Tenure
tenure11 <- tenure11 %>%
  select(
    constituency_geographic_code = ons_id,
    c11HouseOwned,
    c11HouseSocial,
    c11HousePrivate
  )

# Fix NI tenure
colnames(tenure11ni) <- as.character(unlist(tenure11ni[5, ]))
tenure11ni <- tenure11ni[-(1:5), -(c(1, 3:11))]
rownames(tenure11ni) <- NULL

# Unifying data
#c11HouseOwned = owns outright + owns with mortgage + shared ownership
#c11HouseSocial = rented from NI Housing Executive + rented from housing associations
#c11HousePrivate = rented from private landlord + rented from other

tenure11ni <- tenure11ni %>%
  mutate(
    c11HouseOwned = as.numeric(`Owner occupied: Owns outright (%)`) +
      as.numeric(`Owner occupied: Owns with a mortgage or loan (%)`) +
      as.numeric(`Shared ownership (%)`),
    c11HouseSocial = as.numeric(`Rented from: Northern Ireland Housing Executive (%)`) +
      as.numeric(`Rented from: Housing association or charitable trust (%)`),
    c11HousePrivate = as.numeric(`Rented from: Private landlord or letting agency (%)`) +
      as.numeric(`Rented from: Other (%)`)
  ) %>%
  select(
    constituency_geographic_code = `AA Code`,
    c11HouseOwned,
    c11HouseSocial,
    c11HousePrivate
  ) %>%
  mutate(across(-constituency_geographic_code, ~ round(., 2)))

# Make sure both datasets' numeric columns are numeric and rounded
tenure11ni <- tenure11ni %>%
  mutate(across(-constituency_geographic_code, ~ round(as.numeric(.), 2)))

tenure11 <- tenure11 %>%
  mutate(across(-constituency_geographic_code, ~ round(as.numeric(.), 2)))

# Merge and fill missing values with NI data
tenure11 <- tenure11 %>%
  left_join(tenure11ni, by = "constituency_geographic_code", suffix = c("", ".ni")) %>%
  mutate(
    c11HouseOwned = round(ifelse(is.na(c11HouseOwned), c11HouseOwned.ni, c11HouseOwned), 2),
    c11HouseSocial = round(ifelse(is.na(c11HouseSocial), c11HouseSocial.ni, c11HouseSocial), 2),
    c11HousePrivate = round(ifelse(is.na(c11HousePrivate), c11HousePrivate.ni, c11HousePrivate), 2)
  ) %>%
  select(-ends_with(".ni"))


## Education
edu11 <- edu11 %>%
  rename(
    constituency_geographic_code = ons_id)

# Fix NI tenure
colnames(edu11ni) <- as.character(unlist(edu11ni[5, ]))
edu11ni <- edu11ni[-(1:5), -(c(1, 3:15))]
rownames(edu11ni) <- NULL

# Relevant variables
edu11ni <- edu11ni %>%
  select(
    constituency_geographic_code = `AA Code`,
    c11QualNone = `No qualifications: Aged 16+ years (%)`,
    c11QualLevel1 = `Highest level of qualification: Level 1 qualifications: Aged 16+ years (%)`,
    c11QualLevel2 = `Highest level of qualification: Level 2 qualifications: Aged 16+ years (%)`,
    c11QualLevel3 = `Highest level of qualification: Level 3 qualifications: Aged 16+ years (%)`,
    c11QualLevel4 = `Highest level of qualification: Level 4 qualifications and above: Aged 16+ years (%)`
  )

# Convert NI indicators to numeric and round
edu11ni <- edu11ni %>%
  mutate(across(-constituency_geographic_code, ~ round(as.numeric(.), 2)))

# Convert existing GB indicators to numeric and round
edu11 <- edu11 %>%
  mutate(across(-constituency_geographic_code, ~ round(as.numeric(.), 2)))

# Merge and fill missing values from NI data
edu11 <- edu11 %>%
  left_join(edu11ni, by = "constituency_geographic_code", suffix = c("", ".ni")) %>%
  mutate(
    c11QualNone = round(ifelse(is.na(c11QualNone), c11QualNone.ni, c11QualNone), 2),
    c11QualLevel1 = round(ifelse(is.na(c11QualLevel1), c11QualLevel1.ni, c11QualLevel1), 2),
    c11QualLevel2 = round(ifelse(is.na(c11QualLevel2), c11QualLevel2.ni, c11QualLevel2), 2),
    c11QualLevel3 = round(ifelse(is.na(c11QualLevel3), c11QualLevel3.ni, c11QualLevel3), 2),
    c11QualLevel4 = round(ifelse(is.na(c11QualLevel4), c11QualLevel4.ni, c11QualLevel4), 2)
  ) %>%
  select(-ends_with(".ni"))


## Ethnicity
ethni11 <- ethni11 %>%
  rename(
    constituency_geographic_code = ons_id)

# Fix NI tenure
colnames(ethni11ni) <- as.character(unlist(ethni11ni[4, ]))
ethni11ni <- ethni11ni[-(1:4), -(c(1, 3:15))]
rownames(ethni11ni) <- NULL

# Sum attributes
ethni11ni <- ethni11ni %>%
  mutate(
    c11EthnicityWhite = as.numeric(`Ethnic group: White (%)`),
    c11EthnicityMixed = as.numeric(`Ethnic group: Mixed (%)`),
    c11EthnicityAsian = rowSums(cbind(
      as.numeric(`Ethnic group: Chinese (%)`),
      as.numeric(`Ethnic group: Indian (%)`),
      as.numeric(`Ethnic group: Pakistani (%)`),
      as.numeric(`Ethnic group: Bangladeshi (%)`),
      as.numeric(`Ethnic group: Other Asian (%)`)
    ), na.rm = TRUE),
    c11EthnicityBlack = rowSums(cbind(
      as.numeric(`Ethnic group: Black Caribbean (%)`),
      as.numeric(`Ethnic group: Black African (%)`),
      as.numeric(`Ethnic group: Black other (%)`)
    ), na.rm = TRUE),
    c11EthnicityOther = as.numeric(`Ethnic group: Other (%)`)
  ) %>%
  select(
    constituency_geographic_code = `AA Code`,
    c11EthnicityWhite,
    c11EthnicityMixed,
    c11EthnicityAsian,
    c11EthnicityBlack,
    c11EthnicityOther
  ) %>%
  mutate(across(-constituency_geographic_code, ~ round(., 2)))

# Make sure both datasets have numeric, rounded columns
ethni11ni <- ethni11ni %>%
  mutate(across(-constituency_geographic_code, ~ round(as.numeric(.), 2)))

ethni11 <- ethni11 %>%
  mutate(across(-constituency_geographic_code, ~ round(as.numeric(.), 2)))

# Merge and fill missing values with NI data
ethni11 <- ethni11 %>%
  left_join(ethni11ni, by = "constituency_geographic_code", suffix = c("", ".ni")) %>%
  mutate(
    c11EthnicityWhite = round(ifelse(is.na(c11EthnicityWhite), c11EthnicityWhite.ni, c11EthnicityWhite), 2),
    c11EthnicityMixed = round(ifelse(is.na(c11EthnicityMixed), c11EthnicityMixed.ni, c11EthnicityMixed), 2),
    c11EthnicityAsian = round(ifelse(is.na(c11EthnicityAsian), c11EthnicityAsian.ni, c11EthnicityAsian), 2),
    c11EthnicityBlack = round(ifelse(is.na(c11EthnicityBlack), c11EthnicityBlack.ni, c11EthnicityBlack), 2),
    c11EthnicityOther = round(ifelse(is.na(c11EthnicityOther), c11EthnicityOther.ni, c11EthnicityOther), 2)
  ) %>%
  select(-ends_with(".ni"))


## Religion
reli11 <- reli11 %>%
  rename(
    constituency_geographic_code = ons_id)

# Fix NI tenure
colnames(reli11ni) <- as.character(unlist(reli11ni[5, ]))
reli11ni <- reli11ni[-(1:5), -(c(1, 3:11))]
rownames(reli11ni) <- NULL

# c11Christian → sum of Catholic + Presbyterian + Church of Ireland + Methodist + Other Christian
# c11ReligionOther → Religion: Other religions
# c11NoReligion → No religion
# c11ReligionNotStated → Religion not stated
reli11ni <- reli11ni %>%
  mutate(
    c11Christian = rowSums(cbind(
      as.numeric(`Religion: Catholic (%)`),
      as.numeric(`Religion: Presbyterian Church in Ireland (%)`),
      as.numeric(`Religion: Church of Ireland (%)`),
      as.numeric(`Religion: Methodist Church in Ireland (%)`),
      as.numeric(`Religion: Other Christian (including Christian related) (%)`)
    ), na.rm = TRUE),
    c11ReligionOther = as.numeric(`Religion: Other religions (%)`),
    c11NoReligion = as.numeric(`Religion: No religion (%)`),
    c11ReligionNotStated = as.numeric(`Religion: Religion not stated (%)`)
  ) %>%
  select(
    constituency_geographic_code = `AA Code`,
    c11Christian,
    c11ReligionOther,
    c11NoReligion,
    c11ReligionNotStated
  ) %>%
  mutate(across(-constituency_geographic_code, ~ round(., 2)))

# Now for England, Wales and Scotland:
reli11 <- reli11 %>%
  mutate(
    c11ReligionOther = rowSums(cbind(
      as.numeric(c11Buddhist),
      as.numeric(c11Hindu),
      as.numeric(c11Jewish),
      as.numeric(c11Muslim),
      as.numeric(c11Sikh),
      as.numeric(c11ReligionOther)
    ), na.rm = TRUE)
  ) %>%
  select(
    constituency_geographic_code,
    c11Christian,
    c11ReligionOther,
    c11NoReligion,
    c11ReligionNotStated
  ) %>%
  mutate(across(-constituency_geographic_code, ~ round(as.numeric(.), 2)))

# Combining
reli11 <- reli11 %>%
  left_join(reli11ni, by = "constituency_geographic_code", suffix = c("", ".ni")) %>%
  mutate(
    c11Christian = round(ifelse(is.na(c11Christian), c11Christian.ni, c11Christian), 2),
    c11ReligionOther = round(ifelse(is.na(c11ReligionOther), c11ReligionOther.ni, c11ReligionOther), 2),
    c11NoReligion = round(ifelse(is.na(c11NoReligion), c11NoReligion.ni, c11NoReligion), 2),
    c11ReligionNotStated = round(ifelse(is.na(c11ReligionNotStated), c11ReligionNotStated.ni, c11ReligionNotStated), 2)
  ) %>%
  select(-ends_with(".ni"))

# Merge
socio_var <- list(age11, sex11, car11, employment11, tenure11, edu11, ethni11, reli11)
census11 <- reduce(socio_var, full_join, by = "constituency_geographic_code")



### Merge all
dataTurnout = turnout[!turnout$election_year == 2024,]
dataTurnout <- dataTurnout %>%
  left_join(census11, by = "constituency_geographic_code")


## Re-scaling some predictors:
vars_to_rescale <- c(
  "age_0_15", "age_16_24", "age_25_44", "age_45_64", "age_65_plus", 
  "male", "female", "c11CarsNone", "c11CarsOne", "c11CarsTwo", 
  "c11CarsThree", "c11CarsFour", "c11Unemployed", "c11EmployedFullTime", 
  "c11Retired", "c11FulltimeStudent", "c11HouseOwned", "c11HouseSocial", 
  "c11HousePrivate", "c11QualNone", "c11QualLevel1", "c11QualLevel2", 
  "c11QualLevel3", "c11QualLevel4", "c11EthnicityWhite", "c11EthnicityMixed", 
  "c11EthnicityAsian", "c11EthnicityBlack", "c11EthnicityOther", 
  "c11Christian", "c11ReligionOther", "c11NoReligion", "c11ReligionNotStated"
)

# Re-scaling:
dataTurnout <- dataTurnout %>%
  mutate(across(all_of(vars_to_rescale), ~ . / 100))

# Herfindahl-Hirschman Index (HHI) for homogeneity
dataTurnout <- dataTurnout %>%
  mutate(
    religious_fractionalization = 1 - (
      (c11Christian^2) +
        (c11ReligionOther^2) +
        (c11NoReligion^2) +
        (c11ReligionNotStated^2)
    ),
    ethnic_fractionalization = 1 - (
      (c11EthnicityWhite^2) +
        (c11EthnicityMixed^2) +
        (c11EthnicityAsian^2) +
        (c11EthnicityBlack^2) +
        (c11EthnicityOther^2)
    )
  ) %>%
  mutate(across(c(religious_fractionalization, ethnic_fractionalization), ~ round(., 4)))

# Export
# export(dataTurnout, "dataTurnoutFinal.csv")





