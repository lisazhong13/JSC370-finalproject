# Load required libraries
library(dplyr)
library(ggplot2)
library(plotly)
library(leaflet)
library(sf)
library(viridis)
library(ggcorrplot)
library(dbscan)
library(tidyr)

# Load and prepare data
df_cdc <- read.csv("data/cdc.csv")
df_cdc_cleaned <- df_cdc %>% filter(Year == 2021) %>%
  select(Location = LocationDesc, Disability = Response, 
         Prevalence = Data_Value, DisabilityPopulation = WeightedNumber)
json_file_path <- "data/acs1.json"  # Provide the correct path to your saved JSON file
json_data <- fromJSON(json_file_path)
df_census_2021 <- as.data.frame(json_data[-1,], stringsAsFactors = FALSE)
colnames(df_census_2021) <- json_data[1,]
df_census_2021 <- df_census_2021 %>%
  rename(
    State = NAME,
    Total_Population = B27010_001E,
    Uninsured_Population = B27010_017E
  ) %>%
  mutate(
    Total_Population = as.numeric(Total_Population),
    Uninsured_Population = as.numeric(Uninsured_Population),
    Uninsured_Rate = (Uninsured_Population / Total_Population) * 100
  ) %>%
  select(State, Uninsured_Rate)
df_cdc_cleaned <- df_cdc_cleaned %>%
  mutate(Location = tolower(Location))
df_census_2021 <- df_census_2021 %>%
  mutate(State = tolower(State))
df_merged <- df_cdc_cleaned %>%
  inner_join(df_census_2021, by = c("Location" = "State"))
poverty <- read.csv("data/Poverty_by_Age.csv")
insurance <- read.csv("data/Health_Insurance_Coverage_of_the_Total_Population.csv")
disability_percentage <- read.csv("data/Disability_Percentage.csv")
hospital <- read.csv("data/Hospitals.csv")
medicaid <- read.csv("data/Medicaid_Spending.csv")
poverty <- poverty %>%
  mutate(Location = tolower(Location))
insurance <- insurance %>%
  mutate(Location = tolower(Location))
hospital <- hospital %>%
  mutate(Location = tolower(Location))
disability_percentage <- disability_percentage %>%
  mutate(Location = tolower(Location))
medicaid <- medicaid %>%
  mutate(Location = tolower(Location))
df_final <- df_merged %>%
  inner_join(poverty, by = "Location") %>%
  inner_join(insurance, by = "Location") %>%
  inner_join(disability_percentage, by = "Location") %>%
  inner_join(hospital, by = "Location") %>%
  inner_join(medicaid, by = "Location")
df_final <- df_final %>% drop_na()
# Convert relevant columns to numeric
df_final <- df_final %>%
  mutate(
    Military = as.numeric(Military),
    Total.Medicare.Part.A.Enrollees = as.numeric(gsub(",", "", Total.Medicare.Part.A.Enrollees)),
    Total.Hospitals = as.numeric(Total.Hospitals),
    Total.Hospital.Beds = as.numeric(gsub(",", "", Total.Hospital.Beds)),
    Short.Stay.Hospitals = as.numeric(Short.Stay.Hospitals),
    Short.Stay.Hospital.Beds = as.numeric(gsub(",", "", Short.Stay.Hospital.Beds)),
    Critical.Access.Hospitals = as.numeric(Critical.Access.Hospitals),
    Critical.Access.Hospital.Beds = as.numeric(gsub(",", "", Critical.Access.Hospital.Beds)),
    All.Other.Hospitals = as.numeric(All.Other.Hospitals),
    All.Other.Hospital.Beds = as.numeric(gsub(",", "", All.Other.Hospital.Beds)),
    Seniors = as.numeric(gsub("[$,]", "", Seniors)),  # Remove $ and ,
    Individuals.with.Disabilities = as.numeric(gsub("[$,]", "", Individuals.with.Disabilities)),
    Adult = as.numeric(gsub("[$,]", "", Adult)),
    Children = as.numeric(gsub("[$,]", "", Children)),
    Newly.Eligible.Adult = as.numeric(gsub("[$,]", "", Newly.Eligible.Adult)),
    Total.y = as.numeric(gsub("[$,]", "", Total.y))
  ) %>%
  select(
    State = Location, 
    Disability, 
    DisabilityPrevalence = Prevalence, 
    DisabilityPopulation, 
    UninsuredRate = Uninsured_Rate, 
    PovertyChildren = Children.0.18, 
    PovertyAdult = Adults.19.64, 
    PovertySenior = X65., 
    PovertyTotal = Total.x, 
    EmployerInsuranceCoverage = Employer, 
    MedicaidInsuranceCoverage = Medicaid, 
    MedicareInsuranceCoverage = Medicare, 
    MilitaryInsuranceCoverage = Military, 
    SelfCareDisability = Self.Care, 
    HearingDisability = Hearing, 
    VisionDisability = Seeing, 
    IndependentLivingDisability = Living.Independently, 
    MobilityDisability = Ambulatory, 
    CognitiveDisability = Cognitive, 
    AnyDisability = Any.Disability, 
    TotalMedicareEnrollees = Total.Medicare.Part.A.Enrollees, 
    Total.Hospitals, 
    Total.Hospital.Beds, 
    TotalHospitalBedsPer1000Enrollees = Total.Hospital.Beds.Per.1.000.Part.A.Enrollees, 
    SeniorMedicaidSpending = Seniors, 
    DisabilityMedicaidSpending = Individuals.with.Disabilities, 
    AdultMedicaidSpending = Adult, 
    ChildrenMedicaidSpending = Children, 
    TotalMedicaidSpending = Total.y
  )
df_final <- df_final %>% drop_na()
# Load shapefile and join
us_states <- st_read("cb_2018_us_state_500k/cb_2018_us_state_500k.shp")
us_states$NAME <- tolower(us_states$NAME)
df_final$State <- tolower(df_final$State)
map_data <- left_join(us_states, df_final, by = c("NAME" = "State"))

# Standard mapping from full state names to abbreviations
state_lookup <- setNames(state.abb, tolower(state.name))

# Add StateAbbrev column
df_final$StateAbbrev <- state_lookup[tolower(df_final$State)]

# Drop rows that couldn't be matched (e.g., territories or bad data)
df_final_clean <- df_final %>%
  filter(!is.na(StateAbbrev))

# Function 1: Leaflet map for disability prevalence
plot_disability_map <- function() {
  plot_ly(
    data = df_final_clean,
    type = 'choropleth',
    locations = ~StateAbbrev,
    locationmode = 'USA-states',
    z = ~AnyDisability,
    colorscale = 'Reds',
    colorbar = list(title = "Disability Rate"),
    text = ~paste("State:", State, "<br>Disability:", round(AnyDisability * 100, 1), "%")
  ) %>%
    layout(
      geo = list(scope = 'usa'),
      title = "Disability Prevalence by State"
    )
}


plot_poverty_map <- function() {
  plot_ly(
    data = df_final_clean,
    type = 'choropleth',
    locations = ~StateAbbrev,
    locationmode = 'USA-states',
    z = ~PovertyTotal,
    colorscale = 'Reds',
    colorbar = list(title = "Poverty Probability (%)"),
    text = ~paste("State:", State, "<br>Poverty Rate:", round(PovertyTotal * 100, 1), "%")
  ) %>%
    layout(
      geo = list(scope = 'usa'),
      title = "Poverty Probability by State"
    )
}

# Function 3: Plotly scatter of poverty vs. disability with clusters
plot_cluster_scatter <- function() {
  # Drop rows with any NA in numeric columns
  df_numeric <- df_final %>%
    select(where(is.numeric)) %>%
    drop_na()
  
  # Remove 'Year' column before scaling
  df_numeric_noyear <- df_numeric
  
  # Normalize the numeric data (excluding Year)
  df_scaled <- scale(df_numeric_noyear)
  
  # Run HDBSCAN
  hdb <- hdbscan(df_scaled, minPts = 10)
  
  # Add clusters back to original (non-normalized) rows
  df_final_cleaned <- df_final %>%
    filter(complete.cases(select(., where(is.numeric)))) %>%
    mutate(cluster = as.factor(hdb$cluster))
  
  p <- ggplot(df_final_cleaned, aes(
    x = PovertyTotal,
    y = DisabilityPrevalence,
    color = cluster,
    text = paste("State:", State)
  )) +
    geom_point() +
    labs(title = "HDBSCAN Clustering of States Based on Disability and Poverty")
  ggplotly(p, tooltip = "text")
}

plot_disability_hist <- function() {
  plot_ly(df_final, x = ~DisabilityPrevalence, type = "histogram",
          marker = list(color = 'steelblue')) %>%
    layout(title = "Histogram of Disability Prevalence",
           xaxis = list(title = "Disability Prevalence"),
           yaxis = list(title = "Frequency"))
}

plot_poverty_hist <- function() {
  plot_ly(df_final, x = ~PovertyTotal, type = "histogram",
          marker = list(color = 'tomato')) %>%
    layout(title = "Histogram of Poverty Rate",
           xaxis = list(title = "Poverty Rate"),
           yaxis = list(title = "Frequency"))
}

plot_insurance_boxplot <- function() {
  df_plot <- df_final %>%
    select(EmployerInsuranceCoverage, MedicaidInsuranceCoverage, MedicareInsuranceCoverage) %>%
    pivot_longer(everything(), names_to = "InsuranceType", values_to = "Coverage")
  
  plot_ly(df_plot, y = ~Coverage, color = ~InsuranceType, type = "box") %>%
    layout(title = "Boxplot of Insurance Coverage by Type",
           yaxis = list(title = "Coverage Rate"),
           boxmode = "group")
}
