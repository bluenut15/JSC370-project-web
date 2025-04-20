library(httr)
library(jsonlite)
library(readr)
library(dplyr)
library(tidyr)
library(tidyverse)
library(tidytext)
library(ggplot2)
library(ggcorrplot)
library(glm2)
library(purrr)
library(patchwork)
library(data.table)
library(knitr)
library(kableExtra)
library(gridExtra)


## API Data Retrieval
# Function to fetch data for a given indicator
fetch_indicator_data <- function(indicator_code) {
  url <- paste0("https://ghoapi.azureedge.net/api/", indicator_code)
  
  # Send GET request
  response <- GET(url)
  
  # Check if request was successful
  if (status_code(response) == 200) {
    # Parse JSON response
    data <- fromJSON(content(response, as = "text"))
    
    # Extract relevant data
    return(data$value)
  } else {
    print(paste("Failed to retrieve data for indicator:", indicator_code))
    return(NULL)
  }
}

# Retrieve data for indicators
data_suicide_raw <- fetch_indicator_data("SDGSUICIDE") # Crude suicide rates (per 100 000 population)
data_health_spending_raw <- fetch_indicator_data("GHED_CHEGDP_SHA2011") # Current health expenditure (CHE) as percentage of GDP


## API Data Cleaning
# ---- Cleaning Suicide Data ----
if (!is.null(data_suicide_raw)) {
  data_suicide <- data_suicide_raw %>%
    filter(Dim1 == "SEX_BTSX" & Dim2 == "AGEGROUP_YEARSALL") %>% # Both sexes, all age groups
    select(
      Country = SpatialDim,    # Country code
      Region = ParentLocation, # Region
      Year = TimeDim,          # Year
      SuicideRate = NumericValue # Suicide rate value
    )
} else {
  print("Failed to retrieve suicide data.")
}

# ---- Cleaning Health Spending Data ----
if (!is.null(data_health_spending_raw)) {
  data_health_spending <- data_health_spending_raw %>%
    select(
      Country = SpatialDim,    # Country code
      Region = ParentLocation, # Region
      Year = TimeDim,          # Year
      HealthSpendingPercent = NumericValue # Percentage
    )
} else {
  print("Failed to retrieve health spending data.")
}


# Downloaded Data Retrieval
data_gdp <- read.csv("final_report/gdp.csv")
data_gini <- read.csv("final_report/gini.csv")

# Ensure consistent column names for merging
data_gdp <- data_gdp %>% rename(Country = Code)
data_gini <- data_gini %>% rename(Country = Code)


# Remove rows with any NA values in all datasets
data_health_spending <- data_health_spending %>% drop_na()
data_suicide <- data_suicide %>% drop_na()
data_gdp <- data_gdp %>% drop_na()

# Remove the column if all its values are NA
data_gini <- data_gini %>%
  select(-X990179.annotations)  

# Merge datasets by Country and Year
data_merged <- data_health_spending %>%
  inner_join(data_suicide, by = c("Country", "Year", "Region")) %>%
  inner_join(data_gdp, by = c("Country", "Year")) %>%
  inner_join(data_gini, by = c("Country", "Year"))

# Remove rows with NA values
data_merged <- na.omit(data_merged)

# Remove one of the duplicate country name columns (keeping Entity.x)
data_merged <- data_merged %>%
  select(-Entity.y) %>%  # Remove Entity.y
  rename(
    Country_Name = Entity.x,  # Rename country name column
    GDP_per_Capita = GDP.per.capita..PPP..constant.2021.international...,  # Rename GDP column
    GINI_Index = Gini.coefficient  # Rename GINI column
  )
