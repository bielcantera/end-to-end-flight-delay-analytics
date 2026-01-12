# Installing and loading the necessary package
if (!require(matrixStats)) install.packages("matrixStats")
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(data.table)) install.packages("data.table")
if (!require(dplyr)) install.packages("dplyr")
if (!require(reshape2)) install.packages("reshape2")
if (!require(caret)) install.packages("caret")
if (!require(e1071)) install.packages("e1071")
if (!require(rpart)) install.packages("rpart")
if (!require(nnet)) install.packages("nnet")
if (!require(factoextra)) install.packages("factoextra")
if (!require(keras)) install.packages("keras")
if (!require(data.table)) install.packages("data.table")
if (!require(lubridate)) install.packages("lubridate")
if (!require(tidyr)) install.packages("tidyr")

library(data.table)
library(keras)
library(factoextra)
library(rpart)
library(nnet)
library(reshape2)
library(matrixStats) # For more advanced matrix operations
library(ggplot2) # For data visualization, similar to Matplotlib
library(data.table) # Provides fast data manipulation, similar to Pandas dataframes
library(dplyr) # Another option for data manipulation with a different syntax
library(caret)
library(e1071)
library(lubridate)
library(tidyr)

#rm(list = ls())

# Suppress warnings
options(warn = -1)

setwd("C:/Users/JoanCarles/iCloudDrive/Documents/_Universitats/_Bachelors/Salle/data-science/Moduls/Module 2/")

# Read the CSV file into a dataframe
passengers <- read.csv('T3_AIR_CARRIER_SUMMARY_AIRPORT_ACTIVITY_2019.csv')

# Display the contents of the dataframe
print(passengers)

# Read the CSV file into a dataframe
aircraft <- read.csv("B43_AIRCRAFT_INVENTORY.csv", 
                     encoding = "latin1")

# Remove duplicates based on the 'TAIL_NUM' column
aircraft <- aircraft %>% distinct(TAIL_NUM, .keep_all = TRUE)

# Display the contents of the dataframe
print(aircraft)

# Read the CSV file into a dataframe
coords <- read.csv('AIRPORT_COORDINATES.csv')

# Remove duplicates based on the 'ORIGIN_AIRPORT_ID' column
coords <- coords %>% distinct(ORIGIN_AIRPORT_ID, .keep_all = TRUE)

# Display the contents of the dataframe
print(coords)

# Read the CSV file into a dataframe
names <- read.csv("CARRIER_DECODE.csv")

# Remove duplicates
names <- names %>% distinct()

# Further remove duplicates specifically in the 'OP_UNIQUE_CARRIER' column
names <- names %>% distinct(OP_UNIQUE_CARRIER, .keep_all = TRUE)

# Display the dataframe
print(names)

# Read the CSV file into a dataframe
employees <- read.csv('P10_EMPLOYEES.csv')

# Select specific columns and then group by 'OP_UNIQUE_CARRIER', and sum the other columns
employees <- employees %>%
  select(OP_UNIQUE_CARRIER, PASS_GEN_SVC_ADMIN, PASSENGER_HANDLING) %>%
  group_by(OP_UNIQUE_CARRIER) %>%
  summarise_all(sum)

# Display the dataframe
print(employees)

# Read the CSV file into a dataframe
weather_report <- read.csv('airport_weather_2019.csv')

# Check the result
head(weather_report)


# Function to try different date formats and return the first one that works
tryFormats1 <- function(date) {
  formats <- c("%m/%d/%Y") # Add or adjust formats as needed
  for (format in formats) {
    parsed_date <- try(as.Date(date, format=format), silent=TRUE)
    if (!inherits(parsed_date, "try-error")) {
      return(parsed_date)
    }
  }
  return(NA) # Return NA if no format matches
}

weather_report1 <- weather_report
# Apply the function to the DATE column
weather_report1$DATE <- sapply(weather_report$DATE, tryFormats1)
# Remove rows where DATE is NA
weather_report1 <- weather_report1 %>% filter(!is.na(DATE))

# Function to try different date formats and return the first one that works
tryFormats2 <- function(date) {
  formats <- c("%Y-%m-%d") # Add or adjust formats as needed
  for (format in formats) {
    parsed_date <- try(as.Date(date, format=format), silent=TRUE)
    if (!inherits(parsed_date, "try-error")) {
      return(parsed_date)
    }
  }
  return(NA) # Return NA if no format matches
}

weather_report2 <- weather_report
# Apply the function to the DATE column
weather_report2$DATE <- sapply(weather_report$DATE, tryFormats2)
# Remove rows where DATE is NA
weather_report2 <- weather_report2 %>% filter(!is.na(DATE))

weather_report <- rbind(weather_report1, weather_report2)


# Read the CSV file into a dataframe
cities <- read.csv('airports_list.csv')

# Display the dataframe
print(cities)

# Merge the 'cities' and 'weather_report' dataframes
weather_merge <- merge(cities, weather_report, by = 'NAME', all.x = TRUE)

# Display the merged dataframe
print(weather_merge)

# Select specific columns from the 'weather_merge' dataframe
weather <- select(weather_merge, DATE, PRCP, SNOW, SNWD, TMAX, AWND, ORIGIN_AIRPORT_ID)

# Drop rows where 'ORIGIN_AIRPORT_ID' is NA
weather <- weather %>% filter(!is.na(ORIGIN_AIRPORT_ID))

# Display the modified dataframe
print(weather)


# Impute mean in NA rows for TMAX
weather$TMAX <- ifelse(is.na(weather$TMAX),
                       round(ave(weather$TMAX, weather$ORIGIN_AIRPORT_ID, FUN = function(x) mean(x, na.rm = TRUE)), 1),
                       weather$TMAX)

# Impute mean in NA rows for AWND
weather$AWND <- ifelse(is.na(weather$AWND),
                       round(ave(weather$AWND, weather$ORIGIN_AIRPORT_ID, FUN = function(x) mean(x, na.rm = TRUE)), 1),
                       weather$AWND)


# Replace NA in PRCP, SNOW, SNWD with specific values 0

weather <- weather %>%
  mutate(
    PRCP = replace(PRCP, is.na(PRCP), 0),
    SNOW = replace(SNOW, is.na(SNOW), 0),
    SNWD = replace(SNWD, is.na(SNWD), 0),
    AWND = replace(AWND, is.na(AWND), 0)
  )

# remove rows where DATE or TMAX has NA values


weather <- weather %>%
  filter(!(is.na(DATE) | is.na(TMAX)))

# Check for the number of NA values in each column of the 'weather' dataframe
na_counts <- colSums(is.na(weather))

# Display the counts
print(na_counts)



# Convert 'DATE' to datetime and extract 'MONTH' and 'DAY_OF_MONTH'
weather <- weather %>%
  mutate(
    DATE = as.Date(DATE, origin="1970-01-01"),
    MONTH = month(DATE),
    DAY_OF_MONTH = day(DATE)
  )

# Display the dataframe
print(weather)

