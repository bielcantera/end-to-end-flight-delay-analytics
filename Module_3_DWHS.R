# ===================================================================================================
# Part 1:  Datawarehouse creation 
# ===================================================================================================


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

setwd("C:/Users/JoanCarles/iCloudDrive/Documents/_Universitats/_Bachelors/Salle/data-science/Moduls/Module 3/Case/")

# Start the timer
start_time <- Sys.time()

# Function to rename duplicate columns
rename_duplicates <- function(df) {
  names(df) <- make.unique(names(df), sep = "_")
  return(df)
}

# Drop rows where TAIL_NUM is NA 
df <- df %>% filter(!is.na(TAIL_NUM) & TAIL_NUM != "")  

month_cleanup <- function(monthly_data, aircraft, coords, names, weather, passengers, employees) {
  
  # CLEANING
  
  # Dropping rows with no tail number
  print("Dropped NaNs or Space from Tail_Num" )
  
  monthly_data <- monthly_data %>%
    filter(!is.na(TAIL_NUM) & TAIL_NUM != "")   
  
  # FEATURE ENGINEERING - SEGMENT NUMBER
  # List flight segment number for daily flight segments by tracking tail number
  setorder(monthly_data, DAY_OF_MONTH,TAIL_NUM) 
  cat("Adding Flight Number Sequence - SEGMENT_NUMBER\n")
  monthly_data <- monthly_data %>%
    ungroup() %>%
    group_by(TAIL_NUM, DAY_OF_MONTH) %>%
    mutate(SEGMENT_NUMBER = dense_rank(DEP_TIME))
  setorder(monthly_data, DAY_OF_MONTH,TAIL_NUM,SEGMENT_NUMBER) 
  
  # FEATURE ENGINEERING - CONCURRENT FLIGHTS
  cat("Adding Concurrent Flights - CONCURRENT_FLIGHTS\n")
  monthly_data <- monthly_data %>%
    ungroup() %>%
    group_by(ORIGIN_AIRPORT_ID, DAY_OF_MONTH, DEP_TIME_BLK) %>%
    mutate(CONCURRENT_FLIGHTS = n()) %>%
    ungroup()
  
  # MERGING to get NUMBER_OF_SEATS
  cat("Applying seat counts to flights - NUMBER_OF_SEATS\n")
  monthly_data <- merge(monthly_data, aircraft, by = 'TAIL_NUM', all.x = TRUE)
  
  # Fill missing aircraft info with means
  monthly_data$NUMBER_OF_SEATS[is.na(monthly_data$NUMBER_OF_SEATS)] <- mean(monthly_data$NUMBER_OF_SEATS, na.rm = TRUE)
  
  # Simplify data type of number of seats to reduce memory usage
  monthly_data$NUMBER_OF_SEATS <- as.integer(monthly_data$NUMBER_OF_SEATS)
  
  # MERGING to get proper carrier name
  cat("Applying Carrier Names - CARRIER_NAME\n")
  monthly_data <- merge(monthly_data, names, by = 'OP_UNIQUE_CARRIER', all.x = TRUE)
  
  # FEATURE ENGINEERING - Monthly Flight Statistics
  cat("Adding flight statistics for carrier and airport - AIRPORT_FLIGHTS_MONTH, AIRLINE_FLIGHTS_MONTH, AIRLINE_AIRPORT_FLIGHTS_MONTH\n")
  monthly_data <- monthly_data %>%
    group_by(ORIGIN_AIRPORT_ID) %>%
    mutate(AIRPORT_FLIGHTS_MONTH = n()) %>%
    ungroup() %>%
    group_by(OP_UNIQUE_CARRIER) %>%
    mutate(AIRLINE_FLIGHTS_MONTH = n()) %>%
    ungroup() %>%
    group_by(OP_UNIQUE_CARRIER, ORIGIN_AIRPORT_ID) %>%
    mutate(AIRLINE_AIRPORT_FLIGHTS_MONTH = n()) %>%
    ungroup()
  
  # FEATURE ENGINEERING - Average Monthly Passengers
  cat("Adding passenger statistics for carrier and airport - AVG_MONTHLY_PASS_AIRPORT, AVG_MONTHLY_PASS_AIRLINE\n")
  
  monthly_airport_passengers <- passengers %>%
    ungroup() %>%
    group_by(ORIGIN_AIRPORT_ID) %>%
    summarise(REV_PAX_ENP_110 = sum(REV_PAX_ENP_110, na.rm = TRUE), .groups = "drop")
  monthly_data <- merge(monthly_data, monthly_airport_passengers, by = 'ORIGIN_AIRPORT_ID', all.x = TRUE)
  monthly_data$AVG_MONTHLY_PASS_AIRPORT <- (monthly_data$REV_PAX_ENP_110 / 12)
  
  monthly_airline_passengers <- passengers %>%
    ungroup() %>%
    group_by(OP_UNIQUE_CARRIER) %>%
    summarise(REV_PAX_ENP_110 = sum(REV_PAX_ENP_110, na.rm = TRUE), .groups = "drop")
  monthly_data <- merge(monthly_data, monthly_airline_passengers, by = 'OP_UNIQUE_CARRIER', all.x = TRUE)
  monthly_data$AVG_MONTHLY_PASS_AIRLINE <- (monthly_data$REV_PAX_ENP_110.y / 12)
  
  
  # MERGING - Add Employee Stats
  cat("Adding employee statistics for carrier - FLT_ATTENDANTS_PER_PASS, GROUND_SERV_PER_PASS\n")
  monthly_data <- merge(monthly_data, employees, by = 'OP_UNIQUE_CARRIER', all.x = TRUE)
  monthly_data$FLT_ATTENDANTS_PER_PASS <- monthly_data$PASSENGER_HANDLING / monthly_data$REV_PAX_ENP_110.y
  monthly_data$GROUND_SERV_PER_PASS <- monthly_data$PASS_GEN_SVC_ADMIN / monthly_data$REV_PAX_ENP_110.y
  
  # FEATURE ENGINEERING - Plane Age
  cat("Calculate Fleet Age - PLANE_AGE\n")
  monthly_data$MANUFACTURE_YEAR[is.na(monthly_data$MANUFACTURE_YEAR)] <- mean(monthly_data$MANUFACTURE_YEAR, na.rm = TRUE)
  monthly_data$PLANE_AGE <- 2019 - monthly_data$MANUFACTURE_YEAR
  
  # MERGING - Get Airport Coordinates
  cat("Adding airport coordinates - LATITUDE, LONGITUDE, DEPARTING_AIRPORT\n")
  monthly_data <- merge(monthly_data, coords, by = 'ORIGIN_AIRPORT_ID', all.x = TRUE)
  monthly_data$LATITUDE <- round(monthly_data$LATITUDE, 3)
  monthly_data$LONGITUDE <- round(monthly_data$LONGITUDE, 3)
  
  # MERGING
  # Merge weather data
  cat("Adding daily weather data - PRCP, SNOW, SNWD, SMAX, TMIN, AWND\n")
  monthly_data <- merge(monthly_data, weather, by = c("ORIGIN_AIRPORT_ID", "MONTH", "DAY_OF_MONTH"), all.x = TRUE)
  
  # CLEANING - Drop Unneeded Columns
  cat("Clean up unneeded columns\n")
  monthly_data <- monthly_data %>%
    select(-c( CRS_DEP_TIME, CRS_ARR_TIME, ARR_TIME, 
               CANCELLATION_CODE, CRS_ELAPSED_TIME, DISTANCE,
               CARRIER_DELAY, WEATHER_DELAY, NAS_DELAY, SECURITY_DELAY, LATE_AIRCRAFT_DELAY,
               ARR_TIME_BLK, ACTUAL_ELAPSED_TIME,
               PASSENGER_HANDLING, X, AIRLINE_ID,
               REV_PAX_ENP_110.x , REV_PAX_ENP_110.y , 
               DEST_AIRPORT_ID, 
               PASS_GEN_SVC_ADMIN, MANUFACTURE_YEAR))
  
  
  # CLEANING - Specify Data Types
  cat("Cleaning up data types\n")
  monthly_data$MONTH <- as.factor(monthly_data$MONTH)
  monthly_data$DAY_OF_WEEK <- as.factor(monthly_data$DAY_OF_WEEK)
  monthly_data$DEP_DEL15 <- as.integer(monthly_data$DEP_DEL15)
  monthly_data$DISTANCE_GROUP <- as.integer(monthly_data$DISTANCE_GROUP)
  monthly_data$SEGMENT_NUMBER <- as.integer(monthly_data$SEGMENT_NUMBER)
  monthly_data$AIRPORT_FLIGHTS_MONTH <- as.integer(monthly_data$AIRPORT_FLIGHTS_MONTH)
  monthly_data$AIRLINE_FLIGHTS_MONTH <- as.integer(monthly_data$AIRLINE_FLIGHTS_MONTH)
  monthly_data$AIRLINE_AIRPORT_FLIGHTS_MONTH <- as.integer(monthly_data$AIRLINE_AIRPORT_FLIGHTS_MONTH)
  monthly_data$PLANE_AGE <- as.integer(monthly_data$PLANE_AGE)
  
  cat("FINISHED\n")
  setorder(monthly_data, DAY_OF_MONTH,TAIL_NUM,SEGMENT_NUMBER) 
  
  # Return cleaned file
  setorder(monthly_data, DAY_OF_MONTH,TAIL_NUM, DEP_TIME,   SEGMENT_NUMBER)
  return(monthly_data)
  
}

# Read and process each month of raw data using the cleaning function
df <- read.csv('ONTIME_REPORTING_01.csv')
month01 <- month_cleanup(df, aircraft, coords, names, weather, passengers, employees)
df <- read.csv('ONTIME_REPORTING_02.csv')
month02 <- month_cleanup(df, aircraft, coords, names, weather, passengers, employees)
df <- read.csv('ONTIME_REPORTING_03.csv')
month03 <- month_cleanup(df, aircraft, coords, names, weather, passengers, employees)
df <- read.csv('ONTIME_REPORTING_04.csv')
month04 <- month_cleanup(df, aircraft, coords, names, weather, passengers, employees)
df <- read.csv('ONTIME_REPORTING_05.csv')
month05 <- month_cleanup(df, aircraft, coords, names, weather, passengers, employees)
df <- read.csv('ONTIME_REPORTING_06.csv')
month06 <- month_cleanup(df, aircraft, coords, names, weather, passengers, employees)
df <- read.csv('ONTIME_REPORTING_07.csv')
month07 <- month_cleanup(df, aircraft, coords, names, weather, passengers, employees)
df <- read.csv('ONTIME_REPORTING_08.csv')
month08 <- month_cleanup(df, aircraft, coords, names, weather, passengers, employees)
df <- read.csv('ONTIME_REPORTING_09.csv')
month09 <- month_cleanup(df, aircraft, coords, names, weather, passengers, employees)
df <- read.csv('ONTIME_REPORTING_10.csv')
month10 <- month_cleanup(df, aircraft, coords, names, weather, passengers, employees)
df <- read.csv('ONTIME_REPORTING_11.csv')
month11 <- month_cleanup(df, aircraft, coords, names, weather, passengers, employees)
df <- read.csv('ONTIME_REPORTING_12.csv')
month12 <- month_cleanup(df, aircraft, coords, names, weather, passengers, employees)

# Combine master file
all_data <- rbind(month01, month02, month03, month04, month05, month06,month07, month08, month09, month10, month11, month12)

#=============================================================================================================================
# FEATURE ENGINEERING - POPULATE PREVIOUS AIRPORT for each TAIL_NUM, first SEGMENT for each TAIL_NUM has PREV_AIRPORT = None 
#=============================================================================================================================
# Get previous airport for tail number
print("Adding airports - PREVIOUS_AIRPORT")

# Convert monthly_data to a data.table if it's not already
setDT(all_data)
setorder(all_data,TAIL_NUM,MONTH, DAY_OF_MONTH, SEGMENT_NUMBER)

# Create segment_temp as a subset of monthly_data
segment_temp <- all_data[, .(TAIL_NUM, MONTH, DAY_OF_MONTH, SEGMENT_NUMBER, DISPLAY_AIRPORT_NAME)]
# Sort the data.tables
setorder(segment_temp,TAIL_NUM,MONTH, DAY_OF_MONTH,   SEGMENT_NUMBER)

# Create a unique key in all_data
all_data[, unique_key := paste(TAIL_NUM, MONTH, DAY_OF_MONTH, SEGMENT_NUMBER, sep = "_")]
# Create a lagged version of TAIL_NUM
all_data[, PREV_TAIL_NUM := shift(TAIL_NUM, type = "lag", fill = NA)]
# Create a shifted version of the unique key in all_data to refer to the previous row
all_data[, unique_key_prev := ifelse(TAIL_NUM == PREV_TAIL_NUM, shift(unique_key, type = "lag", fill = NA), NA)]

# Create a unique key in segment_temp
segment_temp[, unique_key := paste(TAIL_NUM, MONTH, DAY_OF_MONTH, SEGMENT_NUMBER, sep = "_")]
# Create a lagged version of TAIL_NUM
segment_temp[, PREV_TAIL_NUM := shift(TAIL_NUM, type = "lag", fill = NA)]
# Create a shifted version of the unique key in all_data to refer to the previous row
segment_temp[, unique_key_prev := ifelse(TAIL_NUM == PREV_TAIL_NUM, shift(unique_key, type = "lag", fill = NA), NA)]

# Perform the join
all_data <- segment_temp[all_data, on = .(unique_key = unique_key_prev), nomatch = NA]

cat("Clean up unneeded columns\n")
all_data <- all_data %>%
  select(-c( TAIL_NUM,MONTH, DAY_OF_MONTH, SEGMENT_NUMBER, unique_key, PREV_TAIL_NUM, unique_key_prev,i.unique_key,i.PREV_TAIL_NUM))

# Rename columns and replace NA values
setnames(all_data, old = c("DISPLAY_AIRPORT_NAME", "i.DISPLAY_AIRPORT_NAME","i.SEGMENT_NUMBER","i.TAIL_NUM","i.MONTH","i.DAY_OF_MONTH"), 
         new = c("PREVIOUS_AIRPORT", "DEPARTING_AIRPORT","SEGMENT_NUMBER","TAIL_NUM","MONTH","DAY_OF_MONTH"))

# Assign NONE to PREVIOUS_AIRPORT of first SEGMENT for each TAIL_NUM
all_data[, PREVIOUS_AIRPORT := fifelse(is.na(PREVIOUS_AIRPORT), 'NONE', PREVIOUS_AIRPORT)]

# End routine to populate PREVIOUS AIRPORT 

# CREATE LOOK UP TABLES on DEP_DEL15

cat("Create LOOK UP Tables\n")

# Create carrier historical lookup table
carrier_historical <- all_data %>%
  group_by(CARRIER_NAME, MONTH) %>%
  summarise(CARRIER_HISTORICAL = mean(DEP_DEL15, na.rm = TRUE)) %>%
  ungroup()

# Create departing airport historical lookup table
dep_airport_historical <- all_data %>%
  group_by(DEPARTING_AIRPORT, MONTH) %>%
  summarise(DEP_AIRPORT_HIST = mean(DEP_DEL15, na.rm = TRUE)) %>%
  ungroup()

# Create previous airport historical lookup table
prev_airport_historical <- all_data %>%
  group_by(PREVIOUS_AIRPORT, MONTH) %>%
  summarise(PREV_AIRPORT_HIST = mean(DEP_DEL15, na.rm = TRUE)) %>%
  ungroup()

# Create arrival airport historical lookup table
arr_airport_historical <- all_data %>%
  group_by(DEST, MONTH) %>%
  summarise(ARR_AIRPORT_HIST = mean(DEP_DEL15, na.rm = TRUE)) %>%
  ungroup()

# Create day historical lookup table
day_historical <- all_data %>%
  group_by(DAY_OF_WEEK, MONTH) %>%
  summarise(DAY_HISTORICAL = mean(DEP_DEL15, na.rm = TRUE)) %>%
  ungroup()

# Create departure time block lookup table
dep_block_lookup <- all_data %>%
  group_by(DEP_TIME_BLK, MONTH) %>%
  summarise(DEP_BLOCK_HIST = mean(DEP_DEL15, na.rm = TRUE)) %>%
  ungroup()

# Merge carrier_historical with train
all_data <- left_join(all_data, carrier_historical, by = c("CARRIER_NAME", "MONTH"))

# Merge airport_historical with train
all_data <- left_join(all_data, dep_airport_historical, by = c("DEPARTING_AIRPORT", "MONTH"))

# Merge previous airport with train
all_data <- left_join(all_data, prev_airport_historical, by = c("PREVIOUS_AIRPORT", "MONTH"))

# Merge airport_historical with train
all_data <- left_join(all_data, arr_airport_historical, by = c("DEST", "MONTH"))

# Merge day_historical with train
all_data <- left_join(all_data, day_historical, by = c("DAY_OF_WEEK", "MONTH"))

# Merge dep_block_lookup with train
all_data <- left_join(all_data, dep_block_lookup, by = c("DEP_TIME_BLK", "MONTH"))

# Fill NA values in DEP_AIRPORT_HIST column with its mean
all_data$DEP_AIRPORT_HIST <- ifelse(is.na(all_data$DEP_AIRPORT_HIST), 
                                    mean(all_data$DEP_AIRPORT_HIST, na.rm = TRUE), 
                                    all_data$DEP_AIRPORT_HIST)

# Fill NA values in ARR_AIRPORT_HIST column with its mean
all_data$ARR_AIRPORT_HIST <- ifelse(is.na(all_data$ARR_AIRPORT_HIST), 
                                    mean(all_data$ARR_AIRPORT_HIST, na.rm = TRUE), 
                                    all_data$ARR_AIRPORT_HIST)

# Add TMAX_C (Celsius) to train dataset, keeping TMAX in Fahrenheit
all_data <- all_data %>%
  mutate(TMAX_C = round((TMAX - 32) * 5/9))

# CLEANING  Droppng airports below the 10th percentile
cat("Dropping bottom 10% of airports\n")
threshold <- quantile(all_data$AIRPORT_FLIGHTS_MONTH, probs = 0.1)
all_data <- all_data[all_data$AIRPORT_FLIGHTS_MONTH >= threshold, ]

# Save the combined data as a CSV file ======================================================================
write.csv(all_data, 'train_val.csv', row.names = FALSE)

# Print elapsed time
elapsed_time <- Sys.time() - start_time
cat("Elapsed Time: ", elapsed_time, "\n")




