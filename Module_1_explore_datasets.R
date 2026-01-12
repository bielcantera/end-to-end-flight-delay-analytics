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

rm(list = ls())

# Suppress warnings
options(warn = -1)
#============================================================================================
# define working directory
#============================================================================================
setwd("C:/Users/JoanCarles/iCloudDrive/Documents/_Universitats/_Bachelors/Salle/data-science/Moduls/Module 1/")
# Read the CSV file into a dataframe
df <- read.csv('ONTIME_REPORTING_01.csv')

# Print the dimensions of the dataframe
print(dim(df))
head(df)

# Check memory usage of the dataframe
memory_usage <- object.size(df)
print(memory_usage)

# Display the structure of the dataframe to see data types of each column
str(df)

# Generate descriptive statistics for the dataframe
summary(df)

# Count the number of missing (NA) values in each column of the dataframe
colSums(is.na(df))

