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
if (!require(forcats)) install.packages("forcats")
if (!require(naivebayes)) install.packages("naivebayesforcats")
if (!require(data.table)) install.packages("data.table")
if (!require(corrplot)) install.packages("corrplot")
if (!require(tidyr)) install.packages("tidyr")
if (!require(tidyverse)) install.packages("tidyverse")
if (!require(corrplot)) install.packages("corrplot")

library(naivebayes)
library(keras)
library(factoextra)
library(rpart)
library(nnet)
library(reshape2)
library(matrixStats) # For more advanced matrix operations
library(ggplot2) # For data visualization 
library(data.table) # Provides fast data manipulation, similar to Pandas dataframes
library(dplyr) # Another option for data manipulation with a different syntax
library(caret)
library(e1071)
library(forcats)
library(data.table)
library(tidyr)
library(tidyverse)
library(corrplot)

#rm(list = ls())

# Suppress warnings
options(warn = -1)

setwd("C:/Users/JoanCarles/iCloudDrive/Documents/_Universitats/_Bachelors/Salle/data-science/Moduls/Module 4/Case/")

#======================================================================================================
# Part 1: Classification
#======================================================================================================

# Importing the dataset

#df <- all_data
df <- read.csv("train_val.csv")

df_original <- df #  Keep a copy to avoid reading all data if it fails

# Checking the dataset
head(df)

# Show the dimensions of the dataset, number of rows and columns
dim(df)

# Check the data types of the columns
str(df)

####### clustering per top 40 airport and % of flights with delay > 15 mins
# Assuming 'all_data' is your dataframe and it contains 'DEP_DEL15' and 'departing_airport' columns

airport_delays <- all_data %>%
  group_by(DEPARTING_AIRPORT) %>%
  summarise(Total_Flights = n(),
            Delayed_Flights = sum(DEP_DEL15, na.rm = TRUE)) %>%
  mutate(Delay_Percentage = (Delayed_Flights / Total_Flights) * 100) %>%
  arrange(desc(Delay_Percentage)) %>%
  top_n(40, Delay_Percentage)

ggplot(airport_delays, aes(x = DEPARTING_AIRPORT, y = Delay_Percentage, size = Delay_Percentage)) +
  geom_point(alpha = 0.6, color = "blue") +  # Adjust alpha for transparency
  theme_minimal() +
  labs(title = "Top 10 Airports by Percentage of Delayed Flights",
       x = "Airport",
       y = "Percentage of Delays",
       size = "Percentage of Delays") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Rotate x-axis labels for readability

############### clustering and showing # flights per day of the week

# Assuming 'all_data' is your dataframe and it contains 'DAY_OF_WEEK' field
flights_per_day <- all_data %>%
  group_by(DAY_OF_WEEK) %>%
  summarise(Total_Flights = n())

# Optional: Translate 'DAY_OF_WEEK' to actual day names
day_names <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
flights_per_day$DAY_OF_WEEK <- factor(day_names[flights_per_day$DAY_OF_WEEK], levels = day_names)

ggplot(flights_per_day, aes(x = DAY_OF_WEEK, y = Total_Flights)) +
  geom_col(fill = "blue") +  # Bar plot
  theme_minimal() +
  labs(title = "Total Flights per Day of the Week",
       x = "Day of the Week",
       y = "Number of Flights")

######### clustering per day of the week and delays

flights_per_day <- all_data %>%
  group_by(DAY_OF_WEEK) %>%
  summarise(Total_Flights = n())

# Optional: Translate 'DAY_OF_WEEK' to actual day names
day_names <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
flights_per_day$DAY_OF_WEEK <- factor(day_names[flights_per_day$DAY_OF_WEEK], levels = day_names)

ggplot(flights_per_day, aes(x = DAY_OF_WEEK, y = Total_Flights, size = Total_Flights)) +
  geom_point(alpha = 0.6, color = "blue") +  # Bubble chart
  theme_minimal() +
  labs(title = "Total Flights per Day of the Week",
       x = "Day of the Week",
       y = "Number of Flights",
       size = "Number of Flights") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Rotate x-axis labels for readability


############## clustering per month showing snow and rainfaill

# Assuming 'all_data' contains the columns DEPARTING_AIRPORT, SNOW, and PRCP

# Aggregate snowfall data by airport
airport_snow_precipitation <- all_data %>%
  group_by(DEPARTING_AIRPORT) %>%
  summarise(Total_Snow = sum(SNOW, na.rm = TRUE)) %>%
  arrange(desc(Total_Snow))

# Aggregate rainfall data by airport
airport_rain_precipitation <- all_data %>%
  group_by(DEPARTING_AIRPORT) %>%
  summarise(Total_Rain = sum(PRCP, na.rm = TRUE)) %>%
  arrange(desc(Total_Rain))

# Merge the datasets
combined_precipitation <- inner_join(airport_snow_precipitation, airport_rain_precipitation, by = "DEPARTING_AIRPORT")

# Select top 30 airports (based on snowfall)
top_30_airports <- head(combined_precipitation, 30)

# Reshape for plotting
long_format_data <- pivot_longer(top_30_airports, cols = c(Total_Snow, Total_Rain), names_to = "Precipitation_Type", values_to = "Amount")

# Plotting
ggplot(long_format_data, aes(x = DEPARTING_AIRPORT, y = Amount, fill = Precipitation_Type)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_manual(values = c("Total_Snow" = "blue", "Total_Rain" = "green")) +
  theme_minimal() +
  labs(title = "Comparison of Total Snow and Rain Precipitation for Top 30 Airports",
       x = "Departing Airport",
       y = "Total Precipitation",
       fill = "Precipitation Type") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# Aggregate rainfall data by month
monthly_precipitation_long <- all_data %>%
  group_by(MONTH) %>%
  summarise(Total_Precipitation = sum(PRCP, na.rm = TRUE)) %>%
  arrange(desc(Total_Precipitation))

# Select top 12 months
monthly_precipitation_long <- head(monthly_precipitation_long, 12)

# Plotting precipitation data
ggplot(monthly_precipitation_long, aes(x = MONTH, y = Total_Precipitation, size = Total_Precipitation)) +
  geom_point(alpha = 0.6) +
  theme_minimal() +
  labs(title = "Total Precipitation by Month",
       x = "Month",
       y = "Total Precipitation",
       size = "Total Precipitation") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Aggregate snowfall data by airport
airport_snow_precipitation <- all_data %>%
  group_by(DEPARTING_AIRPORT) %>%
  summarise(Total_Snow = sum(SNOW, na.rm = TRUE)) %>%
  arrange(desc(Total_Snow))

# Select top 30 airports
top_30_airports_snow <- head(airport_snow_precipitation, 30)

ggplot(top_30_airports_snow, aes(x = DEPARTING_AIRPORT, y = Total_Snow, size = Total_Snow)) +
  geom_point(alpha = 0.6, color = "blue") +  # Bubble chart
  theme_minimal() +
  labs(title = "Top 30 Airports by Total Snow Precipitation",
       x = "Departing Airport",
       y = "Total Snow Precipitation",
       size = "Total Snow Precipitation") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Rotate x-axis labels for readability

monthly_precipitation_long <- head(monthly_precipitation_long, 30)

##############  clustering per CANCELLED flights 
airport_cancellations <- all_data %>%
  group_by(DEPARTING_AIRPORT) %>%
  summarise(Total_Flights = n(),
            Cancelled_Flights = sum(CANCELLED, na.rm = TRUE)) %>%
  mutate(Cancellation_Percentage = (Cancelled_Flights / Total_Flights) * 100) %>%
  arrange(desc(Cancellation_Percentage)) %>%
  top_n(40, Cancellation_Percentage)

ggplot(airport_cancellations, aes(x = DEPARTING_AIRPORT, y = Cancellation_Percentage, size = Cancellation_Percentage)) +
  geom_point(alpha = 0.6, color = "blue") +
  theme_minimal() +
  labs(title = "Top 40 Airports by Cancellation Percentage",
       x = "Departing Airport",
       y = "Cancellation Percentage",
       size = "Cancellation Percentage") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Rotate x-axis labels for readability

#==============================================================================================
# CLustering using  K-Means 
#===============================================================================================

# Data Preprocessing:
#  
#  Select relevant columns.
# Apply one-hot encoding to categorical variables.
# Scale the numerical variables.
# Selecting relevant columns

# Calculate the mean of the TMAX column, excluding NA values
mean_TMAX <- mean(df$TMAX, na.rm = TRUE)

# Fill missing values in TMAX with its mean value
df <- df %>%
  mutate(TMAX = ifelse(is.na(TMAX), mean_TMAX, TMAX))

# Fill NAs with 0
df <- df %>%
  mutate(across(where(is.numeric), ~ifelse(is.na(.), 0, .)))

# Data Preprocessing
# Select relevant columns and remove NA values
data_selected <- df %>%
  select(MONTH, ARR_DELAY_NEW, CONCURRENT_FLIGHTS, AIRPORT_FLIGHTS_MONTH, SNOW, PRCP) %>%
  na.omit()

# Remove rows with NA values
data_selected <- na.omit(data_selected)

# One-hot encoding for categorical variables (MONTH)
dummy_model <- dummyVars("~ .", data = data_selected)
data_transformed <- predict(dummy_model, newdata = data_selected)
data_transformed_df <- as.data.frame(data_transformed)

# Scale the data
data_scaled <-  scale(abs(data_transformed_df))

# PCA, or Principal Component Analysis, is a statistical technique used in data analysis and machine learning for dimensionality reduction while preserving as much of the data's variability as possible. It's commonly used to simplify complex datasets with many variables to a few principal components that capture the most important information (variance) in the data. Here's a breakdown of what PCA entails:
#
# Transforming the Data:
#
# PCA transforms a set of possibly correlated variables into a set of values that are linearly uncorrelated, known as principal components.
# These principal components are obtained by eigenvalue decomposition of a data covariance (or correlation) matrix or singular value decomposition of a data matrix.
#
# Dimensionality Reduction:
#
# Often, the first few principal components account for a significant portion of the variance in the dataset.
# By selecting these leading components, PCA reduces the dimensionality of the data. This simplification makes data analysis more manageable and can also help mitigate issues like the curse of dimensionality in machine learning models.
#
# Interpretation:
#
# Each principal component is a linear combination of the original variables.
# The first principal component accounts for the most variance, the second (orthogonal to the first) accounts for the second most, and so on.
#
# Applications:
#
# PCA is widely used for exploratory data analysis and for making predictive models.
# It is used extensively in areas such as image processing, finance, bioinformatics, and many other fields.
# 
# Visualization:
#
# In datasets with many variables, PCA is often used for visualization purposes. By reducing the data to two or three principal components, it can be plotted in a two or three-dimensional graph.
#
# Limitations:
#
# PCA assumes that the principal components with the highest variances are the most important, which might not always be the case.
# It's a linear method, which means it might not capture nonlinear relationships in the data.
# In summary, PCA is a powerful tool for reducing the complexity of data, highlighting the most influential patterns, and often serves as a prelude to other analyses, including clustering or building predictive models.

# This program will now include an elbow plot to help you visually determine the optimal number of clusters (K) for K-means clustering. After observing the elbow plot, you can choose the appropriate value of K (the point where the plot starts to bend, indicating diminishing returns by adding more clusters) and proceed with the clustering and visualization. Remember that the choice of K can significantly influence the clustering outcome, and it might require some experimentation and domain knowledge to select the most appropriate number.
# To include the elbow method for determining the optimal number of clusters in your K-means clustering program, you can add a step to calculate the within-cluster sum of squares (WSS) for a range of cluster numbers and plot these values. This will help you visually identify the "elbow," which is often considered a good choice for the number of clusters.

# Perform K-means clustering and determine the optimal number of clusters


# Add cluster names:
#  Add a step to assign meaningful names or IDs to the clusters after performing K-means clustering.
#
# De-scale the principal components:
#  Use the loadings from the PCA to understand which original variables are most influential for each principal component. This can help you interpret the principal components.
#
# Modify the plot:
#  Adjust the plot to display the cluster names and the influence of the original variables on the principal components.

set.seed(123)  # for reproducibility
wss <- sapply(1:10, function(k) {
  kmeans(data_scaled, centers = k, nstart = 10)$tot.withinss
})

# Elbow method plot
plot(1:10, wss, type = "b", pch = 19, frame = FALSE, 
     xlab = "Number of clusters K", 
     ylab = "Total within-clusters sum of squares")

####################### kmeans with PCA  representation ##########################

# Assuming the optimal number of clusters is chosen based on the elbow plot

optimal_k <- 3  # replace with the chosen number from the elbow plot
kmeans_result <- kmeans(data_scaled, centers = optimal_k, nstart = 10)

all_data$cluster <- NA  # Initialize the cluster column with NAs

# Assign clusters only to those rows that were included in the clustering process
all_data$cluster <- kmeans_result$cluster

cluster_names <- c("High Delay", "Moderate Delay", "Low Delay")

# Perform PCA and interpret the principal components
pca_result <- prcomp(data_scaled)
pca_data <- as.data.frame(pca_result$x)
pca_data$cluster <- factor(all_data$cluster)

# Get the loadings of the PCA to see the contribution of each variable to each PC
loadings <- abs(pca_result$rotation)
max_loading_indices <- apply(loadings[, 1:3], 2, which.max)

# Get the variable names corresponding to these indices
max_loading_names <- names(loadings)[max_loading_indices]

# Ensure column names are present
colnames(loadings) <- colnames(data_scaled)

# For each of the first three principal components, find the field with the highest absolute loading
for (i in 1:3) {
  pc_name <- paste("PC", i, sep = "")
  # Ordering the absolute values of loadings in descending order and getting the name of the top one
  max_loading_name <- names(sort(abs(loadings[, i]), decreasing = TRUE))[1]
  cat(sprintf("The field most related to %s is: %s\n", pc_name, max_loading_name))
}


# Sample data as pca_data is too large
set.seed(42) # for reproducibility
sampled_data <- pca_data[sample(nrow(pca_data), 50000), ] # adjust the number as needed

# Hex bin plot to replace overplotted points
ggplot(sampled_data, aes(x = PC1, y = PC2, color = cluster)) +
  geom_hex() +
  scale_color_manual(values = c("red", "purple", "green"), labels = cluster_names) +
  theme_minimal() +
  labs(title = "Cluster Visualization using PCA",
       x = "Flight Volume Component",
       y = "Weather Component",
       color = "Cluster")

# Another type of plot interactive using plotly

# Install and load the plotly package if you haven't already
if (!require("plotly")) install.packages("plotly")
library(plotly)

# Sample data if pca_data is too large
set.seed(42) # for reproducibility
sampled_data <- pca_data[sample(nrow(pca_data), 5000), ] # adjust the number as needed

# Convert the cluster factor to a character for plotly to use the custom labels correctly
sampled_data$cluster <- as.character(factor(sampled_data$cluster, labels = cluster_names))

# Create a 2D scatter plot using plotly
plot_ly(sampled_data, x = ~PC1, y = ~PC2, color = ~cluster, colors = c("red", "purple", "green"),
        marker = list(size = 10, opacity = 0.5), type = 'scatter', mode = 'markers') %>%
  layout(title = '2D Cluster Visualization using PCA',
         xaxis = list(title = 'PC1: Flight Volume Component'),
         yaxis = list(title = 'PC2: Weather Component'),
         colorway = c('red', 'purple', 'green'))



#====================================================================================================
#   Part 2: Prediction (Log regresion, Decission Tree, Gaussioan NB and MLPC Neural network)
#====================================================================================================

# Eliminate Unused columns 
df <- df %>%
  select(-c( DAY_OF_MONTH,OP_UNIQUE_CARRIER, AIRLINE_FLIGHTS_MONTH, TAIL_NUM,ORIGIN_AIRPORT_ID,OP_CARRIER_FL_NUM,ORIGIN,ORIGIN_CITY_NAME,DEST,DEST_CITY_NAME,DEP_TIME,DEP_DELAY_NEW,ARR_DELAY_NEW,CANCELLED,DATE,CARRIER_HISTORICAL,DEP_AIRPORT_HIST,ARR_AIRPORT_HIST,DAY_HISTORICAL,DEP_BLOCK_HIST, PREV_AIRPORT_HIST, TMAX_C))

# Print message
print("Dropped Cancelled & UnusedColumns ")

#Filter rows with column DEP_DEL15 == NA
df <- df %>% filter(!is.na(DEP_DEL15))

# Function to encode categorical data
clean_labels_encoder <- function(list_of_labels, df) {
  for (label in list_of_labels) {
    df[[label]] <- as.numeric(as.factor(df[[label]]))
  }
  return(df)
}

# List of categorical labels
list_of_labels <- c('CARRIER_NAME', 'DEPARTING_AIRPORT', 'PREVIOUS_AIRPORT', 'DEP_TIME_BLK')

# Apply the encoding
df <- clean_labels_encoder(list_of_labels, df)


# Calculate the mean of the TMAX column, excluding NA values
mean_TMAX <- mean(df$TMAX, na.rm = TRUE)

# Fill missing values in TMAX with its mean value
df <- df %>%
  mutate(TMAX = ifelse(is.na(TMAX), mean_TMAX, TMAX))

head (df)


# Fill NAs with 0
df <- df %>%
  mutate(across(where(is.numeric), ~ifelse(is.na(.), 0, .)))

# Basic summary of the dataset 
head (df)

# Identify non-numeric columns
non_numeric_cols <- names(df)[sapply(df, function(x) !is.numeric(x))]

# Convert non-numeric columns to numeric
df <- df %>% 
  mutate(across(all_of(non_numeric_cols), ~ if (is.factor(.) || is.character(.)) {
    as.numeric(as.character(.))
  } else {
    as.numeric(.)
  }))


#verify conversion
all(sapply(df, is.numeric))

head(df)

# Calculate the correlation matrix
correlation_matrix <- cor(df, use = "complete.obs")
 

# Plot the correlation matrix
corrplot(correlation_matrix, method = "circle")


# Melt the correlation matrix into a long format
melted_correlation <- melt(correlation_matrix)


# Create the heatmap
plot.new(); dev.off()
ggplot(melted_correlation, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1, 1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        axis.text.y = element_text(vjust = 1)) +
  labs(x = '', y = '', title = 'Correlation Matrix Heatmap')



# lets now calculate the correlations only with those flights with delay > 15 mins
dep_del15_correlations <- correlation_matrix["DEP_DEL15", ]

dep_del15_correlations

#Sort these correlation values and select the top 5. 

#Be aware that this will include the correlation of "DEP_DEL15" with itself, which is always 1. You might want to exclude it.

# Removing the correlation of "DEP_DEL15" with itself
dep_del15_correlations <- dep_del15_correlations[names(dep_del15_correlations) != "DEP_DEL15"]

# Convert correlations to absolute values
abs_correlations <- abs(dep_del15_correlations)

print (abs_correlations)

######## First Use 0.04  , later increase to value effect in accuracy improvement

selection_factor <- 0.04

# Selecting only columns where correlation is greater than 0.04
selected_correlations <- dep_del15_correlations[dep_del15_correlations > selection_factor]

# Getting the names of these columns
selected_columns <- names(selected_correlations)

print (selected_columns)

# Subset the data.table 'df' by these column names
X <- df[, ..selected_columns]

# Display the first few rows of X to verify
head(X)

y <- df[, "DEP_DEL15", drop = FALSE]


# Viewing the first few rows of X
head(X)

# Viewing the first few rows of y
head(y)

# Function to scale data using Min-Max Scaler
scale_data <- function(X) {
  preProcValues <- preProcess(X, method = c("range"))
  X_scaled <- predict(preProcValues, X)
  return(X_scaled)
}

# Assuming X is your data frame of features
X_scaled <- scale_data(X)
head(X_scaled)

# Convert to a data frame (if X_scaled is not already one) and view the first few rows
X_scaled <- as.data.frame(X_scaled)
head(X_scaled)

y <- as.data.frame(y)
y_vector <- y$DEP_DEL15

# Setting a seed for reproducibility
set.seed(42)

# Create indices for the training set using 'y_vector'
trainIndex <- createDataPartition(y_vector, p = 0.7, list = TRUE)

# Assuming 'X_scaled' is your feature matrix
# Split the data into training and testing sets using the indices
X_train <- X_scaled[trainIndex[[1]], ]
X_test <- X_scaled[-trainIndex[[1]], ]

y_train <- y$DEP_DEL15[trainIndex[[1]]]
y_test <- y$DEP_DEL15[-trainIndex[[1]]]

# Viewing the dimensions of X_train
dim(X_train)

# Viewing the dimensions of y_train
length(y_train)

# Count the number of occurrences of 0 and 1 in y_train
count_y_train_0 <- sum(y_train == 0, na.rm = TRUE)
count_y_train_1 <- sum(y_train == 1, na.rm = TRUE)

# Print the counts
cat("Count of y_train = 0:", count_y_train_0, "\n")
cat("Count of y_train = 1:", count_y_train_1, "\n")


#  target variable is binary (0 and 1)
y_train <- factor(y_train, levels = c(0, 1))
y_test <- factor(y_test, levels = c(0, 1))

all(levels(y_train) == levels(y_test))  # Should return TRUE

# Check dimensions
cat("Dimensions of X_train:", dim(X_train)[1], "\n")
cat("Length of y_train:", length(y_train), "\n")
cat("Dimensions of X_test:", dim(X_test)[1], "\n")
cat("Length of y_test:", length(y_test), "\n")

# Count the number of occurrences of 0 and 1 in y_train
count_y_train_0 <- sum(y_train == 0, na.rm = TRUE)
count_y_train_1 <- sum(y_train == 1, na.rm = TRUE)

# Print the counts
cat("Count of y_train = 0:", count_y_train_0, "\n")
cat("Count of y_train = 1:", count_y_train_1, "\n")

# Count the number of occurrences of 0 and 1 in y_test
count_y_test_0 <- sum(y_test == 0, na.rm = TRUE)
count_y_test_1 <- sum(y_test == 1, na.rm = TRUE)

# Print the counts
cat("Count of y_test = 0:", count_y_test_0, "\n")
cat("Count of y_test = 1:", count_y_test_1, "\n")

# Build a classification model using various supervised machine 
# learning models and check which model gives you the best accuracy

# use the following models
# 1. Logistic Regression
# 2. Decision Tree
# 3. GaussianNB
# 4. MLPClassifier

separator <- function(count = 50) {
  cat(rep('-', count), "\n")
}

library(doParallel)
numCores <- detectCores()
cl <- makeCluster(numCores)
registerDoParallel(cl)

# Function to sanitize factor levels
sanitize_factor_levels <- function(y) {
  levels(y) <- make.names(levels(y))
  return(y)
}

# Sanitize outcome variable factor levels
y_train <- factor(y_train)
y_test <- factor(y_test)
y_train <- sanitize_factor_levels(y_train)
y_test <- sanitize_factor_levels(y_test)


train_model_and_print_accuracy <- function(model, X_train, y_train, X_test, y_test) {
  set.seed(123)  # For reproducibility
  
  # Define train control with cross-validation
  cross_val_control <- trainControl(method = "cv", number = 5, allowParallel = TRUE, savePredictions = "final", classProbs = TRUE) # Example: 10-fold cross-validation
  
  # Specify the tuneGrid for each model
  if (model$method == "glm") {
    tune_grid <- expand.grid(.parameter = 1) # For glm no specific tuning needed
  } else if (model$method == "rpart") {
    tune_grid <- expand.grid(.cp = seq(0.01, 0.1, by = 0.01)) # Tuning complexity parameter for rpart
  } else if (model$method == "naive_bayes") {
    tune_grid <- expand.grid(laplace = c(0, 0.5, 1), usekernel = c(TRUE, FALSE), adjust = seq(0.5, 2, by = 0.5))
  } else if (model$method == "nnet") {
    #tune_grid <- expand.grid(.size = c(1, 3, 5), .decay = c(0, 0.1, 0.001)) # Tuning size and decay for nnet
    tune_grid <- expand.grid(.size = c(3, 5), .decay = c(0, 0.001)) # Tuning size and decay for nnet
    } else {
    tune_grid <- expand.grid(.dummy = 1) # Dummy grid for other models
  }

  # Train the model
  if (model$method == "glm") {
    fit <- train(x = X_train, y = y_train, method = model$method, family = model$family, trControl = cross_val_control, tuneGrid = tune_grid)
  } else if (model$method == "nnet") {
    fit <- train(x = X_train, y = y_train, method = "nnet",
            trControl = cross_val_control, tuneGrid = tune_grid, maxit = 50, linout = 1, MaxNWts = 1000) # reducing MAx iterations to 50 to speed up model
      }
  else {
    fit <- train(x = X_train, y = y_train, method = model$method, trControl = cross_val_control, tuneGrid = tune_grid)
  }
  
  # Predict on training and testing sets
  predictions_train <- predict(fit, X_train)
  predictions_test <- predict(fit, X_test)
  
  # Calculate accuracy
  accuracy_train <- sum(predictions_train == y_train) / length(y_train)
  accuracy_test <- sum(predictions_test == y_test) / length(y_test)
  
  # Print model name and accuracies
  model_name <- model$method
  cat("\033[1m  Unbalanced: ", model_name, "\033[0m\n")
  separator()
  cat("Train Accuracy for", model_name, ":", accuracy_train, "\n")
  separator()
  cat("Test Accuracy for", model_name, ":", accuracy_test, "\n")
  separator()
  
  # Confusion Matrix and Classification Report for Test data
  cm_test <- confusionMatrix(predictions_test, y_test)
  cat("Confusion Matrix for", model_name, "for test:\n")
  print(cm_test$table)
  separator()
  cat("Classification Report for", model_name, "for test:\n")
  print(cm_test$byClass)
  separator()
}

# Define models
log_reg <- list(method = "glm", family = "binomial")
dt <- list(method = "rpart")
gnb <- list(method = "naive_bayes")
mlp <- list(method = "nnet")
rf <- list(method = "rf")  # Random Forest

# Train and print accuracy for each model
models <- list(log_reg, dt, gnb, rf, mlp)
# Train and print accuracy for each model
#models <- list(mlp)

for (model in models) {
  train_model_and_print_accuracy(model, X_train, y_train, X_test, y_test)
}

#===============================================================================================
#  Repeat part 2 after using PCA and compare results,  did PCA improved result? 
#===============================================================================================

# Now use PCA to reduce the dimensionality of the data and
# retrain the models to see what impacts it has on your model in terms of accuracy.
# keep in mind that many times doing PCA can actually decrease the accuracy of your model
# but computation is much lighter and that's trade off you need to consider while build models in real life
# use PCA to reduce the dimensionality of the data
# Perform PCA

pca_result <- prcomp(X_scaled, scale. = TRUE)

# Determine the number of components to explain 95% of the variance
explained_variance <- summary(pca_result)$importance[3,]
cumulative_variance <- cumsum(explained_variance)
num_components_95 <- which(cumulative_variance >= 0.95)[1]

# Print the number of components
cat("Number of components to explain 95% of variance:", num_components_95, "\n")

# Extract the PCA scores (principal components)
x_pca <- pca_result$x[, 1:num_components_95]

# Setting a seed for reproducibility
set.seed(42)

# Create indices for the training set using 'y_vector'
trainIndex <- createDataPartition(y_vector, p = 0.7, list = TRUE)

# Assuming 'X_scaled' is your feature matrix
# Split the data into training and testing sets using the indices
X_train_PCA <- x_pca[trainIndex[[1]], ]
X_test_PCA <- x_pca[-trainIndex[[1]], ]


# Assuming you have a list 'models' containing the specifications of each model
# Example: models <- list(list(method = "glm"), list(method = "rpart"), ...)


for (model in models) {
  train_model_and_print_accuracy(model, X_train_PCA, y_train, X_test_PCA, y_test)
}

# End Part 2

#======================================================================================================
#  Part 3
# =====================================================================================================

#=====================================================================================================
#  Lets repeat all model improving balancing of DEP_DEL15 sample
# =====================================================================================================

df <- df_original


# Eliminate Unused columns 
df <- df %>%
  select(-c( DAY_OF_MONTH,OP_UNIQUE_CARRIER, AIRLINE_FLIGHTS_MONTH, TAIL_NUM,ORIGIN_AIRPORT_ID,OP_CARRIER_FL_NUM,ORIGIN,ORIGIN_CITY_NAME,DEST,DEST_CITY_NAME,DEP_TIME,DEP_DELAY_NEW,ARR_DELAY_NEW,CANCELLED,DATE,CARRIER_HISTORICAL,DEP_AIRPORT_HIST,ARR_AIRPORT_HIST,DAY_HISTORICAL,DEP_BLOCK_HIST, PREV_AIRPORT_HIST, TMAX_C))

# Print message
print("Dropped Cancelled & UnusedColumns ")

#Filter rows with column DEP_DEL15 == NA
df <- df %>% filter(!is.na(DEP_DEL15))

# Function to encode categorical data
clean_labels_encoder <- function(list_of_labels, df) {
  for (label in list_of_labels) {
    df[[label]] <- as.numeric(as.factor(df[[label]]))
  }
  return(df)
}

# List of categorical labels
list_of_labels <- c('CARRIER_NAME', 'DEPARTING_AIRPORT', 'PREVIOUS_AIRPORT', 'DEP_TIME_BLK')

# Apply the encoding
df <- clean_labels_encoder(list_of_labels, df)


# Calculate the mean of the TMAX column, excluding NA values
mean_TMAX <- mean(df$TMAX, na.rm = TRUE)

# Fill missing values in TMAX with its mean value
df <- df %>%
  mutate(TMAX = ifelse(is.na(TMAX), mean_TMAX, TMAX))

head (df)

# Fill NAs with 0
df <- df %>%
  mutate(across(where(is.numeric), ~ifelse(is.na(.), 0, .)))

# Basic summary of the dataset 
head (df)

# Identify non-numeric columns
non_numeric_cols <- names(df)[sapply(df, function(x) !is.numeric(x))]

# Convert non-numeric columns to numeric
df <- df %>% 
  mutate(across(all_of(non_numeric_cols), ~ if (is.factor(.) || is.character(.)) {
    as.numeric(as.character(.))
  } else {
    as.numeric(.)
  }))


#verify conversion
all(sapply(df, is.numeric))

head(df)

# Calculate the size of each group
size_0 <- nrow(df[df$DEP_DEL15 == 0, ])
size_1 <- nrow(df[df$DEP_DEL15 == 1, ])

# Use the minimum size for sampling
size_smaller_group <- min(size_0, size_1)

# Sample from the DEP_DEL15 == 0 group
sample_0 <- df %>%
  filter(DEP_DEL15 == 0) %>%
  slice_sample(n = size_smaller_group)

# Sample from the DEP_DEL15 == 1 group
sample_1 <- df %>%
  filter(DEP_DEL15 == 1) %>%
  slice_sample(n = size_smaller_group)

# Combine the two samples
sampled_df <- rbind(sample_0, sample_1)

df <- sampled_df
count_dep_del15_0 <- sum(sampled_df$DEP_DEL15 == 0)
count_dep_del15_1 <- sum(sampled_df$DEP_DEL15 == 1)

# Checking the new refined dataset
head(df)


# Identify non-numeric columns
non_numeric_cols <- names(df)[sapply(df, function(x) !is.numeric(x))]

# Convert non-numeric columns to numeric
df <- df %>% 
  mutate(across(all_of(non_numeric_cols), ~ if (is.factor(.) || is.character(.)) {
    as.numeric(as.character(.))
  } else {
    as.numeric(.)
  }))


#verify conversion
all(sapply(df, is.numeric))

head(df)

# Calculate the correlation matrix
correlation_matrix <- cor(df, use = "complete.obs")

# Show correlation matrix
correlation_matrix

# Melt the correlation matrix into a long format
melted_correlation <- melt(correlation_matrix)


# Create the heatmap for plotting  correlation matrix

plot.new(); dev.off()
ggplot(melted_correlation, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1, 1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        axis.text.y = element_text(vjust = 1)) +
  labs(x = '', y = '', title = 'Correlation Matrix Heatmap')

# Plot the correlation matrix (2)
corrplot(correlation_matrix, method = "circle")

# lets calculate the correlations only with those flights with delay > 15 mins
dep_del15_correlations <- correlation_matrix["DEP_DEL15", ]

dep_del15_correlations

#Sort these correlation values and select the top 5. 
#Be aware that this will include the correlation of "DEP_DEL15" with itself, which is always 1. You might want to exclude it.

# Removing the correlation of "DEP_DEL15" with itself
dep_del15_correlations <- dep_del15_correlations[names(dep_del15_correlations) != "DEP_DEL15"]

print (dep_del15_correlations)


# Convert correlations to absolute values
abs_correlations <- abs(dep_del15_correlations)

print (abs_correlations)

######## First Use 0.04  , later increase to value effect in accuracy improvement

selection_factor <- 0.04

# Selecting columns where correlation is greater than 0.04
selected_correlations <- dep_del15_correlations[dep_del15_correlations > selection_factor]

# Getting the names of these columns
selected_columns <- names(selected_correlations)

print (selected_columns)

# Subset the data.table 'df' by these column names
X <- df[, ..selected_columns]


# Display the first few rows of X to verify
head(X)

y <- df[, "DEP_DEL15", drop = FALSE]


# Viewing the first few rows of y
head(y)

# Function to scale data using Min-Max Scaler
scale_data <- function(X) {
  preProcValues <- preProcess(X, method = c("range"))
  X_scaled <- predict(preProcValues, X)
  return(X_scaled)
}

# Assuming X is your data frame of features
X_scaled <- scale_data(X)
head(X_scaled)

# Convert to a data frame (if X_scaled is not already one) and view the first few rows
X_scaled <- as.data.frame(X_scaled)
head(X_scaled)

y <- as.data.frame(y)
y_vector <- y$DEP_DEL15

# Setting a seed for reproducibility
set.seed(42)

# Create indices for the training set using 'y_vector'
trainIndex <- createDataPartition(y_vector, p = 0.7, list = TRUE)

# Assuming 'X_scaled' is your feature matrix
# Split the data into training and testing sets using the indices
X_train <- X_scaled[trainIndex[[1]], ]
X_test <- X_scaled[-trainIndex[[1]], ]

y_train <- y$DEP_DEL15[trainIndex[[1]]]
y_test <- y$DEP_DEL15[-trainIndex[[1]]]

# Viewing the dimensions of X_train
dim(X_train)

# Viewing the dimensions of y_train
length(y_train)

# Count the number of occurrences of 0 and 1 in y_train
count_y_train_0 <- sum(y_train == 0, na.rm = TRUE)
count_y_train_1 <- sum(y_train == 1, na.rm = TRUE)

# Print the counts
cat("Count of y_train = 0:", count_y_train_0, "\n")
cat("Count of y_train = 1:", count_y_train_1, "\n")

# Assuming your target variable is binary (0 and 1)
y_train <- factor(y_train, levels = c(0, 1))
y_test <- factor(y_test, levels = c(0, 1))

all(levels(y_train) == levels(y_test))  # Should return TRUE

# Check dimensions
cat("Dimensions of X_train:", dim(X_train)[1], "\n")
cat("Length of y_train:", length(y_train), "\n")
cat("Dimensions of X_test:", dim(X_test)[1], "\n")
cat("Length of y_test:", length(y_test), "\n")

# Count the number of occurrences of 0 and 1 in y_train
count_y_train_0 <- sum(y_train == 0, na.rm = TRUE)
count_y_train_1 <- sum(y_train == 1, na.rm = TRUE)

# Print the counts
cat("Count of y_train = 0:", count_y_train_0, "\n")
cat("Count of y_train = 1:", count_y_train_1, "\n")

# Count the number of occurrences of 0 and 1 in y_test
count_y_test_0 <- sum(y_test == 0, na.rm = TRUE)
count_y_test_1 <- sum(y_test == 1, na.rm = TRUE)

# Print the counts
cat("Count of y_test = 0:", count_y_test_0, "\n")
cat("Count of y_test = 1:", count_y_test_1, "\n")


# Build a classification model using various supervised machine 
# learning models and check which model gives you the best accuracy

# use the following models
# 1. Logistic Regression
# 2. Decision Tree
# 3. GaussianNB
# 4. MLPClassifier

# Assuming necessary libraries are loaded

library(doParallel)
numCores <- detectCores()
cl <- makeCluster(numCores)
registerDoParallel(cl)

# Function to sanitize factor levels
#sanitize_factor_levels <- function(y) {
#  levels(y) <- make.names(levels(y))
#  return(y)
#}

# Sanitize outcome variable factor levels
#y_train <- factor(y_train)
#y_test <- factor(y_test)
#y_train <- sanitize_factor_levels(y_train)
#y_test <- sanitize_factor_levels(y_test)



separator <- function(count = 30) {
  cat(rep('-', count), "\n")
}

train_model_and_print_accuracy <- function(model, X_train, y_train, X_test, y_test) {
  set.seed(123)  # For reproducibility
  
  # Define train control with cross-validation
  cross_val_control <- trainControl(method = "cv", number = 10, allowParallel = TRUE) # Example: 10-fold cross-validation

  # Specify the tuneGrid for each model
  if (model$method == "glm") {
    tune_grid <- expand.grid(.parameter = 1) # For glm no specific tuning needed
  } else if (model$method == "rpart") {
    tune_grid <- expand.grid(.cp = seq(0.01, 0.1, by = 0.01)) # Tuning complexity parameter for rpart
  } else if (model$method == "naive_bayes") {
    tune_grid <- expand.grid(laplace = c(0, 0.5, 1), usekernel = c(TRUE, FALSE), adjust = seq(0.5, 2, by = 0.5))
  } else if (model$method == "nnet") {
    tune_grid <- expand.grid(.size = c(1, 3, 5), .decay = c(0, 0.1, 0.001)) # Tuning size and decay for nnet
  } else {
    tune_grid <- expand.grid(.dummy = 1) # Dummy grid for other models
  }
  
  # Train the model
  if (model$method == "glm") {
    fit <- train(x = X_train, y = y_train, method = model$method, family = model$family, trControl = cross_val_control, tuneGrid = tune_grid)
  } else if (model$method == "nnet") {
    fit <- train(x = X_train, y = y_train, method = "nnet",
                 trControl = cross_val_control, tuneGrid = tune_grid, maxit = 100, MaxNWts = 1000) # reducing MAx iterations to 50 to speed up model
  }
  else {
    fit <- train(x = X_train, y = y_train, method = model$method, trControl = cross_val_control, tuneGrid = tune_grid)
  }
  
 
  # Predict on training and testing sets
  predictions_train <- predict(fit, X_train)
  predictions_test <- predict(fit, X_test)
  
  # Calculate accuracy
  accuracy_train <- sum(predictions_train == y_train) / length(y_train)
  accuracy_test <- sum(predictions_test == y_test) / length(y_test)
  
  # Print model name and accuracies
  model_name <- model$method
  cat("\033[1m  balanced 50-50: ", model_name, "\033[0m\n")
   separator()
  cat("Train Accuracy for", model_name, ":", accuracy_train, "\n")
  separator()
  cat("Test Accuracy for", model_name, ":", accuracy_test, "\n")
  separator()
  
  # Confusion Matrix and Classification Report for Test data
  cm_test <- confusionMatrix(predictions_test, y_test)
  cat("Confusion Matrix for", model_name, "for test:\n")
  print(cm_test$table)
  separator()
  cat("Classification Report for", model_name, "for test:\n")
  print(cm_test$byClass)
  separator()
}

# Define models
log_reg <- list(method = "glm", family = "binomial") # logistic regresion
dt <- list(method = "rpart")                        # decision tree
gnb <- list(method = "naive_bayes") # Gaussian Naive Bayes
mlp <- list(method = "nnet") # Neural Network  
rf <- list(method = "rf")  # Random Forest

# Train and print accuracy for each model
models <- list(log_reg, dt, gnb,rf, mlp)
#models <- list(rf)
for (model in models) {
  train_model_and_print_accuracy(model, X_train, y_train, X_test, y_test)
}




# =======================================================================================
# Lets use PCA and compare results.  Did PCA improved accuracy, precissin and recall?
#========================================================================================

# Now use PCA to reduce the dimensionality of the data and
# retrain the models to see what impacts it has on your model in terms of accuracy.
# keep in mind that many times doing PCA can actually decrease the accuracy of your model
# but computation is much lighter and that's trade off you need to consider while build models in real life

# use PCA to reduce the dimensionality of the data

# Perform PCA

pca_result <- prcomp(X_scaled, scale. = TRUE)

# Determine the number of components to explain 95% of the variance
explained_variance <- summary(pca_result)$importance[3,]
cumulative_variance <- cumsum(explained_variance)
num_components_95 <- which(cumulative_variance >= 0.95)[1]

# Print the number of components
cat("Number of components to explain 95% of variance:", num_components_95, "\n")

# Compute the rotation matrix
rotation_matrix <- pca_result$rotation

# Each column of the rotation matrix corresponds to a principal component,
# and each row corresponds to the original variables.
# The value at rotation_matrix[i, j] indicates the contribution of the i-th
# original variable to the j-th principal component.

# To identify which original variables contribute most to the first few principal components:
contributions <- abs(rotation_matrix[, 1:num_components_95])

# For each of the selected principal components, find the original variable that contributes the most
max_contributions <- apply(contributions, 2, which.max)

# Get the names of these variables from the original dataset
column_names <- colnames(X_scaled)[max_contributions]

# Print the names of the original variables that contribute the most to each of the principal components
print(column_names)


#================================================================================================
# Now , once understood how PCA works, lets perform the PCA and re-runcalculate the Fitting models
#================================================================================================

# Extract the PCA scores (principal components)
x_pca <- pca_result$x[, 1:num_components_95]

# Setting a seed for reproducibility
set.seed(42)

# Create indices for the training set using 'y_vector'
trainIndex <- createDataPartition(y_vector, p = 0.7, list = TRUE)

# Assuming 'X_scaled' is your feature matrix
# Split the data into training and testing sets using the indices
X_train_PCA <- x_pca[trainIndex[[1]], ]
X_test_PCA <- x_pca[-trainIndex[[1]], ]


for (model in models) {
  train_model_and_print_accuracy(model, X_train_PCA, y_train, X_test_PCA, y_test)
}

# Better before or after PCA??





