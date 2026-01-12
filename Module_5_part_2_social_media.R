# Load necessary libraries
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("reshape2")) install.packages("reshape2")
if (!require("corrplot")) install.packages("corrplot")

library(tidyverse)
library(reshape2)
library(corrplot)

rm(list = ls())

# Set working directory
setwd("C:/Users/JoanCarles/iCloudDrive/Documents/_Universitats/_Bachelors/Salle/data-science/Moduls/Module 5/Case/")

# Load datasets
flights_data <- read_csv("train_val.csv", show_col_types = FALSE)
satisfaction_data <- read_csv("satisfaction_P5.csv", show_col_types = FALSE)

# Calculate average satisfaction
satisfaction_avg <- satisfaction_data %>%
  group_by(CARRIER, OP_CARRIER, FL_NUM, TAIL_NUM, MONTH, DAY_OF_MONTH) %>%
  summarise(AVG_SATISFACTION = mean(Satisfaction, na.rm = TRUE), .groups = 'drop')

# Preprocess and merge datasets on common columns
merged_data <- flights_data %>%
  select(-DATE) %>%
  rename(
    OP_CARRIER = OP_UNIQUE_CARRIER,
    FL_NUM = OP_CARRIER_FL_NUM
  ) %>%
  merge(satisfaction_avg, by = c("OP_CARRIER", "FL_NUM", "TAIL_NUM", "MONTH", "DAY_OF_MONTH")) %>%
  filter(!is.na(AVG_SATISFACTION))

# Optimize NA replacement and data type conversion
mean_TMAX <- mean(as.numeric(merged_data$TMAX), na.rm = TRUE)
mean_TMAX_C <- mean(as.numeric(merged_data$TMAX_C), na.rm = TRUE)

merged_data <- merged_data %>%
  mutate(TMAX = ifelse(is.na(TMAX), mean_TMAX, as.numeric(TMAX)),
         TMAX_C = ifelse(is.na(TMAX_C), mean_TMAX_C, as.numeric(TMAX_C)),
         PRCP = replace_na(as.numeric(PRCP), 0),
         AWND = replace_na(as.numeric(AWND), 0),
         SNOW = replace_na(as.numeric(SNOW), 0),
         SNWD = replace_na(as.numeric(SNWD), 0),
         CANCELLED = as.numeric(CANCELLED),
         DEP_DEL15 = as.numeric(DEP_DEL15),
         across(where(is.character), ~as.numeric(as.factor(.))))

# Proportion of missing values in ARR_DELAY_NEW
mean(is.na(merged_data$ARR_DELAY_NEW))

merged_data$ARR_DELAY_NEW <- ifelse(is.na(merged_data$ARR_DELAY_NEW), mean(merged_data$ARR_DELAY_NEW, na.rm = TRUE), merged_data$ARR_DELAY_NEW)

# Compute the correlation matrix on the filtered data
correlation_matrix <- cor(merged_data, use = "complete.obs")

# Create the correlation plot
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

# Open a PNG graphics device
png("correlation_plot.png", width = 1600, height = 1600) # Specify the dimensions as required

# Create the correlation plot
corrplot(correlation_matrix, method = "circle")

# Close the graphics device
dev.off()

# You can then proceed with your correlation analysis or visualization
corrplot(correlation_matrix, method = "circle")

#=============================================================================================================

# Now, Lets further study the correlation of satisfaction avg with the cancelled flights


# Step 1: Data Preparation
cancelled_data <- merged_data %>%
  filter(CANCELLED == 1) %>%
  select(AVG_SATISFACTION)

# Calculate the mean satisfaction score for cancelled flights
mean_satisfaction_cancelled <- mean(cancelled_data$AVG_SATISFACTION, na.rm = TRUE)

# Step 2: Statistical Analysis
# Perform a t-test to compare satisfaction scores between cancelled and non-cancelled flights
t_test_result <- t.test(merged_data$AVG_SATISFACTION ~ merged_data$CANCELLED)

# Step 3: Probability Estimation
# Assuming low satisfaction is defined as scores below a certain threshold (e.g., below 3 on a scale of 5)
low_satisfaction_threshold <- 3
prob_low_satisfaction_cancelled <- mean(cancelled_data$AVG_SATISFACTION < low_satisfaction_threshold, na.rm = TRUE)

# Output the results
print(paste("Mean Satisfaction for Cancelled Flights:", mean_satisfaction_cancelled))
print(t_test_result)
print(paste("Probability of Low Satisfaction for Cancelled Flights:", prob_low_satisfaction_cancelled))





