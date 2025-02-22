# Load necessary libraries
library(readxl) # For data splitting
library(dplyr)
library(caret)  # For data splitting
library(tidyr)  # Load tidyr for drop_na()
library(Metrics)  # Load package for RMSE & MAE

# Load the datasets
biomarkers <- read_excel("biomarkers.xlsx")
covariates <- read_excel("covariates.xlsx")

# Extract PatientID and Time
biomarkers <- biomarkers %>%
  mutate(PatientID = as.numeric(sub("-.*", "", Biomarker)),
         Time = sub(".*-", "", Biomarker)) %>%
  filter(Time == "0weeks")  # Keep only inclusion data

# Merge with covariates
merged_data <- merge(biomarkers, covariates, by = "PatientID")

# Rename relevant columns
colnames(merged_data)[which(names(merged_data) == "Sex (1=male, 2=female)")] <- "Sex"
colnames(merged_data)[which(names(merged_data) == "Smoker (1=yes, 2=no)")] <- "Smoker"
colnames(merged_data)[which(names(merged_data) == "Vas-12months")] <- "VAS_12M"

# Select relevant variables (biomarkers + covariates + response variable)
biomarkers_list <- c("IL-8", "VEGF-A", "OPG", "TGF-beta-1", "IL-6", "CXCL9", "CXCL1", "IL-18", "CSF-1")
covariates_list <- c("Age", "Sex", "Smoker", "VAS-at-inclusion")

# Keep only selected columns
final_data <- merged_data %>% select(PatientID, VAS_12M,all_of(biomarkers_list), all_of(covariates_list))

# Remove rows with missing VAS_12M values
final_data <- final_data %>% drop_na(VAS_12M)

# Split data into 80% training and 20% testing
set.seed(123)  # For reproducibility
train_index <- createDataPartition(final_data$VAS_12M, p = 0.8, list = FALSE)
train_data <- final_data[train_index, ]
test_data <- final_data[-train_index, ]

# Fit regression model using training data
model <- lm(VAS_12M ~ ., data = train_data %>% select(-PatientID))

# Print summary of model
summary(model)

# Q-Q plot for residuals
plot(model, which = 2)  

# Residuals vs. Fitted Values Plot
par(mfrow = c(1, 2))  # Split plot area into 2 sections
plot(model$fitted.values, resid(model), 
     main = "Residuals vs Fitted Values",
     xlab = "Fitted Values",
     ylab = "Residuals",
     pch = 16, col = "blue")
abline(h = 0, col = "red", lwd = 2, lty = 2)  # Add reference line

# Predict VAS_12M for the test dataset
test_data$predicted_VAS <- predict(model, newdata = test_data)

# Compute error metrics
# Mean Absolute Error (MAE)
mae_value <- mae(test_data$VAS_12M, test_data$predicted_VAS)
# Root Mean Squared Error (RMSE)
rmse_value <- rmse(test_data$VAS_12M, test_data$predicted_VAS)

# Compute R-squared for test data
rss <- sum((test_data$VAS_12M - test_data$predicted_VAS)^2)  # Residual Sum of Squares
tss <- sum((test_data$VAS_12M - mean(test_data$VAS_12M))^2)  # Total Sum of Squares
r_squared_test <- 1 - (rss / tss)

# Print performance metrics
cat("Mean Absolute Error (MAE):", mae_value, "\n")
cat("Root Mean Squared Error (RMSE):", rmse_value, "\n")
cat("R-squared:", r_squared_test, "\n")






