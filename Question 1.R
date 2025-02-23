# Load necessary libraries
library(readxl)  # reading excel spreadsheet
library(dplyr)    

# Load the data
biomarkers <- read_excel("biomarkers.xlsx", sheet = "NPX (Page 2)")
covariates <- read_excel("covariates.xlsx", sheet = "Ark1")

# Extract PatientID and Time
biomarkers <- biomarkers %>%
  mutate(PatientID = as.numeric(sub("-.*", "", Biomarker)),
         Time = sub(".*-", "", Biomarker)) %>%
  filter(Time == "0weeks")  # Keep only inclusion data

# Merge with covariates
merged_data <- merge(biomarkers, covariates, by = "PatientID")

# Rename the sex column for clarity
colnames(merged_data)[which(names(merged_data) == "Sex (1=male, 2=female)")] <- "Sex"

# List of biomarkers
biomarkers_list <- c("IL-8", "VEGF-A", "OPG", "TGF-beta-1", "IL-6", "CXCL9", "CXCL1", "IL-18", "CSF-1")

# Initialize results dataframe
results <- data.frame(Biomarker = character(),
                      T_test_p_value = numeric(),
                      #Mann_Whitney_p_value = numeric(),
                      stringsAsFactors = FALSE)

# Loop through each biomarker
for (biomarker in biomarkers_list) {
  male_values <- merged_data %>% filter(Sex == 1) %>% pull(!!sym(biomarker))
  female_values <- merged_data %>% filter(Sex == 2) %>% pull(!!sym(biomarker))
  
  # Perform independent t-test (Welch's t-test: unequal variance)
  t_test <- t.test(male_values, female_values, var.equal = FALSE)
  
  # Perform Mann-Whitney U test (Wilcoxon rank-sum test) if distribution is not normal
  #mw_test <- wilcox.test(male_values, female_values, alternative = "two.sided")
  
  # Store results
  results <- rbind(results, data.frame(Biomarker = biomarker,
                                       T_test_p_value = t_test$p.value))
                                       #Mann_Whitney_p_value = mw_test$p.value))
}

# Print results
print(results)

# Generate Q-Q plots for each biomarker
par(mfrow = c(3, 3))  # Arrange plots in a 3x3 grid

for (biomarker in biomarkers_list) {
  values <- merged_data %>% pull(!!sym(biomarker))  # Extract biomarker values
  
  # Q-Q plot
  qqnorm(values, main = paste("Q-Q Plot for", biomarker), pch = 20)
  qqline(values, col = "red", lwd = 2)
}

# Reset plot layout
par(mfrow = c(1, 1))



# Define parameters
alpha <- 0.05  # Significance level
n_tests <- 9   # Number of hypothesis tests

# Calculate probability of at least one Type I error
prob_type1_error <- 1 - (1 - alpha)^n_tests

# Print result
print(paste("Probability of at least one Type I error:", round(prob_type1_error, 8)))

# Number of tests
n_tests <- length(biomarkers_list)

# Adjusted alpha using Bonferroni correction
alpha <- 0.05
alpha_corrected <- alpha / n_tests

# Initialize results dataframe
results <- data.frame(Biomarker = character(),
                      T_test_p_value = numeric(),
                      #Mann_Whitney_p_value = numeric(),
                      Bonferroni_Significant = logical(),
                      stringsAsFactors = FALSE)

# Loop through each biomarker
for (biomarker in biomarkers_list) {
  male_values <- merged_data %>% filter(Sex == 1) %>% pull(!!sym(biomarker))
  female_values <- merged_data %>% filter(Sex == 2) %>% pull(!!sym(biomarker))
  
  # Perform independent t-test
  t_test <- t.test(male_values, female_values, var.equal = FALSE)
  
  # Perform Mann-Whitney U test (Wilcoxon rank-sum test), use if dataset not normally distributed
  #mw_test <- wilcox.test(male_values, female_values, alternative = "two.sided")
  
  # Check if significant after Bonferroni correction
  bonferroni_significant <- (t_test$p.value < alpha_corrected) | (mw_test$p.value < alpha_corrected)
  
  # Store results
  results <- rbind(results, data.frame(Biomarker = biomarker,
                                       T_test_p_value = t_test$p.value,
                                       #Mann_Whitney_p_value = mw_test$p.value,
                                       Bonferroni_Significant = bonferroni_significant))
}

# Print results
print(results)

# Print the Bonferroni-corrected significance threshold
print(paste("Bonferroni-corrected alpha:", round(alpha_corrected, 8)))


