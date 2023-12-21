library(tidyverse)
library(caret)
install.packages("fastDummies")
library(fastDummies)

# Load the dataset
Dataset <- read_csv("owid-covid-data.csv", col_types = cols())

# Check data types and values
summary(Dataset)

# Plotting frame
par(mfrow=c(1,3))

# Plot categorical variable
barplot(table(Dataset$continent), main="Continent Distribution", col="lightblue", xlab="Continent", ylab="Count")

# Plot discrete variable
hist(Dataset$total_cases, main="Total cases", col="lightgreen", xlab="Total Cases", ylab="Frequency")

# Plot continuous variable
boxplot(Dataset$gdp_per_capita, main="GDP per Capita", col="lightcoral", ylab="GDP per Capita")

# Missing values
miss <- sapply(Dataset, function(x) sum(is.na(x)))
barplot(miss, main="Missing Values", col="lightpink", xlab="Variables", ylab="Count")

# Select numerical variables
numerical_vars <- select_if(Dataset, is.numeric)

# Calculate statistical parameters of the numerical variables
summary_stats <- summary(numerical_vars)
print(summary_stats)

# Min-Max Normalization
min_max_scaled_data <- numerical_vars %>% 
  mutate_all(~(.-min(.))/(max(.)-min(.)))

# Z-score Standardization
z_score_scaled_data <- numerical_vars %>% 
  mutate_all(~(.-mean(.))/sd(.))

# Robust Scaling
robust_scaled_data <- numerical_vars %>% 
  mutate_all(~(.-median(., na.rm = TRUE))/IQR(., na.rm = TRUE))

summary_stats_after_min_max <- summary(min_max_scaled_data)
summary_stats_after_z_score <- summary(z_score_scaled_data)
summary_stats_after_robust <- summary(robust_scaled_data)

print("Summary Statistics After Min-Max Scaling:")
print(summary_stats_after_min_max)

print("Summary Statistics After Z-score Standardization:")
print(summary_stats_after_z_score)

print("Summary Statistics After Robust Scaling:")
print(summary_stats_after_robust)


# Identify variables with zero standard deviation
vars_with_zero_sd <- names(numerical_vars)[apply(numerical_vars, 2, sd) == 0]

# Eliminate variables with zero standard deviation
numerical_vars_filtered <- numerical_vars[, !(names(numerical_vars) %in% vars_with_zero_sd)]

# Identify and eliminate constant variables
vars_with_constant_values <- names(numerical_vars_filtered)[apply(numerical_vars_filtered, 2, function(x) length(unique(x)) == 1)]
numerical_vars_filtered <- numerical_vars_filtered[, !(names(numerical_vars_filtered) %in% vars_with_constant_values)]

# Correlation matrix
cor_matrix <- cor(numerical_vars_filtered, use = "pairwise.complete.obs")

# Replace values NA to zeros
cor_matrix[is.na(cor_matrix)] <- 0
heatmap(cor_matrix, main="Correlation", col=heat.colors(10))

# Replace values NA to zeros
Dataset$total_cases[is.na(Dataset$total_cases)] <- 0

# Convert 'date' column to date format
Dataset$date <- as.Date(Dataset$date)

# Sort data by date
Dataset <- Dataset[order(Dataset$date), ]
plot(Dataset$date, Dataset$total_deaths, type="l", col="blue", xlab="Date", ylab="Total Deaths", main="Total Deaths Over Time")

# GDP per Capita by deaths
plot(Dataset$gdp_per_capita, Dataset$total_deaths, col="green", xlab="GDP per Capita", ylab="Total Deaths", main="GDP per Capita")

# Continent analysis
continent_summary <- Dataset %>% 
  group_by(continent) %>% 
  summarise(mean_cases = mean(total_cases), mean_deaths = mean(total_deaths))

# Mean Total Cases by Continent
barplot(continent_summary$mean_cases, names.arg = continent_summary$continent, col="skyblue", main="Mean Total Cases by Continent", ylab="Mean Total Cases")

# Total Cases by Continent
boxplot(total_cases ~ continent, data = Dataset, col="lightgreen", main="Total Cases by Continent", ylab="Total Cases")

# Total Deaths regression line
plot(gdp_per_capita ~ total_deaths, data = Dataset, col="yellow", xlab="GDP per Capita", ylab="Total Deaths", main="Regression line")
abline(lm(total_deaths ~ gdp_per_capita, data = Dataset), col="red")

# Vaccine analysis
vaccine_gender_vars <- select(Dataset, continent, location, people_vaccinated, female_smokers, male_smokers)

# Total People Vaccinated by Continent
ggplot(vaccine_gender_vars, aes(x = continent, y = people_vaccinated, fill = continent)) +
  geom_bar(stat = "identity") +
  labs(title = "Total People Vaccinated by Continent",
       x = "Continent",
       y = "Total People Vaccinated") +
  theme_minimal()

# Female Smokers vs. Male Smokers
ggplot(vaccine_gender_vars, aes(x = female_smokers, y = male_smokers, color = continent)) +
  geom_point() +
  labs(title = "Female vs. Male Smokers by Continent",
       x = "Female Smokers",
       y = "Male Smokers") +
  theme_minimal()

# Continent dummy encoding 
dummy_dataset <- Dataset %>% 
  select(-continent) %>% 
  dummy_cols()

head(dummy_dataset)

# Select only numeric columns for Principal Component Analysis
features_for_pca <- select_if(Dataset, is.numeric)

# Remove columns with zero or constant variance
features_for_pca <- features_for_pca[, apply(features_for_pca, 2, function(x) length(unique(x[!is.na(x)])) > 1)]

# Impute missing values with the column mean
features_for_pca <- apply(features_for_pca, 2, function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x))

#Apply PCA with centering and scaling
pca_result <- prcomp(features_for_pca, center = TRUE, scale. = TRUE)

# Transform the original data using PCA
pca_data <- predict(pca_result, features_for_pca)

summary(pca_result)
head(pca_data)