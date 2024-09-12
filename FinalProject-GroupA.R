# Data Import
# Use the readr library to read data from a CSV file.
library(readr)

# Path to the CSV file (replace "path/dataset.csv" with the actual location)
file_path <- "TSOM\\MOD2\\Final Project\\BurgundySip.csv"

# Import dataset
wine_data <- read_csv(file_path)

# Verification of the first few rows of the dataset to ensure correct import
head(wine_data)
summary(wine_data)
str(wine_data)

# Analysis of Variables
# Summary of variables
library(dplyr)

# Basic statistical summary
summary(wine_data)

# Initial. Visualization. 
library(ggplot2)

# Histograms for the variable Price (PR) and Body Score (BD)
ggplot(wine_data, aes(x = PR)) + 
  geom_histogram(binwidth = 5, fill = "blue", color = "black") + 
  labs(title = "Wine Price Distribution", x = "Price (€)", y = "Frequency")

ggplot(wine_data, aes(x = BD)) + 
  geom_histogram(binwidth = 0.5, fill = "green", color = "black") + 
  labs(title = "BD Distribution", x = "BD", y = "Frequency")

# Scatter plot to observe the relationship between price (PR) and acidity (ACD).
ggplot(wine_data, aes(x = ACD, y = PR)) + 
  geom_point(color = "red") + 
  labs(title = "relationship between price (PR) and acidity (ACD)", x = "Acidity", y = "Price (€)")

# Structural integrity check of the data set
# Checking for null or missing values
missing_values <- colSums(is.na(wine_data))
print(missing_values)

# Checking of data types for each column and conversions if needed
str(wine_data)

# Example of converting a numeric variable to a categorical variable
wine_data$REG <- as.factor(wine_data$REG)

# Treatment of Duplicate Entries
# Identification of duplicate rows based on all columns
duplicated_rows <- wine_data[duplicated(wine_data), ]
print(duplicated_rows)

# Removal of duplicate entries if necessary
wine_data <- wine_data %>% distinct()

# Verification of the elimination of duplicates
dim(wine_data)

# Treatment of Outliers and Missing Values
# Strategy: replace missing values with the median or mean, as appropriate

# Replacement of missing values in the PR column with the median
wine_data$PR[is.na(wine_data$PR)] <- median(wine_data$PR, na.rm = TRUE)

# Identification of outliers in the Price (PR) variable using the IQR method
Q1 <- quantile(wine_data$PR, 0.25)
Q3 <- quantile(wine_data$PR, 0.75)
IQR <- Q3 - Q1

# Definition of lower and upper limits
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

# Identification of outliers
outliers <- wine_data$PR[wine_data$PR < lower_bound | wine_data$PR > upper_bound]
print(outliers)

# Treatment of outliers: remove or adjust these values according to the context
# For example, here we simply remove them
wine_data <- wine_data %>% filter(PR >= lower_bound & PR <= upper_bound)

# Final verification of the data set
summary(wine_data)
str(wine_data)

# Descriptive analysis

# 1. Price distribution (PR) of wines
ggplot(wine_data, aes(x = PR)) + 
  geom_histogram(binwidth = 5, fill = "blue", color = "black") + 
  labs(title = "Price distribution (PR) of wines", x = "Price (€)", y = "Frequency")

# 2. Price variation (PR) by region (REG)
ggplot(wine_data, aes(x = REG, y = PR)) + 
  geom_boxplot(fill = "orange") + 
  labs(title = "Price variation (PR) by region (REG)", x = "Region", y = "Price (€)")

# 3. Relationship between body score (BD) and acidity (ACD) of the wines
ggplot(wine_data, aes(x = BD, y = ACD)) + 
  geom_point(color = "red") + 
  labs(title = "Body score (BD) and acidity (ACD)", x = "Body Score (BD)", y = "Acidity (ACD)")

# 4. Year of (YR) associated with the most expensive wines
ggplot(wine_data, aes(x = YR, y = PR)) + 
  geom_boxplot(fill = "green") + 
  labs(title = "Price and by Harvest Year", x = "Year (YR)", y = "Price (€)")

# 5. Trend of prices according to alcohol percentage (AL)
ggplot(wine_data, aes(x = AL, y = PR)) + 
  geom_point(color = "purple") + 
  geom_smooth(method = "lm", color = "blue") + 
  labs(title = "Trend of prices according to alcohol percentage (AL)", x = "Percentage of Alcohol (AL)", y = "Price (€)")

# 6. Year of harvest (YR) associated with the body score
ggplot(wine_data, aes(x = YR, y = BD)) + 
  geom_boxplot(fill = "green") + 
  labs(title = "Body score by Year", x = "Year (YR)", y = "Body score")

# 7. Year of harvest (YR) associated with acidity
ggplot(wine_data, aes(x = YR, y = ACD)) + 
  geom_boxplot(fill = "green") + 
  labs(title = "Acidity by Year", x = "Year (YR)", y = "Acidity")

# 8. Price trend according to wine valuation
ggplot(wine_data, aes(x = RT, y = PR)) + 
  geom_point(color = "purple") + 
  geom_smooth(method = "lm", color = "blue") + 
  labs(title = "Price trend according to wine valuation", x = "Rating", y = "Price (€)")

# Predictive analytics
# Necessary libraries
library(caret)
library(ggplot2)
library(effects)

# 1. Predicting wine price (PR) as a function of vintage year (YR), region (REG) and wine variety (TP)
model1 <- lm(PR ~ YR + REG + TP, data = wine_data)
summary(model1)

# Marginal effects to visualize model 1
plot(allEffects(model1), main = "Marginal effects to visualize model 1")


# 2. Regression model to predict average rating (RT) 
# as a function of body (BD), acidity (ACD), and alcohol (AL).
model2 <- lm(RT ~ BD + ACD + AL, data = wine_data)
summary(model2)

# Marginal effects to visualize model 2
plot(allEffects(model2), main = "Marginal effects to visualize model 2")


# 3. Model for estimating the number of reviewers (NUMR) based on the characteristics of the wine
model3 <- lm(NUMR ~ BD + ACD + AL + PR, data = wine_data)
summary(model3)

# Marginal effects to visualize the model 3
plot(allEffects(model3), main = "Marginal effects to visualize the model 3")

model4 <- lm(PR ~ RT + ACD + BD, data = wine_data)
summary(model4)
plot(allEffects(model4), main = "Marginal effects to visualize the model 4")

model4 <- lm(PR ~ RT + ACD + BD + AL, data = wine_data)
summary(model4)
plot(allEffects(model4), main = "Marginal effects to visualize the model 4")

model5 <- lm(PR ~ RT + ACD + BD + AL + YR, data = wine_data)
summary(model5)
plot(allEffects(model5), main = "Marginal effects to visualize the model 5")

model6 <- lm(RT ~ PR + ACD + BD + AL + YR, data = wine_data)
summary(model6)
plot(allEffects(model6), main = "Marginal effects to visualize the model 6")

# Residuals vs. fitted values for model 1
ggplot(model1, aes(.fitted, .resid)) + 
  geom_point() + 
  geom_smooth(method = "loess", se = FALSE) + 
  labs(title = "Residuals vs Adjusted Values - Model 1", x = "Adjusted Values", y = "Residuals") + 
  theme_minimal()

# Repeting for the other models
ggplot(model2, aes(.fitted, .resid)) + 
  geom_point() + 
  geom_smooth(method = "loess", se = FALSE) + 
  labs(title = "Residuals vs Adjusted Values - Model 2", x = "Adjusted Values", y = "Residuals") + 
  theme_minimal()

ggplot(model3, aes(.fitted, .resid)) + 
  geom_point() + 
  geom_smooth(method = "loess", se = FALSE) + 
  labs(title = "Residuals vs Adjusted Values - Model 3", x = "Adjusted Values", y = "Residuals") + 
  theme_minimal()

# The same for models 4, 5 and 6:
ggplot(model4, aes(.fitted, .resid)) + 
  geom_point() + 
  geom_smooth(method = "loess", se = FALSE) + 
  labs(title = "Residuals vs Adjusted Values - Model 4", x = "Adjusted Values", y = "Residuals") + 
  theme_minimal()

ggplot(model5, aes(.fitted, .resid)) + 
  geom_point() + 
  geom_smooth(method = "loess", se = FALSE) + 
  labs(title = "Residuals vs Adjusted Values - Model 5", x = "Adjusted Values", y = "Residuals") + 
  theme_minimal()

ggplot(model6, aes(.fitted, .resid)) + 
  geom_point() + 
  geom_smooth(method = "loess", se = FALSE) + 
  labs(title = "Residuals vs Adjusted Values - Model 6", x = "Adjusted Values", y = "Residuals") + 
  theme_minimal()


#Clustering-based analysis
# Necessary libraries
library(cluster)
library(factoextra)

# 1. Identify market segments based on wine characteristics (BD, ACD, AL).
wine_data_clean <- na.omit(wine_data)
wine_data_scaled <- scale(wine_data_clean[, c("BD", "ACD", "AL")])

# Determine the optimal number of clusters using the elbow method
fviz_nbclust(wine_data_scaled, kmeans, method = "wss")

# Application of the k-means algorithm with a chosen number of clusters (e.g., 3).
set.seed(123)
km_res <- kmeans(wine_data_scaled, centers = 3, nstart = 25)

# Visualization of clusters
fviz_cluster(km_res, data = wine_data_scaled, ellipse.type = "norm", geom = "point", stand = FALSE)

# 2. Group wines according to their region (REG) and their relation to price (PR)
wine_data_clustering <- wine_data_clean %>%
  select(REG, PR) %>%
  mutate(REG = as.numeric(as.factor(REG)))

# Data scaling
wine_data_clustering_scaled <- scale(wine_data_clustering)

# Determine the optimal number of clusters
fviz_nbclust(wine_data_clustering_scaled, kmeans, method = "wss")

# Application of k-means
km_res2 <- kmeans(wine_data_clustering_scaled, centers = 3, nstart = 25)

# Visualization of clusters
fviz_cluster(km_res2, data = wine_data_clustering_scaled, ellipse.type = "norm", geom = "point", stand = FALSE)

# 3. Group wines according to their rating (RT) and price (PR)
wine_data_clustering <- wine_data_clean %>%select(RT, PR)

# Data scaling
wine_data_clustering_scaled <- scale(wine_data_clustering)

# Determine the optimal number of clusters
fviz_nbclust(wine_data_clustering_scaled, kmeans, method = "wss")

# Application of k-means
km_res2 <- kmeans(wine_data_clustering_scaled, centers = 5, nstart = 25)

# Visualization of clusters
fviz_cluster(km_res2, data = wine_data_clustering_scaled, ellipse.type = "norm", geom = "point", stand = FALSE)

