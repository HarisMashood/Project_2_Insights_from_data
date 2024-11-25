# Installing and Loading Required Libraries
if (!require(visdat)) install.packages("visdat"); library(visdat)
if (!require(tidyverse)) install.packages("tidyverse"); library(tidyverse)
if (!require(cluster)) install.packages("cluster"); library(cluster)
if (!require(factoextra)) install.packages("factoextra"); library(factoextra)
if (!require(dplyr)) install.packages("dplyr"); library(dplyr)
if (!require(ggplot2)) install.packages("ggplot2"); library(ggplot2)

# Loading Datasets
questions_data <- read.csv("C:/Users/haris/OneDrive/Documents/Insights from Data/vaalikone_questions_all.csv", sep = ";")
profiles_data <- read.csv("C:/Users/haris/OneDrive/Documents/Insights from Data/vaalikone_profiles_all.csv", sep = ";")

# Preview Data
cat("Preview of Questions Data Structure:\n")
str(questions_data)
cat("Preview of Profiles Data Structure:\n")
str(profiles_data)

# Handling Missing Values
vis_miss(questions_data)  # Visualizing missing values
questions_data_clean <- na.omit(questions_data)  # Droping rows with missing values
vis_miss(questions_data_clean)  # Checking again after cleaning

# Converting Columns to Suitable Data Types
questions_data_clean <- questions_data_clean %>%
  mutate(across(starts_with("Q"), as.factor)) %>%  # Converting Q columns to factors
  mutate(across(starts_with("W"), ~ as.ordered(.)))  # Converting W columns to ordered factors

# Standardizing Numerical Data and Encoding Categorical Data
# Separating numerical and categorical columns
numerical_data <- questions_data_clean %>% select_if(is.numeric)
categorical_data <- questions_data_clean %>% select_if(is.factor)

# Standardizing numerical data
numerical_data_scaled <- scale(numerical_data)

# One-hot encode categorical data
categorical_data_encoded <- model.matrix(~ . - 1, data = categorical_data)

# Combine standardized numerical and encoded categorical data
combined_data <- cbind(numerical_data_scaled, categorical_data_encoded)

# Computing Gower Distance
# Limit to a subset (e.g., first 500 rows) for visualization
subset_data <- questions_data_clean[1:500, -1]  # Exclude ID column
gower_dist <- daisy(subset_data, metric = "gower")  # Computing Gower distance on subset

# Check Gower Distance Summary
summary(gower_dist)

# Visualize Subset of Distance Matrix (to avoid overcrowded plot)
gower_matrix <- as.matrix(gower_dist)
fviz_dist(as.dist(gower_matrix[1:100, 1:100]), order = FALSE)  # Visualize smaller subset

# Determine Optimal Number of Clusters
# Check for missing or problematic values in combined_data
combined_data[is.na(combined_data)] <- 0  # Replace NA with 0
combined_data[!is.finite(combined_data)] <- 0  # Replace non-finite values with 0

# Remove zero-variance columns
zero_var_cols <- apply(combined_data, 2, var) == 0
combined_data <- combined_data[, !zero_var_cols]

# Re-run clustering evaluation
fviz_nbclust(combined_data, kmeans, method = "wss") + ggtitle("Elbow Method")
fviz_nbclust(combined_data, kmeans, method = "silhouette") + ggtitle("Silhouette Method")

# Performing Clustering
# K-means clustering
set.seed(123)
km_res <- kmeans(combined_data, centers = 4, nstart = 25)  # Use 4 clusters based on analysis

# Add cluster assignments to the cleaned dataset
questions_data_clean$Cluster <- km_res$cluster

# Save cleaned and clustered data
write.csv(questions_data_clean, "C:/Users/haris/OneDrive/Documents/Insights from Data/cleaned_clustered_questions.csv", row.names = FALSE)

# Merging with Profiles Data
merged_data <- merge(profiles_data, questions_data_clean, by = "ID")

# Save the merged dataset
write.csv(merged_data, "C:/Users/haris/OneDrive/Documents/Insights from Data/merged_data.csv", row.names = FALSE)

# Analyze and Visualize Clusters
# Cluster distribution by political party
ggplot(merged_data, aes(x = factor(Cluster), fill = Party)) +
  geom_bar(position = "dodge") +
  labs(title = "Cluster Distribution by Political Party", x = "Cluster", y = "Count") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.text.x = element_text(angle = 0, hjust = 1)
  ) +
  scale_fill_manual(values = scales::hue_pal()(length(unique(merged_data$Party))))

# Visualize clusters using PCA
fviz_cluster(km_res, data = combined_data, ellipse.type = "norm", geom = "point", palette = "jco")
