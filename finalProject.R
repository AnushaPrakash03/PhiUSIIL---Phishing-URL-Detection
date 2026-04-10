#Run the below R-code one-by-one for analyzing PhiUSIIL Phishing URLs 
# Load necessary libraries
library(tidyverse)  # for data manipulation and visualization
library(readr)      # for reading CSV files
library(dplyr)      # for data wrangling

getwd()  # Check current working directory
setwd("/Users/anushaprakash/Documents/NEU/INFO6105/Project")
df <- read.csv("PhiUSIIL_Phishing_URL_Dataset.csv") # Read the CSV file
head(df) # View the first few rows of the dataset

# View the number of rows and columns
dim(df)  # returns a vector: [num_rows, num_columns]
colnames(df) # Print the column names
summary(df) # Summary of the dataset
str(df) # Structure of the dataset

#Data Processing
colSums(is.na(df)) # Check for missing values in each column
df$label <- as.factor(df$label) # Convert Label column to a factor
table(df$label) # Check class distribution

#Rename levels for better readability (0 = phishing, 1 = legitimate)
df$label <- factor(df$label, levels = c(0, 1), labels = c("Phishing", "Legitimate"))

# Re-check class distribution after labeling
table(df$label)

#Exploratory Data Analysis(EDA)
library(scales)
library(ggplot2)
library(tidyverse)

# Visualize class distribution
ggplot(df, aes(x = label, fill = label)) +
  geom_bar() +
  labs(title = "Distribution of URL Labels", x = "URL Type", y = "Count") +
  scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())) +
  theme_minimal()

#Summary Statistics of Key Features
df %>%
  select(TLDLegitimateProb, URLTitleMatchScore, CharContinuationRate) %>%
  summary()

# Histogram of key features
ggplot(df, aes(x = TLDLegitimateProb, fill = label)) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 30) +
  labs(title = "Distribution of TLDLegitimateProb by URL Type", x = "TLD Legitimate Probability", y = "Count") +
  theme_minimal()

ggplot(df, aes(x = URLTitleMatchScore, fill = label)) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 30) +
  labs(title = "Distribution of URLTitleMatchScore by URL Type", x = "URL Title Match Score", y = "Count") +
  theme_minimal()

# Box plot comparison for a feature
ggplot(df, aes(x = label, y = CharContinuationRate, fill = label)) +
  geom_boxplot() +
  labs(title = "Boxplot of CharContinuationRate by URL Type", x = "URL Type", y = "Char Continuation Rate") +
  theme_minimal()

#Question 1: 1.	What are the key distinguishing features between legitimate and phishing URL?
# T-test for TLDLegitimateProb
t.test(TLDLegitimateProb ~ label, data = df)

# T-test for URLTitleMatchScore
t.test(URLTitleMatchScore ~ label, data = df)

# T-test for CharContinuationRate
t.test(CharContinuationRate ~ label, data = df)

#Visualize with boxplots
# TLDLegitimateProb
ggplot(df, aes(x = label, y = TLDLegitimateProb, fill = label)) +
  geom_boxplot() +
  labs(title = "TLD Legitimate Probability by URL Type", x = "URL Type", y = "TLDLegitimateProb")

# URLTitleMatchScore
ggplot(df, aes(x = label, y = URLTitleMatchScore, fill = label)) +
  geom_boxplot() +
  labs(title = "URL Title Match Score by URL Type", x = "URL Type", y = "URLTitleMatchScore")

# CharContinuationRate
ggplot(df, aes(x = label, y = CharContinuationRate, fill = label)) +
  geom_boxplot() +
  labs(title = "Char Continuation Rate by URL Type", x = "URL Type", y = "CharContinuationRate")

#Calculate Effect Size — Cohen’s d
#install.packages("effsize")
library(effsize)

cohen.d(TLDLegitimateProb ~ label, data = df)
cohen.d(URLTitleMatchScore ~ label, data = df)
cohen.d(CharContinuationRate ~ label, data = df)

#Principal Component Analysis
#install.packages("factoextra")
library(factoextra)

# PCA with scaling
df_pca <- prcomp(df_numeric, scale. = TRUE)

#Summary of PCA
summary(df_pca)

# Plot individuals (colored by class)
fviz_pca_ind(df_pca,
             geom.ind = "point",
             col.ind = df$label,
             palette = c("#00AFBB", "#FC4E07"),
             addEllipses = TRUE,
             legend.title = "Label") +
  labs(x = "Legitimacy Signals (18.4%)", y = "URL Structure (10%)")

# Top 10 contributing features to PC1 Legitimacy Signals
fviz_contrib(df_pca, choice = "var", axes = 1, top = 10)

# Top 10 contributing features to PC2 URL Structure
fviz_contrib(df_pca, choice = "var", axes = 2, top = 10)

#Question2: 2.	How do URL characteristics such as TLDLegitimateProb or URLTitleMatchScore affect the likelihood of a malicious URL?
library(tidyverse)
# Convert label to numeric for correlation (Phishing = 0, Legitimate = 1)
df$label_numeric <- ifelse(df$label == "Phishing", 0, 1)

# Correlation matrix
correlation_vars <- df %>%
  select(label_numeric, TLDLegitimateProb, URLTitleMatchScore, CharContinuationRate)

cor_matrix <- cor(correlation_vars)
print(cor_matrix)

#Correlation visualization using heatmap
library(corrplot)

corrplot(cor_matrix, method = "color", addCoef.col = "black", tl.col = "black")

#ANOVA to test feature significance
# ANOVA for TLDLegitimateProb
anova_TLD <- aov(TLDLegitimateProb ~ label, data = df)
summary(anova_TLD)

# ANOVA for URLTitleMatchScore
anova_Title <- aov(URLTitleMatchScore ~ label, data = df)
summary(anova_Title)

# ANOVA for CharContinuationRate
anova_Char <- aov(CharContinuationRate ~ label, data = df)
summary(anova_Char)

#Visualizing ANOVA with boxplots
ggplot(df, aes(x = label, y = TLDLegitimateProb, fill = label)) +
  geom_boxplot() +
  labs(title = "TLDLegitimateProb by URL Type")

ggplot(df, aes(x = label, y = URLTitleMatchScore, fill = label)) +
  geom_boxplot() +
  labs(title = "URLTitleMatchScore by URL Type")

#Scatterplot 
# TLDLegitimateProb vs Phishing Likelihood
ggplot(df, aes(x = TLDLegitimateProb, y = label_numeric)) +
  geom_jitter(alpha = 0.3) +
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "TLDLegitimateProb vs Phishing Likelihood", y = "Phishing (0) or Legitimate (1)")

# URLTitleMatchScore vs Phishing Likelihood
ggplot(df, aes(x = URLTitleMatchScore, y = label_numeric)) +
  geom_jitter(alpha = 0.3) +
  geom_smooth(method = "lm", color = "green") +
  labs(title = "URLTitleMatchScore vs Phishing Likelihood", y = "Phishing (0) or Legitimate (1)")

 