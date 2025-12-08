# Download Libraries

library(ggplot2)
library(dplyr)
library(corrplot)
library(car)
library(lmtest)
library(nortest)
library(gridExtra)
library(moments)

# Set up Workspace and Load Data

setwd("/workspaces/Statistics-Project")
data <- read.csv("data/30457_Assignment_Data_2025_2026.csv", sep=";", header=TRUE)


# Clean Data by Removing NAs for Each Variable Separately

sex <- na.omit(data$sex)
language <- na.omit(data$language)
siblings <- na.omit(data$siblings)
account_num <- na.omit(data$account_num)
story_views <- na.omit(data$story_views)
day_time_min <- na.omit(data$day_time_min)
num_follower <- na.omit(data$num_follower)
num_post <- na.omit(data$num_post)
effectiveness <- na.omit(data$effectiveness)
attractiveness <- na.omit(data$attractiveness)
private_d <- na.omit(data$private_d)




# A. Descriptive Statistics

# A1. Descriptive Statistics (With Outliers)

# Summary of the Data
print(summary(data, na.rm=TRUE))

# Sex Analysis

sex_counts <- table(sex)
sex_props <- prop.table(sex_counts)
sex_mode <- names(sex_counts)[which.max(sex_counts)]

account_num_mean <- mean(account_num)
account_num_median <- median(account_num)
account_num_sd <- sd(account_num)
account_num_var <- var(account_num)
account_num_IQR <- IQR(account_num)
account_num_quantiles <- quantile(account_num, probs = c(0.25, 0.5, 0.75))
account_num_Q1 <- quantile(account_num, 0.25)
account_num_Q3 <- quantile(account_num, 0.75)
account_num_IQR_value <- IQR(account_num)
account_num_lower_limit <- account_num_Q1 - (1.5 * account_num_IQR_value)
account_num_upper_limit <- account_num_Q3 + (1.5 * account_num_IQR_value)

# Language Analysis

language_counts <- table(language)
language_props <- prop.table(language_counts)
language_mode <- names(language_counts)[which.max(language_counts)]

account_num_mean <- mean(account_num)
account_num_median <- median(account_num)
account_num_sd <- sd(account_num)
account_num_var <- var(account_num)
account_num_IQR <- IQR(account_num)
account_num_quantiles <- quantile(account_num, probs = c(0.25, 0.5, 0.75))
account_num_Q1 <- quantile(account_num, 0.25)
account_num_Q3 <- quantile(account_num, 0.75)
account_num_IQR_value <- IQR(account_num)
account_num_lower_limit <- account_num_Q1 - (1.5 * account_num_IQR_value)
account_num_upper_limit <- account_num_Q3 + (1.5 * account_num_IQR_value)


# Siblings Analysis

siblings_counts <- table(siblings)
siblings_props <- prop.table(siblings_counts)
siblings_mode <- names(siblings_counts)[which.max(siblings_counts)]

account_num_mean <- mean(account_num)
account_num_median <- median(account_num)
account_num_sd <- sd(account_num)
account_num_var <- var(account_num)
account_num_IQR <- IQR(account_num)
account_num_quantiles <- quantile(account_num, probs = c(0.25, 0.5, 0.75))
account_num_Q1 <- quantile(account_num, 0.25)
account_num_Q3 <- quantile(account_num, 0.75)
account_num_IQR_value <- IQR(account_num)
account_num_lower_limit <- account_num_Q1 - (1.5 * account_num_IQR_value)
account_num_upper_limit <- account_num_Q3 + (1.5 * account_num_IQR_value)

#account_num Analysis

account_num_counts <- table(account_num)
account_num_props <- prop.table(account_num_counts)
account_num_mode <- names(account_num_counts)[which.max(account_num_counts)]

account_num_mean <- mean(account_num)
account_num_median <- median(account_num)
account_num_sd <- sd(account_num)
account_num_var <- var(account_num)
account_num_IQR <- IQR(account_num)
account_num_quantiles <- quantile(account_num, probs = c(0.25, 0.5, 0.75))
account_num_Q1 <- quantile(account_num, 0.25)
account_num_Q3 <- quantile(account_num, 0.75)
account_num_IQR_value <- IQR(account_num)
account_num_lower_limit <- account_num_Q1 - (1.5 * account_num_IQR_value)
account_num_upper_limit <- account_num_Q3 + (1.5 * account_num_IQR_value)


# Story Views Analysis

story_views_mean <- mean(story_views)
story_views_median <- median(story_views)
story_views_sd <- sd(story_views)
story_views_var <- var(story_views)
story_views_IQR <- IQR(story_views)
story_views_quantiles <- quantile(story_views, probs = c(0.25, 0.5, 0.75))
story_views_Q1 <- quantile(story_views, 0.25)
story_views_Q3 <- quantile(story_views, 0.75)
story_views_lower_IQR_value <- IQR(story_views)
story_views_lower_limit <- story_views_Q1 - (1.5 * story_views_lower_IQR_value)
story_views_upper_limit <- story_views_Q3 + (1.5 * story_views_lower_IQR_value)

# Day Time Minutes Analysis

day_time_min_mean <- mean(day_time_min)
day_time_min_median <- median(day_time_min)
day_time_min_sd <- sd(day_time_min)
day_time_min_var <- var(day_time_min)
day_time_min_IQR <- IQR(day_time_min)
day_time_min_quantiles <- quantile(day_time_min, probs = c(0.25, 0.5, 0.75))
day_time_min_Q1 <- quantile(day_time_min, 0.25)
day_time_min_Q3 <- quantile(day_time_min, 0.75)
day_time_min_IQR_value <- IQR(day_time_min)
day_time_min_lower_limit <- day_time_min_Q1 - (1.5 * day_time_min_IQR_value)
day_time_min_upper_limit <- day_time_min_Q3 + (1.5 * day_time_min_IQR_value)

# Number of Followers Analysis

num_follower_mean <- mean(num_follower)
num_follower_median <- median(num_follower)
num_follower_sd <- sd(num_follower)
num_follower_var <- var(num_follower)
num_follower_IQR <- IQR(num_follower)
num_follower_quantiles <- quantile(num_follower, probs = c(0.25, 0.5, 0.75))
num_follower_Q1 <- quantile(num_follower, 0.25)
num_follower_Q3 <- quantile(num_follower, 0.75)
num_follower_IQR_value <- IQR(num_follower)
num_follower_lower_limit <- num_follower_Q1 - (1.5 * num_follower_IQR_value)
num_follower_upper_limit <- num_follower_Q3 + (1.5 * num_follower_IQR_value)

# Number of Posts Analysis

num_post_mean <- mean(num_post)
num_post_median <- median(num_post)
num_post_sd <- sd(num_post)
num_post_var <- var(num_post)
num_post_IQR <- IQR(num_post)
num_post_quantiles <- quantile(num_post, probs = c(0.25, 0.5, 0.75))
num_post_min_Q1 <- quantile(num_post, 0.25)
num_post_min_Q3 <- quantile(num_post, 0.75)
num_post_IQR_value <- IQR(num_post)
num_post_lower_limit <- num_post_min_Q1 - (1.5 * num_post_IQR_value)
num_post_upper_limit <- num_post_min_Q3 + (1.5 * num_post_IQR_value)

# Effectiveness Analysis

effectiveness_counts <- table(effectiveness)
effectiveness_props <- prop.table(effectiveness_counts)
effectiveness_mode <- names(effectiveness_counts)[which.max(effectiveness_counts)]

account_num_mean <- mean(account_num)
account_num_median <- median(account_num)
account_num_sd <- sd(account_num)
account_num_var <- var(account_num)
account_num_IQR <- IQR(account_num)
account_num_quantiles <- quantile(account_num, probs = c(0.25, 0.5, 0.75))
account_num_Q1 <- quantile(account_num, 0.25)
account_num_Q3 <- quantile(account_num, 0.75)
account_num_IQR_value <- IQR(account_num)
account_num_lower_limit <- account_num_Q1 - (1.5 * account_num_IQR_value)
account_num_upper_limit <- account_num_Q3 + (1.5 * account_num_IQR_value)

# Attractiveness Analysis

attractiveness_counts <- table(attractiveness)
attractiveness_props <- prop.table(attractiveness_counts)
attractiveness_mode <- names(attractiveness_counts)[which.max(attractiveness_counts)]

account_num_mean <- mean(account_num)
account_num_median <- median(account_num)
account_num_sd <- sd(account_num)
account_num_var <- var(account_num)
account_num_IQR <- IQR(account_num)
account_num_quantiles <- quantile(account_num, probs = c(0.25, 0.5, 0.75))
account_num_Q1 <- quantile(account_num, 0.25)
account_num_Q3 <- quantile(account_num, 0.75)
account_num_IQR_value <- IQR(account_num)
account_num_lower_limit <- account_num_Q1 - (1.5 * account_num_IQR_value)
account_num_upper_limit <- account_num_Q3 + (1.5 * account_num_IQR_value)

# Private_D Analysis
private_d_counts <- table(private_d)
private_d_props <- prop.table(private_d_counts)
private_d_counts_mode <- names(private_d_counts)[which.max(private_d_counts)]

account_num_mean <- mean(account_num)
account_num_median <- median(account_num)
account_num_sd <- sd(account_num)
account_num_var <- var(account_num)
account_num_IQR <- IQR(account_num)
account_num_quantiles <- quantile(account_num, probs = c(0.25, 0.5, 0.75))
account_num_Q1 <- quantile(account_num, 0.25)
account_num_Q3 <- quantile(account_num, 0.75)
account_num_IQR_value <- IQR(account_num)
account_num_lower_limit <- account_num_Q1 - (1.5 * account_num_IQR_value)
account_num_upper_limit <- account_num_Q3 + (1.5 * account_num_IQR_value)

# Visuals

# Sex Analysis Visuals

plot_sex <- ggplot(as.data.frame(sex_counts), aes(x = sex, y = Freq)) +
  geom_col(fill = "steelblue") +
  theme_minimal() +
  geom_text(aes(label = Freq), vjust = -0.3) +
  
  labs(title = "Count by Sex", y = "Count")

ggsave("results/plot_sex.png", plot = plot_sex, width = 6, height = 4)

plot_sex_props <- ggplot(as.data.frame(sex_props), aes(x = sex, y = Freq)) +
  geom_col(fill = "steelblue") +
  theme_minimal() +
  geom_text(aes(label = paste0(" (", round(Freq / sum(Freq) * 100, 2), "%)")), vjust = -0.3) +
  
  labs(title = "Count by Sex", y = "Count")

ggsave("results/plot_sex_props.png", plot = plot_sex_props, width = 6, height = 4)


# Language Analysis Visuals

plot_language <- ggplot(as.data.frame(language_counts), aes(x = language, y = Freq)) +
  geom_col(fill = "steelblue") +
  theme_minimal() +
  geom_text(aes(label = Freq), vjust = -0.3) +
  
  labs(title = "Count by Language", y = "Count")

ggsave("results/plot_language.png", plot = plot_language, width = 6, height = 4)

plot_language_props <- ggplot(as.data.frame(language_props), aes(x = language, y = Freq)) +
  geom_col(fill = "steelblue") +
  theme_minimal() +
  geom_text(aes(label = paste0(" (", round(Freq / sum(Freq) * 100, 2), "%)")), vjust = -0.3) +
  
  labs(title = "Count by Language", y = "Count")

ggsave("results/plot_language_props.png", plot = plot_language_props, width = 6, height = 4)

# Siblings Analysis Visuals

plot_siblings <- ggplot(as.data.frame(siblings_counts), aes(x = siblings, y = Freq)) +
  geom_col(fill = "steelblue") +
  theme_minimal() +
  geom_text(aes(label = Freq), vjust = -0.3) +
  
  labs(title = "Count by Siblings", y = "Count")

ggsave("results/plot_siblings.png", plot = plot_siblings, width = 6, height = 4)

plot_siblings_props <- ggplot(as.data.frame(siblings_props), aes(x = siblings, y = Freq)) +
  geom_col(fill = "steelblue") +
  theme_minimal() +
  geom_text(aes(label = paste0(" (", round(Freq / sum(Freq) * 100, 2), "%)")), vjust = -0.3) +
  
  labs(title = "Count by Siblings", y = "Count")

ggsave("results/plot_siblings_props.png", plot = plot_siblings_props, width = 6, height = 4)



# Effectiveness Analysis Visluals

plot_effectiveness <- ggplot(as.data.frame(effectiveness_counts), aes(x = effectiveness, y = Freq)) +
  geom_col(fill = "steelblue") +
  theme_minimal() +
  geom_text(aes(label = Freq), vjust = -0.3) +
  
  labs(title = "Count by Effectiveness", y = "Count")

ggsave("results/plot_effectiveness.png", plot = plot_effectiveness, width = 6, height = 4)

plot_effectiveness_props <- ggplot(as.data.frame(effectiveness_props), aes(x = effectiveness, y = Freq)) +
  geom_col(fill = "steelblue") +
  theme_minimal() +
  geom_text(aes(label = paste0(" (", round(Freq / sum(Freq) * 100, 2), "%)")), vjust = -0.3) +
  
  labs(title = "Count by Effectiveness", y = "Count")

ggsave("results/plot_effectiveness_props.png", plot = plot_effectiveness_props, width = 6, height = 4)

# Attractiveness Analysis Visuals

plot_attractiveness <- ggplot(as.data.frame(attractiveness_counts), aes(x = attractiveness, y = Freq)) +
  geom_col(fill = "steelblue") +
  theme_minimal() +
  geom_text(aes(label = Freq), vjust = -0.3) +
  
  labs(title = "Count by Attractiveness", y = "Count")

ggsave("results/plot_attractiveness.png", plot = plot_attractiveness, width = 6, height = 4)

plot_attractiveness_props <- ggplot(as.data.frame(attractiveness_props), aes(x = attractiveness, y = Freq)) +
  geom_col(fill = "steelblue") +
  theme_minimal() +
  geom_text(aes(label = paste0(" (", round(Freq / sum(Freq) * 100, 2), "%)")), vjust = -0.3) +
  
  labs(title = "Count by Attractiveness", y = "Count")

ggsave("results/plot_attractiveness_props.png", plot = plot_attractiveness_props, width = 6, height = 4)

# Private_D Analysis Visuals

plot_private_d <- ggplot(as.data.frame(private_d_counts), aes(x = private_d, y = Freq)) +
  geom_col(fill = "steelblue") +
  theme_minimal() +
  geom_text(aes(label = Freq), vjust = -0.3) +
  
  labs(title = "Count by Private D", y = "Count")

ggsave("results/plot_private_d.png", plot = plot_private_d, width = 6, height = 4)

plot_private_d_props <- ggplot(as.data.frame(private_d_props), aes(x = private_d, y = Freq)) +
  geom_col(fill = "steelblue") +
  theme_minimal() +
  geom_text(aes(label = paste0(" (", round(Freq / sum(Freq) * 100, 2), "%)")), vjust = -0.3) +
  
  labs(title = "Count by Private D", y = "Count")

ggsave("results/plot_private_d_props.png", plot = plot_private_d_props, width = 6, height = 4)

# Account Num Analysis Visuals

plot_account_num <- ggplot(as.data.frame(account_num_counts), aes(x = account_num, y = Freq)) +
  geom_col(fill = "steelblue") +
  theme_minimal() +
  geom_text(aes(label = Freq), vjust = -0.3) +
  
  labs(title = "Count by Account Number", y = "Count")

ggsave("results/plot_account_num.png", plot = plot_account_num, width = 6, height = 4)

plot_account_num_props <- ggplot(as.data.frame(account_num_props), aes(x = account_num, y = Freq)) +
  geom_col(fill = "steelblue") +
  theme_minimal() +
  geom_text(aes(label = paste0(" (", round(Freq / sum(Freq) * 100, 2), "%)")), vjust = -0.3) +
  
  labs(title = "Count by Account Number", y = "Count")

ggsave("results/plot_account_num_props.png", plot = plot_account_num_props, width = 6, height = 4)

# Story Views Boxplot and Histogram

boxplot_story_views <- ggplot(as.data.frame(num_follower), aes(y=num_follower)) + 
  geom_boxplot(fill="orange") +
  theme_minimal() + ggtitle("Boxplot of Followers (Original)")

ggsave("results/boxplot_story_views.png", plot = boxplot_story_views, width = 8, height = 6)

histogram_story_views <- ggplot(as.data.frame(story_views), aes(x=story_views)) + 
  geom_histogram(fill="skyblue", color="black", bins=30) +
  theme_minimal() + ggtitle("Distribution of Story Views (Original)")

ggsave("results/histogram_story_views.png", plot = histogram_story_views, width = 8, height = 6)

# Number of Followers Analysis Boxplot and Histogram

boxplot_followers <- ggplot(as.data.frame(num_follower), aes(y=num_follower)) + 
  geom_boxplot(fill="orange") +
  theme_minimal() + ggtitle("Boxplot of Followers (Original)")

ggsave("results/boxplot_followers.png", plot = boxplot_followers, width = 8, height = 6)

histogram_followers <- ggplot(as.data.frame(story_views), aes(x=story_views)) + 
  geom_histogram(fill="skyblue", color="black", bins=30) +
  theme_minimal() + ggtitle("Distribution of Followers (Original)")

ggsave("results/histogram_followers.png", plot = histogram_followers, width = 8, height = 6)

# Day Time Minutes Analysis Boxplot and Histogram

boxplot_day_time_min <- ggplot(as.data.frame(day_time_min), aes(y=day_time_min)) + 
  geom_boxplot(fill="orange") +
  theme_minimal() + ggtitle("Boxplot of Day Time Minutes (Original)")

ggsave("results/boxplot_day_time_min.png", plot = boxplot_day_time_min, width = 8, height = 6)

histogram_day_time_min <- ggplot(as.data.frame(day_time_min), aes(x=day_time_min)) + 
  geom_histogram(fill="skyblue", color="black", bins=30) +
  theme_minimal() + ggtitle("Distribution of Day Time Minutes (Original)")

ggsave("results/histogram_day_time_min.png", plot = histogram_day_time_min, width = 8, height = 6)

#Number of Posts Analysis Boxplot and Histogram

boxplot_num_post <- ggplot(as.data.frame(num_post), aes(y=num_post)) + 
  geom_boxplot(fill="orange") +
  theme_minimal() + ggtitle("Boxplot of Number of Posts (Original)")

ggsave("results/boxplot_num_post.png", plot = boxplot_num_post, width = 8, height = 6)

histogram_num_post <- ggplot(as.data.frame(num_post), aes(x=num_post)) + 
  geom_histogram(fill="skyblue", color="black", bins=30) +
  theme_minimal() + ggtitle("Distribution of Number of Posts (Original)")

ggsave("results/histogram_num_post.png", plot = histogram_num_post, width = 8, height = 6)

# A2. Descriptive Statistics (Without Outliers)

no_outliers_account_num <- account_num[account_num > account_num_lower_limit & account_num < account_num_upper_limit]
no_outliers_story_views <- story_views[story_views > story_views_lower_limit & story_views < story_views_upper_limit]
no_outliers_num_follower <- num_follower[num_follower > num_follower_lower_limit & num_follower < num_follower_upper_limit]
no_outliers_day_time_min <- day_time_min[day_time_min > day_time_min_lower_limit & day_time_min < day_time_min_upper_limit]
no_outliers_num_post <- num_post[num_post > num_post_lower_limit & num_post < num_post_upper_limit]

# Account Number Analysis (No Outliers)

boxplot_account_num <- ggplot(as.data.frame(no_outliers_account_num), aes(y=no_outliers_account_num)) + 
  geom_boxplot(fill="orange") +
  theme_minimal() + 
  ggtitle("Boxplot of Account Number (No Outliers)") +
  ylab("Account Number")

ggsave("results/boxplot_account_num_no_outliers.png", plot = boxplot_account_num, width = 8, height = 6)

histogram_account_num <- ggplot(as.data.frame(no_outliers_account_num), aes(x=no_outliers_account_num)) + 
  geom_histogram(fill="skyblue", color="black", bins=30) +
  theme_minimal() + 
  ggtitle("Distribution of Account Number (No Outliers)") +
  xlab("Account Number")

ggsave("results/histogram_account_num_no_outliers.png", plot = histogram_account_num, width = 8, height = 6)

# Story Views Analysis (No Outliers)

boxplot_story_views <- ggplot(as.data.frame(no_outliers_story_views), aes(y=no_outliers_story_views)) + 
  geom_boxplot(fill="orange") +
  theme_minimal() + 
  ggtitle("Boxplot of Story Views (No Outliers)") +
  ylab("Story Views")

ggsave("results/boxplot_story_views_no_outliers.png", plot = boxplot_story_views, width = 8, height = 6)

histogram_story_views <- ggplot(as.data.frame(no_outliers_story_views), aes(x=no_outliers_story_views)) + 
  geom_histogram(fill="skyblue", color="black", bins=30) +
  theme_minimal() + 
  ggtitle("Distribution of Story Views (No Outliers)") +
  xlab("Story Views")

ggsave("results/histogram_story_views_no_outliers.png", plot = histogram_story_views, width = 8, height = 6)

# Number of Followers Analysis (No Outliers)

boxplot_followers <- ggplot(as.data.frame(no_outliers_num_follower), aes(y=no_outliers_num_follower)) + 
  geom_boxplot(fill="orange") +
  theme_minimal() + 
  ggtitle("Boxplot of Followers (No Outliers)") +
  ylab("Number of Followers")

ggsave("results/boxplot_followers_no_outliers.png", plot = boxplot_followers, width = 8, height = 6)

histogram_followers <- ggplot(as.data.frame(no_outliers_num_follower), aes(x=no_outliers_num_follower)) + 
  geom_histogram(fill="skyblue", color="black", bins=30) +
  theme_minimal() + 
  ggtitle("Distribution of Followers (No Outliers)") +
  xlab("Number of Followers")

ggsave("results/histogram_followers_no_outliers.png", plot = histogram_followers, width = 8, height = 6)

# Day Time Minutes Analysis (No Outliers)

boxplot_day_time_min <- ggplot(as.data.frame(no_outliers_day_time_min), aes(y=no_outliers_day_time_min)) + 
  geom_boxplot(fill="orange") +
  theme_minimal() + 
  ggtitle("Boxplot of Day Time Minutes (No Outliers)") +
  ylab("Day Time Minutes")

ggsave("results/boxplot_day_time_min_no_outliers.png", plot = boxplot_day_time_min, width = 8, height = 6)

histogram_day_time_min <- ggplot(as.data.frame(no_outliers_day_time_min), aes(x=no_outliers_day_time_min)) + 
  geom_histogram(fill="skyblue", color="black", bins=30) +
  theme_minimal() + 
  ggtitle("Distribution of Day Time Minutes (No Outliers)") +
  xlab("Day Time Minutes")

ggsave("results/histogram_day_time_min_no_outliers.png", plot = histogram_day_time_min, width = 8, height = 6)

# Number of Posts Analysis (No Outliers)

boxplot_num_post <- ggplot(as.data.frame(no_outliers_num_post), aes(y=no_outliers_num_post)) + 
  geom_boxplot(fill="orange") +
  theme_minimal() + 
  ggtitle("Boxplot of Number of Posts (No Outliers)") +
  ylab("Number of Posts")

ggsave("results/boxplot_num_post_no_outliers.png", plot = boxplot_num_post, width = 8, height = 6)

histogram_num_post <- ggplot(as.data.frame(no_outliers_num_post), aes(x=no_outliers_num_post)) + 
  geom_histogram(fill="skyblue", color="black", bins=30) +
  theme_minimal() + 
  ggtitle("Distribution of Number of Posts (No Outliers)") +
  xlab("Number of Posts")

ggsave("results/histogram_num_post_no_outliers.png", plot = histogram_num_post, width = 8, height = 6)

# Clean Data Set

data_no_outliers <- subset(data, 
    story_views > story_views_lower_limit & story_views < story_views_upper_limit &
    num_follower > num_follower_lower_limit & num_follower < num_follower_upper_limit &
    day_time_min > day_time_min_lower_limit & day_time_min < day_time_min_upper_limit &
    num_post > num_post_lower_limit & num_post < num_post_upper_limit
)

write.csv(data_no_outliers, "data/cleaned_data_final_no_outliers.csv", row.names = FALSE)
print(summary(data_no_outliers))

#B. Confidence Intervals
cat("\n \n")
cat("B1. Story Views (95% Confidence Interval)")

#B1. Story Views (95% Confidence Interval) ---

# 1. Overall 95% CI
ci_story_overall <- t.test(data_no_outliers$story_views, conf.level = 0.95)
cat("\n Story Views: Overall 95% CI \n")
cat(ci_story_overall$conf.int)
cat(paste("Mean:", round(ci_story_overall$estimate, 2)))

# 2. Compare Only Child (siblings == 0) vs Others (siblings > 0)
views_only_child <- subset(data_no_outliers, siblings == 0)$story_views
views_others <- subset(data_no_outliers, siblings > 0)$story_views

ci_story_only <- t.test(views_only_child, conf.level = 0.95)
ci_story_others <- t.test(views_others, conf.level = 0.95)

cat("\n Story Views: Only Child 95% CI \n")
cat(ci_story_only$conf.int)
cat(paste("Mean:", round(ci_story_only$estimate, 2)))

cat("\n Story Views: Others (Has Siblings) 95% CI \n")
cat(ci_story_others$conf.int)
cat(paste("Mean:", round(ci_story_others$estimate, 2)))

# B2. Number of Followers (99% Confidence Interval)

cat("\n \n")
cat("B2. Number of Followers (99% Confidence Interval)")

# Overall 99% CI 
ci_followers <- t.test(data_no_outliers$num_follower, conf.level = 0.99)
cat("\n Followers: Overall 99% CI \n")
cat(ci_followers$conf.int)
cat(paste("Mean:", round(ci_followers$estimate, 2)))

# Compare Men (sex == 'M') vs Women (sex == 'F')
followers_men <- subset(data_no_outliers, sex == 'M')$num_follower
followers_women <- subset(data_no_outliers, sex == 'F')$num_follower

ci_followers_men <- t.test(followers_men, conf.level = 0.99)
ci_followers_women <- t.test(followers_women, conf.level = 0.99)

cat("\n Followers: Men 99% CI \n")
cat(ci_followers_men$conf.int)
cat(paste("Mean:", round(ci_followers_men$estimate, 2)))

cat("\n Followers: Women 99% CI \n")
cat(ci_followers_women$conf.int)
cat(paste("Mean:", round(ci_followers_women$estimate, 2)))

cat("B3. Proportion of Italian Accounts (90% Confidence Interval)")
# B3. Proportion of Italian Accounts (90% Confidence Interval)

# Count occurrences
n_total <- nrow(data_no_outliers)
n_italian <- sum(data_no_outliers$language == "Italian")

# Calculate CI for proportion
ci_italian <- prop.test(n_italian, n_total, conf.level = 0.90)

cat("\n Language: Italian Proportion 90% CI \n")
cat(ci_italian$conf.int)
cat(paste("Proportion:", round(ci_italian$estimate, 4)))

# C Hypothesis Testing

# C1. Followers: Men vs Women
# H0: Mean followers (Men) = Mean followers (Women)
# H1: Mean followers (Men) != Mean followers (Women)

followers_men <- subset(data_no_outliers, sex == 'M')$num_follower
followers_women <- subset(data_no_outliers, sex == 'F')$num_follower

test_c1 <- t.test(followers_men, followers_women)

cat("\nC1 Followers: Men vs Women\n")
print(test_c1)

# C2 Story Views: Public vs Private University ---
# H0: Mean views (Public) = Mean views (Private)
# H1: Mean views (Public) != Mean views (Private)

views_public <- subset(data_no_outliers, private_d == 0)$story_views
views_private <- subset(data_no_outliers, private_d == 1)$story_views

test_c2 <- t.test(views_public, views_private)

cat("\nC2 Story Views: Public vs Private University\n")
print(test_c2)

# C3 Daily Time: English vs Italian
# H0: Mean time (English) = Mean time (Italian)
# H1: Mean time (English) != Mean time (Italian)

time_english <- subset(data_no_outliers, language == 'English')$day_time_min
time_italian <- subset(data_no_outliers, language == 'Italian')$day_time_min

test_c3 <- t.test(time_english, time_italian, conf.level = 0.99)

cat("\nC3 Daily Time: English vs Italian\n")
print(test_c3)

# D. Linear Regression Analysis

reg_data <- data_no_outliers 

# D1. Simple Linear Regression
# Model: story_views (Dependent) ~ num_follower (Independent)


cat("\n\n")
cat(" D1. Simple Linear Regression Results \n")
cat("\n\n")

# 1. Run the model
model_simple <- lm(story_views ~ num_follower, data = reg_data)
summary_simple <- summary(model_simple)

# 2. Calculate Metrics (SSE, SSR, SST, MSE, RSE, R-squared)

# Predictions and Residuals
preds_simple <- predict(model_simple)
resids_simple <- residuals(model_simple)

# Sum of Squares
sse_simple <- sum(resids_simple^2)                         
sst_simple <- sum((reg_data$story_views - mean(reg_data$story_views))^2) 
ssr_simple <- sst_simple - sse_simple                    

# Mean Squares and Errors
n_simple <- nrow(reg_data)
p_simple <- 1 # Number of predictors: num_follower
mse_simple <- sse_simple / (n_simple - p_simple - 1)       
rse_simple <- sqrt(mse_simple)                           
r_squared_simple <- summary_simple$r.squared               

# Print Metrics
cat(sprintf("SSE: %.4f\n", sse_simple))
cat(sprintf("SSR: %.4f\n", ssr_simple))
cat(sprintf("SST: %.4f\n", sst_simple))
cat(sprintf("MSE: %.4f\n", mse_simple))
cat(sprintf("RSE: %.4f\n", rse_simple))
cat(sprintf("R-squared: %.4f\n", r_squared_simple))

# 3. Coefficients (b0, b1, Std Error, t-value, p-value)
cat("\n--- Coefficients ---\n")
print(summary_simple$coefficients)

# 4. Standardized Coefficients

\
model_simple_std <- lm(scale(story_views) ~ scale(num_follower), data = reg_data)
cat("\n--- Standardized Coefficients (Beta) ---\n")
print(coef(model_simple_std))

# 5. Plots

# Histogram of Residuals
plot_hist_resid_simple <- ggplot(data.frame(resids = resids_simple), aes(x = resids)) +
  geom_histogram(fill = "steelblue", color = "white", bins = 30) +
  theme_minimal() +
  labs(title = "Histogram of Residuals (Simple Regression)", x = "Residuals", y = "Frequency")

ggsave("results/D1_histogram_residuals.png", plot = plot_hist_resid_simple, width = 6, height = 4)

# Residuals vs Fitted Plot
plot_resid_fit_simple <- ggplot(data.frame(fitted = preds_simple, resid = resids_simple), aes(x = fitted, y = resid)) +
  geom_point(alpha = 0.5, color = "darkblue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  theme_minimal() +
  labs(title = "Residuals vs Fitted (Simple Regression)", x = "Fitted Values", y = "Residuals")

ggsave("results/D1_residuals_vs_fitted.png", plot = plot_resid_fit_simple, width = 6, height = 4)


# D2. Multiple Linear Regression
# Model: story_views ~ num_follower + sex + account_num + num_post + day_time_min

cat("\n\n")
cat(" D2. Multiple Linear Regression Results \n")
cat("\n")

# 1. Run the model
model_multi <- lm(story_views ~ num_follower + sex + account_num + num_post + day_time_min, data = reg_data)
summary_multi <- summary(model_multi)

# 2. Calculate Metrics
preds_multi <- predict(model_multi)
resids_multi <- residuals(model_multi)

sse_multi <- sum(resids_multi^2)
sst_multi <- sum((reg_data$story_views - mean(reg_data$story_views))^2)
ssr_multi <- sst_multi - sse_multi

n_multi <- nrow(reg_data)
p_multi <- 5 # Number of predictors: num_follower, sex, account_num, num_post, day_time_min
mse_multi <- sse_multi / (n_multi - p_multi - 1)
rse_multi <- sqrt(mse_multi)
r_squared_multi <- summary_multi$r.squared

# Print Metrics
cat(sprintf("SSE: %.4f\n", sse_multi))
cat(sprintf("SSR: %.4f\n", ssr_multi))
cat(sprintf("SST: %.4f\n", sst_multi))
cat(sprintf("MSE: %.4f\n", mse_multi))
cat(sprintf("RSE: %.4f\n", rse_multi))
cat(sprintf("R-squared: %.4f\n", r_squared_multi))

# 3. Coefficients
cat("\nCoefficients\n")
print(summary_multi$coefficients)

# 4. Standardized Coefficients

reg_data_scaled <- reg_data
num_vars <- c("story_views", "num_follower", "account_num", "num_post", "day_time_min")
reg_data_scaled[num_vars] <- scale(reg_data[num_vars])

model_multi_std <- lm(story_views ~ num_follower + sex + account_num + num_post + day_time_min, data = reg_data_scaled)
cat("\n Coefficients\n")
print(coef(model_multi_std))

# 5. Plots

# Histogram of Residuals
plot_hist_resid_multi <- ggplot(data.frame(resids = resids_multi), aes(x = resids)) +
  geom_histogram(fill = "forestgreen", color = "white", bins = 30) +
  theme_minimal() +
  labs(title = "Histogram of Residuals (Multiple Regression)", x = "Residuals", y = "Frequency")

ggsave("results/D2_histogram_residuals.png", plot = plot_hist_resid_multi, width = 6, height = 4)

# Residuals vs Fitted Plot
plot_resid_fit_multi <- ggplot(data.frame(fitted = preds_multi, resid = resids_multi), aes(x = fitted, y = resid)) +
  geom_point(alpha = 0.5, color = "darkgreen") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  theme_minimal() +
  labs(title = "Residuals vs Fitted (Multiple Regression)", x = "Fitted Values", y = "Residuals")

ggsave("results/D2_residuals_vs_fitted.png", plot = plot_resid_fit_multi, width = 6, height = 4)