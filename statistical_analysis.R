library(ggplot2)
library(dplyr)
library(corrplot)
library(car)
library(lmtest)
library(nortest)
library(gridExtra)
library(moments)


setwd("/workspaces/Statistics-Project")
data <- read.csv("data/30457_Assignment_Data_2025_2026.csv", sep=";", header=TRUE)

cat(dim(data))
cat(names(data))


data_clean <- na.omit(data)
write.csv(data_clean, "data/cleaned_data_rows.csv", row.names = FALSE)

cat(dim(data_clean))
cat(names(data_clean))

sex <- data_clean$sex
language <- data_clean$language
siblings <- data_clean$siblings
account_num <- data_clean$account_num
story_views <- data_clean$story_views
day_time_min <- data_clean$day_time_min
num_follower <- data_clean$num_follower
num_post <- data_clean$num_post
effectiveness <- data_clean$effectiveness
attractiveness <- data_clean$attractiveness
private_d <- data_clean$private_d

# Sex Analysis

sex_counts <- table(sex)
print(sex_counts)
sex_props <- prop.table(sex_counts)
print(sex_props)
sex_mode <- names(sex_counts)[which.max(sex_counts)]
print(sex_mode)

# Language Analysis

language_counts <- table(language)
print(language_counts)
language_props <- prop.table(language_counts)
print(language_props)
language_mode <- names(language_counts)[which.max(language_counts)]
print(language_mode)

# Siblings Analysis

siblings_counts <- table(siblings)
print(siblings_counts)
siblings_props <- prop.table(siblings_counts)
print(siblings_props)
siblings_mode <- names(siblings_counts)[which.max(siblings_counts)]
print(siblings_mode)

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

data_no_outliers <- subset(data, story_views > story_views_lower_limit & story_views < story_views_upper_limit)

print(story_views_mean)
print(story_views_median)
print(story_views_sd)
print(story_views_var)
print(story_views_IQR)
print(story_views_quantiles)
print(story_views_lower_limit)
print(story_views_upper_limit)

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

day_time_min_data_no_outliers <- subset(data, day_time_min > day_time_min_lower_limit & day_time_min < day_time_min_upper_limit)

print(day_time_min_mean)
print(day_time_min_median)
print(day_time_min_sd)
print(day_time_min_var)
print(day_time_min_IQR)
print(day_time_min_quantiles)
print(day_time_min_lower_limit)
print(day_time_min_upper_limit)

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

num_follower_data_no_outliers <- subset(data, num_follower > num_follower_lower_limit & num_follower < num_follower_upper_limit)

print(num_follower_mean)
print(num_follower_median)
print(num_follower_sd)
print(num_follower_var)
print(num_follower_IQR)
print(num_follower_quantiles)
print(num_follower_lower_limit)
print(num_follower_upper_limit)

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

num_post_data_no_outliers <- subset(data, num_post > num_post_lower_limit & num_post < num_post_upper_limit)

print(num_post_mean)
print(num_post_median)
print(num_post_sd)
print(num_post_var)
print(num_post_IQR)
print(num_post_quantiles)
print(num_post_lower_limit)
print(num_post_upper_limit)

# Effectiveness Analysis

effectiveness_counts <- table(effectiveness)
print(effectiveness_counts)
effectiveness_props <- prop.table(effectiveness_counts)
print(effectiveness_props)
effectiveness_mode <- names(effectiveness_counts)[which.max(effectiveness_counts)]
print(effectiveness_mode)

# Attractiveness Analysis

attractiveness_counts <- table(attractiveness)
print(attractiveness_counts)
attractiveness_props <- prop.table(attractiveness_counts)
print(attractiveness_props)
attractiveness_mode <- names(attractiveness_counts)[which.max(attractiveness_counts)]
print(attractiveness_mode)

# Private_D Analysis
private_d_counts <- table(private_d)
print(private_d_counts)
private_d_props <- prop.table(private_d_counts)
print(private_d_props)
private_d_counts_mode <- names(private_d_counts)[which.max(private_d_counts)]
print(private_d_counts_mode)

#Visuals

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


# Story Views Histogram

histogram_story_views <- ggplot(as.data.frame(story_views), aes(x=story_views)) + 
  geom_histogram(fill="skyblue", color="black", bins=30) +
  theme_minimal() + ggtitle("Distribution of Story Views (Original)")

ggsave("results/histogram_story_views.png", plot = histogram_story_views, width = 8, height = 6)

# Number of Followers Analysis Boxplot

follower_boxplot <- ggplot(as.data.frame(num_follower), aes(y=num_follower)) + 
  geom_boxplot(fill="orange") +
  theme_minimal() + ggtitle("Boxplot of Followers (Original)")

ggsave("results/boxplot_followers.png", plot = follower_boxplot, width = 8, height = 6)






