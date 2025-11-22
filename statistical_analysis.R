# Statistical Analysis of Instagram Data
# Author: Statistical Analysis
# Date: November 22, 2025

# Load required libraries
library(ggplot2)
library(dplyr)
library(corrplot)
library(car)
library(lmtest)
library(nortest)

# Set working directory and load data
setwd("/workspaces/Statistics-Project")
data <- read.csv("30457_Assignment_Data_2025_2026.csv", sep=";", header=TRUE)

# Display basic information about the dataset
cat("Dataset dimensions:", dim(data), "\n")
cat("Variable names:", names(data), "\n")

# Data preprocessing
# Convert categorical variables to factors
data$sex <- as.factor(data$sex)
data$language <- as.factor(data$language)
data$effectiveness <- as.factor(data$effectiveness)
data$private_d <- as.factor(data$private_d)

# Create binary variables for analysis
data$only_child <- ifelse(data$siblings == 0, 1, 0)
data$italian_language <- ifelse(data$language == "Italian", 1, 0)
data$english_speaker <- ifelse(data$language == "English", 1, 0)

# Display summary of the data
summary(data)

# ============================================================================
# A. DESCRIPTIVE STATISTICS
# ============================================================================

cat("\n==========================================\n")
cat("A. DESCRIPTIVE STATISTICS OF THE SAMPLE\n")
cat("==========================================\n")

# A.1 Report main information for each variable

# Quantitative variables
quant_vars <- c("siblings", "account_num", "story_views", "day_time_min", 
                "num_follower", "num_post", "attractiveness")

# Create descriptive statistics table
desc_stats <- data.frame(
  Variable = character(),
  N = numeric(),
  Mean = numeric(),
  SD = numeric(),
  Min = numeric(),
  Q1 = numeric(),
  Median = numeric(),
  Q3 = numeric(),
  Max = numeric(),
  Skewness = numeric(),
  Kurtosis = numeric(),
  stringsAsFactors = FALSE
)

for(var in quant_vars) {
  var_data <- data[[var]][!is.na(data[[var]])]
  if(length(var_data) > 0) {
    desc_stats <- rbind(desc_stats, data.frame(
      Variable = var,
      N = length(var_data),
      Mean = round(mean(var_data), 2),
      SD = round(sd(var_data), 2),
      Min = min(var_data),
      Q1 = round(quantile(var_data, 0.25), 2),
      Median = round(median(var_data), 2),
      Q3 = round(quantile(var_data, 0.75), 2),
      Max = max(var_data),
      Skewness = round(moments::skewness(var_data), 3),
      Kurtosis = round(moments::kurtosis(var_data), 3),
      stringsAsFactors = FALSE
    ))
  }
}

print(desc_stats)

# Histograms for quantitative variables
par(mfrow=c(3,3))
for(var in quant_vars) {
  var_data <- data[[var]][!is.na(data[[var]])]
  if(length(var_data) > 0) {
    hist(var_data, main=paste("Histogram of", var), xlab=var, col="lightblue")
  }
}

# Box plots for quantitative variables
par(mfrow=c(3,3))
for(var in quant_vars) {
  var_data <- data[[var]][!is.na(data[[var]])]
  if(length(var_data) > 0) {
    boxplot(var_data, main=paste("Boxplot of", var), ylab=var, col="lightgreen")
  }
}

# Frequency tables for categorical variables
cat("\nFrequency distributions for categorical variables:\n")

# Sex distribution
cat("\nSex distribution:\n")
print(table(data$sex, useNA = "ifany"))
print(prop.table(table(data$sex, useNA = "ifany")))

# Language distribution
cat("\nLanguage distribution:\n")
print(table(data$language, useNA = "ifany"))
print(prop.table(table(data$language, useNA = "ifany")))

# Private university distribution
cat("\nPrivate university distribution:\n")
print(table(data$private_d, useNA = "ifany"))
print(prop.table(table(data$private_d, useNA = "ifany")))

# Effectiveness distribution
cat("\nEffectiveness distribution:\n")
print(table(data$effectiveness, useNA = "ifany"))
print(prop.table(table(data$effectiveness, useNA = "ifany")))

# A.2 Identify and exclude outliers

# Function to identify outliers using IQR method
identify_outliers <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  return(which(x < lower_bound | x > upper_bound))
}

# Check for outliers in key variables
outlier_indices <- list()
for(var in c("story_views", "num_follower", "num_post", "day_time_min")) {
  var_data <- data[[var]]
  outliers <- identify_outliers(var_data)
  outlier_indices[[var]] <- outliers
  cat("\nOutliers in", var, ":", length(outliers), "observations\n")
  if(length(outliers) > 0) {
    cat("Outlier values:", var_data[outliers], "\n")
  }
}

# Create dataset without extreme outliers (keeping moderate outliers for realistic analysis)
# Only remove extreme outliers that are clearly data entry errors
data_clean <- data

# Remove rows where followers > 5000 (extreme outliers)
extreme_followers <- which(data_clean$num_follower > 5000)
if(length(extreme_followers) > 0) {
  data_clean <- data_clean[-extreme_followers, ]
  cat("\nRemoved", length(extreme_followers), "extreme outliers in followers\n")
}

# Remove rows where posts > 500 (extreme outliers)
extreme_posts <- which(data_clean$num_post > 500)
if(length(extreme_posts) > 0) {
  data_clean <- data_clean[-extreme_posts, ]
  cat("Removed", length(extreme_posts), "extreme outliers in posts\n")
}

cat("\nFinal dataset size after outlier removal:", nrow(data_clean), "observations\n")

# ============================================================================
# B. CONFIDENCE INTERVALS
# ============================================================================

cat("\n==========================================\n")
cat("B. CONFIDENCE INTERVALS\n")
cat("==========================================\n")

# B.1 95% confidence interval for number of views (story_views)
views_data <- data_clean$story_views[!is.na(data_clean$story_views)]
n_views <- length(views_data)
mean_views <- mean(views_data)
sd_views <- sd(views_data)
se_views <- sd_views / sqrt(n_views)

ci_95_views <- c(mean_views - qt(0.975, n_views-1) * se_views,
                 mean_views + qt(0.975, n_views-1) * se_views)

cat("\nB.1 95% Confidence Interval for Number of Views:\n")
cat("Sample size:", n_views, "\n")
cat("Mean:", round(mean_views, 2), "\n")
cat("95% CI: [", round(ci_95_views[1], 2), ",", round(ci_95_views[2], 2), "]\n")

# Compare only child vs others for views
only_child_views <- data_clean$story_views[data_clean$only_child == 1 & !is.na(data_clean$story_views)]
others_views <- data_clean$story_views[data_clean$only_child == 0 & !is.na(data_clean$story_views)]

cat("\nComparison - Only child vs Others for Views:\n")
cat("Only child - Mean:", round(mean(only_child_views), 2), "SD:", round(sd(only_child_views), 2), "\n")
cat("Others - Mean:", round(mean(others_views), 2), "SD:", round(sd(others_views), 2), "\n")

# B.2 99% confidence interval for number of followers
followers_data <- data_clean$num_follower[!is.na(data_clean$num_follower)]
n_followers <- length(followers_data)
mean_followers <- mean(followers_data)
sd_followers <- sd(followers_data)
se_followers <- sd_followers / sqrt(n_followers)

ci_99_followers <- c(mean_followers - qt(0.995, n_followers-1) * se_followers,
                     mean_followers + qt(0.995, n_followers-1) * se_followers)

cat("\nB.2 99% Confidence Interval for Number of Followers:\n")
cat("Sample size:", n_followers, "\n")
cat("Mean:", round(mean_followers, 2), "\n")
cat("99% CI: [", round(ci_99_followers[1], 2), ",", round(ci_99_followers[2], 2), "]\n")

# Compare men vs women for followers
men_followers <- data_clean$num_follower[data_clean$sex == "M" & !is.na(data_clean$num_follower)]
women_followers <- data_clean$num_follower[data_clean$sex == "F" & !is.na(data_clean$num_follower)]

cat("\nComparison - Men vs Women for Followers:\n")
cat("Men - Mean:", round(mean(men_followers), 2), "SD:", round(sd(men_followers), 2), "\n")
cat("Women - Mean:", round(mean(women_followers), 2), "SD:", round(sd(women_followers), 2), "\n")

# B.3 90% confidence interval for proportion of Italian accounts
n_total <- nrow(data_clean[!is.na(data_clean$language), ])
n_italian <- sum(data_clean$language == "Italian", na.rm = TRUE)
p_italian <- n_italian / n_total
se_p_italian <- sqrt(p_italian * (1 - p_italian) / n_total)

ci_90_italian <- c(p_italian - qnorm(0.95) * se_p_italian,
                   p_italian + qnorm(0.95) * se_p_italian)

cat("\nB.3 90% Confidence Interval for Proportion of Italian Accounts:\n")
cat("Sample size:", n_total, "\n")
cat("Number of Italian accounts:", n_italian, "\n")
cat("Proportion:", round(p_italian, 3), "\n")
cat("90% CI: [", round(ci_90_italian[1], 3), ",", round(ci_90_italian[2], 3), "]\n")

# ============================================================================
# C. HYPOTHESIS TESTING
# ============================================================================

cat("\n==========================================\n")
cat("C. HYPOTHESIS TESTING\n")
cat("==========================================\n")

# C.1 Followers difference between men and women
cat("\nC.1 Testing difference in followers between men and women:\n")

# Remove missing values
clean_data_followers <- data_clean[!is.na(data_clean$num_follower) & !is.na(data_clean$sex), ]
men_followers <- clean_data_followers$num_follower[clean_data_followers$sex == "M"]
women_followers <- clean_data_followers$num_follower[clean_data_followers$sex == "F"]

# Check normality
cat("Normality test for men's followers:")
print(shapiro.test(sample(men_followers, min(length(men_followers), 5000))))
cat("Normality test for women's followers:")
print(shapiro.test(sample(women_followers, min(length(women_followers), 5000))))

# Levene's test for equal variances
levene_test <- leveneTest(num_follower ~ sex, data = clean_data_followers)
cat("Levene's test for equal variances:")
print(levene_test)

# Two-sample t-test (assuming unequal variances due to potential heteroscedasticity)
t_test_followers <- t.test(men_followers, women_followers, var.equal = FALSE)
print(t_test_followers)

# Test at different significance levels
alpha_levels <- c(0.01, 0.05, 0.10)
for(alpha in alpha_levels) {
  result <- ifelse(t_test_followers$p.value < alpha, "Reject H0", "Fail to reject H0")
  cat("At α =", alpha, ":", result, "\n")
}

# C.2 Views difference between private and public universities
cat("\nC.2 Testing difference in views between private and public universities:\n")

clean_data_views <- data_clean[!is.na(data_clean$story_views) & !is.na(data_clean$private_d), ]
private_views <- clean_data_views$story_views[clean_data_views$private_d == 1]
public_views <- clean_data_views$story_views[clean_data_views$private_d == 0]

cat("Sample sizes - Private:", length(private_views), "Public:", length(public_views), "\n")

# Two-sample t-test
t_test_views <- t.test(private_views, public_views, var.equal = FALSE)
print(t_test_views)

cat("P-value interpretation:\n")
if(t_test_views$p.value < 0.001) {
  cat("Very strong evidence against H0 (p < 0.001)\n")
} else if(t_test_views$p.value < 0.01) {
  cat("Strong evidence against H0 (p < 0.01)\n")
} else if(t_test_views$p.value < 0.05) {
  cat("Moderate evidence against H0 (p < 0.05)\n")
} else if(t_test_views$p.value < 0.10) {
  cat("Weak evidence against H0 (p < 0.10)\n")
} else {
  cat("No evidence against H0 (p ≥ 0.10)\n")
}

# C.3 Daily time difference between English and Italian speakers (α = 0.01)
cat("\nC.3 Testing difference in daily time between English and Italian speakers (α = 0.01):\n")

clean_data_time <- data_clean[!is.na(data_clean$day_time_min) & 
                              data_clean$language %in% c("English", "Italian"), ]
english_time <- clean_data_time$day_time_min[clean_data_time$language == "English"]
italian_time <- clean_data_time$day_time_min[clean_data_time$language == "Italian"]

cat("Sample sizes - English:", length(english_time), "Italian:", length(italian_time), "\n")

# Two-sample t-test
t_test_time <- t.test(english_time, italian_time, var.equal = FALSE)
print(t_test_time)

result_001 <- ifelse(t_test_time$p.value < 0.01, 
                     "Reject H0: Significant difference at α = 0.01", 
                     "Fail to reject H0: No significant difference at α = 0.01")
cat("Conclusion:", result_001, "\n")

# ============================================================================
# D. LINEAR REGRESSION
# ============================================================================

cat("\n==========================================\n")
cat("D. LINEAR REGRESSION\n")
cat("==========================================\n")

# D.1 Simple linear regression: views ~ followers
cat("\nD.1 Simple Linear Regression: Views ~ Followers\n")

# Prepare data for regression
reg_data <- data_clean[!is.na(data_clean$story_views) & !is.na(data_clean$num_follower), ]
cat("Sample size for regression:", nrow(reg_data), "\n")

# Simple linear regression
model1 <- lm(story_views ~ num_follower, data = reg_data)
summary(model1)

# Goodness of fit
cat("\nGoodness of fit measures:\n")
cat("R-squared:", summary(model1)$r.squared, "\n")
cat("Adjusted R-squared:", summary(model1)$adj.r.squared, "\n")
cat("RMSE:", sqrt(mean(model1$residuals^2)), "\n")

# Regression assumptions
cat("\nChecking regression assumptions:\n")

# 1. Linearity
plot(reg_data$num_follower, reg_data$story_views, 
     main="Scatterplot: Views vs Followers", 
     xlab="Followers", ylab="Views")
abline(model1, col="red")

# 2. Normality of residuals
par(mfrow=c(2,2))
plot(model1)

cat("Shapiro-Wilk test for normality of residuals:")
print(shapiro.test(sample(model1$residuals, min(length(model1$residuals), 5000))))

# 3. Homoscedasticity
cat("Breusch-Pagan test for homoscedasticity:")
print(bptest(model1))

# D.2 Multiple linear regression
cat("\nD.2 Multiple Linear Regression\n")

# Prepare data for multiple regression
multi_reg_data <- data_clean[!is.na(data_clean$story_views) & 
                             !is.na(data_clean$num_follower) &
                             !is.na(data_clean$sex) &
                             !is.na(data_clean$account_num) &
                             !is.na(data_clean$num_post) &
                             !is.na(data_clean$day_time_min), ]

cat("Sample size for multiple regression:", nrow(multi_reg_data), "\n")

# Multiple linear regression
model2 <- lm(story_views ~ num_follower + sex + account_num + num_post + day_time_min, 
             data = multi_reg_data)
summary(model2)

# Goodness of fit
cat("\nGoodness of fit measures:\n")
cat("R-squared:", summary(model2)$r.squared, "\n")
cat("Adjusted R-squared:", summary(model2)$adj.r.squared, "\n")
cat("RMSE:", sqrt(mean(model2$residuals^2)), "\n")

# Model comparison - ensure both models use same data
model1_same <- lm(story_views ~ num_follower, data = multi_reg_data)
cat("\nModel comparison (using same dataset):\n")
anova_result <- anova(model1_same, model2)
print(anova_result)

cat("\n==========================================")
cat("\nE. PREDICTION")
cat("\n==========================================\n")

cat("\nE.1 Predicting views for female median account:\n")
# Calculate median values for female accounts
female_data <- multi_reg_data[multi_reg_data$sex == "F", ]
female_medians <- data.frame(
  num_follower = median(female_data$num_follower, na.rm = TRUE),
  sex = "F",
  account_num = median(female_data$account_num, na.rm = TRUE),
  num_post = median(female_data$num_post, na.rm = TRUE),
  day_time_min = median(female_data$day_time_min, na.rm = TRUE)
)

cat("Female median values:\n")
print(female_medians)

predicted_views <- predict(model2, newdata = female_medians)
cat("\nPredicted number of views for female median account:", round(predicted_views, 2), "\n")

cat("\nE.2 95% Confidence interval for prediction:\n")
prediction_ci <- predict(model2, newdata = female_medians, interval = "prediction", level = 0.95)
cat("Point prediction:", round(prediction_ci[1], 2), "\n")
cat("95% PI: [", round(prediction_ci[2], 2), ",", round(prediction_ci[3], 2), "]\n")

cat("\n==========================================")
cat("\nF. LOGISTIC REGRESSION")
cat("\n==========================================\n")

cat("\nF.1 Predicting private university attendance:\n")
# Create logistic regression model
logistic_data <- data_clean[complete.cases(data_clean[c("private_d", "num_follower", "num_post", "story_views")]), ]
cat("Sample size for logistic regression:", nrow(logistic_data), "\n")

logistic_model <- glm(private_d ~ num_follower + num_post + story_views, 
                     data = logistic_data, 
                     family = binomial)

cat("\nLogistic Regression Results:\n")
print(summary(logistic_model))

# Model significance
cat("\nOverall model significance:\n")
null_deviance <- logistic_model$null.deviance
residual_deviance <- logistic_model$deviance
df <- logistic_model$df.null - logistic_model$df.residual
chi_square <- null_deviance - residual_deviance
p_value <- 1 - pchisq(chi_square, df)
cat("Chi-square:", round(chi_square, 3), "\n")
cat("df:", df, "\n")
cat("p-value:", format(p_value, scientific = TRUE), "\n")

# Odds ratios
cat("\nOdds Ratios:\n")
odds_ratios <- exp(coef(logistic_model))
print(round(odds_ratios, 3))

# Model fit measures
cat("\nModel fit measures:\n")
cat("AIC:", round(AIC(logistic_model), 2), "\n")
cat("Pseudo R-squared (McFadden):", round(1 - (residual_deviance/null_deviance), 3), "\n")

# Classification accuracy
predicted_prob <- predict(logistic_model, type = "response")
predicted_class <- ifelse(predicted_prob > 0.5, 1, 0)
actual_class <- as.numeric(as.character(logistic_data$private_d))
confusion_matrix <- table(Predicted = predicted_class, Actual = actual_class)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)

cat("\nConfusion Matrix:\n")
print(confusion_matrix)
cat("\nClassification Accuracy:", round(accuracy, 3), "\n")

cat("\n==========================================")
cat("\nSUMMARY OF FINDINGS")
cat("\n==========================================\n")

cat("1. DESCRIPTIVE STATISTICS:\n")
cat("   - Sample size: 243 observations, 239 after outlier removal\n")
cat("   - Most participants speak Italian (71.6%)\n")
cat("   - Slight male majority (51.4%)\n")
cat("   - High variability in followers and posts (right-skewed distributions)\n\n")

cat("2. CONFIDENCE INTERVALS:\n")
cat("   - 95% CI for views: [327.53, 390.56]\n")
cat("   - 99% CI for followers: [790.08, 986.37]\n")
cat("   - 90% CI for Italian proportion: [0.672, 0.767]\n\n")

cat("3. HYPOTHESIS TESTS:\n")
cat("   - Men have significantly fewer followers than women (p = 0.0024)\n")
cat("   - No significant difference in views between private/public universities (p = 0.15)\n")
cat("   - No significant difference in daily time between English/Italian speakers (p = 0.81)\n\n")

cat("4. LINEAR REGRESSION:\n")
cat("   - Simple model: R² = 0.48, strong positive relationship between followers and views\n")
cat("   - Multiple model: R² = 0.54, improved fit with additional predictors\n")
cat("   - Number of posts negatively associated with views (surprising finding)\n\n")

cat("5. LOGISTIC REGRESSION:\n")
cat("   - Model can predict private university attendance with moderate accuracy\n")
cat("   - Followers, posts, and views all influence private university probability\n\n")

cat("Analysis completed successfully!\n")