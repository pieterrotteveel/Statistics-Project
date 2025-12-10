# -----------------------------------------------------------------------------
# 1. SETUP AND LIBRARIES
# -----------------------------------------------------------------------------
library(ggplot2)
library(dplyr)
library(car)      
library(lmtest)   
library(nortest)  
library(gridExtra)

raw_data <- read.csv("data/30457_Assignment_Data_2025_2026.csv", sep=";", header=TRUE)

# -----------------------------------------------------------------------------
# 2. DATA CLEANING
# -----------------------------------------------------------------------------

# Remove rows with ANY NA values to preserve row alignment
data <- na.omit(raw_data)

# Convert Categorical variables to Factors (Important for Regression)
data$sex <- as.factor(data$sex)
data$language <- as.factor(data$language)
data$private_d <- as.factor(data$private_d) # 0 = Public, 1 = Private

# -----------------------------------------------------------------------------
# SECTION A: Descriptive Statistics
# -----------------------------------------------------------------------------

cat("\n--- SECTION A: Descriptive Statistics ---\n")

# A.1 Summary of all variables
print(summary(data))

# Helper function to calculate detailed stats for numeric variables
calc_stats <- function(x) {
  list(
    Mean = mean(x), Median = median(x), SD = sd(x), 
    Var = var(x), IQR = IQR(x),
    Min = min(x), Max = max(x)
  )
}

# numeric variables of interest
numeric_vars <- c("account_num", "story_views", "day_time_min", "num_follower", "num_post")

for(var in numeric_vars) {
  cat(paste("\nStatistics for:", var, "\n"))
  print(unlist(calc_stats(data[[var]])))
  
  # Generate Plots automatically
  p1 <- ggplot(data, aes_string(y = var)) + 
    geom_boxplot(fill="orange") + theme_minimal() + ggtitle(paste("Boxplot:", var))
  
  p2 <- ggplot(data, aes_string(x = var)) + 
    geom_histogram(fill="skyblue", col="black", bins=30) + theme_minimal() + ggtitle(paste("Hist:", var))
  
  # Save plots
  ggsave(paste0("results/A1_", var, "_boxplot.png"), p1, width=6, height=4)
  ggsave(paste0("results/A1_", var, "_hist.png"), p2, width=6, height=4)
}

# A.2 Outlier Removal
# We will identify outliers based on IQR, but we must filter the DATAFRAME.

# Function to check bounds
is_not_outlier <- function(x) {
  Q1 <- quantile(x, 0.25)
  Q3 <- quantile(x, 0.75)
  IQR_val <- IQR(x)
  lower <- Q1 - 1.5 * IQR_val
  upper <- Q3 + 1.5 * IQR_val
  return(x >= lower & x <= upper)
}

# Apply outlier filtering to the dataset for the relevant variables
# Note: This removes a row if it is an outlier in ANY of these columns
data_clean <- data %>%
  filter(is_not_outlier(account_num) &
         is_not_outlier(story_views) &
         is_not_outlier(day_time_min) &
         is_not_outlier(num_follower) &
         is_not_outlier(num_post))

cat(paste("\nOriginal Rows:", nrow(data), "| Rows after outlier removal:", nrow(data_clean), "\n"))

# Save cleaned data
write.csv(data_clean, "data/cleaned_data_final_no_outliers.csv", row.names = FALSE)

# -----------------------------------------------------------------------------
# SECTION B: Confidence Intervals
# -----------------------------------------------------------------------------
cat("\n--- SECTION B: Confidence Intervals ---\n")

# B.1 95% CI for Views (Overall & Comparison)
t_views <- t.test(data_clean$story_views, conf.level = 0.95)
cat("\n[B.1] 95% CI Story Views (Overall):\n"); print(t_views$conf.int)

# Compare Only Child vs Others
# Note: Ensure 'siblings' is numeric or handle appropriately
only_child <- subset(data_clean, siblings == 0)$story_views
others <- subset(data_clean, siblings > 0)$story_views

cat("\n[B.1] Only Child Mean Views:", mean(only_child))
cat("\n[B.1] With Siblings Mean Views:", mean(others), "\n")
# (Optional: You can run t.tests for these groups individually if required)

# B.2 99% CI for Followers (Men vs Women)
t_fol <- t.test(data_clean$num_follower, conf.level = 0.99)
cat("\n[B.2] 99% CI Followers (Overall):\n"); print(t_fol$conf.int)

men_fol <- subset(data_clean, sex == 'M')$num_follower
women_fol <- subset(data_clean, sex == 'F')$num_follower

t_men <- t.test(men_fol, conf.level = 0.99)
t_women <- t.test(women_fol, conf.level = 0.99)
cat("\n[B.2] 99% CI Men:", t_men$conf.int)
cat("\n[B.2] 99% CI Women:", t_women$conf.int, "\n")

# B.3 90% CI Proportion Italian
n_italian <- sum(data_clean$language == "Italian")
prop_it <- prop.test(n_italian, nrow(data_clean), conf.level = 0.90)
cat("\n[B.3] 90% CI Proportion Italian:\n"); print(prop_it$conf.int)

# -----------------------------------------------------------------------------
# SECTION C: Hypothesis Testing
# -----------------------------------------------------------------------------
cat("\n--- SECTION C: Hypothesis Testing ---\n")

# C.1 Followers significantly different between Men and Women?
test_c1 <- t.test(num_follower ~ sex, data = data_clean)
print(test_c1)
# Note: Compare p-value to alpha (0.05, 0.01, etc.)

# C.2 Views different between Private and Public Uni?
test_c2 <- t.test(story_views ~ private_d, data = data_clean)
print(test_c2)

# C.3 Daily Time different between English and Italian?
eng_time <- subset(data_clean, language == "English")$day_time_min
it_time <- subset(data_clean, language == "Italian")$day_time_min
test_c3 <- t.test(eng_time, it_time, conf.level = 0.99) 
# Note: Prompt asked for alpha=0.01, so we check p-value against 0.01
print(test_c3)

# -----------------------------------------------------------------------------
# SECTION D: Linear Regression
# -----------------------------------------------------------------------------
cat("\n--- SECTION D: Linear Regression ---\n")

# D.1 Simple Linear Regression (Views ~ Followers)
model_simple <- lm(story_views ~ num_follower, data = data_clean)
cat("\n[D.1] Simple Model Summary:\n")
print(summary(model_simple))

# D.1 Assumptions Check
# 1. Linearity: Plot residuals vs Fitted (Visual)
png("results/D1_residuals_plot.png")
plot(model_simple, which = 1)
dev.off()

# 2. Normality of Residuals (Shapiro-Wilk Test)
# H0: Residuals are normal. p < 0.05 implies non-normal.
shapiro_res <- shapiro.test(residuals(model_simple))
cat("\n[D.1] Normality Test (Shapiro): p-value =", shapiro_res$p.value, "\n")

# 3. Homoscedasticity (Breusch-Pagan Test)
# H0: Variance is constant. p < 0.05 implies heteroscedasticity.
bp_res <- bptest(model_simple)
cat("\n[D.1] Homoscedasticity Test (Breusch-Pagan): p-value =", bp_res$p.value, "\n")

# D.2 Multiple Linear Regression
model_multi <- lm(story_views ~ num_follower + sex + account_num + num_post + day_time_min, data = data_clean)
cat("\n[D.2] Multiple Model Summary:\n")
print(summary(model_multi))

# Check VIF (Multicollinearity)
cat("\n[D.2] VIF Scores:\n")
print(vif(model_multi))

# -----------------------------------------------------------------------------
# SECTION E: Prediction
# -----------------------------------------------------------------------------
cat("\n--- SECTION E: Prediction ---\n")

# Define Median Female Profile
# Important: Use the same levels as the model expects
median_female <- data.frame(
  sex = factor("F", levels = levels(data_clean$sex)), # Ensure factor level matches
  num_follower = median(data_clean$num_follower),
  account_num = median(data_clean$account_num),
  num_post = median(data_clean$num_post),
  day_time_min = median(data_clean$day_time_min)
)

# E.1 Prediction
pred_val <- predict(model_multi, newdata = median_female)
cat("\n[E.1] Predicted Views:", pred_val, "\n")

# E.2 95% CI for the prediction
pred_ci <- predict(model_multi, newdata = median_female, interval = "confidence", level = 0.95)
cat("\n[E.2] Prediction CI:\n"); print(pred_ci)

# -----------------------------------------------------------------------------
# SECTION F: Logistic Regression
# -----------------------------------------------------------------------------
cat("\n--- SECTION F: Logistic Regression ---\n")

# F.1 Predict Private University (private_d)
# Ensure private_d is a factor or 0/1 numeric. glm works best with 0/1 or factor.
model_logit <- glm(private_d ~ num_follower + num_post + story_views, 
                   data = data_clean, 
                   family = binomial)

print(summary(model_logit))

# Odds Ratios
cat("\n[F.1] Odds Ratios:\n")
print(exp(coef(model_logit)))

# Evaluate Fit (Pseudo R2 or Confusion Matrix - Optional but good)
# Null deviance vs Residual deviance in summary output gives an idea of fit.