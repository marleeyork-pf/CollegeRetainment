# Loading in the data
install.packages("readxl")
install.packages("knitr")
install.packages("kableExtra")
library(readxl)
library(dplyr)
library(emmeans)
library(knitr)
library(kableExtra)

# Helper function for clean table output
fmt_table <- function(df, caption = NULL) {
  kable(df, caption = caption, digits = 3, format = "pipe", row.names = FALSE) %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                  full_width = FALSE)
}

# Loading and inspecting data
df <- read.csv("EGR599_Tracker_Data.csv")
colnames(df)
str(df)

# Cleaning ######################################################################
df$Term.4.Term.GPA[df$Term.4.Term.GPA == "n/a"] <- NA
df$Term.4.Term.GPA[df$Term.4.Term.GPA == ""]    <- NA
df$Term.4.Term.GPA <- as.integer(df$Term.4.Term.GPA)

# Factorizing categorical variables
df$EGR.599              <- as.factor(df$EGR.599)
df$Sex                  <- as.factor(df$Sex)
df$Primary.Academic.Plan <- as.factor(df$Primary.Academic.Plan)

# India citizenship indicator
df$India.Citizen <- ifelse(df$Country.of.Citizenship == "India", 1, 0)

# Retention indicators
df$Retained.2 <- ifelse(is.na(df$Term.2.Enrolled.Units), 0, 1)
df$Retained.2[(df$X.Semesters.before.graduation < 2) &
                !(is.na(df$X.Semesters.before.graduation))] <- NA

df$Retained.3 <- ifelse(is.na(df$Term.3.Enrolled.Units), 0, 1)
df$Retained.3[(df$X.Semesters.before.graduation < 3) &
                !(is.na(df$X.Semesters.before.graduation))] <- NA

df$Retained.4 <- ifelse(is.na(df$Term.4.Enrolled.Units), 0, 1)
df$Retained.4[(df$X.Semesters.before.graduation < 4) &
                !(is.na(df$X.Semesters.before.graduation))] <- NA

# Graduation cumulative GPA
df$Graduation.Cum.GPA <- rep(NA, nrow(df))
for (s in 2:4) {
  idx <- df$X.Semesters.before.graduation == s & !is.na(df$X.Semesters.before.graduation)
  col <- paste0("Term.", s, ".Cum.GPA")
  df$Graduation.Cum.GPA[idx] <- df[[col]][idx]
}

# Verification ################################################################
cat("\n--- Retention & GPA Summary by EGR.599 (Term 2) ---\n")
df %>%
  group_by(EGR.599) %>%
  summarize(
    N             = n(),
    Retained      = sum(Retained.2, na.rm = TRUE),
    Retention_Rate = round(mean(Retained.2, na.rm = TRUE), 3),
    Mean_Cum_GPA  = round(mean(Term.2.Cum.GPA, na.rm = TRUE), 3)
  ) %>%
  fmt_table(caption = "Term 2 Retention & Cumulative GPA by EGR 599 Enrollment")

cat("\n--- Retention & GPA Summary by EGR.599 (Term 3) ---\n")
df %>%
  filter(!is.na(Retained.3)) %>%
  group_by(EGR.599) %>%
  summarize(
    N             = n(),
    Retained      = sum(Retained.3, na.rm = TRUE),
    Retention_Rate = round(mean(Retained.3, na.rm = TRUE), 3),
    Mean_Cum_GPA  = round(mean(Term.3.Cum.GPA, na.rm = TRUE), 3)
  ) %>%
  fmt_table(caption = "Term 3 Retention & Cumulative GPA by EGR 599 Enrollment")

cat("\n--- Retention & GPA Summary by EGR.599 (Term 4) ---\n")
df %>%
  filter(!is.na(Retained.4)) %>%
  group_by(EGR.599) %>%
  summarize(
    N             = n(),
    Retained      = sum(Retained.4, na.rm = TRUE),
    Retention_Rate = round(mean(Retained.4, na.rm = TRUE), 3),
    Mean_Cum_GPA  = round(mean(Term.4.Cum.GPA, na.rm = TRUE), 3)
  ) %>%
  fmt_table(caption = "Term 4 Retention & Cumulative GPA by EGR 599 Enrollment")

# Hypothesis Testing: Retention ################################################

# Helper to extract prop.test results into a clean data frame
prop_test_table <- function(retained, total, label) {
  pt <- prop.test(retained, total)
  data.frame(
    Group         = label,
    N             = total,
    Retained      = retained,
    Proportion    = round(retained / total, 3),
    Chi_Sq        = round(pt$statistic, 3),
    P_Value       = round(pt$p.value, 4),
    CI_Lower      = round(pt$conf.int[1], 3),
    CI_Upper      = round(pt$conf.int[2], 3)
  )
}

sum_ret2 <- df %>%
  group_by(EGR.599) %>%
  summarize(retained = sum(Retained.2 == 1, na.rm = TRUE), total = n())

sum_ret3 <- df %>%
  filter(!is.na(Retained.3)) %>%
  group_by(EGR.599) %>%
  summarize(retained = sum(Retained.3 == 1, na.rm = TRUE), total = n())

sum_ret4 <- df %>%
  filter(!is.na(Retained.4)) %>%
  group_by(EGR.599) %>%
  summarize(retained = sum(Retained.4 == 1, na.rm = TRUE), total = n())

# Two-proportions test results tables
prop_test_fmt <- function(pt_result, label) {
  data.frame(
    Comparison    = label,
    Chi_Sq        = round(pt_result$statistic, 3),
    df            = pt_result$parameter,
    P_Value       = round(pt_result$p.value, 4),
    Prop_No_EGR599 = round(pt_result$estimate[1], 3),
    Prop_EGR599    = round(pt_result$estimate[2], 3),
    CI_Lower      = round(pt_result$conf.int[1], 3),
    CI_Upper      = round(pt_result$conf.int[2], 3),
    p_value       = round(pt_result$p.value, 3)
  )
}

bind_rows(
  prop_test_fmt(prop.test(sum_ret2$retained, sum_ret2$total), "Term 1 → Term 2"),
  prop_test_fmt(prop.test(sum_ret3$retained, sum_ret3$total), "Term 1 → Term 3"),
  prop_test_fmt(prop.test(sum_ret4$retained, sum_ret4$total), "Term 1 → Term 4")
) %>%
  fmt_table(caption = "Two-Proportions Test: Retention Rates by EGR 599 Enrollment")

# Logistic Regression: Retention ###############################################

# Helper to extract glm coefficients into a tidy table
glm_table <- function(model, caption = NULL) {
  s  <- summary(model)$coefficients
  or <- exp(coef(model))
  data.frame(
    Term      = rownames(s),
    Estimate  = round(s[, "Estimate"], 3),
    Std_Error = round(s[, "Std. Error"], 3),
    Z_Value   = round(s[, "z value"], 3),
    P_Value   = round(s[, "Pr(>|z|)"], 4),
    Odds_Ratio = round(or, 3)
  ) %>% fmt_table(caption = caption)
}

# EGR.599-only models
cat("\n--- Logistic Regression: Retained.2 ~ EGR.599 ---\n")
model_ret2_EGR599 <- glm(Retained.2 ~ EGR.599, family = "binomial", data = df)
glm_table(model_ret2_EGR599, "Logistic Regression: Term 2 Retention ~ EGR 599 Only")

cat("\n--- Logistic Regression: Retained.3 ~ EGR.599 ---\n")
model_ret3_EGR599 <- glm(Retained.3 ~ EGR.599, family = "binomial", data = df)
glm_table(model_ret3_EGR599, "Logistic Regression: Term 3 Retention ~ EGR 599 Only")

cat("\n--- Logistic Regression: Retained.4 ~ EGR.599 ---\n")
model_ret4_EGR599 <- glm(Retained.4 ~ EGR.599, family = "binomial", data = df)
glm_table(model_ret4_EGR599, "Logistic Regression: Term 4 Retention ~ EGR 599 Only")

# Full models with covariates
model_ret2 <- glm(Retained.2 ~ EGR.599 + Sex + India.Citizen + Primary.Academic.Plan,
                  data = df, family = "binomial")
model_ret3 <- glm(Retained.3 ~ EGR.599 + Sex + India.Citizen + Primary.Academic.Plan,
                  data = df, family = "binomial")
model_ret4 <- glm(Retained.4 ~ EGR.599 + Sex + India.Citizen + Primary.Academic.Plan,
                  data = df, family = "binomial")

glm_table(model_ret2, "Logistic Regression: Term 2 Retention (Full Model)")
glm_table(model_ret3, "Logistic Regression: Term 3 Retention (Full Model)")
glm_table(model_ret4, "Logistic Regression: Term 4 Retention (Full Model)")

# Reduced models + LRT
model_ret2_red <- glm(Retained.2 ~ Sex + India.Citizen + Primary.Academic.Plan,
                      data = df, family = "binomial")
model_ret3_red <- glm(Retained.3 ~ Sex + India.Citizen + Primary.Academic.Plan,
                      data = df, family = "binomial")
model_ret4_red <- glm(Retained.4 ~ Sex + India.Citizen + Primary.Academic.Plan,
                      data = df, family = "binomial")

lrt_table <- function(anova_res, label) {
  data.frame(
    Comparison  = label,
    Resid_Df_1  = anova_res$`Resid. Df`[1],
    Resid_Df_2  = anova_res$`Resid. Df`[2],
    Df          = anova_res$Df[2],
    Deviance    = round(anova_res$Deviance[2], 3),
    P_Value     = round(anova_res$`Pr(>Chi)`[2], 4)
  )
}

bind_rows(
  lrt_table(anova(model_ret2_red, model_ret2, test = "Chisq"), "Term 2: Reduced vs Full"),
  lrt_table(anova(model_ret3_red, model_ret3, test = "Chisq"), "Term 3: Reduced vs Full"),
  lrt_table(anova(model_ret4_red, model_ret4, test = "Chisq"), "Term 4: Reduced vs Full")
) %>%
  fmt_table(caption = "Likelihood Ratio Tests: Effect of Adding EGR 599 to Retention Models")

# Hypothesis Testing: GPA ######################################################

# Helper to extract t.test results into a tidy table
t_test_table <- function(formula, data, label) {
  tt <- t.test(formula, data = data)
  data.frame(
    Comparison    = label,
    Mean_No_EGR599 = round(tt$estimate[1], 3),
    Mean_EGR599    = round(tt$estimate[2], 3),
    Mean_Diff      = round(diff(tt$estimate), 3),
    T_Statistic   = round(tt$statistic, 3),
    df            = round(tt$parameter, 1),
    P_Value       = round(tt$p.value, 4),
    CI_Lower      = round(tt$conf.int[1], 3),
    CI_Upper      = round(tt$conf.int[2], 3)
  )
}

bind_rows(
  t_test_table(Term.1.Term.GPA ~ EGR.599, df, "Term 1 GPA"),
  t_test_table(Term.2.Term.GPA ~ EGR.599, df, "Term 2 GPA"),
  t_test_table(Term.3.Term.GPA ~ EGR.599, df, "Term 3 GPA"),
  t_test_table(Term.4.Term.GPA ~ EGR.599, df, "Term 4 GPA"),
  t_test_table(Term.4.Cum.GPA  ~ EGR.599, df, "Term 4 Cumulative GPA"),
  t_test_table(Graduation.Cum.GPA ~ EGR.599, df, "Graduation Cumulative GPA")
) %>%
  fmt_table(caption = "Welch Two-Sample t-Tests: GPA by EGR 599 Enrollment")

# ANCOVA/MLR: GPA ###############################################################

# Helper to extract lm coefficients into a tidy table
lm_table <- function(model, caption = NULL) {
  s <- summary(model)$coefficients
  data.frame(
    Term        = rownames(s),
    Estimate    = round(s[, "Estimate"], 3),
    Std_Error   = round(s[, "Std. Error"], 3),
    T_Value     = round(s[, "t value"], 3),
    P_Value     = round(s[, "Pr(>|t|)"], 4)
  ) %>% fmt_table(caption = caption)
}

Grad_GPA_reg <- lm(Graduation.Cum.GPA ~ EGR.599 + Sex + India.Citizen +
                     Primary.Academic.Plan, data = df)
lm_table(Grad_GPA_reg, "ANCOVA: Graduation Cumulative GPA")

# emmeans for ANCOVA adjusted means
emmeans(Grad_GPA_reg, ~ EGR.599) %>%
  as.data.frame() %>%
  rename(EGR_599 = EGR.599, Adj_Mean = emmean) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
  fmt_table(caption = "Estimated Marginal Means: Graduation GPA by EGR 599 (Adjusted)")

Term1_GPA_reg <- lm(Term.1.Cum.GPA ~ EGR.599 + Sex + India.Citizen +
                      Primary.Academic.Plan, data = df)
lm_table(Term1_GPA_reg, "ANCOVA: Term 1 Cumulative GPA")

# Interaction models
Grad_GPA_gender_reg      <- lm(Graduation.Cum.GPA ~ EGR.599 * Sex + India.Citizen +
                                 Primary.Academic.Plan, data = df)
Grad_GPA_nationality_reg <- lm(Graduation.Cum.GPA ~ EGR.599 * India.Citizen + Sex +
                                 Primary.Academic.Plan, data = df)
Grad_GPA_major_reg       <- lm(Graduation.Cum.GPA ~ EGR.599 * Primary.Academic.Plan +
                                 Sex + India.Citizen, data = df)

lm_table(Grad_GPA_gender_reg,      "ANCOVA: Graduation GPA ~ EGR599 × Sex Interaction")
lm_table(Grad_GPA_nationality_reg, "ANCOVA: Graduation GPA ~ EGR599 × Nationality Interaction")
lm_table(Grad_GPA_major_reg,       "ANCOVA: Graduation GPA ~ EGR599 × Major Interaction")