# Loading in the data
install.packages("readxl")
library(readxl)
library(dplyr)
library(emmeans)

# Loading and inspecting data
df <- read.csv("EGR599_Tracker_Data.csv")
colnames(df)
str(df)

# Cleaning #####################################################################
# Converting term 4 GPA to an integer
df$Term.4.Term.GPA[df$Term.4.Term.GPA=="n/a"] <- NA
df$Term.4.Term.GPA[df$Term.4.Term.GPA==""] <- NA
df$Term.4.Term.GPA <- as.integer(df$Term.4.Term.GPA)

# How does participation in EGR599 improve students 1st semester retention? ####
colnames(df)
df$EGR.599
df$Term.2.Enrolled.Units

# Factorizing categorical variables
df$EGR.599 <- as.factor(df$EGR.599)
df$Sex <- as.factor(df$Sex)
df$Primary.Academic.Plan <- as.factor(df$Primary.Academic.Plan)

# Create binary indicator of whether they are from India or not
# 1 = Indian citizen
# 0 = citizen of some other country (bangladesh, Bolivia, Colombia, etc)
df$India.Citizen <- ifelse(df$Country.of.Citizenship=="India",1,0)

# Creating indicator variables for enrollment in each semester
# 0 = Did not enroll for semester
# 1 = Enrolled for semester
# NA = graduated before the semester
df$Retained.2 <- ifelse(is.na(df$Term.2.Enrolled.Units),0,1)
df$Retained.2[(df$X.Semesters.before.graduation < 2) & !(is.na(df$X.Semesters.before.graduation))] <- NA

df$Retained.3 <- ifelse(is.na(df$Term.3.Enrolled.Units),0,1)
df$Retained.3[(df$X.Semesters.before.graduation < 3) & !(is.na(df$X.Semesters.before.graduation))] <- NA

df$Retained.4 <- ifelse(is.na(df$Term.4.Enrolled.Units),0,1)
df$Retained.4[(df$X.Semesters.before.graduation < 4) & !(is.na(df$X.Semesters.before.graduation))] <- NA

# Creating column for cumulative GPA at time of graduation
df$Graduation.Cum.GPA <- rep(NA,nrow(df))
df$Graduation.Cum.GPA[df$X.Semesters.before.graduation==2 & !is.na(df$X.Semesters.before.graduation)] <- df$Term.2.Cum.GPA[df$X.Semesters.before.graduation==2 & !is.na(df$X.Semesters.before.graduation)]
df$Graduation.Cum.GPA[df$X.Semesters.before.graduation==3 & !is.na(df$X.Semesters.before.graduation)] <- df$Term.3.Cum.GPA[df$X.Semesters.before.graduation==3 & !is.na(df$X.Semesters.before.graduation)]
df$Graduation.Cum.GPA[df$X.Semesters.before.graduation==4 & !is.na(df$X.Semesters.before.graduation)] <- df$Term.4.Cum.GPA[df$X.Semesters.before.graduation==4 & !is.na(df$X.Semesters.before.graduation)]

# Verifying ####################################################################
# Checking that I get the same retention counts as Dylan: verified!
df %>% 
  group_by(EGR.599) %>% 
  summarize(sum_retained = sum(Retained.2),   # This matches with first semester retainment
            mean_GPA = mean(Term.2.Cum.GPA, na.rm=TRUE)) # Matches
            
df %>% 
  filter(!is.na(Retained.3)) %>% 
  group_by(EGR.599) %>% 
  summarize(sum_retained = sum(Retained.3), # Matches
            mean_GPA = mean(Term.3.Cum.GPA, na.rm=TRUE)) # Matches

df %>% 
  filter(!is.na(Retained.4)) %>% 
  group_by(EGR.599) %>% 
  summarize(sum_retained = sum(Retained.4), # Matches
            mean_GPA = mean(Term.4.Cum.GPA, na.rm=TRUE)) # Matches

# Hypothesis Testing: Retainment ###############################################
# Calculating the probabilities of semester enrollment when with and without EGR599
sum_ret2 <- df %>% 
  group_by(EGR.599) %>% 
  summarize(retention_rate = mean(Retained.2),
            retained = sum(Retained.2 == 1, na.rm=TRUE),
            total = n())
sum_ret2
prop.test(sum_ret2$retained, sum_ret2$total) # Insignificant p-value for 1st-2nd retention


sum_ret3 <- df %>% 
  group_by(EGR.599) %>% 
  filter(!is.na(Retained.3)) %>% 
  summarize(retention_rate = mean(Retained.3),
            retained = sum(Retained.3 == 1, na.rm=TRUE),
            total = n())
sum_ret3
prop.test(sum_ret3$retained, sum_ret3$total) # Insignificant p-value for 1st-3rd retention

sum_ret4 <- df %>% 
  filter(!is.na(Retained.4)) %>% 
  group_by(EGR.599) %>% 
  summarize(retention_rate = mean(Retained.4),
            retained = sum(Retained.4 == 1, na.rm=TRUE),
            total = n())
sum_ret4
prop.test(sum_ret4$retained, sum_ret4$total) # Insignificant p-value for 1st-4th retention

# Logistic Regression: Retainment ##############################################

# Performing logistic regression with EGR.599 only
model_ret2_EGR599 <- glm(Retained.2 ~ EGR.599, family="binomial", data=df)
summary(model_ret2_EGR599)

model_ret3_EGR599 <- glm(Retained.3 ~ EGR.599, family="binomial", data=df)
summary(model_ret3_EGR599)

model_ret4_EGR599 <- glm(Retained.4 ~ EGR.599, family="binomial", data=df)
summary(model_ret4_EGR599)

# Performing logistic regression with covariates of interest: gender, nationality, and major
# Retention from first to second semester
model_ret2 <- glm(
  Retained.2 ~ EGR.599 + Sex + India.Citizen + Primary.Academic.Plan, 
  data=df,
  family="binomial"
)
summary(model_ret2) # None of the predictors are significant in this model, IT major comes closest

# Retention from first to third semester
model_ret3 <- glm(
  Retained.3 ~ EGR.599 + Sex + India.Citizen + Primary.Academic.Plan, 
  data=df,
  family="binomial"
)
summary(model_ret3) # Being an Indian citizen is an important predictor in this

# Retention from first to fourth semester
model_ret4 <- glm(
  Retained.4 ~ EGR.599 + Sex + India.Citizen + Primary.Academic.Plan, 
  data=df,
  family="binomial"
)
summary(model_ret4) # Being an Indian citizen and in IT 

# Reducing model to not include orientation and performing likelihood ratio test
# First to second semester
model_ret2_red <- glm(
  Retained.2 ~ Sex + India.Citizen + Primary.Academic.Plan, 
  data=df,
  family="binomial"
)
summary(model_ret2_red) # Same -- no significant predictors
anova(model_ret2_red,model_ret2,test="Chisq") # LLT says no difference

# First to third semester
model_ret3_red <- glm(
  Retained.3 ~ Sex + India.Citizen + Primary.Academic.Plan, 
  data=df,
  family="binomial"
)
summary(model_ret3_red) # Same -- only Indian citizenship is important
anova(model_ret3_red,model_ret3,test="Chisq") # LLT says no difference

# First to fourth semester
model_ret4_red <- glm(
  Retained.4 ~ Sex + India.Citizen + Primary.Academic.Plan, 
  data=df,
  family="binomial"
)
summary(model_ret4_red) # Same -- Indian citizenship and IT major are important
anova(model_ret3_red,model_ret3,test="Chisq") # LLT says no difference

# Hypothesis Testing: GPA ######################################################
# Normality of GPA
hist(df$Term.1.Term.GPA)
hist(df$Term.2.Term.GPA)
hist(df$Term.3.Term.GPA)
hist(df$Term.4.Term.GPA)
hist(df$Term.3.Cum.GPA)
hist(df$Term.4.Cum.GPA)

# t test for first semester GPA
boxplot(Term.1.Term.GPA ~ EGR.599, data=df)
t.test(Term.1.Term.GPA ~ EGR.599, data=df) # significant

# t test for second semester GPA
boxplot(Term.2.Term.GPA ~ EGR.599, data=df)
t.test(Term.2.Term.GPA ~ EGR.599, data=df) # significant, but such a minor value

# t test for third semester GPA
boxplot(Term.3.Term.GPA ~ EGR.599, data=df)
t.test(Term.3.Term.GPA ~ EGR.599, data=df) # significantly lower GPA for those in orientation

# t test for fourth semester GPA
boxplot(Term.4.Term.GPA ~ EGR.599, data=df)
t.test(Term.4.Term.GPA ~ EGR.599, data=df) # insignificant

# t test for cum GPA in fourth semester
boxplot(Term.4.Cum.GPA ~ EGR.599, data=df)
t.test(Term.4.Cum.GPA ~ EGR.599, data=df)  # significant 4th term (only ~ 190 people)

# t test for cum graduation GPA
boxplot(Graduation.Cum.GPA ~ EGR.599, data=df)
t.test(Graduation.Cum.GPA ~ EGR.599, data=df) # significant graduation increase in GPA

# ANCOVA/MLR: GPA ##################################################################
# Regress graduation GPA on the covariates
Grad_GPA_reg <- lm(Graduation.Cum.GPA ~ EGR.599 + Sex + India.Citizen + Primary.Academic.Plan, data=df)
summary(Grad_GPA_reg) # EGR599, India citizen, and IT major are significant
emmeans(Grad_GPA_reg, ~ EGR.599) # Looking at ANCOVA results
plot(Grad_GPA_reg) # Diagnostics for this model

# Regress term 1 GPA on the covariates
Term1_GPA_reg <- lm(Term.1.Cum.GPA ~ EGR.599 + Sex + India.Citizen + Primary.Academic.Plan, data=df)
summary(Term1_GPA_reg) # Same as above
plot(Term1_GPA_reg)

# Incorporating gender interaction
Grad_GPA_gender_reg <- lm(Graduation.Cum.GPA ~ EGR.599*Sex + India.Citizen + Primary.Academic.Plan, data=df)
summary(Grad_GPA_gender_reg) # Gender iteraction is not significant

# Incorporating nationality interaction
Grad_GPA_nationality_reg <- lm(Graduation.Cum.GPA ~ EGR.599*India.Citizen + Sex + Primary.Academic.Plan, data=df)
summary(Grad_GPA_nationality_reg) # Nationality iteraction is not significant

# Incorporating major interaction
Grad_GPA_major_reg <- lm(Graduation.Cum.GPA ~ EGR.599*Primary.Academic.Plan + Sex + India.Citizen, data=df)
summary(Grad_GPA_major_reg) # Major interaction is not significant --> may want to break this up into binary classification
