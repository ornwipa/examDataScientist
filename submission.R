##### Data Programming Test #####
# Prepared by Ornwipa Thamsuwan
# Date: Monday, January 18, 2021
#################################
library(dplyr)
library(tidyr)
library(purrr)
library(stats)
library(lme4)
library(MASS)
library(ggplot2)
#################################
# Task 1: Combine three data frames

list.files(path = "./input")
# [1] "Admissions.csv"   "Lab.csv"          "Transfusions.csv"

admission <- read.csv("input/Admissions.csv", header = TRUE, sep = ",", dec = ".")
head(admission, 1)
#   admission_id patient_id admission_date admission_time discharge_date discharge_time                   hospital age sex
# 1         3602      34208     2011-09-07           <NA>     09/11/2011          13:16 St. Joseph's Health Centre  NA   M
#   charlson_comorbidity_index lap_score
# 1                          1        45

lab <- read.csv("input/Lab.csv", header = TRUE, sep = ",", dec = ".")
head(lab, 1)
#   admission_id test_name test_code result_unit result_value result_date result_time
# 1         4416   Albumin       ALB        g/dL          4.1  2003-08-19       02:25

transfusion <- read.csv("input/Transfusions.csv", header = TRUE, sep = ",", dec = ".")
head(transfusion, 1)
#   admission_id issue_date issue_time rbc_transfusion platelet_transfusion plasma_transfusion
# 1         8755 2002-08-25      16:05           FALSE                FALSE               TRUE

all_df <- full_join(admission, lab, by = "admission_id")
all_df <- full_join(all_df, transfusion, by = "admission_id")
head(all_df, 1)
#   admission_id patient_id admission_date admission_time discharge_date discharge_time                   hospital age sex
# 1         3602      34208     2011-09-07           <NA>     09/11/2011          13:16 St. Joseph's Health Centre  NA   M
#   charlson_comorbidity_index lap_score test_name test_code result_unit result_value result_date result_time issue_date issue_time
# 1                          1        45      <NA>      <NA>        <NA>           NA        <NA>        <NA>       <NA>       <NA>
#   rbc_transfusion platelet_transfusion plasma_transfusion
# 1              NA                   NA                 NA

nrow(all_df) # the number of total records in the combined data frame
# [1] 5231

#################################
# Task 2: Impute missing data

sum(is.na(all_df$age)) # the number of data points where age is missing
# [1] 627
sum(is.na(all_df$age))/nrow(all_df) # the ratio of data where age is missing
# [1] 0.1198624

sum(is.na(all_df$charlson_comorbidity_index)) # the number of data points where charlson_comorbidity_index is missing
# [1] 261
sum(is.na(all_df$charlson_comorbidity_index))/nrow(all_df) # the ratio of data where charlson_comorbidity_index is missing
# [1] 0.04989486

sum(is.na(all_df$age) & is.na(all_df$charlson_comorbidity_index)) # the number of data points where both variables are missing
# [1] 33

# Explain hot the imputation method(s) might affect a statistical model that uses these variables as predictor
# The records where there is missing predictor would have to be dropped; thus, reducing the sample size of the analysis.

#################################
# Task 3: Determine if there is a difference in age/sex between patients who had an rbc_transfusion and patients that did not.

summary(all_df$rbc_transfusion)
#    Mode   FALSE    TRUE    NA's 
# logical     281     376    4574 

rbc_y <- all_df %>% filter(rbc_transfusion == TRUE) # Patients who had an rbc_transfusion
rbc_n <- all_df %>% filter(rbc_transfusion == FALSE) # Patients who did not have an rbc_transfusion

t.test(rbc_y$age, rbc_n$age)
# 
# Welch Two Sample t-test
# 
# data:  rbc_y$age and rbc_n$age
# t = 7.6144, df = 494.82, p-value = 1.354e-13
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#  2.235540 3.790447
# sample estimates:
#   mean of x mean of y 
#    58.08358  55.07059 
#
# Answer: With a very small p-value (< 0.05) and the positive 95% confidence interval,
# the difference in age between patients who had rbc_transfusion and those who did not were significant.
# It can also be seen that patients who received rbc_transfusion were older than those who did not.

tbl <- table(all_df$sex, all_df$rbc_transfusion)
tbl
#   FALSE TRUE
# F   169  209
# M   112  167

chisq.test(tbl)
#
# Pearson's Chi-squared test with Yates' continuity correction
# 
# data:  tbl
# X-squared = 1.1868, df = 1, p-value = 0.276
#
# Answer: With p-value larger than 0.05, at the significance level of 0.95,
# There is no significant difference in sex between patients who had rbc_transfusion and those who did not.

#################################
# Task 4: Fit a linear regression model 
# using the result_value of the “Platelet Count” lab tests as the dependent variable and 
# age, sex, and hospital as the independent variables. Interpret the results

model <- lm(result_value ~ age + as.factor(sex) + as.factor(hospital), data = all_df)
summary(model)
# 
# Call:
#   lm(formula = result_value ~ age + as.factor(sex) + as.factor(hospital), 
#      data = all_df)
# 
# Residuals:
#    Min     1Q Median     3Q    Max 
# -94.57 -75.01 -34.65  59.46 513.31 
#
# Coefficients:
#                                                      Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                           86.3092    18.6890   4.618 4.03e-06 ***
# age                                                   -0.2743     0.3307  -0.830    0.407    
# as.factor(sex)M                                       14.0552     3.3791   4.159 3.27e-05 ***
# as.factor(hospital)St. Joseph's Health Centre          4.1695     6.4949   0.642    0.521    
# as.factor(hospital)St. Michael's Hospital              4.3264     5.4462   0.794    0.427    
# as.factor(hospital)Sunnybrook Health Sciences Centre   0.9656     5.5986   0.172    0.863    
# as.factor(hospital)Toronto Western Hospital            7.8805     5.7261   1.376    0.169    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Residual standard error: 95 on 3172 degrees of freedom
# (2052 observations deleted due to missingness)
# Multiple R-squared:  0.006451,	Adjusted R-squared:  0.004571 
# F-statistic: 3.432 on 6 and 3172 DF,  p-value: 0.002218
#
# Answer: the fitted linear regression model is ...
# ... result_value = 86.31 - 0.27*age + 14.06*sex + 4.17*hospital1 + 4.33*hospital2 + 0.97*hospital3 + 7.88*hospital4
# where sex is a binary variable (female: sex = 0, male: sex = 1)
# Note that neither age nor hospital did not significantly affect the result_value but sex did.

#################################
# Task 5: Create a plot demonstrating the relationship between length_of_stay and charlson_comorbidity_index

all_df <- mutate(all_df, length_of_stay =
                   as.numeric(discharge_date) - as.numeric(admission_date) + 
                   as.numeric(discharge_time) - as.numeric(admission_time))

ggplot(data = all_df) + geom_point(aes(x = length_of_stay, y = charlson_comorbidity_index))

# If adding age, the visualization may be faceted with age group.

#################################
# Task 6: Fit statistical model to predict the result_value of the “Hemoglobin” lab tests and evaluate its performance
new_df <- na.omit(all_df)
model <- lm(result_value ~ age + as.factor(sex) + as.factor(hospital) + charlson_comorbidity_index, data = new_df)
step <- stepAIC(model)
# Start:  AIC=3008.16
# result_value ~ age + as.factor(sex) + as.factor(hospital) + charlson_comorbidity_index
#
#                              Df Sum of Sq     RSS    AIC
# - charlson_comorbidity_index  2        56 1325950 3004.2
# <none>                                    1325894 3008.2
# - as.factor(sex)              1     18713 1344607 3011.3
# - as.factor(hospital)         3     42960 1368854 3013.8
# - age                         1    310429 1636323 3082.9
#
# Step:  AIC=3004.18
# result_value ~ age + as.factor(sex) + as.factor(hospital)
#
#                       Df Sum of Sq     RSS    AIC
# <none>                             1325950 3004.2
# - as.factor(sex)       1     18682 1344632 3007.3
# - as.factor(hospital)  3     43038 1368989 3009.8
# - age                  1    311788 1637739 3079.3
#
# Answer: using step-wise regression to select variables and evaluate the model, the model with optimal AIC is as above.