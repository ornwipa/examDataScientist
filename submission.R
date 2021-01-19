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

list.files(path = "./assessment/input")
# [1] "Admissions.csv"   "Lab.csv"          "Transfusions.csv"

admission <- read.csv("assessment/input/Admissions.csv", header = TRUE, sep = ",", dec = ".")
head(admission, 1)
#   admission_id patient_id admission_date admission_time discharge_date discharge_time                   hospital age sex
# 1         3602      34208     2011-09-07           <NA>     09/11/2011          13:16 St. Joseph's Health Centre  NA   M
#   charlson_comorbidity_index lap_score
# 1                          1        45

lab <- read.csv("assessment/input/Lab.csv", header = TRUE, sep = ",", dec = ".")
head(lab, 1)
#   admission_id test_name test_code result_unit result_value result_date result_time
# 1         4416   Albumin       ALB        g/dL          4.1  2003-08-19       02:25

transfusion <- read.csv("assessment/input/Transfusions.csv", header = TRUE, sep = ",", dec = ".")
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

sum(is.na(all_df$age) | is.na(all_df$charlson_comorbidity_index)) # the number of data points where both variables are missing
# [1] 33

# Explain how the imputation method(s) might affect a statistical model that uses these variables as predictors
# Answer: Any records where there is a missing predictor would have to be dropped; thus, reducing the sample size of the analysis.

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
# age, sex, and hospital as the independent variables. Interpret the results.

new_df <- all_df %>% filter(test_name == "Platelet Count")
  
model <- lm(result_value ~ age + as.factor(sex) + as.factor(hospital), data = new_df)
summary(model)
# 
# Call:
#   lm(formula = result_value ~ age + as.factor(sex) + as.factor(hospital), 
#      data = all_df)
# 
# Residuals:
#     Min      1Q  Median      3Q     Max 
# -167.72  -46.78   -3.55   45.25  204.51  
#
# Coefficients:
#                                                      Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                          286.0462    47.9540   5.965  7.8e-09 ***
# age                                                   -2.0445     0.8535  -2.395  0.01730 *  
# as.factor(sex)M                                       31.5331     8.5404   3.692  0.00027 ***
# as.factor(hospital)St. Joseph's Health Centre          0.8819    15.2549   0.058  0.95394    
# as.factor(hospital)St. Michael's Hospital             -0.4617    13.3543  -0.035  0.97245    
# as.factor(hospital)Sunnybrook Health Sciences Centre   2.8307    14.1147   0.201  0.84121    
# as.factor(hospital)Toronto Western Hospital           -6.9817    14.3139  -0.488  0.62612    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Residual standard error: 69.41 on 265 degrees of freedom
# (42 observations deleted due to missingness)
# Multiple R-squared:  0.06697,	Adjusted R-squared:  0.04584 
# F-statistic:  3.17 on 6 and 265 DF,  p-value: 0.005084
#
# Answer: the fitted linear regression model is ...
# ... result_value = 286 - 2*age + 31.5*sex + 0.88*hospital1 - 0.46*hospital2 + 2.83*hospital3 - 6.98*hospital4
# where sex is a binary variable (female: sex = 0, male: sex = 1)
# Note that neither age nor hospital did not significantly affect the result_value but sex did.

#################################
# Task 5: Create a plot demonstrating the relationship between length_of_stay and charlson_comorbidity_index

new_df <- na.omit(admission)

new_df <- mutate(new_df, 
                 length_of_stay =
                   difftime(strptime( paste(new_df$discharge_date, new_df$discharge_time), "%m/%d/%Y %H:%M"),
                            strptime( paste(new_df$admission_date, new_df$admission_time), "%Y-%m-%d %H:%M"),
                            units = "mins"))
new_df <- new_df %>% filter(length_of_stay >= 0)

summary(new_df$charlson_comorbidity_index)
#    0    1   2+ 
# 1181 1352 1265 

ggplot(data = new_df) + geom_point(aes(x = length_of_stay, y = charlson_comorbidity_index))

# If adding age, the visualization may be faceted with age groups.
# Other option is to have charlson_comobidity_index as legend and plot x as age and y as length_of_stay.
ggplot(data = new_df, aes(x = age, y = length_of_stay, group = charlson_comorbidity_index)) + 
  geom_point(aes(color = charlson_comorbidity_index)) +
  facet_grid(cols = vars(charlson_comorbidity_index))

#################################
# Task 6: Fit statistical model to predict the result_value of the “Hemoglobin” lab tests and evaluate its performance

new_df <- all_df %>% filter(test_name == "Platelet Count")

model <- lm(result_value ~ age + as.factor(sex) + as.factor(hospital) + charlson_comorbidity_index, data = new_df)
step <- stepAIC(model)
# Start:  AIC=2174.64
# result_value ~ age + as.factor(sex) + as.factor(hospital) + charlson_comorbidity_index
# 
#Df Sum of Sq     RSS    AIC
# - as.factor(hospital)         4      6477 1207501 2168.0
# - charlson_comorbidity_index  2      1855 1202878 2171.0
# <none>                                    1201023 2174.6
# - age                         1     23673 1224697 2177.6
# - as.factor(sex)              1     66270 1267293 2186.3
#
# Step:  AIC=2168.01
# result_value ~ age + as.factor(sex) + charlson_comorbidity_index
#
# Df Sum of Sq     RSS    AIC
# - charlson_comorbidity_index  2      1891 1209391 2164.4
# <none>                                    1207501 2168.0
# - age                         1     23180 1230681 2170.9
# - as.factor(sex)              1     67949 1275449 2180.0
#
# Answer: using step-wise regression to select variables and evaluate the model, the model with optimal AIC is as above.