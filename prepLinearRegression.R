# Use R built-in data: CO2, Carbon Dioxide uptake in grass plants
data(CO2)
dim(CO2)
# [1] 84  5
head(CO2, 1)
#   Plant   Type  Treatment conc uptake
# 1   Qn1 Quebec nonchilled   95   16.0

##### Simple Linear Regression #####
# Visualization
library(ggplot2)
ggplot(CO2, aes(x = conc, y = uptake)) +
  geom_point() +
  stat_smooth(method = lm) # add smoothed line, with SE and CI

# correlation coefficient measures the level of association between two variables
cor(CO2$conc, CO2$uptake)
# [1] 0.4851774

# Model Building
model = lm(uptake ~ conc, data = CO2)
summary(model)
# 
# Call:
#   lm(formula = uptake ~ conc, data = CO2)
#
# Residuals:
#     Min      1Q  Median      3Q     Max 
# -22.831  -7.729   1.483   7.748  16.394 
#
# Coefficients:
#              Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 19.500290   1.853080  10.523  < 2e-16 ***
# conc         0.017731   0.003529   5.024 2.91e-06 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Residual standard error: 9.514 on 82 degrees of freedom
# Multiple R-squared:  0.2354,	Adjusted R-squared:  0.2261 
# F-statistic: 25.25 on 1 and 82 DF,  p-value: 2.906e-06

y_hat <- model$fitted.values # extract predicted value
error <- model$residuals # extract residuals = y - y_hat

confint(model) # confidence intervals of intercept and slope
#                   2.5 %     97.5 %
# (Intercept) 15.81392333 23.1866563
# conc         0.01071057  0.0247506

# Diagnostic plots
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(fit)

##### Multiple Linear Regression #####
# Model Evaluation
model <- lm(uptake ~ conc + as.factor(Treatment) + as.factor(Type)/as.factor(Plant), data = CO2)
anova(model)

# Step-wise Regression / Variable Selection 
library(MASS)
step <- stepAIC(model, direction = "both")
step$anova
# Stepwise Model Path 
# Analysis of Deviance Table
# 
# Initial Model:
#   uptake ~ conc + as.factor(Treatment) + as.factor(Type)/as.factor(Plant)
#
# Final Model:
#   uptake ~ conc + as.factor(Type) + as.factor(Type):as.factor(Plant)
#
#                     Step Df Deviance Resid. Df Resid. Dev AIC
# 1                                           71       2560 313
# 2 - as.factor(Treatment)  0 3.05e-10        71       2560 313

# K-fold Cross-Validation
library(DAAG)
cv.lm(data = CO2, form.lm = model, m = 3) # 3-fold
