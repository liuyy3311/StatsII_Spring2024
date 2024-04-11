#Load library
library(eha)
library(survival)
library(stargazer)
library(ggfortify)

#Question 1
#Load child dataset
library(eha)
data(child)

#creat a variable of child_surv
child_surv <- with(child, Surv(enter, exit, event))

#fit a Cox Proportional Hazardmodel using mother's age and infants gender as covariates
cox_model <- coxph(child_surv~ m.age+sex, data = child)
summary(cox_model)
#test the coefficient
drop1(cox_model,test = "Chisq")
#summary this model
stargazer(cox_model,type = "text")

# Calculate and interpret the hazard ratios (exponential of coefficients)
exp(coef(cox_model))

#plot this model
cox_fit <- survfit(cox_model)
autoplot(cox_fit)
