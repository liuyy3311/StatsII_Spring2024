#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c(),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#####################
# Problem 1
#####################

# load data
load(url("https://github.com/ASDS-TCD/StatsII_Spring2024/blob/main/datasets/climateSupport.RData?raw=true"))

# Check the structure of the data
summary(climateSupport)

# Coerce from a character vector to a logical vector (assuming 'choice' was originally "Supported"/"Not Supported")
climateSupport$choice <- ifelse(climateSupport$choice == "Supported", 1, 0)

# Convert to logical
climateSupport$choice <- as.logical(climateSupport$choice)

# Set 'countries' and 'sanctions' as ordered factors
climateSupport$countries <- relevel(factor(climateSupport$countries, levels = c("20 of 192", "80 of 192", "160 of 192"), ordered = FALSE),ref = "20 of 192")
climateSupport$sanctions <- relevel(factor(climateSupport$sanctions, levels = c("None", "5%", "15%", "20%"), ordered = FALSE),ref = "5%")

  
# Fit the logistic regression model
glmModel <- glm(choice ~ countries + sanctions, data = climateSupport, family = binomial(link="logit"))

# Display the summary of the model
summary_glmModel <- summary(glmModel)

# The global null hypothesis test using an ANOVA type of approach
anova_glm <- anova(glmModel, test="Chisq")

# Print the summary of the model
print(summary_glmModel)

# Print the results of the global test
print(anova_glm)


#Question2:
#(a)
# Calculate the odds ratio for countries160 of 192
odds_ratio_countries = exp(0.64835)
odds_ratio_countries
# Calculate the odds ratio for an increase in sanctions from 5% to 15%
odds_ratio_increase_sanctions = exp(-0.32510)
odds_ratio_increase_sanctions
# The change in odds
change_in_odds = odds_ratio_countries * odds_ratio_increase_sanctions
change_in_odds

#(b)
# Coefficients
intercept <- -0.08081
coef_countries80 <- 0.33636
coef_sanctionsNone <- -0.19186 

# Calculate the linear predictor (log odds)
log_odds <- intercept + (coef_countries80 * 1) + (coef_sanctionsNone * 1)

# Convert log odds to probability
probability <- exp(log_odds) / (1 + exp(log_odds))
probability

#(c)
# Fit the original model without interaction
original_model <- glm(choice ~ countries + sanctions, family = binomial(link = "logit"), data = climateSupport)

# Fit the new model with interaction
interaction_model <- glm(choice ~ countries * sanctions, family = binomial(link = "logit"), data = climateSupport)

# Compare the two models
model_comparison <- anova(original_model, interaction_model, test = "Chisq")
model_comparison
