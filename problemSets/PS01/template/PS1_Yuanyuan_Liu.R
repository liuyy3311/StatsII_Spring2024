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

# Function to perform the KS test
ks_test <- function(data) {
  n <- length(data)
  # Sort data for ECDF calculation
  data <- sort(data)
  ECDF <- ecdf(data)
  empiricalCDF <- ECDF(data)
  
  # Theoretical CDF for the standard normal distribution
  theoreticalCDF <- pnorm(data)
  
  
  # Function to calculate the p-value using matrix H
  ks_pvalue_matrix <- function(D, n) {
    k <- ceiling(n * D)
    h <- k - n * D
    m <- 2 * k - 1
    
    # Fill the matrix
    # Initialize the matrix H
    H <- matrix(0, nrow = m, ncol = m)
    
    for (i in 1:m) {
      for (j in 2:i) {
        H[i, j] <- (1-h^(i+1-j)) / factorial(i+1-j)
      }
    }
    
    for (i in 1:(m-1)) {
      for (j in 1) {
        H[i, j] <- (1-h^i) / factorial(i)
      }
    }
    
    for (i in 1:m) {
      for (j in 1) {
        H[i, j] <- (1-2*h^i) / factorial(i)
      }
    }
    
    for (i in 2:(m-1)) {
      for (j in 1:i) {
        if (i+1 <= j) {
          H[i, j] <- 1 / factorial(i+1-j) 
        } else {
          H[i, j] <- 0
        }
      }
    }
    
    T <- H
    for (i in 2:n) {
      T <- T %*% H
    }
    p_value <- factorial(n) * T[k, k] / n^n
    return(p_value)
  }
  D <- max(abs(empiricalCDF - theoreticalCDF))
  p_value <- ks_pvalue_matrix(D, n)
  
  return(list(D = D, p_value = p_value))
}

# Set seed for reproducibility
set.seed(123)

# Generate 1,000 Cauchy random variables
cauchy_data <- rcauchy(1000, location = 0, scale = 1)

# Perform the KS test
ks_result <- ks_test(cauchy_data)

# Print the result
print(ks_result)





#####################
# Problem 2
#####################

# Set the seed for reproducibility
set.seed(123)

# Create the data
data <- data.frame(x = runif(200, 1, 10))
data$y <- 0 + 2.75 * data$x + rnorm(200, 0, 1.5)

# Estimate the OLS regression using lm()
lm_fit <- lm(y ~ x, data = data)

# Now using optim() to perform OLS manually using the BFGS method
# Define the objective function (sum of squared residuals)
ssr <- function(params, data) {
  with(data, sum((y - (params[1] + params[2] * x))^2))
}

# Initial parameter guesses
initial_params <- c(0, 0)

# Run optim() with BFGS method
optim_fit <- optim(par = initial_params, fn = ssr, data = data, method = "BFGS")

# Show the coefficients from lm and optim
lm_coefficients <- coef(lm_fit)
optim_coefficients <- optim_fit$par
# Print the results
print(lm_coefficients)
print(optim_coefficients)



