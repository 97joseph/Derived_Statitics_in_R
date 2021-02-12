# ----------------------------------------
# Tentamen Tillämpad statistik, HT20
# Namn: Jane Doe
# Personnummer: YYMMDD-XXXX
# ----------------------------------------

#personnummer" to "000503-7651" 

# Uppgift 1 # --------------------------------------------------------------------------------------------
rm(list = ls())
set.seed(000503-7651)
# Bestäm ditt personliga datamaterial genom att byta ut
# "000503-7651" i set.seed()-kommandot ovan
# mot ditt 000503-7651 sex första siffror

#Import Libraries

library(Ecdat)
library(ggplot2)
library(ggthemes)



# Data Modelling

data("Schooling")
df <- Schooling
df <- df[sample(nrow(df), 500) , ]
View(df)

#Rules for Data Formatting

#To help you we have these variables:
 # wage76: The individuals salary in cent(1/100 dollar).
#ed76: Studying time of the individual in years.
#daded: total studying years for dad
#momed: total studying years for mom
#let X be ed76 and Y be wage76

#Data Structuring
x<-df$ed76
y<-df$wage76

#Question 1
ggplot2(df, aes(x, y)) +
  geom_point() +
  geom_point(data = ds, aes(y = mean), colour = 'red', size = 3)


#Code to generate a linear model
#cars.lm <- lm(dist ~ speed, data = cars)
#summary(cars.lm)

#Calculate Factors of the Linear Model
#Structure of the Code

simple.linear.coef <- function(x, y) {
  # Find length of x to get sample size. Assuming x and y have the same sample size.
  n <- length(x)
  # Calculate the error statistics Sxx, Syy, and Sxy
  sxx <- sum(x^2) - sum(x)^2 / n
  syy <- sum(y^2) - sum(y)^2 / n
  sxy <- sum(x * y) - (sum(x) * sum(y)) / n
  # Coefficients beta0 and beta1
  b1 <- sxy / sxx
  b0 <- mean(y) - b1 * mean(x)
  # Sum of standard error and Mean Standard Error
  sse <- syy - sxy^2 / sxx
  mse <- sse / (n - 2)
  # Standard error beta0 and beta1
  b1.err <- sqrt(mse) / sqrt(sxx)
  b0.err <- sqrt(mse) / sqrt(n) * sqrt(1 + (mean(x)^2 / (sum((x - mean(x))^2) / n)))
  # beta0 and beta1 t-values
  b0.t <- (b0 - 0) / b0.err
  b1.t <- (b1 - 0) / b1.err
  # p-values of beta0 and beta1
  b0.p <- 2 * pt(b0.t, df = n - 2)
  b1.p <- 2 * pt(b1.t, df = n - 2, lower.tail = FALSE)
  # Coefficient of determination R-squared
  r2 <- (syy - sse) /syy
  # R-squared adjusted
  r2.adj <- r2 - (1 - r2) * ((2 - 1) / (length(y) - 2))
  
  rsquare <- paste('Multiple R-squared: ', round(r2, 4), ', Adjusted R-squared: ', round(r2.adj, 4))
  
  coeffs <- data.frame(cbind(c(b0, b1), c(b0.err, b1.err), c(b0.t, b1.t), c(b0.p, b1.p)))
  colnames(coeffs) <- c('Estimate', 'Std. Error', 't value', 'Pr(>|t|)')
  rownames(coeffs) <- c('Intercept', 'x1')
  
  # Fit the line to the data with beta0 and beta1 found above
  fitted <- x * b1 + b0
  
  # The F-Statistic
  msr <- sum((fitted - mean(y))^2) / 1
  mse2 <- sum((y - fitted)^2) / (length(y) - 2)
  f <- msr / mse2
  # p-value
  p <- pf(f, 1, length(y) - 2, lower.tail = FALSE)
  
  f.stat <- paste('F-statistic: ', round(f, 2), ' on 1 and ', n - 2, ' DF, p-value: ', format(p, digits = 3, scientific = TRUE))
  # Calculate and find summary statistics of the residuals
  resd <- y - fitted
  min.res <- round(min(resd), 3)
  max.res <- round(max(resd), 3)
  q1.q3 <- quantile(resd, probs = c(.25, .75))
  med <- round(median(resd), 3)
  residual <- data.frame(cbind(min.res, round(q1.q3[1], 3), med, round(q1.q3[2], 3), max.res))
  colnames(residual) <- c('Min', 'Q1', 'Median', 'Q3', 'Max')
  resdi <- paste('Residual standard error: ', round(sqrt(mse2), 2), ' on ', n - 2, ' degrees of freedom')
  regres <- list('Residuals'=residual, 'Coefficients'=coeffs, resdi, rsquare, f.stat)
  
  return(regres)
}


#Confirmation with the Linear Model
#the results can be verified against the lm() function by using the summary() call.
simple.linear.coef(df$ed76, df$wage76)