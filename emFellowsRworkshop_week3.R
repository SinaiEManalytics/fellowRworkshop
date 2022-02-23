############################################################################
## TITLE BLOCK - optional component
## Project Title: Basic Data Analysis Using R Workshop
## PI: N/A
## Analyst: Dave Buckler, MUSA EMT-P
##    david.buckler@mountsinai.org
## Description: Sample code to demonstrate and acclimate users
##    working in the R and RStudio environment for data analysis.
##    This code relies heavily on use of the tidyverse for data 
##    manipulation and cleaning. Please note there are other ways
##    to perform the same task, this is simply the author's 
##    preference.
## 
## Last update: 2022Feb22
############################################################################

## setup - load packages and data
library(tidyverse)
data <- readRDS('dataFile.rds')


## Linear Regression

# Pre=regression tests
plot(data$HOSPITAL_LENGTH_OF_STAY, data$HOSPITAL_TOTAL_COST)
cor.test(data$HOSPITAL_LENGTH_OF_STAY, data$HOSPITAL_TOTAL_COST)

## perform linear regression
linearModel <- lm(HOSPITAL_TOTAL_COST ~ HOSPITAL_LENGTH_OF_STAY, data = data)
summary(linearModel)
                         
## Extract model summary parts
modelSummary <- summary(linearModel)
modelSummary$coefficients[,1] # coefficients
modelSummary$coefficients[,2] # standard errors
modelSummary$coefficients[,3] # T statistic
modelSummary$coefficients[,4] # p-value



## plot the regression line
plot(data$HOSPITAL_LENGTH_OF_STAY, data$HOSPITAL_TOTAL_COST)
abline(linearModel, col = "red")


## evaluate the results
modelSummary
# -- ideally, the residuals would be centered at zero and symmetric
##residual errors should be random (not trending) and normally distributed.
# -- the coefficients would be non-zerod and significant
# -- the overall goodness-of-fit would be good, measured by R2, F-statistic, p-value
# there are a variety of functions which can be used to extract these results

modelSummary$coefficients         # report the coefficients
modelSummary$residuals            # calculate the residuals
hist(modelSummary$residuals, breaks = 40)

# now plot residuals
plot ( fitted (linearModel), resid (linearModel)) 
hist (resid (linearModel ), breaks = 40)  
plot (density (residuals (linearModel)))


## We can use this model to predict on unseen data
summary(data$HOSPITAL_LENGTH_OF_STAY)
set.seed(42)
newData<- data.frame(LOS = floor(runif(min = 0, max = 100, n = 500)), 
                     HOSPITAL_LENGTH_OF_STAY = floor(runif(min = 0, max = 100, n = 500))) #try rnorm for fun
summary(newData)
hist(newData$LOS)

pp <- predict(linearModel, int = "p", newdata = newData)
pp
pc <- predict(linearModel, int = "c", newdata = newData)
pc

## plot confidence intervals arouns the line of best fit
plot (data$HOSPITAL_LENGTH_OF_STAY, data$HOSPITAL_TOTAL_COST, ylim = range (pp))
matlines (newData$HOSPITAL_LENGTH_OF_STAY, pc, lty = c(1,2,2), col="red")
matlines (newData$HOSPITAL_LENGTH_OF_STAY, pp, lty = c(1,3,3), col="red")




#### Logistic Regression
## Simple Logisitc REgression

## identify dependant variable
summary(data$ICU)  ## according to data dictionary ICU==2 is a ICU stay
data$ICUbinary <- data$ICU-1
summary(data$ICUbinary)

## identify predictor variable(s)
summary(data$AGE)
data$ageCat <- cut(data$AGE, breaks = c(0,74,84,110), labels = c("65-74", "75-84", "85+"))


regLogistic <- glm(ICUbinary ~ AGE, data = data, family = "binomial")
regLogisticCategory <- glm(ICUbinary ~ ageCat, data = data, family = "binomial")
summary(regLogistic)
exp(coef(regLogistic))
exp(confint(regLogistic))

summary(regLogisticCategory)
exp(coef(regLogisticCategory))
exp(confint(regLogisticCategory))

