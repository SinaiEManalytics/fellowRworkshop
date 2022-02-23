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
## Last update: 2022Feb08
############################################################################


####################### Week 2 #######################

##### REVIEW #####
## Packages extend the functionality of R beyond the "base" installation
## To install a package, use the install.packages() command
install.packages("tidyverse")
## Once installed, you have to load the package before you can used the
##   functions in the package. Do this using the library() command
library(tidyverse)
##### Housekeeping - working directory
getwd() ## retrieve the absolute path for the current working directory
setwd("data") ## set the new working directory - can be absolute or relative path
##### Data import and export
## CSV
data_csv <- read.csv('data/sample_data.csv')
## EXCEL
# install.packages("readxl")
library(readxl)
data_excel <- read_excel()
## STATA or SAS data
# install.packages("haven")
library(haven)
data_stata <- read_dta('data/sample_data.dta')
data_sas <- read_sas('data/sample_data.sas7bdat')
# data_spss <- read_spss()
##### Saving Data
saveRDS(data_csv, file = "dataFile.rds")
write.csv(data_csv, file = "dataFile.csv", row.names = F)
write_dta(data_csv, path = "dataFile.dta")
write_sas(data_csv, path = "dataFile.sas7bdat")
## remove one named data frame
rm(data_sas)
## remove everything stored in memory
rm(list=ls()) 



##### DATA CLEANING #####
## Take your time - make a plan 
## THIS WILL SAVE YOU TIME


#### Load the Data
data <- readRDS('dataFile.rds')

# look at n rows
head(data, n = 5)

# table view - count data
table(data$GENDER, useNA = "ifany")
table(data$AGE)

# view in a spreadsheet style table
View(data) ## DO NOT USE THIS IF YOUR DATA IS LARGE AND WEIRD

# view the data structure - including variable type
str(data)

## chr - character format - can be converted to a categorical variable (called a factor)
## int - integer format - numeric variable contianing whole numbers only (no decimals)
## num - numeric format - also called 'double' is a numeric format that allows decimals
## factor - categorical variable - stored in 2 parts, a level (number) and a label (character)
## logi - logical variable - value of TRUE, FALSE or NA
## POSIXct - POSIXct style date/time

### Evaluation of Central Tendencies - mean, median, N, min, max, standard deviation
## view summary for each variable in your dataset
summary(data)
summary(data$AGE)


min(data$AGE)
quantile(data$AGE, 0.25)
median(data$AGE)
quantile(data$AGE, 0.75)
max(data$AGE)
mean(data$AGE)
sd(data$AGE, na.rm= T)
length(data$AGE)

data$AGE > data$q1

hist(data$AGE,breaks = 20)

## view the distribution of a numeric variable in data set
hist(data$TIME_FROM_ARRIVAL_TO_DEPART, breaks = 50)
hist(data$HOSPITAL_LENGTH_OF_STAY, breaks = 25)


boxplot(data$HOSPITAL_TOTAL_COST)
boxplot(HOSPITAL_TOTAL_COST ~ SEVERITY_OF_CONDITION, data=data, ylim = c(0, 300000))

## Q-Q Plot
qqnorm(data$TIME_FROM_ARRIVAL_TO_DEPART)
qqline(data$TIME_FROM_ARRIVAL_TO_DEPART)


## Using normal data
sampleData <- rbind(data.frame(group = rep("A"), valueA = rnorm(600, 9.5, 2), valueB = rnorm(600, 6.75, 1.5)),
                    data.frame(group = rep("B"), valueA = rnorm(400, 9.5, 2), valueB = rnorm(400, 8.5, 1.5)))

qqnorm(sampleData$valueA)
qqline(sampleData$valueA)




## using dplyr from the tidyverse to subset data
data3 <- data %>%
  filter(AGE >=75) %>%
  select(AGE, RACE, GENDER)

data5 <- data %>%
  filter(AGE >=75) %>%
  select(starts_with("CC"))

## using dplyr to create new variables
data4 <- data %>%
  mutate(newAgeCat = cut(data$AGE, breaks = c(0, 64, 74, 84, 115), labels = c("<65", "65-74", "75-84", ">=85")),
         logCost = log(HOSPITAL_TOTAL_COST),
          severityFactor = as.factor(SEVERITY_OF_CONDITION))


## A Sneak peak at next week

plot(data4$ED_LENGTH_OF_STAY, data4$logCost)
  abline(lm(logCost~ED_LENGTH_OF_STAY, data = data4))
  
plot(data4$HOSPITAL_LENGTH_OF_STAY, data4$HOSPITAL_TOTAL_COST)
abline(lm(HOSPITAL_TOTAL_COST~HOSPITAL_LENGTH_OF_STAY, data = data4))  
  
    
plot(data4$HOSPITAL_LENGTH_OF_STAY, data4$logCost)
abline(lm(logCost~HOSPITAL_LENGTH_OF_STAY, data = data4))  


# Categorical Refactoring
table(data$RACE)

data5 <- data %>%
  mutate(raceFactor = as.factor(case_when(str_detect(data$RACE, "Black") ~ "Black",
                                          str_detect(data$RACE, "White") ~ "White", 
                                          TRUE ~ "Other")))
str(data5$raceFactor)


# Dates
library(lubridate)
date <- as.Date("2021-02-06")
data %>%
  filter(year > 2012)
data %>%
  filter(date >= as.Date("2012-01-01"))

difftime(as.Date(today()), as.Date("2014-09-09"))

##### Bivariate Analysis
#### Data Manipulation ####


##Contingency Table - 2 categorical variables
library(gmodels)
gmodels::
table(data$GENDER, data$AGE > 75)
gmodels::CrossTable(data$GENDER, data$AGE > 75, chisq = T)

table(data$SEVERITY_OF_CONDITION, data$GENDER)
CrossTable(data$SEVERITY_OF_CONDITION, data$GENDER, chisq = T)

## Correlation
plot(data4$HOSPITAL_LENGTH_OF_STAY, data4$HOSPITAL_TOTAL_COST)
abline(lm(HOSPITAL_TOTAL_COST~HOSPITAL_LENGTH_OF_STAY, data = data4))  

cor.test(data4$HOSPITAL_LENGTH_OF_STAY, data4$HOSPITAL_TOTAL_COST, na.rm=T)


## Group Mean comparisons

t.test(valueA ~ group, data=sampleData)
t.test(valueB ~ group, data=sampleData)


  # but the data is paired - what abbout treament effect
sampleData$difference <- sampleData$valueA - sampleData$valueB  

t.test(difference ~ group, data = sampleData)


##Crude association
## Simple Linear Regression
regLinear <- lm(HOSPITAL_TOTAL_COST~HOSPITAL_LENGTH_OF_STAY, data = data4)
plot(regLinear)
summary(regLinear)

## Simple Logisitc REgression
regLogistic <- glm(ICU-1 ~ AGE, data = data4, family = "binomial")
summary(regLogistic)
exp(coef(regLogistic))

