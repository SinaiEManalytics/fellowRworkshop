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
## Last update: 2022Jan24
############################################################################


####################### Week 1 #######################

##### Housekeeping - Packages
## Packages extend the functionality of R beyond the "base" installation

## To install a package, use the install.packages() command
install.packages("tidyverse")

## Once installed, you have to load the package before you can used the
##   functions in the package. Do this using the library() command
library(tidyverse)



##### Housekeeping - working directory
getwd() ## retrieve the absolute path for the current working directory
setwd("") ## set the new working directory - can be absolute or relative path


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


##### Removing data in memory

## remove one named data frame
rm(data_sas)

## remove everything stored in memory
rm(list=ls()) 


#### Looking at the Data ####
data <- readRDS('dataFile.rds')

# look at n rowss
head(data, n = 10)

# table view - count data
table(data$GENDER, useNA = "ifany")

# view in a spreadsheet style table
View(data)

# view the data structure - including variable type
str(data)


## chr - character format - can be converted to a categorical variable (called a factor)
## int - integer format - numeric variable contianing whole numbers only (no decimals)
## num - numeric format - also called 'double' is a numeric format that allows decimals
## factor - categorical variable - stored in 2 parts, a level (number) and a label (character)
## logi - logical variable - value of TRUE, FALSE or NA
## POSIXct - POSIXct style date/time

## view summary for each variable in your dataset
summary(data)

## view the distribution of a numeric variable in data set
hist(data$TIME_FROM_ARRIVAL_TO_DEPART, breaks = 30)


#### Data Manipulation ####

## Creating a variable
data$ageCat <- cut(data$AGE, 
                   breaks = c(0, 64, 74, 84, 115), 
                   labels = c("<65", "65-74", "75-84", ">84"))
str(data$AGE)
str(data$ageCat)

## removing a variable
data$ageCat <- NULL


## subsetting data
# newDataset <- oldDataset[row_criteria, column_criteria]

data2 <- data[data$AGE >=75, c("AGE", "RACE", "GENDER")]

## using dplyr from the tidyverse to subset data
data3 <- data %>%
  filter(AGE >=75) %>%
  select(AGE, RACE, GENDER)

## using dplyr to create new variables
data4 <- data %>%
  mutate(newAgeCat = cut(data$AGE, breaks = c(0, 64, 74, 84, 115), labels = c("<65", "65-74", "75-84", ">84")),
         logCost = log(HOSPITAL_TOTAL_COST))


## A Sneak peak at next week

plot(data4$ED_LENGTH_OF_STAY, data4$logCost)
  abline(lm(logCost~ED_LENGTH_OF_STAY, data = data4))
  
plot(data4$HOSPITAL_LENGTH_OF_STAY, data4$HOSPITAL_TOTAL_COST)
abline(lm(HOSPITAL_TOTAL_COST~HOSPITAL_LENGTH_OF_STAY, data = data4))  
  
    
plot(data4$HOSPITAL_LENGTH_OF_STAY, data4$logCost)
abline(lm(logCost~HOSPITAL_LENGTH_OF_STAY, data = data4))  

cor.test(data4$HOSPITAL_LENGTH_OF_STAY, data4$HOSPITAL_TOTAL_COST, na.rm=T)
