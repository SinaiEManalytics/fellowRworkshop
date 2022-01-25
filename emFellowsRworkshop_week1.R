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




##### Data import

## CSV
data_csv <- read.csv()


## EXCEL
# install.packages("readxl")
library(readxl)
data_excel <- read_excel()


## STATA or SAS data
# install.packages("haven")
library(haven)
data_stata <- read_dta()
data_sas <- read_sas()
data_spss <- read_spss()


##### Saving Data
saveRDS(data_csv, file = "dataFile.rds")

write.csv(data_csv, file = "dataFile.csv", row.names = F)
write_dta(data_csv, file = "dataFile.dta")
write_sas(data_csv, file = "dataFile.sas7bdat")




## Looking