#############################################################################################
# Independent Variables Matrix
# Author: Jim Cronan
# Purpose: Collate and format raw data containing independent variables into a single
#matrix with variables arranged in columns and sites arranged in rows.
#############################################################################################

#Load libraries
library(dplyr)#graphics
library(ggplot2)#graphics
library(readr)#??? - read.csv() {utils}.
library(readxl)#readxl()

#---------------------------------------------------------------------------------------------
# 1. Load data
#---------------------------------------------------------------------------------------------

#Set working directory:
setwd("C:/Users/jcronan/Box/SIB/Cronan Wade/3_Data/01_Raw_Data/Severity_indices/Ground_cover")

#Ground cover data.
years <- c("02", "03", "04", "07", "09", "12", "15", "25")
for (i in 1:length(years))
{
  object_name <- paste("g20", years[i], sep = "")
  temp <- read.csv(paste("Ground_20", years[i], ".csv", sep = ""))
  assign(object_name, temp)
  rm(object_name, temp)
}
rm(years, i)

#Set working directory:
setwd("C:/Users/jcronan/Box/SIB/Cronan Wade/3_Data/01_Raw_Data/Severity_indices/Canopy")

#Canopy data.
years <- c("02", "03", "04", "07", "09", "12", "15", "25")
for (i in 1:length(years))
{
  object_name <- paste("c20", years[i], sep = "")
  temp <- read.csv(paste("Canopy_20", years[i], ".csv", sep = ""))
  assign(object_name, temp)
  rm(object_name, temp)
}
rm(years, i)

#---------------------------------------------------------------------------------------------
# 2. Basic structure
#---------------------------------------------------------------------------------------------

nrow(g2012)


names <- objects()
row_count <- vector()

for (name in names(names)) {
  cat("Name:", name, "- Value:", names[name], "\n")
}

for(i in 1:length(names))
{
  
  row_count[i] <- nrow(c2002)
}

cbind(names, row_count) 


nrow(g2002)


#---------------------------------------------------------------------------------------------
# End
#---------------------------------------------------------------------------------------------

