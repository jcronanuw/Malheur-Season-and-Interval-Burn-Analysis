#############################################################################################
# RAWS Data Cleaning
# Author: Jim Cronan
# Purpose: Clean and compare hourly RAWS weather and NFDRS (Nelson-Carlson) 
#dead fuel moisture predictions to see how well they correlate.
#############################################################################################

#Load libraries
library(dplyr)#graphics
library(ggplot2)#graphics
library(readr)#- read.csv() {utils}.
library(readxl)# - read_excel()
library(gridExtra) #to display multiple histograms at once -- grid.arrange()
library(tidyverse) #needed for pipe function (%>%)

#---------------------------------------------------------------------------------------------
# 1. Load data
#---------------------------------------------------------------------------------------------

#Data
# Map usernames to file paths
user_paths_data <- c(
  Nat   = "",
  Becky = "",
  jcronan = "C:/Users/jcronan/Box/SIB/Cronan Wade/3_Data/01_Raw_Data/RAWS/"
)

#Lookup Tables
# Map usernames to file paths
user_paths_lut <- c(
  Nat   = "",
  Becky     = "",
  jcronan = "C:/Users/jcronan/Box/SIB/Cronan Wade/3_Data/03_Treatment_Data/"
)


# Outgoing (saved) data
user_paths_saved_data <- c(
  Nate   = "",
  Becky = "",
  jcronan = ""
)


# Detect current user
current_user <- Sys.info()[["user"]]

# Check if user exists in mapping for data files
if (!current_user %in% names(user_paths_data)) {
  stop("No file path configured for this user: ", current_user)
}

# Check if user exists in mapping for lut files
if (!current_user %in% names(user_paths_lut)) {
  stop("No file path configured for this user: ", current_user)
}


#Load data

#RAWS data
raws <- read.table(paste(user_paths_data[current_user],
                         "crow_flat_weather_20140413_20140515.txt",
                         sep = ""), row.names=NULL, header = T, skip = 0)

#Burn schedule data
fm <- read.table(paste(user_paths_data[current_user],
                             "crow_flat_fuel_moisture_20140413_20140515.txt",
                             sep = ""), row.names=NULL, header = T, skip = 0)

#---------------------------------------------------------------------------------------------
# 2. Comparison
#---------------------------------------------------------------------------------------------

#Convert dates from character to date.
raws$date_ymd <- as.Date(raws$Date, "%m-%d-%Y")

#Relativized precip values
rel_precip <- vector()
for(i in 1:length(raws$Precip_in))
{
  if(raws$Precip_in[i] == 0)
  {
    rel_precip[i] <- 0
  } else
  {
    rel_precip[i] <- ((raws$Precip_in[i]/max(raws$Precip_in))*100)
  }
}

#1-HR & Weather
plot(raws$RH, type = "l", xaxt = "n", col = "blue")
axis(1, seq(1,length(raws$date_ymd),
            length(raws$date_ymd)/length(unique(raws$date_ymd))), 
     unique(raws$date_ymd))
lines(raws$Temp_F, col = "green")
points(rel_precip, col = "black")
lines(fm$FM_1hr, col = "red")

#10-HR & Weather
plot(raws$RH, type = "l", xaxt = "n", col = "blue")
axis(1, seq(1,length(raws$date_ymd),
            length(raws$date_ymd)/length(unique(raws$date_ymd))), 
     unique(raws$date_ymd))
lines(raws$Temp_F, col = "green")
points(rel_precip, col = "black")
lines(fm$FM_10hr, col = "red")

#100-HR & Weather
plot(raws$RH, type = "l", xaxt = "n", col = "blue")
axis(1, seq(1,length(raws$date_ymd),
            length(raws$date_ymd)/length(unique(raws$date_ymd))), 
     unique(raws$date_ymd))
lines(raws$Temp_F, col = "green")
points(rel_precip, col = "black")
lines(fm$FM_100hr, col = "red")

#1000-HR & Weather
plot(raws$RH, type = "l", xaxt = "n", col = "blue")
axis(1, seq(1,length(raws$date_ymd),
            length(raws$date_ymd)/length(unique(raws$date_ymd))), 
     unique(raws$date_ymd))
lines(raws$Temp_F, col = "green")
points(rel_precip, col = "black")
lines(fm$FM_1000hr, col = "red")

#Only Fuel Moisture
plot(fm$FM_1hr, type = "l", xaxt = "n", col = "red")
axis(1, seq(1,length(raws$date_ymd),
            length(raws$date_ymd)/length(unique(raws$date_ymd))), 
     unique(raws$date_ymd))
lines(fm$FM_10hr, col = "orange")
lines(fm$FM_100hr, col = "yellow")
lines(fm$FM_1000hr, col = "green")




#---------------------------------------------------------------------------------------------
# End
#---------------------------------------------------------------------------------------------



