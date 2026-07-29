#############################################################################################
# RAWS Data Cleaning
# Author: Jim Cronan
# Purpose: Segment RAWS data for periods prior to and day of prescribed fires to calculate
# weather and fire behavior parameters that can be used to infer severity.
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
user_paths_raws <- c(
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
if (!current_user %in% names(user_paths_raws)) {
  stop("No file path configured for this user: ", current_user)
}

# Check if user exists in mapping for lut files
if (!current_user %in% names(user_paths_lut)) {
  stop("No file path configured for this user: ", current_user)
}


#Load data

#RAWS data
raws <- read.table(paste(user_paths_raws[current_user],
                         "raws_2014.txt",
                         sep = ""), skip = 6)

#Burn schedule data
schedule <- read_excel(paste(user_paths_lut[current_user],
                         "burn_schedule.xlsx",
                         sep = ""), col_types = c("text", "numeric", "text", 
                                                  "date"))
  
#---------------------------------------------------------------------------------------------
# 2. Data Formatting
#---------------------------------------------------------------------------------------------

raws_header <- c("date", "year", "day_of_year", "day_of_run", "solar_radiation", 
                 "wind_speed_mph", "wind_direction", "wind_gust_mph", 
                 "air_temp_ave_degsF", "air_temp_max_degsF", 
                 "air_temp_min_degsF", "fuel_temp_ave_degsF", 
                 "fuel_temp_max_degsF", "fuel_temp_min_degsF", 
                 "fuel_moisture_ave", "fuel_moisture_max", 
                 "fuel_moisture_min", "rel_humidity_ave", "rel_humidity_max", 
                 "rel_humidity_min", "barometric_pres_inch_Hg", 
                 "precipitation_in")

colnames(raws) <- raws_header

#---------------------------------------------------------------------------------------------
# 3. Data cleaning
#---------------------------------------------------------------------------------------------

#Convert dates to standard format

#For RAWS data
r_date <- mdy(raws$date)
raws$date <- r_date
raws_row <- 1:length(raws$date)

raws_df <- data.frame(rowID = raws_row,
                      raws)

#For schedule dates.
s_date <- ymd(schedule$burn_date)
schedule$burn_date <- s_date

#Collate weather data for 1 month prior to each burn date

#Create a list of burn dates
burn_dates <- schedule[is.na(schedule$burn_date) == F,]

#Temporary
#Remove burn dates later than 2015 - until you get full dataset.
temp <- burn_dates[order(burn_dates$burn_date),]
temp <- temp[1:60,]

#Subset weather data for burn date and previous 29 days for each burn.
wx_subsets <- list()

for(i in 1:length(temp$burn_date))
  {
  row_num_max <- raws_df$rowID[raws_df$date == temp$burn_date[i]]
  row_num_min <- (raws_df$rowID[raws_df$date == temp$burn_date[i]] - 30)
  wx_subsets[[i]] <- raws_df[row_num_min:row_num_max,]
  remove(row_num_max, row_num_min)
  }

#Create dataset of relevant weather data for each burn.
max_temp <- vector()
rh <- vector()
min_rh <- vector()
wind <- vector()
gust <- vector()
fm <- vector()
dsr <- vector()

for(i in 1:length(temp$burn_date))
{
mrID <- max(wx_subsets[[i]]$rowID)
max_temp[i] <- wx_subsets[[i]]$air_temp_max_degsF[wx_subsets[[i]]$rowID == mrID]
min_rh[i] <- wx_subsets[[i]]$rel_humidity_min[wx_subsets[[i]]$rowID == mrID]
wind[i] <- wx_subsets[[i]]$wind_speed_mph[wx_subsets[[i]]$rowID == mrID]
gust[i] <- wx_subsets[[i]]$wind_gust_mph[wx_subsets[[i]]$rowID == mrID]
fm[i] <- wx_subsets[[i]]$fuel_moisture_ave[wx_subsets[[i]]$rowID == mrID]
rain_days <- wx_subsets[[i]]$rowID[wx_subsets[[i]]$precipitation_in > 0.1]
if(length(rain_days) == 0)
   {
     dsr[i] <- 30
} else
  {
    rrID <- max(wx_subsets[[i]]$rowID[wx_subsets[[i]]$precipitation_in > 0.1])
    dsr[i] <- mrID - rrID
    }
remove(mrID, rrID)
}

wx_params <- data.frame(temp,
                        max_temp = max_temp,
                        min_rh = min_rh,
                        wind = wind,
                        gust = gust,
                        fuel_moisture = fm,
                        day_since_rain = dsr)
#No wx data for 1998 burns
#To-do
#1. Check and see if there is another RAWS station nearby
#2. Use GIS to check which RAWS station is closest to each unit.



raws_df$rowID[raws_df$date == "2012-09-21"]

raws_df[10096:10126,]

#---------------------------------------------------------------------------------------------
# End
#---------------------------------------------------------------------------------------------



