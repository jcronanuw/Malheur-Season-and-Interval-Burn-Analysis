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

#---------------------------------------------------------------------------------------------
# 1. Load data
#---------------------------------------------------------------------------------------------

#Data
# Map usernames to file paths
user_paths_ground <- c(
  Nat   = "",
  Becky     = "",
  jcronan = "C:/Users/jcronan/Box/SIB/Cronan Wade/3_Data/01_Raw_Data/Severity_indices/Ground_cover/"
)

#Lookup Tables
# Map usernames to file paths
user_paths_canopy <- c(
  Nat   = "",
  Becky     = "",
  jcronan = "C:/Users/jcronan/Box/SIB/Cronan Wade/3_Data/01_Raw_Data/Severity_indices/Canopy/"
)

# Detect current user
current_user <- Sys.info()[["user"]]

# Check if user exists in mapping for data files
if (!current_user %in% names(user_paths_ground)) {
  stop("No file path configured for this user: ", current_user)
}


# Check if user exists in mapping for lut files
if (!current_user %in% names(user_paths_canopy)) {
  stop("No file path configured for this user: ", current_user)
}

#Ground lookup tables
#Plot list
plot_lut <- read.csv(paste(user_paths_ground[current_user], 
                           "lut_plots.csv", sep = ""))
#File list
ground_year <- read.csv(paste(user_paths_ground[current_user], 
                              "lut_ground_file_names.csv", sep = ""))
#Cover type crosswalk table
ctct <- read.csv(paste(user_paths_ground[current_user], 
                              "data_dictionary.csv", sep = ""))

#Ground cover data.
for (i in 1:length(ground_year$file_name_year))
{
  object_name <- paste("g", ground_year$file_name_year[i], sep = "")
  temp <- read.csv(paste(user_paths_ground[current_user], 
                         "Ground_", ground_year$file_name_year[i], 
                         ".csv", sep = ""))
  assign(object_name, temp)
  rm(object_name, temp)
}

#Canopy lookup tables
canopy_year <- read.csv(paste(user_paths_canopy[current_user], 
                              "canopy_file_lut.csv", sep = ""))

#Canopy data.
for (i in 1:length(canopy_year$file_name_year))
{
  object_name <- paste("c", canopy_year$file_name_year[i], sep = "")
  temp <- read.csv(paste(user_paths_canopy[current_user], 
                         "Canopy_", canopy_year$file_name_year[i], 
                         ".csv", sep = ""))
  assign(object_name, temp)
  rm(object_name, temp)
}


#---------------------------------------------------------------------------------------------
# 2. Data structure
#---------------------------------------------------------------------------------------------


#---------------------------------------------------------------------------------------------
# 2. Remove exclosure data
#---------------------------------------------------------------------------------------------

#Ground cover

#Remove exclosure data
in_names <- ground_year$file_name
out_names <- vector()
removals <- vector()
for (i in 1:length(in_names))#only years 2002, 2003, and 2004 have exclosures
{
  obj <- get(in_names[i])
  temp <- obj[!obj$Plot %in% plot_lut$Plot[plot_lut$Exclosure == 1],]
  out_names[i] <- paste("g", ground_year$file_name_year[i], "_1", sep = "") 
  assign(out_names[i], temp)
  removals[i] <- paste("Note - For", in_names[i], 
                       ":", length(obj$Plot) - length(temp$Plot), 
                       "samples exclosure samples were removed.")
  rm(obj, temp)
}
removals

#---------------------------------------------------------------------------------------------
# 3. Aggregate cover data
#---------------------------------------------------------------------------------------------

out_names_2 <- vector()
removals <- vector()

for (i in 1:length(out_names))#only years 2002, 2003, and 2004 have exclosures
{
  obj <- get(out_names[i])
  temp <- obj[!obj$Plot %in% plot_lut$Plot[plot_lut$Exclosure == 1],]
  out_name <- paste("g", ground_year$file_name_year[i], "_1", sep = "") 
  assign(out_name, temp)
  removals[i] <- paste("Note - For", in_names[i], 
                       ":", length(obj$Plot) - length(temp$Plot), 
                       "samples exclosure samples were removed.")
  rm(obj, out_name, temp)
}
removals


#---------------------------------------------------------------------------------------------
# 2. Merge datasets
#---------------------------------------------------------------------------------------------

#Merge 2002 and 2003 data
m1 <- merge(g2002, g2003, by = c("Plot", "Quad"))

#Make sure matching columns are the same
check_Stand <- match(m1$Stand.x, m1$Stand.y)
any(is.na(check_Stand) == T)
check_SOB <- match(m1$SOB.x, m1$SOB.y)
any(is.na(check_SOB) == T)
check_Exc <- match(m1$Exc.x, m1$Exc.y)
any(is.na(check_Exc) == T)
check_Date <- match(m1$Date.x, m1$Date.y)
any(is.na(check_Date) == T)


#If true there is a mismatch

#Make sure the SOBs are the same
check_SOB <- match(m1$SOB.x, m1$SOB.y)
any(is.na(check_SOB) == T)
#If true there is a mismatch




#---------------------------------------------------------------------------------------------
# End
#---------------------------------------------------------------------------------------------

