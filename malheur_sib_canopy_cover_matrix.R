#############################################################################################
# Independent Variables Matrix
# Author: Jim Cronan, Emily Sanders, Maggie Koontz
# Purpose: Collate and format raw data containing independent variables on canopy cover 
#into a single matrix with variables arranged in columns and sites arranged in rows.
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
<<<<<<< Updated upstream
user_paths_ground <- c(
  Nat   = "",
  Becky     = "",
  jcronan = "",
=======
user_paths_canopy <- c(
  NathanWade   = "C:/Users/NathanWade/Box/SIB/Cronan Wade/3_Data/01_Raw_Data/Severity_indices/Canopy",
  Becky = "",
  jcronan = "C:/Users/jcronan/Box/SIB/Cronan Wade/3_Data/01_Raw_Data/Severity_indices/Canopy",
>>>>>>> Stashed changes
  esande02 = "",
  margaretkoontz = ""
)

#Lookup Tables
# Map usernames to file paths
user_paths_lut <- c(
<<<<<<< Updated upstream
  Nat   = "",
  Becky     = "",
  jcronan = "",
=======
  NathanWade   = "C:/Users/NathanWade/Box/SIB/Cronan Wade/3_Data/01_Raw_Data/Severity_indices/Canopy/",
  Becky     = "",
  jcronan = "C:/Users/jcronan/Box/SIB/Cronan Wade/3_Data/01_Raw_Data/Severity_indices/Canopy/",
  esande02 = "C:/Users/esande02/Downloads/FERA/Malheur/burn_severity/lut_burn_severity_file_names.csv",
  mak600 = "C://Users//mak600//Documents//Malheur//Canopy Data//canopy_file_lut.csv")

# Map usernames to file paths
user_paths_saved_data <- c(
  NathanWade   = "C:/Users/NathanWade/Box/SIB/Cronan Wade/3_Data/02_Clean_Data/Severity_indices/Canopy/",
  Becky = "",
  jcronan = "C:/Users/jcronan/Box/SIB/Cronan Wade/3_Data/02_Clean_Data/Severity_indices/Canopy",
>>>>>>> Stashed changes
  esande02 = "",
  margaretkoontz = ""
)

# Detect current user
current_user <- Sys.info()[["user"]]

# Check if user exists in mapping for data files
if (!current_user %in% names(user_paths_ground)) {
  stop("No file path configured for this user: ", current_user)
}

# Check if user exists in mapping for lut files
if (!current_user %in% names(user_paths_lut)) {
  stop("No file path configured for this user: ", current_user)
}


#Load data
