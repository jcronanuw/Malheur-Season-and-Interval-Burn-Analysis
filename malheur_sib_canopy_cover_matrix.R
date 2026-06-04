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
user_paths_ground <- c(
  Nat   = "",
  Becky     = "",
  jcronan = "",
  esande02 = "",
  margaretkoontz = ""
)

#Lookup Tables
# Map usernames to file paths
user_paths_lut <- c(
  Nat   = "",
  Becky     = "",
  jcronan = "",
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
