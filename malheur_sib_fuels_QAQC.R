#############################################################################################
# Malheur SIB Fuels QA/QC Script
# Author: Emily Sanders, Jim Cronan
# Purpose: Review Malheur SIB Fuels Data for errors
# Date: June 4, 2026
#############################################################################################

#Load libraries
library(dplyr)
library(ggplot2)
library(readr)
library(readxl)#readxl()
library(tidyverse)

#---------------------------------------------------------------------------------------------
# 1. Load data
#---------------------------------------------------------------------------------------------

#Data
#SIB Fuels Data
user_paths_data <- c(
  esande02 = "C:/Users/esande02/Downloads/FERA/Malheur/SIB_fuels/",
  mak600 = "",
  jcronan = ""
)

#Detect current user
current_user <- Sys.info()[["user"]]

# Check if user exists in mapping for data files
if (!current_user %in% names(user_paths_data)) {
  stop("No file path configured for this user: ", current_user)
}

# Load data
hr100 <- read_excel(paste(user_paths_data[current_user], 
                      "SIB_fuels_1_10_100_hr.xlsx",
                       sep = ""))

hr1000 <- read_excel(paste(user_paths_data[current_user], 
                      "SIB_fuels_1000_hr.xlsx",
                       sep = ""))

ld <- read_excel(paste(user_paths_data[current_user], 
                       "SIB_fuels_litter_duff.xlsx",
                        sep = ""))

#---------------------------------------------------------------------------------------------
# 2. Basic structure
#---------------------------------------------------------------------------------------------

#Look at structure of data

#############################
#1, 10, 100 hour fuels  #####
#############################

#############################
#1000 hour fuels        #####
#############################

#############################
#Litter and Duff        #####
#############################

#---------------------------------------------------------------------------------------------
# 3. Numeric Check
#---------------------------------------------------------------------------------------------

#############################
#1, 10, 100 hour fuels  #####
#############################

#############################
#1000 hour fuels        #####
#############################

#############################
#Litter and Duff        #####
#############################

#---------------------------------------------------------------------------------------------
# 3. Categorical Check
#---------------------------------------------------------------------------------------------

#############################
#1, 10, 100 hour fuels  #####
#############################

#############################
#1000 hour fuels        #####
#############################

#############################
#Litter and Duff        #####
#############################

