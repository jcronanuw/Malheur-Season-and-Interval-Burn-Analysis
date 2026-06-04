#############################################################################################
# Malheur SIB Trees QA/QC Script
# Author: Emily Sanders, Jim Cronan
# Purpose: Review Malheur SIB Trees Data for errors
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
#SIB Trees Data 
user_paths_data <- c(
  esande02 = 
  mak600 = 
  jcronan = 
)

#Lookup Tables
# Map usernames to file paths
user_paths_lut <- c(
  esande02 = 
  mak600  = 
  jcronan =
)

#Detect current user
current_user <- Sys.info()[["user"]]

# Check if user exists in mapping for data files
if (!current_user %in% names(user_paths_data)) {
  stop("No file path configured for this user: ", current_user)
}

# Check if user exists in mapping for lut files
if (!current_user %in% names(user_paths_lut)) {
  stop("No file path configured for this user: ", current_user)
}

# Load data
dim <- read_excel(paste(user_paths_data[current_user], 
                      "",
                        sep = ""))

cw <- read_excel(paste(user_paths_data[current_user], 
                      "",
                       sep = ""))
# Load LUTs
plot_lut <- read_excel(paste(user_paths_lut[current_user], 
                            "",
                             sep = ""))

species_lut <- read.csv(paste(user_paths_lut[current_user], 
                             "",
                              sep = ""))