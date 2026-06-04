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
  esande02 = "C:/Users/esande02/Downloads/FERA/Malheur/SIB_trees/",
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
trees <- read_excel(paste(user_paths_data[current_user], 
                      "SIB_trees.xlsx",
                        sep = ""))

#---------------------------------------------------------------------------------------------
# 2. Basic structure
#---------------------------------------------------------------------------------------------

#Look at structure of data

#---------------------------------------------------------------------------------------------
# 3. Numeric Check
#---------------------------------------------------------------------------------------------

#---------------------------------------------------------------------------------------------
# 3. Categorical Check
#---------------------------------------------------------------------------------------------

