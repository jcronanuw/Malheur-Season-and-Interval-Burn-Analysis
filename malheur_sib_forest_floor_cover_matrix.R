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
# 2. Remove exclosure data

#####WARNING
### I only removed exclosure plots for 2002, 2003, and 2004.

#---------------------------------------------------------------------------------------------
#Ground cover

#Remove exclosure data

#Object to hold outgoing object names - you will need this for next action.
names_v1a <- vector()
removals <- vector()
#Subset first three files (2002, 2003, 2004.).
in_names_1a <- ground_year$file_name[ground_year$exclosures_YN == "Y"]
#No other years have exclosures.
for (i in 1:length(in_names_1a))#only years 2002, 2003, and 2004 have exclosures
{
  obj <- get(in_names_1a[i])
  temp <- obj[!obj$Plot %in% plot_lut$Plot[plot_lut$Exclosure == 1],]
  names_v1a[i] <- paste(in_names_1a[i], "_1", sep = "") 
  assign(names_v1a[i], temp)
  removals[i] <- paste("Note - For", in_names_1a[i], 
                       ":", length(obj$Plot) - length(temp$Plot), 
                       "samples exclosure samples were removed.")
  rm(obj, temp)
}
removals

#Update all files to the same version name
#Object to hold outgoing object names - you will need this for next action.
names_v1b <- vector()

#Subset first three files (2002, 2003, 2004.).
in_names_1b <- ground_year$file_name[ground_year$exclosures_YN == "N"]
#No other years have exclosures.
for (i in 1:length(in_names_1b))#only years 2002, 2003, and 2004 have exclosures
{
  obj <- get(in_names_1b[i])
  names_v1b[i] <- paste(in_names_1b[i], "_1", sep = "") 
  assign(names_v1b[i], obj)
  rm(obj)
}

objects()
names_v1 <- c(names_v1a, names_v1b)
names_v1

#---------------------------------------------------------------------------------------------
# 3. Create uniform cover data categories
#---------------------------------------------------------------------------------------------

#List of new column headings for cover types.
replacements <- sort(unique(ctct$new_name))

#Object to hold outgoing object names - you will need this for next action.
names_v2 <- vector()
names_check <- list()

for(a in 1:length(ground_year$file_name_year))
{
  obj <- get(names_v1[a])
  cn <-  paste("cats_", ground_year$file_name_year[a], sep = "") 
  for(i in 1:length(replacements))
  {
    old_names <- ctct$Column_heading[which(ctct[,cn] == "Y" & 
                                             ctct$new_name == replacements[i])]
    new_names <- rep(replacements[i],length(old_names))
    match_names_1 <- new_names[match(names(obj), old_names)]
    match_names_2 <- match_names_1[!is.na(match_names_1)]
    names(obj)[names(obj) %in% old_names] <- match_names_2
    rm(old_names, new_names, match_names_1, match_names_2)
  }
  names_v2[a] <- paste("g", ground_year$file_name_year[a], "_2", sep = "") 
  assign(names_v2[a], obj)
  names_check[[a]] <- unique(colnames(obj))
  rm(obj, cn)
}

#---------------------------------------------------------------------------------------------
# 4. Sum cover data for each year
#---------------------------------------------------------------------------------------------

#Object to hold outgoing object names - you will need this for next action.
names_v3 <- vector()

for(a in 1:length(ground_year$file_name_year))
{
  obj <- get(names_v2[a])
  out_file <- data.frame(
    litter = rep(0,length(obj[,1])), 
    mineral = rep(0,length(obj[,1])), 
    moss = rep(0,length(obj[,1])), 
    other = rep(0,length(obj[,1]))
  )
  for(i in 1:length(replacements))
  {
    obj_cvr <- obj[,colnames(obj) == replacements[i]]
    obj_cvr <- as.matrix(obj_cvr)
    out_file[,i] <- apply(obj_cvr,1,sum, na.rm = T)
    rm(obj_cvr)
  }
  out_file_append <- data.frame(
    Plot = obj$Plot, 
    Quad = obj$Quad, 
    out_file)
  names_v3[a] <- paste("g", ground_year$file_name_year[a], "_3", sep = "") 
  assign(names_v3[a], out_file_append)
  rm(obj, out_file, out_file_append)
}

#---------------------------------------------------------------------------------------------
# 5. Combine files into a single object
#---------------------------------------------------------------------------------------------


#---------------------------------------------------------------------------------------------
# 5. Orient vertically
#---------------------------------------------------------------------------------------------


#---------------------------------------------------------------------------------------------
# End
#---------------------------------------------------------------------------------------------

