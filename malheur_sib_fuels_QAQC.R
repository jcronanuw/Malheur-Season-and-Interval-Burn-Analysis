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
str(hr100)
#looks good

#############################
#1000 hour fuels        #####
#############################
str(hr1000)
#looks good

#############################
#Litter and Duff        #####
#############################
str(ld)
#looks good

#---------------------------------------------------------------------------------------------
# 3. Numeric Check
#---------------------------------------------------------------------------------------------

#############################
#1, 10, 100 hour fuels  #####
#############################

# Check for missing values 
colSums(is.na(hr100))
which(is.na(hr100$`100-hour`))
# D26, Plot 98, Direction 286, 100-hr blank
    # Need to look at data sheets, but should probably = 0
# D28, Plot 102A, Direction 131, 100-hr blank
    # Need to look at data sheets, but should probably = 0 

# Check for negative values in fuel counts 
hr100 %>%
  filter(
    `1-hour` < 0 |
      `10-hour` < 0 |
      `100-hour` < 0
  )
#good 

# Histogram of fuel counts + boxplot/outliers
  # 1-hr
ggplot(hr100, aes(x = `1-hour`)) +
  geom_histogram(bins = 20, fill = "pink", color = "black") +
  labs(title = "1-Hour Fuel Counts", 
       x =  "Count of 1-Hour Fuels",
       y = "Frequency"
       )
#looks good, no extreme outliers 

boxplot.stats(hr100$`1-hour`)$out
#outliers listed are not implausible values, all <= 11
#looks good

  # 10-hr
ggplot(hr100, aes(x = `10-hour`)) +
  geom_histogram(bins = 20, fill = "pink", color = "black") +
  labs(title = "10-Hour Fuel Counts", 
       x =  "Count of 10-Hour Fuels",
       y = "Frequency"
  )
#looks good, no extreme outliers 

boxplot.stats(hr100$`10-hour`)$out
#outliers listed are not implausible values, all <= 19

  # 100-hr
ggplot(hr100, aes(x = `100-hour`)) +
  geom_histogram(bins = 20, fill = "darkgoldenrod", color = "black") +
  labs(title = "100-Hour Fuel Counts", 
       x =  "Count of 100-Hour Fuels",
       y = "Frequency"
  )
#looks good, a few outliers, but not extreme

boxplot.stats(hr100$`100-hour`)$out
#outliers listed not implausible, all <= 27
  #27 is a high 100-hour count but not concerning

# All Direction values between 0 and 360
hr100 %>%
  filter(Direction < 0 | Direction > 360)
#ok

# Each plot should have 2 directions 
hr100 %>%
  count(Stand, Plot) %>%
  filter(n != 2)
#11A has 4 directions.Notes say that plots were labelled wrong in field
#Not sure how this needs to be changed/relabeled, but there shouldn't be 4 11A plots listed

# All transect pairs 180 degrees apart (ignoring 11A plots)
hr100 %>%
  group_by(Stand, Plot) %>%
  filter(n() == 2) %>%
  summarize(
    diff = abs(max(Direction) - min(Direction)),
    .groups = "drop"
  ) %>%
  filter(diff != 180)
#There are 4 plots where the transects are not 180 degrees apart:
  #D14, Plot 12B (200 degrees apart)
  #D17, Plot 91 (190 degrees apart)
  #KF, Plot 49B (170 degrees apart)
  #Trout, Plot 75 (190 degrees apart)
#Not sure if this is an issue or not, but worth pointing out

#############################
#1000 hour fuels        #####
#############################

#DIAMETERS#
# Check for negative or 0 diameters
hr1000 %>%
  filter(Diameter <= 0)
#good 

# Missing diameter values, if missing, should have "NONE" in Spp. column
hr1000 %>%
  filter(is.na(Diameter) & Species != "NONE")

hr1000 %>%
  filter(Species == "NONE" & !is.na(Diameter))
#good 

# Hist of diameter values + outliers
ggplot(hr1000, aes(x = Diameter)) +
  geom_histogram(bins = 20, fill = "darkorchid", color = "black") +
  labs(title = "1000-Hr Diameter Distribution (cm)",
       x = "Diameter",
       y = "Frequency")

outliers <- boxplot.stats(hr1000$Diameter)$out
hr1000 %>%
  mutate(Row = row_number(), .before =1) %>%
  filter(Diameter %in% outliers) %>%
  print(n = 25)
#Some very high values (82.8 cm, 80 cm, etc), but not necessarily concerning.
#82.8 cm (max value) = 32.6 in, which is plausible for large fallen tree.


#DECAY CLASS#
# Missing decay class values 
hr1000%>%
  filter(Species != "NONE" & is.na(`Decay class`))
#ok

# Decay class values should be between 1-5
hr1000%>%
  filter(`Decay class` < 1 | `Decay class` > 5)
unique(hr1000$`Decay class`)
#ok

# Hist of decay class 
ggplot(hr1000, aes(x=`Decay class`)) +
  geom_histogram(bins=20, fill= "springgreen", color = "black" ) +
  labs(title = "1000-Hr Decay Class Distribution",
       x = "Decay Class",
       y = "Frequency")
#Most 1000-hrs are decay class 3 or 4
#ok

#DIRECTION#
#Each plot should have 2 UNIQUE directions
hr1000 %>%
  group_by(Stand, Plot) %>%
  summarize(
    n_directions = n_distinct(Direction),
    .groups = "drop"
  ) %>%
  filter(n_directions != 2)
#A few problems here:
  #D17, Plot 20 associated with 4 directions. Looking at datasheet, there are no
  #1000-hr fuels recorded on any of these directions, but possible that these are
  #two different plots and need to be separated out. Or possible that transects
  #were re-run on different azimuths, and no 1000-hrs were found. In latter case,
  #duplicate rows should be removed 

#Also:
  #D17 Plots 16, 21, 90, and 90A only have 1 transect direction associated. In
  #notes for Plot 16, Maggie wrote that there was no record of absence of the
  #other transect. Assuming this is the case with Plots 21, 90, 90A. Even if no
  #1000-hr fuels were found on the other transect, there should still be row 
  #indicating the other transect direction and that no 1000-hrs were found, i.e.
  #(Species = "NONE"). Or, if transects were never ran, why? Not sure the best
  #way to go about doing this.

# All transect pairs 180 degrees apart (ignoring problem plots)
hr1000 %>%
  group_by(Stand, Plot) %>%
  filter(n_distinct(Direction) == 2) %>% # only plots with 2 unique directions
  summarize(
    dir_min = min(Direction),
    dir_max = max(Direction),
    diff = dir_max - dir_min,
    .groups = "drop"
  ) %>%
  filter(diff != 180)
#There are 4 plots where the transects are not 180 degrees apart:
#(Same as 1-10-100 hr fuels)
  #D14, Plot 12A (200 degrees apart)
  #D17, Plot 91 (190 degrees apart)
  #KF, Plot 49B (170 degrees apart)
  #Trout, Plot 75 (190 degrees apart)
#Not sure if this is an issue or not, but worth pointing out

# Transect directions between 0 and 360
hr1000 %>%
  filter(Direction < 0 | Direction > 360)
#ok

#############################
#Litter and Duff        #####
#############################

#DEPTH (6m and 12m)#
# Missing or negative depth values
ld %>%
  filter(
    is.na(`6m depth`) |
      is.na(`12m depth`) |
      `6m depth` < 0 |
      `12m depth` < 0
    )
#ok 

# Hist of 6m depth + outliers
ggplot(ld, aes(x = `6m depth`)) +
  geom_histogram(bins = 20, fill = "salmon", color = "black" ) +
  labs(title = "Litter/Duff 6m Depth Distribution",
       x = "Litter/Duff Depth (mm)",
       y = "Frequency"
       )

boxplot.stats(ld$`6m depth`)$out
  #Values >200mm pretty large, because that would be >7.87 inches of litter/duff,
  #but not impossible.

# Hist of 12m depth + outliers 
ggplot(ld, aes(x = `12m depth`)) +
  geom_histogram(bins = 20, fill = "turquoise", color = "black" ) +
  labs(title = "Litter/Duff 12m Depth Distribution",
       x = "Litter/Duff Depth (mm)",
       y = "Frequency"
  )

boxplot.stats(ld$`12m depth`)$out
  #All values <150mm, distribution looks good

#DIRECTION#
# Transect directions between 0 and 360
ld %>%
  filter(Direction < 0 | Direction > 360)
#ok

#Each plot should have 2 unique transect directions 
ld %>%
  group_by(Stand, Plot) %>%
  summarize(
    n_directions = n_distinct(Direction),
    .groups = "drop"
  ) %>%
  filter(n_directions != 2)
#good

# All transect pairs 180 degrees apart
ld %>%
  group_by(Stand, Plot) %>%
  filter(n() == 2) %>%
  summarize(
    diff = abs(max(Direction) - min(Direction)),
    .groups = "drop"
  ) %>%
  filter(diff != 180)
#There are 4 plots where the transects are not 180 degrees apart:
#(Same as 1-10-100 hr fuels)
  #D14, Plot 12A (200 degrees apart)
  #D17, Plot 91 (190 degrees apart)
  #KF, Plot 49B (170 degrees apart)
  #Trout, Plot 75 (190 degrees apart)
#Not sure if this is an issue or not, but worth pointing out

#---------------------------------------------------------------------------------------------
# 3. Categorical Check
#---------------------------------------------------------------------------------------------

#############################
#1, 10, 100 hour fuels  #####
#############################

# All stands should match those listed on data sheets
unique(hr100$Stand)
#good 

# Each plot should have one treatment 
hr100 %>%
  group_by(Stand, Plot) %>%
  summarize(
    n_treatments = n_distinct(Treatment),
    .groups = "drop"
  ) %>%
  filter(n_treatments > 1)
#good 

# All treatments should match those listed on data sheets 
unique(hr100$Treatment)
#good 

# All plots should match those listed on data sheets
unique(hr100$Plot)
#ok 

# Missing treatments, plots, or stands
hr100 %>%
  filter(
    is.na(Stand) |
      is.na(Treatment) |
      is.na(Plot)
  )
#good 

#############################
#1000 hour fuels        #####
#############################

# Make sure spp. codes are valid (should only be NONE, PIPO, CELE, JUOC, UNK)
unique(hr1000$Species)
#There are NA values, should not be there

# Finding NA values:
hr1000 %>%
  mutate(Row = row_number(), .before =1) %>%
  filter(is.na(Species))
#Row 171 looks like duplicate row in datasheet.

# Checking for other duplicates:
hr1000 %>%
  duplicated() %>%
  which()
#508 and 564 look like duplicates. Not going to remove in case they are unique
#1000-hr entries that just happened to have the same diameter/decay class as row 
#above, need to check with Jim before removing.

#Safe to remove row 171 because there is no other info in that row (spp., 
#diameter, decay class, etc.):
hr1000 <- hr1000[-171,]

# Frequency table of spp.
ggplot(hr1000, aes(x=Species)) +
  geom_bar(fill = "tomato", color = "black") +
  labs(title = "1000-Hr Species Distribution",
       x = "Species Code",
       y = "Frequency"
  )
#Almost entirely PIPO 

# Only Y/N values for Elevated
hr1000 %>%
  filter(!Elevated %in% c("Y", "N") & !is.na(Elevated))
#ok

# When species = NONE, diameter, decay, elevated should all be NA
hr1000 %>%
  filter(
    Species == "NONE" &
      (!is.na(Diameter) |
       !is.na(`Decay class`) |
       !is.na(Elevated)
      ))
#good

# Each plot belongs to one treatment 
hr1000 %>%
  group_by(Stand, Plot) %>%
  summarize(
    n_treatments = n_distinct(Treatment),
    .groups = "drop"
  ) %>%
  filter(n_treatments > 1)
#good 

# Missing stand, treatment, plot values
hr1000 %>%
  filter(
    is.na(Stand) |
      is.na(Treatment) |
      is.na(Plot))
#good

#############################
#Litter and Duff        #####
#############################

# Missing values 
colSums(is.na(ld))
#good 

# Check validity of spp.
unique(ld$`6m litter/duff type`)
#Thinking these names can be combined (but not sure?), waiting on confirmation:
  #"GRASS" and "GRASS THATCH" -> "GRASS THATCH"
  #"BARE" and "BARE SOIL" -> "BARE SOIL"
  #Unsure about "PIPO BARK"

unique(ld$`12m litter/duff type`)
  #Same as above, plus unsure about "BARK PLATE PIPO", "BARK PIPO", "PIPO BARK",
  #"BARK FLATES (PIPO)", and "PIPO/WOOD ROT" and whether they can be combined
  #into already existing litter/duff types


# Each plot belongs to one treatment
ld %>%
  group_by(Stand, Plot) %>%
  summarize(
    n_treatments = n_distinct(Treatment),
    .groups = "drop"
  ) %>%
  filter(n_treatments > 1)
#ok