#This code cleans the 2025 fuels data and puts it in the same format as the data used in Westlind and Kerns 2017. It joins the 1, 10, 100 hour fuels data with the 1000-hr data, and converts every value to Mg/ha (megagram/hectare).

library(tidyverse)

#set input & output folders to import data####
##dataframe folders
input <- "C:/Users/NathanWade/Box/01. nathan.wade Workspace/Season of burn/Exploration/fuels"

#importing data
fine <- read.csv(paste0(input, "/2025_fuels_1_10_100_hr.csv"))
big <- read.csv(paste0(input, "/2025_fuels_1000_hr.csv"))
litter <- read.csv(paste0(input, "/2025_fuels_litter_duff.csv"))

#removing unnecessary columns
fine <- fine[, -c(9)]
big <- big[, -c(10)]
litter <- litter[, -c(10)]

#filling NA values with 0 in 1000-hr data
big[is.na(big)] <- 0

#defining column types
fine <- fine %>% mutate(Year = as.factor(Year), 
                        Direction = as.factor(Direction))

big <- big %>% mutate(Year = as.factor(Year),
                      Direction = as.factor(Direction),
                      Decay.class = as.factor(Decay.class))

litter <- litter %>% mutate(Year = as.factor(Year), 
                            Direction = as.factor(Direction))


#####################################
#fine fuels prep (1, 10, 100-hr)####
#converting number of hits to tons/acres
##1-hr fuels
fine$hrone <- (11.64*fine$hrone*0.0151*0.48*1.13)/(1.2*3.28084)

##10-hr fuels
fine$hrten <- (11.64*fine$hrten*0.289*0.48*1.13)/(1.2*3.28084)

##100-hr fuels
fine$hrhun <- (11.64*fine$hrhun*2.76*0.40*1.13)/(20.1*3.28084)


#converting from tons/acre to Mg/ha####
##1-hr fuels
fine$hrone <- fine$hrone*(2.471/1.1023113109244)

##10-hr fuels
fine$hrten <- fine$hrten*(2.471/1.1023113109244)

##100-hr fuels
fine$hrhun <- fine$hrhun*(2.471/1.1023113109244)


#####################################
#1000-hr fuels prep####
#converting diameters from cm to in
big$Diameter <- big$Diameter/2.54

##squaring all diameters
big$Diameter <- big$Diameter*big$Diameter

##adding a column for decay class being sound (1-3) or rotton (4-5)
big <- big %>% mutate(Class = case_when(
  (Decay.class %in% c(0, 1, 2, 3)) ~ "Sound",
  (Decay.class %in% c(4, 5)) ~ "Rotten"))

#adding squared diameters together by species and sound/rotten
big <- big %>% group_by(Year, Stand, Treatment, Plot, Direction, Species, Class) %>%
  summarise(Diam2 = sum(Diameter, na.rm = TRUE), .groups = 'drop')


#calculating tons/acre for each species and by sound/rotten
big <- big %>% mutate(TonsAcre = case_when(
  Species == "PIPO" & Class == "Sound" ~ (11.64*Diam2*0.40*1.13)/(20.1*3.28084),
  Species == "PIPO" & Class == "Rotten" ~ (11.64*Diam2*0.30*1.13)/(20.1*3.28084),
  Species == "JUOC" & Class == "Sound" ~ (11.64*Diam2*0.54*1.13)/(20.1*3.28084),
  Species == "JUOC" & Class == "Rotten" ~ (11.64*Diam2*0.405*1.13)/(20.1*3.28084),
  Species == "CELE" & Class == "Sound" ~ (11.64*Diam2*0.81*1.13)/(20.1*3.28084),
  Species == "NONE" & Class == "Sound" ~ Diam2*0,
  TRUE ~ Diam2))

#converting tons/acre to Mg/ha
big$hrthou <- big$TonsAcre*(2.471/1.1023113109244)


#selecting relevant columns
big <- big %>% select(Year, Stand, Treatment, Plot, Direction, Species, Class, hrthou)

