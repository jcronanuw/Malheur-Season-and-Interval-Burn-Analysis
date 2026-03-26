#This code creates a fuels matrix. It cleans the 2025 fuels data and puts it in the same format as the data used in Westlind and Kerns 2017. It joins the 1, 10, 100 hour fuels data with the 1000-hr data, and converts every value to Mg/ha (megagram/hectare). It also takes the 2008 fuels data and does the same cleaning.

#Nathan Wade
#March 5, 2026


library(tidyverse)

#set input & output folders to import data####
##dataframe folders
input <- "C:/Users/NathanWade/Box/SIB/Cronan Wade/3_Data/01_Raw_Data/Fuels"
output <- "C:/Users/NathanWade/Box/SIB/Cronan Wade/3_Data/02_Clean_Data/Fuels"

#importing data
fine25 <- read.csv(paste0(input, "/2025_fuels_1_10_100_hr.csv"))
big25 <- read.csv(paste0(input, "/2025_fuels_1000_hr.csv"))
litter25 <- read.csv(paste0(input, "/2025_fuels_litter_duff.csv"))
fuelsKW <- read.csv(paste0(input, "/KernsWestlind_Fuels_12_13_14.csv"))
treatments <- read.csv(paste0(input, "/Plot_treatments.csv"))

#removing unnecessary columns
fine25 <- fine25[, -c(9)]
big25 <- big25[, -c(10)]
litter25 <- litter25[, -c(10)]
fuelsKW <- fuelsKW[, -c(4, 5, 7, 13, 14, 15)]

#filling NA values with 0 in 1000-hr data
big25[is.na(big25)] <- 0
fuelsKW[is.na(fuelsKW)] <- 0

#defining column types
fine25 <- fine25 %>% mutate(Year = as.factor(Year), 
                        Direction = as.factor(Direction))

big25 <- big25 %>% mutate(Year = as.factor(Year),
                      Direction = as.factor(Direction),
                      Decay.class = as.factor(Decay.class))

litter25 <- litter25 %>% mutate(Year = as.factor(Year), 
                            Direction = as.factor(Direction))

fuelsKW <- fuelsKW %>% mutate(year = as.factor(year), 
                              azimuth = as.factor(azimuth))


#####################################
#2025 fine fuels prep (1, 10, 100-hr)####
#converting number of hits to tons/acres
##1-hr fuels
fine25$hrone <- (11.64*fine25$hrone*0.0151*0.48*1.13)/(1.2*3.28084)

##10-hr fuels
fine25$hrten <- (11.64*fine25$hrten*0.289*0.48*1.13)/(1.2*3.28084)

##100-hr fuels
fine25$hrhun <- (11.64*fine25$hrhun*2.76*0.40*1.13)/(20.1*3.28084)


#converting from tons/acre to Mg/ha####
##1-hr fuels
fine25$hrone <- fine25$hrone*(2.471/1.1023113109244)

##10-hr fuels
fine25$hrten <- fine25$hrten*(2.471/1.1023113109244)

##100-hr fuels
fine25$hrhun <- fine25$hrhun*(2.471/1.1023113109244)


#####################################
#2025 1000-hr fuels prep####
#converting diameters from cm to in
big25$Diameter <- big25$Diameter/2.54

##squaring all diameters
big25$Diameter <- big25$Diameter*big25$Diameter

##adding a column for decay class being sound (1-3) or rotton (4-5)
big25 <- big25 %>% mutate(Class = case_when(
  (Decay.class %in% c(0, 1, 2, 3)) ~ "Sound",
  (Decay.class %in% c(4, 5)) ~ "Rotten"))

#adding squared diameters together by species and sound/rotten
big25 <- big25 %>% group_by(Year, Stand, Treatment, Plot, Direction, Species, Class) %>%
  summarise(Diam2 = sum(Diameter, na.rm = TRUE), .groups = 'drop')


#calculating tons/acre for each species and by sound/rotten
big25 <- big25 %>% mutate(TonsAcre = case_when(
  Species == "PIPO" & Class == "Sound" ~ (11.64*Diam2*0.40*1.13)/(20.1*3.28084),
  Species == "PIPO" & Class == "Rotten" ~ (11.64*Diam2*0.30*1.13)/(20.1*3.28084),
  Species == "JUOC" & Class == "Sound" ~ (11.64*Diam2*0.54*1.13)/(20.1*3.28084),
  Species == "JUOC" & Class == "Rotten" ~ (11.64*Diam2*0.405*1.13)/(20.1*3.28084),
  Species == "CELE" & Class == "Sound" ~ (11.64*Diam2*0.81*1.13)/(20.1*3.28084),
  Species == "UNK" & Class == "Sound" ~ (11.64*Diam2*0.40*1.13)/(20.1*3.28084),
  Species == "UNK" & Class == "Rotten" ~ (11.64*Diam2*0.30*1.13)/(20.1*3.28084),
  Species == "NONE" & Class == "Sound" ~ Diam2*0,
  TRUE ~ Diam2))

#converting tons/acre to Mg/ha
big25$hrthou1 <- big25$TonsAcre*(2.471/1.1023113109244)


#selecting relevant columns
big25 <- big25 %>% select(Year, Stand, Treatment, Plot, Direction, Species, Class, hrthou1)

#adding a column for whether or not there are any fuels on the transect or not
big25 <- big25 %>% mutate(Presence = case_when(
  Species == "PIPO" ~ "FUEL",
  Species == "CELE" ~ "FUEL",
  Species == "JUOC" ~ "FUEL",
  Species == "UNK" ~ "FUEL",
  Species == "NONE" ~ "NONE"))


#summing Mg/ha to the transect level####
big25 <- big25 %>% group_by(Year, Stand, Treatment, Plot, Direction, Presence) %>%
  summarise(hrthou = sum(hrthou1, na.rm = TRUE), .groups = 'drop')

#selecting relevant columns for joining
big25small <- big25 %>% select(Plot, Direction, hrthou)


###############################################
#2025 litter/duff prep####
#averaging litter/duff 6 and 12 m depths
litter25 <- litter25 %>% mutate(landd = (Six.m.depth + Twelve.m.depth)/2)

#converting depths from mm to cm
litter25$landd = litter25$landd/10

#selecting relevent columns for joining
litter25small <- litter25 %>% select(Plot, Direction, landd)


############################################
#joining the 2025 fuels data together####
fuels25 <- left_join(fine25, litter25small, by = c("Plot", "Direction"))

fuels25 <- left_join(fuels25, big25small, by = c("Plot", "Direction"))

#changing stand names from D14 to Driveway 14
fuels25 <- fuels25 %>% mutate(Stand = case_when(
  (Stand == "Driveway 14") ~ "D14",
  (Stand == "Driveway 26") ~ "D26",
  (Stand == "Driveway 28") ~ "D28",
  (Stand == "Kidd Flat") ~ "KF",
  (Stand == "Trout") ~ "Trout"))


###############################################
#prepping the 2012/13/14 fuels data from Westlind and Kerns 2017 to merge####
#changing column names to better merge
fuelsKW <- fuelsKW %>% rename(Year = year)
fuelsKW <- fuelsKW %>% rename(Direction = azimuth)

#removing Driveway 17
fuelsKW <- fuelsKW %>% filter(!Stand == "D17")

#assigning treatment
fuelsKW <- fuelsKW %>% left_join(treatments %>% select(Plot, Treatment), by = "Plot")


#################################################
#joining the Westlind and Kerns fuels with 2025 fuels####
#transect direction level
fuels <- rbind(fuelsKW, fuels25)

#averaging to the plot level
fuelsPlot <- fuels %>% group_by(Year, Treatment, Stand, Plot) %>%
  summarise(hrone = mean(hrone, na.rm = TRUE),
            hrten = mean(hrten, na.rm = TRUE),
            hrhun = mean(hrhun, na.rm = TRUE),
            hrthou = mean(hrthou, na.rm = TRUE)) %>%
  ungroup()


#exporting####
write.csv(fuels, paste0(output, "/Fuels_direction.csv"))
write.csv(fuelsPlot, paste0(output, "/Fuels_plot.csv"))


