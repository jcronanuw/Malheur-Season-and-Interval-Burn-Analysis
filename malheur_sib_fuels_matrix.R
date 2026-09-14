#This code creates a fuels matrix. It cleans the 2025 fuels data and puts it in the same format as the data used in Westlind and Kerns 2017. It joins the 1, 10, 100 hour fuels data with the 1000-hr data, and converts every value to Mg/ha (megagram/hectare). It also takes the 2008 fuels data and does the same cleaning.

#Nathan Wade
#March 5, 2026


library(tidyverse)
library(glmmTMB)
library(DHARMa)
library(car)
library(emmeans)

#set input & output folders to import data####
##dataframe folders
input <- "C:/Users/NathanWade/Box/SIB/Cronan Wade/3_Data/01_Raw_Data/Fuels"
output <- "C:/Users/NathanWade/Box/SIB/Cronan Wade/3_Data/02_Clean_Data/Fuels"

output1 <- "C:/Users/NathanWade/Box/01. nathan.wade Workspace/Season of burn"

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

#changin 1314 to 2013/2014
fuelsKW <- fuelsKW %>% mutate(
  Year = case_when(
    Year == "1314" ~ "2013/2014",
    TRUE ~ Year))


#################################################
#joining the Westlind and Kerns fuels with 2025 fuels####
#transect direction level
fuels <- rbind(fuelsKW, fuels25)

#adding a season of burn (SOB), interval of burn (IB), and unique combinations columns to the dataframe
fuels <- fuels %>% 
  mutate(SOB = case_when(
    (Treatment == "Fall 5") ~ "Fall",
    (Treatment == "Fall 15") ~ "Fall",
    (Treatment == "Spring 5") ~ "Spring",
    (Treatment == "Spring 15") ~ "Spring",
    (Treatment == "Control") ~ "Control")) %>%
  mutate(SOB = as.factor(SOB))

fuels <- fuels %>% 
  mutate(IB = case_when(
    (Treatment == "Fall 5") ~ "5 yr",
    (Treatment == "Fall 15") ~ "15 yr",
    (Treatment == "Spring 5") ~ "5 yr",
    (Treatment == "Spring 15") ~ "15 yr",
    (Treatment == "Control") ~ "0 yr")) %>%
  mutate(IB = as.factor(IB))

#adding plot and subplot ID for SOB and IB
fuels$PlotID <- paste(fuels$Stand, fuels$SOB, sep = " ")
fuels$SubplotID <- paste(fuels$PlotID, fuels$IB, sep = " ")

#defining column types
fuels <- fuels %>%
  mutate(Year = as.factor(Year),
         Plot = as.factor(Plot),
         Stand = as.factor(Stand),
         Direction = as.factor(Direction),
         Treatment = as.factor(Treatment),
         SOB = as.factor(SOB),
         IB = as.factor(IB),
         PlotID = as.factor(PlotID), 
         SubplotID = as.factor(SubplotID))


#averaging to the plot level
fuelsPlot <- fuels %>% group_by(Year, Stand, Treatment, SOB, IB, PlotID, SubplotID, Plot) %>%
  summarise(hrone = mean(hrone, na.rm = TRUE),
            hrten = mean(hrten, na.rm = TRUE),
            hrhun = mean(hrhun, na.rm = TRUE),
            hrthou = mean(hrthou, na.rm = TRUE),
            landd = mean(landd, na.rm = TRUE)) %>%
  ungroup()


#averaging to the IB level####
fuelssplit <- fuelsPlot %>% group_by(Year, Stand, Treatment, SOB, IB, PlotID, SubplotID) %>%
  summarise(hrone = mean(hrone, na.rm = TRUE),
            hrten = mean(hrten, na.rm = TRUE),
            hrhun = mean(hrhun, na.rm = TRUE),
            hrthou = mean(hrthou, na.rm = TRUE),
            landd = mean(landd, na.rm = TRUE)) %>%
  ungroup()


#histograms####
hist(fuelssplit$hrone, breaks = seq(from = 0, to = 0.15, by = 0.01))
hist(fuelssplit$hrten, breaks = seq(from = 0, to = 5, by = 0.1))
hist(fuelssplit$hrhun, breaks = seq(from = 0, to = 4, by = 0.1))
hist(fuelssplit$hrthou, breaks = seq(from = 0, to = 35, by = 1))
hist(fuelssplit$landd, breaks = seq(from = 0, to = 5, by = 0.5))


#averaging to the treatment level####
fuelstreat <- fuelssplit %>% group_by(Year, SOB, IB, Treatment) %>%
  summarise(hrone = mean(hrone, na.rm = TRUE),
            hrten = mean(hrten, na.rm = TRUE),
            hrhun = mean(hrhun, na.rm = TRUE),
            hrthou = mean(hrthou, na.rm = TRUE),
            landd = mean(landd, na.rm = TRUE)) %>%
  ungroup()


#making a table of treatment means####


#exporting####
write.csv(fuels, paste0(output, "/Fuels_direction.csv"))
write.csv(fuelssplit, paste0(output, "/Fuels_plot.csv"))
write.csv(fuelstreat, paste0(output, "/Fuels_treatment.csv"))

#######################################
#graphing and adding letters####
fuelsgraph <- fuels %>% select(Year, Stand, SOB, IB, Treatment, Plot, Direction, hrone, hrten, hrhun, hrthou, landd)
fuelsgraph <- fuelsgraph %>% pivot_longer(col= (hrone:landd), names_to = "Fuel", values_to = "Load")

fuelsgraph$Treatment <- factor(fuelsgraph$Treatment, levels = c("Control", "Fall 5", "Fall 15", "Spring 5", "Spring 15"))
fuelsgraph$Fuel <- factor(fuelsgraph$Fuel, levels = c("hrone", "hrten", "hrhun", "hrthou", "landd"))
fuelsgraph$Year <- factor(fuelsgraph$Year, levels = c("2012", "2013/2014", "2025"))

## adding a longer fuel name column
fuelsgraph <- fuelsgraph %>% 
  mutate(FuelsName = case_when(
    (Fuel == "hrone") ~ "1-hr",
    (Fuel == "hrten") ~ "10-hr",
    (Fuel == "hrhun") ~ "100-hr",
    (Fuel == "hrthou") ~ "1000-hr",
    (Fuel == "landd") ~ "Litter/duff"))

fuelsgraph$FuelsName <- factor(fuelsgraph$FuelsName, levels = c("1-hr", "10-hr", "100-hr", "1000-hr", "Litter/duff"))

# adding letters from model outputs
letters <- read.csv(paste0(output1, "/letters.csv"))

## defining column types
letters <- letters %>% mutate(Year = as.factor(Year), 
                              Treatment = as.factor(Treatment),
                              Fuel = as.factor(Fuel),
                              FuelsName = as.factor(FuelsName))


## calculating the maximum fuel loading to assign letter positions
letter_positions <- fuelsgraph %>% group_by(Year, Treatment, FuelsName) %>%
  summarise(y_position = max(Load, na.rm = TRUE), .groups = 'drop') %>%
  left_join(letters, by = c("Year", "Treatment", "FuelsName")) 

letter_positions <- letter_positions %>%
mutate(y_position = case_when(
  (FuelsName == "1-hr") ~ y_position + 0.3,
  (FuelsName == "10-hr") ~ y_position + 3,
  (FuelsName == "100-hr") ~ y_position + 3,
  (FuelsName == "1000-hr") ~ y_position + 15,
  (FuelsName == "Litter/duff") ~ y_position + 3))

## removing 2012 treatments that aren't Control
letter_positions <- letter_positions[!is.na(letter_positions$.group),]

## changing 2012 to 2013/2014
letter_positions <- letter_positions %>% mutate(
  Year = case_when(
    Year == "2012" ~ "2013/2014",
    TRUE ~ Year))

#4 panel graph of every year####
(ggplot(fuelsgraph %>% filter(!Fuel == "landd"), aes(x = Year, y = Load, fill = Treatment)) +
   geom_boxplot() +
   stat_summary(fun = mean, 
                geom = "point", 
                position = position_dodge(width = 0.76),
                shape = 18, 
                size = 2, 
                color = "red") +
   facet_wrap(~Fuel, scales = "free_y") +
   theme_bw(13) + 
   theme(legend.position = "bottom",
         legend.title = element_text(size = 12),
         legend.text = element_text(size = 10)) + 
   scale_fill_manual(values = c("Control" = "khaki", "Fall 5" = "coral", "Fall 15" = "coral3", "Spring 5" = "springgreen2", "Spring 15" = "springgreen4"))) +
  labs(x = "Fuel type", y = "Mg/ha") + 
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 60, hjust = 1))


# 2025 graphs####
twentyfive <- fuelsgraph %>% filter(Year == "2025")

## one-hr fuels
(ggplot(twentyfive %>% filter(Fuel == "hrone"), aes(x = Year, y = Load, fill = Treatment)) +
    geom_boxplot() +
    stat_summary(fun = mean, 
                 geom = "point", 
                 position = position_dodge(width = 0.76),
                 shape = 18, 
                 size = 2, 
                 color = "red") +
    #facet_wrap(~Fuel, scales = "free_y") +
    theme_bw(13) + 
    theme(legend.position = "bottom",
          legend.title = element_text(size = 12),
          legend.text = element_text(size = 10)) + 
    scale_fill_manual(values = c("Control" = "khaki", "Fall 5" = "coral", "Fall 15" = "coral3", "Spring 5" = "springgreen2", "Spring 15" = "springgreen4"))) +
  labs(x = NULL, y = "Mg/ha")

## ten-hr fuels
(ggplot(twentyfive %>% filter(Fuel == "hrten"), aes(x = Year, y = Load, fill = Treatment)) +
    geom_boxplot() +
    stat_summary(fun = mean, 
                 geom = "point", 
                 position = position_dodge(width = 0.76),
                 shape = 18, 
                 size = 2, 
                 color = "red") +
    #facet_wrap(~Fuel, scales = "free_y") +
    theme_bw(13) + 
    theme(legend.position = "bottom",
          legend.title = element_text(size = 12),
          legend.text = element_text(size = 10)) + 
    scale_fill_manual(values = c("Control" = "khaki", "Fall 5" = "coral", "Fall 15" = "coral3", "Spring 5" = "springgreen2", "Spring 15" = "springgreen4"))) +
  labs(x = NULL, y = "Mg/ha")

## hundred-hr fuels
(ggplot(twentyfive %>% filter(Fuel == "hrhun"), aes(x = Year, y = Load, fill = Treatment)) +
    geom_boxplot() +
    stat_summary(fun = mean, 
                 geom = "point", 
                 position = position_dodge(width = 0.76),
                 shape = 18, 
                 size = 2, 
                 color = "red") +
    #facet_wrap(~Fuel, scales = "free_y") +
    theme_bw(13) + 
    theme(legend.position = "bottom",
          legend.title = element_text(size = 12),
          legend.text = element_text(size = 10)) + 
    scale_fill_manual(values = c("Control" = "khaki", "Fall 5" = "coral", "Fall 15" = "coral3", "Spring 5" = "springgreen2", "Spring 15" = "springgreen4"))) +
  labs(x = NULL, y = "Mg/ha")

## thousand-hr fuels
(ggplot(twentyfive %>% filter(Fuel == "hrthou"), aes(x = Year, y = Load, fill = Treatment)) +
    geom_boxplot() +
    stat_summary(fun = mean, 
                 geom = "point", 
                 position = position_dodge(width = 0.76),
                 shape = 18, 
                 size = 2, 
                 color = "red") +
    #facet_wrap(~Fuel, scales = "free_y") +
    theme_bw(13) + 
    theme(legend.position = "bottom",
          legend.title = element_text(size = 12),
          legend.text = element_text(size = 10)) + 
    scale_fill_manual(values = c("Control" = "khaki", "Fall 5" = "coral", "Fall 15" = "coral3", "Spring 5" = "springgreen2", "Spring 15" = "springgreen4"))) +
  labs(x = NULL, y = "Mg/ha")

## litter and duff depth
(ggplot(twentyfive %>% filter(Fuel == "landd"), aes(x = Year, y = Load, fill = Treatment)) +
    geom_boxplot() +
    stat_summary(fun = mean, 
                 geom = "point", 
                 position = position_dodge(width = 0.76),
                 shape = 18, 
                 size = 2, 
                 color = "red") +
    #facet_wrap(~Fuel, scales = "free_y") +
    theme_bw(13) + 
    theme(legend.position = "bottom",
          legend.title = element_text(size = 12),
          legend.text = element_text(size = 10)) + 
    scale_fill_manual(values = c("Control" = "khaki", "Fall 5" = "coral", "Fall 15" = "coral3", "Spring 5" = "springgreen2", "Spring 15" = "springgreen4"))) +
  labs(x = NULL, y = "Depth (cm)")


# 2013/2014 and 2025 graphs####
fuelsgraph1325 <- fuelsgraph %>% 
  mutate(Year = case_when(
    (Year == "2012" & Treatment == "Control") ~ "2013/2014",
    TRUE ~ Year))

fuelsgraph1325 <- fuelsgraph1325 %>% filter(!Year == "2012")


## one-hr fuels
(ggplot(fuelsgraph1325 %>% filter(FuelsName == "1-hr"), aes(x = Year, y = Load, fill = Treatment)) +
   geom_boxplot() +
   stat_summary(fun = mean, 
                geom = "point", 
                position = position_dodge(width = 0.76),
                shape = 18, 
                size = 2, 
                color = "red") +
    geom_text(data = letter_positions %>% filter(FuelsName == "1-hr"),
              aes(x = Year, 
                  y = y_position,
                  group = Treatment,
                  label = .group1),
              position = position_dodge(width = 0.75),
              inherit.aes = FALSE) + 
   #facet_wrap(~Fuel, scales = "free_y") +
   theme_bw(13) + 
   theme(legend.position = "bottom",
         legend.title = element_text(size = 12),
         legend.text = element_text(size = 10)) + 
   scale_fill_manual(values = c("Control" = "khaki", "Fall 5" = "coral", "Fall 15" = "coral3", "Spring 5" = "springgreen2", "Spring 15" = "springgreen4"))) +
  labs(x = "Year", y = "Mg/ha")

## ten-hr fuels
(ggplot(fuelsgraph1325 %>% filter(FuelsName == "10-hr"), aes(x = Year, y = Load, fill = Treatment)) +
    geom_boxplot() +
    stat_summary(fun = mean, 
                 geom = "point", 
                 position = position_dodge(width = 0.76),
                 shape = 18, 
                 size = 2, 
                 color = "red") +
    geom_text(data = letter_positions %>% filter(FuelsName == "10-hr"),
              aes(x = Year, 
                  y = y_position,
                  group = Treatment,
                  label = .group1),
              position = position_dodge(width = 0.75),
              inherit.aes = FALSE) + 
    #facet_wrap(~Fuel, scales = "free_y") +
    theme_bw(13) + 
    theme(legend.position = "bottom",
          legend.title = element_text(size = 12),
          legend.text = element_text(size = 10)) + 
    scale_fill_manual(values = c("Control" = "khaki", "Fall 5" = "coral", "Fall 15" = "coral3", "Spring 5" = "springgreen2", "Spring 15" = "springgreen4"))) +
  labs(x = "Year", y = "Mg/ha")

## hundred-hr fuels
(ggplot(fuelsgraph1325 %>% filter(FuelsName == "100-hr"), aes(x = Year, y = Load, fill = Treatment)) +
    geom_boxplot() +
    stat_summary(fun = mean, 
                 geom = "point", 
                 position = position_dodge(width = 0.76),
                 shape = 18, 
                 size = 2, 
                 color = "red") +
    geom_text(data = letter_positions %>% filter(FuelsName == "100-hr"),
              aes(x = Year, 
                  y = y_position,
                  group = Treatment,
                  label = .group1),
              position = position_dodge(width = 0.75),
              inherit.aes = FALSE) + 
    #facet_wrap(~Fuel, scales = "free_y") +
    theme_bw(13) + 
    theme(legend.position = "bottom",
          legend.title = element_text(size = 12),
          legend.text = element_text(size = 10)) + 
    scale_fill_manual(values = c("Control" = "khaki", "Fall 5" = "coral", "Fall 15" = "coral3", "Spring 5" = "springgreen2", "Spring 15" = "springgreen4"))) +
  labs(x = "Year", y = "Mg/ha")

## thousand-hr fuels
(ggplot(fuelsgraph1325 %>% filter(FuelsName == "1000-hr"), aes(x = Year, y = Load, fill = Treatment)) +
    geom_boxplot() +
    stat_summary(fun = mean, 
                 geom = "point", 
                 position = position_dodge(width = 0.76),
                 shape = 18, 
                 size = 2, 
                 color = "red") +
    geom_text(data = letter_positions %>% filter(FuelsName == "1000-hr"),
              aes(x = Year, 
                  y = y_position,
                  group = Treatment,
                  label = .group1),
              position = position_dodge(width = 0.75),
              inherit.aes = FALSE) + 
    #facet_wrap(~Fuel, scales = "free_y") +
    theme_bw(13) + 
    theme(legend.position = "bottom",
          legend.title = element_text(size = 12),
          legend.text = element_text(size = 10)) + 
    scale_fill_manual(values = c("Control" = "khaki", "Fall 5" = "coral", "Fall 15" = "coral3", "Spring 5" = "springgreen2", "Spring 15" = "springgreen4"))) +
  labs(x = "Year", y = "Mg/ha")

## litter and duff depth
(ggplot(fuelsgraph1325 %>% filter(FuelsName == "Litter/duff"), aes(x = Year, y = Load, fill = Treatment)) +
    geom_boxplot() +
    stat_summary(fun = mean, 
                 geom = "point", 
                 position = position_dodge(width = 0.76),
                 shape = 18, 
                 size = 2, 
                 color = "red") +
    geom_text(data = letter_positions %>% filter(FuelsName == "Litter/duff"),
              aes(x = Year, 
                  y = y_position,
                  group = Treatment,
                  label = .group1),
              position = position_dodge(width = 0.75),
              inherit.aes = FALSE) + 
    #facet_wrap(~Fuel, scales = "free_y") +
    theme_bw(13) + 
    theme(legend.position = "bottom",
          legend.title = element_text(size = 12),
          legend.text = element_text(size = 10)) + 
    scale_fill_manual(values = c("Control" = "khaki", "Fall 5" = "coral", "Fall 15" = "coral3", "Spring 5" = "springgreen2", "Spring 15" = "springgreen4"))) +
  labs(x = "Year", y = "Depth (cm)")


## combined 1 and 10-hr fuel graphs####
(ggplot(fuelsgraph1325 %>% filter(FuelsName == "1-hr" | FuelsName == "10-hr"), aes(x = Year, y = Load, fill = Treatment)) +
   geom_boxplot() +
   stat_summary(fun = mean, 
                geom = "point", 
                position = position_dodge(width = 0.76),
                shape = 18, 
                size = 2, 
                color = "red") +
   geom_text(data = letter_positions %>% filter(FuelsName == "1-hr" | FuelsName == "10-hr"),
             aes(x = Year, 
                 y = y_position,
                 group = Treatment,
                 label = .group1),
             position = position_dodge(width = 0.75),
             inherit.aes = FALSE) + 
   facet_wrap(~FuelsName, scales = "free_y") +
   theme_bw(13) + 
   theme(legend.position = "bottom",
         legend.title = element_text(size = 12),
         legend.text = element_text(size = 10)) + 
   scale_fill_manual(values = c("Control" = "khaki", "Fall 5" = "coral", "Fall 15" = "coral3", "Spring 5" = "springgreen2", "Spring 15" = "springgreen4"))) +
  labs(x = "Year", y = "Mg/ha")


## combined 100 and 1000-hr fuel graphs####
(ggplot(fuelsgraph1325 %>% filter(FuelsName == "100-hr" | FuelsName == "1000-hr"), aes(x = Year, y = Load, fill = Treatment)) +
   geom_boxplot() +
   stat_summary(fun = mean, 
                geom = "point", 
                position = position_dodge(width = 0.76),
                shape = 18, 
                size = 2, 
                color = "red") +
   geom_text(data = letter_positions %>% filter(FuelsName == "100-hr" | FuelsName == "1000-hr"),
             aes(x = Year, 
                 y = y_position,
                 group = Treatment,
                 label = .group1),
             position = position_dodge(width = 0.75),
             inherit.aes = FALSE) + 
   facet_wrap(~FuelsName, scales = "free_y") +
   theme_bw(13) + 
   theme(legend.position = "bottom",
         legend.title = element_text(size = 12),
         legend.text = element_text(size = 10)) + 
   scale_fill_manual(values = c("Control" = "khaki", "Fall 5" = "coral", "Fall 15" = "coral3", "Spring 5" = "springgreen2", "Spring 15" = "springgreen4"))) +
  labs(x = "Year", y = "Mg/ha")


#######################################
#GLMMs####
#1-hr fuels####
oneModel <- glmmTMB(hrone ~ Treatment
                    + (1|Stand/SOB),
                    #ziformula = ~ Treatment,
                    family = tweedie(link = "log"), 
                    data = fuelssplit %>% filter(Year == "2025"))

hist(fuelssplit$hrone %>% filter(Year == "2025"))
#Model checks
oneRes <- simulateResiduals(oneModel, n = 1000)
plot(oneRes, quantreg = F)
testDispersion(oneRes) # p < 0.05 then model is over or under dispersed
testZeroInflation(oneRes) # p < 0.05 model is zero inflated

Anova(oneModel)
summary(oneModel)


#Inference and marginal means on the response (proportion) scale
oneEmm <- emmeans(oneModel, ~ Treatment, type = "response")
summary(oneEmm)             #marginal means and CIs
pairs(oneEmm)               #treatment contrasts within each year as proportions
plot(oneEmm)


#10-hr fuels####
tenModel <- glmmTMB(hrten ~ Treatment
                    + (1|Stand/SOB), 
                    #ziformula = ~ Treatment,
                    family = gaussian(),  
                    data = fuelssplit %>% filter(Year == "2025"))

diagnose(tenModel)
#Model checks
tenRes <- simulateResiduals(tenModel, n = 1000)
plot(tenRes, quantreg = F)
testDispersion(tenRes) # p < 0.05 then model is over or under dispersed
testZeroInflation(tenRes) # p < 0.05 model is zero inflated

Anova(tenModel)
summary(tenModel)


#Inference and marginal means on the response (proportion) scale
tenEmm <- emmeans(tenModel, ~ Treatment, type = "response")
summary(tenEmm)             #marginal means and CIs
pairs(tenEmm)               #treatment contrasts within each year as proportions
plot(tenEmm)


#100-hr fuels####
hunModel <- glmmTMB(hrhun ~ Treatment
                    + (1|Stand/SOB), 
                    #ziformula = ~ Treatment,
                    family = gaussian(), 
                    data = fuelssplit %>% filter(Year == "2025"))


#Model checks
hunRes <- simulateResiduals(hunModel, n = 1000)
plot(hunRes, quantreg = F)
testDispersion(hunRes) # p < 0.05 then model is over or under dispersed
testZeroInflation(hunRes) # p < 0.05 model is zero inflated

Anova(hunModel)
summary(hunModel)


#Inference and marginal means on the response (proportion) scale
hunEmm <- emmeans(hunModel, ~ Treatment, type = "response")
summary(hunEmm)             #marginal means and CIs
pairs(hunEmm)               #treatment contrasts within each year as proportions
plot(hunEmm)


#1000-hr fuels####
thouModel <- glmmTMB(hrthou ~ Treatment
                    + (1|Stand/SOB), 
                    #ziformula = ~ Treatment,
                    family = tweedie(link = "log"), 
                    data = fuelssplit %>% filter(Year == "2025"))


#Model checks
thouRes <- simulateResiduals(thouModel, n = 1000)
plot(thouRes, quantreg = F)
testDispersion(oneRes) # p < 0.05 then model is over or under dispersed
testZeroInflation(oneRes) # p < 0.05 model is zero inflated

Anova(thouModel)
summary(thouModel)


#Inference and marginal means on the response (proportion) scale
thouEmm <- emmeans(thouModel, ~ Treatment, type = "response")
summary(thouEmm)             #marginal means and CIs
pairs(thouEmm)               #treatment contrasts within each year as proportions
plot(thouEmm)


#litter and duff depth####
landdModel <- glmmTMB(landd ~ Treatment
                    + (1|Stand/SOB), 
                    #ziformula = ~ Treatment,
                    family = gaussian(), 
                    data = fuelssplit %>% filter(Year == "2025"))


#Model checks
landdRes <- simulateResiduals(landdModel, n = 1000)
plot(landdRes, quantreg = F)
testDispersion(landdRes) # p < 0.05 then model is over or under dispersed
testZeroInflation(landdRes) # p < 0.05 model is zero inflated

Anova(landdModel)
summary(landdModel)


#Inference and marginal means on the response (proportion) scale
landdEmm <- emmeans(landdModel, ~ Treatment, type = "response")
summary(landdEmm)             #marginal means and CIs
pairs(landdEmm)               #treatment contrasts within each year as proportions
plot(landdEmm)


##############################################
#GLMMS for both 2013/14 and 2025####
#creating a dataframe that has 2012 controls with the 2013/14 and 2025 data####
fuelssplitYr <- fuelssplit %>%  
  mutate(Year = case_when(
    (Year == "2012" & Treatment == "Control") ~ "2013/2014",
    TRUE ~ Year))

fuelssplitYr <- fuelssplitYr %>% filter(!Year == "2012")

fuelssplitYr$Year <- factor(fuelssplitYr$Year, levels = c("2013/2014", "2025"))


#histograms for the new dataset####
hist(fuelssplitYr$hrone, breaks = seq(from = 0, to = 0.13, by = 0.01))
hist(fuelssplitYr$hrten, breaks = seq(from = 0, to = 5, by = 0.1))
hist(fuelssplitYr$hrhun, breaks = seq(from = 0, to = 4, by = 0.1))
hist(fuelssplitYr$hrthou, breaks = seq(from = 0, to = 35, by = 1))
hist(fuelssplitYr$landd, breaks = seq(from = 0, to = 5, by = 0.5))


#1-hr fuels####
oneModelYr <- glmmTMB(hrone ~ Treatment*Year
                    + (1|Stand/SOB)
                    + (1|SubplotID), 
                    ziformula = ~ Treatment,
                    family = tweedie(), 
                    data = fuelssplitYr)

fixef(oneModelYr)

#Model checks
oneResYr <- simulateResiduals(oneModelYr, n = 1000)
plot(oneResYr, quantreg = F)
testDispersion(oneModelYr) # p < 0.05 then model is over or under dispersed
testZeroInflation(oneModelYr) # p < 0.05 model is zero inflated

Anova(oneModelYr)
summary(oneModelYr)


#Inference and marginal means on the response (proportion) scale
oneEmmYr <- emmeans(oneModelYr, ~ Treatment, type = "response")
summary(oneEmmYr)             #marginal means and CIs
pairs(oneEmmYr)               #treatment contrasts within each year as proportions
plot(oneEmmYr)


#10-hr fuels####
tenModelYr <- glmmTMB(hrten ~ Treatment*Year
                    + (1|Stand/SOB) 
                    + (1|SubplotID), 
                    #ziformula = ~ Treatment,
                    family = tweedie(), 
                    data = fuelssplitYr)

diagnose(tenModelYr)
#Model checks
tenResYr <- simulateResiduals(tenModelYr, n = 1000)
plot(tenResYr, quantreg = F)
testDispersion(tenModelYr) # p < 0.05 then model is over or under dispersed
testZeroInflation(tenModelYr) # p < 0.05 model is zero inflated

Anova(tenModel)
summary(tenModel)


#Inference and marginal means on the response (proportion) scale
tenEmmYr <- emmeans(tenModelYr, ~ Treatment, type = "response")
summary(tenEmmYr)             #marginal means and CIs
pairs(tenEmmYr)               #treatment contrasts within each year as proportions
plot(tenEmmYr)


#100-hr fuels####
hunModelYr <- glmmTMB(hrhun ~ Treatment*Year
                    + (1|Stand/SOB) 
                    + (1|SubplotID),
                    #ziformula = ~ Treatment,
                    family = gaussian(), 
                    data = fuelssplitYr)


#Model checks
hunResYr <- simulateResiduals(hunModelYr, n = 1000)
plot(hunResYr, quantreg = F)
testDispersion(hunModelYr) # p < 0.05 then model is over or under dispersed
testZeroInflation(hunModelYr) # p < 0.05 model is zero inflated

Anova(hunModelYr)
summary(hunModelYr)


#Inference and marginal means on the response (proportion) scale
hunEmmYr <- emmeans(hunModelYr, ~ Treatment, type = "response")
summary(hunEmmYr)             #marginal means and CIs
pairs(hunEmmYr)               #treatment contrasts within each year as proportions
plot(hunEmmYr)


#1000-hr fuels####
thouModelYr <- glmmTMB(hrthou ~ Treatment*Year
                     + (1|Stand/SOB) 
                     + (1|SubplotID),
                     #ziformula = ~ Treatment,
                     family = tweedie(link = "log"), 
                     data = fuelssplitYr)


#Model checks
thouResYr <- simulateResiduals(thouModelYr, n = 1000)
plot(thouResYr, quantreg = F)
testDispersion(thouModelYr) # p < 0.05 then model is over or under dispersed
testZeroInflation(thouModelYr) # p < 0.05 model is zero inflated

Anova(thouModelYr)
summary(thouModelYr)


#Inference and marginal means on the response (proportion) scale
thouEmmYr <- emmeans(thouModelYr, ~ Treatment, type = "response")
summary(thouEmmYr)             #marginal means and CIs
pairs(thouEmmYr)               #treatment contrasts within each year as proportions
plot(thouEmmYr)


#litter and duff depth####
landdModelYr <- glmmTMB(landd ~ Treatment*Year
                      + (1|Stand/SOB) 
                      + (1|SubplotID),
                      #ziformula = ~ Treatment,
                      family = gaussian(), 
                      data = fuelssplitYr)


#Model checks
landdResYr <- simulateResiduals(landdModelYr, n = 1000)
plot(landdResYr, quantreg = F)
testDispersion(landdModelYr) # p < 0.05 then model is over or under dispersed
testZeroInflation(landdModelYr) # p < 0.05 model is zero inflated

Anova(landdModelYr)
summary(landdModelYr)


#Inference and marginal means on the response (proportion) scale
landdEmmYr <- emmeans(landdModelYr, ~ Treatment, type = "response")
summary(landdEmmYr)             #marginal means and CIs
pairs(landdEmmYr)               #treatment contrasts within each year as proportions
plot(landdEmmYr)
