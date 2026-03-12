#This script takes the cleaned output for the dependent variables (fuel loading, vegetation cover from quadrats, shrub cover from plots, and the O-horizon depths) and runs summary statistics on them to make sure nothing is out of place.

#March 12, 2026
#Nathan Wade

library(tidyverse)

#importing data
input <- "C:/Users/NathanWade/Box/SIB/Cronan Wade/3_Data/02_Clean_Data/"

#importing data
fuelsQuad <- read.csv(paste0(input, "Fuels/Fuels_direction.csv"))
odepthQuad <- read.csv(paste0(input, "O_horizon_depth/ODepth_quad.csv"))
vegQuad <- read.csv(paste0(input, "Vegetation/Vegetation_quad.csv"))
shrubPlot <- read.csv(paste0(input, "Shrubs/Shrub_plot.csv"))

#defining column types
fuelsQuad <- fuelsQuad %>% mutate(Year = as.factor(Year), 
                                  Plot = as.factor(Plot),
                                  Stand = as.factor(Stand),
                                  Direction = as.factor(Direction),
                                  Treatment = as.factor(Treatment))

odepthQuad <- odepthQuad %>% mutate(Year = as.factor(Year), 
                                    Plot = as.factor(Plot),
                                    Stand = as.factor(Stand),
                                    Quad = as.factor(Quad),
                                    Treatment = as.factor(Treatment))

vegQuad <- vegQuad %>% mutate(Yr = as.factor(Yr), 
                              Plot = as.factor(Plot),
                              Stand = as.factor(Stand),
                              Quad = as.factor(Quad),
                              Treatment = as.factor(Treatment))

shrubPlot <- shrubPlot %>% mutate(Year = as.factor(Year), 
                                  Plot = as.factor(Plot),
                                  Stand = as.factor(Stand),
                                  Treatment = as.factor(Treatment))


#running summary statistics####
##fuels data
###one-hour fuels
fuelsQuad %>% group_by(Year) %>%
  summarise(meanHrone = mean(hrone, na.rm = TRUE),
            medianHrone = median(hrone, na.rm = TRUE),
            minHrone = min(hrone, na.rm = TRUE),
            maxHrone = max(hrone, na.rm = TRUE),
            rangeHrone = max(hrone, na.rm = TRUE) - min(hrone, na.rm = TRUE),
            sdHrone = sd(hrone, na.rm = TRUE),
            n = n())

###ten-hour fuels
fuelsQuad %>% group_by(Year) %>%
  summarise(meanHrten = mean(hrten, na.rm = TRUE),
            medianHrten = median(hrten, na.rm = TRUE),
            minHrten = min(hrten, na.rm = TRUE),
            maxHrten = max(hrten, na.rm = TRUE),
            rangeHrten = max(hrten, na.rm = TRUE) - min(hrten, na.rm = TRUE),
            sdHrten = sd(hrten, na.rm = TRUE),
            n = n())

###hundred-hour fuels
fuelsQuad %>% group_by(Year) %>%
  summarise(meanHrhun = mean(hrhun, na.rm = TRUE),
            medianHrhun = median(hrhun, na.rm = TRUE),
            minHrhun = min(hrhun, na.rm = TRUE),
            maxHrhun = max(hrhun, na.rm = TRUE),
            rangeHrhun = max(hrhun, na.rm = TRUE) - min(hrhun, na.rm = TRUE),
            sdHrhun = sd(hrhun, na.rm = TRUE),
            n = n())

###thousand-hour fuels
fuelsQuad %>% group_by(Year) %>%
  summarise(meanHrthou = mean(hrthou, na.rm = TRUE),
            medianHrthou = median(hrthou, na.rm = TRUE),
            minHrthou = min(hrthou, na.rm = TRUE),
            maxHrthou = max(hrthou, na.rm = TRUE),
            rangeHrthou = max(hrthou, na.rm = TRUE) - min(hrthou, na.rm = TRUE),
            sdHrthou = sd(hrthou, na.rm = TRUE),
            n = n())

##o-horizon depth (Brown's transect)
fuelsQuad %>% group_by(Year) %>%
  summarise(meanLandd = mean(landd, na.rm = TRUE),
            medianLandd = median(landd, na.rm = TRUE),
            minLandd = min(landd, na.rm = TRUE),
            maxLandd = max(landd, na.rm = TRUE),
            rangeLandd = max(landd, na.rm = TRUE) - min(landd, na.rm = TRUE),
            sdLandd = sd(landd, na.rm = TRUE),
            n = n())


##histograms
###one-hour fuels


#o-horizon depth
odepthQuad %>% group_by(Year) %>%
  summarise(meanO_Depth = mean(O_Depth, na.rm = TRUE),
            medianO_Depth = median(O_Depth, na.rm = TRUE),
            minO_Depth = min(O_Depth, na.rm = TRUE),
            maxO_Depth = max(O_Depth, na.rm = TRUE),
            rangeO_Depth = max(O_Depth, na.rm = TRUE) - min(O_Depth, na.rm = TRUE),
            sdO_Depth = sd(O_Depth, na.rm = TRUE),
            n = n())

#quadrat vegetation cover
vegQuad %>% group_by(Year) %>%
  summarise(meanCover = mean(Cover, na.rm = TRUE),
            medianCover = median(Cover, na.rm = TRUE),
            minCover = min(Cover, na.rm = TRUE),
            maxCover = max(Cover, na.rm = TRUE),
            rangeCover = max(Cover, na.rm = TRUE) - min(Cover, na.rm = TRUE),
            sdCover = sd(Cover, na.rm = TRUE),
            n = n())

#plot shrub cover
shrubPlot %>% group_by(Year) %>%
  summarise(meanCover = mean(Cover, na.rm = TRUE),
            medianCover = median(Cover, na.rm = TRUE),
            minCover = min(Cover, na.rm = TRUE),
            maxCover = max(Cover, na.rm = TRUE),
            rangeCover = max(Cover, na.rm = TRUE) - min(Cover, na.rm = TRUE),
            sdCover = sd(Cover, na.rm = TRUE),
            n = n())

