#This script takes the quadrat-level litter depths and joins them and summarizes them to the plot-level.

#March 10, 2026
#Nathan Wade

library(tidyverse)

#set input & output folders to import data####
##dataframe folders
input <- "C:/Users/NathanWade/Box/SIB/Cronan Wade/3_Data/01_Raw_Data/O_horizon_depth"
output <- "C:/Users/NathanWade/Box/SIB/Cronan Wade/3_Data/02_Clean_Data/O_horizon_depth"

#importing data
odepth02 <- read.csv(paste0(input, "/ODepth_2002.csv"))
odepth03 <- read.csv(paste0(input, "/ODepth_2003.csv"))
odepth04 <- read.csv(paste0(input, "/ODepth_2004.csv"))
odepth07 <- read.csv(paste0(input, "/ODepth_2007.csv"))
odepth09 <- read.csv(paste0(input, "/ODepth_2009.csv"))
odepth12 <- read.csv(paste0(input, "/ODepth_2012.csv"))
odepth15 <- read.csv(paste0(input, "/ODepth_2015.csv"))
odepth25 <- read.csv(paste0(input, "/ODepth_2025.csv"))

treatments <- read.csv(paste0(input, "/Plot_treatments.csv"))

#removing unnecessary columns
odepth02 <- odepth02[, -c(2, 3)]
odepth03 <- odepth03[, -c(2, 3, 5, 8)]
odepth04 <- odepth04[, -c(2, 3, 5, 8)]
odepth07 <- odepth07[, -c(6)]
odepth09 <- odepth09[, -c(6)]
odepth12 <- odepth12[, -c(6)]
odepth15 <- odepth15[, -c(6)]
odepth25 <- odepth25[, -c(7)]

#removing exclosures
odepth02 <- odepth02 %>% filter(!Plot %in% c(9801, 9802, 9803, 9804, 9805, 9806, 9807, 9808, 9809, 9810, 9811, 9812, 9813, 9814, 9815, 9816, 9817, 9818, 9819, 9828, 9829, 9830, 9831, 9832, 9833, 9834, 9835, 9836, 9818, 9820, 9821, 9822, 9823, 9824, 9825, 9826, 9827, 9828, 9829, 9830, 9831, 9832, 9833, 9834, 9835, 9836))

odepth03 <- odepth03 %>% filter(!Plot %in% c(9801, 9802, 9803, 9804, 9805, 9806, 9807, 9808, 9809, 9810, 9811, 9812, 9813, 9814, 9815, 9816, 9817, 9818, 9819, 9828, 9829, 9830, 9831, 9832, 9833, 9834, 9835, 9836, 9818, 9820, 9821, 9822, 9823, 9824, 9825, 9826, 9827, 9828, 9829, 9830, 9831, 9832, 9833, 9834, 9835, 9836))

odepth04 <- odepth04 %>% filter(!Plot %in% c(9801, 9802, 9803, 9804, 9805, 9806, 9807, 9808, 9809, 9810, 9811, 9812, 9813, 9814, 9815, 9816, 9817, 9818, 9819, 9828, 9829, 9830, 9831, 9832, 9833, 9834, 9835, 9836, 9818, 9820, 9821, 9822, 9823, 9824, 9825, 9826, 9827, 9828, 9829, 9830, 9831, 9832, 9833, 9834, 9835, 9836))

odepth07 <- odepth07 %>% filter(!Plot %in% c(9801, 9802, 9803, 9804, 9805, 9806, 9807, 9808, 9809, 9810, 9811, 9812, 9813, 9814, 9815, 9816, 9817, 9818, 9819, 9828, 9829, 9830, 9831, 9832, 9833, 9834, 9835, 9836, 9818, 9820, 9821, 9822, 9823, 9824, 9825, 9826, 9827, 9828, 9829, 9830, 9831, 9832, 9833, 9834, 9835, 9836))

odepth09 <- odepth09 %>% filter(!Plot %in% c(9801, 9802, 9803, 9804, 9805, 9806, 9807, 9808, 9809, 9810, 9811, 9812, 9813, 9814, 9815, 9816, 9817, 9818, 9819, 9828, 9829, 9830, 9831, 9832, 9833, 9834, 9835, 9836, 9818, 9820, 9821, 9822, 9823, 9824, 9825, 9826, 9827, 9828, 9829, 9830, 9831, 9832, 9833, 9834, 9835, 9836))

odepth12 <- odepth12 %>% filter(!Plot %in% c(9801, 9802, 9803, 9804, 9805, 9806, 9807, 9808, 9809, 9810, 9811, 9812, 9813, 9814, 9815, 9816, 9817, 9818, 9819, 9828, 9829, 9830, 9831, 9832, 9833, 9834, 9835, 9836, 9818, 9820, 9821, 9822, 9823, 9824, 9825, 9826, 9827, 9828, 9829, 9830, 9831, 9832, 9833, 9834, 9835, 9836))

odepth15 <- odepth15 %>% filter(!Plot %in% c(9801, 9802, 9803, 9804, 9805, 9806, 9807, 9808, 9809, 9810, 9811, 9812, 9813, 9814, 9815, 9816, 9817, 9818, 9819, 9828, 9829, 9830, 9831, 9832, 9833, 9834, 9835, 9836, 9818, 9820, 9821, 9822, 9823, 9824, 9825, 9826, 9827, 9828, 9829, 9830, 9831, 9832, 9833, 9834, 9835, 9836))

#pivoting 2007, 09, 12, and 15 longer for better summarizing
odepth07 <- odepth07 %>% pivot_longer(col = (N:W), names_to = "Quad", values_to = "O_Depth")
odepth09 <- odepth09 %>% pivot_longer(col = (N:W), names_to = "Quad", values_to = "O_Depth")
odepth12 <- odepth12 %>% pivot_longer(col = (N:W), names_to = "Quad", values_to = "O_Depth")
odepth15 <- odepth15 %>% pivot_longer(col = (N:W), names_to = "Quad", values_to = "O_Depth")

#remove quads with no cover recorded
odepth02 <- odepth02[!is.na(odepth02$O_Depth), ]
odepth03 <- odepth03[!is.na(odepth03$O_Depth), ]
odepth04 <- odepth04[!is.na(odepth04$O_Depth), ]
odepth07 <- odepth07[!is.na(odepth07$O_Depth), ]
odepth09 <- odepth09[!is.na(odepth09$O_Depth), ]
odepth12 <- odepth12[!is.na(odepth12$O_Depth), ]
odepth15 <- odepth15[!is.na(odepth15$O_Depth), ]
odepth25 <- odepth25[!is.na(odepth25$Depth), ]


#assigning treatment to every dataframe####
odepth02 <- 
odepth03
odepth04
odepth07
odepth09
odepth12
odepth15
odepth25