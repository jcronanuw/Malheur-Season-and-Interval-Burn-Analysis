#This code takes the quadrat cover data from the entire season of burn study and groups it to the 2025 functional groups (native groups: perennial forbs, annual forbs, grasses, and sedges; and then invasives and shrubs by species). It then summarizes the total cover data to the plot level, the stand level, and the treatment level for analyses and graphing.

#Jim Cronan and Nathan Wade
#February 27, 2026


library(tidyverse)

#set input & output folders to import data####
##dataframe folders
input <- "C:/Users/NathanWade/Box/SIB/Cronan Wade/3_Data/01_Raw_Data/Invasives"

#importing data
cover02 <- read.csv(paste0(input, "/Cover_2002.csv"))
cover03 <- read.csv(paste0(input, "/Cover_2003.csv"))
cover04 <- read.csv(paste0(input, "/Cover_2004.csv"))
cover07 <- read.csv(paste0(input, "/Cover_2007.csv"))
cover09 <- read.csv(paste0(input, "/Cover_2009.csv"))
cover12 <- read.csv(paste0(input, "/Cover_2012.csv"))
cover15 <- read.csv(paste0(input, "/Cover_2015.csv"))
cover1215 <- read.csv(paste0(input, "/SIB_UnderstoryVegetation.csv"))
cover25 <- read.csv(paste0(input, "/Cover_2025.csv"))

codes <- read.csv(paste0(input, "/Sp list.csv"))

#removing unnecessary columns
cover02 <- cover02[, -c(2:4, 7, 8)]
cover03 <- cover03[, -c(2:4, 7)]
cover04 <- cover04[, -c(2:4, 7)]
cover07 <- cover07[, -c(1)]
cover12 <- cover12[, -c(1, 6:20)]
cover15 <- cover15[, -c(1, 6:13)]
cover1215 <- cover1215[, -c(4:6, 9:10, 12:16)]
cover25 <- cover25[, -c(32, 33)]

#filling na's with 0's
cover02[is.na(cover02)] <- 0
cover03[is.na(cover03)] <- 0
cover04[is.na(cover04)] <- 0
cover07[is.na(cover07)] <- 0
cover09[is.na(cover09)] <- 0
cover12[is.na(cover12)] <- 0
cover15[is.na(cover15)] <- 0
cover1215[is.na(cover1215)] <- 0
cover25[is.na(cover25)] <- 0

#pre 2025 cover prep####
#removing exclosures
cover02 <- cover02 %>% filter(!Plot %in% c(9801, 9802, 9803, 9804, 9805, 9806, 9807, 9808, 9809, 9810, 9811, 9812, 9813, 9814, 9815, 9816, 9817, 9818, 9819, 9828, 9829, 9830, 9831, 9832, 9833, 9834, 9835, 9836, 9818, 9820, 9821, 9822, 9823, 9824, 9825, 9826, 9827, 9828, 9829, 9830, 9831, 9832, 9833, 9834, 9835, 9836))

cover03 <- cover03 %>% filter(!Plot %in% c(9801, 9802, 9803, 9804, 9805, 9806, 9807, 9808, 9809, 9810, 9811, 9812, 9813, 9814, 9815, 9816, 9817, 9818, 9819, 9828, 9829, 9830, 9831, 9832, 9833, 9834, 9835, 9836, 9818, 9820, 9821, 9822, 9823, 9824, 9825, 9826, 9827, 9828, 9829, 9830, 9831, 9832, 9833, 9834, 9835, 9836))

cover04 <- cover04 %>% filter(!Plot %in% c(9801, 9802, 9803, 9804, 9805, 9806, 9807, 9808, 9809, 9810, 9811, 9812, 9813, 9814, 9815, 9816, 9817, 9818, 9819, 9828, 9829, 9830, 9831, 9832, 9833, 9834, 9835, 9836, 9818, 9820, 9821, 9822, 9823, 9824, 9825, 9826, 9827, 9828, 9829, 9830, 9831, 9832, 9833, 9834, 9835, 9836))

cover07 <- cover07 %>% filter(!Plot %in% c(9801, 9802, 9803, 9804, 9805, 9806, 9807, 9808, 9809, 9810, 9811, 9812, 9813, 9814, 9815, 9816, 9817, 9818, 9819, 9828, 9829, 9830, 9831, 9832, 9833, 9834, 9835, 9836, 9818, 9820, 9821, 9822, 9823, 9824, 9825, 9826, 9827, 9828, 9829, 9830, 9831, 9832, 9833, 9834, 9835, 9836))

cover09 <- cover09 %>% filter(!Plot %in% c(9801, 9802, 9803, 9804, 9805, 9806, 9807, 9808, 9809, 9810, 9811, 9812, 9813, 9814, 9815, 9816, 9817, 9818, 9819, 9828, 9829, 9830, 9831, 9832, 9833, 9834, 9835, 9836, 9818, 9820, 9821, 9822, 9823, 9824, 9825, 9826, 9827, 9828, 9829, 9830, 9831, 9832, 9833, 9834, 9835, 9836))

cover12 <- cover12 %>% filter(!Plot %in% c(9801, 9802, 9803, 9804, 9805, 9806, 9807, 9808, 9809, 9810, 9811, 9812, 9813, 9814, 9815, 9816, 9817, 9818, 9819, 9828, 9829, 9830, 9831, 9832, 9833, 9834, 9835, 9836, 9818, 9820, 9821, 9822, 9823, 9824, 9825, 9826, 9827, 9828, 9829, 9830, 9831, 9832, 9833, 9834, 9835, 9836))

cover15 <- cover15 %>% filter(!Plot %in% c(9801, 9802, 9803, 9804, 9805, 9806, 9807, 9808, 9809, 9810, 9811, 9812, 9813, 9814, 9815, 9816, 9817, 9818, 9819, 9828, 9829, 9830, 9831, 9832, 9833, 9834, 9835, 9836, 9818, 9820, 9821, 9822, 9823, 9824, 9825, 9826, 9827, 9828, 9829, 9830, 9831, 9832, 9833, 9834, 9835, 9836))

cover1215 <- cover1215 %>% filter(!Plot %in% c(9801, 9802, 9803, 9804, 9805, 9806, 9807, 9808, 9809, 9810, 9811, 9812, 9813, 9814, 9815, 9816, 9817, 9818, 9819, 9828, 9829, 9830, 9831, 9832, 9833, 9834, 9835, 9836, 9818, 9820, 9821, 9822, 9823, 9824, 9825, 9826, 9827, 9828, 9829, 9830, 9831, 9832, 9833, 9834, 9835, 9836))

#removing Driveway 17 by stand
cover02 <- cover02 %>% filter(!Stand == "D17")
cover03 <- cover03 %>% filter(!Stand == "D17")
cover04 <- cover04 %>% filter(!Stand == "D17")

##by plot for 2007, 09, 12, and 15
cover07 <- cover07 %>% filter(!Plot %in% c(13, 14, 15,16, 17, 18, 19, 20, 21, 22, 23, 24, 90, 91, 92, 93, 95, 201))

cover09 <- cover09 %>% filter(!Plot %in% c(13, 14, 15,16, 17, 18, 19, 20, 21, 22, 23, 24, 90, 91, 92, 93, 95, 201))

cover12 <- cover12 %>% filter(!Plot %in% c(13, 14, 15,16, 17, 18, 19, 20, 21, 22, 23, 24, 90, 91, 92, 93, 95, 201))

cover15 <- cover15 %>% filter(!Plot %in% c(13, 14, 15,16, 17, 18, 19, 20, 21, 22, 23, 24, 90, 91, 92, 93, 95, 201))

#assigning stand for 07, 09, 12, and 15
cover07 <- cover07 %>% 
  mutate(Stand = case_when(
    Plot %in% c("1","2","3","4","5","6","7","8","9","10","11","12","85","86","87","88","89","200") ~ "D14",
    Plot %in% c("31","32","33","34","35","36","28","29","30","25","26","27","99","100","101","96","97","98") ~ "D26",
    Plot %in% c("43","44","45","46","47","48","37","38","39","40","41","42","102","103","104","105","106","107") ~ "D28" ,
    Plot %in% c("55","56","57","58","59","60","49","50","51","52","53","54","79","80","81","78","82","83") ~ "KF",
    Plot %in% c("67","68","69","70","71","61","62","63","64","65","66","74","75","76","72","73","77") ~ "Trout"))

cover09 <- cover09 %>% 
  mutate(Stand = case_when(
    Plot %in% c("1","2","3","4","5","6","7","8","9","10","11","12","85","86","87","88","89","200") ~ "D14",
    Plot %in% c("31","32","33","34","35","36","28","29","30","25","26","27","99","100","101","96","97","98") ~ "D26",
    Plot %in% c("43","44","45","46","47","48","37","38","39","40","41","42","102","103","104","105","106","107") ~ "D28" ,
    Plot %in% c("55","56","57","58","59","60","49","50","51","52","53","54","79","80","81","78","82","83") ~ "KF",
    Plot %in% c("67","68","69","70","71","61","62","63","64","65","66","74","75","76","72","73","77") ~ "Trout"))

cover12 <- cover12 %>% 
  mutate(Stand = case_when(
    Plot %in% c("1","2","3","4","5","6","7","8","9","10","11","12","85","86","87","88","89","200") ~ "D14",
    Plot %in% c("31","32","33","34","35","36","28","29","30","25","26","27","99","100","101","96","97","98") ~ "D26",
    Plot %in% c("43","44","45","46","47","48","37","38","39","40","41","42","102","103","104","105","106","107") ~ "D28" ,
    Plot %in% c("55","56","57","58","59","60","49","50","51","52","53","54","79","80","81","78","82","83") ~ "KF",
    Plot %in% c("67","68","69","70","71","61","62","63","64","65","66","74","75","76","72","73","77") ~ "Trout"))

cover15 <- cover15 %>% 
  mutate(Stand = case_when(
    Plot %in% c("1","2","3","4","5","6","7","8","9","10","11","12","85","86","87","88","89","200") ~ "D14",
    Plot %in% c("31","32","33","34","35","36","28","29","30","25","26","27","99","100","101","96","97","98") ~ "D26",
    Plot %in% c("43","44","45","46","47","48","37","38","39","40","41","42","102","103","104","105","106","107") ~ "D28" ,
    Plot %in% c("55","56","57","58","59","60","49","50","51","52","53","54","79","80","81","78","82","83") ~ "KF",
    Plot %in% c("67","68","69","70","71","61","62","63","64","65","66","74","75","76","72","73","77") ~ "Trout"))


#assigning treatment
cover02 <- cover02 %>% 
  mutate(Treatment = case_when(
    (Plot %in% c(4, 5, 6, 1, 2, 3, 34, 35, 36, 31, 32, 33, 43, 44, 45, 46, 47, 48, 68, 70, 67, 69, 71, 55, 56, 57, 58, 59, 60)) ~ "Control",
    (Plot %in% c(7, 8, 9, 28, 29, 30, 37, 38, 39, 61, 62, 63, 49, 50, 51)) ~ "Fall 15",
    (Plot %in% c(10, 11, 12, 25, 26, 27, 40, 41, 42, 64, 65, 66, 52, 53, 54)) ~ "Fall 5",
    (Plot %in% c(85, 86, 87, 99, 100, 101, 102, 103, 104, 74, 75, 76, 79, 80, 81)) ~ "Spring 15",
    (Plot %in% c(88, 89, 200, 96, 97, 98, 105, 106, 107, 72, 73, 77, 78, 82, 83)) ~ "Spring 5"))

cover03 <- cover03 %>% 
  mutate(Treatment = case_when(
    (Plot %in% c(4, 5, 6, 1, 2, 3, 34, 35, 36, 31, 32, 33, 43, 44, 45, 46, 47, 48, 68, 70, 67, 69, 71, 55, 56, 57, 58, 59, 60)) ~ "Control",
    (Plot %in% c(7, 8, 9, 28, 29, 30, 37, 38, 39, 61, 62, 63, 49, 50, 51)) ~ "Fall 15",
    (Plot %in% c(10, 11, 12, 25, 26, 27, 40, 41, 42, 64, 65, 66, 52, 53, 54)) ~ "Fall 5",
    (Plot %in% c(85, 86, 87, 99, 100, 101, 102, 103, 104, 74, 75, 76, 79, 80, 81)) ~ "Spring 15",
    (Plot %in% c(88, 89, 200, 96, 97, 98, 105, 106, 107, 72, 73, 77, 78, 82, 83)) ~ "Spring 5"))

cover04 <- cover04 %>% 
  mutate(Treatment = case_when(
    (Plot %in% c(4, 5, 6, 1, 2, 3, 34, 35, 36, 31, 32, 33, 43, 44, 45, 46, 47, 48, 68, 70, 67, 69, 71, 55, 56, 57, 58, 59, 60)) ~ "Control",
    (Plot %in% c(7, 8, 9, 28, 29, 30, 37, 38, 39, 61, 62, 63, 49, 50, 51)) ~ "Fall 15",
    (Plot %in% c(10, 11, 12, 25, 26, 27, 40, 41, 42, 64, 65, 66, 52, 53, 54)) ~ "Fall 5",
    (Plot %in% c(85, 86, 87, 99, 100, 101, 102, 103, 104, 74, 75, 76, 79, 80, 81)) ~ "Spring 15",
    (Plot %in% c(88, 89, 200, 96, 97, 98, 105, 106, 107, 72, 73, 77, 78, 82, 83)) ~ "Spring 5"))

cover07 <- cover07 %>% 
  mutate(Treatment = case_when(
    (Plot %in% c(4, 5, 6, 1, 2, 3, 34, 35, 36, 31, 32, 33, 43, 44, 45, 46, 47, 48, 68, 70, 67, 69, 71, 55, 56, 57, 58, 59, 60)) ~ "Control",
    (Plot %in% c(7, 8, 9, 28, 29, 30, 37, 38, 39, 61, 62, 63, 49, 50, 51)) ~ "Fall 15",
    (Plot %in% c(10, 11, 12, 25, 26, 27, 40, 41, 42, 64, 65, 66, 52, 53, 54)) ~ "Fall 5",
    (Plot %in% c(85, 86, 87, 99, 100, 101, 102, 103, 104, 74, 75, 76, 79, 80, 81)) ~ "Spring 15",
    (Plot %in% c(88, 89, 200, 96, 97, 98, 105, 106, 107, 72, 73, 77, 78, 82, 83)) ~ "Spring 5"))

cover09 <- cover09 %>% 
  mutate(Treatment = case_when(
    (Plot %in% c(4, 5, 6, 1, 2, 3, 34, 35, 36, 31, 32, 33, 43, 44, 45, 46, 47, 48, 68, 70, 67, 69, 71, 55, 56, 57, 58, 59, 60)) ~ "Control",
    (Plot %in% c(7, 8, 9, 28, 29, 30, 37, 38, 39, 61, 62, 63, 49, 50, 51)) ~ "Fall 15",
    (Plot %in% c(10, 11, 12, 25, 26, 27, 40, 41, 42, 64, 65, 66, 52, 53, 54)) ~ "Fall 5",
    (Plot %in% c(85, 86, 87, 99, 100, 101, 102, 103, 104, 74, 75, 76, 79, 80, 81)) ~ "Spring 15",
    (Plot %in% c(88, 89, 200, 96, 97, 98, 105, 106, 107, 72, 73, 77, 78, 82, 83)) ~ "Spring 5"))

cover12 <- cover12 %>% 
  mutate(Treatment = case_when(
    (Plot %in% c(4, 5, 6, 1, 2, 3, 34, 35, 36, 31, 32, 33, 43, 44, 45, 46, 47, 48, 68, 70, 67, 69, 71, 55, 56, 57, 58, 59, 60)) ~ "Control",
    (Plot %in% c(7, 8, 9, 28, 29, 30, 37, 38, 39, 61, 62, 63, 49, 50, 51)) ~ "Fall 15",
    (Plot %in% c(10, 11, 12, 25, 26, 27, 40, 41, 42, 64, 65, 66, 52, 53, 54)) ~ "Fall 5",
    (Plot %in% c(85, 86, 87, 99, 100, 101, 102, 103, 104, 74, 75, 76, 79, 80, 81)) ~ "Spring 15",
    (Plot %in% c(88, 89, 200, 96, 97, 98, 105, 106, 107, 72, 73, 77, 78, 82, 83)) ~ "Spring 5"))

cover15 <- cover15 %>% 
  mutate(Treatment = case_when(
    (Plot %in% c(4, 5, 6, 1, 2, 3, 34, 35, 36, 31, 32, 33, 43, 44, 45, 46, 47, 48, 68, 70, 67, 69, 71, 55, 56, 57, 58, 59, 60)) ~ "Control",
    (Plot %in% c(7, 8, 9, 28, 29, 30, 37, 38, 39, 61, 62, 63, 49, 50, 51)) ~ "Fall 15",
    (Plot %in% c(10, 11, 12, 25, 26, 27, 40, 41, 42, 64, 65, 66, 52, 53, 54)) ~ "Fall 5",
    (Plot %in% c(85, 86, 87, 99, 100, 101, 102, 103, 104, 74, 75, 76, 79, 80, 81)) ~ "Spring 15",
    (Plot %in% c(88, 89, 200, 96, 97, 98, 105, 106, 107, 72, 73, 77, 78, 82, 83)) ~ "Spring 5"))

cover1215 <- cover1215 %>% 
  mutate(Treatment = case_when(
    (Plot %in% c(4, 5, 6, 1, 2, 3, 34, 35, 36, 31, 32, 33, 43, 44, 45, 46, 47, 48, 68, 70, 67, 69, 71, 55, 56, 57, 58, 59, 60)) ~ "Control",
    (Plot %in% c(7, 8, 9, 28, 29, 30, 37, 38, 39, 61, 62, 63, 49, 50, 51)) ~ "Fall 15",
    (Plot %in% c(10, 11, 12, 25, 26, 27, 40, 41, 42, 64, 65, 66, 52, 53, 54)) ~ "Fall 5",
    (Plot %in% c(85, 86, 87, 99, 100, 101, 102, 103, 104, 74, 75, 76, 79, 80, 81)) ~ "Spring 15",
    (Plot %in% c(88, 89, 200, 96, 97, 98, 105, 106, 107, 72, 73, 77, 78, 82, 83)) ~ "Spring 5"))

#adding year
cover03 <- cover03 %>% mutate(Yr = "2003")
cover04 <- cover04 %>% mutate(Yr = "2004")
cover07 <- cover07 %>% mutate(Yr = "2007")
cover09 <- cover09 %>% mutate(Yr = "2009")
cover12 <- cover12 %>% mutate(Yr = "2012")
cover15 <- cover15 %>% mutate(Yr = "2015")

#filtering out covers with -999 and NAs in 2012, 2015, and 2012/2015 (archived data)
cover12 <- cover12 %>% filter(!Cover == -999)
cover12 <- cover12 %>% filter(!Plot == 0)
cover15 <- cover15 %>% filter(!Cover == -999)
cover1215 <- cover1215 %>% filter(!Cover == -999)


#2025 cover prep####
#changing quad to match other dataframes
cover25 <- cover25 %>% mutate(Quad = case_when(
  (Quad == "N1") ~ "NQ1",
  (Quad == "N2") ~ "NQ2",
  (Quad == "E1") ~ "EQ1",
  (Quad == "E2") ~ "EQ2",
  (Quad == "S1") ~ "SQ1",
  (Quad == "S2") ~ "SQ2",
  (Quad == "W1") ~ "WQ1",
  (Quad == "W2") ~ "WQ2"))

#changing stand to match other dataframes
cover25 <- cover25 %>% mutate(Stand = case_when(
  (Stand == "Driveway 14") ~ "D14",
  (Stand == "Driveway 26") ~ "D26",
  (Stand == "Driveway 28") ~ "D28",
  (Stand == "Kidd Flat") ~ "KF",
  (Stand == "Trout") ~ "Trout"))


####################################################
#prepping to group by functional group to merge with 2025 cover####
#pivoting longer for 02, 03, 04, 07, 09, 25
cover02long <- cover02 %>% pivot_longer(col = (ABGR:VIPU), names_to = "Code", values_to = "Cover")

cover03long <- cover03 %>% pivot_longer(col = (ABGR:VIPU), names_to = "Code", values_to = "Cover")

cover04long <- cover04 %>% pivot_longer(col = (ABGR:VIPU), names_to = "Code", values_to = "Cover")

cover07long <- cover07 %>% pivot_longer(col = (ACMI:VIPU), names_to = "Code", values_to = "Cover")

cover09long <- cover09 %>% pivot_longer(col = (ACMI:VIPU), names_to = "Code", values_to = "Cover")

cover25long <- cover25 %>% pivot_longer(col = (Aforb:SYMP), names_to = "Code", values_to = "Cover")


#defining column types
cover02long <- cover02long %>% mutate(Stand = as.factor(Stand), 
                                      Plot = as.factor(Plot),
                                      Yr = as.factor(Yr),
                                      Quad = as.factor(Quad),
                                      Treatment = as.factor(Treatment))

cover03long <- cover03long %>% mutate(Stand = as.factor(Stand), 
                                      Plot = as.factor(Plot),
                                      Yr = as.factor(Yr),
                                      Quad = as.factor(Quad),
                                      Treatment = as.factor(Treatment))

cover04long <- cover04long %>% mutate(Stand = as.factor(Stand), 
                                      Plot = as.factor(Plot),
                                      Yr = as.factor(Yr),
                                      Quad = as.factor(Quad),
                                      Treatment = as.factor(Treatment))

cover07long <- cover07long %>% mutate(Stand = as.factor(Stand), 
                                      Plot = as.factor(Plot),
                                      Yr = as.factor(Yr),
                                      Quad = as.factor(Quad),
                                      Treatment = as.factor(Treatment))

cover09long <- cover09long %>% mutate(Stand = as.factor(Stand), 
                                      Plot = as.factor(Plot),
                                      Yr = as.factor(Yr),
                                      Quad = as.factor(Quad),
                                      Treatment = as.factor(Treatment))

cover12 <- cover12 %>% mutate(Stand = as.factor(Stand), 
                              Plot = as.factor(Plot),
                              Yr = as.factor(Yr),
                              Quad = as.factor(Quad),
                              Treatment = as.factor(Treatment))

cover15 <- cover15 %>% mutate(Stand = as.factor(Stand), 
                              Plot = as.factor(Plot),
                              Yr = as.factor(Yr),
                              Quad = as.factor(Quad),
                              Treatment = as.factor(Treatment))

cover1215 <- cover1215 %>% mutate(year = as.factor(year), 
                                  Plot = as.factor(Plot),
                                  Stand = as.factor(Stand),
                                  Quad = as.factor(Quad),
                                  Treatment = as.factor(Treatment))

cover25long <- cover25long %>% mutate(Stand = as.factor(Stand), 
                                      Plot = as.factor(Plot),
                                      Yr = as.factor(Yr),
                                      Yr = as.factor(Yr),
                                      Quad = as.factor(Quad),
                                      Treatment = as.factor(Treatment))


#2012/2015 cover cleanup####
#quick check to see which species are repeated for the unique year*plot*quadrat combination
dups1 <- cover1215 %>% group_by(Stand, Treatment, year, Plot, Quad, Code) %>%
  summarise(n = n(), .groups = 'drop') %>%
  filter(n>1)

##seeing where each species was duplicated
dup_rows1 <- cover1215 %>% group_by(Stand, Treatment, year, Plot, Quad, Code) %>%
  filter(n()>1) %>%
  arrange(Stand, Treatment, year, Plot, Quad, Code)
#ELEL and POSE are repeated twice

#summing ELEL and POSE covers together
cover1215 <- cover1215 %>% group_by(Stand, Treatment, year, Plot, Quad, Code) %>%
  summarise(Cover1 = sum(Cover, na.rm = TRUE), .groups = 'drop')

#renaming year and cover columns for merging
cover1215 <- cover1215 %>% rename(Yr = year)
cover1215 <- cover1215 %>% rename(Cover = Cover1)


##############################################
#full cover####
#combining all the dataframes to get cover by year for the full study
cover <- rbind(cover02long, cover03long, cover04long, cover07long, cover09long, cover12, cover15, cover25long)

###############################
#quick check to make sure every year*plot combination has 8 quadrats####
all_quadrats <- c("NQ1", "NQ2", "EQ1", "EQ2", "SQ1", "SQ2", "WQ1", "WQ2")

#chicking which quadrats are missing
missing_quads <- cover %>% distinct(Yr, Plot, Quad) %>%
  group_by(Yr, Plot) %>%
  summarise(missing = list(setdiff(all_quadrats, Quad)), .groups = 'drop') %>%
  filter(lengths(missing)>0)
#WQ2 is missing from Plot 59 in 2009


#quick check to see which species are repeated for the unique year*plot*quadrat combination
dups <- cover %>% group_by(Stand, Treatment, Yr, Plot, Quad, Code) %>%
  summarise(n = n(), .groups = 'drop') %>%
  filter(n>1)

##seeing where each species was duplicated
dup_rows <- cover %>% group_by(Stand, Treatment, Yr, Plot, Quad, Code) %>%
  filter(n()>1) %>%
  arrange(Stand, Treatment, Yr, Plot, Quad, Code)

#exporting for species check
#write.csv(cover, paste0(input, "/checks/SP_check.csv"))


##############################
#left joining with the classifications to summarize everything to the 2025 classifications####
cover <- cover %>% left_join(codes, by = "Code")

#summarizing to the "Group" level to get to the 2025 classifications
cover <- cover %>% group_by(Yr, Stand, Treatment, Plot, Quad, Group) %>%
  summarise(Cover1 = sum(Cover, na.rm = TRUE), .groups = 'drop')

#pivoting wider and then longer to create true 0's for every neophyte and shrub
cover <- cover %>% pivot_wider(names_from = Group, values_from = Cover1, values_fill = 0)

#pivoting longer with the true 0's
cover <- cover %>% pivot_longer(col = (ABGR:Tree), names_to = "Group", values_to = "Cover1")


#joining exotic/native to codes
codesmall <- codes %>% select(Group, Exotic)

codesmall <- codesmall %>% distinct(Group, .keep_all = TRUE)

cover <- cover %>% left_join(codesmall, by = "Group")

#summarizing to the plot level by group####
coverplot <- cover %>% group_by(Yr, Stand, Treatment, Plot, Exotic, Group) %>%
  summarise(Cover = mean(Cover1, na.rm = TRUE), .groups = 'drop')


#summarizing to the stand level by group####
coverstand <- coverplot %>% group_by(Yr, Stand, Treatment, Exotic, Group) %>%
  summarise(Cover1 = mean(Cover, na.rm = TRUE), .groups = 'drop')


#summarizing to the year*treatment level by group####
coveryr <- cover %>% group_by(Yr, Treatment, Exotic, Group) %>%
  summarise(Cover = mean(Cover1, na.rm = TRUE),
            sd_cover = sd(Cover1, na.rm = TRUE), .groups = 'drop')

#defining factor order for graphing
coveryr$Group <- factor(coveryr$Group, levels = c("Pforb", "Aforb", "Grass", "Sedge", "AMAL", "ARTR", "BERE", "CELE", "CEVE", "CHUM", "LIPU", "PRUN", "PUTR", "RABBIT", "RICE", "ROSA", "SYMP", "PIPO", "JUOC", "ABGR", "Tree", "BRTE", "AGIN", "CIRS", "CIVU", "LASE", "LIDA", "MAMA", "POBU", "POPR", "RUAC", "TAOF", "TRDU", "VETH", "Unknown"))

coveryr$Treatment <- factor(coveryr$Treatment, levels = c("Control", "Fall 5", "Fall 15", "Spring 5", "Spring 15"))


#summarizing to the study level by group to determine the most common shrubs and neophytes
coverstudy <- coveryr %>% group_by(Yr, Exotic, Group) %>%
  summarise(Cover1 = mean(Cover, na.rm = TRUE), .groups = 'drop')

#exporting for species check
#write.csv(coverstudy, paste0(input, "/checks/SP_study_cover.csv"))


##############################################
#graphing native herbaceous species (FOR REFERENCE ONLY)####
#year*treatment bar graph for native herbaceous species
(ggplot(coveryr %>% filter(Group == "Pforb" | Group == "Aforb" | Group == "Grass" | Group == "Sedge"), aes(x = Group, y = Cover, fill = Treatment)) +
   geom_bar(stat = "identity", position = position_dodge()) +
   geom_errorbar(aes(ymin = Cover, ymax = Cover + sd_cover),
                 width = 0, position = position_dodge(0.9)) +
   facet_wrap(~Yr, scales = "free_y") +
   theme_bw(13) + 
   theme(legend.position = "bottom",
         legend.title = element_text(size = 12),
         legend.text = element_text(size = 10))) + 
  labs(x = NULL, y = "% Cover") + 
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 60, hjust = 1))

#line graph of each native functional group throughout the years
(ggplot(coveryr %>% filter(Group == "Pforb" | Group == "Aforb" | Group == "Grass" | Group == "Sedge"), aes(x = Yr, y = Cover, color = Treatment, group = Treatment)) +
    geom_line() +
    geom_point() +
    geom_errorbar(aes(ymin = Cover - sd_cover,
                      ymax = Cover + sd_cover),
                  width = 0.2) +
    facet_wrap(~Group) +
    labs(x = "Year",
         y = "% Cover",
         color = "Treatment") +
    theme_bw())


#graphing cheatgrass####
#year*treatment bar graph for BRTE cover
(ggplot(coveryr %>% filter(Group == "BRTE"), aes(x = Yr, y = Cover, fill = Treatment)) +
    geom_bar(stat = "identity", position = position_dodge()) +
    geom_errorbar(aes(ymin = Cover, ymax = Cover + sd_cover),
                  width = 0, position = position_dodge(0.9)) +
    #facet_wrap(~Yr, scales = "free_y") +
    theme_bw(13) + 
    theme(legend.position = "bottom",
          legend.title = element_text(size = 12),
          legend.text = element_text(size = 10))) + 
  labs(x = NULL, y = "% Cover") + 
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 60, hjust = 1))

#line graph of BRTE throughout the years
(ggplot(coveryr %>% filter(Group == "BRTE"), aes(x = Yr, y = Cover, color = Treatment, group = Treatment)) +
    geom_line() +
    geom_point() +
    geom_errorbar(aes(ymin = Cover - sd_cover,
                      ymax = Cover + sd_cover),
                  width = 0.2) +
    facet_wrap(~Treatment) +
    labs(x = "Year",
         y = "% Cover",
         color = "Treatment") +
    theme_bw())


#year*treatment bar graph for snowbrush and rabbitbrush
(ggplot(coveryr %>% filter(Group == "CEVE" | Group == "RABBIT"), aes(x = Group, y = Cover, fill = Treatment)) +
    geom_bar(stat = "identity", position = position_dodge()) +
    geom_errorbar(aes(ymin = Cover, ymax = Cover + sd_cover),
                  width = 0, position = position_dodge(0.9)) +
    facet_wrap(~Yr, scales = "free_y") +
    theme_bw(13) + 
    theme(legend.position = "bottom",
          legend.title = element_text(size = 12),
          legend.text = element_text(size = 10))) + 
  labs(x = NULL, y = "% Cover") + 
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 60, hjust = 1))

#line graph of the 4 most common shrubs throughout the years
(ggplot(coveryr %>% filter(Group == "CEVE" | Group == "RABBIT"), aes(x = Yr, y = Cover, color = Treatment, group = Treatment)) +
    geom_line() +
    geom_point() +
    geom_errorbar(aes(ymin = Cover - sd_cover,
                      ymax = Cover + sd_cover),
                  width = 0.2) +
    facet_wrap(~Group) +
    labs(x = "Year",
         y = "% Cover",
         color = "Treatment") +
    theme_bw())


########################################
#filtering for just cheatgrass, snowbrush, and rabbitbrush####
vegplot <- coverplot %>% filter(Group == "BRTE" | Group == "CEVE" | Group == "RABBIT")

vegstand <- coverstand %>% filter(Group == "BRTE" | Group == "CEVE" | Group == "RABBIT")

vegyr <- coveryr %>% filter(Group == "BRTE" | Group == "CEVE" | Group == "RABBIT")


