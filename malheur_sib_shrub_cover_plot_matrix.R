#This code takes the shrub cover data that was collected on the plot level, and summarizes it to shrub cover by species at the plot level and the overall year by treatment level. Because shrub methodology changed in 2007, the code only takes 2007, 2009, 2013, 2015, and 2025 cover data.

#March 10, 2026
#Nathan Wade

library(tidyverse)

#set input & output folders to import data####
##dataframe folders
input <- "C:/Users/NathanWade/Box/SIB/Cronan Wade/3_Data/01_Raw_Data/Shrubs"
output <- "C:/Users/NathanWade/Box/SIB/Cronan Wade/3_Data/02_Clean_Data/Shrubs"

#importing data
shrub07 <- read.csv(paste0(input, "/Shrub_2007.csv"))
shrub09 <- read.csv(paste0(input, "/Shrub_2009.csv"))
shrub1215 <- read.csv(paste0(input, "/Shrub_2012_2015.csv"))
shrub25 <- read.csv(paste0(input, "/Shrub_2025.csv"))

#removing unnecessary columns
shrub07 <- shrub07[, -c(2, 20)]
shrub09 <- shrub09[, -c(20)]
shrub1215 <- shrub1215[, -c(4:6, 8, 9, 11:14)]
shrub25 <- shrub25[, -c(19, 20)]

#filling na's with 0's
shrub07[is.na(shrub07)] <- 0
shrub09[is.na(shrub09)] <- 0
shrub1215[is.na(shrub1215)] <- 0
shrub25[is.na(shrub25)] <- 0


#pre 2025 cover prep####
#removing exclosures
shrub07 <- shrub07 %>% filter(!Plot %in% c(9801, 9802, 9803, 9804, 9805, 9806, 9807, 9808, 9809, 9810, 9811, 9812, 9813, 9814, 9815, 9816, 9817, 9818, 9819, 9828, 9829, 9830, 9831, 9832, 9833, 9834, 9835, 9836, 9818, 9820, 9821, 9822, 9823, 9824, 9825, 9826, 9827, 9828, 9829, 9830, 9831, 9832, 9833, 9834, 9835, 9836))

shrub09 <- shrub09 %>% filter(!Plot %in% c(9801, 9802, 9803, 9804, 9805, 9806, 9807, 9808, 9809, 9810, 9811, 9812, 9813, 9814, 9815, 9816, 9817, 9818, 9819, 9828, 9829, 9830, 9831, 9832, 9833, 9834, 9835, 9836, 9818, 9820, 9821, 9822, 9823, 9824, 9825, 9826, 9827, 9828, 9829, 9830, 9831, 9832, 9833, 9834, 9835, 9836))

shrub1215 <- shrub1215 %>% filter(!Plot %in% c(9801, 9802, 9803, 9804, 9805, 9806, 9807, 9808, 9809, 9810, 9811, 9812, 9813, 9814, 9815, 9816, 9817, 9818, 9819, 9828, 9829, 9830, 9831, 9832, 9833, 9834, 9835, 9836, 9818, 9820, 9821, 9822, 9823, 9824, 9825, 9826, 9827, 9828, 9829, 9830, 9831, 9832, 9833, 9834, 9835, 9836))

#removing Driveway 17 by stand
shrub1215 <- shrub1215 %>% filter(!Stand == "D17")

##by plot for 2007, 09, 12, and 15
shrub07 <- shrub07 %>% filter(!Plot %in% c(13, 14, 15,16, 17, 18, 19, 20, 21, 22, 23, 24, 90, 91, 92, 93, 95, 201))

shrub09 <- shrub09 %>% filter(!Plot %in% c(13, 14, 15,16, 17, 18, 19, 20, 21, 22, 23, 24, 90, 91, 92, 93, 95, 201))

#assigning stand for 07, 09, 12, and 15
shrub07 <- shrub07 %>% 
  mutate(Stand = case_when(
    Plot %in% c("1","2","3","4","5","6","7","8","9","10","11","12","85","86","87","88","89","200") ~ "D14",
    Plot %in% c("31","32","33","34","35","36","28","29","30","25","26","27","99","100","101","96","97","98") ~ "D26",
    Plot %in% c("43","44","45","46","47","48","37","38","39","40","41","42","102","103","104","105","106","107") ~ "D28" ,
    Plot %in% c("55","56","57","58","59","60","49","50","51","52","53","54","79","80","81","78","82","83") ~ "KF",
    Plot %in% c("67","68","69","70","71","61","62","63","64","65","66","74","75","76","72","73","77") ~ "Trout"))

shrub09 <- shrub09 %>% 
  mutate(Stand = case_when(
    Plot %in% c("1","2","3","4","5","6","7","8","9","10","11","12","85","86","87","88","89","200") ~ "D14",
    Plot %in% c("31","32","33","34","35","36","28","29","30","25","26","27","99","100","101","96","97","98") ~ "D26",
    Plot %in% c("43","44","45","46","47","48","37","38","39","40","41","42","102","103","104","105","106","107") ~ "D28" ,
    Plot %in% c("55","56","57","58","59","60","49","50","51","52","53","54","79","80","81","78","82","83") ~ "KF",
    Plot %in% c("67","68","69","70","71","61","62","63","64","65","66","74","75","76","72","73","77") ~ "Trout"))

#assigning treatment
shrub07 <- shrub07 %>% 
  mutate(Treatment = case_when(
    (Plot %in% c(4, 5, 6, 1, 2, 3, 34, 35, 36, 31, 32, 33, 43, 44, 45, 46, 47, 48, 68, 70, 67, 69, 71, 55, 56, 57, 58, 59, 60)) ~ "Control",
    (Plot %in% c(7, 8, 9, 28, 29, 30, 37, 38, 39, 61, 62, 63, 49, 50, 51)) ~ "Fall 15",
    (Plot %in% c(10, 11, 12, 25, 26, 27, 40, 41, 42, 64, 65, 66, 52, 53, 54)) ~ "Fall 5",
    (Plot %in% c(85, 86, 87, 99, 100, 101, 102, 103, 104, 74, 75, 76, 79, 80, 81)) ~ "Spring 15",
    (Plot %in% c(88, 89, 200, 96, 97, 98, 105, 106, 107, 72, 73, 77, 78, 82, 83)) ~ "Spring 5"))

shrub09 <- shrub09 %>% 
  mutate(Treatment = case_when(
    (Plot %in% c(4, 5, 6, 1, 2, 3, 34, 35, 36, 31, 32, 33, 43, 44, 45, 46, 47, 48, 68, 70, 67, 69, 71, 55, 56, 57, 58, 59, 60)) ~ "Control",
    (Plot %in% c(7, 8, 9, 28, 29, 30, 37, 38, 39, 61, 62, 63, 49, 50, 51)) ~ "Fall 15",
    (Plot %in% c(10, 11, 12, 25, 26, 27, 40, 41, 42, 64, 65, 66, 52, 53, 54)) ~ "Fall 5",
    (Plot %in% c(85, 86, 87, 99, 100, 101, 102, 103, 104, 74, 75, 76, 79, 80, 81)) ~ "Spring 15",
    (Plot %in% c(88, 89, 200, 96, 97, 98, 105, 106, 107, 72, 73, 77, 78, 82, 83)) ~ "Spring 5"))

shrub1215 <- shrub1215 %>% 
  mutate(Treatment = case_when(
    (Plot %in% c(4, 5, 6, 1, 2, 3, 34, 35, 36, 31, 32, 33, 43, 44, 45, 46, 47, 48, 68, 70, 67, 69, 71, 55, 56, 57, 58, 59, 60)) ~ "Control",
    (Plot %in% c(7, 8, 9, 28, 29, 30, 37, 38, 39, 61, 62, 63, 49, 50, 51)) ~ "Fall 15",
    (Plot %in% c(10, 11, 12, 25, 26, 27, 40, 41, 42, 64, 65, 66, 52, 53, 54)) ~ "Fall 5",
    (Plot %in% c(85, 86, 87, 99, 100, 101, 102, 103, 104, 74, 75, 76, 79, 80, 81)) ~ "Spring 15",
    (Plot %in% c(88, 89, 200, 96, 97, 98, 105, 106, 107, 72, 73, 77, 78, 82, 83)) ~ "Spring 5"))

#adding year
shrub07 <- shrub07 %>% mutate(Year = "2007")
shrub09 <- shrub09 %>% mutate(Year = "2009")


#2025 shrub prep####
#changing stand to match other dataframes
shrub25 <- shrub25 %>% mutate(Stand = case_when(
  (Stand == "Driveway 14") ~ "D14",
  (Stand == "Driveway 26") ~ "D26",
  (Stand == "Driveway 28") ~ "D28",
  (Stand == "Kidd Flat") ~ "KF",
  (Stand == "Trout") ~ "Trout"))

#changing Sambucus to SAMB
shrub25 <- shrub25 %>% rename(SAMB = Sambucus)


#combining columns in previous years to match the groupings in 2025 (PRUN, RABBIT, SYMP, and SAMB)
shrub07 <- shrub07 %>% mutate(PRUN = PREM + PRVI,
                              RABBIT = CHNA + CHVI + HABL,
                              SYMP = SYAL + SYOR + SYMP) %>%
  select(-PREM, -PRVI, -CHNA, -CHVI, -HABL, -SYAL, -SYOR)


shrub09 <- shrub09 %>% mutate(PRUN = PREM + PRVI,
                              RABBIT = CHNA + CHVI + HABL,
                              SYMP = SYAL + SYOR + SYMP) %>%
  select(-PREM, -PRVI, -CHNA, -CHVI, -HABL, -SYAL, -SYOR)

#renaming LEPU to LIPU
shrub09 <- shrub09 %>% rename(LIPU = LEPU)


##pivoting shrub1215 wider to add together columns
###pivoting wider
shrub1215 <- shrub1215 %>% pivot_wider(names_from = Code, values_from = Cover, values_fill = 0)

###combining columns
shrub1215 <- shrub1215 %>% mutate(PRUN = PREM + PRVI,
                                  RABBIT = CHNA + CHVI + HABL,
                                  SYMP = SYAL + SYOR + SYMP) %>%
  select(-PREM, -PRVI, -CHNA, -CHVI, -HABL, -SYAL, -SYOR)

#renaming LEPU to LIPU and SALI2 to SAMB
shrub1215 <- shrub1215 %>% rename(LIPU = LEPU,
                                  SAMB = SALI2)



#pivoting longer for 02, 03, 04, 07, 09, 12/15, 25####
shrub07long <- shrub07 %>% pivot_longer(col = -c(Plot, Stand, Treatment, Year), names_to = "Code", values_to = "Cover")

shrub09long <- shrub09 %>% pivot_longer(col = -c(Plot, Stand, Treatment, Year), names_to = "Code", values_to = "Cover")

shrub1215long <- shrub1215 %>% pivot_longer(col = -c(Year, Plot, Stand, Treatment), names_to = "Code", values_to = "Cover")

shrub25long <- shrub25 %>% pivot_longer(col = -c(Year, Stand, Treatment, Plot), names_to = "Code", values_to = "Cover")


#merging the different years####
shrub <- rbind(shrub07long, shrub09long, shrub1215long, shrub25long)

#pivoting wider and back to longer to create true 0's
shrub <- shrub %>% pivot_wider(names_from = Code, values_from = Cover, values_fill = 0)

##pivoting longer
shrub <- shrub %>% pivot_longer(col = c(AMAL:SAMB), names_to = "Code", values_to = "Cover")


#filtering for snowbrush and rabbitbrush
shrub <- shrub %>% filter(Code == "CEVE" | Code == "RABBIT")


#summarizing to the year/treatment level
shrubYear <- shrub %>% group_by(Year, Treatment, Code) %>%
  summarise(Cover1 = mean(Cover, na.rm = TRUE), .groups = 'drop')
##renaming Cover1 to Cover
shrubYear <- shrubYear %>% rename(Cover = Cover1)

#exporting
write.csv(shrub, paste0(output, "/Shrub_plot.csv"))
write.csv(shrubYear, paste0(output, "/Shrub_year.csv"))
