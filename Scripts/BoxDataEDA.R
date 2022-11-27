#The purpose of this script is for analyzing trends within the 
#Pokemon that comprise my storage boxes as I 
#journey as a Pokemon Trainer through the Gen2!Johto & Kanto regions.

#Run preliminary packages and turn off scientific notion
library(tidyverse)
options(scipen = 99)

#Optional 
setwd(dir = "Projects/Pokemon")

#Assign the .csvs of Pokemon Crystal and Pokemon Yellow box data to functions
CrystalBox <- read.csv(file = "Data/CrystalBoxData.csv", 
                       stringsAsFactors = FALSE)

YellowBox <-  read.csv(file = "Data/YellowBoxData.csv")

# You can use the code below to get rid of unneeded row names
# CrystalBox$X <- NULL

#View data as is 
View(CrystalBox)
head(CrystalBox)

#For Yellow
#View(YellowBox)
#head(YellowBox)

#After taking a look at the columns present, take a look at the classes 

str(CrystalBox)
str(YellowBox)

#The .csv that the data was sourced from does not include types for 
#the various assortments of Pokemon, so it would serve well it include them as 
#that can answer many interesting questions when analyzing the data  

CrystalBox$Type1 <- NA
CrystalBox$Type2 <- NA

YellowBox$Type1 <- NA
YellowBox$Type2 <- NA

##Select specific columns and then rewrite into a new dataset
#Some of the columns in the original dataset aren't even applicable to 
#the way some of the mechanics of generation 2 Pokemon games work; or are just 
#not useful, so for the purpose of this script and analysis they can be removed.

CrystalBox <- CrystalBox %>% select(Species, Gender, Type1, Type2, Move1, Move2, 
                                    Move3, Move4, HeldItem, EXP,
                                    Level, HP, ATK, DEF, SPA, 
                                    SPD, SPE, MetLoc, MetLevel, OT,
                                    HP_IV, ATK_IV, DEF_IV, SPD_IV, SPE_IV, 
                                    HP_EV, ATK_EV, DEF_EV, SPA_EV, 
                                    SPD_EV, SPE_EV, IsNicknamed, IsShiny)


YellowBox <- YellowBox %>% select(Species, Gender, Type1, Type2, Move1, Move2, 
                                    Move3, Move4, HeldItem, EXP,
                                    Level, HP, ATK, DEF, SPA, 
                                    SPD, SPE, MetLoc, MetLevel, OT,
                                    HP_IV, ATK_IV, DEF_IV, SPD_IV, SPE_IV,  
                                    HP_EV, ATK_EV, DEF_EV, SPA_EV, 
                                    SPD_EV, SPE_EV, IsNicknamed)

CrystalBox$X <- NULL


#View changed dataet, cut from 90 rows to 32
View(CrystalBox)
View(YellowBox)

#Two new columns are created, as some Pokemon have two types. Now you can either 
#choose to export the current .csv to Excel and edit the data manually, or you 
#use 'c("Psychic", "Bug", etc.) to enter the types manually, but for 95 rows of 
#different Pokemon, doing that will take some time. So instead, I am using 
#write.csv() to edit this offsite on Excel

##DO NOT run lines 72 & 73 after running them the first time! 

write.csv(x = CrystalBox, "Data/CrystalBoxData3.csv", row.names = FALSE)
write.csv(x = YellowBox, "Data/YellowBoxData3.csv", row.names = FALSE)

#Re-read the data from the new .csv file to double-check if needed, use file in
#new scripts
CrystalBox <- read.csv(file = "Data/CrystalBoxData3.csv", 
                       stringsAsFactors = FALSE)

YellowBox <- read.csv(file = "Data/YellowBoxData3.csv", 
                      stringsAsFactors = FALSE)







