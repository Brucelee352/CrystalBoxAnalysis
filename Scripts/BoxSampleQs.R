#Run first
library(tidyverse)

# Import data from previous script for plots
CrystalBox <- read.csv(file = "Data/CrystalBoxData3.csv")
YellowBox <- read.csv(file = "Data/YellowBoxData3.csv")
CrystalBox$X <- NULL
YellowBox$X <- NULL

####Sample Questions: 

#Lets run a summary to glean some insights at top level, my Crystal Version has
#a more in varied collection, so i'll use it for my summary stats
summary(CrystalBox)

#Columns 1-8 are very interesting, especially the gender and type columns

table(CrystalBox$Gender)

#According to the data, I have 64 male Pokemon, 18 female Pokemon and 13 
#genderless, as a programming quirk in Gen 2, stats are sexist against females   

types <- tibble(CrystalBox[c(1,3:4)])
#Apparently, I have an overwhelming amount of water types within my boxes at 21.
#Most of my Pokemon that do have a second type are Flying, likely legendaries. 

#Most of the Pokemon I have are only of one type it seems, as they account for 
#the NAs that are being reported. 

#---

#Lets take a look at EXP and Levels:
summary(CrystalBox[,9:10])

#That's about right considering the number of Pokemon in my boxes that are 
#around level 50. 

#Lets take a look at items:
table(CrystalBox$HeldItem)

#It seems as though Leftovers is the most common item among my boxed Pokemon,
#a lot of the sets I've made were purpose-built for competitive battling and 
#other nerdiness; so that's about right. 

#Now lets take a look at the Pokemon's stats...

## Which Pokemon has the highest ATK(Attack) stat? 
# Either of the lines below can be ran

CrystalBox[which.max(CrystalBox$ATK),]
subset(CrystalBox, ATK == max(ATK))

## Which Pokemon has the highest level? 

CrystalBox %>% 
  filter(Level == "100") %>% 
  pull(Species)


## Which Pokemon has the highest Stat totals among them?
rowSums(CrystalBox[,12:17])

#---------------------------------------

# Its important to consider the following for this next section.

# The way Pokemon get stronger in the Gen 1 & Gen 2 games is as they battle 
# different Pokemon, their base stats get added to the winning Pokemon's total
# individual stats. This is called StatEXP

## This is referred to as EVs in the data, meaning "Effort Values" to maintain 
## compatibility with newer games, Gen 3 and up. 

# So for example, if my Pikachu KOs a wild Raticate then all of Raticate's
# base stats are added to Pikachu's StatEXP totals, making him stronger.

# All Pokemon can max out each individual stat for a total of 393210, 65535 for 
# each stat. Needless to say, a lot of grinding is needed for getting the ideal
# stats you want and decisively winning in battle. 

#---------------------------------------

## So with that said, to try and determine the best team I can come up with
## we'll first have to see which Pokemon in Yellow acquired the most StatEXP. 

# First, lets total up the EXP gained by each Pokemon...

CrystalBox$Total_EV <- rowSums(CrystalBox[,26:31])
YellowBox$Total_EV <- rowSums(YellowBox[,23:28])

# Resort, the data reads better this way in my opinion. 
CrystalBox <- CrystalBox %>% select(1:31, 34, 32:33)
YellowBox <- YellowBox %>% select(1:28, 30, 29)

# Then run the line below and it'll give you...
subset(YellowBox, YellowBox[,23:28] == max(YellowBox[,23:28]))

# ...Pikachu! 

# What happened was that I told R to subset the YellowBox dataset to
# a particular set of columns and then report back the row with the maximum sum 
# of the 5 columns I told it to. 

# When I run this identical line for the Crystal boxes, something weird happens.
subset(CrystalBox, CrystalBox[,26:31] == max(CrystalBox[,26:31]))

# R seems to generate a selection the original dataset, yet adds in a bunch of 
# NA blank rows for reasons I haven't been able to figure out. So I found a more
# efficient way to filter the information I need...

# Using dplyr, the filter function
CrystalBox %>% filter(Total_EV == 393210)
YellowBox %>% filter(Total_EV == 393210)

# Using Base R, subsetting using [] 

  CrystalBox[which(CrystalBox$Total_EV == max(CrystalBox$Total_EV)), ]

YellowStrongestMons <- 
  YellowBox[which(YellowBox$Total_EV == max(YellowBox$Total_EV)), ]

# An interesting thing that happens is that when you assign both methods to 
# their own variables, the filter function indexes the Pokemon species in order,
# whereas the subsetting method references the original index number in the data

# So to answer the question...
# The most trained up Pokemon in Yellow is Pikachu and it's not even close. So,
# I can't really pull a team of strong Pokemon from Yellow Version. 

View(YellowStrongestMons)

# Whereas in Crystal, we'll see...

View(CrystalStrongestMons)

#---------------------------------------

# 28 different Pokemon share the distinction of being trained to their utmost
# limits, needless to say, when I want to win a battle; I'll need to send out 
# a combination of those 28 Pokemon from Crystal to crush my opponent. 

# Ash Ketchum and PKMN Trainer Red never stood a chance. 

# However, which one of these 28 is the strongest? 

#---------------------------------------

# Level, while not the best indicator of strength outside of StatEXP, 
# can tell us what Pokemon separate themselves from the pack. 

StrongestMons <- CrystalBox[which(CrystalBox$Level == max(CrystalBox$Level)), ]

# Below, I'll add up the individual Pokemon's stats

StrongestMons$StatTotals <- rowSums(StrongestMons[,12:17])

#---------------------------------------

# This gives us 2053 stats for each observation, which indicates that all three 
# Pokemon have reached the same limits, now lets try something a bit more 
# interesting...

# I found these formulas for calculating base stats, all numbers in this formula
# use Lugia's stats for calculation

# For Hit Points: Stat = floor((2 * B + I + E) * L / 100 + L + 10)
# For all of stats: floor(floor((2 * B + I + E) * L / 100 + 5) * N)
# For Gen 1 & 2 Effort Values: E = floor(min(255, ceiling(sqrt(StatEXP))) / 4)

# B = Base Stat 
# I = Individual Values, Gen 1 & 2 only go up to 15, so multiply 15 by 2
# E = Effort Values, see above for StatEXP explanation, the formula used here is 
#     different for the older system's methods
# L = Level
# N = Nature, which doesn't exist in Gens 1 & 2, so N is = 1

#---------------------------------------

E = floor(min(255, ceiling(sqrt(test[1,26]))) / 4)
HPStat = floor((2 * 106 + 30 + 63) * 100 / 100 + 100 + 10)
AttackStat = floor(floor((2 * 90 + 30 + 63) * 100 / 100 + 5) * 1)
DefenseStat =  floor(floor((2 * 130 + 30 + 63) * 100 / 100 + 5) * 1)
SpAttackStat =  floor(floor((2 * 90 + 30 + 63) * 100 / 100 + 5) * 1)
SpDefenseStat =  floor(floor((2 * 154 + 30 + 63) * 100 / 100 + 5) * 1)
SpeedStat =  floor(floor((2 * 110 + 30 + 63) * 100 / 100 + 5) * 1)

#---------------------------------------

# This could be infinitely useful if I only had access to the Base Stats of each
# Pokemon and every stat weren't outlined in the data already. I would run these 
# formulas in a situation where I didn't trust where the data I have was sourced
# from. Everything was pulled directly from the game, so the data is sound.

# I could reassign the values those formulas generated to the stat columns in 
# the dataset, but I'm not going to do that since everything lines up. This was
# more of a demonstration of a proof of concept. 

# Another nifty thing I thought of is that I can use the IV columns and look up
# base stats to calculate how strong all of my Pokemon can get at level 100. 

#---------------------------------------

# All of that to answer the question. Among all of my boxed Pokemon, which is 
# the strongest? 

# Well, three Pokemon at level 100 qualify, all their stats total to 2053. 
# Its safe to say that Lugia, Ho-Oh and Mewtwo are all tied for 1st because in 
# Gens 1 & 2; no other Pokemon have higher than 680 for their total base stats!

# Now lets see what measures up to the literal gods of Pocket Monsters! 

#---------------------------------------

#Re-add Stat Totals and append them to a new column

CrystalStrongestMons$StatTotals <- rowSums(CrystalStrongestMons[,12:17])

#Next, we're going to subset Pokemon with higher than normal StatTotals

StrongerPokes <- 
  CrystalStrongestMons[which(CrystalStrongestMons[,'StatTotals'] < 2053), ]

#Lastly, we're going to choose the six strongest Pokemon we've found and put 
#them into one team: 

StrongestTeamC <- CrystalStrongestMons %>% 
  filter(StatTotals < 2053) %>% 
  top_n(6,StatTotals)

print(test3)

#That gives us, Zapdos, Raikou, Entei, Arcanine, Tyranitar and Moltres.
#However, something isn't right...4/6 of those Pokemon are indeed legendary and
#while not as godly, are still in a different league to that of our mortal mons. 

#Let's do something about that.

CrystalStrongestMons$IsLegendary <- c("Yes", "No", "No", "Yes", "Yes", "No", 
                                      "Yes", "Yes", "No","No", "No", "No", "No",
                                      "No", "No", "No", "No", "No", "No", "No", 
                                      "Yes", "No", "Yes", "No", "No", "No", 
                                      "No", "No")

#The above line will tell us if a particular Pokemon is "legendary" or not
#as Legendary Pokemon tend to have base stat totals above 580 which are factored
#in the StatTotal column I made earlier

# Lastly, what if I wanted to select a team of the six strongest Pokemon I 
# have with a StatTotal below what my godly Pokemon possesses?

StrongestTeamC <- CrystalStrongestMons %>% 
  distinct(StatTotals, .keep_all = "True") %>% 
  filter(StatTotals < 2053, IsLegendary == "No") %>%
    top_n(6 , StatTotals)

#Lets see what we get:
print(StrongestTeamC)

# My team of Gyarados, Arcanine, Charizard, Tyranitar, Feraligatr, and Umbreon 
# seem like viable picks, sure to win me a championship or two. 





