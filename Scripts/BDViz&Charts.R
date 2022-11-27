#Run first, mostly using ggplot2 here
library(tidyverse)
options(scipen = 99)

#For this script, i'll be taking many of the variables contained in our box data
#and plotting them to work on visualizations. 


#One variable, discrete variables
ggplot(CrystalBox, aes(y = Type1, fill = OT)) + geom_bar() + 
  xlab("Count") + ylab("1st Type") + 
  ggtitle("# of Pokemon by Types & Original Trainers", 
          subtitle = "2nd Type") +
  theme_light() +
  facet_wrap(Type2 ~ .)


ggplot(CrystalBox, aes(y = HeldItem, fill = OT)) +
         geom_bar() + 
  ggtitle("Held Items by Sex & Original Trainer") +
  xlab("Count") +
  ylab("Item") +
  facet_wrap(Gender ~ .)

ggplot(CrystalBox, aes(x = Gender, fill = OT)) + geom_bar() +
  ggtitle("# of Pokemon by Sex and Original Trainer") + 
  xlab("Sex") +
  ylab("Count")

#One variable, continuous

ggplot(CrystalBox, aes(x = Level, fill = Gender)) + 
  geom_histogram(bins = 5, position = "stack") + 
  ggtitle("# of Pokemon by Level, Original Trainer & Sex", 
          subtitle = "From Levels 0 to 100") + 
  ylab("Count") +
  facet_grid(cols = vars(OT))

#Two variable

#Ver. 1
ggplot(CrystalBox, aes(x = OT, y = Level, color = EXP)) + geom_count() +
  ggtitle("Level of Pokemon by Original Trainer", 
          subtitle = "w/ Experience Points") +
  xlab("Original Trainer") +
  theme_minimal() 

#Ver. 2 — Using Facet Wrap
gender.labs <- c("Genderless", "Male", "Female")
names(gender.labs) <- c("Genderless", "M", "F")
ggplot(CrystalBox, aes(x = Level, y = OT, color = EXP)) + geom_count() +
  ggtitle("Level of Pokemon by Original Trainer", 
          subtitle = "w/ Experience Points") +
  xlab("Level") +
  ylab("Original Trainer") +
  theme_linedraw() +
  facet_wrap(Gender ~ ., labeller = labeller(Gender = gender.labs))

ggplot(CrystalBox, aes(x = Level, y = ..density.., fill = OT)) + 
  geom_histogram(bins = 6) + 
  geom_density(kernel = "gaussian") +
  ggtitle("Boxed Pokemon Levels & Density by Original Trainer") +
  scale_fill_discrete(name = "Trainer") +
  xlab("Level") +
  ylab("Density") +
  theme_light()

ggplot(CrystalBox, aes(x = StatTotals, y = ..density.., fill = OT)) + 
  geom_histogram(bins = 6) + 
  geom_density(kernel = "gaussian") +
  ggtitle("Boxed Pokemon Total Stats & Density by Original Trainer") +
  scale_fill_discrete(name = "Trainer") +
  xlab("Total Stats") +
  ylab("Density") +
  theme_light()


#Sample violin plot, with quantiles 

ggplot(CrystalBox, aes(x = Level, y = StatTotals)) + 
  geom_violin(draw_quantiles = c(0.25, 0.50, 0.75), trim = FALSE) +
  xlim(0,100) +
  ylab("StatTotals") +
  ggtitle("Stat Totals by Level") +
  coord_flip()

#Line graphs showing correlation between Hit points and offensive stats by level

ggplot(CrystalBox, aes(x = HP, y = ATK, size = Level)) + 
  geom_point() +
  geom_line(linewidth = 1, color = "sky blue") + 
  xlab("Hit Points") +
  ylab("Attack") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 20)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 15)) +
  ggtitle("Hit Points by Attack stat and Level") +
  theme(axis.text.x = element_text(face = "bold"),
        axis.text.y = element_text(face = "bold"),
        axis.title.x = element_text(face = "bold", 
          margin = margin(t = 8, r = 10, b = 0, l = 0)),
        axis.title.y = element_text(face = "bold", 
            margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.line = element_line(colour = "black", 
        linewidth = 1, linetype = "solid")) 
  

ggplot(CrystalBox, aes(x = HP, y = SPA, size = Level)) + 
  geom_point() + 
  geom_line(linewidth = .75, color = "red") +
  xlab("Hit Points") +
  ylab("Special Attack") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 20)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 15)) +
  ggtitle("Hit Points by Special Attack stat and Level") +
  theme(axis.text.x = element_text(face = "bold"),
        axis.text.y = element_text(face = "bold"),
        axis.title.x = 
          element_text
            (face = "bold", margin = margin(t = 8, r = 10, b = 0, l = 0)),
        axis.title.y = 
          element_text
            (face = "bold", margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.line = element_line(colour = "black", 
                                 linewidth = 1, linetype = "solid")) 

#Different graphs showing correlation between offensive stats and speed


ggplot(CrystalBox, aes(x = Type1, y = Type2, color = ATK, size = SPE)) + 
  geom_jitter() +
 geom_hex(bins = 75) +
scale_fill_continuous(type = "viridis") +
  geom_point(color = "red")


ggplot(CrystalBox, aes(x = ATK, y = SPE) ) +
  stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
  scale_fill_distiller(palette=4, direction=-1) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme(
    legend.position='none'
  )

 ggplot(CrystalBox, aes(x = SPE, y = SPA)) + geom_jitter()

 
 #Heatmap 
 
 #--Subset test dataset
 
 #--CrystalBoxHM_test %>% 
 #--remove_rownames %>% 
 #--column_to_rownames(var="Type1") - Good to remember

 CrystalBoxHM_test2 <-  subset(CrystalBox[,c(3:4,9:10,12:17,19,21:31)])
 
 #--Edit data set for use with heat map, as it can only use matrices with 
 #--numeric variables
 
 #--
 #--Replace NAs with blank spaces for sum functions below
 CrystalBoxHM_test2[,2] <- 
   replace(CrystalBoxHM_test2$Type2,is.na(CrystalBoxHM_test2$Type2),"")
 
 #--Make Type columns
 CrystalBoxHM_test3 <- unique( CrystalBoxHM_test2[ , 1 ] )
 CrystalBoxHM_test3 <- as.data.frame(CrystalBoxHM_test3)
 
 CrystalBoxHM_test3$Type2 <- NA
 colnames(CrystalBoxHM_test3) <- c("Type1", "Type2")
 CrystalBoxHM_test3$Type2 = CrystalBoxHM_test3$Type1 # Run once

 CrystalBoxHM_test3 <- CrystalBoxHM_test3 %>% add_row(Type1 = "Flying")
 CrystalBoxHM_test3$Type2 = CrystalBoxHM_test3$Type1 #Run again
 
 #--Make counts of Type 1 & 2 roles
 CrystalBoxHM_test3$Type1_Count <-
   sapply(CrystalBoxHM_test3[,1], function(string) 
     sum(string == CrystalBoxHM_test2[,1]))
 
 
 CrystalBoxHM_test3$Type2_Count <- 
   sapply(CrystalBoxHM_test3[,2], function(string) 
     sum(string == CrystalBoxHM_test2[,2]))
 
 #--Edit row names for heatmap

 row.names(CrystalBoxHM_test3) <- c("Psychic",  "Bug", "Normal", "Fire", 
                                    "Ground", "Water",  "Electric", 
                                    "Poison", "Dragon", "Ice", "Dark", 
                                    "Fighting", "Rock", "Steel",  "Grass", 
                                    "Ghost", "Flying")

 #--Make other columns for the matrix that the heatmap will be based on 
 
 
#Make Type counts: 
 
 CrystalBoxHM_test2$Type1_Count <-
   sapply(CrystalBoxHM_test2[,1], function(string) 
     sum(string == CrystalBoxHM_test2[,1]))
 
 
 CrystalBoxHM_test2$Type2_Count <- 
   sapply(CrystalBoxHM_test2[,2], function(string) 
     sum(string == CrystalBoxHM_test2[,2]))

#Splitting data, merging new datasets, summarising and subsetting:  
 
 test3 <-
   CrystalBoxHM_test2 %>% group_by(Type1) %>% summarize(mean_EXP = mean(EXP))
 test3 <-
   test3 %>% remove_rownames %>% 
   column_to_rownames(var = "Type1") %>%  as.data.frame()
 
 test4 <- merge(CrystalBoxHM_test3, test3, by = 0, all = TRUE)
 test4 <-
   test4 %>% remove_rownames %>% 
   column_to_rownames(var = "Row.names") %>%  as.data.frame()
 
 test5 <-
   test4 %>% group_by(Type1) %>% summarize(mean_Lvl = mean(Level))
 test5 <-
   test5 %>% remove_rownames %>% 
   column_to_rownames(var = "Type1") %>%  as.data.frame()
 
 test4 <- merge(test4, test5, by = 0, all = TRUE)
 
 test6 <- subset(CrystalBoxHM_test2, select = c(Type1:Type2, HP:SPE))
 
 test6[, 9:14] <- colMeans(x = test6[, 3:8])
 
 test6[, 3:8] <- NULL

#Rename rows to proper averages: 
 test6 <- test6 %>%
   rename(
     HP_Avg = V9,
     ATK_Avg = V10,
     DEF_Avg = V11,
     SPA_Avg = V12,
     SPD_Avg = V13,
     SPE_Avg = V14
   )
 
 test6 <-
   test6 %>% group_by(Type1) %>% summarise_all(mean, na.rm = TRUE)
 
 test6[, 2] <- NULL
 
 test6 <-
   test6 %>% remove_rownames %>% column_to_rownames(var = "Type1") %>%  as.data.frame()

#Merge everything and fix row names for heatmap usage: 
 
 test4 <- merge(test4, test6,  by = 0, all = TRUE)
 test4 <-
   test4 %>% remove_rownames %>% column_to_rownames(var = "Row.names") %>%
   as.data.frame() #Run after each merge to move row names for heatmap to work

#--The heatmap 
 
par(cex.main = .85)
 heatmap(as.matrix(x = test4), scale="col", 
         margins = c(6, 6),
         Colv = NA, Rowv = NA,
         na.rm = TRUE,
         main = "Average Stats/Counts by Type",
         xlab= "Aggregations", 
         ylab= "Type",
         cexRow=.8, cexCol=.8, col = cm.colors(256))
 
 legend(x="bottomright", legend=c("min", "med", "max"),fill=cm.colors(3))
 
ggplot(CrystalBox, aes(y = OT, fill = Type2)) + 
   geom_bar() + 
   xlab("Count") + 
   ylab("1st Type") + 
   ggtitle("# of Pokémon by Types & Original Trainers", 
           subtitle = "2nd Type") +
   theme_light() +
   facet_wrap(Type1 ~ .)

 