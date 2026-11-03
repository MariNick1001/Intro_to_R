# Exercises from 11-03

library(dplyr)

df <- read_csv("https://raw.githubusercontent.com/wegmann/R_data/master/Steigerwald_sample_points_all_data_subset_withNames.csv")

#---------------------
#indexing a dataframe
#- - - - - - - - - - -
View(df)

#Only sentinel
View(select(df, matches("S2")))
View(df[,paste0("S2.", seq(1,10, by=1))]) #no dplyr

#Second to last column
View(df[,ncol(df)-1])

#first ten rows
View(df[1:10,])

#LUCAS_LC and SRTM only
View(df[,c("LUCAS_LC", "SRTM")])




