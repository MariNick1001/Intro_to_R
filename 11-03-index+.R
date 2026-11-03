# Exercises from 11-03

library(dplyr)
library(ggplot2)

df <- read_csv("https://raw.githubusercontent.com/wegmann/R_data/master/Steigerwald_sample_points_all_data_subset_withNames.csv")

#---------------------
#indexing a dataframe
#- - - - - - - - - - -
View(df)

#Only sentinel
View(select(df, matches("S2")))
View(df[,grep("S2.")])
View(df[,paste0("S2.", seq(1,10, by=1))]) #no dplyr

#Second to last column
View(df[,ncol(df)-1])

#first ten rows
View(df[1:10,])

#LUCAS_LC and SRTM only
View(df[,c("LUCAS_LC", "SRTM")])


#---------------------------
# ggplot2 introduction
#---------------------------
# plot = data + aesthetics + geometry
ggplot(df, aes(x = "S2.1", y="S2.2")) +
  geom_boxplot()

x <- data.frame(x=1, y=1, label="ggplot2 introduction \n@ EAGLE")
ggplot(data=x, aes(x=x, y=y)) + geom_text(aes(label=label), colour = "green", size=15) 

       
# own df
x1 <- rnorm(1000, 0, 1)
x2 <- rnorm(1000, 5, 10)
x3 <- rep(c("catA", "catB", "catB", "catC", "catC", "catC"), 200)[1:1000]
x4 <- factor(rep(c("yes", "no"), 500))

df <- data.frame(a=x1, b=x2, c=x3, d=x4)

ggplot(data = df, aes(a, b, color=c)) +
  geom_point() +
  scale_color_brewer(name = waiver(), type = "seq", palette = 1, direction = -1, aesthetics = "color")

ggplot(df, aes(a)) +
  geom_histogram(fill = "white", binwidth = 0.1)

ggplot(df, aes(a)) +
  geom_density()

ggplot(df) +
  geom_histogram(aes(a, after_stat(density)),  fill = "darkblue", color = "darkgrey", binwidth =  0.3) +
  geom_density(aes(a, after_stat(density)), color="black")+
  geom_rug(aes(a), color = "darkgreen")



help.search("geom_", package = "ggplot2") #look for which geometries are there
