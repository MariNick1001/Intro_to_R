library(readr)
library(car)

setwd("~/EAGLE/Intro_to_R")

df <- read_csv("https://raw.githubusercontent.com/wegmann/R_data/master/Steigerwald_sample_points_all_data_subset_withNames.csv")
df <- read_csv("Steigerwald_sample_points_all_data_subset_withNames.csv")

plot(df$TimeScan.NDVIavg, df$TimeScan.NDBIavg, xlab="NDVI", ylab="NDBI")

model <- lm(TimeScan.NDVIavg ~ TimeScan.NDBIavg, data=df)
summary(model)
plot(model)
#-----------------------------------------

x <- seq(11, 110, by = 1)
x[5] #this
x[10:20] # these
x[-(2:5)] #everything except these
x > 20
(x <= 20) | (x >= 30)
x[x <= 10 | x > 30]


x2 <- recode(x, "10:20=1; 30:70=4") #not indexing but value between 10:20
x2[0:20]
sum(x)-sum(x2)
#---------------------------------
# MATRIX (just a vector squeezed into a rectangular layout)

m1 = matrix(x, nrow = ceiling(sqrt(length(x))), byrow = TRUE)
m1[5,6]
length(x)
m1

num_1 = rnorm(8000, mean = 0, sd=1)
mat_1 = matrix(num_1, nrow = ceiling(sqrt(length(num_1))))
mat_1
heatmap(mat_1, scale = "column", col = hcl.colors(50, "lajolla", rev = TRUE), Rowv = NA, Colv = NA)


df_1 = data.frame(mat_1)
names(df_1) = c("name_1", "name_2", "name_3", "name4")
df_1


#testdf
test <- data.frame(A=c(1,2,3), B=c("aB1", "aB2", "aB3"))
  #querying just "A" works with
test[,1] # first column
test[,"A"] # column "A"
test$A # column "A" also i guess

#---------------------------------
# 