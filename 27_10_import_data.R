library(readr)
library(readxl)
library(stringr)
library(geodata)

setwd("~/EAGLE/Intro_to_R")

xls = read_excel("Praktika_Afrika.xlsx")
write_csv(xls, "Internships.csv")

data = read_csv("Internships.csv")

data$`Land / Region`
sum(data$`Land / Region` == "Südafrika")


countries <- str_trim(unlist(str_split(data$`Land / Region`, "[,+()/]")))
countries <- countries[countries != "" & !is.na(countries)]

country_counts <- as.data.frame(table(countries), stringsAsFactors = FALSE)
names(country_counts) <- c("word", "count")
country_counts <- country_counts[order(-country_counts$count), ]

country_counts
