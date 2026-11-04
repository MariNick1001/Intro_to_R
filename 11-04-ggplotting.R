#11-04 more ggplot

#Steigerwald plot L8.savi x L8.ndvi for landcover types
# bare
# broadleaf_woodland
# conferious_woodland
# cropland
# grassland
# urban
# water
# wetland
library(readr)
library(ggplot2)

df <- read_csv("https://raw.githubusercontent.com/wegmann/R_data/master/Steigerwald_sample_points_all_data_subset_withNames.csv")
#create subsets

lcovs = unique(df$LCname)

# turn categorical variable into factor
df$LCname <- as.factor(df$LCname)

#change display of each vector/factor name -> important!: keep order as is
levels(df$LCname) <- c("Broadlead\nWoodland", "Coniferious\nWoodland", "Cropland",
                       "Grassland", "Shrubland", "Wetland", "Water", "Urban")
# REORDERING:
#df$LCname <- factor(df$LCname, levels = c("Broadlead Woodland", "Coniferious Woodland", "Cropland",
                                          #"Grassland", "Shrubland", "Wetland", "Water", "Urban")) #new name order (-> REQUIRES a new color order too)

ggplot(df, aes(x = L8.ndvi, y = L8.savi, color = LCname)) +
  geom_point(shape = 2) +
  labs(x = "NDVI", y = "SAVI") +
  scale_color_manual(values = c("#244C00", "#8CBE5F", "#AAAA39", "#92C060", "#D4826A", "#226666", "#162856", "darkgrey")) + #has to come before the facet wrap for some reason
  facet_wrap(~LCname, nrow = 1) +  # very important!, plots grouped plots by unique[~variable]
  theme(legend.position = "None", axis.text.x = element_text(angle = 45, hjust = 1))

