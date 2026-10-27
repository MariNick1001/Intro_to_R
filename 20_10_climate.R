library(geodata)
library(terra)
library(sf)
library(rnaturalearth)
library(magick)
library(gifski)

setwd("~/EAGLE/Programming_in_EO") #set working directory to source file location

#germ = st_as_sf(geodata::gadm(country="Germany", level=0, path = "."))
ger <- ne_countries(country="Kenya", scale = "medium", returnclass = "sf")
plot(ger)

prec = geodata::worldclim_global(var = "prec", res = 10, download = T, path = ".")
temp = geodata::worldclim_global(var = "tmin", res = 10, download = T, path = ".")
#precipitation and temperatures, plotted per month, masked to germany

ger.r <- st_transform(ger, st_crs(temp))
temp.m <- crop(temp, ger.r, mask=TRUE)
prec.m <- crop(prec, ger.r, mask=TRUE)
plot(temp.m)
plot(prec.m)


temp_vect = terra::extract(temp.m, ger, mean)
prec_vect = terra::extract(prec.m, ger, mean)
plot(unlist(temp_vect[,-1])) # [rows, columns]
plot(unlist(prec_vect[,-1])) # empty first means all rows, 


#First Approach at animation
frames <- image_graph(width = 600, height = 600, res = 96)
for (i in 1:nlyr(temp.m)) {
  plot(temp.m[[i]], main = month.name[i], range = range(minmax(temp.m)))
}
dev.off()

anim <- image_animate(frames, fps = 2)
anim                                  # preview in viewer

#Faster Approach at animation
save_gif({
  for (i in 1:nlyr(temp.m)) {
    plot(temp.m[[i]], main = month.name[i], range = range(minmax(temp.m)))
  }
}, "temp.gif", delay = 0.5, width = 600, height = 600)
