library(geodata)
library(terra)
library(sf)
library(rnaturalearth)
library(magick)
library(gifski)

setwd("~/EAGLE/Intro_to_R") #set working directory to source file location

#countrym = st_as_sf(geodata::gadm(country="countrymany", level=0, path = "."))
country <- ne_countries(country="Kenya", scale = "medium", returnclass = "sf")
plot(country)

prec = geodata::worldclim_global(var = "prec", res = 10, download = T, path = ".")
temp = geodata::worldclim_global(var = "tmin", res = 10, download = T, path = ".")
srad = geodata::worldclim_global(var = "srad", res = 10, download = T, path = ".")
#precipitation and temperatures, plotted per month, masked to countrymany

country.r <- st_transform(country, st_crs(temp))
temp.m <- crop(temp, country.r, mask=TRUE)
prec.m <- crop(prec, country.r, mask=TRUE)
srad.m <- crop(srad, country.r, mask=TRUE)
plot(temp.m)
plot(prec.m)


temp_vect = terra::extract(temp.m, country, mean)
prec_vect = terra::extract(prec.m, country, mean)
srad_vect = terra::extract(srad.m, country, mean)
plot(unlist(temp_vect[,-1])) # [rows, columns]
plot(unlist(prec_vect[,-1])) # empty first means all rows, 
plot(unlist(srad_vect[,-1]))

#First Approach at animation
frames <- image_graph(width = 600, height = 600, res = 96)
for (i in 1:nlyr(temp.m)) {
  plot(prec.m[[i]], main = month.name[i], range = range(minmax(temp.m)))
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
