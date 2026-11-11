library(rnaturalearth)
library(rnaturalearthdata)

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

country_guessr <- function(duration = 10) {
  points = 0
  for(i in c(1:duration)){
    random = world[ceiling(runif(1)*nrow(world)), ]
    #cat(random$sovereignt)
    cat("\nYour guess: ")
    
    p <- ggplot(data = random) +
      geom_sf(fill = "darkgreen") +
      annotation_scale(location = "bl", width_hint = 0.5) +
      annotation_north_arrow(location = "bl", which_north="true",
                             pad_x = unit(0.2, "in"), pad_y = unit(0.5, "in"),
                             style = north_arrow_fancy_orienteering)
    print(p)
    
    guess = readline()
    if(guess == random$sovereignt) {
      points = points + 1
    } else {
      cat("\nIt is:", random$sovereignt)
    }
    cat("\nYour points: ", points)
  }
}
