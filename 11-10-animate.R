library(ggplot2)
library(gganimate)
library(RCurl)
library(tidyverse)

df <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vS0wAfZt28-tqhdxEX4EartljewLmOKpa0XCvDFgqKqibaNYuHFABsiN3NGgOIWj06tfyynEdg3q6bO/pub?output=csv")
summary(df)

df1 <- data.frame(df[, c(2:6, 10, 14)], semester = 1, courses = 11)
df2 <- data.frame(df[, c(2:5, 7, 11, 15)], semester = 2, courses = 15)
df3 <- data.frame(df[, c(2:5, 8, 12, 16)], semester = 3, courses = 2)
df4 <- data.frame(df[, c(2:5, 9, 13, 17)], semester = 4, courses = 3)

df.names <- c("eyecol", "haircol", "glasses", "sex", "eoExp", "progExp", "praesExp", "semester", "courses")

names(df1) <- df.names
names(df2) <- df.names
names(df3) <- df.names
names(df4) <- df.names

df <- rbind(df1, df2, df3, df4)

p <- ggplot(data=df, aes(y=eoExp, x=progExp, color=eyecol, size=sex))+
  geom_point(alpha = 8)

p

p + transition_time(semester) +
  ease_aes('linear')+
  shadow_wake(wake_length = 0.1, alpha = FALSE)+
  enter_fade() +
  exit_fade()

anim_save("eagle_EO_vs_prog_experience.gif")

b <- ggplot(data = df, aes(y=eoExp, x=sex, color=glasses))+
  geom_boxplot(alpha=.5, fill = "green") +
  geom_jitter(col="blue", width = 0.3, alpha = 0.5, size = 3) 
b

b + transition_time(semester) +
  ease_aes('linear') +
  labs(title = "semester: {round(frame_time, 1)}")+
  enter_fade() +
  exit_fade() +
  ease_aes('sine-in-out')


#-----------------------------
# Migratest
library(migest)

# code from https://guyabel.com/post/animated-directional-chord-diagrams/
# with some minor fixes

library(tidyverse)
d0 <- read_csv(system.file("imr", "reg_flow.csv", package = "migest"))
d0 

d1 <- read_csv(system.file("vidwp", "reg_plot.csv", package = "migest"))
d1

library(tweenr)

d2 <- d0 %>%
  mutate(corridor = paste(orig_reg, dest_reg, sep = " -> ")) %>%
  select(corridor, year0, flow) %>%
  mutate(ease = "linear") %>%
  tween_elements(time = "year0", group = "corridor", ease = "ease", nframes = 100) %>%
  tbl_df()
d2

d2 <- d2 %>%
  separate(col = .group, into = c("orig_reg", "dest_reg"), sep = " -> ") %>%
  select(orig_reg, dest_reg, flow, everything()) %>%
  mutate(flow = flow/1e06)
d2

# create a directory to store the individual plots
dir.create("./plot-gif/")

library(circlize)
for(f in unique(d2$.frame)){
  # open a PNG plotting device
  png(file = paste0("./plot-gif/globalchord", f, ".png"), height = 7, width = 7, 
      units = "in", res = 500)
  
  # intialise the circos plot
  circos.clear()
  par(mar = rep(0, 4), cex=1)
  circos.par(start.degree = 90, track.margin=c(-0.1, 0.1), 
             gap.degree = 4, points.overflow.warning = FALSE)
  
  # plot the chord diagram
  chordDiagram(x = filter(d2, .frame == f), directional = 1, order = d1$region,
               grid.col = d1$col1, annotationTrack = "grid",
               transparency = 0.25,  annotationTrackHeight = c(0.05, 0.1),
               direction.type = c("diffHeight", "arrows"), link.arr.type = "big.arrow",
               diffHeight  = -0.04, link.sort = TRUE, link.largest.ontop = TRUE)
  
  # add labels and axis
  circos.track(track.index = 1, bg.border = NA, panel.fun = function(x, y) {
    xlim = get.cell.meta.data("xlim")
    sector.index = get.cell.meta.data("sector.index")
    reg1 = d1 %>% filter(region == sector.index) %>% pull(reg1)
    reg2 = d1 %>% filter(region == sector.index) %>% pull(reg2)
    
    circos.text(x = mean(xlim), y = ifelse(is.na(reg2), 3, 4),
                labels = reg1, facing = "bending", cex = 1.1)
    circos.text(x = mean(xlim), y = 2.75, labels = reg2, facing = "bending", cex = 1.1)
    circos.axis(h = "top", labels.cex = 0.8,
                labels.niceFacing = FALSE, labels.pos.adjust = FALSE)
  })
  
  # close plotting device
  dev.off()
}

library(magick)

img <- image_read(path = "./plot-gif/globalchord0.png")
for(f in unique(d2$.frame)[-1]){
  img0 <- image_read(path = paste0("./plot-gif/globalchord",f,".png"))
  img <- c(img, img0)
  message(f)
}

img1 <- image_scale(image = img, geometry = "720x720")

ani0 <- image_animate(image = img1, fps = 10)
image_write(image = ani0, path = "./plot-gif/globalchord.gif")

# fixes for scale gap

scale_gap <- function(flow_m, flow_max, gap_at_max = 1, gaps = NULL) {
  p <- flow_m / flow_max
  if(length(gap_at_max) == 1 & !is.null(gaps)) {
    gap_at_max <- rep(gap_at_max, gaps)
  }
  gap_degree <- (360 - sum(gap_at_max)) * (1 - p)
  gap_m <- (gap_degree + sum(gap_at_max))/gaps
  return(gap_m)
}

d3 <- d2 %>%
  group_by(.frame) %>%
  summarise(flow = sum(flow)) %>%
  mutate(gaps = scale_gap(flow_m = flow, flow_max = max(.$flow), 
                          gap_at_max = 4, gaps = 9))

d3

circos.par(start.degree = 90, track.margin = c(-0.1, 0.1),
           gap.degree = filter(d3, .frame == f)$gaps, 
           points.overflow.warning = FALSE)


library(magrittr)

reg_max <- d0 %>%
  group_by(year0, orig_reg) %>%
  mutate(tot_out = sum(flow)) %>%
  group_by(year0, dest_reg) %>%
  mutate(tot_in = sum(flow)) %>%
  filter(orig_reg == dest_reg) %>%
  mutate(tot = tot_in + tot_out) %>%
  mutate(reg = orig_reg) %>%
  group_by(reg) %>%
  summarise(tot_max = max(tot)/1e06) %$%
  'names<-'(tot_max, reg)

reg_max

chordDiagram(x = filter(d2, .frame == f), directional = 1, order = d1$region,
             grid.col = d1$col1, annotationTrack = "grid",
             transparency = 0.25,  annotationTrackHeight = c(0.05, 0.1),
             direction.type = c("diffHeight", "arrows"), link.arr.type = "big.arrow",
             diffHeight  = -0.04, link.sort = TRUE, link.largest.ontop = TRUE, 
             xmax = reg_max)






#-----------------------------
# PIPE operators||||||||||||||
#-----------------------------
library(babynames)


babynames %>% filter(sex=="M", name=="Taylor") %>%
  select(n) %>%
  sum
bn = babynames %>% filter(name=="Taylor")


p <- ggplot() +
  geom_line(data = bn[bn$sex == "F", ], aes(x=year, y=n), color = "red") +
  geom_line(data = bn[bn$sex == "M", ], aes(x=year, y=n), color = "blue")
p

#-----------------------------
# Spatial ggplot
#-----------------------------

library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)
library(sf)

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)




game <- function(duration = 10) {
  points = 0
  for(i in c(1:duration)){
    random = world[ceiling(runif(1)*nrow(world)), ]
    #cat(random$sovereignt)
  
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

game()

ggsave("map.pdf")
ggsave("map_web.png", width = 6, height = 6, dpi = "screen")


ggplot(data = world) +
  geom_sf(aes(fill = pop_est)) +
  scale_fill_viridis_c(option = "inferno", trans = "sqrt")

ggplot(data = world) +
  geom_sf() +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("World map", subtitle = paste0("(Displaying ", length(unique(world$name)), " countries)"))

ggplot(data = world) +
  geom_sf() +
  coord_sf(crs = "EPSG:2050")

points_sf <- data.frame(
  id = 1:3,
  lon = c(10.53, 10.58, 10.62),
  lat = c(49.82, 49.79, 49.85)
)
points_sf <- st_as_sf(points_df, coords = c("lon", "lat"), crs = 4326)
points_sf
plot(points_sf["id"])

#load
sw_dataset <- read_csv("https://raw.githubusercontent.com/wegmann/R_data/master/Steigerwald_sample_points_all_data_subset_withNames.csv")

sw_points <- st_as_sf(sw_dataset, coords = c("x", "y"), crs = 32632)

ggplot(sw_points) +
  geom_sf(aes (color = LCname) , size = 2 , alpha = 0.7) +
  theme_minimal() +
  labs(title = "Land-Cover Classes in the Steigerwald", color = "LC Class") 

  
  
  
  