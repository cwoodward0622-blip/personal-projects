

library(ggplot2)
library(dplyr)
library(readr)
library(maps)
library(patchwork)

# Load data
walmart <- read_csv("walmart.csv")

walmart$lat <- as.numeric(walmart$lat)
walmart$lng <- as.numeric(walmart$lng)

# 2010 stores
stores2010 <- walmart %>% filter(year == 2010)

# US map
us_map <- map_data("state")

# -------------------
# REGION BOUNDARIES
# -------------------

west_bounds <- c(-125, -102, 31, 49)
midwest_bounds <- c(-102, -85, 36, 49)
south_bounds <- c(-105, -75, 24, 37)
north_bounds <- c(-85, -67, 37, 47)

# -------------------
# FUNCTION TO MAKE MAP
# -------------------

make_map <- function(xmin,xmax,ymin,ymax,title,subtitle=TRUE){
  
  p <- ggplot() +
    
    geom_polygon(
      data = us_map,
      aes(long, lat, group = group),
      fill="gray95",
      color="white"
    ) +
    
    geom_point(
      data = walmart,
      aes(lng, lat),
      color="blue",
      alpha=.25,
      size=.7
    ) +
    
    geom_point(
      data = stores2010,
      aes(lng, lat),
      color="red",
      size=2
    ) +
    
    coord_quickmap(
      xlim=c(xmin,xmax),
      ylim=c(ymin,ymax)
    ) +
    
    theme_void() +
    
    theme(
      plot.title = element_text(size=16, face="bold"),
      plot.subtitle = element_text(size=11)
    )
  
  if(subtitle){
    p <- p + labs(
      title = title,
      subtitle = "Blue = Existing Stores | Red = Opened in 2010"
    )
  } else{
    p <- p + labs(title = title)
  }
  
  return(p)
}

# -------------------
# CREATE MAPS
# -------------------

# West has NO subtitle
west <- make_map(west_bounds[1],west_bounds[2],west_bounds[3],west_bounds[4],"",FALSE)

# Midwest keeps subtitle
midwest <- make_map(midwest_bounds[1],midwest_bounds[2],midwest_bounds[3],midwest_bounds[4],"",TRUE)

south <- make_map(south_bounds[1],south_bounds[2],south_bounds[3],south_bounds[4],"South",TRUE)

north <- make_map(north_bounds[1],north_bounds[2],north_bounds[3],north_bounds[4],"North",TRUE)

# -------------------
# PANEL
# -------------------

(west | midwest) /
  (south | north) +
  plot_annotation(
    title = "West / Midwest",
    theme = theme(
      plot.title = element_text(size = 20, face = "bold", hjust = 0.5)
    )
  )

