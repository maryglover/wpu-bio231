# mapping streams in Raleigh 

library(sf)

shp <- 'data/Major_Rivers/'
streams <- st_read(shp)
st_bbox(streams)
st_crs(streams)

library(ggplot2)
ggplot() +
  geom_sf(data = streams, color = 'blue') + 
  theme_void()

library(dplyr)
st_crs(streams)
streams |>
  distinct(NAME)

streams |>
  filter(NAME == 'Cemetery Branch') |>
  st_bbox()


## county line
county_shp <- 'data/Wake_County_Line-shp//'
county <- st_read(county_shp)
st_bbox(county)
st_crs(county)

ggplot() +
  geom_sf(data = county, color = 'red', fill = 'white') +
  geom_sf(data = streams, color = 'blue') + 
  theme_void()


# transform 
county <- st_transform(
  county,
  crs = 4269)

streams <- st_transform(
  streams,
  crs = 4269)

# Add point
park <- st_sfc(st_point(
  c(-78.6247498208728, 35.7947396684644)),
  crs = st_crs(streams))

ggplot() +
  geom_sf(data = county, color = 'black', fill = 'white', size = 2) +
  geom_sf(data = streams, color = 'blue') + 
  geom_sf(data = park, color = 'red') +
  theme_void()

# google maps
library(ggmap)

bbox <- setNames(st_bbox(streams), c("left", "bottom", "right", "top"))
#setting zoom to 9 gives us a bit of padding around the bounding box
basemap_streets <- get_map(maptype = "roadmap", location = bbox, zoom = 9)
basemap_satellite <- get_map(maptype = "satellite", location = bbox, zoom = 9)
street_map <- ggmap(basemap_streets)
satellite_map <- ggmap(basemap_satellite)
print(street_map)