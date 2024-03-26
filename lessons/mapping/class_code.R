## mapping in R

library(ggplot2)
library(dplyr)

map <- map_data('county')
head(map)
nc <- filter(map, region == 'north carolina')
head(nc)

nc |>
  filter(subregion == 'wake')

ggplot(data = nc, aes(x = long, y = lat, group = group)) +
  geom_polygon(color = 'darkblue', fill = 'lightblue')


ggplot(data = nc, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = subregion)) +
  theme(legend.position = 'none')

library(sf)
rivers <- st_read('data/Major_Rivers/')
head(rivers)
rivers$geometry[1]

ggplot() +
  geom_sf(data = rivers, color = 'blue' )

rivers |>
  distinct(NAME)

rivers |>
  filter(NAME == 'Cemetery Branch') 

cemetery_branch <- filter(rivers, NAME == 'Cemetery Branch')

ggplot() +
  geom_sf(data = rivers, color = 'cornflowerblue') +
  geom_sf(data = cemetery_branch, color = 'red')

county <- st_read('data/Wake_County_Line-shp/')

ggplot() +
  geom_sf(data = rivers, color = 'cornflowerblue') +
  geom_sf(data = cemetery_branch, color = 'red') +
  geom_sf(data = county, fill = 'transparent') +
  theme_void()

park <- st_point(c(-78.6247498208728, 35.7947396684644))


ggplot() +
  geom_sf(data = rivers, color = 'cornflowerblue') +
  geom_sf(data = park)

rivers <- st_transform(rivers, crs = 4269)

park <- st_sfc(park, crs = 4269)

ggplot() +
  geom_sf(data = rivers, color = 'blue')+
  geom_point(aes(x = -78.62475, y = 35.79474), color = 'red') +
  theme_minimal()

stream_code$Lat <- as.numeric(stream_code$Lat)

stream_code$Long <- as.numeric(stream_code$Long)

ggplot() +
  geom_sf(data = rivers, color = 'blue')+
  geom_point(data = stream_code, aes(x=Long, y =Lat))
