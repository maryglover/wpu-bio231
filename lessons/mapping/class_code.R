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


# March 28
library(sf)
rivers <- st_read('data/Major_Rivers/')
county <- st_read('data/Wake_County_Line-shp/')
towns <- st_read('data/Townships-shp/')

rivers <- st_transform(rivers, crs = 4269)
county <- st_transform(county, crs = 4269)

library(ggplot2)
ggplot()+
  geom_sf(data= towns)
towns

library(dplyr)
raleigh <- filter(towns, NAME == 'RALEIGH')

raleigh <- st_transform(raleigh, crs = 4269)

raleigh_stream <- st_crop(rivers, raleigh) 

ggplot() +
  geom_sf(data = raleigh) +
  geom_sf(data = raleigh_stream, color = 'cornflowerblue') +
  theme_bw()

svi <- st_read('data/CDC_SVI/')

ggplot(data = svi) +
  geom_sf(aes(fill = EP_NOHSDP))

library(terra)
temp2023 <-rast('data/tmean_2023.grd')

plot(temp2023)

tmean_90s <- rast('data/tmean_1991_1995.grd')
tmean_20s <- rast('data/tmean_2019_2023.grd')

plot(tmean_90s)
tmean_90s

avg90s <- mean(tmean_90s)
avg20s <- mean(tmean_20s)

temp_change <- avg20s - avg90s
plot(temp_change)


wake_temp <- crop(avg20s, county)
wake_temp <- mask(wake_temp, county)
library(tidyterra)

ggplot()+
  geom_spatraster(data = wake_temp, aes(fill = mean)) +
  scale_fill_continuous(na.value = 'transparent') +
  geom_sf(data = county, fill = 'transparent', linewidth = 1.5) +
  geom_sf(data = rivers, color = 'orange') +
  theme_minimal()


map <- map_data('county')
head(map)
nc <- filter(map, region == 'north carolina')
head(nc)

ggplot(data = nc, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes( fill = subregion))+
  theme_minimal()+
  theme(legend.position = 'none') +
  scale_fill_viridis_d(option = 'H') 


ggplot()+
  geom_spatraster(data = wake_temp, aes(fill = mean)) +
  scale_fill_viridis_c(na.value = 'transparent', direction = -1)

ggplot()+
  geom_spatraster(data = wake_temp, aes(fill = mean)) +
  scale_fill_gradient(low = 'red', high= 'darkgreen', na.value = "transparent") +
  theme_minimal()

ggplot()+
  geom_spatraster(data = wake_temp, aes(fill = mean)) +
  scale_fill_gradientn(colors = heat.colors(10),  na.value = "transparent") +
  theme_minimal()

library(ggspatial) 
ggplot() +
  annotation_map_tile(type = 'osm', zoomin = -1)+
  geom_sf(data = raleigh_stream, color = 'darkblue', linewidth = 1.5) +
  theme_void()  

