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
county_shp <- 'data/Wake_County_Line-shp/'
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


## plot precipitation raster
library(raster)
library(ggplot2)
library(sf)
precip <- brick("data/wake_precip_stack.grd")

precip_jan1_2022 <- precip$PRISM_ppt_stable_4kmD2_20230101_bil
plot(precip_jan1_2022)

library(ggthemes)

jan1.df <- as.data.frame(precip_jan1_2022, xy=T) 
jan1.sf <- st_as_sf(rasterToPoints(precip_jan1_2022, spatial=T))

ggplot() +
  geom_raster(data = jan1.df, aes(x=x, y=y, fill= PRISM_ppt_stable_4kmD2_20230101_bil))+
scale_fill_viridis_c() +
  coord_equal() +  theme_map() +
  theme(legend.position="bottom") +
  theme(legend.key.width=unit(2, "cm")) +
  geom_sf(data = county, color = 'black', fill = 'transparent',  size = 2)+
  geom_sf(data = streams, color = 'blue') 
 
library(ggspatial) 
  ggplot() +
  annotation_map_tile(type = 'cartolight', zoomin = -1)+ 
    geom_sf(data = streams, color = 'blue', linewidth = 2) +
    geom_sf(data = county, color = 'black', fill = 'transparent',  linewidth = 3)
 #   annotation_scale(location = "tl") +
#    annotation_north_arrow(location = "br", which_north = "true")

ggplot() + layer_spatial(precip_jan1_2022)


## Census data
library(raster)
library(sf)

shp00 <- 'data/Census_Tracts_2000/'
census2000<- st_read(shp00)
census2020<-st_read('data/Census_Tracts_2020/Census_Tracts_2020.shp')
census2010<-st_read('data/Census_Tracts_2010/Census_Tracts_2010.shp')
st_bbox(census2000)

library(ggplot2)
ggplot() +
  geom_sf(data = census2020, aes( fill=POPULATION))+
  scale_fill_viridis_c() 

## combine data
library(tidyr)
library(dplyr)
nrow(census2000)
census2020

#census2010 |>
#  select(POP2000, TRACT, geometry) |>
#  st_join(census2020) |>
#  mutate(pop_difference = POPULATION - POP2000) |>
 
  
ggplot(census2020)+
  geom_sf(aes(fill = POPULATION)) +
# scale_fill_gradient2(low="yellow", high="red", mid = 'white')
#  scale_fill_gradientn(colours = terrain.colors(10))
scale_fill_distiller(palette = 'RdGy')

towns <- st_read('data/Townships-shp/01364030-cddb-4670-a095-4e15193665872020330-1-d3s0ym.2yd2o.shp')

census2020 |>
  group_by(paste(st_intersects(towns, census2020, sparse=T)))
 s_df %>% 
  group_by(group = paste(st_intersects(s_df, s_df, sparse = T))) %>% 
  summarise(score = sum(score))
st_intersection(census2020, towns)


townpop20<- st_join(census2020, towns) |>
  group_by(NAME.y)|>
  summarize(pop20 = sum(POPULATION)) 

townpop10 <- st_join(census2010, towns) |>
  group_by(NAME)|>
  summarize(pop10 = sum(TOTAL_POP)) 
ggplot(townpop10) +
  geom_sf(aes(fill = pop10))


st_join(townpop10, townpop20) |>
  mutate(pop_diff = pop20-pop10)
st_join(census2020, towns) |>
  group_by(NAME.y)|>
  summarize(pop = sum(POPULATION)) |>
  ggplot()+
  geom_sf(aes(fill = pop)) +
  # scale_fill_gradient2(low="yellow", high="red", mid = 'white')
  #  scale_fill_gradientn(colours = terrain.colors(10))
  scale_fill_distiller(palette = 'Oranges',direction = 1)

census2000 |>
  select(POP2000, TRACT, geometry) |>
  st_join(census2020) |>
  mutate(pop_difference = POPULATION - POP2000) |>
  arrange(pop_difference)

# resources
#https://tmieno2.github.io/R-as-GIS-for-Economists/geom-raster.html

#https://ggplot2-book.org/scales-colour

