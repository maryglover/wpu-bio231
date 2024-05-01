# impervious surfaces data
# NCLD data downloaded from MRLC https://www.mrlc.gov/viewer/
# Data is in google student data folder

library(terra)

impervious <- rast('./data/NLCD_wake/NLCD_2021_Impervious_L48_20230630_chEYjUNvvA8Jo6INlj0I.tiff')

library(ggplot2)
library(tidyterra)
ggplot()+
  geom_spatraster(data = impervious, aes(fill = Layer_1), use_coltab = F)  #doesn't work because is integer data not numeric
 
plot(impervious)

#change values to numeric for plotting in ggplot
values(impervious) <- as.numeric(values(impervious))

ggplot()+
  geom_spatraster(data = impervious, aes(fill = Layer_1)) +
  geom_sf(data = county, fill = 'transparent', linewidth = 1.5) +
  scale_fill_distiller(palette = 'Oranges', direction = 1)

# coord system
impervious <- project(impervious, "EPSG:4269")

## crop for wake county 
library(sf)
county <- st_read('data/Wake_County_Line-shp/')
county <- st_transform(county, crs = 4269)

wake_impervious <- crop(impervious, county, mask = T)
ggplot()+
  geom_spatraster(data = wake_impervious, aes(fill = Layer_1)) +
  geom_sf(data = county, fill = 'transparent', linewidth = 1.5) +
  scale_fill_distiller(palette = 'Oranges', direction = 1, na.value = 'transparent') + 
  theme_void()

# with streams for fun
raleigh_water <-st_read('data/Major_Rivers/')
raleigh_water <- st_transform(raleigh_water, crs = 4269)

ggplot()+
  geom_spatraster(data = wake_impervious, aes(fill = Layer_1)) +
  geom_sf(data = county, fill = 'transparent', linewidth = 1.5) +
  geom_sf(data = raleigh_water, color = 'cornflowerblue', linewidth= 1) +
  scale_fill_distiller(palette = 'Oranges', direction = 1, na.value = 'transparent') + 
  theme_void()

# save for class to use (don't run)
#writeRaster(wake_impervious, 'data/wake_impervious_nlcd.tif')

# get stream info for extracting values
library(tidyr)
streams <- read.csv('data/raleigh_wq_stream_codes.csv', header = F)
streams <- streams |>
  rename(stream = V1) |>
  separate(V2, into = c('lat', 'long'), sep = ",") |>
  filter(!is.na(long))

streams$lat <- as.numeric(streams$lat)
streams$long <- as.numeric(streams$long)

#extracts the impervious surface % for each stream site for City of Raleigh
raster::extract(impervious, streams[,c(3,2)])
## In future, might be worth looking into buffering this and getting an estimate for around the stream site. 

## land cover data with feddata
library(FedData)
library(sf)

county <- st_read('data/Wake_County_Line-shp/')
county <- st_transform(county, crs = 4269)

impervious_wake <- get_nlcd(template = county, label = 'wake', year = 2021, dataset = 'impervious')

impervious_wake <- subst(impervious_wake, NA, 0) 

impervious_wake <- project(impervious_wake, "EPSG:4269")

impervious_mask <- mask(impervious_wake, county)

ggplot()+
  geom_spatraster(data = impervious_mask) +
  scale_fill_continuous(na.value = 'transparent')
