
library(dplyr)
library(ggplot2)
library(sf)
library(lubridate)
library(tidyr)
library(tidyterra)
library(zoo)
library(terra)
library(prism)
library(FedData)
clean <- read.csv(file = "wq_clean3_data.csv")
clean

codes <- read.csv(file ="wq_stream_codes.csv")
Lat <- as.numeric (codes$Lat)
Long <- as.numeric (codes$Long)
codes

# Coding for setting up map, including town data, county data, and streams data

towns <- st_read('data/Townships-shp/') 
towns <- st_transform(towns, crs = 4269)
county <- st_read('data/Wake_County_Line-shp/')
county <- st_transform(county, crs = 4269)
streams <- st_read('data/Major_Rivers/')
streams <- st_transform(streams, crs = 4269)

# Code for WakeCounty map, streams, and points where data was taken

ggplot() +
  geom_sf(data = towns) +
  geom_sf(data = county, fill = 'transparent', linewidth = 0.75) + 
  geom_sf(data = streams, color = 'blue', linewidth = 0.3)+
  geom_point(data = codes, aes(x = Long, y = Lat), color = "red") +
  theme_minimal() +
  scale_fill_continuous(na.value = "transparent") 



lc_wake <- get_nlcd(template = county, label = 'wake', year = 2021, dataset = 'landcover')

crs(lc_wake)

countymask <- st_transform(county, crs = crs (lc_wake))

waketest <- project(lc_wake, 'EPSG:4269')

wacomask <- mask(waketest, county)

plot(mask(waketest ,county))

lc_wake
 
ggplot() +
  geom_spatraster(data = wacomask, aes(fill = Class)) +
  geom_sf(data = county, fill = 'transparent', linewidth = 0.75) +
  geom_sf(data = streams, color = 'blue', linewidth = 0.50)+
  geom_point(data = codes, aes(x = Long, y = Lat), color = 'black') +
  theme_minimal()

pointinfo <- extract(wacomask, codes[, c (5,4)])







#project maps

#2008

wake2008 <- get_nlcd(template = county, label = 'wake', year = 2008, dataset = 'impervious')

wake2008 <- subst(wake2008, NA, 0)

wake2008

crs(wake2008)

countymask2008 <- st_transform(county, crs = crs (wake2008))

waketest2008 <- project(wake2008, 'EPSG:4269')

wacomask2008 <- mask(waketest2008, county)

plot(mask(waketest2008 ,county))

ggplot()+
  geom_sf(data = county, fill = 'grey21', linewidth = 0.75) +
  geom_spatraster(data = wacomask2008, aes(fill = wake_NLCD_Impervious_2008)) + 
  geom_sf(data = streams, color = 'deepskyblue', linewidth = 0.5) +
  geom_point(data = codes, aes(x = Long, y = Lat), color = "lawngreen", size = 2.5) +
  scale_fill_viridis_c(na.value = 'transparent') +
theme_minimal() 
  
  
pointinfo <- extract(wacomask2008, codes[, c (5,4)])

#2016

wake2016 <- get_nlcd(template = county, label = 'wake', year = 2016, dataset = 'impervious')

wake2016 <- subst(wake2016, NA, 0)

wake2016

crs(wake2016)

countymask2016 <- st_transform(county, crs = crs (wake2016))

waketest2016 <- project(wake2016, 'EPSG:4269')

wacomask2016 <- mask(waketest2016, county)

plot(mask(waketest2016 ,county))


ggplot()+
  geom_sf(data = county, fill = 'grey21', linewidth = 0.75) +
  geom_spatraster(data = wacomask2016, aes(fill = wake_NLCD_Impervious_2016)) + 
  geom_sf(data = streams, color = 'deepskyblue', linewidth = 0.5) +
  geom_point(data = codes, aes(x = Long, y = Lat), color = "lawngreen", size = 2.5) +
  scale_fill_viridis_c(na.value = 'transparent') +
  theme_minimal() 

#2021

wake2021 <- get_nlcd(template = county, label = 'wake', year = 2021, dataset = 'impervious')

wake2021 <- subst(wake2021, NA, 0)

wake2021

crs(wake2021)

countymask2021 <- st_transform(county, crs = crs (wake2021))

waketest2021 <- project(wake2021, 'EPSG:4269')

wacomask2021 <- mask(waketest2021, county)

plot(mask(waketest2021 ,county))


ggplot()+
  geom_sf(data = county, fill = 'grey21', linewidth = 0.75) +
  geom_spatraster(data = wacomask2021, aes(fill = wake_NLCD_Impervious_2021)) + 
  geom_sf(data = streams, color = 'deepskyblue', linewidth = 0.5) +
  geom_point(data = codes, aes(x = Long, y = Lat), color = "lawngreen", size = 2.5) +
  scale_fill_viridis_c(na.value = 'transparent') +
  theme_minimal() 

#2001

wake2001 <- get_nlcd(template = county, label = 'wake', year = 2001, dataset = 'impervious')

wake2001 <- subst(wake2001, NA, 0)

wake2001

crs(wake2001)

countymask2001 <- st_transform(county, crs = crs (wake2001))

waketest2001 <- project(wake2001, 'EPSG:4269')

wacomask2001 <- mask(waketest2001, county)


plot(mask(waketest2001 ,county))

ggplot()+
  geom_sf(data = county, fill = 'transparent', linewidth = 0.75) +
  geom_spatraster(data = wacomask2001, aes(fill = wake_NLCD_Impervious_2001)) + 
  geom_sf(data = streams, color = 'deepskyblue', linewidth = 0.60) +
  geom_point(data = codes, aes(x = Long, y = Lat), size = 2.5, color = "lawngreen") +
  scale_fill_viridis_c(na.value = 'transparent') +
  theme_minimal() 


#map for difference in the years 

wakediff <- wacomask2021 - wacomask2001

mean(values(wakediff),na.rm = T)
# average difference in impeervious surface from 2001 to 2021

geom_spatraster(data = wakediff)

ggplot()+
  #geom_sf(data = county, fill = 'grey21', linewidth = 0.75) +
  geom_spatraster(data = wakediff, aes(fill = wake_NLCD_Impervious_2021)) + 
  geom_sf(data = streams, color = 'deepskyblue', linewidth = 0.60) +
  geom_point(data = codes, aes(x = Long, y = Lat), size = 2.5, color = "lawngreen") +
  scale_fill_viridis_c(na.value = 'transparent') +
    theme_minimal() 

wakediff
mean(wakediff$values)









