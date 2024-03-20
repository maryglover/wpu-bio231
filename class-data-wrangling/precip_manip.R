# get precipitation data

library(dplyr)
library(prism)
library(sf)
library(raster)
library(reshape2)

streams <- read.csv('data/raleigh_wq_stream_codes.csv', header = F)
streams <- streams |>
  rename(stream = V1) |>
  separate(V2, into = c('lat', 'long'), sep = ",") |>
  filter(!is.na(long))

water <- read.csv('data/raleigh_wq_clean-units.csv')
water <- water |>
  mutate(Date = ymd(Date))

coll.dates <- unique(water$Date)
unique(month(coll.dates))

prism_set_dl_dir('prism-climate-data/')
#get_prism_dailys('ppt', minDate = "2008-01-01", maxDate = "2023-12-31", keepZip = F)
#get_prism_dailys('ppt', minDate = "2018-01-01", maxDate = "2023-12-31", keepZip = F)

#precip.2023 <- prism_archive_subset('ppt', 'daily', year = 2023) |>
  pd_stack() 

#precip <- prism_archive_subset('ppt', 'daily') |>
#  pd_stack() 

## county line
county_shp <- 'data/Wake_County_Line-shp//'
county <- st_read(county_shp)
st_bbox(county)
extent(county)

county <- st_transform(
  county,
  crs = 4269)

#crop(precip.2023, extent(county))

for(i in 2008:2023){
  print(i)
  temp<- prism_archive_subset('ppt', 'daily', year = i)
  temp.stack <- pd_stack(temp)
  crop.name <- paste('crop', i, sep = '_')
  assign(crop.name, crop(temp.stack, extent(county)))
}

files <- paste('crop', 2008:2023, sep = '_')
crop_stack <- stack(mget(files))

streams$lat <- as.numeric(streams$lat)
streams$long <- as.numeric(streams$long)
ppt.point <- extract(crop_stack, streams[,c(3,2)])
row.names(ppt.point) <- streams$stream
#ppt.name <- cbind( stream = streams$stream, ppt.point)

#writeRaster(crop_stack, 'data/wake_precip_stack.tif')

library(terra)
#terra::writeRaster(crop_stack, 'data/wake_precip_stack.tif', overwrite = T)
terra::writeRaster(crop_stack, 'data/wake_precip_stack.grd', overwrite = T)

y <- raster::brick("data/wake_precip_stack.tif") 

precip_clean <- melt(ppt.point, value.name = 'precip') |>
  tidyr::separate(col=Var2, sep = '_', into = c(NA, NA, NA,NA, 'data',NA) ) |>
  mutate(Date = ymd(data), .keep = 'unused') |>
  separate(col = Var1, sep = '—', into=c('Site', 'Stream' ))

test <- melt(ppt.point, value.name = 'precip') |>
  tidyr::separate(col=Var2, sep = '_', into = c(NA, NA, NA,NA, 'data',NA) ) |>
  head()

test |>
  mutate(Date = ymd(data), .keep = 'unused')
  ymd(as.numeric(data))
write.csv(precip_clean, 'data/precip_stream_sites.csv', row.names = F)


### how to get rolling sum

precip_clean
unique(water$Date)

precip_prev5days <- precip_clean |>
  group_by(Site, Stream) |>
  arrange(Date) |>
  mutate(sum_prev5 = zoo::rollsum(precip, k = 5, align = 'right', na.pad = TRUE))

precip_prev5days |>
  filter(Date %in% unique(water$Date)) |>
  arrange(-precip)

precip_prev5days |>
  filter(Site == 'RB15', year(Date) == 2009, month(Date) ==6)

# raster plot
# precip last year
files2023<- prism_archive_subset('ppt', 'daily', year = 2023)
stack2023 <- pd_stack(files2023)
avg2023 <- mean(stack2023)
plot(avg2023)


## climate yearlys 
library(prism)
# get yearly data
prism_set_dl_dir('prism-climate-data/')
get_prism_annual(type = 'ppt', year = 2008:2023, keepZip = F)
get_prism_annual(type = 'tmean', year = 2008:2023, keepZip = F)
get_prism_annual(type = 'tmin', year = 2008:2023, keepZip = F)
get_prism_annual(type = 'tmax', year = 2008:2023, keepZip = F)

# mask for wake county shape

year_ppt_files<- prism_archive_subset('ppt', 'annual')
year_ppt <- pd_stack(year_ppt_files)

avg_ppt <- mean(year_ppt)
plot(avg_ppt)

year_ppt_crop <- crop(year_ppt, county_bbox)
year_ppt_mask <- mask(year_ppt_crop, county)
plot(year_ppt_mask)

library(stars)
year_ppt.st <- st_as_stars(year_ppt_mask)
ggplot() +
  geom_stars(data = year_ppt.st) +
  geom_sf(data = county, fill = NA, linewidth=2) +
  theme_map() 

st_extract(year_ppt.st, park)
