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


# raster plot
# precip last year
files2023<- prism_archive_subset('ppt', 'daily', year = 2023)
stack2023 <- pd_stack(files2023)
avg2023 <- mean(stack2023)
plot(avg2023)
