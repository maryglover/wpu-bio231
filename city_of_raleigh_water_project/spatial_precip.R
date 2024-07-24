library(tidyr)
precip_clean <- read.csv('data/precip_stream_sites.csv')
water <- read.csv('data/raleigh_wq_clean-units.csv')
streams <- read.csv('data/raleigh_wq_stream_codes.csv', header = F)

stream_mod <- streams |>
  separate(V1, into = c('Site', 'Stream'), sep = '—') |>
  filter(!is.na(Stream)) |>
  separate(V2, into = c('lat', 'long'), sep = ',')

write.csv(stream_mod, 'data/stream_codes.csv')

water_precip <- left_join(water, precip_clean) |>
  left_join(stream_mod)

head(water_precip)

## points to utm
library(sf)
lat_long <- dplyr::select(stream_mod, lat, long)
summary(lat_long)
lat_long$lat <- as.numeric(lat_long$lat)
lat_long$long <- as.numeric(lat_long$long)

site_lat <- st_multipoint(as.matrix(lat_long))
site_lat <- st_sfc(site_lat, crs = "+proj=longlat +datum=WGS84")
site_utm <- st_transform(site_lat, crs = "+proj=utm +zone=17 +datum=WGS84 +units=m" )
site_utm.df <- data.frame(Site = stream_mod$Site, st_coordinates(site_utm))

water_utm_precip<- site_utm.df |>
  select(Site, lat = X, long = Y) |>
  inner_join(precip_clean) |>
  right_join(water) 

write.csv(water_utm_precip, 'data/wq_precipitation.csv', row.names = F)

# confirm zone
library(gfcanalysis)
library(dplyr)
lat_long |>
  arrange(long)
utm_zone(-78.79608, 35.889)
utm_zone(-78.53568, 35.74925)

library(ggplot2)

joined <- precip_clean |>
  group_by(Site, Stream) |>
  arrange(Date) |>
  mutate(sum_prev5 = zoo::rollsum(precip, k = 5, align = 'right', na.pad = TRUE)) |>
  inner_join(water_utm_precip) 

ggplot(joined, aes(x=precip, y = E_coli_MPN_100mL)) +
  geom_point() +
  geom_smooth(method = 'lm') +
#  coord_cartesian(ylim = c(0, 30000)) +
  facet_wrap(~Site) +
  scale_y_continuous(trans = 'log1p', breaks = c(10, 100, 1000, 10000))
