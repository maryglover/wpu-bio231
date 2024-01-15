library(prism)
library(reshape2)
library(dplyr)
library(tidyr)

prism_set_dl_dir("prism-climate-data/")
options(prism.path = 'prism-climate-data/')

get_prism_monthlys(type='tmean', keepZip=F, mon=1:12, keep_pre81_months = FALSE , years = 1981:2023)
get_prism_monthlys(type='ppt', keepZip=F, mon=1:12, keep_pre81_months = FALSE , years = 1981:2023)
get_prism_monthlys(type='tmin', keepZip=F, mon=1:12, keep_pre81_months = FALSE , years = 1981:2023)
get_prism_monthlys(type='tmax', keepZip=F, mon=1:12, keep_pre81_months = FALSE , years = 1981:2023)


# Extract project coordinates from raster stack
climate_crs <- climate_data@crs@projargs

raleigh <- data.frame(long = -78.6375, lat = 35.7892) 
coordinates(raleigh) <- c('long', 'lat')
proj4string(raleigh) <- CRS(climate_crs)

tmean_data <- prism_archive_subset(type='tmean', temp_period = 'monthly') %>%
  pd_stack() %>%
  raster::extract(raleigh)

precip_data <- prism_archive_subset(type='ppt', temp_period = 'monthly') %>%
  pd_stack()  %>%
  raster::extract(raleigh)

tmax_data <- prism_archive_subset(type='tmax', temp_period = 'monthly') %>%
  pd_stack() %>%
  raster::extract(raleigh)  

tmin_data <- prism_archive_subset(type='tmin', temp_period = 'monthly') %>%
  pd_stack()  %>%
  raster::extract(raleigh)


tmean_clean<- melt(tmean_data, value.name = 'tmean') %>%
  tidyr::separate(col=Var2, sep = '_', into = c(NA, NA, NA,NA, 'data',NA) )%>%
  separate(col=data, sep=4, into=c('year', 'month'))

precip_clean<- melt(precip_data, value.name = 'precip') %>%
  tidyr::separate(col=Var2, sep = '_', into = c(NA, NA, NA,NA, 'data',NA) )%>%
  separate(col=data, sep=4, into=c('year', 'month'))

tmin_clean<- melt(tmin_data, value.name = 'tmin') %>%
  tidyr::separate(col=Var2, sep = '_', into = c(NA, NA, NA,NA, 'data',NA) )%>%
  separate(col=data, sep=4, into=c('year', 'month'))

tmax_clean<- melt(tmax_data, value.name = 'tmax') %>%
  tidyr::separate(col=Var2, sep = '_', into = c(NA, NA, NA,NA, 'data',NA) )%>%
  separate(col=data, sep=4, into=c('year', 'month'))

climate_data<-tmean_clean %>%
  full_join(tmin_clean, by=c('month', 'year', 'Var1'))%>%
  full_join(tmax_clean, by=c('month', 'year', 'Var1'))%>%
  full_join(precip_clean, by=c('month', 'year', "Var1")) %>%
  select(-Var1)

write.csv(climate_data, 'data/raleigh_prism_climate.csv', row.names = F)
