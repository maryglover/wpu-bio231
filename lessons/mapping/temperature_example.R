# temperature files for class
library(prism)

# get yearly data
#prism_set_dl_dir('prism-climate-data/')
#get_prism_annual(type = 'tmean', year = 1991:1995, keepZip = F)
#get_prism_annual(type = 'tmean', year = 2019:2023, keepZip = F)

tmean90s_files<- prism_archive_subset('tmean', 'annual', years = 1991: 1995)
tmean90s <- pd_stack(tmean90s_files)

tmean20s_files<- prism_archive_subset('tmean', 'annual', years = 2019: 2023)
tmean20s <- pd_stack(tmean20s_files)

writeRaster(tmean90s, 'data/tmean_1991_1995.grd', overwrite = T)
writeRaster(tmean20s, 'data/tmean_2019_2023.grd', overwrite = T)

pd_stack(prism_archive_subset('tmean', 'annual', years = 2023)) |>
  writeRaster('data/tmean_2023.grd')
writeRaster(tmean20s, 'data/tmean_2019_2023.grd', overwrite = T)


library(terra)
temp<-rast('data/tmean_1991_1995.grd' )
plot(temp)
