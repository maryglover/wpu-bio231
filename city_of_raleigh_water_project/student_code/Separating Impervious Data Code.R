  # General Coding and Set up
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr) 
library(sf)
library(tidyterra)
library(zoo)
library(terra)
library(FedData)

clean <- read.csv(file = 'wq_clean3_data.csv', header = T)

clean

cleanbase <- clean %>% select('Site', 'year', 'E_coli_MPN_100mL', 'E_coli_CFU_100mL', 'do_percent_sat', 'do_mg_L', 'NO2_NO3_mg_L') 

codes <- read.csv(file = 'wq_stream_codes.csv', header = T)
codes
Lat <- as.numeric (codes$Lat)
Long <- as.numeric (codes$Long) 


  # Coding for setting up map, including county data and streams data
county <- st_read('data/Wake_County_Line-shp/')
county <- st_transform(county, crs = 4269)
streams <- st_read('data/Major_Rivers/')
streams <- st_transform(streams, crs = 4269)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # Setting a new base of the big dataset that just includes the things we're looking at
cleanbase <- clean %>% select('Site', 'year', 'E_coli_MPN_100mL', 'do_percent_sat', 'NO2_NO3_mg_L', 'Turbidity')

  # Getting the impervious data for 2021
wake21 <- get_ncld(template = county, label = 'wake', year = 2021, dataset = 'impervious') 

  # This is changing the NAs to 0s
wake21 <- subst(wake21, NA, 0)

  # Changing the data to the same (lat/long)
wake21a <- project(wake21, "EPSG:4269")

  # Extracting the impervious data to the codes data
pointinfo <- extract(wake21b, codes[, c(5, 4)]) 

  # Changing the column names and joining them together
colnames(codes)[1] <- "ID"
codes21 <- left_join(codes, pointinfo, by = join_by(ID))
clean21 <- left_join(cleanbase, codes21, by = join_by(Site))
colnames(clean21)[12] <- "Impervious"

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # Getting all the impervious data for all applicable years
wake08 <- get_nlcd(template = county, label = 'wake', year = 2008, dataset = 'impervious')

wake11 <- get_nlcd(template = county, label = 'wake', year = 2011, dataset = 'impervious')

wake13 <- get_nlcd(template = county, label = 'wake', year = 2013, dataset = 'impervious')

wake16 <- get_nlcd(template = county, label = 'wake', year = 2016, dataset = 'impervious')

wake19 <- get_nlcd(template = county, label = 'wake', year = 2019, dataset = 'impervious')

wake21 <- get_nlcd(template = county, label = 'wake', year = 2021, dataset = 'impervious')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # Setting up county data to get template
county <- st_read('data/Wake_County_Line-shp/')
county <- st_transform(county, crs = 4269)

  # Getting impervious data
wake21 <- get_nlcd(template = county, label = 'wake', year = 2021, dataset = 'impervious')

  # Setting the NAs to 0s
wake21 <- subst(wake21, NA, 0)

  # Converting the coordinates to the same as the other files
wake21a <- project(wake21, "EPSG:4269")

  # Masking the impervious information with county information
wake21b <- mask(wake21a, county) 

  # Extracting the impervious data
pointinfo <- extract(wake21b, codes[, c(5, 4)]) 

  # Some coding to join the impervious data and a previously-used stream data
colnames(codes)[1] <- "ID"
codes21 <- left_join(codes, pointinfo, by = join_by(ID))
clean21 <- left_join(cleanbase, codes21, by = join_by(Site))
colnames(clean21)[12] <- "Impervious"

  # Filtering just for 2021 for testing
clean21 |> filter(year == 2021)

  # Testing to see if everything works as intended
ggplot() +
  geom_point(data = clean21, aes(x = wake_NLCD_Impervious_2021, y = do_percent_sat, color = Site))  

  # Saving file as .csv to send to group mates
write.csv(clean21, file = "wake_impervious_2021.csv") 

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # WAKE 2008
wake08 <- subst(wake08, NA, 0)
wake08a <- project(wake08, "EPSG:4269")
wake08b <- mask(wake08a, county) 
pointinfo <- extract(wake08b, codes[, c(5, 4)]) 
colnames(codes)[1] <- "ID"
codes08 <- left_join(codes, pointinfo, by = join_by(ID))
clean08 <- left_join(cleanbase, codes08, by = join_by(Site))
colnames(clean08)[12] <- "Impervious"
clean08 |> filter(year == 2008)

write.csv(clean08, file = "wake_impervious_2008.csv") 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # WAKE 2011
wake11 <- subst(wake11, NA, 0)
wake11a <- project(wake11, "EPSG:4269")
wake11b <- mask(wake11a, county) 
pointinfo <- extract(wake11b, codes[, c(5, 4)]) 
colnames(codes)[1] <- "ID"
codes11 <- left_join(codes, pointinfo, by = join_by(ID))
clean11 <- left_join(cleanbase, codes11, by = join_by(Site))
colnames(clean11)[12] <- "Impervious"
clean11 |> filter(year == 2011)

write.csv(clean11, file = "wake_impervious_2011.csv") 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # WAKE 2013
wake13 <- subst(wake13, NA, 0)
wake13a <- project(wake08, "EPSG:4269")
wake13b <- mask(wake13a, county) 
pointinfo <- extract(wake13b, codes[, c(5, 4)]) 
colnames(codes)[1] <- "ID"
codes13 <- left_join(codes, pointinfo, by = join_by(ID))
clean13 <- left_join(cleanbase, codes13, by = join_by(Site))
colnames(clean13)[12] <- "Impervious"
clean13 |> filter(year == 2013)

write.csv(clean13, file = "wake_impervious_2013.csv") 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # WAKE 2016
wake16 <- subst(wake16, NA, 0)
wake16a <- project(wake16, "EPSG:4269")
wake16b <- mask(wake16a, county) 
pointinfo <- extract(wake16b, codes[, c(5, 4)]) 
colnames(codes)[1] <- "ID"
codes16 <- left_join(codes, pointinfo, by = join_by(ID))
clean16 <- left_join(cleanbase, codes16, by = join_by(Site))
colnames(clean16)[12] <- "Impervious"
clean16 |> filter(year == 2016)

write.csv(clean16, file = "wake_impervious_2016.csv") 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # WAKE 2019
wake19 <- subst(wake19, NA, 0)
wake19a <- project(wake19, "EPSG:4269")
wake19b <- mask(wake19a, county) 
pointinfo <- extract(wake19b, codes[, c(5, 4)]) 
colnames(codes)[1] <- "ID"
codes19 <- left_join(codes, pointinfo, by = join_by(ID))
clean19 <- left_join(cleanbase, codes19, by = join_by(Site))
colnames(clean19)[12] <- "Impervious"
clean19 |> filter(year == 2019)

write.csv(clean19, file = "wake_impervious_2019.csv") 
