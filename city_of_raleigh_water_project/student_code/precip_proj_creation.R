# GROUP PROJECT - Data file creation
library(tidyr)
library(dplyr)
library(lubridate)

## Reading in data for final file creation (final file is also attached separately)

precip <-
  read.csv('data/raleigh_wq_clean-units.csv')

codes <-
  read.csv('data/raleigh_wq_stream_codes.csv', header = F)

## Separating codes into site, stream, lat, and long

codes <-
  codes |>
  separate(V1, into = c('Site', 'Stream'), sep = '—')

codes <-
  codes |>
  separate(V2, into = c('Latitude', 'Longitude'), sep = ',')

## Joining precip and codes

precip <-
  left_join(precip, codes)

## Converting Lat and Long to numbers

precip$Latitude <- as.numeric(precip$Latitude)
precip$Longitude <- as.numeric(precip$Longitude)

## Creating new data frame and extracting only what we want to use
precip_proj <-
  precip |>
  select(Site, Stream, Date, Time, Phosphorus_total_mg_L, E_coli_MPN_100mL, do_percent_sat, pH, Turbidity_NTU, NO2_NO3_mg_L, Latitude, Longitude)

## Displaying column names so we can actually see them
colnames(precip_proj)

## Converting NTU to JTU
precip_proj <-
  precip_proj |>
  mutate(Turbidity_JTU = Turbidity_NTU * 2.5)


## Mutating the data frame to add in individual scores

precip_proj <-
  precip_proj |>
  mutate(Phos_score = 
           case_when(Phosphorus_total_mg_L > 4 ~ 2,
                     Phosphorus_total_mg_L > 2 & Phosphorus_total_mg_L <= 4 ~ 3,
                     Phosphorus_total_mg_L <= 2 ~ 4)
  )
  
precip_proj <-
  precip_proj |>
  mutate(do_score = 
           case_when(do_percent_sat >= 91 ~ 4,
                     do_percent_sat >= 71 & do_percent_sat < 91 ~ 3,
                     do_percent_sat >= 50 & do_percent_sat < 71 ~ 2,
                     do_percent_sat < 50 ~ 1)
  )


precip_proj <-
  precip_proj |>
  mutate(turb_score = 
           case_when(Turbidity_JTU == 0 ~ 4,
                     Turbidity_JTU > 0  & Turbidity_JTU <= 40 ~ 3,
                     Turbidity_JTU > 40 & Turbidity_JTU <= 100 ~ 2,
                     Turbidity_JTU > 100 ~ 1)
  )
         

precip_proj <-
  precip_proj |>
  mutate(pH_score =
           case_when(pH <= 5.5 ~ 1,
                     pH > 5.5 & pH <= 6.5 ~ 3,
                     pH > 6.5 & pH <= 7.5 ~ 4,
                     pH > 7.5 & pH <= 8.5 ~ 3,
                     pH > 8.5 ~ 1)
  )
  
precip_proj <-
  precip_proj |>
  mutate(e_coli_score =
           case_when(E_coli_MPN_100mL > 0 ~ 1, E_coli_MPN_100mL == 0 ~ 3))

precip_proj <-
  precip_proj |>
  mutate(nitrate_score =
           case_when(NO2_NO3_mg_L == 0 ~ 3,
                     NO2_NO3_mg_L > 0 & NO2_NO3_mg_L <= 5 ~ 2,
                     NO2_NO3_mg_L > 5 ~ 1))

## Creating overall wq score

precip_proj <-
  precip_proj |>
  mutate(wq_score =
           precip_proj$do_score + precip_proj$Phos_score + precip_proj$turb_score + precip_proj$pH_score + precip_proj$e_coli_score + precip_proj$nitrate_score)

## Removing NAs from our data set
precip_proj <-
  precip_proj |>
  filter(wq_score != 'NA')

## Creating overall stream rating for context
precip_proj <-
  precip_proj |>
  mutate(stream_rating =
           case_when(wq_score >= 18 ~ 'excellent',
                     wq_score >= 14 & wq_score < 18 ~ 'good',
                     wq_score >= 10 & wq_score < 14 ~ 'fair',
                     wq_score < 10 ~ 'poor'))


## Creating a way to separate out seasons

precip_proj <-
  precip_proj |>
  mutate(Date = ymd(Date))

precip_proj <-
  precip_proj |>
  separate(Date, into = c('Year', 'Month', 'Day'), sep = '-')

# Converting Month column to numbers to separate it out for a case when
precip_proj$Month <- as.numeric(precip_proj$Month)

#Figuring out which numbers are used as Months
precip_proj |>
  distinct(Month)

#Actually doing the case when to create season column
precip_proj <-
  precip_proj |>
  mutate(Season = 
           case_when(Month == 12 ~ 'Winter',
                     Month == 1 ~ 'Winter',
                     Month == 3 ~ 'Spring',
                     Month == 6 ~ 'Summer',
                     Month == 9 ~ 'Fall'))


write.csv(precip_proj, 'Group_project/precip_proj_final.csv', row.names = F)

