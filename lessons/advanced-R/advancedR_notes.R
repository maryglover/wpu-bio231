# lesson for advanced R

library(dplyr)
library(lubridate)

water <- read.csv('data/raleigh_wq_clean-units.csv')
summary(water)
head(water)

# go over the code for cleaning up the data frame

# dates
# show separate and then the date
# dates as characters 
water <- water |>
  mutate(Date = ymd(Date))

unique(water$Site)
ggplot(filter(water, Site == 'PHB12'), aes(x = Date, y = Copper_mg_L)) +
  geom_point()+
  geom_line()

water |>
  filter(Site == 'PHB12', !is.na(Copper_mg_L)) |>
  select(Date, Copper_mg_L) |>
  ggplot(aes(x = Date, y = Copper_mg_L)) +
  geom_line()+
  geom_point()


# missing data
# what is missing data
temp_fun <- function(x){
  sum(!is.na(x))
}
water |>
  filter(!grepl('DUP', Site, ignore.case = T)) |>
  separate(Date, into = c('year', 'month', 'day'), sep = '-')|>
  group_by( year) |>
  summarise_all(temp_fun) |>
  select( year, do_percent_sat, Copper_mg_L, Nitrogen_total_mg_L)

water |>
  separate(Date, into = c('year', 'month', 'day'), sep = '-')|>
  filter(year == 2018)

# joins


## part of the exercise
# use separate to on the stream codes to get column for lat and column for long
precip <- read.csv("data/precip_stream_sites.csv")
summary(precip)
