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

# joins




