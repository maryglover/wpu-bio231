x <- c(1, 5, NA, 6, 3)
max(x, na.rm= T)

water <- read.csv("data/raleigh_wq_clean-units.csv")
library(dplyr)
water |>
  filter(Site == 'PHB12', Date == '2023-12-07')

sum(is.na(water$Salinity_ppt))

water |>
  group_by(Site) |>
  summarize(n_NA = sum(is.na(Salinity_ppt)), n =n())

water |>
  filter(is.na(Temperature_C)) |>
  select(Site, Date, Temperature_C)


height <- c(61, 69, 65, 72, 620, 67, 70, 71, 74, 68)
height
recode(height, `620` = 62)
na_if(height, 620)

water |>
  arrange(desc(E_coli_MPN_100mL)) |> 
  select(Site, Date, E_coli_MPN_100mL) |>
  head() 

water |>
  mutate(E_coli_MPN_100mL = na_if(E_coli_MPN_100mL, 155310)) |>
  arrange(desc(E_coli_MPN_100mL)) |> 
  select(Site, Date, E_coli_MPN_100mL) |>
  head() 

distinct(water, Site)

water |>
  filter(!grepl('DUP', Site, ignore.case = T)) |>
  select(Site)

water_edit <- water |>
  mutate(E_coli_MPN_100mL = na_if(E_coli_MPN_100mL, 155310)) |>
  filter(!grepl('DUP', Site, ignore.case = T))

write.csv(water_edit, "raleigh_wq_edit.csv")

head(water)

library(tidyr)
water |>
  separate(Date, into = c('year', 'month', 'day'), sep ='-', remove = F ) |>
  head()

library(lubridate)
water_edit <- water_edit |>
  mutate(Date = ymd(Date)) |>
  head()
water_edit |>
  mutate(year = year(Date))|>
  head()
