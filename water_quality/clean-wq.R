# clean up the water quality data from City of Raleigh
# data received from records request

library(dplyr)
library(stringr)
library(tidyr)

wq <- read.csv("data/raleigh_wq_2008_2023.csv")
summary(wq)

head(wq, 20)

# Make 0 if has a <
wq$Result <- ifelse(grepl("<", wq$Result), "0", wq$Result)

# if not detected, get a 0 instead of ND
wq.det <- wq |>
  mutate(Result = recode(Result, ND = '0')) 
wq.det$Result<-as.numeric(wq.det$Result)
summary(wq.det)

wq.det |>
  dplyr::select(-Unit, -PQL)|>
  pivot_wider(id_cols = c(Site, Date, Time), names_from = Parameter, values_from = Result)

wq.det %>%
  dplyr::group_by(Site, Date, Time, Parameter) %>%
  dplyr::summarise(n = dplyr::n()) %>%
  dplyr::filter(n > 1) 

# two rows for Salinity here
wq.det %>%
  filter(Site=='MC10', Time == "10:29")

#clean.wq <- wq.det |>
#  distinct() |> # get rid of one duplicate
#  dplyr::select(-Unit, -PQL) |>
#  pivot_wider(id_cols = c(Site, Date, Time), names_from = Parameter, #values_from = Result) 

#write.csv(clean.wq, "data/raleigh_wq_clean.csv", row.names = F)

#####  Units do match for parameter for DO
# view data rows where do doesn't match up
wq |>
  filter(Parameter == 'do_percent_sat' , Unit == 'mg/L')

# only issue with mismatch is in HC7. All do units reported as mg/L. 
wq |> 
  filter(Site == 'HC7') |>
  filter(grepl("do", Parameter)) |>
  separate(Date, into = c('year', 'month', 'day'), sep = '-')|>
  group_by(Parameter, Unit, year)|>
  tally() |>
  arrange(year)|>
  print(n=33)

#####  Units different for E coli
library(lubridate)
wq |> 
  mutate(Date = ymd(Date)) |>
  mutate(Year = year(Date))|>
  filter(Parameter == 'E_coli')|>
  group_by(Date, Site)|>
  filter(n > 1)

# never have more than 1 E_coli for 1 date, so only measured 1 unit each time, not three

wq |> 
  mutate(Date = ymd(Date)) |>
  mutate(Year = year(Date))|>
  filter(Parameter == 'E_coli')|>
  group_by(Year, Unit)|>
  summarize(n_na = sum(is.na(Result)), n = n())

# What is up with 2013
# Only have /100 after 2021 and 1/2 of 2020

wq |>
  filter(Unit %in% c('MPN/100mL', 'CFU/100mL'  ))

# Looks like on March 2 2021, measured E coli different. 
# but MPN and MPN/100mL are the sam


##### Add units in the name of the column

### fix the units
wq_units <- wq.det |>
  mutate(Unit = case_when(Parameter == 'do_percent_sat' ~ 'percent_sat', 
                          Unit == 'MPN' ~ 'MPN/100mL',
                          .default = Unit)) |>
  distinct() |> # get rid of one duplicate
  mutate(new_name = paste(Parameter, Unit, sep = '_')) |>
  select(-Unit, -PQL) |>
  pivot_wider(id_cols = c(Site, Date, Time), names_from = new_name, values_from = Result) |>
  rename(do_percent_sat = do_percent_sat_percent_sat, do_mg_L = `do_mgl_mg/L`, pH = pH_std_unit)

wq_units<- rename_with(wq_units, ~ gsub("/", '_', .x))
  
write.csv(wq_units, "data/raleigh_wq_clean-units.csv", row.names = F)

#example plot
library(ggplot2)

clean.wq %>%
  filter(!grepl("DUP",Site)) |> # gets rid of duplicate sites
  filter(!grepl("Dup",Site)) |>
  separate(Date, into = c('Year', 'Month', 'Day'), sep = '-')|>
  ggplot(aes(x = Temperature, y = E_coli)) +
  geom_point() 