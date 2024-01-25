# clean up the water quality data from City of Raleigh
library(dplyr)
library(stringr)
library(tidyr)

wq <- read.csv("data/raleigh_wq_2008_2023.csv")
summary(wq)

# Make 0 if has a <
wq$Result <- ifelse(grepl("<", wq$Result), "0", wq$Result)

# if not detected, get a 0 instead of ND
wq.det <- wq |>
  mutate(Result = recode(Result, ND = '0')) 

wq.det$Result<-as.numeric(wq.det$Result)

wq.det |>
  select(-Unit, -PQL)|>
  pivot_wider(id_cols = c(Site, Date, Time), names_from = Parameter, values_from = Result)

wq.det %>%
  dplyr::group_by(Site, Date, Time, Parameter) %>%
  dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
  dplyr::filter(n > 1L) 

# two rows for Salinity here
wq.det %>%
  filter(Site=='MC10', Time == "10:29")

nrow(wq.det)

clean.wq <- wq.det |>
  distinct() |> # get rid of one duplicate
  select(-Unit, -PQL) |>
  pivot_wider(id_cols = c(Site, Date, Time), names_from = Parameter, values_from = Result) 

write.csv(clean.wq, "data/raleigh_wq_clean.csv", row.names = F)

clean.wq %>%
  group_by(Site)|>
  filter(!grepl("DUP",Site)) |> # gets rid of duplicate sites
  filter(!grepl("Dup",Site)) |>
 distinct(Site) 

