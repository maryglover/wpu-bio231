# Data manipulation class exercise

yahtzee <- read.csv('data/yahtzee-example.csv')
yahtzee
rownames(yahtzee) <- c("ones", "twos", "" )

vinny <- sum(yahtzee$Vinny)
vinny
mary <- sum(yahtzee$Mary)
mary
quentin <- sum(yahtzee$Quentin)
quentin
yahtzee$Vinny
sum(yahtzee$Vinny[1:6])
vinny <- vinny + 35

sum(yahtzee$Mary[1:6])

install.packages('dplyr')
library(dplyr)


max(vinny, mary, quentin)

climate <- read.csv("data/raleigh_prism_climate.csv")
climate

head(climate)

climate |> 
  head()

climate |>
  filter(year > 2020, month == 1)

climate |>
  filter(year == 2023 | year == 1985)

climate |>
  filter(year %in% c(1991, 1993, 1995))

climate |>
  filter(year %in% 1990:1999)
  
climate2023 <- climate |>
  filter(year == 2023)

climate2023

climate |>
  select(precip, year, month) |>
  head()

climate |>
  select(-precip) |>
  head()

climate |>
  select(year, month, precipitation = precip ) |>
  head()

climate |> 
  filter(month == 1 ) |>
  select(year, tmax)

climate |>
  mutate(temp_difference = tmax - tmin) |>
  arrange(temp_difference)|>
  head()

climate |>
  mutate(temp_difference = tmax - tmin) |>
  arrange(desc(temp_difference))|>
  head()

head(climate)

