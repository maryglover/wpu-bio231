library(dplyr)

precip <- read.csv('data/precip_stream_sites.csv')

wq <- read.csv('data/raleigh_wq_edit.csv')

head(precip)

library(zoo)

precip_sum <- precip |>
  group_by(Site, Stream) |>
  arrange(Date) |>
  mutate(sum_prev5 = rollsum(precip, k = 5, align = 'right', na.pad = TRUE))

inner_join(precip_sum, wq)
