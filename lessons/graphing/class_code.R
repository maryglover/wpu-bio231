library(ggplot2)

climate <- read.csv('data/raleigh_prism_climate.csv')

head(climate)

ggplot(data = climate, aes(x = precip)) +
  geom_histogram(bins = 50)

ggplot(data = climate, aes(x = month, y = tmean)) +
  geom_point() +
  geom_line(aes(group = year))

ggplot(data = climate, aes(x = month, y = tmean)) +
  geom_boxplot(aes(group = month))

library(dplyr)

month_temp <- climate |>
  group_by(month) |>
  summarize(mean_temp = mean(tmean), sd_temp = sd(tmean))

ggplot(data = month_temp, 
       aes(x = month, y =mean_temp)) +
  geom_point() +
  geom_line()

climate |>
  filter(month == 9) |>
  ggplot(aes(y = tmax, x = year)) +
  geom_point()


tmean_month_plot <- ggplot(data = month_temp, 
       aes(x = month, y =mean_temp)) +
  geom_point() +
  geom_line() +
  labs(x = 'Month', y = 'Average temperature', title = 'Average Tempeature in Raleigh')

tmean_month_plot +
  scale_x_continuous(breaks = 1:12, labels = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')) +
  theme_classic()


ggplot(data = month_temp, 
       aes(x = month, y =mean_temp)) +
  geom_line(color = 'cornflowerblue') +
  geom_point(color = 'darkblue', shape = '*') 

ggplot(month_temp, aes(x=month, y=mean_temp)) +
  geom_bar(stat = 'identity', color = 'darkblue', fill = 'lightblue')


climate_decade <- climate |>
  filter(year %in% c(1983, 1993, 2003, 2013, 2023))

climate_decade <-ggplot(data = climate_decade, aes(x = month, y = tmean, color = as.factor(year))) +
  geom_point() +
  geom_line(aes(group = year)) +
  scale_color_manual(values = c('red', 'blue', 'green', 'orange', 'purple')) +
  labs(color = 'Year') +
  facet_wrap(~year)

ggsave(climate_decade, file = 'climate_plot.jpeg')
