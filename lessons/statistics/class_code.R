climate <- read.csv('data/raleigh_prism_climate.csv')

summary(climate)

library(ggplot2)

ggplot(data = climate, aes(x = tmean, y = precip)) + 
  geom_point()

precip_lm <- lm(data = climate, precip ~ tmean)

summary(precip_lm)

precip_lm$coefficients
precip_lm$coef[2]

ggplot(data = climate, aes(x = tmean, y = precip)) +
  geom_point() +
  geom_abline(intercept = precip_lm$coef[1], 
              slope = precip_lm$coef[2], 
              color = 'red')

ggplot(data = climate, aes(x = tmean, y = precip)) +
  geom_point()+
  geom_smooth()

library(dplyr)

climate_season <- climate |>
  mutate(season = case_when((month %in% c(12, 1, 2)) ~ 'winter', 
                            (month %in% c(3, 4, 5)) ~ 'spring',
                            (month %in% c(6, 7, 8) ~ 'summer'), 
                            (month %in% c(9, 10, 11) ~ 'fall')))


ggplot(data = climate_season, aes(x = season, y = tmean)) +
  geom_boxplot()

season_lm <- lm(data = climate_season, 
                tmean ~ season)
summary(season_lm)

stat_example <- read.csv('data/stats_example.csv')

stats_lm <- lm(data= stat_example, Test_score ~ Hours_TV)

summary(stats_lm)
