

# data analysis for DO plot 

impdata <- read.csv(file = "impervious_full_water.csv", header = TRUE)

doimp <- impdata %>% select('Site', 'Stream', 'Date', 'do_percent_sat', 'Impervious')

dotest<- lm(data = doimp,do_percent_sat ~Impervious)

summary(dotest)

cols <- c('BB2' = 'red', 'BBS3' = 'orange', 'BDB1' = 'yellow', 'CC4' = 'green', 'CC5' = "blue", 'HC7' = 'purple', 'HSC6' = 'pink', 'LBC8' = 'cyan', 'MC10' = 'gold', 'MC9' = '#EEDD82', 'PC11' = '#191970', 'PHB12' = '#00FF7F', 'RB15' = '#C0FF3E', 'RC13' = '#8B4513', 'RC14' = '#CDC5BF', 'SC16' = '#FF6A6A', 'TC17' = 'white', 'WC18' = 'black')

#2008
do2008 <- impdata |> filter(year %in% c(2008, 2009, 2010))

do2008

ggplot() +
  geom_point(data= do2008, aes(x = Impervious, y =do_percent_sat, color = Site)) +
  scale_color_manual(values = cols) +
  theme_dark() 


#2011
do2011 <- impdata |> filter(year %in% c(2011, 2012))

ggplot() +
  geom_point(data= do2011, aes(x = Impervious, y =do_percent_sat, color = Site)) +
  scale_color_manual(values = cols) +
  theme_dark()

#2013
do2013 <- impdata |> filter(year %in% c(2013, 2014, 2015))

ggplot() +
  geom_point(data= do2013, aes(x = Impervious, y =do_percent_sat, color = Site)) +
  scale_color_manual(values = cols) +
  theme_dark()

#2016
do2016 <- impdata |> filter(year %in% c(2016, 2017, 2018))

ggplot() +
  geom_point(data= do2016, aes(x = Impervious, y =do_percent_sat, color = Site)) +
  scale_color_manual(values = cols) +
  theme_dark()

#2019
do2019 <- impdata |> filter(year %in% c(2019, 2020))

ggplot() +
  geom_point(data= do2019, aes(x = Impervious, y =do_percent_sat, color = Site)) +
  scale_color_manual(values = cols) +
  theme_dark()

#2021
do2021 <- impdata |> filter(year %in% c(2021,2022, 2023))

ggplot() +
  geom_point(data= do2021, aes(x = Impervious, y =do_percent_sat, color = Site)) +
  scale_color_manual(values = cols) +
  theme_dark()

#everything
ggplot() +
  geom_point(data= impdata, aes(x = Impervious, y =do_percent_sat, color = Site)) +
  scale_color_manual(values = cols) +
  theme_dark() +
  guides(color=guide_legend(ncol=2)) +
  theme(axis.text = element_text(size = 15))

sum





