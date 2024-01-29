ggplot(data = climate, aes(x = year, y = tmax)) +
  geom_bar(position='dodge', stat='summary', fun='mean')

climate |>
  group_by(year) |>
  summarize(max_temp = mean(tmax)) |>
  ggplot(aes(x = year, y = max_temp)) +
  geom_bar(stat = 'identity') +
  theme_bw()

climate |>
  group_by(year) |>
  summarize(max_temp = mean(tmax), sd_temp = sd(tmax)) |>
  ggplot(aes(x = year, y = max_temp)) +
  geom_point()+
  geom_errorbar(aes(ymin = max_temp - sd_temp, ymax = max_temp + sd_temp))


climate |>
  group_by(year) |>
  summarize(max_temp = mean(tmax), sd_temp = sd(tmax)) |>
  ggplot(aes(x = year, y = max_temp)) +
  geom_line(size = 2, color = 'red')+
  geom_point(data = climate, aes(x = year, y = tmax), color = 'gray', alpha = .5) +
  theme_bw()


### fly example
fly <- read.csv('~/Dropbox/M/Rhagoletis/SDM/clean-data/recent_collections_2012-2019.csv')
summary(fly)

# box plot
ggplot(fly, aes(x = species, y = emerge_mean, fill = species)) +
  geom_boxplot() + 
  theme_classic() +
  labs(x = 'Species', y= 'Days to emergence', title = 'Fly emergence') +theme(text = element_text(size=20))


# bar plots
ggplot(filter(fly, Coll.year == '2013'), aes(x = species, y = emerge_mean, fill = species)) +
  geom_bar(position= 'dodge', stat = 'summary', fun.y='mean') + 
  theme_classic() +
  labs(x = 'Species', y= 'Days to emergence', title = 'Fly emergence')+theme(text = element_text(size=20))

fly |>
  group_by(species) |>
  summarize(emerge = mean(emerge_mean, na.rm=T), sd = mean(emerge_sd, na.rm= T)) |>
  ggplot(aes(x = species, y = emerge)) +
  geom_bar(stat='identity', fill= 'gray')+
  geom_errorbar(aes(ymin = emerge - sd, ymax = emerge + sd))+ 
  theme_classic() +
  labs(x = 'Species', y= 'Days to emergence', title = 'Fly emergence')+theme(text = element_text(size=20))




ggplot(fly, aes(x = species, y = emerge_mean, fill = species)) +
  geom_boxplot() + 
  geom_point(alpha = .5, position = 'jitter')+
  theme_classic()+
  labs(x = 'Species', y= 'Days to emergence', title = 'Fly emergence') +theme(text = element_text(size=20))


coll_summary <- fly |>
  group_by(Coll.year, species) |>
  summarize(n = sum(N)) 

ggplot(coll_summary, aes(x = Coll.year, y = n )) +
  geom_bar(stat = 'identity')+
  theme_classic() +
  labs(x = 'Year', y= 'Number flies collected', title = 'Fly collections')+theme(text = element_text(size=20))



ggplot(coll_summary, aes(x = Coll.year, y = n, fill = species)) +
  geom_bar(stat = 'identity')+
  theme_classic() +
  labs(x = 'Year', y= 'Number flies collected', title = 'Fly collections')+theme(text = element_text(size=20))

coll_summary |>
  group_by(Coll.year) |>
  mutate(prop = n / sum(n)) |>
  ggplot(aes(x = Coll.year, y = prop, fill = species)) +
  geom_bar(stat = 'identity') +
  theme_classic() +
  labs(x = 'Year', y= 'Proportion flies collected', title = 'Fly collections')+theme(text = element_text(size=20))

# scatterplot
ggplot(fly, aes(x = ow, y = emerge_mean, color = species)) +
  geom_point(size = 2) +
  theme_classic()+
  labs(x = 'Months overwintered', y= 'Days to emergence', title = 'Fly emergence') +
  theme_classic() +theme(text = element_text(size=20))


# line graph
fly |>
  group_by(Coll.year) |>
  summarize(n = sum(N))|>
  arrange(Coll.year) |>
  mutate(n = ifelse(is.na(n), 0, n)) |>
  mutate(cumm_sum = cumsum(n))|>
  ggplot(aes(x = Coll.year, y = cumm_sum)) +
  geom_point() +
  geom_line() +
  theme_classic() +
  labs(x = 'Year', y= 'Number flies collected', title = 'Fly collections')+theme(text = element_text(size=20))

fly |>
  group_by(Coll.year, species) |>
  summarize(n = sum(N))|>
  group_by(species) |>
  mutate(cumm_sum = cumsum(n))|>
  ggplot(aes(x = Coll.year, y = cumm_sum, color = species)) +
  geom_point() +
  geom_line() +
  theme_classic() +
  labs(x = 'Year', y= 'Number flies collected', title = 'Fly collections')+theme(text = element_text(size=20))

fly |>
  group_by(species) |>
  summarize(n = sum(N, na.rm = T)) |>
ggplot(aes(x="", y=n, fill=species)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) + theme_void()

# histograms
ggplot(fly, aes(x = emerge_mean)) +
  geom_histogram(fill = 'darkgray', color = 'black') +
  xlab('Fly emergence')+
  theme_classic() +theme(text = element_text(size=20))

ggplot(fly, aes(x = emerge_mean)) +
  geom_histogram(fill = 'darkgray', color = 'black') +
  xlab('Fly emergence')+
  theme_classic() +theme(text = element_text(size=20))
