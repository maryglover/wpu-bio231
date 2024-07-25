## Load in relevant packages

library(dplyr)
library(gglot2)

## Read in data

precip_proj <-
  read.csv('Group_project/precip_proj_final.csv')

turb_temporal_change <-
  read.csv('Group_project/turb_temporal_change.csv')

do_temporal_change <-
  read.csv('Group_project/do_temporal_change.csv')

##Bringing in stream data

streams <-
  st_read('data/Major_Rivers/')

streams <-
  st_transform(streams, crs = 4269)

## Seaparating lat and long out into their own data table so I can join them with stuff

coords <-
  precip_proj |>
  group_by(Site, Year) |>
  select(Site, Latitude, Longitude)

## Creating average score per site and year

score_avg <-
  precip_proj |>
  group_by(Site, Year) |>
  summarize(score_avg = mean(wq_score, na.rm = T))

## Joining score average and coords to map later

score_sites <-
  inner_join(score_avg, coords)

## Creating averages for year for each of the measurements

phos_avg <-
  precip_proj |>
  group_by(Site, Year) |>
  summarize(phos_avg = mean(Phosphorus_total_mg_L, na.rm = T))

e_coli_avg <-
  precip_proj |>
  group_by(Site, Year) |>
  summarize(e_coli_avg = mean(E_coli_MPN_100mL, na.rm = T))

do_avg <-
  precip_proj |>
  group_by(Site, Year) |>
  summarize(do_avg = mean(do_percent_sat, na.rm = T))

pH_avg <-
  precip_proj |>
  group_by(Site, Year) |>
  summarize(pH_avg = mean(pH, na.rm = T))

turb_avg <-
  precip_proj |>
  group_by(Site, Year) |>
  summarize(turb_avg = mean(Turbidity_NTU, na.rm = T))

nitrate_avg <-
  precip_proj |>
  group_by(Site, Year) |>
  summarize(nitrate_avg = mean(NO2_NO3_mg_L, na.rm = T))

## Graphing DO average
do_avg |>
  ggplot(aes(x = Site, y = do_avg)) +
  geom_boxplot(color = 'darkblue') +
  theme_minimal()

## Turb Separated by season

precip_proj |>
  ggplot(aes(x = factor(Season, level=c('Winter', 'Spring', 'Summer', 'Fall')), y = Turbidity_JTU, group = Season, color = Season)) +
  geom_boxplot() +
  scale_color_manual(values = c('darkmagenta', 'mediumspringgreen', 'orange', 'slateblue1')) +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 20)) + ylab("Turbidity (JTU)") + xlab("Season") +
  theme_minimal()

## Turb separated by year

precip_proj |>
  ggplot(aes(x = Year, y = Turbidity_JTU, group = Year, color = Season)) +
  geom_point(size = 1) +
  geom_point(stat = 'summary', fun = 'mean',
             color = 'black', fill = 'white',  shape = 24, size = 2) +
  scale_color_manual(values = c('darkmagenta', 'mediumspringgreen', 'orange', 'slateblue1')) +
  labs(y = 'Turbidity (JTU)') +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 20)) +
  theme_minimal()

## DO separated by season

precip_proj |>
  ggplot(aes(x = factor(Season, level=c('Winter', 'Spring', 'Summer', 'Fall')), y = do_percent_sat, group = Season, color = Season)) +
  geom_boxplot() +
  scale_color_manual(values = c('darkmagenta', 'mediumspringgreen', 'orange', 'slateblue1')) +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 20)) + ylab("Dissolved Oxygen Percent Saturation") + xlab("Season") +
  theme_minimal()

## DO separated by year

precip_proj |>
  ggplot(aes(x = Year, y = do_percent_sat, group = Year, color = Season)) +
  geom_point(size = 1) +
  geom_point(stat = 'summary', fun = 'mean',
             color = 'black', fill = 'white',  shape = 24, size = 2) +
  scale_color_manual(values = c('darkmagenta', 'mediumspringgreen', 'orange', 'slateblue1')) +
  labs(y = 'Dissolved Oxygen Percent Saturation') +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 20)) +
  theme_minimal()

## Creating map with streams separated out and colored based on wq score average

# Creating overall average score for sites
score_sites <-
  score_sites |>
  group_by(Site) |>
  mutate(overall_avg =
           mean(score_avg))

# Bringing in Wake county shape file
wake_line <-
  st_read('data/Wake_County_Line-shp/')
wake_line <-
  st_transform(wake_line, crs = 4269)

# Graphing using the overall average score
ggplot() +
  geom_sf(data = wake_line) +
  geom_sf(data = streams, color = 'navy') +
  geom_point(data = score_sites, aes(x = Longitude, y = Latitude, color = overall_avg), size = 3) +
  scale_color_gradient(low = 'red', high = 'green') +
  theme_minimal()

## Creating cutoffs for graph data

graph_data <- data.frame(stream_rating = c("excellent", "good", "fair", "poor"),
                         cutoff = c(18, 14, 10, 0))

## Site WQ score graph

site_wq_graph <- 
  ggplot(data = precip_proj, aes(x = Site, y = wq_score)) +
  geom_boxplot(color = 'navy') +
  geom_hline(data = filter(graph_data, stream_rating =='excellent'),
             aes(yintercept = cutoff, color = stream_rating)) +
  geom_hline(data = filter(graph_data, stream_rating =='good'),
             aes(yintercept = cutoff, color = stream_rating)) +
  theme_minimal() +
  labs(x = 'Site', y = 'Water Quality Score')

## Year WQ score graph

year_wq_graph <- ggplot(data = precip_proj, aes(x = as.factor(Year), y = wq_score)) +
  geom_boxplot(color = 'navy') +
  geom_hline(data = filter(graph_data, stream_rating =='excellent'),
             aes(yintercept = cutoff, color = stream_rating)) +
  geom_hline(data = filter(graph_data, stream_rating =='good'),
             aes(yintercept = cutoff, color = stream_rating)) +
  theme_minimal() +
  labs(x = 'Year', y = 'Water Quality Score')

site_wq_graph
year_wq_graph
