# Water quality data
# From wqp data: https://www.waterqualitydata.us/
# for zip code 27603

wqp <- read.csv('data/wqp-wake.csv')

summary(wqp)

library(tidyverse)
wqp <- wqp |>
  discard(~all(is.na(.) | . =="")) 

wqp |>
  group_by(ProjectName) |>
  summarize(n = n())

wqp |>
  filter(ProjectName == 'National Rivers and Streams Assessment 2018-2019')|>
  discard(~all(is.na(.) | . ==""))|>
  select(ActivityMediaName, ProjectName, MonitoringLocationName, CharacteristicName, ResultMeasureValue,ResultMeasure.MeasureUnitCode ) |> 
  group_by(CharacteristicName) %>%
  summarize(n = n())

# National Rivers and Streams has both biological and chemcial


wqp |>
  filter(ProjectName == "NC Ambient Monitoring System - MODERN")|>
  discard(~all(is.na(.) | . ==""))|>
  select(ActivityMediaName, ProjectName, MonitoringLocationName, CharacteristicName, ResultMeasureValue,ResultMeasure.MeasureUnitCode ) |> 
  group_by(SampleCollectionMethod.MethodIdentifier) %>%
  summarize(n = n())

# NC Ambient Monitoring System - MODERN has 5 locations with 20 different chemical tests. 

clean_cols <- wqp |>
  separate(ActivityStartDate, into = c('Year', 'Month', 'Day'), sep = '-') |> 
  select(Year, Media = ActivityMediaName, Project = ProjectName, Location = MonitoringLocationName, Latitude = ActivityLocation.LatitudeMeasure, Longitude = ActivityLocation.LongitudeMeasure, Detection = ResultDetectionConditionText, Characteristic = CharacteristicName, Result = ResultMeasureValue, Result_Unit = ResultMeasure.MeasureUnitCode, Comment = ResultCommentText, Taxonomy = SubjectTaxonomicName, Detection_Measure = DetectionQuantitationLimitMeasure.MeasureValue, Detection_Unit = DetectionQuantitationLimitMeasure.MeasureUnitCode) 
wake_bio <- clean_cols |>
  filter(Media == 'Biological') |>
  select(-Detection_Measure, - Detection_Unit, -Detection)

water_chem <- clean_cols |>
  filter(Media == 'Water') |> 
   mutate(Result = replace(Result, Detection == 'Not Detected', 0)) |>
  select(-Detection, - Detection_Measure, -Detection_Unit)

water_chem$Result <- as.numeric(water_chem$Result)

water_chem |>
  filter(Characteristic == "Dissolved oxygen (DO)") |>
  group_by(Location, Year) |> 
  summarize(mean.DO = mean(Result))

write.csv(water_chem, 'data/water_chem_raleigh_wqp.csv', row.names = F)
write.csv(wake_bio, 'data/bio_assess_raleigh_wqp.csv', row.names = F)
