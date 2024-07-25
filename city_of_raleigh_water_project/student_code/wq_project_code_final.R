# Water Quality Score Code 
library(dplyr)
colnames(water.edit)


#E.Coli 
water.edit |>
  select(Site, Date, E_coli_MPN_100mL) |>
  mutate(e_coli_score = 
           case_when(E_coli_MPN_100mL > 0 ~ '1', E_coli_MPN_100mL == 0 ~ '3', 
                     E_coli_CFU_100mL > 0 ~ '1', E_coli_CFU_100mL == 0 ~ '3'))

stream_codes <- read.csv('raleigh_wq_stream_codes.csv')


  





#Nitrate 
water.edit |>
  select(Site, Date, NO2_NO3_mg_L) |>
  mutate(nitrate_score = 
           case_when(NO2_NO3_mg_L == 0 ~ '3',
                     NO2_NO3_mg_L > 0 & NO2_NO3_mg_L <= 5 ~ '2',
                     NO2_NO3_mg_L > 5 ~ '1')) |>
  arrange(desc(nitrate_score))



# final Project data file
project_master_file <- read.csv("WQ_project/precip_proj.csv")

overall_stream_score <- project_master_file |>
  mutate(stream_rating = 
           case_when(wq_score >= 18 ~ 'excellent',
                     wq_score >= 14 & wq_score < 18 ~ 'good',
                     wq_score >= 10 & wq_score < 14 ~ 'fair', 
                   wq_score < 10 ~ 'poor')) 
library(dplyr)
library(ggplot2)
#precip & water quality 
ggplot(overall_stream_score, aes(x = wq_score, y = precip)) + geom_point() + facet_wrap(~ Stream) +
  geom_abline(color = 'red')
precip_lm <- lm(data = overall_stream_score, precip ~ wq_score)
summary(precip_lm)

#exploratory into precip/e_coli 
overall_stream_score |>
  select(Stream, E_coli_MPN_100mL, precip, stream_rating) |>
  arrange(desc(E_coli_MPN_100mL))
ggplot(overall_stream_score, aes(x = precip, y = E_coli_MPN_100mL)) + geom_point() +
  facet_wrap(~ Stream) +
  geom_abline(intercept = E_coli_precip_lm$coeff[1], slope = E_coli_precip_lm$coeff[2], color = 'blue')


#LM e_coli + precip model
E_coli_precip_lm <- lm(data = overall_stream_score, E_coli_MPN_100mL ~ precip)
summary(E_coli_precip_lm)
E_coli_precip_lm$coefficients


# E_coli & Turbidity 
overall_stream_score |>
  select(Stream, E_coli_MPN_100mL, precip, Date, Time, Turbidity_NTU, stream_rating) |>
  arrange(desc(E_coli_MPN_100mL)) |>
  filter(Date == '2012-03-20')

#graph ecoli turb
ggplot(data = overall_stream_score, aes( x = Turbidity_NTU, y = E_coli_MPN_100mL)) +
  geom_point() + geom_abline(intercept = e_coli_turb_lm$coeff[1], slope = e_coli_turb_lm$coeff[2], color = 'blue') + facet_wrap(~ Stream)
e_coli_turb_lm <- lm(data = overall_stream_score, E_coli_MPN_100mL ~ Turbidity_NTU)
summary(e_coli_turb_lm)

#e_coli + date 
e_coli_date_lm <- lm(data = overall_stream_score, E_coli_MPN_100mL ~ Date)
summary(e_coli_date_lm)

ggplot(data = overall_stream_score, aes(x = Date, y = E_coli_MPN_100mL)) + facet_wrap(~ Stream) + 
  geom_point() + geom_abline(intercept = e_coli_date_lm$coeff[1], slope = e_coli_date_lm$coeff[2], color = 'blue')



#graphing on ecoli
ggplot(data = overall_stream_score, aes(x = Date, y = E_coli_MPN_100mL)) + geom_point() + facet_wrap(~ Stream) +geom_abline(color = 'blue')

# nitrate and precip
nitrate_precip_lm <- lm(data = overall_stream_score, NO2_NO3_mg_L ~ precip)
summary(nitrate_precip_lm)


#nitrate+precip table
overall_stream_score |>
  select(Date, Stream, NO2_NO3_mg_L, precip, wq_score, stream_rating) |>
  arrange(desc(NO2_NO3_mg_L))


#graphing nitrate
ggplot(data = overall_stream_score, aes(x = precip, y = NO2_NO3_mg_L)) + geom_point() +
  facet_wrap(~ Stream) + geom_abline(intercept = nitrate_precip_lm$coeff[1], slope = nitrate_precip_lm$coeff[2], color = 'blue')


# separating date 
overall_stream_score <-
  overall_stream_score |>
  separate(Date, into = c('Year', 'Month', 'Day'), sep = '-')

#season column 
overall_stream_score <-
  overall_stream_score |>
  mutate(Season =
           case_when(Month == 12 ~ 'Winter',
                     Month == 01 ~ 'Winter',
                     Month == 03 ~ 'Spring',
                     Month == 06 ~ 'Summer',
                     Month == 09 ~ 'Fall'))

#averages
e_coli_avg <-
  overall_stream_score |>
  group_by(Stream, Year, Season) |>
  summarize(overall_stream_score = mean(E_coli_MPN_100mL, na.rm = T))

#graphing ecoli over time 
overall_stream_score |>
  select(Stream, Year, Month, Season, E_coli_MPN_100mL, NO2_NO3_mg_L, wq_score) |>
  group_by(Season)
ggplot(data = e_coli_avg, aes(x = Year, y = overall_stream_score, group = Year, color = Season)) +
          geom_point() + geom_point(size = .8) +
  scale_color_manual(values = c('darkmagenta', 'mediumspringgreen', 'orange', 'slateblue1')) +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 20)) + theme_minimal() + geom_smooth(aes(group = 1), method = "lm", linewidth = 1, se = FALSE, linetype = 2, color = "black") +
  ylab("E.Coli MPN/100mL")

#graphing season trends e_coli 
ggplot(data = e_coli_avg, aes(x = factor(Season, level=c('Winter', 'Spring', 'Summer', 'Fall')), y = overall_stream_score, group = Season, color = Season)) +
  geom_point() + geom_point(size = .8) +
  scale_color_manual(values = c('darkmagenta', 'mediumspringgreen', 'orange', 'slateblue1')) +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 20)) + ylab("E.Coli MPN/100mL") + xlab("Season") +
  theme_minimal()


#graphing Nitrate
nitrate_avg <-
  overall_stream_score |>
  group_by(Site, Year, Season) |>
  summarize(nitrate_avg = mean(NO2_NO3_mg_L, na.rm = T))

#Nitrate yearly change
ggplot(data = overall_stream_score, aes(x = Year, y = NO2_NO3_mg_L, group = Year, color = Season)) +
  geom_point() + geom_point(size = .8) +
  scale_color_manual(values = c('darkmagenta', 'mediumspringgreen', 'orange', 'slateblue1')) +
  theme(axis.text = element_text(size =18),
        axis.title = element_text(size = 20)) + theme_minimal() + 
  geom_smooth(aes(group = 1), method = "lm", linewidth = 1, se = FALSE, linetype = 2,
              color = "black") + ylab("Nitrate mg/L")

#Nitrate seasonal
ggplot(data = nitrate_avg, aes(x = factor(Season, level=c('Winter', 'Spring', 'Summer', 'Fall')), y = nitrate_avg, group = Season, color = Season)) +
  geom_point() + geom_point(size = .8) +
  scale_color_manual(values = c('darkmagenta', 'mediumspringgreen', 'orange', 'slateblue1')) +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 20)) + ylab("Nitrate mg/L") + xlab("Season") +
  theme_minimal()

e_coli_avg |> 
  filter(Year == 2008) |>
  summary()
e_coli_avg |>
  filter(Year == 2023) |>
  summary()

nitrate_avg |> 
  filter(Year == 2008) |>
  summary()
nitrate_avg |> 
  filter(Year == 2023) |>
  summary()

overall_stream_score |> 
  select(Stream, E_coli_MPN_100mL, Year, Month, Day) |> 
  arrange(desc(E_coli_MPN_100mL))

nitrate_avg |> 
  arrange(desc(nitrate_avg))

overall_stream_score |> 
  select(Stream, NO2_NO3_mg_L, Year, Month, Day) |> 
  arrange(desc(NO2_NO3_mg_L))

overall_stream_score |> 
  select(Stream, Date, NO2_NO3_mg_L) |>
  filter(Date == 2012-09-19)
