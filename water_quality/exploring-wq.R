summary(lm(tmean ~ as.factor(month), data = climate))
summary(clean.wq)

wq_dupsrm<- clean.wq |>
  filter(!grepl("DUP",Site)) |> # gets rid of duplicate sites
  filter(!grepl("Dup",Site))


ggplot(wq_dupsrm, aes(x = Site, y = Phosphorus_total)) +
  geom_boxplot()
  

wq_dupsrm %>%
  separate(Date, into = c('Year', 'Month', 'Day'), sep = '-')|>
  ggplot(aes(x = Temperature, y = do_percent_sat)) +
  geom_point() +
  geom_smooth(method = 'lm')

wq_dupsrm %>%
  separate(Date, into = c('Year', 'Month', 'Day'), sep = '-')|>
  ggplot(aes(x = Temperature, y = do_percent_sat)) +
  geom_point(es(color=Site)) +
  geom_smooth(method = 'lm', aes(color = Site))


wq_dupsrm %>%
  separate(Date, into = c('Year', 'Month', 'Day'), sep = '-') |>
  filter(Year == 2023) |>
  ggplot(aes(x = Site, y = Nitrogen_total)) +
  geom_boxplot()

ggplot(wq_dupsrm, aes(x = Nitrogen_total, y = Phosphorus_total)) +
  geom_point()

wq_dupsrm$E_coli |> max(na.rm = T)

wq_out <- wq_dupsrm |>
 mutate(E_coli = gsub('155310', NA, E_coli))
  
wq_out$E_coli<- as.numeric(as.character(wq_out$E_coli))

max(wq_dupsrm$E_coli, na.rm = T)
max(wq_out$E_coli, na.rm = T)

ggplot(wq_out, aes(x = Site, y = E_coli)) +
  geom_boxplot(aes(group = Site))

wq_out <- wq_out |> 
  separate(Date, into = c('Year', 'Month', 'Day'), sep = '-') 

ggplot(wq_out, aes(x= Year, y = TKN)) +
  geom_point() +
  facet_wrap(~Site)+
  geom_smooth(method = 'lm', na.rm=T, aes(group = Site))


mean_tkn <- wq_out |>
  group_by(Year) |>
  summarize(tkn = mean(TKN, na.rm=T))
  
ggplot(wq_out, aes(x = Site, y = TKN, fill = Site)) +
  geom_boxplot() +
  facet_wrap(~Year) +
  geom_hline(aes(yintercept = tkn),data = mean_tkn,  color = 'blue')
  
