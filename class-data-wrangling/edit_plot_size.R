wq <- read.csv('data/raleigh_wq_edit.csv')

library(dplyr)
library(ggplot2)

 ggplot(data = wq, aes(x = Turbidity_NTU, y = do_percent_sat)) +
  geom_point(size = 3, aes(color = Site)) +
   labs(x = 'Turbidity (NTU)', y = 'Dissolved Oxygen (% sat)')+
   theme_bw()+
   theme(axis.text = element_text(size = 18), 
         axis.title = element_text(size = 20)) +
  guides(color=guide_legend(ncol=2))
  # theme(legend.position="bottom")

 ggsave(filename = 'plots/base_plot.png', width = 6, height = 6, units = 'in')
 
 
library(ggplot2)

wq <- read.csv('data/raleigh_wq_edit.csv')
 
ggplot(data = wq, aes(x = Site, y = do_percent_sat)) +
   geom_point() +
   geom_point(stat = 'summary', fun = 'mean',
              fill = 'red',  shape = 24, size = 2)
 

 