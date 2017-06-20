pacman::p_load("ggplot2")
pacman::p_load("stargazer")
pacman::p_load("CBPS")
pacman::p_load("scales")
pacman::p_load("cobalt")
pacman::p_load("readr")
pacman::p_load("gridExtra")
pacman::p_load("grid")
pacman::p_load("readxl")
pacman::p_load("dplyr")

iso4all <- read_csv("iso4all.csv")

###Plots
fsm.tt5.plot <- ggplot(iso4all, aes(y=ac.gcse.fsm.3y, x=avg5.tt.nid)) +
  geom_point(shape=20, size=3) + 
  stat_smooth(se=FALSE, fullrange=TRUE, colour="Red", size=1) +
  labs(x= "Average travel time (in minutes) to five nearest secondary schools") + 
  labs(y= "Ever6FSM students at 5+ A*-C GCSE(EM)") + 
  scale_y_continuous(breaks=pretty_breaks(n=11), labels = scales::percent_format()) +
  scale_x_continuous(breaks=pretty_breaks(n=12)) +
  theme(axis.text.y = element_text(size=14, colour = "black"),
        axis.text.x = element_text(size=14, colour = "black")) + 
ggtitle(expression(paste("Disadvantaged Pupils, 2013-2015 5+ A*-C GCSE(EM), from ", italic("Lonely Schools")))) 


all.tt5.plot <- ggplot(iso4all, aes(y=ac.gcse.all.3y, x=avg5.tt.nid))  +
  geom_point(shape=20, size=3) + 
  geom_smooth(se=FALSE, fullrange=TRUE, colour="Blue", size=1) +
  labs(x= "Average travel time (in minutes) to five nearest secondary schools") + 
  labs(y= "All students at 5+ A*-C GCSE(EM)") + 
  scale_y_continuous(breaks=pretty_breaks(n=11), labels = scales::percent_format()) +
  scale_x_continuous(breaks=pretty_breaks(n=12)) +
  theme(axis.text.y = element_text(size=14, colour = "black"),
        axis.text.x = element_text(size=14, colour = "black")) + 
  ggtitle(expression(paste("All Pupils, 2013-2015 5+ A*-C GCSE(EM), from ", italic("Lonely Schools"))))

all.tt5.plot

# for blog ----------------------------------------------------------------



ks4_2016 <- read_excel("~/Documents/GitHub/lonely-schools/england_ks4final2016.xlsx")

trip_time <- read_excel("~/Documents/GitHub/lonely-schools/trip_time.xlsx")

ks4_2016 <- left_join(ks4_2016, trip_time)

### Plots -------------------
fsm.tt5.plot_2016 <- ggplot(ks4_2016, aes(y=attainment8_ever6fsm, x=avg5.tt.nid)) +
  geom_point(shape=20, size=3) + 
  stat_smooth(se=FALSE, fullrange=TRUE, colour="Red", size=1) +
  labs(x= "Average travel time (in minutes) to five nearest secondary schools") + 
  labs(y= "Average Attainment 8 Point Score") +
  scale_y_continuous(breaks=pretty_breaks(n=10)) +
  scale_x_continuous(breaks=pretty_breaks(n=12)) +
  theme(axis.text.y = element_text(size=14, colour = "black"),
        axis.text.x = element_text(size=14, colour = "black")) + 
  ggtitle("Disadvantaged Pupils, 2016 Attainment 8 Score")

all.tt5.plot_2016 <- ggplot(ks4_2016, aes(y=attainment8_all, x=avg5.tt.nid)) +
  geom_point(shape=20, size=3) + 
  geom_smooth(se=FALSE, fullrange=TRUE, colour="Blue", size=1) +
  labs(x= "Average travel time (in minutes) to five nearest secondary schools") + 
  labs(y= "Average Attainment 8 Point Score") +
  scale_y_continuous(breaks=pretty_breaks(n=10)) +
  scale_x_continuous(breaks=pretty_breaks(n=12)) +
  theme(axis.text.y = element_text(size=14, colour = "black"),
        axis.text.x = element_text(size=14, colour = "black")) + 
  ggtitle("All Pupils, 2016 Attainment 8 Score")

all.tt5.plot_2016

combined_lonely <- grid.arrange(all.tt5.plot, fsm.tt5.plot, ncol=2)

combined_2016 <- grid.arrange(all.tt5.plot_2016, fsm.tt5.plot_2016, ncol=2)


ggsave(plot=combined_lonely, filename="combined_lonely.png", type = "cairo-png", dpi = 500, width = 40, height = 20, units = "cm")

ggsave(plot=combined_2016, filename="combined_2016.png", type = "cairo-png", dpi = 500, width = 40, height = 20, units = "cm")

combined <- grid.arrange(all.tt5.plot, fsm.tt5.plot, all.tt5.plot_2016, fsm.tt5.plot_2016, ncol=2, nrow=2, bottom = textGrob("Evan Odell, 10.1080/00131881.2017.1339285", x = 0.98, hjust = 1.05, gp = gpar(fontface = 3L, fontsize = 9)))

combined

ggsave(plot=combined, filename="combined.png", type = "quartz", dpi = 500, width = 40, height = 20, units = "cm")

