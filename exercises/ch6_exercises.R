##### 1 Location of Pitches for Left and Right-Handed Batters
load("DataFiles/balls_strikes_count.RData")
library(lattice)

densityplot(~speed | batter_hand, data=verlander, groups=pitch_type, plot.points=FALSE, auto.key = TRUE)

##### 2 Comparing Pitch Location for Two Pitchers
library(ggplot2)
library(dplyr)

verlander_fb <- subset(verlander, pitch_type =='FF')
sanchez_fb <- subset(sanchez, pitch_type =='FF')

verlander_fb$pitcher <- 'Verlander'
sanchez_fb$pitcher <- 'Sanchez'

verlander_fb = verlander_fb[c('pitches','speed','px','pz','batter_hand','pitcher')]
sanchez_fb = sanchez_fb[c('pitches','speed','px','pz','batter_hand','pitcher')]

fastball_comp <- rbind(verlander_fb, sanchez_fb)

ggplot(data=fastball_comp, aes(x=pitches, y=speed)) +
  facet_wrap(~pitcher) +
  geom_point()

##### 3 Graphical View of the Speeds of Justin Verlander's Fastballs
#3a)
verlander_fb$groups <- cut(verlander_fb$pitches, breaks=10)

#3b)
ggplot(data=verlander_fb, aes(speed, groups)) +
  geom_boxplot()

##### 4 Exploring Miguel Cabrera's Slugging Power
#4a)
cabrera_hit <- subset(cabrera, hit_outcome=='H')

#4b)
cabrera_hit$distance <- sqrt(with(cabrera_hit, hitx ** 2 + hity ** 2))

#4c)
cabrera_hit$gameDay <- as.integer((format(cabrera_hit$gamedate, format="%j")))

#4d)

ggplot(data=cabrera_hit, aes(gameDay, distance)) +
  geom_smooth()

#Dip around game 225, but at its heights around games 125 and ending the season.