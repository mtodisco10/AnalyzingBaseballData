##### 1 Run Value of Individual Pitches
#a) 
pbp2011 <- read.csv('DataFiles/retrosheets/events/all2011.csv')
header <- read.csv('DataFiles/retrosheets/events/fields.csv')
names(pbp2011) <- names(header)

pbp2011rc <- read.csv('https://raw.githubusercontent.com/maxtoki/baseball_R/master/data/pbp11rc.csv')
head(pbp2011rc)

##### 2 Length of Plate Appearances
#a) 
library(dplyr)
#removing all non-pitches from the pitch sequence using regular expression
pbp2011$pseq <- gsub("[.>123N+*]", "", pbp2011$pitch_sequence)
#pbp2011$ab_length <- apply(pbp2011[, c('pseq')], 2, nchar)
pbp2011 <- mutate(pbp2011, ab_length = nchar(pseq))

pbp2011 %>%
  group_by(lineup_position) %>%
  summarise(avg_ab_length = mean(ab_length, na.rm = TRUE))

head(pbp2011)
#b)
pbp2011 %>%
  filter(lineup_postition == 8) %>%
  group_by()

##### 3 Pickoff Attempts
pbp2011 <- mutate(pbp2011, pick_off_attempts_from_2nd = sum(grepl(2, pbp2011$pitch_sequence)))
head(pbp2011)

