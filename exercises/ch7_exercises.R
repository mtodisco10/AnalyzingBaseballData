##### 1 Run Value of Individual Pitches
#a) 
pbp2011 <- read.csv('DataFiles/retrosheets/events/all2011.csv', stringsAsFactors = FALSE)
header <- read.csv('DataFiles/retrosheets/events/fields.csv')
names(pbp2011) <- names(header)

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

#b)
pbp2011 %>%
  filter(lineup_postition == 8) %>%
  group_by()

##### 3 Pickoff Attempts to 2nd Base

countCharOccurrences <- function(char, s) {
  s2 <- gsub(char,"",s)
  return (nchar(s) - nchar(s2))
}

pbp2011 <- mutate(pbp2011, second_base_pickoff_count = countCharOccurrences("2", pitch_sequence))
head
