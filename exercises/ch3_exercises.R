##### 1 Hall of Fame Pitching Dataset

hof.pitching <- read.csv('https://raw.githubusercontent.com/maxtoki/baseball_R/master/data/hofpitching.csv')

hof.pitching$BF.group <- with(hof.pitching,
                              cut(BF, c(0, 10000, 15000, 20000, 30000),
                                  labels = c('Less than 10000','(10000,15000)',
                                             '(15000,20000)','more than 20000')))
#1a)

table(hof.pitching$BF.group)

#1b)
barplot(table(hof.pitching$BF.group))
#14 pitchers faced more than 20k pitchers in their career

#1c)
pie(table(hof.pitching$BF.group))
#Pie graph less effective than bar graph for comparing frequencies that are close in value

##### 2 Hall of Fame Pitching Dataset Continued...
#2a)
with(hof.pitching, hist(WAR))

#2b)
hof.pitching[order(hof.pitching$WAR, decreasing = TRUE), ]
#Cy Young and Walter Johnson have by far the best WAR at 162 and 144 respectively

##### 3 Hall of Fame Pitching Dataset Continued...

hof.pitching$WAR.Season <- with(hof.pitching, WAR / Yrs)

#3a)
stripchart(WAR.Season ~ BF.group, data = hof.pitching,
           method = 'jitter', pch=1, las=2)

#3b)
boxplot(WAR.Season ~ BF.group, data = hof.pitching,
           method = 'jitter', pch=1, las=2, horizontal=TRUE)

#3c)
#WAR per season is positively correlated with number of batters faced

##### 4 Hall of Fame Pitching Dataset Continued...
hof.pitching$MidYear <- with(hof.pitching, (From + To) / 2)
hof.pitching.recent <- subset(hof.pitching, MidYear >= 1960)

#4a)
hof.pitching.recent <- hof.pitching.recent[order(hof.pitching.recent$WAR.Season),]

#4b)
dotchart(as.numeric(hof.pitching.recent$WAR.Season), labels=hof.pitching.recent$X, xlab='WAR per Season')

#4c)
#Tom Seaver and Bob Gibson stand out as the two best pitchers since 1960 in regards to WAR per Season

##### 5 Hall of Fame Pitching Dataset Continued...
#5a)
with(hof.pitching, plot(MidYear, WAR.Season))

#5b)
#There is a negative trend

#5c)
with(hof.pitching, plot(MidYear, WAR.Season))
with(hof.pitching, identify(MidYear, WAR.Season, X, n=2))
#Hank O'Day & Monte Ford

##### 6 Working with the Lahman Batting Dataset

#6a)
batting <- read.csv('DataFiles/lahman/core/batting.csv')
master <- read.csv('https://raw.githubusercontent.com/dgrtwo/dgrtwo.github.com/master/pages/lahman/Master.csv')

#6b)
getinfo <- function(firstname, lastname){
  playerline <- subset(master,
                       nameFirst==firstname & nameLast==lastname)
  name.code <- as.character(playerline$playerID)
  birthyear <- playerline$birthYear
  birthmonth <- playerline$birthMonth
  byear <- ifelse(birthmonth <= 6, birthyear, birthyear + 1)
  list(name.code=name.code, byear=byear)
}

ty.cobb <- getinfo('Ty', 'Cobb')
ted.williams <- getinfo('Ted','Williams')
pete.rose <- getinfo('Pete','Rose')
pete.rose$name.code <- pete.rose$name.code[-2]
pete.rose$byear <- pete.rose$byear[-2]

cobb.data <- subset(batting, playerID == ty.cobb$name.code)
williams.data <- subset(batting, playerID == ted.williams$name.code)
rose.data <- subset(batting, playerID == pete.rose$name.code)

#6c)
cobb.data$Age <- cobb.data$yearID - ty.cobb$byear
williams.data$Age <- williams.data$yearID - ted.williams$byear
rose.data$Age <- rose.data$yearID - pete.rose$byear

#6d)
with(rose.data, plot(Age, cumsum(H), type='l', lty=3, lwd =2,
                     xlab = 'Age', ylab='Hits',
                     xlim = c(15,50), ylim = c(0, 5000)))

#6e)
with(cobb.data, lines(Age, cumsum(H), lty=2, lwd=2))
with(williams.data, lines(Age, cumsum(H), lty=1, lwd=2))

#6f)
#Rose had the steadiest hitting trend over his career.  Williams got off to a very fast start in his early 20's and then his hit total stopped growing for a few years because of his time in the service.  Cobb had a similar trajectory as Rose, but fell just a little short in his total career hits.

##### Working with Retrosheet Play-by-Play Dataset
#7a)
data1998 <- read.csv('DataFiles/retrosheets/events/all1998.csv')
fields <- read.csv('DataFiles/retrosheets/events/fields.csv')
names(data1998) <- names(fields)
id.codes <- read.csv('DataFiles/retrosheets/ID_codes.csv', header=TRUE)
park.codes <- read.csv('DataFiles/retrosheets/Park_codes.csv', header=TRUE)
team.names <- read.csv('DataFiles/retrosheets/team_nicknames.csv', header = TRUE)


