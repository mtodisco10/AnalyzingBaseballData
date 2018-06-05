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


