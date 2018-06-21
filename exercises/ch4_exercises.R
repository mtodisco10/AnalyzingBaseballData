##### 1. Relationship Between Winning Percentage and Run Differential Across Decades

#1a)

teams <- read.csv('DataFiles/lahman/core/Teams.csv')
teams$RD <- teams$R - teams$RA
teams$Wpct <- teams$W / (teams$W + teams$L)

linfit <- lm(Wpct ~ RD, data=teams)

teams$linWpct <- predict(linfit)
teams$linResiduals <- residuals(linfit)

teams.RD.10 <- subset(teams, RD > 5 & RD < 15)

teams.Wpct.1960 <- mean(subset(teams.RD.10, yearID > 1960 & yearID <=1970)$linWpct)
teams.Wpct.1970 <- mean(subset(teams.RD.10, yearID > 1970 & yearID <=1980)$linWpct)
teams.Wpct.1980 <- mean(subset(teams.RD.10, yearID > 1980 & yearID <=1990)$linWpct)
teams.Wpct.1990 <- mean(subset(teams.RD.10, yearID > 1990 & yearID <=2000)$linWpct)

#### 2 Pythagorean Residuals for Poor and Great Teams in the 19th Century
#a)

teams.19th.century <- subset(teams, yearID < 1900)
teams.19th.century$PyWpct <- with(teams.19th.century, R ^ 1.903 / (R ^ 1.903 + RA ^ 1.903))
teams.19th.century$PyResiduals <- with(teams.19th.century, Wpct - PyWpct)

plot(teams.19th.century$RD, teams.19th.century$PyResiduals)

##### 3 Exploring Manager Effect in Baseball
#a)
teams.2015 <- subset(teams, yearID > 2005 & yearID <= 2015)
teams.2015$PyWpct <- with(teams.2015, R ^ 1.903 / (R ^ 1.903 + RA ^ 1.903))
teams.2015$PyResiduals <- with(teams.2015, Wpct - PyWpct)

managers <- read.csv('DataFiles/lahman/core/Managers.csv')

teams.2015.merged <- merge(teams.2015, managers, by = c("yearID", "teamID"))

#10 Worst Managers
head(teams.2015.merged[order(teams.2015.merged$PyResiduals), ]$playerID, 10)

#10 Best Managers
head(teams.2015.merged[order(-teams.2015.merged$PyResiduals), ]$playerID, 10)

#4 Pythagorean Relationship for Other Sports
#opensourcesports.com