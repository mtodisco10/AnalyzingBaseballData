##### 1 Top Base Stealers in the HOF #####
#1a) 
sb <- c(1406,938,897,741,738,689,506,504,474)
cs <- c(335,307,212,195,109,162,136,131,114)
g <- c(3081,2616,3034,2826,2476,2649,2599,2683,2379)

#1b)
sb.attempt <- sb + cs

#1c)
success.rate <- sb / sb.attempt

#1d)
sb.game <- sb / g

#1e)
plot(sb.game, success.rate)
#Max Carey had a very high success rate at 87%
#Rickie Henderson had the highest stolen bases per game at nearly .5

###### 2 Character, Factor, and Logical Variables in R #####
#2a)
outcomes <- c('Single','Out','Out','Single','Out','Double','Out','Walk','Out','Single')

#2b)
table(outcomes)

#2c)
f.outcomes <- factor(outcomes, levels = c('Out','Walk','Single','Double'))
table(f.outcomes)

#2d)
outcomes == 'Walk'
#Returns a logical vector.  TRUE on the indices where it is Walk.
sum(outcomes == 'Walk')
#Adds up all the TRUEs, which = 1

###### 3 Pitchers in the 350 Win Club #####
#3a)
w <- c(373,354,364,417,355,373,361,363,511)
l <- c(208,184,310,279,227,188,208,245,316)
name <- c('Alexander','Clemens','Galvin','Johnson','Maddux','Mathewson','Nichols','Spahn','Young')

#3b)
win.pct <- w / (w + l) * 100

#3c)
wins.350 <- data.frame(name, w, l, win.pct)

#3d)
wins.350[order(win.pct),]
#Mathewson had the highest winning percentage at 66.5%
#Galvin had the lowest winning percentage at 54.0%

##### 4 Pitchers in the 350 Wins Club, Continued... #####
#4a)
so <- c(2198,4672,1806,3509,3371,2502,1868,2583,2803)
bb <- c(951,1580,745,1363,999,844,1268,1434,1217)

#4b)
so.bb.ratio <- so / bb

#4c)
so.bb <- data.frame(name, so, bb, so.bb.ratio)

#4d)
subset(so.bb, so.bb.ratio > 2.8)

#4e)
so.bb[order(bb),]
#The pitcher with the highest walks (Roger Clemens) also had a high so / bb rat at 2.95