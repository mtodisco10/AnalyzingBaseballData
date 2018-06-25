##### 1 Runs Values of Hits

#1a) 
data2017 <- read.csv('DataFiles/retrosheets/events/all2017.csv')
fields <- read.csv('DataFiles/retrosheets/events/fields.csv')
names(data2017) <- names(fields)

d.double <- subset(data2017, event_num == 21)

library(MASS)

data2017$RUNS <- with(data2017, vis_score + home_score)

data2017$HALF.INNING <- with(data2017, paste(game_id, inning, batting_team))

data2017$RUNS.SCORED <- with(data2017, (batter_dest_5_if_scores_and_unearned > 3) + 
                             (runner_on_1st_dest_5_if_scores_and_unearned > 3) +
                             (runner_on_2nd_dest_5_if_scores_and_unearned > 3) +
                             (runner_on_3rd_dest_.5_if_socres_and_uneanred))

RUNS.SCORED.INNING <- aggregate(data2017$RUNS.SCORED, list(HALF.INNING= data2017$HALF.INNING), sum)

RUNS.SCORED.START <- aggregate(data2017$RUNS, list(HALF.INNING=data2017$HALF.INNING), "[", 1)

MAX <- data.frame(HALF.INNING=RUNS.SCORED.START$HALF.INNING)
MAX$x <- RUNS.SCORED.INNING$x + RUNS.SCORED.START$x
data2017 <- merge(data2017, MAX)
N <- ncol(data2017)
names(data2017)[N] <- "MAX.RUNS"

data2017$RUNS.ROI <- with(data2017, MAX.RUNS - RUNS)

names(data2017)

RUNNER1 <- ifelse(data2017[ , "runner_on_1st_dest"] == 0, 0, 1)
RUNNER2 <- ifelse(data2017[ , "runner_on_2nd_dest"] == 0, 0, 1)
RUNNER3 <- ifelse(data2017[ , "runner_on_3rd_dest"] == 0, 0, 1)

get.state <- function(runner1, runner2, runner3, outs){
  runners <- paste(runner1, runner2, runner3, sep="")
  paste(runners, outs)
}

data2017$STATE <- get.state(RUNNER1, RUNNER2, RUNNER3, data2017$outs)

NRUNNER1 <- with(data2017, as.numeric(runner_on_1st_dest == 1 | batter_dest == 1))
NRUNNER2 <- with(data2017, as.numeric(runner_on_1st_dest == 1 | batter_dest == 1))

names(data2017)
