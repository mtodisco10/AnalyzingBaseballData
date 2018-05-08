##### 1 Which Datafile? #####

#a) 

##### 2 Lahman Pitching Data #####

#2a)  What percentage of games did Bob Gibson complete in 1968?

#Read in the lahman pitching data
lahman_pitching <- read.csv('DataFiles/lahman/core/Pitching.csv')

#Subset data for Gibson and the 1968 Season
gibson_1968 <- subset(lahman_pitching, ((playerID == 'gibsobo01') & (yearID == 1968)))

print(paste0('Bob Gibson completed ', round(gibson_1968$CG / gibson_1968$GS * 100, 0), '% of the games he started in 1968'))

#2b) What was Gibson's ratio of strikeouts to walks this season?

print(paste0(round(gibson_1968$SO / gibson_1968$BB, 1), ' Strikeouts to 1 Walk'))

#2c) How many innings did Gibson Pitch?

print(paste0(round(gibson_1968$IPouts / 3, 2), ' Innings'))

#2d) Gibson's WHIP?

print(paste0(round((gibson_1968$H + gibson_1968$BB) / (gibson_1968$IPouts / 3), 2), ' WHIP'))

##### 3 Retrosheet Game Log #####

source('utils/headers/gamelogs_header.R')

gamelogs.1964 <- read.table("DataFiles/retrosheets/gamelogs/gl1960_69/GL1964.TXT", sep = ',', header = FALSE)

names(gamelogs.1964) <- gamelogs_header

#3a)
bunnings <- subset(gamelogs.1964, ((Date == 19640621) & (HomeTeam=='NYN') & (HomeTeamGameNumber == 67)))

#Time of the game in hours and minutes
hours <- round(bunnings$Duration / 60, 0)
minutes <- bunnings$Duration - (hours * 60)

print(paste0("The game lasted ", hours, " hours and ", minutes, " minutes"))

#3b)
print("The attendance is not known")

#3c)
print(paste0("The Phillies had ", bunnings$VisitorD + bunnings$VisitorT + bunnings$VisitorHR, " extra base hits in this game"))

#3d)
print(paste0("The Phillies on-base percentage was ", (bunnings$VisitorH + bunnings$VisitorBB)/bunnings$VisitorAB))

##### 4 Retrosheet Play-by-Play #####
#4) Mickie Morandini caught a line drive while playing second, stepped on 2nd bse to double Van Slyke and then on first base to get Bonds for an unassisted triple play.


##### Pitchf/x Record of Several Pitches #####

#5) The knuckle ball has similar characteristics as the fastball with the exception of much slower start/end speed and much different horizontal movement
