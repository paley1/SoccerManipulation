##soccer data manipulations
team_id_match <- merge(Match_team_level,Team_names)##merges the two data tables based on teamId
event_idteam1_4 <- team_id_match[team_id_match$Team %in% c("Team1","Team2","Team3","Team4"),]##to get the event id's for the games of team1-4 being involved
View(event_idteam1_4)
event_id_column <- event_idteam1_4$eventId##this is now the column of id's that team1-4 are involved in

team1_4involved <- team_id_match[team_id_match$eventId %in% event_id_column,] ##gets all matches team 1 through are involved in based on event id
View(team1_4involved)
team14_play <- team1_4involved[team1_4involved$Team %in% c("Team1","Team2","Team3","Team4"),] ##teams 1 through 4 playing each other
View(team14_play)

team14_play_goals_scored <- team14_play[order(-team14_play$Score),]##order these games by goals scored in descending order
View(team14_play_goals_scored)
team14_play_goals_scored_shots <- team14_play[order(-team14_play$Score,-team14_play$TotalShots,-team14_play$ShotsOnTarget),]##order by goals scored,then shots,and shots on target in descending order
View(team14_play_goals_scored_shots)

top5_performances <- team14_play_goals_scored_shots[1:5,]##first 5 rows of previously found data sorted
View(top5_performances)

##part2
Performance <- c("bad","medium","good")##makes a list for the three groups of performance level
team_id_match$Performance <-cut(team_id_match$PossessionPct,3,labels=Performance)##makes a new variable named performance and assigned to three groups
Player_Performance <- c("bad","medium","good")
Player_Rating <-Player_Performance
Player_match_level$PlayerRating=(Player_match_level$Goals+.5*Player_match_level$Assists+.5*Player_match_level$Shots.on.Goal-.2*Player_match_level$Fouls.Committed-.8*Player_match_level$Red.Cards-.4*Player_match_level$Yellow.Cards)
##creates new column for player rating statistic based on assigned conditions
Player_match_level$Player_Rating <- cut(Player_match_level$PlayerRating,3,labels=Player_Rating)##cuts players into 3 groups
View(Player_match_level)

merge_player_teamid <- merge(Player_match_level,Player_team_mapping,by=c("eventId","athleteId"))##merges player id's
team14players <- merge_player_teamid[merge_player_teamid$Team %in% c("Team1","Team2","Team3","Team4"),]##gets player data for only teams 1 through 4
team14players_goals <- aggregate(team14players$Goals,by=list(merge_player_teamid$athleteId,teamId=team14players$teamId),sum)##sums and sorts team 1 through players by goals scored
team14players_goals <- aggregate(team14players$Goals,by=list(merge_player_teamid$athleteId,teamId=team14players$teamId),sum,)##sums and sorts team 1 through players by goals scored
team14players_goals <- aggregate(team14players$Goals,by=list(merge_player_teamid$athleteId,teamId=team14players$teamId),sum)##sums and sorts team 1 through players by goals scored
max_goalscorer_team <- aggregate(x~teamId,team14players_goals,max)##max scorer on each team for 1 through 4
merge_goalscorer_team <- merge(max_goalscorer_team,team14players_goals)##gives table of highest scoring players with team number

##part3
match$date=as.character(match$eventDate) ##turns event date into date object
team14_matches =subset(match, (match$Team=="Team1"| match$Team=="Team2"| match$Team=="Team3"|match$Team=="Team4") & (match$date>="2011-12-20" & match$date<="2012-01-05")) ##subsets the data and matches up for the matches of teams 1 through 4
team14_matches_occur <- table(team14_matches$Team) ##creates a table to show occurences of team 1 through 4 matches

team14_time <- team14_matches[order(team14_matches$Team,as.Date(team14_matches$date, format="%Y-%m-%d")),] ##orders by team and date, which finds matches for time interval
team14_time$prev <- c(rep(NA,1),head(team14_time$date,-1))##creates previous date column
team14_time$diff <- as.numeric(team14_time$date-team14_time$prev)##creates difference in time between matches for teams 1 through 4 and subtracts from date column
team14_time$diff[team14_time$diff<=0] <- NA

tapply(P5time$diff,P5time$Team,na.rm=TRUE,mean) ##finds average time difference in matches for teams invloved

Player_name_team <- function(ID){+ newPTM<-unique(Player_team_mapping[,2:3]) + x<-newPTM[newPTM$athleteId %in% ID,1] + factor(Team_names[Team_names$teamId %in% x,2]) + }
## gives player name and team based on given ID


##part 4
team_goals <- subset(match,match$date<="2012-1-06"& match$date>="2011-06-1") ##subsets data for teams' total goals
tapply(team_goals$Score,team_goals$Team,sum)##sums the score to give values for teams' goals

average_team_pass$estpass=(average_team_pass$PossessionPct/100*540)##assumes that there is one pass every 10 seconds and because there are 90 minutes in the average game, there are 540 passes on average
tapply(average_team_pass$estpass,average_team_pass$Team,mean) ##tapply gets the average for each team

number_players <- merge(Player_team_mapping,average_team_pass,by=c("eventId","teamId")) ## merges Player_mapping table with the 2011-12 game table
number_players_unique <- unique(number_players[,c("teamId","athleteId")]) ##limits players to unique since there are multiple appearances by the same players
number_players_appear <- aggregate(athleteId~teamId,number_players_unique,length) ##aggreagte function counts the appearances


##part5
Match_team_level <- read.csv("C:/Downloads/Match_team_level.csv")
View(Match_team_level)
Player_match_level <- read.csv("C:/Downloads/Player_match_level.csv")
View(Player_match_level)
Player_team_mapping <- read.csv("C:/Downloads/Player_team_mapping.csv")
View(Player_team_mapping)
Team_names <- read.csv("C:/Downloads/Team_names.csv")
View(Team_names)
team_id_match <- merge(Match_team_level,Team_names)##merges the two data tables based on teamId
View(team_id_match)

#aggregate option
team_goals <- subset(team_id_match,select=c("Team","Score"))##subsets just these two variables
team_goals_sum <- aggregate(team_goals$Score,by=list(team_goals$Team),FUN=sum)##aggregate the total goals by teams
colnames(team_goals_sum) <- c("Team","Goals")##renaming columns

#lapply method, displays output in list
team_goalsl <- split(team_id_match,team_id_match$Team)
lapply(team_goalsl,function(x) sum(x$Score))

#sapply method, displays output in vector
team_goalss <- split(team_id_match,team_id_match$Team)
sapply(team_goalss,function(x) sum(x$Score))


##part6
team_games_stadium <- table(team_id_match$Team,team_id_match$venuesName)#table command makes a table matching games played by team and stadium
prop.table(team_games_stadium, 1)#proportions are given based on column or row with the prop.table command. The value received represents the proportion of games a team played in a given stadium
summary(team_games_stadium)
#the test checks whether there is an association between team and venue. It reveals that there is, which makes sense since every team plays half of their games at their home venue.


##part7
unique(team_id_match[grep("^S", team_id_match$venuesName),6])#searches for and returns unique venues the begin with 'S'
unique(team_id_match[grep("Park", team_id_match$venuesName),6])## The 'grep' command searches venue for "Park" and returns the unique venues of Villa Park, Ewood Park, Goodison Park, and Upton Park.
Match_team_level$eventType <- sub("Barclays Premier League", "EPL",Match_team_level$eventType) ## The eventType column in Match_team_level is searched for "Barclays Premier League" and replaced by "EPL" in the respective column by the 'sub' function. 
team_id_match$Team <- paste("FC",team_id_match$Team,sep=" ")#paste function puts "FC" before each team and uses 'sep=" "' to leave a space between the words and updates the column

##part8
team_id_match$date <- as.Date(team_id_match$eventDate)#modifies 'eventDate' into a date class
team_id_match$year <- as.POSIXct(team_id_match$date)
team_id_match$year <- strftime(team_id_match$year,"%Y")
team_id_match$month <- as.POSIXct(team_id_match$date)
team_id_match$month <- strftime(team_id_match$month,"%m") #now labeled by year and month

team_id_match_sum <- aggregate(Score~month+year,team_id_match,sum) #aggregating returns a dataframe that has goals summed by month and year
team_id_match_sum$month_year <- with(team_id_match_sum, ISOdate(team_id_match_sum$year,team_id_match_sum$month,1))
team_id_match_sum <- team_id_match_sum[order(team_id_match_sum$month_year),] #new column with month and year
plot(team_id_match_sum$month,team_id_match_sum$Score)#plot with respect to month


##part9
install.packages("sqldf") ##SQL functioning within R

join_team <- sqldf("SELECT * FROM Match_team_level INNER JOIN Team_names ON Match_team_level.teamId=Team_names.teamId") #join tables by teamid
join_teamb <- sqldf("select * from join_team where Team is 'Team1' or Team is 'Team2' or Team is 'Team3' or Team is 'Team4'")#limit to only team 1 through 4
team_joinc <- sqldf("SELECT l.eventId,r.* FROM team_joinb AS l INNER JOIN team_join AS r ON l.eventId = r.eventId;GO")#join by eventid for opponents of tem 1 through 4

team_1_4 <- sqldf("select * from team_joinc where Team is 'Team1' or Team is 'Team2' or Team is 'Team3' or Team is 'Team4'") #use 'where' to limit to games between the 4 teams only
team_1_4_order <- sqldf("select * from team_joinc order by Score")
team_1_4_shots <- sqldf("select * from team_joinc order by TotalShots desc,ShotsonTarget desc")#order by with desc gives top 5 scoring teams

season2011_12 <- sqldf("select * from team_join where eventDate>'2011-06-01'")#data for 2011-12 season
season2011_12 <- sqldf("select Team, sum(Score) `CumScore` from season2011_12 group by Team")

team_pass <- sqldf("ALTER TABLE season2011_12 ADD passes AS (PossessionPct / 100 * 540)")#based on average of 540 passes a game
team_passb <- sqldf("select Team, avg(passes) `Avgpasses` from team_pass group by Team")#average passes by team
player_appear <- sqldf("select teamId, count(distinct(athleteId)) `Playercount` from Player_team_mapping group by teamId")##"count(distinct())" gets unique player appearances


