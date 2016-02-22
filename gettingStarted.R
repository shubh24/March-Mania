TourneySeeds <- read.csv("../TourneySeeds.csv")
SampleSubmission <- read.csv("../SampleSubmission.csv")
Seasons <- read.csv("../Seasons.csv")
Teams <- read.csv("../Teams.csv")
TourneySlots <- read.csv("../TourneySlots.csv")
TourneyDetailedResults <- read.csv("../TourneyDetailedResults.csv")
TourneyCompactResults <- read.csv("../TourneyCompactResults.csv")

RegularCompact = read.csv("../RegularSeasonCompactResults.csv")
RegularDetailed = read.csv("../RegularSeasonDetailedResults.csv")
library(reshape)
library(dplyr)
TourneySeeds <- TourneySeeds%>%mutate(SeedNum = gsub("[A-Z+a-z]", "", Seed))%>%select(Season, Team, SeedNum)
games.to.predict <- cbind(SampleSubmission$Id, colsplit(SampleSubmission$Id, split = "_", names = c('season', 'team1', 'team2')))  

temp <- left_join(games.to.predict, TourneySeeds, by=c("season"="Season", "team1"="Team"))
games.to.predict <- left_join(temp, TourneySeeds, by=c("season"="Season", "team2"="Team"))
games.to.predict$SeedDiff = as.numeric(games.to.predict$SeedNum.x)-as.numeric(games.to.predict$SeedNum.y)

temp <- left_join(TourneyCompactResults, TourneySeeds, by=c("Season", "Wteam"="Team"))
compact.results <- left_join(temp, TourneySeeds, by=c("Season", "Lteam"="Team"))

train = subset(compact.results,as.numeric(compact.results$Season)<2012)
booleanWin = (train$Wteam<train$Lteam)
train$SeedDiff = as.numeric(train$SeedNum.x)-as.numeric(train$SeedNum.y)
train$TeamOneWins=as.factor(booleanWin)

model = lm(TeamOneWins~SeedDiff, data = train,family = gaussian)
pred = predict(model, games.to.predict)
pred = pred - 1
games.to.predict$Pred = pred

write.csv(games.to.predict %>% select(`SampleSubmission$Id`, Pred), 'seed_submission.csv', row.names=FALSE)
