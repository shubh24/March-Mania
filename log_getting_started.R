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

temp <- left_join(TourneyCompactResults, TourneySeeds, by=c("Season", "Wteam"="Team"))
compact.results <- left_join(temp, TourneySeeds, by=c("Season", "Lteam"="Team"))

train = subset(compact.results,as.numeric(compact.results$Season)<2012)

#set1 <- train %>% select(SeedNum.x, SeedNum.y) %>% mutate(result=1)
#set2 <- train %>% select(SeedNum.y, SeedNum.x) %>% mutate(result=0)

#train = rbind(set1,set2)
train$SeedNum.x=as.numeric(train$SeedNum.x)
train$SeedNum.y=as.numeric(train$SeedNum.y)
train$SeedDiff = train$SeedNum.x-train$SeedNum.y
train$result = as.numeric(train$Wteam<train$Lteam)

games.to.predict$SeedNum.x=as.numeric(games.to.predict$SeedNum.x)
games.to.predict$SeedNum.y=as.numeric(games.to.predict$SeedNum.y)
games.to.predict$SeedDiff = games.to.predict$SeedNum.x-games.to.predict$SeedNum.y

model = glm(result~SeedDiff, data = train,family = binomial())
pred = predict(model, games.to.predict,type = "response")
games.to.predict$Pred = pred

write.csv(games.to.predict %>% select(`SampleSubmission$Id`, Pred), 'seed_based_log_reg.csv', row.names=TRUE)
