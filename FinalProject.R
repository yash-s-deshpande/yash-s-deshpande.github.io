library(dplyr)
library(tree)
library(tidyverse)

#Load the data
library(readr)
NBA <- read_csv("NBA.csv")

#Cleaning up the dataset

NBA$URL <- NULL

NBA[, c("GameType", "Time" , "Shooter" , "ShotType" , "ShotOutcome" , "ShotDist",
        "Assister", "Blocker" , "FoulType" , "Fouler" , "Fouled" , "Rebounder" , 
        "ReboundType" , "ViolationPlayer" , "ViolationType" , "TimeoutTeam")] <- list(NULL)

NBA[, c("FreeThrowShooter", "FreeThrowOutcome", "FreeThrowNum", "EnterGame" , "LeaveGame", 
        "TurnoverPlayer", "TurnoverType", "TurnoverCause" , "TurnoverCauser" , 
        "JumpballAwayPlayer" , "JumpballHomePlayer" , "JumpballPoss" , "...41")] <- list(NULL)

#Modifying columns

NBA <- NBA %>%
  mutate(
    Team = ifelse(AwayPlay != "" & !is.na(AwayPlay), AwayTeam, HomeTeam),
    Play = ifelse(AwayPlay != "" & !is.na(AwayPlay), AwayPlay, HomePlay)
  ) %>%
  select(Date, Quarter, SecLeft, Team, Play, AwayScore, HomeScore, AwayTeam, HomeTeam)

classify_event <- function(play) {
  p <- tolower(play)
  
  if (grepl("defensive rebound", p)) return("defensive_rebound")
  if (grepl("offensive rebound", p)) return("offensive_rebound")
  if (grepl("misses 2-pt", p)) return("missed_two")
  if (grepl("misses 3-pt", p)) return("missed_three")
  if (grepl("misses free throw", p)) return("missed_free_throw")
  if (grepl("makes 2-pt", p)) return("made_two")
  if (grepl("makes 3-pt", p)) return("made_three")
  if (grepl("free throw", p)) return("free_throw")
  if (grepl("foul", p)) return("foul")
  if (grepl("turnover", p)) return("turnover")
  if (grepl("jump ball", p)) return("jump_ball")
  if (grepl("violation", p)) return("violation")
  if (grepl("substitution", p)) return("substitution")
  if (grepl("timeout", p)) return("timeout")
  
  return("other")
}


NBA$EventType <- sapply(NBA$Play, classify_event) #This line may take a minute

NBA <- NBA %>%
  mutate(
    GameID = paste(Date, AwayTeam, HomeTeam, sep = "_")
  )


NBA <- NBA %>%
  group_by(GameID) %>%
  arrange(Quarter, desc(SecLeft), .by_group = TRUE) %>%
  mutate(
    NextEvent = lead(EventType), 
    PrevEvent1 = lag(EventType, 1)
  ) %>%
  ungroup()

#Creating the decision trees

NBA$NextEvent <- as.factor(NBA$NextEvent)
NBA$PrevEvent1 <- as.factor(NBA$PrevEvent1)

set.seed(1)

train_i <- sample(seq_len(nrow(NBA)), 0.7*nrow(NBA))

train <- NBA[train_i, ]
test <- NBA[-train_i, ]

train_clean <- train %>% drop_na(PrevEvent1, NextEvent)
test_clean  <- test  %>% drop_na(PrevEvent1, NextEvent)


train_clean$NextEvent  <- factor(train_clean$NextEvent)
train_clean$PrevEvent1 <- factor(train_clean$PrevEvent1)
train_clean$Team       <- factor(train_clean$Team)
train_clean$Quarter    <- factor(train_clean$Quarter)


test_clean$NextEvent  <- factor(test_clean$NextEvent)
test_clean$PrevEvent1 <- factor(test_clean$PrevEvent1)
test_clean$Team       <- factor(test_clean$Team)
test_clean$Quarter    <- factor(test_clean$Quarter)

tree_1 <- tree(
  NextEvent ~ PrevEvent1 + Quarter + SecLeft + HomeScore + AwayScore,
  data = train_clean, control = tree.control(nobs = nrow(NBA), 
                                       mindev = 0.0001,
                                       minsize = 6, mincut = 3)
)

summary(tree_1)

plot(tree_1)
text(tree_1,pretty = 0, cex = 0.5) #expand the plot for readability

#Calculating accuracy of the tree

pred <- predict(tree_1, newdata = test_clean, type = "class")
table(pred, test_clean$NextEvent)
mean(as.character(pred) == as.character(test_clean$NextEvent))
mean(pred == test_clean$NextEvent)


#Pruning the tree and finding the optimal size
set.seed(1)
cv <- cv.tree(tree_1, FUN = prune.misclass)
cv
plot(cv$size, cv$dev, type = "b")
optimal <- cv$size[which.min(cv$dev)]
optimal #disregard since any size from 3-54 works?


#Creating a tree with optimal size of 10

optimal_tree <- prune.tree(tree_1, best = 10)
summary(optimal_tree)
plot(optimal_tree)
text(optimal_tree, pretty = 0, cex = 0.8) #expand the plot for readability

#Calculating accuracy of new tree

pred_test <- predict(optimal_tree, test_clean, type = "class")
mean(pred_test == test_clean$NextEvent)

