if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(dslabs)) install.packages("dslabs", repos = "http://cran.us.r-project.org")
if(!require(purrr)) install.packages("purrr", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("plyr", repos = "http://cran.us.r-project.org")
if(!require(DescTools)) install.packages("DescTools", dependencies=TRUE, INSTALL_opts = c('--no-lock'))


set.seed(2, sample.kind="Rounding")

# Importing dataset and converting to dataframe.
URLDATA <- "https://raw.githubusercontent.com/boomerwv1/FinalProject/master/nba.games.stats.csv"
NBAData <- read_csv(url(URLDATA))

# Listing fields from dataset to be gathered.
fields = c("FieldGoals", "FieldGoalsAttempted", "X3PointShots", "X3PointShotsAttempted", "FreeThrows", "FreeThrowsAttempted", "OffRebounds", "TotalRebounds", "Assists", "Steals", "Blocks", "Turnovers", "TotalFouls")

#Gathering data into tidy format.
NBAplot <- NBAData %>%
  gather(Variable, Stat, fields)

# Creating boxgraph with tidy data.
NBAplot %>%
  group_by(Variable) %>%
  ggplot(aes(WINorLOSS, Stat)) +
  geom_boxplot() +
  facet_wrap( ~ Variable)

#Creating binary field for W/L and Stratifying statistics for regression analysis.
Averages <- NBAData %>%
  mutate(W = ifelse(WINorLOSS == "W", 1, 0),
         FG_round = plyr::round_any(FieldGoals, 5)) %>%
  group_by(FG_round) %>%
  mutate(FG_W_odds = mean(W)) %>%
  # Field Goals Attempted
  mutate(FGA_round = plyr::round_any(FieldGoalsAttempted, 5)) %>%
  group_by(FGA_round) %>%
  mutate(FGA_W_odds = mean(W)) %>%
  # 3 pointers made
  mutate(X3P_round = plyr::round_any(X3PointShots, 5)) %>%
  group_by(X3P_round) %>%
  mutate(X3P_W_odds = mean(W)) %>%
  # 3 pointers attempted
  mutate(X3A_round = plyr::round_any(X3PointShotsAttempted, 5)) %>%
  group_by(X3A_round) %>%
  mutate(X3A_W_odds = mean(W)) %>%
  # Free throws  made
  mutate(FT_round = plyr::round_any(FreeThrows, 5)) %>%
  group_by(FT_round) %>%
  mutate(FT_W_odds = mean(W)) %>%
  # Free Throws attempted
  mutate(FTA_round = plyr::round_any(FreeThrowsAttempted, 5)) %>%
  group_by(FTA_round) %>%
  mutate(FTA_W_odds = mean(W)) %>%
  # Offensive Rebounds
  mutate(OR_round = plyr::round_any(OffRebounds, 2)) %>%
  group_by(OR_round) %>%
  mutate(OR_W_odds = mean(W)) %>%
  # Total Rebounds
  mutate(TR_round = plyr::round_any(TotalRebounds, 5)) %>%
  group_by(TR_round) %>%
  mutate(TR_W_odds = mean(W)) %>%
  # Assists
  mutate(AST_round = plyr::round_any(Assists, 5)) %>%
  group_by(AST_round) %>%
  mutate(AST_W_odds = mean(W)) %>%
  # Steals
  mutate(STL_round = plyr::round_any(Steals, 2)) %>%
  group_by(STL_round) %>%
  mutate(STL_W_odds = mean(W)) %>%
  # Blocks
  mutate(BLK_round = plyr::round_any(Blocks, 2)) %>%
  group_by(BLK_round) %>%
  mutate(BLK_W_odds = mean(W)) %>%
  # Turnovers
  mutate(TO_round = plyr::round_any(Turnovers, 2)) %>%
  group_by(TO_round) %>%
  mutate(TO_W_odds = mean(W)) %>%
  # Fouls
  mutate(FLS_round = plyr::round_any(TotalFouls, 2)) %>%
  group_by(FLS_round) %>%
  mutate(FLS_W_odds = mean(W))
  
#Creating lists of field names for wrangling.
fields2 <- c("FG_W_odds", "FGA_W_odds", "X3P_W_odds", "X3A_W_odds",
             "FT_W_odds", "FTA_W_odds", "OR_W_odds", "TR_W_odds",
             "AST_W_odds", "STL_W_odds", "BLK_W_odds",
             "TO_W_odds", "FLS_W_odds")

fields3 <- c("FG_round", "FGA_round", "X3P_round", "X3A_round",
             "FT_round", "FTA_round", "OR_round", "TR_round",
             "AST_round", "STL_round", "BLK_round",
             "TO_round", "FLS_round")


#Gathering data into tidy.
AveragesTidy <- Averages %>%
  gather(Variable, W_Percentage, fields2) %>%
  gather(Stat, n, fields3) %>%
  mutate(statcheck1 = StrLeft(Stat, 3),
         statcheck2 = StrLeft(Variable, 3)) %>%
  filter(statcheck1 == statcheck2) %>%
  mutate(name = statcheck1) %>%
  select(Home, WINorLOSS, Variable, W_Percentage, Stat, n, name)


#Changing names in Tidy data for display in graphs.
AveragesTidy$name[AveragesTidy$name=="FG_"] <- "Field Goals"
AveragesTidy$name[AveragesTidy$name=="FGA"] <- "Field Goal Attempts"
AveragesTidy$name[AveragesTidy$name=="X3P"] <- "Three-Pointer"
AveragesTidy$name[AveragesTidy$name=="X3A"] <- "Three-Point Attempts"
AveragesTidy$name[AveragesTidy$name=="FT_"] <- "Free Throws"
AveragesTidy$name[AveragesTidy$name=="FTA"] <- "Free Throw Attempts"
AveragesTidy$name[AveragesTidy$name=="OR_"] <- "Offensive Rebounds"
AveragesTidy$name[AveragesTidy$name=="TR_"] <- "Total Rebounds"
AveragesTidy$name[AveragesTidy$name=="AST"] <- "Assists"
AveragesTidy$name[AveragesTidy$name=="STL"] <- "Steals"
AveragesTidy$name[AveragesTidy$name=="BLK"] <- "Blocks"
AveragesTidy$name[AveragesTidy$name=="TO_"] <- "Turnovers"
AveragesTidy$name[AveragesTidy$name=="FLS"] <- "Fouls"

#Graphing statistics against win percentage in scatterplots
AveragesTidy %>%
  group_by(Stat) %>%
  ggplot(aes(n, W_Percentage)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap( ~ name)
  
#Creating binary field for W/L (1,0)
NBA_LM <- NBAData %>%
  mutate(W = ifelse(WINorLOSS == "W", 1, 0))

#Exploring linier model fit for various stats
fit_W_FG <- lm(W ~ FieldGoals, data = NBA_LM)
fit_W_TR <- lm(W ~ TotalRebounds, data = NBA_LM)
fit_W_AST <- lm(W ~ Assists, data = NBA_LM)
fit_W_3P <- lm(W ~ X3PointShots, data = NBA_LM)
fit_W_TO <- lm(W ~ Turnovers,data = NBA_LM)

  
summary(fit_W_FG)
summary(fit_W_TR)
summary(fit_W_AST)
summary(fit_W_3P)
summary(fit_W_TO)

#Creating matrix of data to explore correlations
NBA_Matrix <- NBA_LM %>%
  select(fields, W)

#Calculating correlations of each statistic with one another
NBAMatrix <- data.matrix(NBA_Matrix, rownames.force = NA)

NBACor <- cor(NBAMatrix)

NBACor

# Creating patition and test/trianing sets.
test_index <- createDataPartition(NBAData$WINorLOSS, times = 1, p = 0.5, list = FALSE)
test_set <- NBAData[test_index, ]
train_set <- NBAData[-test_index, ]

# Establishing list of models for ensemble.
models <- c("glm", "lda", "naive_bayes", "svmLinear", "knn", "gamLoess", "multinom", "qda", "rf")  #"adaboost")


#Running several ensemble models
#Calculating fit on single variable.
fits <- lapply(models, function(model){ 
  print(model)
  train(WINorLOSS ~ FieldGoals, method = model, data = train_set)
})

#Making predictions
pred <- sapply(fits, function(object) 
  predict(object, newdata = test_set))

#Checking accuracy on test set with single variable.
wins <- rowMeans(pred == "W")
y_hat <- ifelse(wins > 0.5, "W", "L")
results <- mean(y_hat == test_set$WINorLOSS)
chart <- data_frame(Variables = "Single Variable", Accuracy = results)

chart %>% knitr::kable()

#Calculating fit on single variable.
fits2 <- lapply(models, function(model){ 
  print(model)
  train(WINorLOSS ~ FieldGoals + Turnovers, method = model, data = train_set)
})

#Making predictions
pred2 <- sapply(fits2, function(object) 
  predict(object, newdata = test_set))

#Checking accuracy on test set with two variable.
wins2 <- rowMeans(pred2 == "W")
y_hat2 <- ifelse(wins2 > 0.5, "W", "L")
results <- mean(y_hat2 == test_set$WINorLOSS)
chart <- bind_rows(chart,
                   data_frame(Variables="Two Variables",
                              Accuracy = results))
chart %>% knitr::kable()

#Recalculating fit with multivariate model
fits3 <- lapply(models, function(model){ 
  print(model)
  train(WINorLOSS ~ FieldGoals + X3PointShots + Assists + TotalRebounds, method = model, data = train_set)
})

#Making predictions
pred3 <- sapply(fits3, function(object) 
  predict(object, newdata = test_set))

#Repeating accuracy on test set with multiple variables.
wins3 <- rowMeans(pred3 == "W")
y_hat3 <- ifelse(wins3 > 0.5, "W", "L")
results <- mean(y_hat3 == test_set$WINorLOSS)
chart <- bind_rows(chart,
                   data_frame(Variables="Multivariate",
                              Accuracy = results))
chart %>% knitr::kable()

# Calculating fit of ensemble against whole dataset.
fits4 <- lapply(models, function(model){ 
  print(model)
  train(WINorLOSS ~ ., method = model, data = train_set)
}) 

# making predictions
pred4 <- sapply(fits4, function(object) 
  predict(object, newdata = test_set))

# Checking accuracy of all variable model against test set.
wins4 <- rowMeans(pred4 == "W")
y_hat4 <- ifelse(wins4 > 0.5, "W", "L")
results <- mean(y_hat4 == test_set$WINorLOSS)
chart <- bind_rows(chart,
                     data_frame(Variables="All",
                                Accuracy = mean(y_hat4 == test_set$WINorLOSS)))
chart %>% knitr::kable()




