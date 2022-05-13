library(readxl)
ufc_master <- read_excel("C:/Users/Dave/Desktop/Gavin/Software Project/ufc-master.xlsx")
View(ufc_master)


#changing stances to numerical. Orthodox = 0, Southpaw = 1, Switch = 2
ufc_master$B_Stance[ufc_master$B_Stance == "Orthodox"] <- 0
ufc_master$B_Stance[ufc_master$B_Stance == "Southpaw"] <- 1
ufc_master$B_Stance[ufc_master$B_Stance == "Switch"] <- 2
ufc_master$B_Stance[ufc_master$B_Stance == "Open Stance"] <- 3

ufc_master$B_Stance <- as.numeric(ufc_master$B_Stance)
ufc_master$R_Stance <- as.numeric(ufc_master$R_Stance)


ufc_master$R_Stance[ufc_master$R_Stance == "Orthodox"] <- 0
ufc_master$R_Stance[ufc_master$R_Stance == "Southpaw"] <- 1
ufc_master$R_Stance[ufc_master$R_Stance == "Switch"] <- 2
ufc_master$R_Stance[ufc_master$R_Stance == "Open Stance"] <- 3

#No draws in data set

#changing winners column to red winner = 1, blue winner = 2
ufc_master$Winner[ufc_master$Winner == "Red"] <- 1
ufc_master$Winner[ufc_master$Winner == "Blue"] <- 2
#changing winners column to red winner = 0, blue winner = 1
ufc_master$Winner[ufc_master$Winner == "1"] <- 0
ufc_master$Winner[ufc_master$Winner == "2"] <- 1

ufc_master$Winner <- as.numeric(ufc_master$Winner)


#Changing Gender to Male = 1, Female = 2
ufc_master$gender[ufc_master$gender == "MALE"] <- 1
ufc_master$gender[ufc_master$gender == "FEMALE"] <- 2

ufc_master$gender <- as.numeric(ufc_master$gender)

#Changing Weight Classes to numeric
ufc_master$weight_class[ufc_master$weight_class == "Flyweight"] <- 0
ufc_master$weight_class[ufc_master$weight_class == "Bantamweight"] <- 1
ufc_master$weight_class[ufc_master$weight_class == "Featherweight"] <- 2
ufc_master$weight_class[ufc_master$weight_class == "Lightweight"] <- 3
ufc_master$weight_class[ufc_master$weight_class == "Welterweight"] <- 4
ufc_master$weight_class[ufc_master$weight_class == "Middleweight"] <- 5
ufc_master$weight_class[ufc_master$weight_class == "Light Heavyweight"] <- 6
ufc_master$weight_class[ufc_master$weight_class == "Heavyweight"] <- 7
ufc_master$weight_class[ufc_master$weight_class == "Women's Strawweight"] <- 8
ufc_master$weight_class[ufc_master$weight_class == "Women's Flyweight"] <- 9
ufc_master$weight_class[ufc_master$weight_class == "Women's Bantamweight"] <- 10
ufc_master$weight_class[ufc_master$weight_class == "Women's Featherweight"] <- 11
ufc_master$weight_class[ufc_master$weight_class == "Catch Weight"] <- 12

ufc_master$weight_class <- as.numeric(ufc_master$weight_class)

#finding max of certain variables to normalize the data
max(ufc_master$B_current_lose_streak) #=6
max(ufc_master$B_current_win_streak) #=12

max(ufc_master$R_current_lose_streak) #=7
max(ufc_master$R_current_win_streak) #=16

max(ufc_master$R_age) #47
max(ufc_master$B_age) #47

max(ufc_master$R_wins) #33
max(ufc_master$B_wins) #31

max(ufc_master$R_losses) #18
max(ufc_master$B_losses) #15

#NORMALISATION

ufc_master$B_current_lose_streak <- ufc_master$B_current_lose_streak/6
ufc_master$B_current_win_streak <- ufc_master$B_current_win_streak/12
ufc_master$R_current_lose_streak <- ufc_master$R_current_lose_streak/7
ufc_master$R_current_win_streak <- ufc_master$R_current_win_streak/16
ufc_master$B_age <- ufc_master$B_age/47
ufc_master$R_age <- ufc_master$R_age/47
ufc_master$R_wins <- ufc_master$R_wins/33
ufc_master$B_wins <- ufc_master$B_wins/31
ufc_master$R_losses <- ufc_master$R_losses/18
ufc_master$B_losses <- ufc_master$B_losses/15

#dropping the columns that are not needed
ufc_master= subset(ufc_master, select = -c (ufc_master$R_odds, ufc_master$B_odds, ufc_master$date, ufc_master$location, ufc_master$country, ufc_master$title_bout, ufc_master$empty_arena, ufc_master$B_match_weightclass_rank, ufc_master$R_match_weightclass_rank,ufc_master$`R_Women's Featherweight_rank` ,ufc_master$`R_Women's Flyweight_rank`, ufc_master$R_Heavyweight_rank, ufc_master$`R_Light Heavyweight_rank`,  ufc_master$R_Middleweight_rank,  ufc_master$R_Welterweight_rank,  ufc_master$R_Lightweight_rank, ufc_master$R_Featherweight_rank, ufc_master$R_Bantamweight_rank, ufc_master$R_Flyweight_rank, ufc_master$`R_Pound-for-Pound_rank`, ufc_master$`R_Women's Bantamweight_rank`,ufc_master$`R_Women's Strawweight_rank`,ufc_master$B_match_weightclass_rank,ufc_master$`B_Women's Featherweight_rank` ,ufc_master$`B_Women's Flyweight_rank`, ufc_master$B_Heavyweight_rank, ufc_master$`B_Light Heavyweight_rank`,  ufc_master$B_Middleweight_rank,  ufc_master$B_Welterweight_rank,  ufc_master$B_Lightweight_rank, ufc_master$B_Featherweight_rank, ufc_master$B_Bantamweight_rank, ufc_master$B_Flyweight_rank, ufc_master$`B_Pound-for-Pound_rank`, ufc_master$`B_Women's Bantamweight_rank`,ufc_master$`B_Women's Strawweight_rank`))
ufc_master= subset(ufc_master, select = -c(3,4,5,6,7,8,9,11,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119))
ufc_master = subset(ufc_master, select = -c(70))



is.na(training_data$B_avg_SIG_STR_landed)

training_data[152,26] <- 0
training_data[197,26] <- 0
training_data[is.na(training_data)] <- 0
validation_data[is.na(validation_data)] <- 0


ufcdata <- ufc_master
na.omit(ufcdata)
ufcdata[is.na(ufcdata)] <- 0

#splitting the data into 3 sections for the training, validation and test splits
set.seed(222)
inp <- sample(3, nrow(ufc_master), replace = TRUE, prob = c(0.8, 0.1, 0.1))
training_data <- ufc_master[inp==1,  ]
validation_data <- ufc_master[inp==2,  ]
test_data <- ufc_master[inp==3, ]


#Neural Network

install.packages("neuralnet")
library(neuralnet)

require(neuralnet)

str(training_data)
#Removing fighter names to use correlation matrix
corData = subset(validation_data, select = -c(1,2))

#Correlation Matrix
res <- cor(corData)
round(res, 2)

#Neural Network Model
nen=neuralnet(Winner ~ avg_td_dif + avg_sub_att_dif + reach_dif + total_title_bout_dif
              + longest_win_streak_dif + win_streak_dif + lose_streak_dif + R_age + R_win_by_Decision_Split
              + B_win_by_Decision_Split + R_current_lose_streak
              + R_current_win_streak + B_current_lose_streak + B_current_win_streak, 
             data=training_data,learningrate = 0.001,stepmax = 1000000,
             rep = 1, hidden = 2)

#Predicting the outcomes....Validation set was changed to Test set to get final results.
Predict=neuralnet::compute(nen,test_data)

#Produced results
Predict$net.result
prob <- Predict$net.result

#changing the results to round them up
pred <- ifelse(prob>0.5, 1, 0)
pred
#accuracy gathering
confusionMatrix(table(pred, test_data$Winner ))

plot(nen)
#viewing the different tables
str(training_data)
str(validation_data$Winner)
table(training_data$B_Stance)

#accuracy gathering
confusionMatrix(table(pred, validation_data$Winner ))


#H2O GRID SEARCH FOR THE RANDOM FOREST 
h2o.init()

h2o_training_data <- as.h2o(training_data)
h2o_validation_data <- as.h2o(validation_data)
h2o_test_data <- as.h2o(test_data)

y <- "Winner"
x <- setdiff(colnames(ufc_master), y)

gbm_params1 <- list(learn_rate = c(0.001,0.01, 0.1)
                  
                   )

gbm_grid4 <- h2o.grid("gbm", x = , y = y,
                      grid_id = "gbm_grid4",
                      training_frame = h20_training_data,
                      validation_frame = h2o_validation_data,
                      ntrees = 100,
                      seed = 1,
                      hyper_params = gbm_params1)


# Get the grid results, sorted by validation residual deviance
gbm_gridperf1 <- h2o.getGrid(grid_id = "gbm_grid4",
                             sort_by = "residual_deviance",
                             decreasing = FALSE
                             )
print(gbm_gridperf1)
gbm_gridperf1@summary_table

#Grab the top GBM model, chosen by validation AUC
best_gbm1 <- h2o.getModel(gbm_gridperf1@model_ids[[1]])
best_gbm1


#estimate of top model performance
best_gbm_perf1 <- h2o.performance(model = best_gbm1,
                                  newdata = h2o_validation_data)
best_gbm_perf1
h2o.(best_gbm_perf1)
h2o.auc(best_gbm_perf1)


# Look at the hyperparameters for the best model
print(best_gbm1@model[["model_summary"]])


summary(gbm_grid1, show_stack_traces = TRUE)

gbm_grid4

#RANDOM FOREST ALGORITHM

library(randomForest)

#training random forest model
rf <- randomForest( 
  Winner ~ avg_td_dif + avg_sub_att_dif + reach_dif + total_title_bout_dif
  + longest_win_streak_dif + win_streak_dif + lose_streak_dif + R_age + R_win_by_Decision_Split
  + B_win_by_Decision_Split + R_current_lose_streak + R_current_win_streak + B_current_lose_streak
  + B_current_win_streak , 
  data = training_data, learningrate = 0.01
)

#predicting for the new test dataset....Validation set was changed to Test set to get final results.
rf.predict <- predict(
  rf, 
  newdata = test_data, 
  type="class" 
)
#Rounding results to 0 or 1
rf.pred <- ifelse(rf.predict>0.5, 1, 0)

confusionMatrix(table(rf.pred,test_data$Winner))





