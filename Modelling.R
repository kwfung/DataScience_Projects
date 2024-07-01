## read file 
happy = read.csv("SimpleData.csv")
View(happy)
summary(happy)

### check for collinearity 
cor = cor(na.omit(happy[,4:20]))

###Linear Model 
###############

#test and train set 
happyTrain = happy[happy$Year <= 2016,]
happyTest = happy[happy$Year > 2016,]

#model 
mod1 = lm(Life.Ladder ~. , data = happyTrain[,4:20])
summary(mod1)

## dealing with NA to predict 
NA_count = rep(0,20)
for (i in 1:20) {
  NA_count[i] = sum(is.na(happyTest[,i]))
}
NA_count
NA_percentage = NA_count/283
NA_percentage

happyTest_noNA = na.omit(happyTest)  #out of 283 obs, left with 73 after removing all na

pred1 = predict(mod1, newdata= happyTest_noNA)
pred1

SSE = sum((pred1 - happyTest_noNA$Life.Ladder)^2)
train.mean = mean(happyTrain$Life.Ladder)
SST = sum((train.mean - happyTest_noNA$Life.Ladder)^2)
# Finally, we have the OSR2
1 - SSE/SST
## OSR2 very high of 0.95

MAE = mean(abs(pred1 - happyTest_noNA$Life.Ladder))
MAE
RMSE = sqrt(mean((pred1 - happyTest_noNA$Life.Ladder)^2))
RMSE

##visualization 
happyTest_noNA$pred = pred1
plot(happyTest_noNA$Country.name, happyTest_noNA$Life.Ladder, type="s", col="blue")
# We can add the predicted prices in red using the "lines" command
?plot
lines(happyTest_noNA$Country.name, happyTest_noNA$Life.Ladder, type="b", col="blue")
lines(happyTest_noNA$Country.name, happyTest_noNA$pred, type="b", col="red")

###Random Forest 
###############

library(caret)
library(randomForest)

split = createDataPartition(happy$Life.Ladder, p = 0.7, list = FALSE)
happyTrain.rf = happy[split,3:length(happy)]
happyTest.rf = happy[-split,3:length(happy)]

##### Cross Validation ####
set.seed(144)
###omits na for CV 

happyTrain.rf_noNA = na.omit(happyTrain.rf) ### 446 out of 1196 obs remaining
happyTest.rf_noNA = na.omit(happyTest.rf) ### 176 out of 508 obs remaining

rf.cv = train(y = happyTrain.rf_noNA$Life.Ladder,
              x = subset(happyTrain.rf_noNA, select=-c(Life.Ladder)),
              method="rf", nodesize=20, ntree=500,
              trControl=trainControl(method="cv", number=5),
              tuneGrid=data.frame(mtry=seq(1,50,2)))

rf.cv
plot(rf.cv$results$mtry, rf.cv$results$Rsquared, type = "l")

##find the best model 
rf.mod.cv = rf.cv$finalModel

pred.train = predict(rf.mod.cv, newdata=happyTrain.rf_noNA)
pred.test = predict(rf.mod.cv, newdata=happyTest.rf_noNA)

# Calculate R2 and OSR2 as usual.
# These number may not be very good because we only used 10 trees in our RF.
mean_train = mean(happyTrain.rf_noNA$Life.Ladder)
SSETrain = sum((pred.train - happyTrain.rf_noNA$Life.Ladder)^2)
SSTTrain = sum((happyTrain.rf_noNA$Life.Ladder - mean_train)^2)
R2 = 1 - SSETrain/SSTTrain
R2

SSETest = sum((pred.test - happyTest.rf_noNA$Life.Ladder)^2)
SSTTest = sum((happyTest.rf_noNA$Life.Ladder - mean_train)^2)
OSR2 = 1 - SSETest/SSTTest
OSR2

importance(rf.mod.cv)


###Time Series 
###############
str(happy)
library(dplyr)

## create lag variable by country name
happy <- 
  happy %>% 
  group_by(Country.name) %>%
  mutate(lag.value=dplyr::lag(Life.Ladder,n=1,default=NA))

happyTrain_ts = happy[happy$Year <= 2016,]
happyTest_ts = happy[happy$Year > 2016,]

mod2 = lm(Life.Ladder ~. , data = happyTrain_ts[,4:21])
summary(mod2)

## dealing with NA to predict 
happyTest_tsNoNA = na.omit(happyTest_ts)  #out of 283 obs, left with 73 after removing all na

pred2 = predict(mod2, newdata= happyTest_tsNoNA)
pred2

SSE = sum((pred2 - happyTest_tsNoNA$Life.Ladder)^2)
train.mean = mean(happyTrain_ts$Life.Ladder)
SST = sum((train.mean - happyTest_tsNoNA$Life.Ladder)^2)
# Finally, we have the OSR2
1 - SSE/SST
## OSR2 very high of 0.95

MAE = mean(abs(pred2 - happyTest_tsNoNA$Life.Ladder))
MAE
RMSE = sqrt(mean((pred2 - happyTest_tsNoNA$Life.Ladder)^2))
RMSE
