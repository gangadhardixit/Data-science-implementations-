red_wine<-wineoutlier_treated[which(wineoutlier_treated$style=="red"),]
white_wine<-wineoutlier_treated[which(wineoutlier_treated$style=="white"),]


#reddata
str(red_wine)
red_wine$quality<-as.factor(red_wine$quality)
row=sample.split(red_wine$quality,SplitRatio = 0.7,group = NULL)
summary(row)
red_wine_train=red_wine[row,]
red_wine_test=red_wine[!row,]
summary(red_wine_train$quality)
summary(red_wine_test$quality)



#white
str(white_wine)
white_wine$quality<-as.factor(white_wine$quality)
row_white=sample.split(white_wine$quality,SplitRatio = 0.7,group = NULL)
summary(row_white)
white_wine_train=white_wine[row_white,]
white_wine_test=white_wine[!row_white,]
summary(white_wine_train$quality)
summary(white_wine_test$quality)



#red wine model
set.seed(111)    #set seed to avoid randomeness 
library(randomForest)
str(red_wine_train)

randomforestmodel=randomForest::randomForest(quality ~ .,mtry=5,data=red_wine_train)
randomForest::importance(randomforestmodel)
randomForest::varImpPlot(randomforestmodel)
summary(red_wine_test$quality)
prediction=predict(object = randomforestmodel,newdata = red_wine_test,type = "response")

table(testdata2=red_wine_test$quality,predicteddata=prediction)



#white wine model
set.seed(111)    #set seed to avoid randomeness 
library(randomForest)
str(white_wine_train)

randomforestmodel=randomForest::randomForest(quality ~ .,mtry=5,data=white_wine_train)
randomForest::importance(randomforestmodel)
randomForest::varImpPlot(randomforestmodel)
summary(white_wine_train$quality)
prediction=predict(object = randomforestmodel,newdata = white_wine_test,type = "response")

table(testdata2=white_wine_test$quality,predicteddata=prediction)



