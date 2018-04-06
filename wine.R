library (MASS)
library(car)

library(caTools)
library(corrgram)
wine_data=read.csv(file = "C://Users/gandixit/Desktop/wine/wine_dataset.csv",header = T)
str(wine_data)


red_wine<-wine_data[which(wine_data$style=="red"),]
white_wine<-wine_data[which(wine_data$style=="white"),]


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





######regression #############

red_wine_train$quality=as.integer(red_wine_train$quality)
red_wine_test$quality=as.integer(red_wine_test$quality)

initial_logistic_model = lm(quality ~ . , data = red_wine_train[-13] )
summary(initial_logistic_model)
step <- stepAIC(initial_logistic_model, direction="both")
step

redwinr_model2=lm(formula = quality ~ volatile_acidity + chlorides + free_sulfur_dioxide + 
                    total_sulfur_dioxide + pH + sulphates + alcohol, data = red_wine_train[-13])
summary(redwinr_model2)

vif(redwinr_model2)

predict1<-predict(redwinr_model2,red_wine_test[-13])

cor(red_wine_test$quality,predict1)^2







######plot vaiables 
table(wine_data$quality)
ggplot(data=wine_data,aes(x=wine_data$fixed_acidity, fill=quality)) + geom_bar()
#Only 198 out of6497 have high quality --3%   

#fixed_acidity
ggplot(data = wine_data,aes(x=fixed_acidity,fill=quality)) + geom_bar()
summary(wine_data$fixed_acidity)


#fixed_acidity
windows()
ggplot(data = red_wine,aes(x=fixed_acidity,fill=quality),t) + geom_histogram(binwidth = 100, position = "dodge")+ ggtitle("red wine")

windows()
ggplot(data = white_wine,aes(x=fixed_acidity,fill=quality)) + geom_histogram(binwidth = 100, position = "dodge")




#volatile_acidity
windows()
ggplot(data = red_wine,aes(x=volatile_acidity,fill=quality),t) + geom_histogram(binwidth = 100, position = "dodge")+ ggtitle("red wine")

windows()
ggplot(data = white_wine,aes(x=volatile_acidity,fill=quality)) + geom_histogram(binwidth = 100, position = "dodge")




#citric_acid
windows()
ggplot(data = red_wine,aes(x=citric_acid,fill=quality),t) + geom_histogram(binwidth = 100, position = "dodge")+ ggtitle("red wine")

windows()
ggplot(data = white_wine,aes(x=citric_acid,fill=quality)) + geom_histogram(binwidth = 100, position = "dodge")




#residual_sugar
windows()
ggplot(data = red_wine,aes(x=residual_sugar,fill=quality),t) + geom_histogram(binwidth = 100, position = "dodge")+ ggtitle("red wine")

windows()
ggplot(data = white_wine,aes(x=residual_sugar,fill=quality)) + geom_histogram(binwidth = 100, position = "dodge")


#chlorides
windows()
ggplot(data = red_wine,aes(x=chlorides,fill=quality),t) + geom_histogram(binwidth = 100, position = "dodge")+ ggtitle("red wine")

windows()
ggplot(data = white_wine,aes(x=chlorides,fill=quality)) + geom_histogram(binwidth = 100, position = "dodge")





#free_sulfur_dioxide  total_sulfur_dioxide density pH
windows()
ggplot(data = red_wine,aes(x=free_sulfur_dioxide,fill=quality),t) + geom_histogram(binwidth = 100, position = "dodge")+ ggtitle("red wine")

windows()
ggplot(data = white_wine,aes(x=free_sulfur_dioxide,fill=quality)) + geom_histogram(binwidth = 100, position = "dodge")



#free_sulfur_dioxide  total_sulfur_dioxide density pH
windows()
ggplot(data = red_wine,aes(x=total_sulfur_dioxide,fill=quality),t) + geom_histogram(binwidth = 100, position = "dodge")+ ggtitle("red wine")

windows()
ggplot(data = white_wine,aes(x=total_sulfur_dioxide,fill=quality)) + geom_histogram(binwidth = 100, position = "dodge")



#free_sulfur_dioxide  total_sulfur_dioxide density pH
windows()
ggplot(data = red_wine,aes(x=density,fill=quality),t) + geom_histogram(binwidth = 100, position = "dodge")+ ggtitle("red wine")

windows()
ggplot(data = white_wine,aes(x=density,fill=quality)) + geom_histogram(binwidth = 100, position = "dodge")





#free_sulfur_dioxide  total_sulfur_dioxide density pH
windows()
ggplot(data = red_wine,aes(x=pH,fill=quality),t) + geom_histogram(binwidth = 100, position = "dodge")+ ggtitle("red wine")

windows()
ggplot(data = white_wine,aes(x=pH,fill=quality)) + geom_histogram(binwidth = 100, position = "dodge")

#sulphates alcohol

windows()
ggplot(data = red_wine,aes(x=sulphates,fill=quality),t) + geom_histogram(binwidth = 100, position = "dodge")+ ggtitle("red wine")

windows()
ggplot(data = white_wine,aes(x=sulphates,fill=quality)) + geom_histogram(binwidth = 100, position = "dodge")


windows()
ggplot(data = red_wine,aes(x=alcohol,fill=quality),t) + geom_histogram(binwidth = 100, position = "dodge")+ ggtitle("red wine")

windows()
ggplot(data = white_wine,aes(x=alcohol,fill=quality)) + geom_histogram(binwidth = 100, position = "dodge")

windows()
ggplot(data = wine_data,aes(x=alcohol,fill=quality)) + geom_histogram(binwidth = 100, position = "dodge")



windows()
corrgram(wine_data, order=T, lower.panel=panel.shade,
         upper.panel=NULL, text.panel=panel.txt,
         main="winedata")  
windows()
corrgram(red_wine, order=T, lower.panel=panel.shade,
         upper.panel=NULL, text.panel=panel.txt,
         main="winedata")  
windows()
corrgram(white_wine, order=T, lower.panel=panel.shade,
         upper.panel=NULL, text.panel=panel.txt,
         main="winedata")  


set.seed(111)    #set seed to avoid randomeness 
rownumber1.1=sample(1:nrow(wine_data),0.7*nrow(wine_data))

wine_traindata=wine_data[rownumber1.1,]
wine_testdata=wine_data[-rownumber1.1,]

nrow(wine_data)
nrow(wine_traindata)
nrow(wine_testdata)





#Tree model
library(rpart)
winedecision=rpart(formula=quality ~.,data = wine_traindata)  #build model 
summary(winedecision)


winepredict=predict(object = winedecision,newdata = wine_testdata,type = "class")

table(testdata=wine_testdata$quality,preictedvalues=winepredict)




nrow(nps_testdata)
#49% accuracy



###################outlier treatment###########




outlierTreatment <- function(data){
  print("this will do the outlier treatment of all interger and numeric columns")
  if (class(data) == "data.frame"){
    
    for (colName  in names(data)){
      print (paste(colName,"at index" ,match(colName,names(data) ) ) )
      if ( class(data[,colName]) == "integer" | class(data[,colName]) == "numeric" ) {
        
        iqr = quantile(data[,colName])[4] -quantile(data[,colName])[2]
        
        lower_bound=quantile(data[,colName])[2] - 1.5*iqr
        upper_bound=quantile(data[,colName])[4] + 1.5*iqr
        data[,colName][which(data[,colName] >upper_bound)] <- upper_bound
        data[,colName][which(data[,colName] <lower_bound)] <- lower_bound
        
        boxplot(data[,colName])
        
      }
      else
      {
      
        print(paste(colName," :not elegible for outlier treatment"))
      }
      
    }
    
  }
  else 
    print ("Error: Input variable should be a data.frame !!")
  
  return(data)
}

##call outlier###
str(wineoutlier_treated)
wineoutlier_treated=wine_data
wineoutlier_treated$quality=as.factor(wineoutlier_treated$quality)
wineoutlier_treated<-outlierTreatment(wineoutlier_treated)
summary(wineoutlier_treated$quality)
wineoutlier_treated$quality<-as.character(wineoutlier_treated$quality)
summary(as.factor(wineoutlier_treated$quality))



###################Random forest
# for redwine
set.seed(111)    #set seed to avoid randomeness 
library(randomForest)
str(red_wine_train)
summary(white_wine$quality)
randomforestmodel=randomForest::randomForest(quality ~ .,mtry=5,data=red_wine_train)
randomForest::importance(randomforestmodel)
randomForest::varImpPlot(randomforestmodel)
summary(red_wine_test$quality)
prediction=predict(object = randomforestmodel,newdata = red_wine_test,type = "response")

table(testdata2=red_wine_test$quality,predicteddata=prediction)

#for white wine
set.seed(111)    #set seed to avoid randomeness 
library(randomForest)
str(white_wine_train)

randomforestmodel=randomForest::randomForest(quality ~ .,mtry=10,data=white_wine_train)
randomForest::importance(randomforestmodel)
randomForest::varImpPlot(randomforestmodel)
summary(white_wine_test$quality)
randomforestmodel$confusion
prediction=predict(object = randomforestmodel,newdata = white_wine_test,type = "response")

table(testdata2=white_wine_test$quality,predicteddata=prediction)





