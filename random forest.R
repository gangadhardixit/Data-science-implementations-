library(rpart)
library(rpart.plot)
employee_data=read.csv(file="c://Users/gandixit/Desktop/WA_Fn-UseC_-HR-Employee-Attrition.csv")
View(employee_data)
attach(employee_data)
summary(employee_data)
if(sum(is.na(employee_data))>0)   #check if there are any missing values or NULL fields 
{     
  print("there are missing values")
}
else
  print("no missing values!!")

set.seed(111)    #set seed to avoid randomeness 

randomforestmodel=randomForest::randomForest(Attrition~.,mtry=5,data=employee_data)
randomForest::importance(randomforestmodel)
randomForest::varImpPlot(randomforestmodel)

prediction=predict(object = randomforestmodel,newdata = employee_testdata,type = "response")

table(testdata2=employee_testdata$Attrition,predicteddata=prediction)

rpart