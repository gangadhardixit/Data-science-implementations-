adadata=read.csv(file = "C:/Users/gandixit/Desktop/R tutorial/SVM/Iris.csv",header = T)
install.packages("adabag")
library(adabag)
summary(adadata)
str(adadata)


set.seed(111)
rownumber1=sample(1:nrow(adadata),0.7*nrow(dataset))
adadata=adadata[,-1]
traindata=dataset[rownumber1,col]
testdata=dataset[-rownumber1,col]

nrow(adadata)
nrow(traindata)
nrow(testdata)


boosting()

adamodel=boosting(formula =Species ~ .,data = traindata, boos = T,mfinal = 100)


modelpre=predict(adamodel,newdata = testdata,type="class")

table(actualdata=testdata$Species,predictedvalues=modelpre$confusion)

modelpre$confusion
modelpre$votes
modelpre$class
modelpre$error

adamodel$trees
adamodel$importance

adamodel$weights

