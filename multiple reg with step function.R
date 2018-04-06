View(compdata)
attach(compdata)
set.seed(111)
rownumbers=sample(1:nrow(compdata),0.7*nrow(compdata))

comptraindata=compdata[1:rownumbers,]
comptestdata=compdata[-rownumbers,]

nrow(comptraindata)
nrow(comptestdata)

colnames(comptraindata)
compregression=lm(formula=price~.,data = comptraindata)
summary(compregression)

usestep=step(object = compregression,direction = "backward",trace = 0)
summary(usestep) 

predicteddata=predict(object = compregression,data=comptestdata)
View(comptestdata)
comptestdata$price[1:10]
predicteddata[1:10]