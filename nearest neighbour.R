
library(class)
cancerdata=read.csv(file = "c://Users/gandixit/Desktop/Prostate_Cancer.csv",stringsAsFactors = F)
View(cancerdata)
cancerdata=cancerdata[,-1]


cancerdata$diagnosis<- factor(cancerdata$diagnosis_result, levels = c("B", "M"), labels = c("Benign", "Malignant"))

normalize=function(x)
{
  return((x-min(x))/(max(x)-min(x)))
}

cancerdata_n= as.data.frame(lapply(cancerdata[2:9],normalize))
class(cancerdata_n)
summary(cancerdata_n$radius)



set.seed(111)
#numberrows=sample(1:nrow(cancerdata_n),0.7*nrow(cancerdata_n))
#cancer_traindata=cancerdata_n[numberrows,]
#cancer_testdata=cancerdata_n[-numberrows,]

cancer_traindata=cancerdata_n[1:65,]
cancer_testdata=cancerdata_n[66:100,]
cancer_train_labels=cancerdata[1:65,1]
cancer_test_labels=cancerdata[66:100,1]
asfactor=factor(cancerdata$diagnosis_result,levels=c("M","B"),labels=c("Malignant","Benign"))

sum(is.na(cancerdata))

knnmodel =knn(train = cancer_traindata,test = cancer_testdata,cl=cancer_train_labels,k=4)

library(gmodels)


CrossTable(x=cancer_test_labels,y=knnmodel,prop.chisq = F)




