library(Boruta)

traindata=read.csv(file = "c://Users/gandixit/Desktop/R tutorial/Boruta/train_u6lujuX_CVtuZ9i.csv"
                   ,header = T)


View(traindata)


summary(traindata)

traindata[traindata==""]=NA
sum(is.na(traindata))

traindata=traindata[complete.cases(traindata),]

str(traindata)

convert=c(2:6,11:13)
attach(traindata)
boruta.train=Boruta(Loan_Status ~ .- Loan_ID,data = traindata,doTrace=2)
summary(boruta.train)
class(boruta.train)
print(boruta.train)

plot(boruta.train,xlab="",xaxt="n")

final.boruta=TentativeRoughFix(boruta.train)

print(final.boruta)

lz<-lapply(1:ncol(boruta.train$ImpHistory),function(i)
  boruta.train$ImpHistory[is.finite(boruta.train$ImpHistory[,i]),i])
names(lz) <- colnames(boruta.train$ImpHistory)
Labels <- sort(sapply(lz,median))
axis(side = 1,las=2,labels = names(Labels),
       at = 1:ncol(boruta.train$ImpHistory), cex.axis = 0.7)


plot(final.boruta)

lz<-lapply(1:ncol(final.boruta$ImpHistory),function(i)
  final.boruta$ImpHistory[is.finite(final.boruta$ImpHistory[,i]),i])
names(lz) <- colnames(final.boruta$ImpHistory)
Labels <- sort(sapply(lz,median))
axis(side = 1,las=2,labels = names(Labels),
     at = 1:ncol(final.boruta$ImpHistory), cex.axis = 0.7)



windows()


getSelectedAttributes(final.boruta,withTentative = F)

attStats(final.boruta)

