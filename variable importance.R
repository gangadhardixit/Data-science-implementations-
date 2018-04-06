library(Boruta)
library(ranger)
boruta.train=Boruta(quality ~ .,data = wine_data,doTrace=2)
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

###################



boruta.train=Boruta(quality ~ .,data = white_wine,doTrace=2)
summary(boruta.train)
class(boruta.train)
print(boruta.train)

plot(boruta.train,xlab="",xaxt="n")

final.boruta=TentativeRoughFix(boruta.train)

print(final.boruta)


attStats(final.boruta)