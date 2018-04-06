
library(caret)
library(corrplot)
library(Boruta)
library(recommenderlab)
install.packages("ranger")
install.packages("Boruta")
elearning=read.csv(file = "c://Users/gandixit/Desktop/elearning/Machine Learning Dataset Registration v1.csv",header = T)


####Sampling -splitting data into 70:30 ratio
r=nrow(elearning)*0.7
elearning_train=elearning[1:r,]
elearning_test=elearning[1680-1176:nrow(elearning),]
summary(elearning)

#####check for any missing values 
elearning[elearning==""]=NA

#elearning[elearning=="NULL"]=NA

sum(is.na(elearning))

set.seed(111)
###

#boruta.train <- Boruta(OT_ID ~ ., data = elearning, doTrace = 2)


#str(elearning)
#elearning=elearning[,-1]
#elearning=elearning[,-2]
#elearning=elearning[,-4]

##select only user id ,course and completion status 
elearning_train=elearning_train[,c(2,5,6)]
elearning_test=elearning_test[,c(2,5,6)]
nrow(elearning)
nrow(elearning_train)
nrow(elearning_test)

#courses= unique(elearning$OT_ID)
#courses=courses[order(courses)]

#target_courses=courses
#sno=1:length(target_courses)
#courses_ident=cbind(target_courses,sno)

##colnames(courses_ident) <- c("individual_course","sno")

library(reshape2)
elearning_train=elearning_train[,c(2,1,3)]
elearning_test=elearning_test[,c(2,1,3)]
View(elearning)


#elearning_train$COMPLETION_STATUS_DESC=as.character(elearning_train$COMPLETION_STATUS_DESC)
#elearning_test$COMPLETION_STATUS_DESC=as.character(elearning_test$COMPLETION_STATUS_DESC)



g_train=acast(elearning_train,STUDENT_CECID ~ OT_ID,value.var = "COMPLETION_STATUS_DESC",fun.aggregate = mean)
g_train=acast(elearning_test,STUDENT_CECID ~ OT_ID,value.var = "COMPLETION_STATUS_DESC",fun.aggregate = mean)


class(g_train)

R=as.matrix(g_train)
r=as(R,"realRatingMatrix")
r
getRatingMatrix(r)
class(R)

r_m=normalize(r)



rec=Recommender(r[1:nrow(r)],method="UBCF",
                param=list(normalize = "Z-score",method="Cosine",nn=3, minRating=1))





summary(rec)

print(rec)


recom <- predict(rec, r[1:nrow(r)], n=5)
recom
names(getModel(rec))
#recommenderlab::getRatings(recom)
#class(recom)
#print(recom)
#class(recom)
as(recom,matrix)

#finalone=as.matrix(recom)
#finalone
rec_list<-as(recom,"list")
head(summary(rec_list))

rec_list[[4]]

length(rec_list)

rec_list[[5]]



############

for ( u in 1:length(elearning[,2]))
{
  # Read userid and movieid from columns 2 and 3 of test data
  userid <- test[u,2]
  movieid<-test[u,3]
  # Get as list & then convert to data frame all recommendations for user: userid
  u1<-as.data.frame(rec_list[[userid]])
  # Create a (second column) column-id in the data-frame u1 and populate it with row-names
  # Remember (or check) that rownames of u1 contain are by movie-ids
  # We use row.names() function
  u1$id<-row.names(u1)
  # Now access movie ratings in column 1 of u1
  x= u1[u1$id==movieid,1]
  # print(u)
  # print(length(x))
  # If no ratings were found, assign 0. You could also
  #   assign user-average
  if (length(x)==0)
  {
    ratings[u] <- 0
  }
  else
  {
    ratings[u] <-x
  }
  
}
length(ratings)
tx<-cbind(test[,1],round(ratings))
# Write to a csv file: submitfile.csv in your folder
write.table(tx,file="submitfile.csv",row.names=FALSE,col.names=FALSE,sep=',')


f1=getRatingMatrix(recom)
class(f1)  
library(Matrix)
writeMM(f1,file = "test.txt")

str(recom)
as(recom, "matrix")[1:4,1:3]

as.data.frame(rec_list)


lapply(rec_list, cat, "\n", file="test.txt", append=TRUE)

#write.table(rec_list,file="test.txt",append = T,sep = "->")

df<-data.frame(words = unlist(rec_list))

write.table(df,file = "recomend.txt")
View(df)

rec_list



write.table(df,file = "recomend1.csv",sep = ",")