library(tm)
library(SnowballC)
library(tidyr)
library(NLP)
library(tokenizers)
library(lsa)
library(dplyr)
library(RWeka)
install.packages("rjava")
install.packages("qdap")
path="c://Users/gandixit/Desktop/SET 2/"
nonsense=read.table(file="C://Users/gandixit/Desktop/nonsense1.txt",header = T)
class(nonsense)
head(nonsense)
cus_stop=as.vector(nonsense$custom_words)
stopwords()
# LSA

myMatrix = textmatrix(path, stemming = F,language = "english",
                      minWordLength = 2,removeNumbers = T, stopwords=cus_stop)
myMatrix = lw_logtf(myMatrix) * gw_idf(myMatrix)


myLSAspace = lsa((myMatrix), dims=dimcalc_share())
as.textmatrix(myLSAspace)
lsadf=as.matrix(myLSAspace)
class(lsadf)
write.csv(as.textmatrix(myLSAspace),"set21.csv")
interim=read.csv(file = "C:/Users/gandixit/Documents/set21.csv",header = T)

interim$new=apply(interim[,2:ncol(interim)],1,function(x) sum(x))
set21=select(interim,X,new)
View(set21)
set22=arrange(set21,desc(new))
set23=unique.data.frame(set22)
number=round(nrow(set23)*0.2)
set23=set23[1:number,]

write.csv(set23,"set23.csv")


library(qdap)