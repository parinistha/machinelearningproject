#import the dataset data_banknote_authentication.txt
head(data_banknote_authentication) 

#using the function lfda() to reduce the dimension from 4 to 3

library(lfda)
x<-data_banknote_authentication[,-5]
y<-data_banknote_authentication[,5]
r<-3
model<-lfda(x,y,r,metric = "plain")
newdata<-model$Z
newdata
newdata1<-data.frame(V1=y ,
                     V2=newdata[,1],
                     V3=newdata[,2],
                     V4=newdata[,3]
)
newdata1

plot(x=model,labels=y)


#splitting data into training and test sets 
library(caret)
set.seed(123)
train_index<-createDataPartition(newdata1$V1,p=0.75,list=FALSE)
train<-newdata1[train_index,]
test<-newdata1[-train_index,]
summary(test)
summary(train)

#performimg fisher's linear discriminant analysis using the function lda()
library(MASS)
bn.lda<-lda(V1 ~.,data=train)
bn.lda

#prediction of test data oputput
bn.predict<- predict(bn.lda,test,type="class")
#plot(test,bn.predict)
bn.predict

#finding the accuracy of the original output with the test output
table(bn.predict$class,test$V1)
confusionMatrix(table(bn.predict$class,test$V1))

#the accuracy of the model is 98.25%