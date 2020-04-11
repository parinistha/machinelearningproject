#import the dataset letter-recognition.data 

head(letter.recognition) 

#splitting data into training and test sets 
library(caret)
set.seed(123)
train_index<-createDataPartition(letter.recognition$V1,p=0.75,list=FALSE)
train<-letter.recognition[train_index,]
test<-letter.recognition[-train_index,]
summary(test)
summary(train)

#performimg fisher's linear discriminant analysis using the function lda()
library(MASS)
lr.lda<-lda(V1 ~.,data=train)
lr.lda

#prediction of test data oputput
lr.predict<- predict(lr.lda,test,type="class")
#plot(test,lr.predict)
lr.predict

#finding the accuracy of the original output with the test output
table(lr.predict$class,test$V1)
confusionMatrix(table(lr.predict$class,test$V1))

#the accuracy of the model is 69.7%