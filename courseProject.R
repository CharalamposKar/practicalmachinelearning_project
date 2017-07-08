##Loading required libraries

library(lattice)
library(ggplot2)
library(caret)
library(rpart)
library(rattle)
library(randomForest)

##loading the training set.Working directory must be set....
training<-read.csv("pml-training.csv")

##dim(training)  
##head(training)  if you want to see some details concerning the training set
##summary(training)


##Create data partition 70% training and 30% testing
##The testing set provided will be used for validating the  model 

set.seed(333);
inTrain<-createDataPartition(training$classe,p=0.70,list=FALSE)
newtraining<-training[inTrain,] 
newtesting<-training[-inTrain,]

##Preprocessing includes removing features with high NA percentage
##Remove features with zero variability
##Normalize
##Remove features that are irrelevant such as ,counters, timestampts,dates etc...
##There were a lot of features and samples so reducing the original number 
##and cleaning the dat would be very beneficial. 

##See the percentage of NA values for every feature 
NApercentages<-apply(newtraining, 2, function(col)sum(is.na(col))/length(col))
which(NApercentages>0.90) ##see the features that are to be excluded from the analysis

##Remove features with NA percentage more than 90%
newtraining <- subset(newtraining, select = -c(which(NApercentages>0.90)) )

##Removefeatures with non zero variability
nsc<-nearZeroVar(newtraining,saveMetrics = TRUE) ## 
newtraining <- newtraining[,nsc$nzv==FALSE]

##See what variables are categorical
catvar<-sapply(newtraining,class) ## except form the classe which is the output variable 
                                  ##only user_name is also categorical therefore this variable will be transformed

##table(newtraining$user_name) ## exploring the user_name variable

##Remove all irrelevant inputs
##Remove the first column  which is a count variable and the date and time columns
newtraining<-newtraining[,-c(1,3,4,5,6)]

##Standarize
preobj<-preProcess(newtraining[,-54],method = c("center","scale"))
newtraining1<-predict(preobj,newtraining[,-54]) 
newtraining<-cbind(newtraining1,newtraining[,54])
colnames(newtraining)[54] <- "classe"

##Train a classification tree 
set.seed(333);
modelFit<-train(classe~.,method="rpart",data=newtraining)

##Create a plot to demonstrate the tree structure
fancyRpartPlot(modelFit$finalModel)

##Train random forest (Takes a lot of time aprox 12 hours..... please use with caution)
##I have saved to my working directory a trained model so that I can run this script without retraining....
##If yu want to train the model use line 56 

##set.seed(333);
##modelFit2<-train(classe~.,method="rf",data=newtraining,prox=TRUE)
load("my_model1.rda")

##Use the same features with the testing set 
##As a result use the same features and standarize

usedfeatures <- colnames(newtraining)
newtesting<-newtesting[usedfeatures]
dim(newtesting)
newtesting1<-predict(preobj,newtesting[,-54])
newtesting<-cbind(newtesting1,newtesting$classe)
colnames(newtesting)[54] <- "classe"

##see accuracy at the testing set

##for random forest
predictions2<-predict(modelFit2,newdata=newtraining)
CM22<-confusionMatrix(predictions2,newtraining$classe)
CM22

##for the simple classification tree
predictions<-predict(modelFit,newdata=newtesting)
CM2<-confusionMatrix(predictions,newtesting$classe)
CM2

##Have a look at the accuracy for the training set 
##for random forest 
##predictions2<-predict(modelFit2,newdata=newtraining)
##CM12<-confusionMatrix(predictions2,newtraining$classe)
##CM12
##for simple classification tree
##predictions<-predict(modelFit,newdata=newtraining)
##CM1<-confusionMatrix(predictions,newtraining$classe)
##CM1


##Calculate predictions at the validation set 
##after transforming the input to match the input for the models constructed

validation<-read.csv("pml-testing.csv")
usedfeatures3<-usedfeatures[1:53]
newvalidation<-validation[usedfeatures3]
newvalidation<-predict(preobj,newvalidation) 
predictionsval<-predict(modelFit,newdata=newvalidation)
predictions2val<-predict(modelFit2,newdata=newvalidation)

## Since random forest was the most accurate model at the testing set 
## it will be used to estimate the values for the validation set  
## The out of sample accuracy would be close to the one calculated
## of the testing set for the random forest model and the classification results
## as follows 

predictions2val
