#data importing and cleaning methods

f<-read.csv("student-mat.csv")
f$school=as.factor(f$school)
f$sex=as.factor(f$sex)
f$address=as.factor(f$address)
f$famsize=as.factor(f$famsize)
f$Pstatus=as.factor(f$Pstatus)
f$Mjob=as.factor(f$Mjob)
f$Fjob=as.factor(f$Fjob)
f$reason=as.factor(f$reason)
f$guardian=as.factor(f$guardian)
f$schoolsup=as.factor(f$schoolsup)
f$famsup=as.factor(f$famsup)
f$paid=as.factor(f$paid)
f$activities=as.factor(f$activities)
f$nursery=as.factor(f$nursery)
f$higher=as.factor(f$higher)
f$internet=as.factor(f$internet)
f$romantic=as.factor(f$romantic)
#print(f)

#plotting of some plots for comparison
#first is the bar plot

library(ggplot2)
a<-ggplot(f, aes(x =G3)) + geom_bar(fill="Blue") + labs(x="Final score G3",y="Quantity")
print(a)

#As there are some final score values with 0 score due to some reasons,
#to avoid decrease in accuracy we filtered them
#install.packages("dplyr")
library(dplyr)
f<-filter(f,G3!=0)
a<-ggplot(f, aes(x =G3)) + geom_bar(fill="Blue") + labs(x="Final score G3",y="Quantity")
print(a) #change in graph after filtering the values for 0 score
#print(f)



#now there are some graphs to compare important aspects for grade of person

#first is the support of school for their studies relation to their g3
d<-ggplot(f,aes(x=G3))+geom_density(aes(fill=schoolsup),alpha=.9)+  labs(y="Frequency",x="Final Score (G3)")
print(d)


#next is the support of family for their studies relation to their g3
e<-ggplot(f,aes(x=G3))+geom_density(aes(fill=famsup),alpha=.9)+labs(y="Frequency",x="Final Score (G3)")
print(e)


#splitting of data into training set and test set
#install.packages("caTools")
library(caTools)
set.seed(60)
split<-sample.split(f$G3,SplitRatio=0.8)
train_set<-subset(f,split ==TRUE)
test_set<-subset(f,split ==FALSE)

#First method-Multiple Linear Regression

#this is with all the variables 
l<-lm(G3~.,train_set)
s<-summary(l)
pred<-predict(l,test_set)
#print(pred)

#after the first regression variables with most significance are once again used in regression
l1<-lm(G3~studytime+failures+schoolsup+famsup+goout+absences,train_set)
s1<-summary(l1)
pred1<-predict(l1,test_set)
#print(pred1)



#install.packages("forecast")
library(forecast)

acc<-accuracy(pred1,test_set$G3)
#print(acc)



#second is the support vector regression
#install.packages("e1071")
library(e1071)
l2<-svm(G3~studytime+failures+schoolsup+famsup+health+absences+goout,train_set,type='nu-regression')
s2<-summary(l2)
pred2<-predict(l2,test_set)
print(pred2)


acc1<-accuracy(pred2,test_set$G3)
print(acc1)


#Third is the Decision Tree regression
#install.packages("rpart")
library(rpart)
l3<-rpart(G3~studytime+failures+schoolsup+famsup+health+absences+goout,train_set)
pred3<-predict(l3,test_set)
print(pred3)


acc2<-accuracy(pred3,test_set$G3)
print(acc2)


#Third is the Random Forest regression
#install.packages("randomForest")
library(randomForest)
l4<-randomForest(G3~studytime+failures+schoolsup+famsup+health+absences+goout,train_set)

pred4<-predict(l4,test_set)
print(pred4)

acc3<-accuracy(pred4,test_set$G3)
print(acc3)