
Rahul Sonawala

The Sparks Foundation

Task 1: Prediction using Supervised ML

#importing the libraries

library(tidyr)
library(dplyr)
library(ggplot2)

#Reading the dataset
data=read.csv(file = "E:/Internship/Task 1/Book1.csv")

#Understanding the dataset
nrow(data)
ncol(data)
head(data,5)

#Describing the data

summary(data)


#Plotting the data to understand the relation betwwen the data


plotdata=ggplot(data = data)+geom_point(aes(x = Scores,y = Hours))+ggtitle("Scores Vs Hours")+xlab("Hours Studied")+ylab("Scores obtained")

plotdata

#Splitting data into test and train

s=sample(1:nrow(data),0.8*nrow(data))
data1=data[s,]
data12=data[-s,]

# applying linear model 
fit=lm(Scores~.,data=data1)

summary(fit)

fit=step(fit)

val.pred=predict(fit,newdata = data12)

val.pred


# Calculating prediction of our data

a=data.frame(Hours=9.25)


fit.final=lm(Scores~.,data=data)

summary(fit.final)

fit.final=step(fit.final)

val.pred=predict(fit.final,newdata = a)

val.pred

# calculating errors of prediction

errors=data12$Scores-val.pred
errors**2%>%mean()%>%sqrt()
