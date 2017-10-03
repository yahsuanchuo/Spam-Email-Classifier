#set.seed(999)
library(bitops)
library(datasets)
#import data
spam <- read.csv("C:/Users/Amy Chuo/Downloads/data.csv",header=FALSE,sep=";")
names <- read.csv("C:/Users/Amy Chuo/Downloads/names.csv",header=FALSE,sep=";")
names(spam) <- sapply((1:nrow(names)),function(i) toString(names[i,1]))
spam$y <- as.factor(spam$y)

indexes <- sample(1:nrow(spam), size=0.3*nrow(spam))

#Normalize data 
normalize <- function(x) {
  num <- x - min(x)
  denom <- max(x) - min(x)
  return (num/denom)
}

nspam <- as.data.frame(lapply(spam[,1:57], normalize))
nspam$y <- spam[,58]

### Split Data
#spam.test <- spam[indexes,]
#spam.train <- spam[-indexes,]
nspam.test <- nspam[indexes,]
nspam.train <- nspam[-indexes,]

install.packages('e1071')
library(e1071)
svm.model1 <- svm(y ~ . , kernel="linear", cost= 0.1, scale=FALSE, data=nspam.train)
svm.model2 <- svm(y ~ . , kernel="radial", cost= 10, scale=FALSE, data=nspam.train)
svm.model3 <- svm(y ~ . , kernel="polynomial", cost= 10, scale=FALSE, data=nspam.train)


#cross validation
#this is the best!
tuned <- tune(svm,y ~ . ,data=nspam.train, kernel="radial", ranges=list(cost=c(0.01,0.1,1,10))) 
summary(tuned)

bestmodel <- tuned$best.model
svm.prediction4 <- predict(bestmodel,newdata=nspam.test,type="class")
table(`Actual Class` = nspam.test$y, `Predicted Class` = svm.prediction4)
svm.error.rate4 <- sum(nspam.test$y != svm.prediction4)/nrow(nspam.test)
print(paste0("Accuary (Precision): ", 1 - svm.error.rate4))


x <- c("40:60","30:70","20:80")
accuracy1 <- as.numeric(c("0.929347","0.936956","0.916847","0.928804","0.925543"))
accuracy2 <- as.numeric(c("0.92826","0.93188","0.94347","0.93115","0.9326"))
accuracy3 <- as.numeric(c("0.93913","0.9163","0.9326","0.93043","0.92173"))
mean <- c(mean(accuracy1),mean(accuracy2),mean(accuracy3))

df <- data.frame(x,total)

plot(df$name,df$a)

install.packages('ggplot2')
library('ggplot2')
ggplot(df,aes(x,mean,group=1))+geom_line()+geom_point()+geom_text(label=mean)



tuned1 <- tune(svm,y ~ . ,data=nspam.train, kernel="linear", ranges=list(cost=c(0.01,0.1,1,10)))
summary(tuned1)
bestmodel1 <- tuned1$best.model

tuned2 <- tune(svm,y ~ . ,data=nspam.train, kernel="polynomial", ranges=list(cost=c(0.01,0.1,1,10)))
summary(tuned2)
bestmodel2 <- ntuned$best.model




svm.prediction1 <- predict(svm.model1, newdata = nspam.test,type="class")

svm.prediction2 <- predict(svm.model2 , newdata = nspam.test,type="class")

svm.prediction3<- predict(svm.model2 , newdata =nspam.test,type="class")

svm.prediction4 <- predict(bestmodel,newdata=nspam.test,type="class")

svm.prediction5 <- predict(bestmodel1,newdata =nspam.test,type="class")

svm.prediction6 <- predict(bestmodel2,newdata =nspam.test,type="class")


svm.predictionn <- predict(bestmodel1, newdata = nspam.train,type="class")



table(`Actual Class` = nspam.test$y, `Predicted Class` = svm.prediction1)
table(`Actual Class` = nspam.test$y, `Predicted Class` = svm.prediction2)
table(`Actual Class` = nspam.test$y, `Predicted Class` = svm.prediction3)
table(`Actual Class` = nspam.test$y, `Predicted Class` = svm.prediction4)
table(`Actual Class` = nspam.test$y, `Predicted Class` = svm.prediction5)
table(`Actual Class` = nspam.test$y, `Predicted Class` = svm.prediction6)

table(`Actual Class` = nspam.train$y, `Predicted Class` = svm.predictionn)

svm.error.rate1 <- sum(nspam.test$y != svm.prediction1)/nrow(nspam.test)
print(paste0("Accuary (Precision): ", 1 - svm.error.rate1))

svm.error.rate2 <- sum(nspam.test$y != svm.prediction2)/nrow(nspam.test)
print(paste0("Accuary (Precision): ", 1 - svm.error.rate2))

svm.error.rate3 <- sum(nspam.test$y != svm.prediction3)/nrow(nspam.test)
print(paste0("Accuary (Precision): ", 1 - svm.error.rate3))

svm.error.rate4 <- sum(nspam.test$y != svm.prediction4)/nrow(nspam.test)
print(paste0("Accuary (Precision): ", 1 - svm.error.rate4))

svm.error.rate5 <- sum(nspam.train$y != svm.prediction5)/nrow(nspam.test)
print(paste0("Accuary (Precision): ", 1 - svm.error.rate5))

svm.error.rate6 <- sum(nspam.train$y != svm.prediction6)/nrow(nspam.test)
print(paste0("Accuary (Precision): ", 1 - svm.error.rate6))

svm.error.raten <- sum(nspam.train$y != svm.predictionn)/nrow(nspam.train)
print(paste0("Accuary (Precision): ", 1 - svm.error.raten))



#reference: https://www.youtube.com/watch?v=ueKqDlMxueE
#https://www.youtube.com/watch?v=iu9rrCbSqgM


# summarize the fit
summary(fit)
# make predictions
predictions <- predict(fit, s_test)$class
fit <- lda(y~., data=s_train)
# summarize accuracy
table(predictions, s_test$y)


library(pROC)
ROC<-roc(as.numeric(svm.prediction4),as.numeric(nspam.test$y))
par("mar")
par(mar=c(1,1,1,1))
plot(ROC,col="blue",legacy.axes = TRUE)


ROC1<-roc(as.numeric(svm.predictionn),as.numeric(nspam.train$y))
par("mar")
par(mar=c(1,1,1,1))
{plot(ROC,col="blue",legacy.axes = TRUE)
lines(ROC1,col="red",legacy.axes = TRUE)}



install.packages('ggplot2')
library(ggplot2)


mean=colMeans(nspam[,1:57])
df <- as.data.frame(mean)
ggplot(df,aes(df$mean))+geom_histogram(aes(fill = ..count..))+labs(x="mean", y="Count") 

names(spam)[-58]

install.packages('dplyr')
library(dplyr)
arrange(dfff,mean)


#Standardize test & train data
train1<- model.matrix(y~.,spam.train)[,-1]
train <- scale(model.matrix(y~.,spam.train))
train <- train[,-1]
train <- as.data.frame(train)
train$y <- spam.train[,58]

install.packages("matrixStats")
library(matrixStats)
test <- model.matrix(y~.,spam.test)
test <- test[,-1]
test <- scale(test[,1:57], center = colMeans(train1[,1:57]), scale = colSds(train1[,1:57]))
test <- as.data.frame(test)
test$y <- spam.test[,58]
