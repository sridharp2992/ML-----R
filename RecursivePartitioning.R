
OptDigits = read.csv(file.choose())
OptDigits$OptDigit <- as.factor(OptDigits$OptDigit) 
head(OptDigits)
ShuffledOptDigits <-OptDigits[sample(nrow(OptDigits)),] 
RPTrain<-ShuffledOptDigits[1:2810,] 
RPTest<-ShuffledOptDigits[2811:5620,] 
library(ggplot2) 
library(rpart) 
OptDigit.ct<-rpart(RPTrain$OptDigit ~ ., data=RPTrain[,1:64], cp=0) 
plot(OptDigit.ct) text(OptDigit.ct, use.n = T, digits = 3, cex = 0.6)
Prediction <- predict(OptDigit.ct, newdata=RPTest, type='class') 
head(Prediction)
table(Prediction, RPTest$OptDigit)