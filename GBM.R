#Something is not quite right here… 
library(gbm) 
OptDigits = read.csv(file.choose())
OptDigits$OptDigit <- as.factor(OptDigits$OptDigit) 
head(OptDigits)
ShuffledOptDigits <-OptDigits[sample(nrow(OptDigits)),] 
RPTrain<-ShuffledOptDigits[1:2810,] 
RPTest<-ShuffledOptDigits[2811:5620,] 
OptDigit.gbm <- gbm(OptDigit ~ ., data=RPTrain, n.trees=200, interaction.depth=6, shrinkage=0.01) 
Prediction = predict(OptDigit.gbm, newdata=RPTest, n.trees=200, type="response") 
Prediction[1,,] 
PredictionClass = ifelse(Prediction[,1,]>.5,0,0) 
PredictionClass = ifelse(Prediction[,2,]>.5,1,PredictionClass) 
PredictionClass = ifelse(Prediction[,3,]>.5,2,PredictionClass) 
PredictionClass = ifelse(Prediction[,4,]>.5,3,PredictionClass) 
PredictionClass = ifelse(Prediction[,5,]>.5,4,PredictionClass) 
PredictionClass = ifelse(Prediction[,6,]>.5,5,PredictionClass) 
PredictionClass = ifelse(Prediction[,7,]>.5,6,PredictionClass) 
PredictionClass = ifelse(Prediction[,8,]>.5,7,PredictionClass) 
PredictionClass = ifelse(Prediction[,9,]>.5,8,PredictionClass) 
PredictionClass = ifelse(Prediction[,10,]>.5,9,PredictionClass) 
PredictionClass
table(PredictionClass, RPTest$OptDigit)