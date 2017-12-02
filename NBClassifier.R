library ("klaR") 
library ("caret") 
library ("e1071") 
set.seed(1234) 
OptDigits = read.csv(file.choose())
OptDigits$OptDigit <- as.factor(OptDigits$OptDigit) 
head(OptDigits)
#Clean out columns that are all "0"
ShuffledOptDigits <-OptDigits[sample(nrow(OptDigits)),] 
train <- ShuffledOptDigits[1:2810,] 
test <- ShuffledOptDigits[2811:5620,] head(train)
model <- NaiveBayes(OptDigit ~ ., data=train) # remove some columns…
#test the model 
predictions <- predict(model, test) 
warnings() 
confusionMatrix(test$OptDigit, predictions$class)