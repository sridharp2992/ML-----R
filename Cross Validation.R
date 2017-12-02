
## Cross Validation
## You will choose different 25% every run as the testing data - That way all of the data gets chance to be a part of the test set

ctrl <- trainControl(method="cv", number=10)
lmCVFit <- train(mpg~., data=mtcars, method="lm", trControl = ctrl, metric="Rsquared")
summary(lmCVFit)
