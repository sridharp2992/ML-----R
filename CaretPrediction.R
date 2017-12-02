
mtcars
mpg.lm <- lm(mpg ~ hp, data = mtcars)
summary(mpg.lm)
plot(mpg ~ hp, data = mtcars)

mtcars
mpg2.lm <- lm(mpg ~ hp + hp^2, data = mtcars)
summary(mpg2.lm)
plot(mpg ~ predict(mpg2.lm), data = mtcars)


library(ggplot2)
library(lattice)
library(caret)

set.seed(10)
require(caret)

model.mtcars_lm <- train(mpg ~ wt, data=mtcars, method="lm")
coef.icept <- coef(model.mtcars_lm$finalModel)[1] ## Intercept
coef.slope <- coef(model.mtcars_lm$finalModel)[2] ## Slope

ggplot(data=mtcars, aes(x=wt, y=mpg)) + geom_point() + geom_abline(slope=coef.slope, intercept=coef.icept, color="red")

set.seed(10)
subset75 <- createDataPartition(y=mtcars$mpg, p=.75, list=FALSE)
subset75

training <- mtcars[subset75,]
training
testing <- mtcars[-subset75,]
testing


mtcarReg <- train(mpg~., data=training, method="lm")
summary(mtcarReg)

predict(mtcarReg, testing)
