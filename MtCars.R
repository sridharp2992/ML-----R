
class((mtcars$gear))

mtcars
mpg.lm = lm(mpg ~ hp, data = mtcars) 
summary(mpg.lm)
plot(mpg ~ hp, data = mtcars)

mpg2.lm = lm(mpg ~ hp + hp^2, data = mtcars) 
summary(mpg2.lm)
plot(mtcars$mpg ~ predict(mpg) , data = mtcars)
