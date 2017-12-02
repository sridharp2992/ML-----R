## STatistical Model

data(iris)
head(iris)

library(ggplot2)

cor(x = iris$Petal.Length, y = iris$Petal.Width)
# Scatterplot of 2 variables
ggplot(data = iris, aes(x = Petal.Length, y = Petal.Width)) +
      geom_point() +
      ggtitle("Petal length vs Petal wwidth") +
      xlab("Petal Length (cm)") +
      ylab("Petal width (cm)")

# Draw the line of best fit using linear regression
# Create a linear regression model
model_lm <- lm(iris$Petal.Width ~ iris$Petal.Length, data = iris)


#Summarize the model
summary(model_lm)

#Draw reression line to plot
lines(x = iris$Petal.Length, 
      y = pred_model, 
      col = "red", 
      lwd = 3)

#Get correlation coefficients

cor(x = iris$Petal.Length, y = iris$Petal.Width)

## High corr => Petal Length can be used to identify Petal Width

predict(object = model_lm, newdata = data.frame(
        Petal.Length = c(2,5,7)))

