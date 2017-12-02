library(ggplot2)
cars <- read.table(file.choose(), header = TRUE)
#Peek what you loaded
head(cars)

# 1.Create a frequency distribution table
table(cars$Transmission)

#Visualize the freq ditribution
ggplot(data = cars, aes(x = Transmission)) +
  geom_bar() +
  ggtitle("Frequent distibution of Transmission") +
  xlab("Transmission Type") +
  ylab("Frequency")

# 2. Quantitative stats for Numeric variable Fuel.Economy

#Minimum
min(cars$Fuel.Economy)

#Maximum
max(cars$Fuel.Economy)

#Mean
mean(cars$Fuel.Economy)

#Median
median(cars$Fuel.Economy)

#Get the quartiles
quantile(cars$Fuel.Economy)

#Standard Deviation = Spread
sd(cars$Fuel.Economy)

#Get the total
sum(cars$Fuel.Economy)

# Histogram with 10 equi distant bins
ggplot(data = cars, aes(x = cars$Fuel.Economy)) + 
      geom_histogram(bins = 10) + 
      ggtitle("Distribution of Fuel Economy") +
      xlab("Fuel Economy (mpg)") +
      ylab("Count of Cars")

# Create a density plot

ggplot(data = cars, aes(x = Fuel.Economy)) +
 # geom_histogram(bins = 10) +
  geom_density() +
  ggtitle("Density plot of Fuel Economy") +
  xlab("Fuel Economy in mpg") +
  ylab("Probability distribution of Fuel Economy")

# 3.Get the correlation of FUel Economoy and Cylinders -- Both are numeric so we can create a Corr Coefficient 
cor(x = cars$Cylinders, y = cars$Fuel.Economy)

## Strong negative correlation as value is close to -1

# Create a scater plot

ggplot(data = cars, 
       aes(x = Cylinders, y = Fuel.Economy)) +
       geom_point() +
       ggtitle("Correlation of cylinders and fuel economy") +
       xlab("No of cylinders") +
       ylab("Fuel Economy (mpg)") 
