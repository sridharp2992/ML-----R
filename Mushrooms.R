install.packages('rattle')
install.packages('rpart.plot')
install.packages('RColorBrewer')
install.packages("ModelMetrics")
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(ggplot2)
library(rpart)
library(party)
library(ModelMetrics)
#Load the data
mush_data <- read.csv(file.choose())

#Lets get the distribution of the target variable
#Create a frequency table for the response variable
table(mush_data$PE)

#Check for null values
null_value <- mush_data[is.na(mush_data), ]
options("na.action")
# Split data as 70% Training and 30% Test
set.seed(22)
train_sample_size <- floor(0.70 * nrow(mush_data))
train_split <- sample(seq_len(nrow(mush_data)), size = train_sample_size)
train_data <- mush_data[train_split, ]
test_data <- mush_data[-train_split, ]

#Build a decision tree model

#model_tree <- rpart(train_data$PE ~ .,data = train_data, method = "class")
model_tree <- rpart(train_data$PE ~ .,data = train_data, method = "class", control = rpart.control(minsplit = 2, cp = 0.2))
summary(model_tree)

## Evaluate the mdoel built on test data
model_predict <- predict(model_tree, test_data, type = "class")
cf <- table(test_data$PE, model_predict)
correct_predictions <- (cf[1,1] + cf[2,2])
total_predicitons <- (cf[1,1] + cf[1,2] + cf[2,1] + cf[2,2])
overall_accuracy <- correct_predictions/ total_predicitons
actual_poisonous <- (cf[2,1] + cf[2,2])
recall <- actual_poisonous/ total_predicitons
predicted_poisonous <- (cf[1,2] + cf[2,2])
precision <- predicted_poisonous/ total_predicitons
print(model_tree)

#Plot the tree
## Come up with a better plotting system
fancyRpartPlot(model_tree)

