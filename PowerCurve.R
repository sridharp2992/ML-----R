Demand = read.csv(file.choose())

Demand

Demand$LogY = log(Demand$Quantity)

Regr = lm(Demand$LogY ~ Demand$Price)

summary(Regr)

intercept = Regr$coefficients[1]

slope = Regr$coefficients[2]

alpha = exp(intercept)

beta = exp(slope)

Demand$Pred = alpha * beta ^ Demand$Price

plot(Demand$Price, Demand$Pred, col="blue")

points(Demand$Price, Demand$Quantity, col="orange")