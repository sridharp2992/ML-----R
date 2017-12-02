Advertising = read.csv(file.choose())
head(Advertising)
Adv.lm = lm(Sales~TV, data=Advertising)
summary(Adv.lm)
plot(Advertising$Sales~Advertising$TV, col="blue")
points(Advertising$TV,predict(Adv.lm), col="orange")

resid(TV.lm)
plot(density(resid(TV.lm)))
qqnorm(resid(TV.lm))
qqline(resid(TV.lm))

TV.lm = lm(Sales~TV, data=Advertising)
summary(TV.lm)
plot(Sales~TV, data = Advertising,col="blue")
points(Advertising$TV,predict(TV.lm), col="orange")

Radio.lm = lm(Sales~Radio, data=Advertising)
summary(Radio.lm)
plot(Sales~Radio,  data = Advertising, col="blue")
points(Advertising$Radio,predict(Radio.lm), col="yellow")


TVRadio.lm = lm(Sales ~ TV + Radio, data = Advertising)
summary(TVRadio.lm)
plot(Advertising$Sales ~ predict(TVRadio.lm), col="green")


TVRadioInt.lm = lm(Sales ~ TV + Radio + TV * Radio, data = Advertising)
summary(TVRadioInt.lm)
plot(Advertising$Sales ~ predict(TVRadioInt.lm), col="orange")
## Better graph 45 deg line