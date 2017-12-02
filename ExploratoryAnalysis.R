setwd("/Users/priyasridharan/Desktop/PRIYA/Grow Fin/")
perf_dat <- readRDS(file.choose())
perf_dat <- perf_dat %>% dplyr::filter(perf_dat$IndirectDirect == 'Indirect' & perf_dat$NewUsed == 'Used')
summary(perf_dat)
View(perf_dat)

library(xlsx) #load the package
write.xlsx(x = perf_dat, file = "Perf_03_11_2017.xlsx",
           sheetName = "NewData", row.names = TRUE)

n4 <- perf_dat[perf_dat$ScoreGroup == 'No Group',]

perf_chargedoff <- perf_dat[perf_dat$CHARGEOFFDATE != "1975-01-01",]
perf_chargedoff$ChMonths <-  (as.yearmon(perf_chargedoff$CHARGEOFFDATE, format="%y-%m-%d") - 
                                as.yearmon(perf_chargedoff$ORIGINALDATE, format="%y-%m-%d")) * 12
perf_chargedoff$ChMonths <- perf_chargedoff$ChMonths + 1
head(perf_chargedoff)

View(perf_chargedoff)

#ggplot(perf_chargedoff, aes(X=perf_chargedoff$ChMonths, Y=perf_chargedoff$CHARGEOFFAMOUNT))

      
      #Distr of CHarge Off Months
      d <- density(perf_chargedoff$ChMonths)
      plot(d, xlab = "Chargeoff amount in $", ylab = "Distribution of Chargeoff Amount", col = "blue" )
      summary(perf_chargedoff$ChMonths)
    
      hist(perf_chargedoff$ChMonths, breaks = 50, freq = F, main = "Charge Off Month Distribution" ,xlab = 'Chargeoff Months', ylim = c(0, 0.15), ylab = 'Distribution of Chargeoff Month')
      lines(density(perf_chargedoff$ChMonths, na.rm = T, from = min(perf_chargedoff$ChMonths), to = max(perf_chargedoff$ChMonths)), col="red")
      summary(perf_chargedoff$ChMonths)
      
      #Distr of CHarge off Amount
      
      d <- density(perf_chargedoff$ChargeOffAmount)
      plot(d, xlab = "Chargeoff amount in 1000 $", ylab = "Distribution of Chargeoff Amount", col = "blue" )
      summary(perf_chargedoff$ChargeOffAmount/1000)
      
      hist(perf_chargedoff$ChargeOffAmount/1000, breaks = 50, freq = F, main = "Charge Off Amount Distribution" ,xlab = 'Chargeoff Amount (in 1000 $)', ylim = c(0, 0.125), ylab = 'Distribution of Chargeoff Amount')
      lines(density(perf_chargedoff$ChargeOffAmount/1000, na.rm = T, from = min(perf_chargedoff$ChargeOffAmount/1000), to = max(perf_chargedoff$ChargeOffAmount/1000)), col="red")
      summary(perf_chargedoff$ChargeOffAmount/1000)
      
      #Score Group
      
      CB <- as.factor(perf_chargedoff$ScoreGroup)
      n <- perf_chargedoff[perf_chargedoff$ScoreGroup == 'No Group']
      n2 <- perf_chargedoff[perf_chargedoff$ScoreGroup != 'No Group']
      
      n <- perf_chargedoff %>% dplyr::filter(perf_chargedoff$ScoreGroup == 'No Group')
      
      
          plot(CB, col = "blue")
      summary(CB)
      hist(CB, breaks = 50, freq = F, main = "Score Group Distribution" ,xlab = 'Credit Score Group', ylim = c(0, 0.15), ylab = 'Distribution of Score Group')
      lines(density(perf_chargedoff$ChMonths, na.rm = T, from = min(perf_chargedoff$ChMonths), to = max(perf_chargedoff$ChMonths)), col="red")
      summary(perf_chargedoff$ChMonths)
      