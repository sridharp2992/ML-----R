loanDat <- read.csv(file.choose())
library(dplyr)

setwd("/Users/priyasridharan/Desktop/Priya/R Scripts/BinningModel")

loanDat <- dplyr::filter(.data = loanDat, loanDat$LoanType == 32)
loanDat$PTI <- as.numeric(loanDat$PTI)

loanDat$LTVBin <- ifelse((loanDat$LTV >= 0 & loanDat$LTV < 100), "0 to 100",
                         ifelse((loanDat$LTV >= 100 & loanDat$LTV < 200), "100 to 200",
                                ifelse((loanDat$LTV >= 200), "200 +",
                                       "Error")))



loanDat$PTIBin <- ifelse((loanDat$PTI >= 0 & loanDat$PTI < 10), "0 to 10",
                         ifelse((loanDat$PTI >= 10 & loanDat$PTI < 20), "10 to 20",
                                ifelse((loanDat$PTI >= 20), "20 +",
                         "Error")))
loanDat$TermBin <- ifelse((loanDat$Term >= 0 & loanDat$Term < 35), "0 to 36",
                          ifelse((loanDat$Term >= 35 & loanDat$Term < 47), "36 to 48",
                                 ifelse((loanDat$Term >= 47 & loanDat$Term < 72), "48 to 72",
                                        ifelse((loanDat$Term >= 72), "72 +",
                                               "Error"))))

binned_dat <- loanDat[, c("MonthEndDate","ORIGINALBALANCE","ChargeOffAmount","ScoreGroup","LTVBin","PTIBin","TermBin","TimePeriod","OrigDt_YM","IsChargedOff")]
#binned_dat <- loanDat[, c("MonthEndDate","ORIGINALBALANCE","ChargeOffAmount","ScoreGroup","TermBin","TimePeriod","OrigDt_YM","IsChargedOff")]

write.csv(loanDat, "BinnedData.csv")
binned_dat <- read.csv(file.choose())

 Score_loop <- (unique(binned_dat$ScoreGroup))
 LTV_loop <- (unique(binned_dat$LTVBin))
 PTI_loop <- (unique(binned_dat$PTIBin))
 Term_loop <- (unique(binned_dat$TermBin))

dat_new <- binned_dat %>% group_by(ScoreGroup, LTVBin, PTIBin, TermBin,IsChargedOff) %>% summarise(Count = n())
 dat_aggregated <- binned_dat %>% group_by(ScoreGroup, LTVBin, PTIBin, TermBin, TimePeriod, IsChargedOff) %>% summarise(Count = n()) %>% arrange(IsChargedOff, ScoreGroup, TimePeriod, LTVBin, PTIBin, TermBin)
 write.csv(dat_aagregated, "Sample.csv")
