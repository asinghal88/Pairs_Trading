
library(quantmod)
library(PerformanceAnalytics)
library(depmixS4)


simula <- function(trans,N) {
  transita <- function(char,trans) {
    sample(colnames(trans),1,prob=trans[char,])
  }
  
  sim <- character(N)
  sim[1] <- sample(colnames(trans),1)
  for (i in 2:N) {
    sim[i] <- transita(sim[i-1],trans)
  }
  
  sim
}

EstimateParameters <- function(price.pair, method = lm)
{
  x <- log(price.pair)
  
  reg <- method(x[, 2] ~ x[, 1])
  hedge.ratio <- as.numeric(reg$coef[2])
  premium     <- as.numeric(reg$coef[1])
  spread      <- x[, 2] - (hedge.ratio * x[, 1] + premium)
  list(spread = spread, hedge.ratio = hedge.ratio, premium = premium)
} 

IsStationary <- function(spread, threshold)
  {
    Is.passed.PP.test  <- PP.test(as.numeric(spread))$p.value <= threshold
    Is.passed.adf.test <- adf.test(as.numeric(diff(log(abs(spread)))))$p.value <= threshold
    c(PP.test = Is.passed.PP.test, adf.test = Is.passed.adf.test)
    print(adf.test(as.numeric(diff(log(abs(spread)))))$p.value)
  }
IsStationary(diff$diff,.01)
library("quantmod")
library("PerformanceAnalytics")
library("fUnitRoots")
library("tseries")

#Specify dates for downloading data, training models and running simulation
hedgeTrainingStartDate = as.Date("2009-01-01") #Start date for training the hedge ratio
hedgeTrainingEndDate = as.Date("2009-12-31") #End date for training the hedge ratio

symbolLst<-c('',"IFX")
title<-c("Adobe A vs B Cern")
symbolData <- new.env() #Make a new environment for quantmod to store data in
getSymbols(symbolLst, env = symbolData, src = "yahoo", from = hedgeTrainingStartDate, to=hedgeTrainingEndDate)

x1=symbolData$DTE[,4]
x2=symbolData$IFX[,4]
train=read.csv("dax.csv",header = TRUE)
str(train)
pair=train[,c('DTE','IFX')]


pair=as.data.frame(pair)
write.csv('pair2.csv',x = pair,row.names = T)

ret_stock1=Delt(train[,14])
ret_stock2=Delt(train[,20])
ret_stock1=na.omit(ret_stock1)
ret_stock2=na.omit(ret_stock2)
pair=pair[c(2:254),]
pair=na.omit(pair)
lis=EstimateParameters(pair)
diff=lis$spread
hedge_ratio=lis$hedge.ratio
#spread_return <- ret_stock1 - hedge_ratio*(ret_stock2)
diff=as.data.frame(diff)

hmm <- depmix(diff ~ 1, family = gaussian(), nstates = 3, data=diff)
hmmfit <- fit(hmm)
post_probs <- posterior(hmmfit)
summary(hmmfit)
head(post_probs)
# Plot the returns stream and the posterior
# probabilities of the separate regimes
time=c(1:254)
post_probs=cbind(index,post_probs)
layout(1:1)
plot(diff$diff, type='l', main='Regime Detection', xlab='', ylab='Spread')
matplot(post_probs[,-1], type='l', main='Regime Posterior Probabilities', ylab='Probability')
legend(x='bottomleft', c('Regime #1','Regime #2', 'Regime #3'), fill=1:3, bty='n')
a=ggplot(post_probs, aes(x=time,y=Probability,colour=Variable)) + 
  geom_line(aes(y = post_probs$S1, colour = "S1")) + 
  geom_line(aes(y = post_probs$S2, colour = "S2"))+
  geom_line(aes(y = post_probs$S3, colour = "S3"))
a+theme_bw()
post_probs$state=as.character(post_probs$state)
b=ggplot(post_probs, aes(x=time,y=state,colour=Variable,)) + 
  geom_line(aes(y = post_probs$state, colour = ""))
b+theme_bw()
y=getpars(hmmfit)
m1=y[13]
sd1=y[14]
m2=y[15]
sd2=y[16]
m3=y[17]
sd3=y[18]
s11=y[4]
s12=y[5]
s13=y[6]
s21=y[7]
s22=y[8]
s23=y[9]
s31=y[10]
s32=y[11]
s33=y[12]
s=matrix(c(s11,s12,s13,s21,s22,s23,s31,s32,s33),3)
colnames(s) <- c('1','2','3')
row.names(s) <- c('1','2','3')

sim_no=10
final=data.frame(matrix(nrow=sim_no,ncol=100))
sim <- data.frame(matrix(nrow = 254, ncol = sim_no))

for (j in 1:sim_no){
  x=simula(s,254)
  for( i in 1:length(x)){
    ifelse(x[i]<-1,sim[i,j]<-rnorm(1,m1,sd1),ifelse(x[i]<-2,sim[i,j]<-rnorm(1,m2,sd2),sim[i,j]<-rnorm(1,m3,sd3)))
  }
  threshold=seq(-2*StdDev(sim[,j]),2*StdDev(sim[,j]),0.005)
  for (i in 1:length(threshold)){
    signal <- ifelse(sim[,j] > threshold[i], -1, 
                     ifelse(sim[,j] < -threshold[i], 1, 0))
    trade_return <- return*lag(signal)
    final[j,i]=prod(1+trade_return) - 1
  }
}
return=sim[2:length(sim[,j]),j]-lag(sim[,j])





  
library(tseries)
cointegration<-function(x,y)
{
  vals<-data.frame(x,y)
  beta<-coef(lm(vals[,2]~vals[,1]+0,data=vals))[1]
  (adf.test(vals[,2]-beta*vals[,1], alternative="stationary", k=0))$p.value
}
  
cointegration(pair$MUV2,pair$MRK)
 pair=as.data.frame(pair)





