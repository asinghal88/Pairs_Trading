library(tseries)
library(quantmod)
library(xlsx)
library(timeSeries)

#-----------download stock info----------
start.date='2012-01-01'
end.date='2017-06-30'
in.sample.date='2015-12-31'
volume.limit<-15000000

#load the different listed companies from the excel i've provided
#work computer
emisoras<-read.xlsx2("Emisoras BMV.xlsx",header=T,sheetName="Sheet1")


getSymbols(as.character(emisoras$TICKER.BMV.GOOGLE),from=start.date,to=end.date)


#what are the different sectors?
sectors<-as.character(unique(emisoras$SECTOR))

table(emisoras$SECTOR)
#since energy and technology only have 1 value, we eliminate them from the list
sectors<-c("financials","industrials","consumer.goods","basic.materials","goods.services","telecom","health")

#----filter by volume and complete data-----
#calculate number of observations per stock
emisoras$sector.en=ifelse(emisoras$SECTOR=="SERVICIOS FINANCIEROS","financials",ifelse(emisoras$SECTOR=="INDUSTRIAL","industrials",ifelse(emisoras$SECTOR=="PRODUCTOS DE CONSUMO FRECUENTE","consumer.goods",ifelse(emisoras$SECTOR=="MATERIALES","basic.materials",ifelse(emisoras$SECTOR=="SERVICIOS Y BIENES DE CONSUMO NO BASICO","goods.services",ifelse(emisoras$SECTOR=="SERVICIOS DE TELECOMUNICACIONES","telecom",ifelse(emisoras$SECTOR=="SALUD","health","empty")))))))
for (i in 1:nrow(emisoras)){
  emisoras$days.data[i]=nrow(get(as.character(emisoras$TICKER.BMV.GOOGLE[i])))
}
total.days=max(emisoras$days.data)

#remove stocks with incomplete information:
emisoras=emisoras[emisoras$days.data==total.days,]

#calculate in sample days
insample.days=which(index(Cl(get(as.character(emisoras$TICKER.BMV.GOOGLE[1]))))==in.sample.date)

#divide the stocks into the different sectors
#calculate average daily trading volume & filter those that don't reach volume.limit

#financials
financials<-as.data.frame(emisoras$TICKER.BMV.GOOGLE[emisoras$SECTOR=="SERVICIOS FINANCIEROS"])
for (i in 1:nrow(financials)){
  financials$amt.volume[i]<-mean(Vo(get(as.character(financials[i,1])))[1:insample.days]*Cl(get(as.character(financials[i,1])))[1:insample.days])
}

financials<-financials[order(-financials$amt.volume),]
financials<-financials[financials[,2]>volume.limit,]

#industrials
industrials<-as.data.frame(emisoras$TICKER.BMV.GOOGLE[emisoras$SECTOR=="INDUSTRIAL"])

for (i in 1:nrow(industrials)){
  industrials$amt.volume[i]<-mean(Vo(get(as.character(industrials[i,1])))[1:insample.days]*Cl(get(as.character(industrials[i,1])))[1:insample.days])
}

industrials<-industrials[order(-industrials$amt.volume),]
industrials<-industrials[industrials[,2]>volume.limit,]

#consumer goods
consumer.goods<-as.data.frame(emisoras$TICKER.BMV.GOOGLE[emisoras$SECTOR=="PRODUCTOS DE CONSUMO FRECUENTE"])

for (i in 1:nrow(consumer.goods)){
  consumer.goods$amt.volume[i]<-mean(Vo(get(as.character(consumer.goods[i,1])))[1:insample.days]*Cl(get(as.character(consumer.goods[i,1])))[1:insample.days])
}

consumer.goods<-consumer.goods[order(-consumer.goods$amt.volume),]
consumer.goods<-consumer.goods[consumer.goods[,2]>volume.limit,]
consumer.goods<-consumer.goods[!is.na(consumer.goods[,2]),]

#basic materials
basic.materials=as.data.frame(emisoras$TICKER.BMV.GOOGLE[emisoras$SECTOR=="MATERIALES"])

for (i in 1:nrow(basic.materials)){
  basic.materials$amt.volume[i]<-mean(Vo(get(as.character(basic.materials[i,1])))[1:insample.days]*Cl(get(as.character(basic.materials[i,1])))[1:insample.days])
}

basic.materials<-basic.materials[order(-basic.materials$amt.volume),]
basic.materials<-basic.materials[basic.materials[,2]>volume.limit,]

#goods services
goods.services<-as.data.frame(emisoras$TICKER.BMV.GOOGLE[emisoras$SECTOR=="SERVICIOS Y BIENES DE CONSUMO NO BASICO"])

for (i in 1:nrow(goods.services)){
  goods.services$amt.volume[i]<-mean(Vo(get(as.character(goods.services[i,1])))[1:insample.days]*Cl(get(as.character(goods.services[i,1])))[1:insample.days])
}

goods.services<-goods.services[order(-goods.services$amt.volume),]
goods.services<-goods.services[goods.services[,2]>volume.limit,]

#telecom
telecom<-as.data.frame(emisoras$TICKER.BMV.GOOGLE[emisoras$SECTOR=="SERVICIOS DE TELECOMUNICACIONES"])

for (i in 1:nrow(telecom)){
  telecom$amt.volume[i]<-mean(Vo(get(as.character(telecom[i,1])))[1:insample.days]*Cl(get(as.character(telecom[i,1])))[1:insample.days])
}

telecom<-telecom[order(-telecom$amt.volume),]
telecom<-telecom[telecom[,2]>volume.limit,]

#health
health<-as.data.frame(emisoras$TICKER.BMV.GOOGLE[emisoras$SECTOR=="SALUD"])

for (i in 1:nrow(health)){
  health$amt.volume[i]<-mean(Vo(get(as.character(health[i,1])))[1:insample.days]*Cl(get(as.character(health[i,1])))[1:insample.days])
}

health<-health[order(-health$amt.volume),]
health<-health[health[,2]>volume.limit,]

#--------ADF tests using insample data----
min.pvalue=0.025
#create every unique pair for each sector

#financialpairs
#create every unique pair from financial sector
financialpairs=data.frame()
x=data.frame()
for (i in 1:nrow(financials)){
  financialpairs=rbind(financialpairs,x)
  x=data.frame()
  for(j in (i+1):nrow(financials)){
    x[j,1]=financials[i,1]
    x[j,2]=financials[j,1]
  }
}
financialpairs=na.omit(financialpairs)
#calculate betas
for(i in 1:nrow(financialpairs)){
  financialpairs$beta[i]=coef(lm(Cl(get(as.character(financialpairs$V1[i])))[1:insample.days]~Cl(get(as.character(financialpairs$V2[i])))[1:insample.days]+0))[1]
}

#test for cointegration using ADF
for(i in 1:nrow(financialpairs)){
    financialpairs$p.value[i]=adf.test(Cl(get(as.character(financialpairs$V1[i])))[1:insample.days]-financialpairs$beta[i]*Cl(get(as.character(financialpairs$V2[i])))[1:insample.days],alternative="stationary",k=0)[4]
}
financialpairs=financialpairs[financialpairs$p.value<min.pvalue,]
financialpairs$pairs=paste(financialpairs[,1],financialpairs[,2],sep="/")

#industrial pairs
industrialpairs=data.frame()
x=data.frame()

for (i in 1:nrow(industrials)){
  industrialpairs=rbind(industrialpairs,x)
  x=data.frame()
  for(j in (i+1):nrow(industrials)){
    x[j,1]=industrials[i,1]
    x[j,2]=industrials[j,1]
  }
}
industrialpairs=na.omit(industrialpairs)
#calculate betas
for(i in 1:nrow(industrialpairs)){
  industrialpairs$beta[i]=coef(lm(Cl(get(as.character(industrialpairs$V1[i])))[1:insample.days]~Cl(get(as.character(industrialpairs$V2[i])))[1:insample.days]+0))[1]
}

#test for cointegration using ADF
for(i in 1:nrow(industrialpairs)){
  industrialpairs$p.value[i]=adf.test(Cl(get(as.character(industrialpairs$V1[i])))[1:insample.days]-industrialpairs$beta[i]*Cl(get(as.character(industrialpairs$V2[i])))[1:insample.days],alternative="stationary",k=0)[4]
}
industrialpairs=industrialpairs[industrialpairs$p.value<min.pvalue,]
industrialpairs$pairs=paste(industrialpairs[,1],industrialpairs[,2],sep="/")

#consumer goods pairs
consumer.goodspairs=data.frame()
x=data.frame()

for (i in 1:nrow(consumer.goods)){
  consumer.goodspairs=rbind(consumer.goodspairs,x)
  x=data.frame()
  for(j in (i+1):nrow(consumer.goods)){
    x[j,1]=consumer.goods[i,1]
    x[j,2]=consumer.goods[j,1]
  }
}
consumer.goodspairs=na.omit(consumer.goodspairs)
#calculate betas
for(i in 1:nrow(consumer.goodspairs)){
  consumer.goodspairs$beta[i]=coef(lm(Cl(get(as.character(consumer.goodspairs$V1[i])))[1:insample.days]~Cl(get(as.character(consumer.goodspairs$V2[i])))[1:insample.days]+0))[1]
}

#test for cointegration using ADF
for(i in 1:nrow(consumer.goodspairs)){
  consumer.goodspairs$p.value[i]=adf.test(Cl(get(as.character(consumer.goodspairs$V1[i])))[1:insample.days]-consumer.goodspairs$beta[i]*Cl(get(as.character(consumer.goodspairs$V2[i])))[1:insample.days],alternative="stationary",k=0)[4]
}
consumer.goodspairs=consumer.goodspairs[consumer.goodspairs$p.value<min.pvalue,]
consumer.goodspairs$pairs=paste(consumer.goodspairs[,1],consumer.goodspairs[,2],sep = "/")

#basic materials pairs
basic.materials.pairs=data.frame()
x=data.frame()

for (i in 1:nrow(basic.materials)){
  basic.materials.pairs=rbind(basic.materials.pairs,x)
  x=data.frame()
  for(j in (i+1):nrow(basic.materials)){
    x[j,1]=basic.materials[i,1]
    x[j,2]=basic.materials[j,1]
  }
}
basic.materials.pairs=na.omit(basic.materials.pairs)
#calculate betas
for(i in 1:nrow(basic.materials.pairs)){
  basic.materials.pairs$beta[i]=coef(lm(Cl(get(as.character(basic.materials.pairs$V1[i])))[1:insample.days]~Cl(get(as.character(basic.materials.pairs$V2[i])))[1:insample.days]+0))[1]
}

#test for cointegration using ADF
for(i in 1:nrow(basic.materials.pairs)){
  basic.materials.pairs$p.value[i]=adf.test(Cl(get(as.character(basic.materials.pairs$V1[i])))[1:insample.days]-basic.materials.pairs$beta[i]*Cl(get(as.character(basic.materials.pairs$V2[i])))[1:insample.days],alternative="stationary",k=0)[4]
}
basic.materials.pairs=basic.materials.pairs[basic.materials.pairs$p.value<min.pvalue,]
basic.materials.pairs$pairs=paste(basic.materials.pairs[,1],basic.materials.pairs[,2],sep = "/")

#telecom pairs
telecompairs=data.frame()
x=data.frame()

for (i in 1:nrow(telecom)){
  telecompairs=rbind(telecompairs,x)
  x=data.frame()
  for(j in (i+1):nrow(telecom)){
    x[j,1]=telecom[i,1]
    x[j,2]=telecom[j,1]
  }
}
telecompairs=na.omit(telecompairs)
#calculate betas
for(i in 1:nrow(telecompairs)){
  telecompairs$beta[i]=coef(lm(Cl(get(as.character(telecompairs$V1[i])))[1:insample.days]~Cl(get(as.character(telecompairs$V2[i])))[1:insample.days]+0))[1]
}

#test for cointegration using ADF
for(i in 1:nrow(telecompairs)){
  telecompairs$p.value[i]=adf.test(Cl(get(as.character(telecompairs$V1[i])))[1:insample.days]-telecompairs$beta[i]*Cl(get(as.character(telecompairs$V2[i])))[1:insample.days],alternative="stationary",k=0)[4]
}
telecompairs=telecompairs[telecompairs$p.value<min.pvalue,]
telecompairs$pairs=paste(telecompairs[,1],telecompairs[,2],sep="/")

#healthpairs
healthpairs=data.frame()
x=data.frame()

for (i in 1:nrow(health)){
  healthpairs=rbind(healthpairs,x)
  x=data.frame()
  for(j in (i+1):nrow(health)){
    x[j,1]=health[i,1]
    x[j,2]=health[j,1]
  }
}
healthpairs=na.omit(healthpairs)
#calculate betas
for(i in 1:nrow(healthpairs)){
  healthpairs$beta[i]=coef(lm(Cl(get(as.character(healthpairs$V1[i])))[1:insample.days]~Cl(get(as.character(healthpairs$V2[i])))[1:insample.days]+0))[1]
}

#test for cointegration using ADF
for(i in 1:nrow(healthpairs)){
  healthpairs$p.value[i]=adf.test(Cl(get(as.character(healthpairs$V1[i])))[1:insample.days]-healthpairs$beta[i]*Cl(get(as.character(healthpairs$V2[i])))[1:insample.days],alternative="stationary",k=0)[4]
}
healthpairs=healthpairs[healthpairs$p.value<min.pvalue,]
healthpairs$pairs=paste(healthpairs[,1],healthpairs[,2],sep="/")

#calculate the number of remaining pairs by sector
remaining.sector.pairs=data.frame(c("financialpairs","industrialpairs","consumer.goodspairs","basic.materials.pairs","telecompairs","healthpairs"))
for (i in 1:nrow(remaining.sector.pairs)){
  remaining.sector.pairs$number.pairs[i]=nrow(get(as.character(remaining.sector.pairs[i,1])))
}

#group all the remaining pairs
remaining.pairs=data.frame()
for (i in 1:nrow(remaining.sector.pairs)){
  remaining.pairs=rbind(remaining.pairs,get(as.character(remaining.sector.pairs[i,1])))
}
remaining.pairs$sector=emisoras$sector.en[match(remaining.pairs$V1,emisoras$TICKER.BMV.GOOGLE)]

#----- eliminate pairs with correlation <0.60-----
for(i in 1:nrow(remaining.pairs)){
  remaining.pairs$correlation[i]=cor(Cl(get(as.character(remaining.pairs$V1[i])))[1:insample.days],Cl(get(as.character(remaining.pairs$V2[i])))[1:insample.days])
}
remaining.pairs=remaining.pairs[remaining.pairs$correlation>0.6,]
#scatter plot of the prices of each remaining pair
par(mfrow=c(3,2))
for(i in 1:nrow(remaining.pairs)){
  plot(as.numeric(Cl(get(as.character(remaining.pairs$V1[i])))[1:insample.days]),as.numeric(Cl(get(as.character(remaining.pairs$V2[i])))[1:insample.days]),main=remaining.pairs$pairs[i])
}
#---------ratios & zscores using full data---------
#create price ratios for each pair
ratios=data.frame(Cl(get(as.character(remaining.pairs$V1[1])))/Cl(get(as.character(remaining.pairs$V2[1]))))
for(i in 2:nrow(remaining.pairs)){
  ratios[,i]=Cl(get(as.character(remaining.pairs$V1[i])))/Cl(get(as.character(remaining.pairs$V2[i])))
}
colnames(ratios)=remaining.pairs$pairs

#create zscores
mavg.days=60
zscores=(ratios-rollapplyr(ratios,mavg.days,mean,fill=NA))/rollapplyr(ratios,mavg.days,sd,fill=NA)
colnames(zscores)=remaining.pairs$pairs

par(mfrow=c(3,2)) #so 8 plots are shown in the same window
#plot zscores with +/- 2 line
for(i in 1:nrow(remaining.pairs)){
  plot(zscores[,i],type="l",main=colnames(zscores)[i])
  abline(2,0,col="red") #horizontal line on 2
  abline(-2,0,col="red")
}
#---- trading strategy ----
#view the distribution of observations above/below 2 standard dev
par(mfrow=c(1,1))
hist(na.omit(unlist(zscores)[zscores>2]),breaks = 15)
hist(na.omit(unlist(zscores)[zscores>2]),breaks = 15,plot=F)
quantile(na.omit(unlist(zscores)[zscores>2]),c(.25,.5,.75))

#given the histogram, i will choose +/-2 as a primary entry limit
#i will add a secondary entry limit 
primary.entry=2.0
secondary.entry=2.25
#define the exit target
exit.target=0

#create a function that lags the data frame/matrix
lag.matrix=function(x){
  return(rbind(rep(NA,ncol(x)),x[1:(nrow(x)-1),]))
}

#create signals matrix
signals=matrix(NA,ncol=nrow(remaining.pairs),nrow=nrow(zscores))
signals=
  ifelse(secondary.entry>zscores & zscores>primary.entry,"primary sell",
  ifelse(secondary.entry<zscores,"secondary sell",
  ifelse(zscores<(-primary.entry)&zscores>(-secondary.entry),"primary buy",
  ifelse(zscores<(-secondary.entry),"secondary buy",
  ifelse(is.na(lag.matrix(zscores))|sign(zscores)==sign(lag.matrix(zscores)),"flat","exit")))))
  
#create primary positions matrix
primary.positions=matrix(NA,ncol=nrow(remaining.pairs),nrow=nrow(zscores))
colnames(primary.positions)=remaining.pairs$pairs
rownames(primary.positions)=rownames(zscores)
for( i in mavg.days:nrow(zscores)){
  primary.positions[i,]=
    ifelse(signals[i,]=="primary sell"|signals[i,]=="secondary sell","short",
    ifelse(signals[i,]=="primary buy"|signals[i,]=="secondary buy","long",
    ifelse(signals[i,]=="flat",ifelse(primary.positions[i-1,]=="exit"|is.na(primary.positions[i-1,]),"flat",primary.positions[i-1,]),"flat")))       
}

#create primary position prices
primary.prices=matrix(NA,ncol=nrow(remaining.pairs),nrow=nrow(zscores))
primary.prices[mavg.days,]=ifelse(primary.positions[mavg.days,]=="flat",0,as.numeric(ratios[mavg.days,]))
colnames(primary.prices)=remaining.pairs$pairs
rownames(primary.prices)=rownames(zscores)
for(i in (mavg.days+1):nrow(zscores)){
  primary.prices[i,]=
    ifelse(primary.positions[i,]=="flat",ifelse(primary.positions[i-1,]=="flat",0,as.numeric(ratios[i,])),
    ifelse(primary.positions[i,]==primary.positions[i-1,],primary.prices[i-1,],as.numeric(ratios[i,])))
}

#create primary p&l
primary.pl=matrix(NA,ncol=nrow(remaining.pairs),nrow=nrow(zscores))
colnames(primary.pl)=remaining.pairs$pairs
rownames(primary.pl)=rownames(zscores)
for(i in (mavg.days+1):nrow(zscores)){
  primary.pl[i,]=
    ifelse(primary.prices[i,]!=primary.prices[i-1,],
    ifelse(primary.prices[i-1,]==0,0,
    ifelse(primary.positions[i-1,]=="long",log(primary.prices[i,]/primary.prices[i-1,]),
    ifelse(primary.positions[i-1,]=="short",log(primary.prices[i-1,]/primary.prices[i,]),0))),0)
}

#individual equity curve:
par(mfrow=c(3,2)) 
for (i in 1:nrow(remaining.pairs)){
  plot(as.timeSeries(exp(cumsum(na.omit(primary.pl[,i])))),type="l",main=paste(colnames(zscores[i]),"Equity curve"))
}
#create secondary positions matrix
secondary.positions=matrix(NA,ncol=nrow(remaining.pairs),nrow=nrow(zscores))
colnames(secondary.positions)=remaining.pairs$pairs
rownames(secondary.positions)=rownames(zscores)
for( i in mavg.days:nrow(zscores)){
  secondary.positions[i,]=
    ifelse(signals[i,]=="secondary sell","short",
    ifelse(signals[i,]=="secondary buy","long",
    ifelse(signals[i,]=="primary buy",ifelse(secondary.positions[i-1,]=="short","flat",secondary.positions[i-1,]),
    ifelse(signals[i,]=="primary sell",ifelse(secondary.positions[i-1,]=="long","flat",secondary.positions[i-1,]),       
    ifelse(signals[i,]=="flat",ifelse(secondary.positions[i-1,]=="exit"|is.na(secondary.positions[i-1,]),"flat",secondary.positions[i-1,]),"flat")))))       
}

#create secondary price matrix
secondary.prices=matrix(NA,ncol=nrow(remaining.pairs),nrow=nrow(zscores))
secondary.prices[mavg.days,]=ifelse(secondary.positions[mavg.days,]=="flat",0,as.numeric(ratios[mavg.days,]))
colnames(secondary.prices)=remaining.pairs$pairs
rownames(secondary.prices)=rownames(zscores)
for(i in (mavg.days+1):nrow(zscores)){
  secondary.prices[i,]=
    ifelse(secondary.positions[i,]=="flat",ifelse(secondary.positions[i-1,]=="flat",0,as.numeric(ratios[i,])),
    ifelse(secondary.positions[i,]==secondary.positions[i-1,],secondary.prices[i-1,],as.numeric(ratios[i,])))
}

#create secondary p&l
secondary.pl=matrix(NA,ncol=nrow(remaining.pairs),nrow=nrow(zscores))
colnames(secondary.pl)=remaining.pairs$pairs
rownames(secondary.pl)=rownames(zscores)
for(i in (mavg.days+1):nrow(zscores)){
  secondary.pl[i,]=
    ifelse(secondary.prices[i,]!=secondary.prices[i-1,],
    ifelse(secondary.prices[i-1,]==0,0,
    ifelse(secondary.positions[i-1,]=="long",log(secondary.prices[i,]/secondary.prices[i-1,]),
    ifelse(secondary.positions[i-1,]=="short",log(secondary.prices[i-1,]/secondary.prices[i,]),0))),0)
}
par(mfrow=c(3,2)) 
for (i in 1:nrow(remaining.pairs)){
  plot(as.timeSeries(exp(cumsum(na.omit(secondary.pl[,i])))),type="l",main=paste(colnames(zscores[i])," Equity curve"))
}

#----analyze results----

riskfree.rate=0.04
#individual results for each pair:
#primary in-sample trades
individual.primary.equity=exp(colSums(primary.pl[1:insample.days,],na.rm = T))
individual.primary.CAGR=individual.primary.equity**(252/nrow(na.omit(primary.pl[1:insample.days,])))-1
individual.primary.sd=apply(na.omit(primary.pl[1:insample.days,]),2,function(x)sd(x))*sqrt(252)
#max drawdown:
individual.primary.maxdrawdown=""
for(i in 1:nrow(remaining.pairs)){
  individual.primary.maxdrawdown[i]=as.numeric(maxdrawdown(cumsum(na.omit(primary.pl[1:insample.days,i])))[1])
}
individual.primary.Sharpe=(as.numeric(individual.primary.CAGR)-riskfree.rate)/as.numeric(individual.primary.sd)
individual.primary.trades=apply(primary.pl[1:insample.days,],2,function(x)table(sign(x)))[1,]+apply(primary.pl[1:insample.days,],2,function(x)table(sign(x)))[3,]
individual.primary.hit.ratio=apply(primary.pl[1:insample.days,],2,function(x)table(sign(x)))[3,]/apply(primary.pl[1:insample.days,],2,function(x)table(sign(x)))[1,]
individual.primary.av.positive=apply(na.omit(primary.pl[1:insample.days,]),2,function(x)mean(x[x>0]))
individual.primary.av.negative=apply(na.omit(primary.pl[1:insample.days,]),2,function(x)mean(x[x<0]))

individual.primary.results=t(data.frame(individual.primary.equity,individual.primary.CAGR,individual.primary.sd,individual.primary.maxdrawdown,individual.primary.Sharpe,individual.primary.trades,individual.primary.hit.ratio,individual.primary.av.positive,individual.primary.av.negative))
#calculate correlation matrix for pair returns
primary.correlation.matrix=as.matrix(cor(na.omit(primary.pl)))

#secondary in-sample trades
individual.secondary.equity=exp(colSums(secondary.pl[1:insample.days,],na.rm = T))
individual.secondary.CAGR=individual.secondary.equity**(252/nrow(na.omit(secondary.pl[1:insample.days,])))-1
individual.secondary.sd=apply(na.omit(secondary.pl[1:insample.days,]),2,function(x)sd(x))*sqrt(252)
#max drawdown:
individual.secondary.maxdrawdown=""
for(i in 1:nrow(remaining.pairs)){
  individual.secondary.maxdrawdown[i]=as.numeric(maxdrawdown(cumsum(na.omit(secondary.pl[1:insample.days,i])))[1])
}
individual.secondary.Sharpe=(as.numeric(individual.secondary.CAGR)-riskfree.rate)/as.numeric(individual.secondary.sd)
individual.secondary.trades=apply(secondary.pl[1:insample.days,],2,function(x)table(sign(x)))[1,]+apply(secondary.pl[1:insample.days,],2,function(x)table(sign(x)))[3,]
individual.secondary.hit.ratio=apply(secondary.pl[1:insample.days,],2,function(x)table(sign(x)))[3,]/apply(secondary.pl[1:insample.days,],2,function(x)table(sign(x)))[1,]
individual.secondary.av.positive=apply(na.omit(secondary.pl[1:insample.days,]),2,function(x)mean(x[x>0]))
individual.secondary.av.negative=apply(na.omit(secondary.pl[1:insample.days,]),2,function(x)mean(x[x<0]))

individual.secondary.results=t(data.frame(individual.secondary.equity,individual.secondary.CAGR,individual.secondary.sd,individual.secondary.maxdrawdown,individual.secondary.Sharpe,individual.secondary.trades,individual.secondary.hit.ratio,individual.secondary.av.positive,individual.secondary.av.negative))

#is using secondary trades helpful?
as.numeric(individual.primary.results[2,])>as.numeric(individual.secondary.results[2,])

#aggregate in-sample results (equal initial equity for each pair)
aggregate.primary.pl=rowSums(primary.pl/nrow(remaining.pairs),na.rm = T)
aggregate.primary.CAGR=exp(sum(aggregate.primary.pl[1:insample.days]))**(252/length(na.omit(aggregate.primary.pl[1:insample.days])))-1
aggregate.primary.sd=sd(na.omit(aggregate.primary.pl[1:insample.days]))*sqrt(252)
aggregate.primary.Sharpe=(as.numeric(aggregate.primary.CAGR)-riskfree.rate)/as.numeric(aggregate.primary.sd)
aggregate.secondary.pl=rowSums(secondary.pl/nrow(remaining.pairs),na.rm = T)
aggregate.secondary.CAGR=exp(sum(aggregate.secondary.pl[1:insample.days]))**(252/length(na.omit(aggregate.secondary.pl[1:insample.days])))-1
aggregate.secondary.sd=sd(na.omit(aggregate.secondary.pl[1:insample.days]))*sqrt(252)
aggregate.secondary.Sharpe=(as.numeric(aggregate.secondary.CAGR)-riskfree.rate)/as.numeric(aggregate.secondary.sd)

#if Sharpe(b)>cor(a,b)*Sharpe(a) then adding b will increase the portfolio Sharpe
correlation.primsec=cor(na.omit(aggregate.primary.pl[1:insample.days]),na.omit(aggregate.secondary.pl[1:insample.days]))
ifelse(aggregate.secondary.Sharpe>(aggregate.primary.Sharpe*correlation.primsec),"use combined primary & secondary trades","only use primary trades")

#use a combined .75primary .25secondary positions with equal equity for every pair
combined.pl=.75*aggregate.primary.pl+.25*aggregate.secondary.pl
par(mfrow=c(1,1))
plot(as.timeSeries(exp(cumsum(na.omit(combined.pl)))),type="l",main=paste("Equity curve",start.date,"to",end.date))
abline(v=as.POSIXct(in.sample.date),col="red")
text(x=as.POSIXct(in.sample.date),y=1,labels = paste("in sample limit",in.sample.date))

#in-sample results
insample.equity=exp(sum(na.omit(combined.pl[1:insample.days])))
insample.CAGR=insample.equity**(252/length(na.omit(combined.pl[1:insample.days])))-1
insample.sd=sd(na.omit(combined.pl[1:insample.days]))*sqrt(252)
insample.maxdrawdown=maxdrawdown(cumsum(na.omit(combined.pl[1:insample.days])))[1]
insample.Sharpe=(as.numeric(insample.CAGR)-riskfree.rate)/as.numeric(insample.sd)

#out-of-sample results
outsample.equity=exp(sum(na.omit(combined.pl[(insample.days+1):length(combined.pl)])))
outsample.CAGR=outsample.equity**(252/length(combined.pl[(insample.days+1):length(combined.pl)]))-1
outsample.sd=sd(combined.pl[(insample.days+1):length(combined.pl)])*sqrt(252)
outsample.maxdrawdown=maxdrawdown(cumsum(combined.pl[(insample.days+1):length(combined.pl)]))[1]
outsample.Sharpe=(as.numeric(outsample.CAGR)-riskfree.rate)/as.numeric(outsample.sd)

#complete results
complete.equity=exp(sum(na.omit(combined.pl)))
complete.CAGR=complete.equity**(252/length(na.omit(combined.pl)))-1
complete.sd=sd(na.omit(combined.pl))*sqrt(252)
complete.maxdrawdown=as.numeric(maxdrawdown(cumsum(na.omit(combined.pl)))[1])
complete.Sharpe=(as.numeric(complete.CAGR)-riskfree.rate)/as.numeric(complete.sd)

#final results
final.results=t(data.frame(insample.equity,insample.CAGR,insample.sd,insample.maxdrawdown,insample.Sharpe))
final.results=cbind(final.results,t(data.frame(outsample.equity,outsample.CAGR,outsample.sd,outsample.maxdrawdown,outsample.Sharpe)))
final.results=cbind(final.results,t(data.frame(complete.equity,complete.CAGR,complete.sd,complete.maxdrawdown,complete.Sharpe)))
rownames(final.results)=c("Final equity","CAGR","standard deviation","max drawdown","Sharpe")
colnames(final.results)=c("in-sample results","out-of-sample results","complete results")
