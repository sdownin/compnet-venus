library(relevent)
library(informR)
library(rem)

## RELEVENT --------------------------------------------
roweff<-rnorm(10) #Build rate matrix
roweff<-roweff-roweff[1] #Adjust for later convenience
coleff<-rnorm(10)
coleff<-coleff-coleff[1]
lambda<-exp(outer(roweff,coleff,"+"))
diag(lambda)<-0
ratesum<-sum(lambda)
esnd<-as.vector(row(lambda)) #List of senders/receivers
erec<-as.vector(col(lambda))
time<-0
edgelist<-vector()
while(time<15){ # Observe the system for 15 time units
  drawsr<-sample(1:100,1,prob=as.vector(lambda)) #Draw from model
  time<-time+rexp(1,ratesum)
  if(time<=15) #Censor at 15
    edgelist<-rbind(edgelist,c(time,esnd[drawsr],erec[drawsr]))
  else
    edgelist<-rbind(edgelist,c(15,NA,NA))
  }
#Fit the model, ordinal BPM
effects<-c("FESnd","FERec")
fit.ord<-rem.dyad(edgelist,10,effects=effects,hessian=TRUE)
summary(fit.ord)
par(mfrow=c(1,2)) #Check the coefficients
plot(roweff[-1],fit.ord$coef[1:9],asp=1)
abline(0,1)
plot(coleff[-1],fit.ord$coef[10:18],asp=1)
abline(0,1)

#Now, find the temporal BPM
fit.time<-rem.dyad(edgelist,10,effects=effects,ordinal=FALSE,hessian=TRUE)
summary(fit.time)
plot(fit.ord$coef,fit.time$coef,asp=1) #Similar results
abline(0,1)
#Finally, try the BSIR method (note: a much larger expansion factor
#is recommended in practice)
fit.bsir<-rem.dyad(edgelist,10,effects=effects,fit.method="BSIR",
                   sir.draws=100,sir.expand=5)
summary(fit.bsir)
par(mfrow=c(3,3)) #Examine the approximate posterior marginals
for(i in 1:9){
  hist(fit.bsir$post[,i],main=names(fit.bsir$coef)[i],prob=TRUE)
  abline(v=roweff[i+1],col=2,lwd=3)
}
for(i in 10:18){
  hist(fit.bsir$post[,i],main=names(fit.bsir$coef)[i],prob=TRUE)
  abline(v=coleff[i-8],col=2,lwd=3)
}









## REM package
# create some data with 'sender', 'target' and a 'time'-variable
# (Note: Data used here are random events from the Correlates of War Project)
sender <- c('TUN', 'NIR', 'NIR', 'TUR', 'TUR', 'USA', 'URU',
            'IRQ', 'MOR', 'BEL', 'EEC', 'USA', 'IRN', 'IRN',
            'USA', 'AFG', 'ETH', 'USA', 'SAU', 'IRN', 'IRN',
            'ROM', 'USA', 'USA', 'PAN', 'USA', 'USA', 'YEM',
            'SYR', 'AFG', 'NAT', 'NAT', 'USA')
target <- c('BNG', 'ZAM', 'JAM', 'SAU', 'MOM', 'CHN', 'IRQ',
            'AFG', 'AFG', 'EEC', 'BEL', 'ITA', 'RUS', 'UNK',
            'IRN', 'RUS', 'AFG', 'ISR', 'ARB', 'USA', 'USA',
            'USA', 'AFG', 'IRN', 'IRN', 'IRN', 'AFG', 'PAL',
            'ARB', 'USA', 'EEC', 'BEL', 'PAK')
time <- c('800107', '800107', '800107', '800109', '800109',
          '800109', '800111', '800111', '800111', '800113',
          '800113', '800113', '800114', '800114', '800114',
          '800116', '800116', '800116', '800119', '800119',
          '800119', '800122', '800122', '800122', '800124',
          '800125', '800125', '800127', '800127', '800127',
          '800204', '800204', '800204')
type <- sample(c('cooperation', 'conflict'), 33,
               replace = TRUE)
# combine them into a data.frame
dt <- data.frame(sender, target, time, type)
# create event sequence and order the data
dt <- eventSequence(datevar = dt$time, dateformat = '%y%m%d',
                    data = dt, type = 'continuous',
                    byTime = 'daily', returnData = TRUE,
                    sortData = TRUE)
# calculate sender-outdegree statistic
dt$sender.outdegree <- degreeStat(data = dt,
                                  time = dt$time,
                                  degreevar = dt$sender,
                                  halflife = 2,
                                  returnData = FALSE)
# plot sender-outdegree over time
library('ggplot2')
ggplot(dt, aes ( event.seq.cont, sender.outdegree) ) +
  geom_point()+ geom_smooth()

# calculate sender-indegree statistic
dt$sender.indegree <- degreeStat(data = dt,
                                 time = dt$time,
                                 degreevar = dt$sender,
                                 degree.on.other.var = dt$target,
                                 halflife = 2,
                                 returnData = FALSE)
# calculate target-indegree statistic
dt$target.indegree <- degreeStat(data = dt,
                                 time = dt$time,
                                 degreevar = dt$target,
                                 halflife = 2,
                                 returnData = FALSE)
# calculate target-outdegree statistic
dt$target.outdegree <- degreeStat(data = dt,
                                  time = dt$time,
                                  degreevar = dt$target,
                                  degree.on.other.var = dt$sender,
                                  halflife = 2,
                                  returnData = FALSE)
# calculate target-indegree with typematch
dt$target.indegree.tm <- degreeStat(data = dt,
                                    time = dt$time,
                                    degreevar = dt$target,
                                    halflife = 2,
                                    eventtypevar = dt$type,
                                    eventtypevalue = 'valuematch',
                                    returnData = FALSE)
