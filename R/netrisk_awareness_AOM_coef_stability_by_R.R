#######################################################################
##
## Competition Network Analysis
##
## for "Competitive Dynamics: Of Whom Should You Be Aware?"
## AOM 2017
##
#######################################################################

## set working directory
setwd("C:/Users/T430/Google Drive/PhD/Dissertation/competition networks/compnet2")

## load required libraries
## if not installed run:
## >  install.packages(c('pkg1','pkg2',...))
library(parallel)
library(btergm)
library(texreg)
library(plyr)

## set directory paths to save data file an dimages
data_dir <- "C:/Users/T430/Google Drive/PhD/Dissertation/crunchbase/"
img_dir  <- "C:/Users/T430/Google Drive/PhD/Dissertation/competition networks/envelopment/img"

## binary data file of competition network lists
#load('netrisk_dynamic_firm_nets_1yr_v3_misc.RData')
load('tergm_firm_nets_6pd_1yr.RData')

##==========================================================================
##        BTERGM:  TERGM via MPLE with Bootstrapped standard error
##--------------------------------------------------------------------------

nPeriods <- 6           ## 6 out of 7, skips first period for DV lag
net_group <- 'misc'     ## network group name
firm_i <- 'clarabridge' ## focal firm name

# subset of networks used to fit model
nets.sub <- firm.nets[[net_group]][[firm_i]]
nets.sub <- nets.sub[(length(nets.sub)-nPeriods+1):(length(nets.sub))]

## Edge covariates list of matrices [[NxN],[NxN],...]
mmc <- lapply(nets.sub, function(net) as.matrix(net %n% 'mmc'))
sim <- lapply(nets.sub, function(net) as.matrix(net %n% 'similarity'))
ldv <- lapply(nets.sub, function(net) as.matrix(net %n% 'DV_lag'))



m1 <-   nets.sub ~ edges + gwesp(0, fixed=T) + 
  nodematch('ipo_status', diff=TRUE) +
  nodematch('state_code', diff=F) +
  nodematch('npm',diff=F) + 
  nodecov('age') +   
  edgecov(mmc)  + edgecov(ldv) +   edgecov(sim)  +
  nodecov('net_risk') 

rf <- list()

for (R in 2^c(4:10)) {
  rf[[paste0('R',R)]] <- btergm(m1, R=R,parallel="multicore",ncpus=detectCores())
  summary(rf[[paste0('R',R)]])
}



##=================================================
## check PMLE estimates and stderrs consistency as
## number of resamples R increases
##-------------------------------------------------
par(mfrow=c(3,4))
for (i in 1:ncol(f0@boot$t)) {
  df <- ldply(list(f0,f0.40,f0.60,f0.80,f0.100), function(fit){
    x <- fit@boot$t[,i];
    out <- c(L99=unname(quantile(x,.005)),
             L95=unname(quantile(x,.025)),
             mu=mean(x,na.rm = T),
             U99=unname(quantile(x,.975)),
             U99=unname(quantile(x,.995)),
             R=length(x))
    return(out)
  })
  matplot(x=df[,6],y=df[,1:5],
          main=colnames(f0@boot$t)[i],
          ylab='Estimate', xlab=('Bootstrap Resamples'),
          log='x', pch=c(20,18,15,18,20),
          type = 'b', lty=c(3,2,1,2,3),lwd=c(.5,1,2,1,.5),
          col=c('gray','darkgray','red','darkgray','gray'))
}


## Save model output as binary data file
## load saved models by running:
## >  load('AOM_awareness_tergm_fits.RData')
saveRDS(list(rf=rf,df=df), file='netrisk_awareness_AOM_tergm_m1_coef_stability_by_R.rds')


