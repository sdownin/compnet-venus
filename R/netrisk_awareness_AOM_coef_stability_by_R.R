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

for (R in 2^c(4:12)) {
  rf[[paste0('R',R)]] <- btergm(m1, R=R,parallel="multicore",ncpus=detectCores())
  summary(rf[[paste0('R',R)]])
}


##=================================================
## check PMLE estimates and stderrs consistency as
## number of resamples R increases
##-------------------------------------------------
par(mfrow=c(3,4))
degpct <- c()
for (i in 1:ncol(rf[[1]]@boot$t)) {
  df <- ldply(rf, function(fit){
    x <- fit@boot$t[,i]
    ## filter degenerate model coefs
    # x <- x[x > quantile(x,.005) & x < quantile(x,.995)]
    iqr <- IQR(x)
    ol <- c(quantile(x,.25)-iqr, quantile(x,.75)+iqr)
    which.outliers <- which(x < ol[1] | x > ol[2])
    degpct <<- c(degpct, 100*length(which.outliers)/length(x)) ## break scope
    print(sprintf("%s degenerate: %.1f",'%',100*length(which.outliers)/length(x) ))
    out <- c(L99=unname(quantile(x,.005)),
             L95=unname(quantile(x,.025)),
             mu=unname(median(x,na.rm = T)),
             U95=unname(quantile(x,.975)),
             U99=unname(quantile(x,.995)),
             R=unname(length(x)))
    return(out)
  })
  matplot(x=df[7:length(df$R),"R"],y=df[7:nrow(df),c('L99','L95','mu','U95','U99')],
          main=colnames(rf[[1]]@boot$t)[i],
          ylab='Estimate', xlab=('Bootstrap Resamples'),
          log='x',
          pch=c(20,18,15,18,20),
          type = 'b', lty=c(3,2,1,2,3),lwd=c(.5,1,2,1,.5),
          col=c('steelblue','steelblue','red','steelblue','steelblue'))
  abline(h=0)
}; hist(degpct, main="% degenerate bootstrap coefficients")

## difference in coefficients
##-------------------------------------------------
par(mfrow=c(3,4))
for (i in 1:ncol(rf[[1]]@boot$t)) {
  df <- ldply(rf, function(fit){
    x <- fit@boot$t[,i]
    out <- c(L99=unname(quantile(x,.005)),
             L95=unname(quantile(x,.025)),
             mu=unname(median(x,na.rm = T)),
             U95=unname(quantile(x,.975)),
             U99=unname(quantile(x,.995)),
             R=unname(length(x)))
    return(out)
  })
  Rs <- df[,"R"][2:length(df[,"R"])]
  diff.prop <- diff(df$mu)/df$mu[2:length(df$mu)]
  plot(Rs[1:length(Rs)],diff.prop[1:length(diff.prop)], ylim=c(-.4,.4), log='x', type='b', 
       pch=c(15), col='darkred', 
       xlab=("Bootstrap Resamples"), ylab="% Diff in Estimated Mean",
       main=colnames(rf[[1]]@boot$t)[i])
  abline(h=0)
}


## Save model output as binary data file
## load saved models by running:
## >  load('AOM_awareness_tergm_fits.RData')
saveRDS(list(rf=rf,df=df), file='netrisk_awareness_AOM_tergm_m1_coef_stability_by_R.rds')


