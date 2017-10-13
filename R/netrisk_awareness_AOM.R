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

R <- 1000                 ## number of bootstrap resamples
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

## Models
m0 <-   nets.sub ~ edges + gwesp(0, fixed=T) + 
  nodematch('ipo_status', diff=TRUE) +
  nodematch('state_code', diff=F) +
  nodematch('npm',diff=F) + 
  nodecov('age') +   
  edgecov(mmc)  + edgecov(ldv) +  edgecov(sim)

m1 <-   nets.sub ~ edges + gwesp(0, fixed=T) + 
  nodematch('ipo_status', diff=TRUE) +
  nodematch('state_code', diff=F) +
  nodematch('npm',diff=F) + 
  nodecov('age') +   
  edgecov(mmc)  + edgecov(ldv) +   edgecov(sim)  +
  nodecov('net_risk') 

m2 <-   nets.sub ~ edges + gwesp(0, fixed=T) + 
  nodematch('ipo_status', diff=TRUE)  +
  nodematch('state_code', diff=F) +
  nodematch('npm',diff=F) + 
  nodecov('age') +   
  edgecov(mmc)  + edgecov(ldv) +   edgecov(sim)  +
  nodecov('constraint') + absdiff('constraint') 

m3 <- nets.sub ~ edges + gwesp(0, fixed=T)   +
  nodematch('ipo_status', diff=TRUE)  +
  nodematch('state_code', diff=F) +
  nodematch('npm',diff=F) + 
  nodecov('age') +   
  edgecov(mmc)  + edgecov(ldv) +  edgecov(sim)  +
  cycle(3:5)

m4 <-  nets.sub ~ edges + gwesp(0, fixed=T) + 
  nodematch('ipo_status', diff=TRUE)  +
  nodematch('state_code', diff=F) +
  nodematch('npm',diff=F) + 
  nodecov('age') +   
  edgecov(mmc)  + edgecov(ldv) +  edgecov(sim)  +
  nodecov('net_risk') +
  nodecov('constraint') + absdiff('constraint') + 
  cycle(3:5) 

## RUN Bootstrap MPLE
f0 <- btergm(m0, R=R, parallel = "multicore", ncpus = detectCores()); summary(f0)
f1 <- btergm(m1, R=R, parallel = "multicore", ncpus = detectCores()); summary(f1)
f2 <- btergm(m2, R=R, parallel = "multicore", ncpus = detectCores()); summary(f2)
f3 <- btergm(m3, R=R, parallel = "multicore", ncpus = detectCores()); summary(f3)
f4 <- btergm(m4, R=R, parallel = "multicore", ncpus = detectCores()); summary(f4)

## save as a list of model fits
fits <- list(f0=f0,f1=f1,f2=f2,f3=f3,f4=f4)

## show models table comparison on screen 
screenreg(fits, digits = 3, single.row = F, 
          ci.force = T, ci.force.level = .99, ci.test = 0)

## save models table to .html file (view formatted results in web browser like chrome) 
htmlreg(fits, file = 'AOM_awareness_tergm_fits.html', 
        digits = 3, single.row = F, 
        ci.force = T, ci.force.level = .99, ci.test = 0)

## Save model output as binary data file
## load saved models by running:
## >  load('AOM_awareness_tergm_fits.RData')
save(fits, file='AOM_awareness_tergm_fits.RData')
saveRDS(fits, file='AOM_awareness_tergm_fits.rds')



# ##=================================================
# ## check PMLE estimates and stderrs consistency as 
# ## number of resamples R increases
# ##-------------------------------------------------
# par(mfrow=c(3,3))
# for (i in 1:ncol(f0@boot$t)) {
#   df <- ldply(list(f0,f0.40,f0.60,f0.80,f0.100), function(fit){
#     x <- fit@boot$t[,i];
#     out <- c(L99=unname(quantile(x,.005)),
#              L95=unname(quantile(x,.025)),
#              mu=mean(x,na.rm = T),
#              U99=unname(quantile(x,.975)),
#              U99=unname(quantile(x,.995)),
#              R=length(x))
#     return(out)
#   })
#   matplot(x=df[,6],y=df[,1:5],
#           main=colnames(f0@boot$t)[i],
#           ylab='Estimate', xlab=('Bootstrap Resamples'),
#           log='x', pch=c(20,18,15,18,20),
#           type = 'b', lty=c(3,2,1,2,3),lwd=c(.5,1,2,1,.5),
#           col=c('gray','darkgray','red','darkgray','gray'))
# }

