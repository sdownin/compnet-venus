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
smt <- lapply(nets.sub, function(net) as.matrix(net %n% 'similarity'))
ldv <- lapply(nets.sub, function(net) as.matrix(net %n% 'DV_lag'))

## Models
m0 <-   nets.sub ~ edges + gwesp(0, fixed=T) + 
  nodematch('ipo_status', diff=TRUE) +
  nodematch('state_code', diff=F) +
  nodematch('npm',diff=F) + 
  nodecov('age') +   
  edgecov(mmc)  + edgecov(ldv) +  edgecov(smt)

m1 <-   nets.sub ~ edges + gwesp(0, fixed=T) + 
  nodematch('ipo_status', diff=TRUE) +
  nodematch('state_code', diff=F) +
  nodematch('npm',diff=F) + 
  nodecov('age') +   
  edgecov(mmc)  + edgecov(ldv) +   edgecov(smt)  +
  nodecov('net_risk') 

m2 <-   nets.sub ~ edges + gwesp(0, fixed=T) + 
  nodematch('ipo_status', diff=TRUE)  +
  nodematch('state_code', diff=F) +
  nodematch('npm',diff=F) + 
  nodecov('age') +   
  edgecov(mmc)  + edgecov(ldv) +   edgecov(smt)  +
  nodecov('constraint') + absdiff('constraint') 

m3 <- nets.sub ~ edges + gwesp(0, fixed=T)   +
  nodematch('ipo_status', diff=TRUE)  +
  nodematch('state_code', diff=F) +
  nodematch('npm',diff=F) + 
  nodecov('age') +   
  edgecov(mmc)  + edgecov(ldv) +  edgecov(smt)  +
  cycle(3) + cycle(4) + cycle(5)

m4 <-  nets.sub ~ edges + gwesp(0, fixed=T) + 
  nodematch('ipo_status', diff=TRUE)  +
  nodematch('state_code', diff=F) +
  nodematch('npm',diff=F) + 
  nodecov('age') +   
  edgecov(mmc)  + edgecov(ldv) +  edgecov(smt)  +
  nodecov('net_risk') +
  nodecov('constraint') + absdiff('constraint') + 
  cycle(3)  + cycle(4) + cycle(5)

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
# save(fits, file='AOM_awareness_tergm_fits.RData')
saveRDS(fits, file='AOM_awareness_tergm_fits.rds')


## Goodness of Fit
options(error=function() dump.frames(to.file=TRUE))
f4.gof1 <- gof(f4,formula=m4,nsim=100,
               statistics = c(esp, dsp, geodesic,deg, triad.undirected, 
                              walktrap.modularity))
plot(f4.gof1)
saveRDS(f4.gof1, file="tergm_f4_gof1.rds")


## TERGM Micro-interpretation
## Predict Ties i--j (for i:=clarabridge and all j!=i) at period 6 (2016)
N <- nrow(f4@data$networks$`2017`[,])
Nn <- N*(N-1)
df4.pij <- data.frame(i=rep(NA,Nn),
                     j=rep(NA,Nn),
                     t1=rep(NA,Nn),
                     t2=rep(NA,Nn),
                     t3=rep(NA,Nn),
                     t4=rep(NA,Nn),
                     t5=rep(NA,Nn),
                     t6=rep(NA,Nn))
idx <- 1
ts <- c(6)
ego.j <- which(f4@data$networks$`2017` %v% 'vertex.names' == 'clarabridge')
for (j in ego.j) { ## ego firm column j
  for (i in 1:N) { ## alter firm row i
    if (i != j) {
      for (t in ts) { ## time periods
        df4.pij$i[idx] <- i
        df4.pij$j[idx] <- j
        col <- paste0("t",t)
        df4.pij[idx,col] <- btergm::interpret(f4,i=i,j=j,t=t)
        idx <- idx + 1
        if (idx %% 10 == 0)  cat(sprintf("finished i = %s --> j = %s",i,j))
      }
    }
  }
  cat(paste0("finished j =",j))
}; saveRDS(df4.pij, file="tergm_f4_t6_interpret_pij.rds")


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

