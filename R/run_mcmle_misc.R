cat('\n\n');timestamp();cat('\n')
library(btergm)
library(parallel)
library(snow)

setwd('/home/sdowning/data')
load('netrisk_dynamic_firm_nets_1yr_v2_misc.RData')

ncores <- detectCores()
ncpus <- ifelse(ncores > 24, 24, ncores)
cat(sprintf('using %s cpus of %s detected cores\n', ncpus, ncores))

cl <- snow::makeCluster(ncpus)

#------------------------------------------------------------------------------
#------------- MISCELLANEOUS markets mtergm firm MODEL FIT LIST ---------------
#------------------------------------------------------------------------------

net_group <- 'misc'
nets.group <- firm.nets[[net_group]]
firms.todo <- names(firm.nets[[net_group]])
# firms.todo <- c("fitbit", "runtastic", "zipcar", "ridejoy", "visa", "mastercard", 
#                 "medallia", "clarabridge", "satmetrix")

#####
if ( !('l.fit.m' %in% ls()) ) l.fit.m <- list()
if ( !(net_group %in% names(l.fit.m)) ) l.fit.m[[net_group]] <- list()
nPeriods <- min(sapply(nets.group,function(net)length(net))) 
yrpd <- 1
if ('tmp.npds' %in% ls()) rm(tmp.npds)
for (i in 1:length(firms.todo)) {
  firm_i <- firms.todo[i]; cat(sprintf('---------%s---------\n',firm_i))
  nets <- nets.group[[firm_i]]
  nets.sub <- nets[ (length(nets)-nPeriods+1):length(nets) ]
  if ( !('tmp.npds' %in% ls()) )   tmp.npds <- length(nets.sub)
  mmc <- lapply(nets.sub, function(net) as.matrix(net %n% 'mmc'))
  sim <- lapply(nets.sub, function(net) as.matrix(net %n% 'similarity'))
  fit <- mtergm(nets.sub ~ edges + gwesp(0, fixed=T)  + 
                  nodefactor('state_code') + nodematch('state_code', diff=F) +
                  nodecov('age') +   edgecov(mmc)  +
                  nodematch('npm',diff=F) + 
                  edgecov(sim)  +
                  nodecov('net_risk') + 
                  nodematch('ipo_status', diff=TRUE)  +
                  nodecov('constraint') + absdiff('constraint') + 
                  cycle(3) + cycle(4) + cycle(5) + cycle(6)
                , parallel = "snow", ncpus = ncpus, cl=cl)
  l.fit.m[[net_group]][[firm_i]] <- fit
  #file.name <- sprintf('fit_list_btergm_%syr_%spd_%sR_%s-grp.RData', yrpd, tmp.npds, resamp,net_group)
  #save.image(file.name) # 
  save.image('run_mcmle_misc.RData')
}

#-------------------------------------------------------------------------------

snow::stopCluster(cl)
cat('completed successfully.\n\n')
