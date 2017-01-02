cat('\n\n');timestamp();cat('\n')
library(btergm)
library(parallel)
library(snow)

setwd('/home/sdowning/data')
load('netrisk_dynamic_firm_nets_1yr_v2_misc.RData')

n <- length(firm.nets$misc$clarabridge)
nets.sub <- firm.nets$misc$clarabridge[(n-6+1):n]

mmc <- lapply(nets.sub, function(net) as.matrix(net %n% 'mmc'))
sim <- lapply(nets.sub, function(net) as.matrix(net %n% 'similarity'))
cat('summary(sim)\n')
print(summary(sim))

ncores <- detectCores()
ncpus <- ifelse(ncores > 24, 24, ncores)
cat(sprintf('using %s cpus of %s detected cores\n', ncpus, ncores))

cl <- snow::makeCluster(ncpus)

#---------------------------------------------------------
# --------- BTERGM HYPOTHESES MODEL  COMPARE -------------
#---------------------------------------------------------
nPeriods <- 6
net_group <- 'misc'
firm_i <- 'clarabridge'
if ( !('l.hyp' %in% ls()) ) l.hyp <- list()
if ( !(net_group %in% names(l.hyp)) ) l.hyp[[net_group]] <- list()
if ( !(firm_i %in% names(l.hyp[[net_group]])) ) l.hyp[[net_group]][[firm_i]] <- list()
nets.sub <- firm.nets[[net_group]][[firm_i]]
nets.sub <- nets.sub[(length(nets.sub)-nPeriods+1):(length(nets.sub))]
mmc <- lapply(nets.sub, function(net) as.matrix(net %n% 'mmc'))
sim <- lapply(nets.sub, function(net) as.matrix(net %n% 'similarity'))

l.hyp[[net_group]][[firm_i]]$fc <- mtergm(
  nets.sub ~ edges + gwesp(0, fixed=T) + 
    nodefactor('state_code') + nodematch('state_code', diff=F) +
    nodecov('age') +   edgecov(mmc)  +
    nodematch('npm',diff=F) + 
    edgecov(sim)  #+
  , parallel = "snow", ncpus = ncpus, cl=cl)

l.hyp[[net_group]][[firm_i]]$f0 <- mtergm(
  nets.sub ~ edges + gwesp(0, fixed=T) + 
    nodefactor('state_code') + nodematch('state_code', diff=F) +
    nodecov('age') +   edgecov(mmc)  +
    nodematch('npm',diff=F) + 
    edgecov(sim)  +
    nodecov('net_risk') 
  , parallel = "snow", ncpus = ncpus, cl=cl)

l.hyp[[net_group]][[firm_i]]$f1 <- mtergm(
  nets.sub ~ edges + gwesp(0, fixed=T) + 
    nodefactor('state_code') + nodematch('state_code', diff=F) +
    nodecov('age') +   edgecov(mmc)  +
    nodematch('npm',diff=F) + 
    edgecov(sim)  +
    nodematch('ipo_status', diff=TRUE)
  , parallel = "snow", ncpus = ncpus, cl=cl)

l.hyp[[net_group]][[firm_i]]$f2 <- mtergm(
  nets.sub ~ edges + gwesp(0, fixed=T) + 
    nodefactor('state_code') + nodematch('state_code', diff=F) +
    nodecov('age') +   edgecov(mmc)  +
    nodematch('npm',diff=F) + 
    edgecov(sim)  +
    nodecov('constraint') + absdiff('constraint') 
  , parallel = "snow", ncpus = ncpus, cl=cl)

l.hyp[[net_group]][[firm_i]]$f3 <- mtergm(
  nets.sub ~ edges + gwesp(0, fixed=T)   +
    nodefactor('state_code') + nodematch('state_code', diff=F) +
    nodecov('age') +   edgecov(mmc)  +
    nodematch('npm',diff=F) + 
    edgecov(sim)  +
    cycle(3) + cycle(4) + cycle(5) + cycle(6)
  , parallel = "snow", ncpus = ncpus, cl=cl)

l.hyp[[net_group]][[firm_i]]$f4 <- mtergm(
  nets.sub ~ edges + gwesp(0, fixed=T) + 
    nodefactor('state_code') + nodematch('state_code', diff=F) +
    nodecov('age') +   edgecov(mmc)  +
    nodematch('npm',diff=F) + 
    edgecov(sim)  +
    nodecov('net_risk') +
    nodematch('ipo_status', diff=TRUE)  +
    nodecov('constraint') + absdiff('constraint') + 
    cycle(3) + cycle(4) + cycle(5) + cycle(6)
  , parallel = "snow", ncpus = ncpus, cl=cl)

save.image(sprintf('mtergm_fit_HYP_%s_%s_.RData',net_group,firm_i))
#-----------------------------------------------------------------------------

print(btergm::btergm.se(fc, print=T))
print(btergm::btergm.se(f0, print=T))
print(btergm::btergm.se(f1, print=T))
print(btergm::btergm.se(f2, print=T))
print(btergm::btergm.se(f3, print=T))
print(btergm::btergm.se(f4, print=T))

cat('completed btergm fit. running diagnostics...\n')
gc <- tryCatch( btergm::gof(fc, nsim=30), error=function(e) e, finally=print('\n\npassing gof(fc)\n\n'))
g0 <- tryCatch( btergm::gof(f0, nsim=30), error=function(e) e, finally=print('\n\npassing gof(f0)\n\n'))
g1 <- tryCatch( btergm::gof(f1, nsim=30), error=function(e) e, finally=print('\n\npassing gof(f1)\n\n'))
g2 <- tryCatch( btergm::gof(f2, nsim=30), error=function(e) e, finally=print('\n\npassing gof(f2)\n\n'))
g3 <- tryCatch( btergm::gof(f3, nsim=30), error=function(e) e, finally=print('\n\npassing gof(f3)\n\n'))
g4 <- tryCatch( btergm::gof(f4, nsim=30), error=function(e) e, finally=print('\n\npassing gof(f4)\n\n'))


cat('saving image\n')
save.image('run_mcmle_hyp.RData')

snow::stopCluster(cl)

cat('completed successfully.\n\n')

