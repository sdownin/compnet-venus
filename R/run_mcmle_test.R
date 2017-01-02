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

fm1 <- mtergm(nets.sub ~ edges + gwesp(0, fixed=T)  +
                nodefactor('state_code') + nodematch('state_code', diff=F) +
                nodecov('age') +   edgecov(mmc)  +
                nodematch('npm',diff=F) +
                edgecov(sim)  +
                nodecov('net_risk')  +
                nodematch('ipo_status', diff=T)  +
                nodecov('constraint') + absdiff('constraint') +
                cycle(3) + cycle(4) + cycle(5)
              , parallel = "snow", ncpus = ncpus, cl=cl)

btergm::btergm.se(fm1, print=T)

cat('completed btergm fit. running diagnostics...\n')
g1 <- tryCatch( btergm::gof(fm1, nsim=30), error=function(e) e, finally=print('\n\npassing gof()\n\n'))

cat('saving image\n')
save.image('run_mcmle.RData')

snow::stopCluster(cl)

cat('completed successfully.\n\n')

