library(btergm)
library(parallel)
library(snow)

setwd('/home/sdowning/data')
load('netrisk_dynamic_firm_nets_1yr_.RData')

nets.sub <- firm.nets$customergauge

mmc <- lapply(nets.sub, function(net) as.matrix(net %n% 'mmc'))
sim <- lapply(nets.sub, function(net) as.matrix(net %n% 'similarity'))

cat('beginning tergm computation')
ncores <- detectCores()
cat(sprintf('detected %s cores',ncores))
ncpus <- ifelse(ncores > 16, 16, ncores)
cat(sprintf('using %s cpus', ncpus))

fb1 <- btergm(nets.sub ~ edges + gwesp(0, fixed=T)  +
              nodefactor('state_code') + nodematch('state_code', diff=F) +
              nodecov('age') +   edgecov(mmc)  +
              nodecov('net_risk')  +
              nodematch('npm',diff=F) +
              nodematch('ipo_status', diff=T)  +
              nodecov('constraint') + absdiff('constraint') +
              edgecov(sim)  +
              cycle(3) + cycle(4) + cycle(5)
            , R=10 ,  parallel = "multicore", ncpus = ncpus)

btergm::btergm.se(fb1, print=T)

cat('completed btergm fit. running diagnostics...\n')
(g1 <- btegm::gof(fb1))

cat('saving image\n')
save.image('run_nrd_1y_1.RData')

cat('completed successfully.\n')

