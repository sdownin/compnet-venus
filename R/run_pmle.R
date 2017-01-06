cat('\n\n');timestamp();cat('\n')
library(btergm)
library(parallel)
library(snow)
library(texreg)

data.dir <- '/home/sdowning/data'
data.file <- 'netrisk_dynamic_firm_nets_1yr_v2_misc.RData'
out.file <- 'run_pmle_hyp_OUT.RData'
out.txt <- 'run_pmle_hyp_OUT.txt'
load(sprintf('%s/%s',data.dir,data.file))

ncpus <- 16
(cl <- snow::makeCluster(ncpus))

R <- 1000

#---------------------------------------------------------
# --------- BTERGM HYPOTHESES MODEL COMPARE MCMLE --------
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

l.hyp[[net_group]][[firm_i]]$fc <- btergm(
  nets.sub ~ edges + gwesp(0, fixed=T) + 
    nodefactor('state_code') + nodematch('state_code', diff=F) +
    nodecov('age') +   edgecov(mmc)  +
    nodematch('npm',diff=F) + 
    edgecov(sim)  #+
  , R=R, parallel = "snow", ncpus = ncpus, cl=cl)
save.image(sprintf('%s/%s',data.dir,out.file))

l.hyp[[net_group]][[firm_i]]$f0 <- btergm(
  nets.sub ~ edges + gwesp(0, fixed=T) + 
    nodefactor('state_code') + nodematch('state_code', diff=F) +
    nodecov('age') +   edgecov(mmc)  +
    nodematch('npm',diff=F) + 
    edgecov(sim)  +
    nodecov('net_risk') 
  , R=R, parallel = "snow", ncpus = ncpus, cl=cl)
save.image(sprintf('%s/%s',data.dir,out.file))

l.hyp[[net_group]][[firm_i]]$f1 <- btergm(
  nets.sub ~ edges + gwesp(0, fixed=T) + 
    nodefactor('state_code') + nodematch('state_code', diff=F) +
    nodecov('age') +   edgecov(mmc)  +
    nodematch('npm',diff=F) + 
    edgecov(sim)  +
    nodematch('ipo_status', diff=TRUE)
  , R=R, parallel = "snow", ncpus = ncpus, cl=cl)
save.image(sprintf('%s/%s',data.dir,out.file))

l.hyp[[net_group]][[firm_i]]$f2 <- btergm(
  nets.sub ~ edges + gwesp(0, fixed=T) + 
    nodefactor('state_code') + nodematch('state_code', diff=F) +
    nodecov('age') +   edgecov(mmc)  +
    nodematch('npm',diff=F) + 
    edgecov(sim)  +
    nodecov('constraint') + absdiff('constraint') 
  , R=R, parallel = "snow", ncpus = ncpus, cl=cl)
save.image(sprintf('%s/%s',data.dir,out.file))

l.hyp[[net_group]][[firm_i]]$f3 <- btergm(
  nets.sub ~ edges + gwesp(0, fixed=T)   +
    nodefactor('state_code') + nodematch('state_code', diff=F) +
    nodecov('age') +   edgecov(mmc)  +
    nodematch('npm',diff=F) + 
    edgecov(sim)  +
    cycle(3) + cycle(4) + cycle(5) + cycle(6)
  , R=R, parallel = "snow", ncpus = ncpus, cl=cl)
save.image(sprintf('%s/%s',data.dir,out.file))

l.hyp[[net_group]][[firm_i]]$f4 <- btergm(
  nets.sub ~ edges + gwesp(0, fixed=T) + 
    nodefactor('state_code') + nodematch('state_code', diff=F) +
    nodecov('age') +   edgecov(mmc)  +
    nodematch('npm',diff=F) + 
    edgecov(sim)  +
    nodecov('net_risk') +
    nodematch('ipo_status', diff=TRUE)  +
    nodecov('constraint') + absdiff('constraint') + 
    cycle(3) + cycle(4) + cycle(5) + cycle(6)
  , R=R, parallel = "snow", ncpus = ncpus, cl=cl)
save.image(sprintf('%s/%s',data.dir,out.file))
#-----------------------------------------------------------------------------

cat('finished model fits\n')

tryCatch( 
	screenreg( l.hyp[[net_group]][[firm_i]] )  , 
	error=function(e) e, 
	finally=print('finishing screenreg')
)
tryCatch( 
	screenreg( l.hyp[[net_group]][[firm_i]], file=sprintf('%s/%s',data.dir,out.txt)  )  , 
	error=function(e) e, 
	finally=print('finishing screenreg output')
)


#-----------------------------------------------------------------------------

cat('saving image\n')
save.image(sprintf('%s/%s',data.dir,out.file))

snow::stopCluster(cl)

cat('completed successfully.\n\n')

