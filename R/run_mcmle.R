cat('\n\n');timestamp();cat('\n')
library(btergm)
library(parallel)
# library(snow)
library(texreg)

data.dir <- '/home/sdowning/data'
data.file <- 'netrisk_dynamic_firm_nets_1yr_v3_misc.RData'
out.file <- 'run_mcmle_hyp_OUT.RData'
out.txt <- 'run_mcmle_hyp_OUT.txt'
load(sprintf('%s/%s',data.dir,data.file))

##ncpus <- 12
##(cl <- parallel::makeCluster(ncpus))
cores <- detectCores() 
ncpus <- ifelse(cores > 30, 30, cores)
cat(sprintf('using %s cpus of %s cores detected.\n', ncpus, cores))
parm <- "multicore" ## "snow"

ctrl <- control.ergm(MCMC.burnin=30, MCMC.interval=3, MCMC.samplesize=30)

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
ldv <- lapply(nets.sub, function(net) as.matrix(net %n% 'DV_lag'))


l.hyp[[net_group]][[firm_i]]$f0 <- mtergm(
  nets.sub ~ edges + gwesp(0, fixed=T) + 
    #nodefactor('state_code') + 
    nodematch('state_code', diff=F) +
    nodecov('age') +   edgecov(mmc)  + edgecov(ldv) +
    nodematch('npm',diff=F) + 
    edgecov(sim)  + 
    nodematch('ipo_status', diff=TRUE)
  , parallel = parm, ncpus = ncpus, control=ctrl)
save.image(sprintf('%s/%s',data.dir,out.file))

l.hyp[[net_group]][[firm_i]]$f1 <- mtergm(
  nets.sub ~ edges + gwesp(0, fixed=T) + 
    #nodefactor('state_code') + 
    nodematch('state_code', diff=F) +
    nodecov('age') +   edgecov(mmc)  + edgecov(ldv) +
    nodematch('npm',diff=F) + 
    edgecov(sim)  +
    nodematch('ipo_status', diff=TRUE) +
    nodecov('net_risk') 
  ,parallel = parm, ncpus = ncpus, control=ctrl)
save.image(sprintf('%s/%s',data.dir,out.file))

l.hyp[[net_group]][[firm_i]]$f2 <- mtergm(
  nets.sub ~ edges + gwesp(0, fixed=T) + 
    #nodefactor('state_code') + 
    nodematch('state_code', diff=F) +
    nodecov('age') +   edgecov(mmc)  + edgecov(ldv) +
    nodematch('npm',diff=F) + 
    edgecov(sim)  +
    nodematch('ipo_status', diff=TRUE)  +
    nodecov('constraint') + absdiff('constraint') 
  , parallel = parm, ncpus = ncpus, control=ctrl)
save.image(sprintf('%s/%s',data.dir,out.file))


l.hyp[[net_group]][[firm_i]]$f3 <- mtergm(
  nets.sub ~ edges + gwesp(0, fixed=T)   +
    #nodefactor('state_code') + 
    nodematch('state_code', diff=F) +
    nodecov('age') +   edgecov(mmc)  + edgecov(ldv) +
    nodematch('npm',diff=F) + 
    edgecov(sim)  +
    nodematch('ipo_status', diff=TRUE)  +
    cycle(3) + cycle(4) + cycle(5) #  + cycle(6)
  , parallel = parm, ncpus = ncpus, control=ctrl)
save.image(sprintf('%s/%s',data.dir,out.file))


l.hyp[[net_group]][[firm_i]]$f4 <- mtergm(
  nets.sub ~ edges + gwesp(0, fixed=T) + 
    #nodefactor('state_code') + 
    nodematch('state_code', diff=F) +
    nodecov('age') +   edgecov(mmc)  + edgecov(ldv) +
    nodematch('npm',diff=F) + 
    edgecov(sim)  +
    nodematch('ipo_status', diff=TRUE)  +
    nodecov('net_risk') +
    nodecov('constraint') + absdiff('constraint') + 
    cycle(3) + cycle(4) + cycle(5) #  + cycle(6)
  , parallel = parm, ncpus = ncpus, control=ctrl)
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

parallel::stopCluster(cl)

cat('completed successfully.\n\n')

