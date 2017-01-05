cat('\n\n');timestamp();cat('\n')
library(btergm)
library(parallel)
library(snow)
library(texreg)

data.dir <- '/home/sdowning/data'
data.file <- 'netrisk_dynamic_firm_nets_1yr_v2_misc.RData'
out.file <- 'run_mcmle_hyp_OUT.RData'
out.txt <- 'run_mcmle_hyp_OUT.txt'
load(sprintf('%s/%s',data.dir,data.file))

##ncpus <- 12
##(cl <- parallel::makeCluster(ncpus))
cores <- detectCores() 
ncpus <- ifelse(cores > 30, 30, cores)
cat(sprintf('using %s cpus of %s cores detected.\n', ncpus, cores))
parm <- "multicore" ## "snow"

ctrl <- control.ergm(MCMC.burnin=20000, MCMC.interval=2000, MCMC.samplesize=20000)

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


l.hyp[[net_group]][[firm_i]]$f2 <- mtergm(
  nets.sub ~ edges + gwesp(0, fixed=T) + 
    nodefactor('state_code') + nodematch('state_code', diff=F) +
    nodecov('age') +   edgecov(mmc)  +
    nodematch('npm',diff=F) + 
    edgecov(sim)  +
    nodematch('ipo_status', diff=TRUE) + 
    nodecov('constraint') + absdiff('constraint') 
  , parallel = parm, ncpus = ncpus,  control=ctrl)
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

