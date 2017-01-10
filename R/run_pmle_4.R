cat('\n\n');timestamp();cat('\n')
library(btergm)
library(parallel)
library(snow)
library(texreg)

data.dir <- '/home/sdowning/data'
data.file <- 'netrisk_dynamic_firm_nets_1yr_v3_misc.RData'
out.file <- 'run_pmle_4_hyp_OUT.RData'
out.txt <- 'run_pmle_4_hyp_OUT.txt'
load(sprintf('%s/%s',data.dir,data.file))

ncores <- detectCores()
ncpus <- 4 # ncores
cat(sprintf('using %s cpus of %s cores detected.\n', ncpus, ncores))

R <- 100

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


l.hyp[[net_group]][[firm_i]]$f4 <- btergm(
  nets.sub ~ edges + gwesp(0, fixed=T) + 
    #nodefactor('state_code') + 
    nodematch('state_code', diff=F) +
    nodecov('age') +   edgecov(mmc)  + edgecov(ldv) +
    nodematch('npm',diff=F) + 
    edgecov(sim)  +
    nodematch('ipo_status', diff=TRUE)  +
    nodecov('net_risk') +
    nodecov('constraint') + absdiff('constraint') + 
    cycle(3) + cycle(4) + cycle(5) + cycle(6)
  , R=R, parallel = "multicore", ncpus = ncpus)
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

cat('completed successfully.\n\n')

