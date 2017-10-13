#---------------------------------------------------------------------
setwd("C:/Users/sdowning/Google Drive/PhD/Dissertation/competition networks/compnet")
# .libPaths('C:/Users/sdowning/Documents/R/win-library/3.2')
library(parallel)
library(btergm, quietly = T)
library(texreg, quietly = T)
library(igraph, quietly = T)
library(lattice, quietly = T); library(latticeExtra, quietly = T)
###
source(file.path(getwd(),'R','comp_net_functions.R'))
source(file.path(getwd(),'R','netrisk_functions.R'))
# source(file.path(getwd(),'R','cb_data_prep.R'))
par.default <- par()
lattice::trellis.par.set(strip.background=list(col="lightgrey"))

#---------------------------------------------------------------------

data.dir <- 'C:/Users/sdowning/Google Drive/PhD/Dissertation/competition networks/compnet'
data.file <- 'netrisk_dynamic_firm_nets_1yr_v3_misc.RData'
out.file <- 'netrisk_dynamic_firm_nets_1yr_v3_misc.RData'
out.txt <- 'run_pmle_hyp_OUT.txt'
load(sprintf('%s/%s',data.dir,data.file))

ncores <- detectCores()
ncpus <- ncores
cat(sprintf('using %s cpus of %s cores detected.\n', ncpus, ncores))

R <- 500

#-----------------------------------------------------------------------

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

#------------------------------------------------------------------------

l.hyp[[net_group]][[firm_i]]$f0 <- btergm(
  nets.sub ~ edges + gwesp(0, fixed=T) + 
    #nodefactor('state_code') + 
    nodematch('state_code', diff=F) +
    nodecov('age') +   edgecov(mmc)  + edgecov(ldv) +
    nodematch('npm',diff=F) + 
    edgecov(sim)  + 
    nodematch('ipo_status', diff=TRUE)
  , R=R, parallel = "multicore", ncpus = ncpus)
save.image(sprintf('%s/%s',data.dir,out.file)); gc()
cat('completed f0\n')

l.hyp[[net_group]][[firm_i]]$f1 <- btergm(
  nets.sub ~ edges + gwesp(0, fixed=T) + 
    #nodefactor('state_code') + 
    nodematch('state_code', diff=F) +
    nodecov('age') +   edgecov(mmc)  + edgecov(ldv) +
    nodematch('npm',diff=F) + 
    edgecov(sim)  +
    nodematch('ipo_status', diff=TRUE) +
    nodecov('net_risk') 
  , R=R, parallel = "multicore", ncpus = ncpus)
save.image(sprintf('%s/%s',data.dir,out.file)); gc()
cat('completed f1\n')

l.hyp[[net_group]][[firm_i]]$f2 <- btergm(
  nets.sub ~ edges + gwesp(0, fixed=T) + 
    #nodefactor('state_code') + 
    nodematch('state_code', diff=F) +
    nodecov('age') +   edgecov(mmc)  + edgecov(ldv) +
    nodematch('npm',diff=F) + 
    edgecov(sim)  +
    nodematch('ipo_status', diff=TRUE)  +
    nodecov('constraint') + absdiff('constraint') 
  , R=R, parallel = "multicore", ncpus = ncpus)
save.image(sprintf('%s/%s',data.dir,out.file)); gc()
cat('completed f2\n')

l.hyp[[net_group]][[firm_i]]$f3 <- btergm(
  nets.sub ~ edges + gwesp(0, fixed=T)   +
    #nodefactor('state_code') + 
    nodematch('state_code', diff=F) +
    nodecov('age') +   edgecov(mmc)  + edgecov(ldv) +
    nodematch('npm',diff=F) + 
    edgecov(sim)  +
    nodematch('ipo_status', diff=TRUE)  +
    cycle(3) + cycle(4) + cycle(5) + cycle(6)
  , R=R, parallel = "multicore", ncpus = ncpus)
save.image(sprintf('%s/%s',data.dir,out.file)); gc()
cat('completed f3\n')

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
save.image(sprintf('%s/%s',data.dir,out.file)); gc()
cat('completed f4\n')

cat('completed successfully.\n')



l.hyp[[net_group]][[firm_i]]$f3.5 <- btergm(
  nets.sub ~ edges + gwesp(0, fixed=T)   +
    #nodefactor('state_code') + 
    nodematch('state_code', diff=F) +
    nodecov('age') +   edgecov(mmc)  + edgecov(ldv) +
    nodematch('npm',diff=F) + 
    edgecov(sim)  +
    nodematch('ipo_status', diff=TRUE)  +
    cycle(3) + cycle(4) + cycle(5)
  , R=R, parallel = "multicore", ncpus = ncpus)
save.image(sprintf('%s/%s',data.dir,out.file)); gc()
cat('completed f3\n')

l.hyp[[net_group]][[firm_i]]$f4.5 <- btergm(
  nets.sub ~ edges + gwesp(0, fixed=T) + 
    #nodefactor('state_code') + 
    nodematch('state_code', diff=F) +
    nodecov('age') +   edgecov(mmc)  + edgecov(ldv) +
    nodematch('npm',diff=F) + 
    edgecov(sim)  +
    nodematch('ipo_status', diff=TRUE)  +
    nodecov('net_risk') +
    nodecov('constraint') + absdiff('constraint') + 
    cycle(3) + cycle(4) + cycle(5) 
  , R=R, parallel = "multicore", ncpus = ncpus)
save.image(sprintf('%s/%s',data.dir,out.file)); gc()
cat('completed f4\n')
