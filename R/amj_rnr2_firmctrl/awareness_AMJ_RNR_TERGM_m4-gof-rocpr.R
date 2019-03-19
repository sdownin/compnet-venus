cat('\n\n');timestamp();cat('\n')
library(btergm)
library(parallel)
library(texreg)

data_dir <- '/home/sdowning/data/firm_nets_rnr2/firmctrl'
results_dir <- '/home/sdowning/compnet/results/amj_rnr2_firmctrl'

## SETTINGS
firm_i <- 'qualtrics'
d <- 3
R <- 2000
m_x <- 'm4'
nPeriod <- 11
nsim <- 200
ncpus <- 4
parallel <- "multicore"

## NETWORKS LIST
data_file <- file.path(data_dir,sprintf('%s_d%s.rds',firm_i,d))
nets <- readRDS(data_file)

## make MMC nets list
mmc <- lapply(nets, function(net) as.matrix(net %n% 'mmc'))
cpc <- lapply(nets, function(net) as.matrix(net %n% 'coop'))
cpp <- lapply(nets, function(net) as.matrix(net %n% 'coop_past'))
cpa <- lapply(nets, function(net) as.matrix(net %n% 'coop') + as.matrix(net %n% 'coop_past') )
cossim <- lapply(nets, function(net) as.matrix(net %n% 'cat_cos_sim'))
centjoin <- lapply(nets, function(net) as.matrix(net %n% 'joint_cent_pow_n0_4'))
centratio <- lapply(nets, function(net) as.matrix(net %n% 'cent_ratio_pow_n0_4'))
shcomp <- lapply(nets, function(net) as.matrix(net %n% 'shared_competitor'))
shinv <- lapply(nets, function(net) as.matrix(net %n% 'shared_investor_nd'))

## TERGM RESULT
results_file <- file.path(results_dir, sprintf('fit_%s_pd%s_R%s_%s.rds',firm_i,nPeriod,R,m_x))
fits <- readRDS(results_file)
fit <- fits[[firm_i]][[m_x]]

#### SAVE GOODNESS OF FIT
gf <- gof(fit, nsim=nsim, statistics=c(rocpr), parallel = parallel, ncpus = ncpus)  ## rocpr
gof.file <- sprintf('%s/gof_%s_pd%s_R%s_%s_nsim%s_rocpr.rds', results_dir, firm_i, nPeriod, R, m_x, nsim)
saveRDS(gf, file=gof.file)

cat('finished successfully.')

