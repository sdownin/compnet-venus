cat('\n\n');timestamp();cat('\n')
library(btergm)
library(parallel)
library(texreg)

data_dir <- '/home/sdowning/data/firm_nets_rnr2'

firm_i <- 'satmetrix'
d <- 3
ncpus <- 4
parallel <- "multicore"

data_file <- file.path(data_dir,sprintf('%s_d%s.rds',firm_i,d))
nets <- readRDS(data_file)

nPeriods <- 11  ## 5


if (!("fits" %in% ls())) fits <- list()
if (!(firm_i %in% names(fits)) ) fits[[firm_i]] <- list()
if (nPeriods < length(nets))   nets <- nets[(length(nets)-nPeriods+1):length(nets)] 

cat("\n------------ estimating TERGM for:",firm_i,'--------------\n')
cat(sprintf("Using %s cores\n", detectCores()))

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

####################### DEFINE MODELS ###################################

m4 <-   nets ~ edges + gwesp(0, fixed = T) + gwdegree(0, fixed=T) + 
  nodematch("ipo_status", diff = F) + 
  nodematch("state_code", diff = F) + 
  nodecov("age") + absdiff("age") + 
    ##nodecov("employee_na_age") +
    ##nodecov("sales_na_0_mn") +
  edgecov(cossim) +
  edgecov(centjoin) + 
    ##edgecov(shcomp) + 
  edgecov(shinv) +   
  edgecov(mmc) + 
    ##edgecov(cpa) +
    ##edgecov(cpc) + 
    ##edgecov(cpp) +
  memory(type = "stability", lag = 1) + 
  timecov(transform = function(t) t) +
  nodecov("genidx_multilevel") + 
  nodecov("cent_pow_n0_4") + absdiff("cent_pow_n0_4") + 
  cycle(3) + cycle(4) + cycle(5) 

################################ end models#######################


##
# DEFINE MODEL and MODEL NAME TO COMPUTE
## 
m_x <- 'm4'
##
# SET RESAMPLES
##
R <- 2000

## RUN TERGM
fits[[firm_i]][[m_x]] <- btergm(get(m_x), R=R, parallel = parallel, ncpus = ncpus)

## SAVE SERIALIZED
fits.file <- sprintf('/home/sdowning/compnet/results/amj_rnr2/fit_%s_pd%s_R%s_%s.rds', firm_i, nPeriods, R, m_x)
saveRDS(fits, file=fits.file)

## SAVE FORMATTED REGRESSION TABLE
html.file <- sprintf('/home/sdowning/compnet/results/amj_rnr2/%s_tergm_results_pd%s_R%s_%s.html',  firm_i, nPeriods, R, m_x)
htmlreg(fits[[firm_i]], digits = 2, file=html.file)

#### SAVE GOODNESS OF FIT
##gf <- gof(fits[[firm_i]][[m_x]], nsim=1000, 
##          statistics=c(dsp, esp, deg, geodesic, rocpr, walktrap.modularity))
##gof.file <- sprintf('/home/sdowning/compnet/results/amj_rnr2/gof_%s_pd%s_R%s_%s.rds', firm_i, nPeriods, R, m_x)
##saveRDS(gf, file=gof.file)

cat('finished successfully.')

