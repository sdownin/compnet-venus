cat('\n\n');timestamp();cat('\n')
library(btergm)
library(parallel)
library(texreg)

data_dir <- '/home/sdowning/data/firm_nets_rnr'

firm_i <- 'bloomberg'
d <- 3

data_file <- file.path(data_dir,sprintf('%s_d%s.rds',firm_i,d))
nets <- readRDS(data_file)

nPeriods <- 10  ## 5


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

####################### DEFINE MODELS ###################################
cent.pow <- 'cent_pow_n0_4'

m4 <-   nets ~ edges + gwesp(0, fixed = T) + 
  nodematch("ipo_status", diff = F) + 
  nodematch("state_code", diff = F) + 
  nodecov("age") + absdiff("age") + 
  edgecov(mmc) + 
  edgecov(cpa) +
  ##edgecov(cpc) + 
  ##edgecov(cpp) +
  memory(type = "stability", lag = 1) + 
  nodecov("genidx_multilevel") + absdiff("genidx_multilevel") +
  nodecov(cent.pow) + absdiff(cent.pow) + 
  cycle(3) + cycle(4) + cycle(5) 

m3 <-   nets ~ edges + gwesp(0, fixed = T) + 
  nodematch("ipo_status", diff = F) + 
  nodematch("state_code", diff = F) + 
  nodecov("age") + absdiff("age") + 
  edgecov(mmc) +
  memory(type = "stability", lag = 1) + 
  cycle(3) + cycle(4) + cycle(5) 

m2 <-   nets ~ edges + gwesp(0, fixed = T) + 
  nodematch("ipo_status", diff = F) + 
  nodematch("state_code", diff = F) + 
  nodecov("age") + absdiff("age") + 
  edgecov(mmc) +
  memory(type = "stability", lag = 1) + 
  nodecov(cent.pow) + absdiff(cent.pow) 

m1 <-   nets ~ edges + gwesp(0, fixed = T) + 
  nodematch("ipo_status", diff = F) + 
  nodematch("state_code", diff = F) + 
  nodecov("age") + absdiff("age") + 
  edgecov(mmc) +
  memory(type = "stability", lag = 1) + 
  nodecov("genidx_multilevel")

m0 <-   nets ~ edges + gwesp(0, fixed = T) + 
  nodematch("ipo_status", diff = F) + 
  nodematch("state_code", diff = F) + 
  nodecov("age") + absdiff("age") + 
  edgecov(mmc) +
  memory(type = "stability", lag = 1) 
################################ end models#######################


##
# DEFINE MODEL and MODEL NAME TO COMPUTE
## 
m_x <- 'm4'
##
# SET RESAMPLES
##
R <- 300

## RUN TERGM
fits[[firm_i]][[m_x]] <- btergm(get(m_x), R=R, parallel = "multicore", ncpus = detectCores())

## SAVE SERIALIZED
fits.file <- sprintf('/home/sdowning/compnet/results/fit_%s_pd%s_R%s_%s.rds', firm_i, nPeriods, R, m_x)
saveRDS(fits, file=fits.file)

## SAVE FORMATTED REGRESSION TABLE
html.file <- sprintf('/home/sdowning/compnet/results/amj_rnr/%s_tergm_results_pd%s_R%s_%s.html',  firm_i, nPeriods, R, m_x)
htmlreg(fits[[firm_i]], digits = 3, file=html.file)


cat('finished successfully.')

