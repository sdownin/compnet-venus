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

###
str <- function(x){return(deparse(substitute(x)))}
###


firm_i <- 'qualtrics'
net_group <- 'cem'
nets <- readRDS(paste0('firm_nets_cem/',firm_i,'.rds'))
nPeriods <- 7  ## 5
net_group <- 'cem'

if (!("fits" %in% ls())) fits <- list()
if (!(net_group %in% names(fits))) fits[[net_group]] <- list()
if ( !(firm_i %in% names(fits[[net_group]])) ) fits[[net_group]][[firm_i]] <- list()


if (nPeriods < length(nets)) {
  nets <- nets[(length(nets)-nPeriods+1):length(nets)]
}

cat("\n------------ estimating TERGM for:",firm_i,'--------------\n')

## make MMC nets list
mmc <- lapply(nets, function(net) as.matrix(net %n% 'mmc'))

####################### DEFINE MODELS ###################################

m4 <-   nets ~ edges + gwesp(0, fixed = T) + 
  nodematch("ipo_status", diff = F) + 
  nodematch("state_code", diff = F) + 
  nodecov("age") + absdiff("age") + 
  edgecov(mmc) +
  memory(type = "stability", lag = 1) + 
  nodecov("genidx_multilevel") +
  nodecov("cent_pow_n0_5") + absdiff("cent_pow_n0_5") + 
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
  nodecov("cent_pow_n0_5") + absdiff("cent_pow_n0_5") 

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
mod <- m0
m_x <- 'm0'
##
# SET RESAMPLES
##
R <- 1

## RUN TERGM
fits[[net_group]][[firm_i]][[m_x]] <- btergm(mod, R=R, parallel = "multicore", ncpus = detectCores())

## SAVE SERIALIZED
fits.file <- sprintf('%s_pd%s_%s.rds', firm_i, nPeriods, m_x)
saveRDS(fits, file=fits.file)

## SAVE FORMATTED REGRESSION TABLE
html.file <- sprintf('%s_tergm_results_pd%s_%s.html',  firm_i, nPeriods, m_x)
htmlreg(fits[[net_group]][[firm_i]], digits = 3, file=html.file)


cat('finished successfully.')

