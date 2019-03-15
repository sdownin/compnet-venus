cat('\n\n');timestamp();cat('\n')
library(btergm)
library(parallel)
library(texreg)

data_dir <- '/home/sdowning/data/firm_nets_cem'

firm_i <- 'qualtrics'
d <- 3

data_file <- file.path(data_dir,sprintf('%s_d%s.rds',firm_i,d))
nets <- readRDS(data_file)

nPeriods <- 7  ## 5
net_group <- 'cem'

if (!("fits" %in% ls())) fits <- list()
if (!(net_group %in% names(fits))) fits[[net_group]] <- list()
if (!(firm_i %in% names(fits[[net_group]])) ) fits[[net_group]][[firm_i]] <- list()
if (nPeriods < length(nets))   nets <- nets[(length(nets)-nPeriods+1):length(nets)] 

cat("\n------------ estimating TERGM for:",firm_i,'--------------\n')
cat(sprintf("Using %s cores", detectCores()))

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
R <- 2000


## RUN TERGM
fits[[net_group]][[firm_i]][[m_x]] <- btergm(mod, R=R, parallel = "multicore", ncpus = detectCores())

## SAVE SERIALIZED
fits.file <- sprintf('/home/sdowning/compnet/results/fit_%s_pd%s_R%s_%s.rds', firm_i, nPeriods, R, m_x)
saveRDS(fits, file=fits.file)

## SAVE FORMATTED REGRESSION TABLE
html.file <- sprintf('/home/sdowning/compnet/results/%s_tergm_results_pd%s_R%s_%s.html',  firm_i, nPeriods, R, m_x)
htmlreg(fits[[net_group]][[firm_i]], digits = 3, file=html.file)


cat('finished successfully.')

