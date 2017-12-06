cat('\n\n');timestamp();cat('\n')
library(btergm)
library(parallel)
library(texreg)

data_dir <- 'C:/Users/T430/Google Drive/PhD/Dissertation/competition networks/compnet2/firm_nets_cem'


data_file <- file.path(data_dir,paste0(firm_i,'.rds'))
nets <- readRDS(data_file)

firm_i <- 'qualtrics'
net_group <- 'cem'

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

