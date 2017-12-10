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
cat(sprintf("Using %s cores\n", detectCores()))

## make MMC nets list
mmc <- lapply(nets, function(net) as.matrix(net %n% 'mmc'))

####################### DEFINE MODELS ###################################
m41  <-   nets ~ edges + gwesp(0, fixed = T) + 
  nodematch("ipo_status", diff = F) + 
  nodematch("state_code", diff = F) + 
  nodecov("age") + absdiff("age") + 
  edgecov(mmc) +
  memory(type = "stability", lag = 1) + 
  nodecov("genidx_multilevel") +
  nodecov("cent_pow_n0_1") + absdiff("cent_pow_n0_1") + 
  cycle(3) + cycle(4) + cycle(5) 

m42  <-   nets ~ edges + gwesp(0, fixed = T) + 
  nodematch("ipo_status", diff = F) + 
  nodematch("state_code", diff = F) + 
  nodecov("age") + absdiff("age") + 
  edgecov(mmc) +
  memory(type = "stability", lag = 1) + 
  nodecov("genidx_multilevel") +
  nodecov("cent_pow_n0_2") + absdiff("cent_pow_n0_2") + 
  cycle(3) + cycle(4) + cycle(5) 

m43  <-   nets ~ edges + gwesp(0, fixed = T) + 
  nodematch("ipo_status", diff = F) + 
  nodematch("state_code", diff = F) + 
  nodecov("age") + absdiff("age") + 
  edgecov(mmc) +
  memory(type = "stability", lag = 1) + 
  nodecov("genidx_multilevel") +
  nodecov("cent_pow_n0_3") + absdiff("cent_pow_n0_3") + 
  cycle(3) + cycle(4) + cycle(5) 

m44  <-   nets ~ edges + gwesp(0, fixed = T) + 
  nodematch("ipo_status", diff = F) + 
  nodematch("state_code", diff = F) + 
  nodecov("age") + absdiff("age") + 
  edgecov(mmc) +
  memory(type = "stability", lag = 1) + 
  nodecov("genidx_multilevel") +
  nodecov("cent_pow_n0_4") + absdiff("cent_pow_n0_4") + 
  cycle(3) + cycle(4) + cycle(5) 

m45 <-   nets ~ edges + gwesp(0, fixed = T) + 
  nodematch("ipo_status", diff = F) + 
  nodematch("state_code", diff = F) + 
  nodecov("age") + absdiff("age") + 
  edgecov(mmc) +
  memory(type = "stability", lag = 1) + 
  nodecov("genidx_multilevel") +
  nodecov("cent_pow_n0_5") + absdiff("cent_pow_n0_5") + 
  cycle(3) + cycle(4) + cycle(5) 

m46 <-   nets ~ edges + gwesp(0, fixed = T) + 
  nodematch("ipo_status", diff = F) + 
  nodematch("state_code", diff = F) + 
  nodecov("age") + absdiff("age") + 
  edgecov(mmc) +
  memory(type = "stability", lag = 1) + 
  nodecov("genidx_multilevel") +
  nodecov("cent_pow_n0_5") + absdiff("cent_pow_n0_5") + 
  cycle(3) + cycle(4) + cycle(5) + cycle(6) 

m47 <-   nets ~ edges + gwesp(0, fixed = T) + 
  nodematch("ipo_status", diff = F) + 
  nodematch("state_code", diff = F) + 
  nodecov("age") + absdiff("age") + 
  edgecov(mmc) +
  memory(type = "stability", lag = 1) + 
  nodecov("genidx_multilevel") +
  nodecov("cent_pow_n0_5") + absdiff("cent_pow_n0_5") + 
  cycle(3) + cycle(4) + cycle(5) + cycle(6) + cycle(7) 

################################ end models#######################

getModel <- function(m_x){
  switch(as.character(m_x),
    'm41'=m41, 'm42'=m42, 'm43'=m43, 'm44'=m44, 'm45'=m45, 'm46'=m46, 'm47'=m47
  )
}

##
# SET RESAMPLES
##
R <- 1000

for (i in 1:7) {

  m_x <- sprintf("m4%s", i)  #'m4'
  mod <-  getModel(m_x) # m4

  ## RUN TERGM
  fit <- tryCatch(
    btergm(mod, R=R, parallel = "multicore", ncpus = detectCores())
    , error = function(e)e
  )
  
  if (!inherits(fit, "error")) {
    ## SAVE SERIALIZED
    fits.file <- sprintf('/home/sdowning/compnet/results/fit_%s_pd%s_d%s_R%s_%s_betaNeg0_%s.rds', 
                         firm_i, nPeriods, d, R, m_x, i)
    saveRDS(fit, file=fits.file)
    
    ## SAVE FORMATTED REGRESSION TABLE
    html.file <- sprintf('/home/sdowning/compnet/results/%s_tergm_results_pd%s_d%s_R%s_%s_betaNeg0_%s.html',  
                         firm_i, nPeriods, d, R, m_x, i)
    htmlreg(fit, digits = 3, file=html.file)
    
  } else {
    cat(sprintf("\nfirm %s error msg: %s\n", firm_i, fit))
  }

}


cat('finished successfully.')

