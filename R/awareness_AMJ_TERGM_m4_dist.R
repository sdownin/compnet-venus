cat('\n\n');timestamp();cat('\n')
library(btergm)
library(parallel)
library(texreg)

data_dir <- '/home/sdowning/data/firm_nets_cem'

firm_i <- 'qualtrics'

nPeriods <- 7  ## 5

cat("\n------------ estimating TERGM for:",firm_i,'--------------\n')
cat(sprintf("Using %s cores\n", detectCores()))


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
################################ end models#######################

readCombinePdNets <- function(firm_i, d, data_dir) 
{
  nets <- list()
  base <-  sprintf("%s_d%s_pd", firm_i, d)
  files <- dir(path= data_dir, pattern = sprintf("%s\\d{2,4}\\.rds", base) )
  pds <- unname(sapply(files, function(file){
    tail <- gsub(base, "", file)
    pdstr <- strsplit(tail, "\\.rds", perl = T)[[1]][1]
    return(as.numeric(pdstr))
  }))
  for (t in 1:length(pds)) {
    pd <- pds[t]
    data_file <- file.path(data_dir,sprintf('%s_d%s_pd%s.rds', firm_i, d, pd))
    nets[[pd]] <- readRDS(data_file)
    cat(sprintf("loaded %s pd net; object size %s\n", pd, object.size(nets)))
    if (t >= 2) break;
  }
  return(nets)
}

##
# SET RESAMPLES
##
R <- 1000
##
# SET MODEL
##
m_x <- 'm4'
mod <-  m4

#############################################################################
##
# DISTANCE
##
d <- 2

cat(sprintf("computing %s networks for distance d = %s:\n", firm_i, d))
## LOAD DATA
data_file <- file.path(data_dir,sprintf('%s_d%s.rds',firm_i,d))
nets <- readRDS(data_file)
if (nPeriods < length(nets)) 
  nets <- nets[(length(nets)-nPeriods+1):length(nets)] 
## make MMC nets list
mmc <- lapply(nets, function(net) as.matrix(net %n% 'mmc'))

## RUN TERGM
fit <- tryCatch(
  btergm(mod, R=R, parallel = "multicore", ncpus = detectCores())
  , error = function(e)e
)
if (!inherits(fit, "error")) {
  ## SAVE SERIALIZED
  fits.file <- sprintf('/home/sdowning/compnet/results/fit_%s_pd%s_d%s_R%s_%s.rds', 
                       firm_i, nPeriods, d, R, m_x)
  saveRDS(fit, file=fits.file)
  ## SAVE FORMATTED REGRESSION TABLE
  html.file <- sprintf('/home/sdowning/compnet/results/%s_tergm_results_pd%s_d%s_R%s_%s.html',  
                       firm_i, nPeriods, d, R, m_x)
  htmlreg(fit, digits = 3, file=html.file)
} else {
  cat(sprintf("\nfirm %s error msg: %s\n", firm_i, fit))
}
###############################################################################

###############################################################################
##
# DISTANCE
##
d <- 4

##
# MODEL
##
m_x <- 'm4'
mod <-  m4

cat(sprintf("computing %s networks for distance d = %s:\n", firm_i, d))
## LOAD DATA
nets <- readCombinePdNets(firm_i, d, data_dir)
if (nPeriods < length(nets))   
  nets <- nets[(length(nets)-nPeriods+1):length(nets)] 
## make MMC nets list
mmc <- lapply(nets, function(net) as.matrix(net %n% 'mmc'))

## RUN TERGM
fit <- tryCatch(
  btergm(mod, R=R, parallel = "multicore", ncpus = detectCores())
  , error = function(e)e
)
if (!inherits(fit, "error")) {
  ## SAVE SERIALIZED
  fits.file <- sprintf('/home/sdowning/compnet/results/fit_%s_pd%s_d%s_R%s_%s.rds', 
                       firm_i, nPeriods, d, R, m_x)
  saveRDS(fit, file=fits.file)
  ## SAVE FORMATTED REGRESSION TABLE
  html.file <- sprintf('/home/sdowning/compnet/results/%s_tergm_results_pd%s_d%s_R%s_%s.html',  
                       firm_i, nPeriods, d, R, m_x)
  htmlreg(fit, digits = 3, file=html.file)
} else {
  cat(sprintf("\nfirm %s error msg: %s\n", firm_i, fit))
}
###############################################################################

cat('finished successfully.')

