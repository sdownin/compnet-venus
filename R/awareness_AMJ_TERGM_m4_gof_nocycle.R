cat('\n\n');timestamp();cat('\n')
library(btergm)
library(parallel)
library(texreg)

# ## ## local windows
# base_dir <- 'C:/Users/T430/Google Drive/PhD/Dissertation/competition networks'
# repo <- 'compnet2'
# ## remote unix/linux
base_dir <- '/home/sdowning'
repo <- 'compnet'

data_dir <- file.path(base_dir, 'data', 'firm_nets_cem')
results_dir <- file.path(base_dir, repo, 'results')

firm_i <- 'qualtrics'
d <- 3
R <- 2000
m_x <- 'm4'
nPeriods <- 7  ## 5
net_group <- 'cem'

fit_file <- file.path(results_dir,sprintf('fit_%s_pd%s_R%s_%s.rds',firm_i,nPeriods,R,m_x))
tmp <- readRDS(fit_file)
fit <- tmp[[net_group]][[firm_i]][[m_x]]

data_file <- file.path(data_dir,sprintf('%s_d%s.rds',firm_i,d))
nets <- readRDS(data_file)



if (nPeriods < length(nets))   nets <- nets[(length(nets)-nPeriods+1):length(nets)] 

cat("\n------------ estimating TERGM for:",firm_i,'--------------\n')
cat(sprintf("Using %s cores\n", detectCores()))

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
################################ end models#######################


##
# DEFINE MODEL and MODEL NAME TO COMPUTE
## 
mod <- m4
m_x <- 'm4'
##
# SET RESAMPLES
##
#R <- 2000

nsim <- 100

## make MMC nets list
mmc <- lapply(nets, function(net) as.matrix(net %n% 'mmc'))

## RUN TERGM
gof <- tryCatch(
  btergm::gof(object = fit, nsim=nsim, target=nets, 
              statistics = c(dsp, esp, deg, geodesic, rocpr))
  , error = function(e)e
)
if (!inherits(gof, "error")) {
  ## SAVE SERIALIZED
  gof.file <- sprintf('%s/gof_nocycle_%s_pd%s_d%s_R%s_%s_nsim%s.rds', 
                       results_dir, firm_i, nPeriods, d, R, m_x, nsim)
  saveRDS(gof, file=gof.file)
  ## PRINT
  print(gof)
} else {
  cat(sprintf("\nfirm %s error msg: %s\n", firm_i, fit))
}


cat('finished successfully.')

