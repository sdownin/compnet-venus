cat('\n\n');timestamp();cat('\n')
library(btergm)
library(parallel)
library(texreg)

# data_dir <- '/home/sdowning/data/firm_nets_cem'
data_dir <- "C:/Users/T430/Google Drive/PhD/Dissertation/competition networks/compnet2/firm_nets_rnr"
results_dir <- "C:/Users/T430/Google Drive/PhD/Dissertation/competition networks/compnet2/amj_rnr_results"

firm_i <- 'qualtrics'
d <- 3

data_file <- file.path(data_dir,sprintf('%s_d%s.rds',firm_i,d))
nets <- readRDS(data_file)

nPeriods <- 11  ## 5

if (nPeriods < length(nets))   nets <- nets[(length(nets)-nPeriods+1):length(nets)] 

cat("\n------------ estimating TERGM for:",firm_i,'--------------\n')
cat(sprintf("Using %s cores\n", detectCores()))

## make MMC nets list
mmc <- lapply(nets, function(net) as.matrix(net %n% 'mmc'))
cpc <- lapply(nets, function(net) as.matrix(net %n% 'coop'))
cpp <- lapply(nets, function(net) as.matrix(net %n% 'coop_past'))
cpa <- lapply(nets, function(net) as.matrix(net %n% 'coop') + as.matrix(net %n% 'coop_past') )

####################### DEFINE MODELS ###################################

m1 <-   nets ~ edges + gwesp(0, fixed = T) + gwdegree(0, fixed=T) + 
  nodematch("ipo_status", diff = F) + 
  nodematch("state_code", diff = F) + 
  nodecov("age") + absdiff("age") + 
  edgecov(mmc) + 
  edgecov(cpa) +
  memory(type = "stability", lag = 1) + 
  nodecov("genidx_multilevel") + 
  nodecov("cent_pow_n0_4")  

m4 <-   nets ~ edges + gwesp(0, fixed = T) + gwdegree(0, fixed=T) + 
  nodematch("ipo_status", diff = F) + 
  nodematch("state_code", diff = F) + 
  nodecov("age") + absdiff("age") + 
  edgecov(mmc) + 
  edgecov(cpa) +
  ##edgecov(cpc) + 
  ##edgecov(cpp) +
  memory(type = "stability", lag = 1) + 
  nodecov("genidx_multilevel") + 
  nodecov("cent_pow_n0_4") + absdiff("cent_pow_n0_4") + 
  cycle(3) + cycle(4) + cycle(5) 

################################ end models#######################


models <- list(m4=m4)
# models <- list(m1=m1)

##
# SET RESAMPLES
##
R <- 500


##
# MAIN
##
for (i in 1:length(models)) {
  m_x <- names(models)[i]

  ## RUN TERGM
  fit <- tryCatch(
    btergm(models[[m_x]], R=R, parallel = "multicore", ncpus = detectCores())
    , error = function(e)e
  )
  
  if (!inherits(fit, "error")) {
    ## SAVE SERIALIZED
    fits.file <- sprintf('%s/fit_winlocal_v2__%s_pd%s_d%s_R%s_%s.rds', 
                         results_dir, firm_i, nPeriods, d, R, m_x)
    saveRDS(fit, file=fits.file)
    
    ## SAVE FORMATTED REGRESSION TABLE
    html.file <- sprintf('%s/%s_tergm_results_winlocal_v2__pd%s_d%s_R%s_%s.html',  
                         results_dir, firm_i, nPeriods, d, R, m_x)
    htmlreg(fit, digits = 3, file=html.file)
    
  } else {
    cat(sprintf("\nfirm %s error msg: %s\n", firm_i, fit))
  }
  
}


cat('finished successfully.')


# fits <- list()
# for (i in 1:4) {
#   m_x <- paste0('m',i);
#   fits.file <- sprintf('%s/fit_winlocal_v2__%s_pd%s_d%s_R%s_%s.rds', results_dir, firm_i, nPeriods, d, R, m_x)
#   fits[[m_x]] <- readRDS(fits.file)
# }
fits.file <- sprintf('%s/fit_winlocal_v2__%s_pd%s_d%s_R%s_%s.rds', results_dir, firm_i, nPeriods, d, R, 'm4')
m4<- readRDS(fits.file)




##---------------- GOF -------------------------------

nsim <- 50
R <- 2000
firm_i <- 'qualtrics'
pd <- 6
nPeriods <- 7 ## before deducline lag 1
nets.gof <- nets[(length(nets)-pd+1):length(nets)]

models <- list(m4=m4, m3=m3, m2=m2, m1=m1, m0=m0)

## completed m4
# models$m4 <- NULL

for (i in 1:length(models)) {
  m_x <- names(models)[i]
  mod <- models[[m_x]]
  
  ## SAVE SERIALIZED
  fits.file <- sprintf('%s/fit_winlocal_v2__%s_pd%s_d%s_R%s_%s.rds', 
                       results_dir, firm_i, nPeriods, d, R, m_x)
  fit <- readRDS(file=fits.file)
  gc()
  gof <- tryCatch(
    btergm::gof(object = fit, nsim=nsim, target=nets.gof, 
                statistics = c(rocpr)) ##  c(dsp, esp, deg, geodesic, rocpr)
    , error = function(e)e
  )
  if (!inherits(gof, "error")) {
    gof.file <- sprintf('%s/gof_auc_roc_winlocal_v2__%s_pd%s_d%s_R%s_%s_nsim%s.rds', 
                        results_dir, firm_i, nPeriods, d, R, m_x, nsim)
    saveRDS(gof, file=gof.file)
    print(gof)
  } else {
    cat(sprintf("ERROR: %s", gof))
  }
 
}


##----------------------- CORRELATIONS
library(psych)

X <- fits$m4@effects
X <- X[X$nodecov.age < 110 & X$nodecov.age >= 0, ]

(desc <- psych::describe(X))
write.csv(desc, file = 'qualtrics_m4_describe.csv', row.names = T)

(cr <- cor(X))
write.csv(cr, file = 'qualtrics_m4_correlations.csv', row.names = T)


(ct <- psych::corr.test(X))
write.csv(ct$p, file = 'qualtrics_m4_corr_test_p.csv', row.names = T)


