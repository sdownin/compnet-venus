cat('\n\n');timestamp();cat('\n')
library(relevent)
library(informR)
library(texreg)

mod <- 1

data_dir <- '/home/sdowning/data'

l1 <- readRDS(file.path(data_dir,"acquisitions_verts_df_2.rds"))
l2 <- readRDS(file.path(data_dir,"acquisitions_rem_covs_2.rds"))
l3 <- readRDS(file.path(data_dir,"acquisitions_cov_list_2.rds"))
l4 <- readRDS(file.path(data_dir,"acquisitions_cov_rec_2.rds"))
df.verts <- l1$df.verts
acq.src.allpd <- l1$acq.src.allpd
df.rem <- l2$df.rem
ar.cov <- l2$ar.cov
df.verts.pd.cov <- l3[[1]]$cov
CovRec <- l4$CovRec

##-------------------------------------------------------------------------------

el <- data.frame(
  t = df.rem$t,
  s.f = sapply(as.character(df.rem$src), function(x)df.verts$id[which(x==df.verts$name)]),
  t.f = sapply(as.character(df.rem$trg), function(x)df.verts$id[which(x==df.verts$name)]),
  stringsAsFactors = F
)

effects <- c('CovSnd') # CovRec NODSnd
##  [1] mmc.sum,     mmc.sum.sq,   num.mkts,    deg,          pow.n4,  
##  [6] pow.n3,      pow.n2,       pow.n1,      pow.1,        pow.2,
## [11] pow.3,       pow.4,        betweenness, constraint,   eig
cov.idx <- c(1,2,3,4,  6)
ar.cov.na0 <- ar.cov[ , cov.idx, ]
ar.cov.na0[is.na(ar.cov.na0)] <- 0
##
covar <- list(CovSnd=ar.cov.na0)
fit <- rem.dyad(edgelist = el, n = nrow(df.verts), effects = effects, ordinal = F, 
                          covar = covar, fit.method = "BPM", gof=F, hessian = T, verbose = T)
summary(fit)
saveRDS(list(fit=fit), file = sprintf("acq_rem_m%s.rds", mod))

####################### DEFINE MODELS ###################################



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
