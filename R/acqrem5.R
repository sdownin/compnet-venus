cat('\n\n');timestamp();cat('\n')
library(relevent)
library(informR)
library(texreg)

mod <- 5

data_dir <- '/home/sdowning/data'
results_dir <- '/home/sdowning/compnet/results'

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

effects <- c('CovSnd', 'CovEvent') # NODSnd
##  [1] mmc.sum,     mmc.sum.sq,   num.mkts,    deg,          pow.n4,  
##  [6] pow.n3,      pow.n2,       pow.n1,      pow.1,        pow.2,
## [11] pow.3,       pow.4,        betweenness, constraint,   eig
cov.idx <- c(1,2,3,4,  6)
ar.cov.na0 <- ar.cov[ , cov.idx, ]
ar.cov.na0[is.na(ar.cov.na0)] <- 0
## MMC x target location (local/global) interaction
cat("computing MMC target location interaction array...")
dms <- dim(ar.cov.na0)
ar.cov.event <- array(dim=c(dms[1], 1, dms[3], dms[3]))
for (i in 1:length(ar.cov.na0[,1,1])) {
  mmc <- ar.cov.na0[i,1,]
  target.position <- CovRec
  ar.cov.event[i,1, , ] <- outer(mmc, target.position, '*')
}
cat("done.\n")
##
covar <- list(CovSnd=ar.cov.na0, CovEvent=ar.cov.event)
##
fit <- rem.dyad(edgelist = el, n = nrow(df.verts), effects = effects, ordinal = F, 
                covar = covar, fit.method = "BPM", gof=F, hessian = T, verbose = T)
summary(fit)
saveRDS(list(fit=fit), file = sprintf("acq_rem_fit_m%s.rds", mod))

htmlreg(list(fit), digits = 3, file = sprintf("acq_rem_fit_m%s.html", mod))

####################### DEFINE MODELS ###################################



cat('finished successfully.')
