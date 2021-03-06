cat('\n\n');timestamp();cat('\n')
library(btergm)
library(parallel)
library(texreg)

data_dir <- '/home/sdowning/data/firm_nets_rnr'

firm_i <- 'qualtrics'
d <- 3
ncpus <- 4
parallel <- "multicore"

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
cpab <- lapply(cpa, function(net) {tmp <- net; tmp[tmp>=1] <- 1; return(tmp)} )

####################### DEFINE MODELS ###################################

m41 <-   nets ~ edges + gwesp(0, fixed = T) + gwdegree(0, fixed=T) +
  nodematch("ipo_status", diff = F) + 
  nodematch("state_code", diff = F) + 
  nodecov("age") + absdiff("age") + 
  nodecov("cent_deg") +
  edgecov(mmc) + 
  ##edgecov(cpab) +
  ##edgecov(cpa) +
  ##edgecov(cpc) + 
    ##edgecov(cpp) +
  memory(type = "stability", lag = 1) + 
  timecov(transform = function(t) t) +
  nodecov("njobs_multilevel") +
  nodecov("cent_pow_n0_4") + absdiff("cent_pow_n0_4") + 
  cycle(3) + cycle(4) + cycle(5) 

################################ end models#######################


##
# DEFINE MODEL and MODEL NAME TO COMPUTE
## 
m_x <- 'm41'
##
# SET RESAMPLES
##
R <- 300

## RUN TERGM
fits[[firm_i]][[m_x]] <- btergm(get(m_x), R=R, parallel = parallel, ncpus = ncpus)

## SAVE SERIALIZED
fits.file <- sprintf('/home/sdowning/compnet/results/amj_rnr/fit_%s_pd%s_R%s_%s.rds', firm_i, nPeriods, R, m_x)
saveRDS(fits, file=fits.file)

## SAVE FORMATTED REGRESSION TABLE
html.file <- sprintf('/home/sdowning/compnet/results/amj_rnr/%s_tergm_results_pd%s_R%s_%s.html',  firm_i, nPeriods, R, m_x)
htmlreg(fits[[firm_i]], digits = 3, file=html.file)


cat('finished successfully.')

