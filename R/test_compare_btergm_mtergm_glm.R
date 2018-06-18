library(igraph)
library(intergraph)
library(btergm)
library(xergm)
library(parallel)
library(texreg)
library(stringr)

## DIRECTORIES
data_dir <- "C:/Users/T430/Google Drive/PhD/Dissertation/crunchbase/crunchbase_export_20161024"
work_dir <- "C:/Users/T430/Google Drive/PhD/Dissertation/competition networks/compnet2"
img_dir  <- "C:/Users/T430/Google Drive/PhD/Dissertation/competition networks/envelopment/img"

## set woring dir
setwd(work_dir)

name_i <- 'qualtrics'
d <- 2

nets.all <- readRDS(sprintf('firm_nets_rnr/%s_d%s.rds',name_i,d))


nets <- nets.all[7:(length(nets.all)-1)]


mmc <- lapply(nets, function(net) as.matrix(net %n% 'mmc'))
# cpc <- lapply(nets, function(net) as.matrix(net %n% 'coop'))
cpp <- lapply(nets, function(net) as.matrix(net %n% 'coop_past'))



m4 <-   nets ~ edges + gwesp(0, fixed = T) + #gwdegree(0, fixed=T, cutoff=30) +
  nodematch("ipo_status", diff = F) +
  nodematch("state_code", diff = F) +
  nodecov("age") + absdiff("age") +
  # edgecov(mmc) + 
  # edgecov(cpp) + #edgecov(cpc) +
  nodecov("njobs_multilevel") + #absdiff("njobs_multilevel") +
  nodecov("cent_pow_n0_2") + absdiff("cent_pow_n0_2") +
  cycle(3)  + #cycle(4) + cycle(5) + 
  # timecov(transform=function(t)t) +  
  # timecov(mmc, minimum=2, maximum=6, transform=function(t)t) +  
  memory(type = "stability", lag = 1)




##
# DEFINE MODEL and MODEL NAME TO COMPUTE
##
m_x <- 'm4'
##
# SET RESAMPLES
##
R <- 30


## RUN TERGM
fits <- btergm(get(m_x), R=R, parallel = "multicore", ncpus = detectCores())

saveRDS(fits, 'test_nets_qualtrics_d2.rds')

screenreg(fits)

## GOF
gf <- gof(fits, target=nets.all[length(nets.all)],
          nsim=50, 
          statistics=c(dsp, esp, deg, ideg, 
                       geodesic, rocpr, walktrap.modularity))
par(mfrow=c(1,5))
plot(gf)


## Compare to GLM
X <- cbind(fits@response,fits@effects)
fixnames <- unname(sapply(names(fits@effects),function(x)str_replace_all(x,"[\\[i\\]]","")))
names(X) <- c('y',fixnames)

formula1 <- as.formula(sprintf('y ~ %s',paste(fixnames,collapse=" + ")))
glfit <- glm(formula1 ,
             data=X,
             family=binomial(link = "logit"))

screenreg(list(glm=glfit,tergm=fits), ci.force = T, digits=3)


fitm <- mtergm(get(m_x))


screenreg(list(glm=glfit,btergm=fits,mtergm=fitm), digits=3)



m0 <-   nets ~ edges + gwesp(0, fixed = T) +
  nodematch("ipo_status", diff = F) +
  nodematch("state_code", diff = F) +
  nodecov("age") + absdiff("age") +
  # edgecov(mmc) + 
  # edgecov(cpp) + #edgecov(cpc) +
  nodecov("njobs_multilevel") + #absdiff("njobs_multilevel") +
  nodecov("cent_pow_n0_2") + absdiff("cent_pow_n0_2") +
  cycle(3)  #+ #cycle(4) + cycle(5) + 




##
# DEFINE MODEL and MODEL NAME TO COMPUTE
##
m_x <- 'm0'
##
# SET RESAMPLES
##
R <- 30


## RUN TERGM
fits0 <- btergm(get(m_x), R=R, parallel = "multicore", ncpus = detectCores())

saveRDS(fits0, 'test_nets_qualtrics_d2.rds')

screenreg(fits0)

gf0 <- gof(fits0, nsim=30, statistics=c(dsp, esp, deg, ideg, geodesic, rocpr, 
                                      walktrap.modularity))
plot(gf0)
