cat('\n\n');timestamp();cat('\n')
library(btergm)
library(parallel)
library(texreg)
library(igraph)

data_dir <- '/home/sdowning/data/firm_nets_rnr'

firm_i <- 'qualtrics'
d <- 3
ncpus <- 4
parallel <- "multicore"

data_file <- file.path(data_dir,sprintf('%s_d%s.rds',firm_i,d))
nets <- readRDS(data_file)

nPeriods <- 11  ## 5


if (!("fits" %in% ls())) fits <- list()
if (!(firm_i %in% names(fits)) ) fits[[firm_i]] <- list()
if (nPeriods < length(nets))   nets <- nets[(length(nets)-nPeriods+1):length(nets)] 

cat("\n------------ estimating TERGM for:",firm_i,'--------------\n')
cat(sprintf("Using %s cores\n", detectCores()))

## make covars  list
mmc <- lapply(nets, function(net) as.matrix(net %n% 'mmc'))
cpc <- lapply(nets, function(net) as.matrix(net %n% 'coop'))
cpp <- lapply(nets, function(net) as.matrix(net %n% 'coop_past'))
cpa <- lapply(nets, function(net) as.matrix(net %n% 'coop') + as.matrix(net %n% 'coop_past') )
h1  <- lapply(nets, function(net) {
    gi <- net %v% 'genidx_multilevel'
    n <- length(gi)
    return( as.matrix( sapply(seq_len(n), function(x) gi ) ) )
})
h2 <- lapply(nets, function(net) {
    pc  <- net %v% 'cent_pow_n0_5'
    return( as.matrix( abs(outer(pc,pc,'-')) ) )
})
h3 <- lapply(nets, function(net) {
    g <- igraph::graph_from_adjacency_matrix(as.matrix(net[,]), mode="undirected")
    tri <- as.integer(igraph::triangles(g))
    n <- length(tri)
    return( as.matrix( sapply(seq_len(n), function(x) tri) ) )
})


####################### DEFINE MODELS ###################################

m4_6cycle_t1_hall <-   nets ~ edges + gwesp(0, fixed = T) + gwdegree(0, fixed=T) + 
  nodematch("ipo_status", diff = F) + 
  nodematch("state_code", diff = F) + 
  nodecov("age") + absdiff("age") + 
  edgecov(mmc) + 
    ##edgecov(cpa) +
    ##edgecov(cpc) + 
    ##edgecov(cpp) +
  memory(type = "stability", lag = 1) + 
  nodecov("genidx_multilevel") + 
  nodecov("cent_pow_n0_4") + absdiff("cent_pow_n0_4") + 
  cycle(3) + cycle(4) + cycle(5) + cycle(6) +
  timecov(transform=function(t) t) +
  timecov(x=h1, transform=function(t) t) +
  timecov(x=h2, transform=function(t) t) +
  timecov(x=h3, transform=function(t) t)

################################ end models#######################


##
# DEFINE MODEL and MODEL NAME TO COMPUTE
## 
m_x <- 'm4_6cycle_t1_hall'
##
# SET RESAMPLES
##
R <- 2000

## RUN TERGM
fits[[firm_i]][[m_x]] <- btergm(get(m_x), R=R, parallel = parallel, ncpus = ncpus)

## SAVE SERIALIZED
fits.file <- sprintf('/home/sdowning/compnet/results/amj_rnr/fit_%s_pd%s_R%s_%s.rds', firm_i, nPeriods, R, m_x)
saveRDS(fits, file=fits.file)

## SAVE FORMATTED REGRESSION TABLE
html.file <- sprintf('/home/sdowning/compnet/results/amj_rnr/%s_tergm_results_pd%s_R%s_%s.html',  firm_i, nPeriods, R, m_x)
htmlreg(fits[[firm_i]], digits = 3, file=html.file)

#### SAVE GOODNESS OF FIT
##gf <- gof(fits[[firm_i]][[m_x]], nsim=1000, 
##          statistics=c(dsp, esp, deg, geodesic, rocpr, walktrap.modularity))
##gof.file <- sprintf('/home/sdowning/compnet/results/amj_rnr/gof_%s_pd%s_R%s_%s.rds', firm_i, nPeriods, R, m_x)
##saveRDS(gf, file=gof.file)

cat('finished successfully.')

