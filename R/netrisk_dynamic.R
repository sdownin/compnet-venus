setwd("C:/Users/sdowning/Google Drive/PhD/Dissertation/competition networks/compnet")
# .libPaths('C:/Users/sdowning/Documents/R/win-library/3.2')
library(parallel)
library(statnet, quietly = T)
library(network, quietly = T)
library(ergm, quietly = T)
library(tergm, quietly = T)
library(btergm, quietly = T)
library(hergm, quietly = T)
library(xergm, quietly = T)  ## includes rem, tnam, GERGM
library(texreg, quietly = T)
library(igraph, quietly = T)
library(plyr, quietly = T)
library(dplyr, quietly = T)
library(stringr, quietly = T)
library(ndtv, quietly = T)
library(visNetwork, quietly = T)
library(scatterplot3d, quietly = T)
library(lattice, quietly = T)
library(latticeExtra, quietly = T)
library(directlabels, quietly = T)
library(ggplot2, quietly = T)
data_dir <- "C:/Users/sdowning/Google Drive/PhD/Dissertation/crunchbase"
img_dir  <- "C:/Users/sdowning/Google Drive/PhD/Dissertation/competition networks/envelopment/img"
if( !('net' %in% ls()) )
  load('netrisk_dynamic_1.RData')
###
# save.image('netrisk_dynamic_1.RData')
###
source(file.path(getwd(),'R','comp_net_functions.R'))
source(file.path(getwd(),'R','netrisk_functions.R'))
source(file.path(getwd(),'R','cb_data_prep.R'))
par.default <- par()
lattice::trellis.par.set(strip.background=list(col="lightgrey"))
###########################################################################
## Firm type global variables
multi.prod <- c("cisco","google","microsoft","ibm","yahoo","oracle","hewlett-packard","intel",
                "aol","apple","facebook","amazon","adobe-systems","nokia","dell","sap","motorola",
                "groupon","autodesk","salesforce","iac","quest-software","zayo-group","sas",
                "qualcomm","blackberry","berkshire-hathaway-corp","carlyle-group","intuit",
                "symantec","broadcom","kkr","medtronic","vmware","sony","lg","motorola-mobility",
                "motorola-solutions")

single.prod <- c('netflix','medallia','dropbox','surveymonkey')  #
###########################################################################


#####################################################################################
## MAKE FULL COMP NET OF ALL RELATIONS IN DB 
#####################################################################################
g.full <- makeGraph(comp = comp, vertdf = co)
## fill in missing founded_year values from founded_at year
V(g.full)$founded_year <- ifelse( (!is.na(V(g.full)$founded_year)|V(g.full)$founded_year==''), V(g.full)$founded_year, as.numeric(substr(V(g.full)$founded_at,1,4)))
## cut out confirmed dates >= 2016
g.full <- igraph::induced.subgraph(g.full, vids=V(g.full)[which(V(g.full)$founded_year <= 2015 
                                                                | is.na(V(g.full)$founded_year)
                                                                | V(g.full)$founded_year=='' ) ] )
g.full <- igraph::delete.edges(g.full, E(g.full)[which(E(g.full)$relation_created_at >= '2016-01-01')])
## change NA funding to 0
V(g.full)[is.na(V(g.full)$funding_total_usd)]$funding_total_usd <- 0
## Make list of attributes functions to simplify duplicate edges
# to.min <- c('relation_created_at','competitor_founded_on','competitor_closed_on','founded_at','founded_month','founded_quarter','founded_year','acquired_at')
# to.collapse <- c('funding_total_usd','category_list','country_code','state_code','city')
# edgeAttrCombList <- list(weight='sum')
# gAttrList <- names(igraph::vertex.attributes(g.full))
# for (i in 1:length(to.min)) {
#   if (to.min[i] %in% gAttrList) {
#     edgeAttrCombList[[ to.min[i] ]] <- function(x){return(min(na.omit(x)))}
#   }
# }
# for (i in 1:length(to.collapse))  {
#   if (to.collapse[i] %in% gAttrList) {
#     edgeAttrCombList[[ to.collapse[i] ]] <- function(x)paste(na.omit(x), collapse = "||||")
#   }
# }
## SIMPLIFY
g.full <- igraph::simplify(g.full, remove.loops=T,remove.multiple=T, 
                           edge.attr.comb = list(weight='sum', 
                                                 relation_created_at='min',
                                                 competitor_founded_on='min',
                                                 competitor_closed_on='min'))




####################################################################
####################################################################
#-----------------------------------------------------------------
#
#                 Create Dynamic igraph form TERGM
#                   by ONLY REMOVING EDGES
#                   KEEP ALL N nodes
#             
#                 MAIN LOOP
#
#-----------------------------------------------------------------
name_i <- 'medallia'
yrpd <- 1
startYr <- 2007
endYr <- 2016
periods <- seq(startYr,endYr,yrpd)
company.name <- 'company_name_unique'
verbose <- TRUE
k <- 3
#
#g.base <- igraph::make_ego_graph(g.full,order=k,nodes=V(g.full)[V(g.full)$name=='surveymonkey'])[[1]]
g.base <- g.full
g.k.sub <- igraph::make_ego_graph(graph = g.base, nodes = V(g.full)[V(g.full)$name==name_i], order = k, mode = 'all')[[1]]
net.k.sub <- getNetFromIgraph(g.k.sub)
net <- net.k.sub
nd <- NA
nd <- initNetworkDynamic(net.k.sub, periods[1], periods[length(periods)])

#-----------------------------------
gd <- list()
for(t in length(periods):2) {
  cat(sprintf('\nmaking period %s-%s:\n', periods[t-1],periods[t]))
  # gd[[t]] <- makeIgraphPdSubgraphAllNActivateEdges(g.k.sub, start=periods[t-1], end=periods[t],acq=acq,rou=rou,br=br)
  nd <- updateNetworkDynamicPdActivateEdges(net = net.k.sub, nd = nd, start=periods[t-1], end=periods[t])
}; #names(gd) <- periods;  gd <- gd[2:length(gd)]


#-------------------------------------------------------
# nl <- list()
# nlnames <- c()
# afterlag <- 2
# nPeriods <- length(gl1)
# counter <- 1
# for (i in afterlag:nPeriods) {
#   # g <- g.list[[i]]
#   g <- gd[[i]]
#   if(class(g)=='igraph') {
#     if(ecount(g) > 1) {
#       #V(g)$type <- factor(ifelse(V(g)$name %in% multi.prod, 'MultiProd', 'SingleProd'))
#       er <- envRisk(g)
#       g <- er$g
#       ##
#       nl[[counter]] <- getNetFromIgraph(g)
#       nl[[counter]] <- network::set.network.attribute(nl[[counter]], 'envrisk', value = get.graph.attribute(g, 'envrisk'))
#       founded_year_filled <- nl[[counter]] %v% 'founded_year'
#       founded_year_filled[is.na(founded_year_filled)] <- median(founded_year_filled, na.rm = T)
#       nl[[counter]] %v% 'founded_year_filled' <-  founded_year_filled
#       nl[[counter]] %v% 'type' <- ifelse(nl[[counter]] %v% 'vertex.names' %in% multi.prod, 'MultiProd', 'SingleProd')
#       rdf <-  envRisk(gd[[i-1]], out.df = T)
#       nl[[counter]]  %v% 'envrisk_lag1' <- ifelse(any(is.na(rdf)), NA, rdf$envrisk)
#       nl[[counter]] <- network::set.network.attribute(nl[[counter]],'period',names(gd[i]))
#       nlnames[counter] <- names(gd[i])
#       #
#       counter <- counter + 1
#     }
#   }
#   cat(sprintf('i %s  counter %s  year %s\n',i,counter,names(gd[i])))
# }
# names(nl) <- nlnames
# nl <- nl[ ! sapply(nl, is.null) ]
#-------------------------------------------------------
############################################################################
#--------------------- BTERGM ------------------------------
############################################################################

nets <- list()
pds <- 2007:2016
for (i in 2:length(pds)) {
  nets[[i]] <- network(network.extract(nd, onset=pds[i-1], terminus=pds[i])[,], directed = F, hyper = F, multiple = F, loops = F, bipartite = F)
}; names(nets) <- pds; nets <- nets[-1]

fb1 <- btergm(nets ~ edges + gwesp(0, fixed=T) + kstar(3:6), 
              R = 500, parallel = "multicore", ncpus = detectCores())
fb2 <- btergm(nets ~ edges + gwesp(0, fixed=T) + gwdegree(0, fixed=T) + kstar(3:6) + cycle(4:6), 
              R = 500, parallel = "multicore", ncpus = detectCores())


texreg::screenreg(list(fb1=fb1,fb2=fb2), single.row = T, ci.force = F)

############################################################################
#--------------------- STERGM ------------------------------
############################################################################
c.s <- control.stergm(seed = 1111, parallel = 4, parallel.version.check = T)

# baseline check
f1 <- stergm(nd, 
             formation= ~ edges ,
             dissolution= ~ edges ,
             estimate="CMLE", control = c.s, times = 2008:2016)
write.summary(f1); plot(gof(f1))

f2 <- stergm(nd,
             formation= ~ edges + gwesp(0, fixed=T) + kstar(3:6),
             dissolution= ~ edges + gwesp(0, fixed=T) + kstar(3:6),
             estimate="CMLE",control = c.s, times = 2008:2016)
write.summary(f2); plot(gof(f2))

f3 <- stergm(nd,
             formation= ~ edges + gwesp(0, fixed=T) + cycle(4:6),
             dissolution= ~ edges + kstar(3:5) + triangle,
             estimate="CMLE",control = c.s, times = 2008:2016)
write.summary(f3); plot(gof(f3))

f4 <- stergm(nd,
             formation = ~ edges + gwesp(0, fixed=T) + kstar(3:6)  + cycle(4:6),
             dissolution = ~ edges + gwesp(0, fixed=T) + kstar(3:6) + cycle(4:6),
             estimate="CMLE",control = c.s, times = 2008:2016)
write.summary(f4); plot(gof(f4))

## CUREVED EXPONENTIAL FAMILY fixed=FALSE  breaks the stergm 
## [error msg: "if(any(bad.stat))" evals to TRUE]
f5 <- stergm(nd,
             formation = ~ edges + gwesp(0, fixed=T) + gwdegree(0, fixed=T) + kstar(3:6) + cycle(4:5),
             dissolution = ~ edges + gwesp(0, fixed=T) + gwdegree(0, fixed=T) + kstar(3:6) + cycle(4:5), 
             estimate="CMLE",control = c.s, times = 2008:2016)
write.summary(f5); plot(gof(f5))

# ## LOCAL TRIANGLE -- where COMMUNITY GIVEN BY NPM (COMMUNITY) PARTITIONING
# formation = ~ localtriangle(comMat)
g <- getIgraphFromNet(net)
mlcom <- igraph::multilevel.community(g)
dmat <- as.matrix( dist(mlcom$membership, method = 'manhattan', diag = T, upper = T) )
comMat <- dmat
comMat[dmat == 0] <- 1  ## same community -->  =  0 distance
comMat[dmat >  0] <- 0  ## diff community -->  >= 1 distance
f6 <- stergm(nd,
            formation = ~ edges 
             + gwesp(0, fixed=T) 
             # + gwdegree(0, fixed=T)
             + kstar(3:6) 
             # + cycle(4:5)
             + localtriangle(comMat)
            ,
            dissolution = ~ edges 
             + gwesp(0, fixed=T) 
             # + gwdegree(0, fixed=T) 
             + kstar(3:6) 
             # + cycle(4:5)
             + localtriangle(comMat)
            ,
            estimate="CMLE",control = c.s, times=2008:2016)
write.summary(f6);plot(gof(f6))


#-----------------------------------------------------------------------
texreg::screenreg(list(btergm=fb1, stergm=f2), stars=c(.001,.01,.05,.1), single.row = F)


## ------------------- NETWORKDYNAMIC PLOTS -----------------------------
data("short.stergm.sim")
ss <- short.stergm.sim
ndtv::timeline(ss)
ndtv::timePrism(ss, at=seq(1,10,by=3))
ndtv::filmstrip(ss, frames=9, mfrow=c(3,3))
ndtv::render.d3movie(ss, filename = )





# library("statnet")
# set.seed(5)
# 
# networks <- list()
# for(i in 1:10){            # create 10 random networks with 10 actors
#   mat <- matrix(rbinom(100, 1, .25), nrow = 10, ncol = 10)
#   diag(mat) <- 0           # loops are excluded
#   nw <- network(mat)       # create network object
#   networks[[i]] <- nw      # add network to the list
# }
# 
# covariates <- list()
# for (i in 1:10) {          # create 10 matrices as covariate
#   mat <- matrix(rnorm(100), nrow = 10, ncol = 10)
#   covariates[[i]] <- mat   # add matrix to the list
# }
# 
# fit <- btergm(networks ~ edges + istar(2) +
#                 edgecov(covariates), R = 100)
# 
# summary(fit)               # show estimation results
# 
# # The same example using MCMC MLE:
# 
# fit2 <- mtergm(networks ~ edges + istar(2) + 
#                  edgecov(covariates))
# 
# summary(fit2)
# 
# # For an example with real data, see help("knecht").
# 
# 
# # Examples for parallel processing:
# 
# # Some preliminaries: 
# # - "Forking" means running the code on multiple cores in the same 
# #   computer. It's fast but consumes a lot of memory because all 
# #   objects are copied for each node. It's also restricted to 
# #   cores within a physical computer, i.e. no distribution over a 
# #   network or cluster. Forking does not work on Windows systems.
# # - "MPI" is a protocol for distributing computations over many 
# #   cores, often across multiple physical computers/nodes. MPI 
# #   is fast and can distribute the work across hundreds of nodes 
# #   (but remember that R can handle a maximum of 128 connections, 
# #   which includes file access and parallel connections). However, 
# #   it requires that the Rmpi package is installed and that an MPI 
# #   server is running (e.g., OpenMPI).
# # - "PSOCK" is a TCP-based protocol. It can also distribute the 
# #   work to many cores across nodes (like MPI). The advantage of 
# #   PSOCK is that it can as well make use of multiple nodes within 
# #   the same node or desktop computer (as with forking) but without 
# #   consuming too much additional memory. However, the drawback is 
# #   that it is not as fast as MPI or forking.
# # The following code provides examples for these three scenarios.
# 
# # btergm works with clusters via the parallel package. That is, the 
# # user can create a cluster object (of type "PSOCK", "MPI", or 
# # "FORK") and supply it to the 'cl' argument of the 'btergm' 
# # function. If no cluster object is provided, btergm will try to 
# # create a temporary PSOCK cluster (if parallel = "snow") or it 
# # will use forking (if parallel = "multicore").
# 
# # To use a PSOCK cluster without providing an explicit cluster 
# # object:
# require("parallel")
# fit <- btergm(networks ~ edges + istar(2) + edgecov(covariates), 
#               R = 100, parallel = "snow", ncpus = 25)
# 
# # Equivalently, a PSOCK cluster can be provided as follows:
# require("parallel")
# cores <- 25
# cl <- makeCluster(cores, type = "PSOCK")
# fit <- btergm(networks ~ edges + istar(2) + edgecov(covariates), 
#               R = 100, parallel = "snow", ncpus = cores, cl = cl)
# stopCluster(cl)
# 
# # Forking (without supplying a cluster object) can be used as 
# # follows.
# require("parallel")
# cores <- 25
# fit <- btergm(networks ~ edges + istar(2) + edgecov(covariates), 
#               R = 100, parallel = "multicore", ncpus = cores)
# stopCluster(cl)
# 
# # Forking (by providing a cluster object) works as follows:
# require("parallel")
# cores <- 25
# cl <- makeCluster(cores, type = "FORK")
# fit <- btergm(networks ~ edges + istar(2) + edgecov(covariates), 
#               R = 100, parallel = "snow", ncpus = cores, cl = cl)
# stopCluster(cl)
# 
# # To use MPI, a cluster object MUST be created beforehand. In 
# # this example, a MOAB HPC server is used. It stores the number of 
# # available cores as a system option:
# require("parallel")
# cores <- as.numeric(Sys.getenv("MOAB_PROCCOUNT"))
# cl <- makeCluster(cores, type = "MPI")
# fit <- btergm(networks ~ edges + istar(2) + edgecov(covariates), 
#               R = 100, parallel = "snow", ncpus = cores, cl = cl)
# stopCluster(cl)
# 
# # In the following example, the Rmpi package is used to create a 
# # cluster. This may not work on all systems; consult your local 
# # support staff or the help files on your HPC server to find out how 
# # to create a cluster object on your system.
# 
# # snow/Rmpi start-up
# if (!is.loaded("mpi_initialize")) {
#   library("Rmpi")
# }
# library(snow);
# 
# mpirank <- mpi.comm.rank (0)
# if (mpirank == 0) {
#   invisible(makeMPIcluster())
# } else {
#   sink (file="/dev/null")
#   invisible(slaveLoop (makeMPImaster()))
#   mpi.finalize()
#   q()
# }
# # End snow/Rmpi start-up
# 
# cl <- getMPIcluster()
# 
# fit <- btergm(networks ~ edges + istar(2) + edgecov(covariates), 
#               R = 100, parallel = "snow", ncpus = 25, cl = cl)