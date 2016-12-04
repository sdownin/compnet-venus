#---------------------------------------------------------------------
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
data_dir <- "C:/Users/sdowning/Google Drive/PhD/Dissertation/crunchbase/"
img_dir  <- "C:/Users/sdowning/Google Drive/PhD/Dissertation/competition networks/envelopment/img"
# if( !('net' %in% ls()) )
#   load('netrisk_dynamic_2.RData')
###
# save.image('netrisk_dynamic_2.RData')
###
source(file.path(getwd(),'R','comp_net_functions.R'))
source(file.path(getwd(),'R','netrisk_functions.R'))
source(file.path(getwd(),'R','cb_data_prep.R'))
par.default <- par()
lattice::trellis.par.set(strip.background=list(col="lightgrey"))
###########################################################################
## Firm type global variables
# multi.prod <- c("cisco","google","microsoft","ibm","yahoo","oracle","hewlett-packard","intel",
#                 "aol","apple","facebook","amazon","adobe-systems","nokia","dell","sap","motorola",
#                 "groupon","autodesk","salesforce","iac","quest-software","zayo-group","sas",
#                 "qualcomm","blackberry","berkshire-hathaway-corp","carlyle-group","intuit",
#                 "symantec","broadcom","kkr","medtronic","vmware","sony","lg","motorola-mobility",
#                 "motorola-solutions")
# 
# single.prod <- c('netflix','medallia','dropbox','surveymonkey')  #
###########################################################################

#---------------------------------------------------------------------
#   1.0 ORGANIZATIONS
##   1.1 ORGANIZATION PARENT
##      **RELATION**
##   1.2 BRANCHES

# net <- net1
# end <- 2017
# 
# system.time(
#   mmc <- getMultiMarketContact(br, net%v%'vertex.names', end)
# )
# mmc

##   2 ACQUISITIONS
##      **RELATION**
##      **AGGREGATE**

##   3. PRODUCTS
##      **AGGREGATE**

##   4 Category group -- ALL OK

##   5 COMPETITORS
##      **RELATION**

##   6 CUSTOMERS
##      **RELATION**

##   7.1 EVENTS
##   7.2 EVENTS RELATIONSHIPS
##      **RELATIONS**

##   8.1 FUNDS
##   8.2 FUNDING ROUNDS
##      **RELATION**
##      **AGGREGATE**
##   8.3 IPOs

##   9.1 INVESTORS
##   9.2 INVESTMENTS
##      **RELATION**
##   9.3 INVESTMENT PARTNERS
##      **RELATION**

##   10 JOBS

##   11.1 PEOPLE
##   11.2 PEOPLE  DESCRIPTIONS

#####################################################################################
## MAKE FULL COMP NET OF ALL RELATIONS IN DB 
#####################################################################################
# g.full <- makeGraph(comp = co_comp, vertdf = co)
# ## cut out confirmed dates >= 2016
# g.full <- igraph::induced.subgraph(g.full, vids=V(g.full)[which(V(g.full)$founded_year <= 2016 
#                                                                 | is.na(V(g.full)$founded_year)
#                                                                 | V(g.full)$founded_year=='' ) ] )
# g.full <- igraph::delete.edges(g.full, E(g.full)[which(E(g.full)$relation_created_at >= '2017-01-01')])
# ## SIMPLIFY
# g.full <- igraph::simplify(g.full, remove.loops=T,remove.multiple=T, 
#                            edge.attr.comb = list(weight='sum', 
#                                                  relation_began_on='min',
#                                                  relation_ended_on='min'))
#igraph::write.graph(graph = g.full, file="g_full.graphml", format = 'graphml')

g.full <- read.graph('g_full.graphml', format='graphml')

####################################################################
####################################################################
#-----------------------------------------------------------------
#
#                 Create Dynamic igraph form TERGM
#                   by ONLY REMOVING EDGES
#                   KEEP ALL N nodes
#                   compute PREDICTORS
#             
#                 MAIN LOOP
#
#infomap
#optimal
#walktrap
#spinglass
#leading.eigenvector
#multilevel
#fastgreedy
#label.propagation
#edge.betweenness
#----------------------------------------------------------------
# firms <- c('medallia','clarabridge','qualtrics','satmetrix','confirmit',
#            'empathica','allegiance','hybris','customergauge')
#-----------------------------------------------------------------
firms.todo <- c('qualtrics','satmetrix','confirmit',
                'empathica','allegiance','hybris','customergauge')

for (i in 1:length(firms.todo)) {

  name_i <- firms.todo[i]
  yrpd <- 2
  startYr <- 2005
  endYr <- 2017
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
  #----------------Network List-------------------
  nl <- list()
  for (t in 2:length(periods)) {
    cat(sprintf('\nmaking period %s-%s:\n', periods[t-1],periods[t]))
    tmp.net <- makePdNetwork(net.k.sub, 
                             start=periods[t-1], end=periods[t])
    nl[[t]] <- setCovariates(tmp.net, periods[t-1], periods[t],
                             netRiskCommunityAlgo='multilevel.community',
                             downweight.env.risk=FALSE,
                             acq=co_acq,br=co_br,rou=co_rou,ipo=co_ipo)
    # nl[[t]] <- makePdNetworkSetCovariates(net.k.sub, start=periods[t-1], end=periods[t],
    #                                       acq=co_acq,br=co_br,rou=co_rou,ipo=co_ipo,
    #                                       netRiskCommunityAlgo='multilevel.community')
  }
  nl.bak <- nl
  nl <- nl[which(sapply(nl, length)>0)]
  names(nl) <- periods[2:length(periods)]
  ## ---------- add LAGS ----------------
  for (t in 2:length(nl)) { 
    nl[[t]] %v% 'net_risk_lag' <- nl[[t-1]] %v% 'net_risk'
    nl[[t]] %n% 'dist_lag' <- as.matrix(nl[[t-1]] %n% 'dist')
    dl <- nl[[t]] %n% 'dist_lag'
    dl[dl == Inf] <- 999999 
    nl[[t]] %n% 'dist_lag' <- dl 
    # nl[[t]] <- network::set.network.attribute(nl[[t]], 'dist_lag', (nl[[t-1]] %n% 'dist') )
  }
  ##--------------- GET TERGM NETS LIST -----------
  ## only nets with edges > 0
  nets.all <- nl[2:length(nl)]
  nets <- nets.all[ which(sapply(nets.all, getNetEcount) > 0) ]
  #-------------------------------------------------
  
  ## SAVE variable in image
  # firm.nl <- list()
  firm.nets[[name_i]] <- nets
  
  save.image('netrisk_dynamic_firm_nets.RData')
    
}


# load('netrisk_dynamic_firm_nets.RData')

#------------------------------------------------------
############################################################################
#--------------------- BTERGM ------------------------------
############################################################################
nets.sub <- nets
mmc <- lapply(nets.sub,function(net) as.matrix(net %n% 'mmc'))


# nets <- list()
# pds <- 2007:2016
# for (i in 2:length(pds)) {
#   nets[[i]] <- network(network.extract(nd, onset=pds[i-1], terminus=pds[i])[,], directed = F, hyper = F, multiple = F, loops = F, bipartite = F)
# }; names(nets) <- pds; nets <- nets[-1]

fb0 <- btergm(nets.sub ~ edges + gwesp(0, fixed=T) + nodecov('age') + 
                nodefactor('state_code') + nodematch('state_code', diff=F) +  
                 nodecov('net_risk'),
              R = 5, parallel = "multicore", ncpus = detectCores())

fb1 <- btergm(nets.sub ~ edges + gwesp(0, fixed=T) + nodecov('age') + 
                nodematch('state_code', diff=F) +  nodefactor('state_code') + 
                nodecov('net_risk') + edgecov(mmc),
              R = 5, parallel = "multicore", ncpus = detectCores())

fb3 <- btergm(nets.sub ~ edges + gwesp(0, fixed=T)  + cycle(3:4) + 
                nodefactor('state_code') + nodematch('state_code', diff=F) +
                nodecov('age') +   edgecov(mmc)  +
                nodecov('net_risk') + nodecov('net_risk_lag') +
                nodematch('npm',diff=F)
               ,
              R = 20, parallel = "multicore", ncpus = detectCores())


fb4 <- btergm(nets.sub ~ edges + gwesp(0, fixed=T)  + cycle(3:4) + 
                nodefactor('state_code') + nodematch('state_code', diff=F) +
                nodecov('age') +   edgecov(mmc)  +
                nodecov('net_risk') + nodecov('net_risk_lag') +
                nodematch('npm',diff=F) + 
                nodematch('ipo_status', diff=TRUE) 
              ,
              R = 5, parallel = "multicore", ncpus = detectCores())

fb5med <- btergm(nets.sub ~ edges + gwesp(0, fixed=T)  + cycle(3:5) + 
                nodefactor('state_code') + nodematch('state_code', diff=F) +
                nodecov('age') +   edgecov(mmc)  +
                nodecov('net_risk') + nodecov('net_risk_lag') +
                nodematch('npm',diff=F) + 
                nodematch('ipo_status', diff=TRUE) 
              ,
              R = 100, parallel = "multicore", ncpus = detectCores())



#(l <- list(m1=fb2,m2=fb3,m3a=fb4f,m3b=fb4t,m4b=fb5) )
# (l <- list(m0=fb0,m2=fb3,m3b=fb4t,m4b=fb5) )

# (l <- list(fb6))
# texreg::screenreg(l, single.row = T)
# write.regtable(filterModels(l), filename = "netflix_2yr", digits=3)

(l <- list(fb0,fb5))
texreg::screenreg(l, single.row = T)
write.regtable(filterModels(l), filename = "clarabridge_3k_2y_btergm", digits=3)

write.regtable(list(Clarabridge=fb5clar, Medallia=fb5med),
               filename = "clarabridge_medallia_3k_2y_btergm", digits=3)

# save.image('netrisk_dynamic_netflix_2y.RData')
 
#------------------------------------------------------------
#                MCMLE TERGM
##--------------------------------------------------------
# load('netrisk_dynamic_netflix_2y.RData')

nets.sub <- nets[3:4]

## Covariate Matrices
mmc <- lapply(nets.sub,function(net) as.matrix(net %n% 'mmc'))

#---------------------------------------------------
mt0 <- mtergm(nets ~ edges + gwesp(0, fixed=T) + cycle(3:4)  +
               nodecov('age') + 
                nodefactor('state_code') + nodematch('state_code', diff=F) +  
              nodecov('net_risk') # + nodecov('net_risk_lag') +
               # nodematch('npm', diff=F) + nodematch('ipo_status', diff=T)
               ,
             parallel = "multicore", ncpus = detectCores())


mt1 <- mtergm(nets ~ edges + gwesp(0, fixed=T) + 
                nodecov('age') + nodematch('state_code', diff=F) +  
                nodecov('net_risk'),
              parallel = "multicore", ncpus = detectCores())
# fb1 <- btergm(nets ~ edges + gwesp(0, fixed=F) + triangle + cycle(4:6) ,
#               R = 500, parallel = "multicore", ncpus = detectCores())
# fb2 <- btergm(nets ~ edges + gwesp(0, fixed=F) + triangle + cycle(4:6) + 
#                 nodecov('net_risk') , 
#               R = 500, parallel = "multicore", ncpus = detectCores())
mt3 <- mtergm(nets ~ edges + gwesp(0, fixed=T) + triangle + cycle(4:7) + 
                nodecov('age') + nodematch('state_code', diff=F) +  
                nodecov('net_risk') +  edgecov(mmc),
              parallel = "multicore", ncpus = detectCores())

mt4 <- mtergm(nets ~ edges + gwesp(0, fixed=T) + triangle + cycle(4:7) + 
                nodecov('age') + nodematch('state_code', diff=F) +  
                nodecov('net_risk') + nodecov('net_risk_lag') + edgecov(mmc),
              parallel = "multicore", ncpus = detectCores())
mt5<- mtergm(nets ~ edges + gwesp(0, fixed=T) + triangle + cycle(4:7) + 
                nodecov('age') + nodematch('state_code', diff=F) +  
                nodecov('net_risk') + nodecov('net_risk_lag') + 
                edgecov(mmc) + 
                nodematch('npm', diff=TRUE),
              parallel = "multicore", ncpus = detectCores())

mt6<- mtergm(nets ~ edges + gwesp(0, fixed=T) + triangle + cycle(4:7) + 
               nodecov('age') + nodematch('state_code', diff=F) +  
               nodecov('net_risk') + nodecov('net_risk_lag') + 
               edgecov(mmc) + nodematch('npm', diff=TRUE) +
               nodematch('ipo_status', diff=TRUE),
             parallel = "multicore", ncpus = detectCores())


(l <- list(mt1, mt3, mt4, mt5, mt6))
texreg::screenreg(l, single.row = T)
write.regtable(filterModels(l), filename = "netflix_k3_2yr_m", digits=3)

#_-------------------------------------------------------------
#                       Compare STERGM, BTERG, MTERGM
#--------------------------------------------------------------
bt1 <- btergm(nets ~ edges + gwesp(0, fixed=T) + nodecov('age') + nodematch('state_code', diff=F) +  nodecov('net_risk'), 
              R = 200, parallel = "multicore", ncpus = detectCores())
mt1 <- mtergm(nets ~ edges + gwesp(0, fixed=T) + nodecov('age') + nodematch('state_code', diff=F) + nodecov('net_risk'), 
              parallel = "multicore", ncpus = detectCores())

c.s <- control.stergm(seed = 1111, parallel = 4, parallel.version.check = T)
nd <- networkDynamic::networkDynamic(network.list=nets.sub)
st1 <- stergm(nets.sub,
             formation= ~ edges + gwesp(0, fixed=T) + nodecov('age') + nodematch('state_code', diff=F) +  nodecov('net_risk') ,
             dissolution= ~ edges + gwesp(0, fixed=T) +  nodecov('net_risk'),
             estimate="CMLE", control = c.s, times = 1:length(nets.sub))

l <- list(boot1=bt1,mle1=mt1)
texreg::screenreg(list(mt1), single.row = T, ci.force = F )
write.regtable(list(mt1), filename = "netflix_k3_2yr_m", digits=3)

# texreg::screenreg(list(bt=bt1,mt=mt1,st=st1), single.row = T, ci.force = F)


#-------------------------------------------------------------------#
#                        Curved Exponential Family
#_-------------------------------------------------------------------
ceb2 <- btergm(nets ~ edges 
              + gwesp(1, fixed=F) 
              + gwdsp(1, fixed=F) 
              + gwdegree(1, fixed=F) 
              + cycle(4:6), 
              R = 500, parallel = "multicore", ncpus = detectCores())
cem2 <- mtergm(nets ~ edges 
               + gwesp(1, fixed=F) 
               + gwdsp(1, fixed=F) 
               + gwdegree(1, fixed=F) 
               + cycle(4:6), 
               R = 500, parallel = "multicore", ncpus = detectCores())

screenreg(list(b=ce2,m=cem2))

############################################################################
#--------------------- STERGM ------------------------------
############################################################################
## example 
library(ergm) 
library(statnet) 
library(tergm) 
data(samplk) 
samp <- list() 
samp[[1]] <- samplk1 
samp[[2]] <- samplk2 
samp[[3]] <- samplk3 
samp[[1]] %v% "charm" <- runif(18) 
samp[[2]] %v% "charm" <- runif(18) 
samp[[3]] %v% "charm" <- runif(18) 
samp[[1]] %n% "closeness" <- matrix(runif(18*18),ncol=18,nrow=18) 
samp[[2]] %n% "closeness" <- matrix(runif(18*18),ncol=18,nrow=18) 
samp[[3]] %n% "closeness" <- matrix(runif(18*18),ncol=18,nrow=18) 
fit1 <- ergm(samp[[1]] ~ edges +nodecov("charm") +edgecov("closeness") ) 
summary(fit1)
samp.fit <- stergm(samp, 
                   formation = ~edges +nodecov("charm") +edgecov("closeness"), 
                   dissolution = ~edges +mutual, 
                   estimate = "CMLE", times = 1:3) 
summary(samp.fit)
##----------------------------------------------------------
c.s <- control.stergm(seed = 1111, parallel = 4, parallel.version.check = T)
##

nd <- networkDynamic::networkDynamic(network.list = nets,
                                    start=as.numeric(names(nets)[1])-1,
                                    end=as.numeric(names(nets)[length(nets)])-1 )

# nodematch('market2') + nodemix('status')
# baseline check
f0 <- stergm(nd, 
             formation= ~ edges ,
             dissolution= ~ edges ,
             estimate="CMLE", control = c.s, times = periods[-1])
write.summary(f0); plot(gof(f0))


network::get.edge.attribute(nd,'de_alio_entry.active')
network::get.edge.attribute(nd,'de_alio_entry.active')
pred[[1]]
f1 <- stergm(nd, 
              formation= ~ edges ,
              dissolution= ~ edges,
              estimate="CMLE", control = c.s, times = periods[-1])
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

nets.u <- unname(nets)
f6 <- stergm(nw = nets.u, 
            formation= ~ edges + triangle + cycle(4:5) + # gwesp(0, fixed=F) #  + 
              nodecov('env_risk') #+ 
              #nodematch('ipo_status') + 
              #edgecov('mmc') 
            ,
            dissolution= ~ edges ,
            estimate="CMLE", control = c.s, times = 1:length(nets.u))

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

