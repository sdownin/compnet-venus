##--------------------------------------------------------------
##
##  AMJ 2018 SPECIAL ISSUE 
##  CREATE FIRM COMPETITION NETWORK PERIODS AS LIST OBJECTS
##
##--------------------------------------------------------------
# .libPaths('C:/Users/T430/Documents/R/win-library/3.2')
library(igraph)
library(intergraph)

## DIRECTORIES
cb$data_dir <- "C:/Users/T430/Google Drive/PhD/Dissertation/crunchbase/crunchbase_export_20161024"
cb$work_dir <- "C:/Users/T430/Google Drive/PhD/Dissertation/competition networks/compnet2"
cb$img_dir  <- "C:/Users/T430/Google Drive/PhD/Dissertation/competition networks/envelopment/img"

## set woring dir
setwd(cb$work_dir)

source(file.path(getwd(),'R','amj_awareness_functions.R'))
source(file.path(getwd(),'R','amj_cb_data_prep.R'))
source(file.path(getwd(),'R','amj_cb_sic_codes.R'))
source(file.path(getwd(),'R','amj_sdc_coop.R'))

## CACHE ENVIRONMENT to keep when clearing tmp objects added here
.ls <- ls()

## graph filename
full.graph.file <- 'g_full.graphml'

## make full graph if  not exists in working directory
if ( !(full.graph.file %in% dir()) ) {
  source(file.path(getwd(),'R','amj_make_full_graph.R')) 
} else {
  g.full <- read.graph(full.graph.file, format='graphml')
}


## add full network vertex IDs for acquirer|acquiree
## to identify unique  vertex over time after node collapsing
if (!('acquirer_vid' %in% names(cb$co_acq))) {
  tmp <- data.frame(acquirer_vid=as.integer(V(g.full)), acquirer_uuid=V(g.full)$company_uuid)
  cb$co_acq <- merge(cb$co_acq, tmp, by='acquirer_uuid')
}
if (!('acquiree_vid' %in% names(cb$co_acq))) {
  tmp <- data.frame(acquiree_vid=as.integer(V(g.full)), acquiree_uuid=V(g.full)$company_uuid)
  cb$co_acq <- merge(cb$co_acq, tmp, by='acquiree_uuid')
}


## set firms to create networks (focal firm or replication study focal firms)
firms.todo <- c('qualtrics') 

## -- settings --
d <- 3
yrpd <- 1
startYr <- 2006
endYr <- 2017  ## dropping first for memory term; actual dates 2007-2016
## --------------  

## run main network period creation loop
for (i in 1:length(firms.todo)) {

  name_i <- firms.todo[i]
  cat(sprintf('\n---------%s----------\n',name_i))
  periods <- seq(startYr,endYr,yrpd)
  company.name <- 'company_name_unique'
  g.base <- g.full  
  
  ## focal firm ego network sample
  g.d.sub <- igraph::make_ego_graph(graph = g.base, nodes = V(g.full)[V(g.full)$name==name_i], order = d, mode = 'all')[[1]]
  net.d.sub <- asNetwork(g.d.sub)
  net <- net.d.sub
  net %n% 'ego' <- name_i
  
  ##-------process pre-start-year acquisitions----------
  acqs.pd <- cb$co_acq[cb$co_acq$acquired_on <= sprintf('%d-12-31',startYr-1), ]
  g.d.sub <- aaf$nodeCollapseGraph(g.d.sub, acqs.pd, verbose = T)
  net.d.sub <- asNetwork
  
  ##------------Network Time Period List--------------------
  nl <- list()
  for (t in 2:length(periods)) {
    cat(sprintf('\nmaking period %s-%s:\n', periods[t-1],periods[t]))
    t1 <- sprintf('%d-01-01',periods[t-1])
    t2 <- sprintf('%d-12-31',periods[t-1])
    ## 1. Node Collapse acquisitions within period
    acqs.pd <- cb$co_acq[cb$co_acq$acquired_on >= t1 & cb$co_acq$acquired_on <= t2, ]
    g.d.sub <- aaf$nodeCollapseGraph(g.d.sub, acqs.pd, verbose = T)
    net.d.sub <- asNetwork(g.d.sub)
    ## 2. Subset Period Network
    net.d.sub <- aaf$makePdNetwork(net.d.sub, periods[t-1], periods[t], isolates.remove = F) 
    ## 3. Set Covariates for updated Period Network
    nl[[t]] <- aaf$setCovariates(net.d.sub, periods[t-1], periods[t],
                                 acq=cb$co_acq,br=cb$co_br,rou=cb$co_rou,ipo=cb$co_ipo,
                                 coop=coop)
  }
  
  ## ----drop null and skipped periods----
  nl.bak <- nl
  nl <- nl[which(sapply(nl, length)>0)]
  
  if (length(nl) > 1) {
    names(nl) <- periods[2:length(periods)]
  }
  
  ## ---------- add LAGS ----------------
  if (length(nl) > 1) {
    for (t in 2:length(nl)) { 
      nl[[t]] %n% 'DV_lag' <- nl[[t-1]][,]
    }
  }
  
  ##--------------- GET TERGM NETS LIST -----------
  ## only nets with edges > 0
  if (length(nl) > 1) {
    nets.all <- nl[2:length(nl)]
  } else {
    nets.all <- nl
  }
  nets <- nets.all[ which(sapply(nets.all, aaf$getNetEcount) > 0) ]
  #-------------------------------------------------
  
  ## CAREFUL TO OVERWRITE 
  file.rds <- sprintf('firm_nets_rnr/%s_d%d.rds',name_i,d)
  saveRDS(nets, file = file.rds)
  
}


# load('netrisk_dynamic_firm_nets.RData')




# # ADD CONSTRAINT NODE PROPERTY
# for (t in 1:length(nets)) {
#   g.tmp <- getIgraphFromNet(nets[[t]])
#   if (vcount(g.tmp)>0 & ecount(g.tmp)>0) {
#     cons <-  igraph::constraint(g.tmp)
#     cons[is.nan(cons) | is.na(cons)] <- 0 ### ???
#     nets[[t]] %v% 'constraint' <- cons
#   }
# }

#------------------------------------------------------
#              Predictors Diagnostics
# #------------------------------------------------------
# ## Plot density
# n <- ceiling(sqrt(length(firm.nets)))
# m <- ifelse(n*(n-1) >= length(firm.nets), n-1, n)
# par(mfrow=c(m,n), mar=c(2.5,2.5,2,1))
# for (firm_i in names(firm.nets)) {
#   nets <- firm.nets[[firm_i]]
#   plot(as.numeric(names(nets))-1,   
#        sapply(nets,function(net) {
#          sum(net[lower.tri(net)])/(nrow(net[,])*nrow(net[,]-1)/2) 
#        }), 
#        ylab='density', xlab='year', type='b', main=firm_i)
# }
# 
# ## Net Risk
# par(mfrow=c(3,3), mar=c(2.5,2.5,2,1))
# for (firm_i in names(firm.nets)) {
#   nets <- firm.nets[[firm_i]]
#   sapply(seq_along(nets), function(j) {
#     hist(nets[[j]] %v% 'net_risk', breaks=25, main=sprintf('%s %s',firm_i,names(nets)[j]))
#   })
# }

#------------------------------------------------------
