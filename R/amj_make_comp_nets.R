##--------------------------------------------------------------
##
##  AMJ 2018 SPECIAL ISSUE 
##  CREATE FIRM COMPETITION NETWORK PERIODS AS LIST OBJECTS
##
##--------------------------------------------------------------

setwd("C:/Users/T430/Google Drive/PhD/Dissertation/competition networks/compnet2")
# .libPaths('C:/Users/T430/Documents/R/win-library/3.2')

source(file.path(getwd(),'R','amj_awareness_functions.R'))
source(file.path(getwd(),'R','amj_cb_data_prep.R'))
source(file.path(getwd(),'R','amj_cb_sic_codes.R'))

data_dir <- "C:/Users/T430/Google Drive/PhD/Dissertation/crunchbase/"
wd <- getwd()
full.graph.file <- 'g_full.graphml'

## make full graph if not working directory
if ( !(full.graph.file %in% dir()) ) {
  source(file.path(getwd(),'R','amj_make_full_graph.R')) 
} 


## load full global competition network
g.full <- read.graph(full.graph.file, format='graphml')


## set firms to create networks (focal firm or replication study focal firms)
firms.todo <- c('qualtrics') 

## -- settings --
d <- 3
yrpd <- 1
startYr <- 2007
endYr <- 2017
## --------------  

## run main network period creation loop
for (i in 1:length(firms.todo)) {

  name_i <- firms.todo[i]
  cat(sprintf('\n---------%s----------\n',name_i))
  periods <- seq(startYr,endYr,yrpd)
  company.name <- 'company_name_unique'
  verbose <- TRUE
  #
  g.base <- g.full
  g.d.sub <- igraph::make_ego_graph(graph = g.base, nodes = V(g.full)[V(g.full)$name==name_i], order = d, mode = 'all')[[1]]
  net.d.sub <- asIgraph(g.d.sub)
  net <- net.d.sub
  net %n% 'ego' <- name_i
  #----------------Network List-------------------
  nl <- list()
  for (t in 2:length(periods)) {
    cat(sprintf('\nmaking period %s-%s:\n', periods[t-1],periods[t]))
    ##
    # TODO : CHECK USING competitive NODE COLLAPSE in makepdnetwork algorithm
    #
    tmp.net <- aaf$makePdNetwork(net.d.sub, 
                                start=periods[t-1], end=periods[t])
    nl[[t]] <- aaf$setCovariates(tmp.net, periods[t-1], periods[t],
                                 netRiskCommunityAlgo='multilevel.community',
                                 downweight.env.risk=FALSE,
                                 acq=co_acq,br=co_br,rou=co_rou,ipo=co_ipo)
  }
  nl.bak <- nl
  nl <- nl[which(sapply(nl, length)>0)]
  names(nl) <- periods[2:length(periods)]
  ## ---------- add LAGS ----------------
  for (t in 2:length(nl)) { 
    nl[[t]] %n% 'DV_lag' <- nl[[t-1]][,]
  }
  
  ##--------------- GET TERGM NETS LIST -----------
  ## only nets with edges > 0
  nets.all <- nl[2:length(nl)]
  nets <- nets.all[ which(sapply(nets.all, getNetEcount) > 0) ]
  #-------------------------------------------------
  
  ## SAVE variable in image
  # firm.nl <- list()
  firm.nets[[net_group]][[name_i]] <- nets
  
  ## CAREFUL TO OVERWRITE 
  file.rds <- sprintf('netrisk_dynamic_firm_nets_1yr_v3_%s.rds',net_group)
  saveRDS()
  
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
