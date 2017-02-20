#############################################################################################
#
#  Competition Networks and Acquisition Activity
#
#  Data frame preparation
#
#############################################################################################
setwd("C:/Users/sdowning/Google Drive/PhD/Dissertation/competition networks/compnet")

library(plyr)
library(reshape2)
library(lattice); library(latticeExtra)
library(ggplot2)
library(igraph)
library(stringr)
library(sna)
library(network)

source(file.path(getwd(),'R','comp_net_functions.R'))
source(file.path(getwd(),'R','cb_data_prep.R'))

data_dir <- "C:/Users/sdowning/Google Drive/PhD/Dissertation/crunchbase/"
img_dir  <- "C:/Users/sdowning/Google Drive/PhD/Dissertation/competition networks/acquisitions/img"
par.default <- par()
lattice::trellis.par.set(strip.background=list(col="lightgrey"))
#---------------------------------------------------------------------
# f6 <- count ~ age + m100_usd  + t10_employees + status + state_code + 
#   comp_count + density + overlap + 
#   full_net_constraint + full_net_log_betweenness + 
#   full_net_constraint:comp_count + full_net_constraint:density + full_net_constraint:overlap + 
#   full_net_log_betweenness:comp_count + full_net_log_betweenness:density + full_net_log_betweenness:overlap +
#   I(full_net_constraint^2) + I(full_net_log_betweenness^2)

#####################################################################################
## MAKE FULL COMP NET OF ALL RELATIONS IN DB 
#####################################################################################
# co_companies <- co[co$primary_role=='company', ]
# vertAttrs <- c('founded_on','founded_year','closed_on','closed_year',
#                 'category_list','category_group_list',
#                 'state_code','country_code','region','city')
# g.full <- makeGraph(comp = co_comp, vertdf = co_companies, vertAttrs = vertAttrs)
# g.full <- igraph::induced.subgraph(g.full, vids=V(g.full)[V(g.full)$founded_year > 1800
#                                                           | is.na(V(g.full)$founded_year)])
# g.full <- igraph::induced.subgraph(g.full, vids=V(g.full)[V(g.full)$founded_year <= 2016
#                                                           | is.na(V(g.full)$founded_year)])
# # ## remove wrongly dated edges
# g.full <- igraph::delete.edges(g.full, E(g.full)[which(E(g.full)$relation_created_at >= '2017-01-01')])
# # ## SIMPLIFY
# g.full <- igraph::simplify(g.full, remove.loops=T,remove.multiple=T,
#                            edge.attr.comb = list(weight='sum',
#                                                  relation_began_on='min',
#                                                  relation_ended_on='min'))
# ## SAVE GRAPH
# igraph::write.graph(graph = g.full, file="g_full_compacq.graphml", format = 'graphml')

## LOAD GRAPH
g.full <- read.graph('g_full_compacq.graphml', format='graphml')


#----------------------------------------------------------------------------------
# Regression Dataframe
# static:  HQ region, category
# dynamic: age, status:IPO/private, status:operating/closed/acquired, # branches
#          # funding rounds, $ funding, 
#          acq experience, MMC, 
#----------------------------------------------------------------------------------
baseCols <- c('company_name_unique','country_code','state_code','region','city', 'category_group_list')
baseRows <- which(co$company_name_unique %in% V(g.full)$name)
rdf <- co[ baseRows , baseCols]

rdf$category <- sapply(rdf$category, function(x)str_split(x, '[|]')[[1]][1])



###--------------------------------------------------------------------------------
# Compute Dynamic covariates
#----------------------------------------------------------------------------------







#----------------------------------------------------------------------------------
#  Test distance proxy competition --> acquisitions
#----------------------------------------------------------------------------------
name_i <- 'medallia'
k <- 4
gsub <- igraph::make_ego_graph(g.full, order=k, nodes=V(g.full)[V(g.full)$name==name_i])[[1]]

d = igraph::distances(gsub, 1:2)








##----------------------------------------------------------------------------------
# Filter Acquisitions Dataframe / Companies
#-----------------------------------------------------------------------------------
# acquisitions where target is reachable over network by acquirer
acq.r <- co_acq[which(co_acq$acquiree_name_unique %in% V(g.full)$name
                     & co_acq$acquirer_name_unique %in% V(g.full)$name), ]
## limit recent 20 years
acq.r <- acq.r[which(acq.r$acquired_year >= 1997), ]
acq.names <- unique(c(acq.r$acquirer_name_unique, acq.r$acquiree_name_unique))

## filter to acquirers|acquiree with known founded_on years
names.f <- co$company_name_unique[which(!is.na(co$founded_year))]
acq.r <- acq.r[which(acq.r$acquiree_name_unique %in% names.f
                     & acq.r$acquirer_name_unique %in% names.f ), ]
acq.names <- unique(c(acq.r$acquirer_name_unique, acq.r$acquiree_name_unique))

#-------------------- Explore -----------------------------------------------
## Acquisition counts by year
yc <- plyr::count(acq.r$acquired_year)
yc <- yc[order(yc$freq, decreasing = T),]
plot(yc$x, yc$freq, main=sprintf('n=%d',nrow(acq.r)), log='y',col='steelblue',pch=16)

periods <- c(2011,2013,2015,2017)

if( !('l' %in% ls()) ) l <- list()   ## coariates list
for (t in 2:length(periods)) {
  start <- periods[t-1] ## including
  end <- periods[t]   ## exluding
  cat(sprintf('\n\n#------starting period %s-%s-------#\n',start,end-1))
  ## pd subgraph
  pd <- as.character(end)
  gx.sub <- makePdSubgraph(g.full, start, end)
  ## Period induced subgraph (degree >= 1)
  gx <- igraph::induced.subgraph(gx.sub, vids=V(gx.sub)[which(igraph::degree(gx.sub)>0)])
  ## init period covariates list
  l[[pd]] <- list()
  ## Names of companies with acquisitions (acquired|acquiree) this period
  acq.sub <- co_acq[which(co_acq$acquired_year >= start  &  co_acq$acquired_year < end), ]
  acq.sub.names <- unique(c(acq.sub$acquirer_name_unique, acq.sub$acquiree_name_unique))
  # Names and indices of companies that had an acquisiton and are in the period induced subgraph (degree >= 1)
  gx.names <- V(gx)$name[which(V(gx)$name %in% acq.sub.names)]
  # Dependent variable Acquisition network
  acq.sub.idx <- which(acq.sub$acquirer_name_unique %in% gx.names 
                       & acq.sub$acquiree_name_unique %in% gx.names)
  #vertices <- unique(c(acq.sub$acquirer_name_unique[acq.sub.idx], acq.sub$acquiree_name_unique[acq.sub.idx]))
  g.acq <- igraph::graph.data.frame(acq.sub[acq.sub.idx, c('acquirer_name_unique','acquiree_name_unique')],
                                    directed = TRUE,  
                                    vertices = gx.names )
  ## Dyadic data frame
  cat('creating period dyadic dataframe...\n')
  gx.names.df <- data.frame(i=seq_along(gx.names),name=gx.names)
  v <- length(gx.names)
  m <- v * (v-1) / 2
  l[[pd]]$df <- data.frame(e=seq_len(m), i=rep(NA,m), j=rep(NA,m), firm_i=rep(NA,m), firm_j=rep(NA,m), 
                           Y_ij=rep(NA,m), Y_ji=rep(NA,m))
  ## indices
  mat.j <- matrix(rep(1:v,v),nrow=v, byrow = T)  ## same Cols [[1,2,3], [1,2,3], [1,2,3]]
  mat.i <- matrix(rep(1:v,v),nrow=v, byrow = F)  ## same Rows [[1,1,1], [2,2,2], [3,3,3]]
  vec.j <- as.vector(mat.j[lower.tri(mat.j)])
  vec.i <- as.vector(mat.i[lower.tri(mat.i)])
  l[[pd]]$df$j <- vec.j
  l[[pd]]$df$i <- vec.i  
  ## firms
  mat.j <- matrix(rep(gx.names,v),nrow=v, byrow = T)  ## same Cols [[1,2,3], [1,2,3], [1,2,3]]
  mat.i <- matrix(rep(gx.names,v),nrow=v, byrow = F)  ## same Rows [[1,1,1], [2,2,2], [3,3,3]]
  vec.j <- as.vector(mat.j[lower.tri(mat.j)])
  vec.i <- as.vector(mat.i[lower.tri(mat.i)])
  l[[pd]]$df$firm_j <- vec.j
  l[[pd]]$df$firm_i <- vec.i  
  ## DEPENDENT VARIABLE as directed
  mat.acq <- as.matrix(as_adjacency_matrix(g.acq))
  l[[pd]]$df$Y_ij <- as.vector(mat.acq[lower.tri(mat.acq)])
  tmat.acq <- t(mat.acq)
  l[[pd]]$df$Y_ji <- as.vector(tmat.acq[lower.tri(tmat.acq)])
  ##  - - - - - - -    as undirected
  l[[pd]]$df$Y <-  l[[pd]]$df$Y_ij + l[[pd]]$df$Y_ji
  ## PERIOD
  l[[pd]]$df$start <- start
  l[[pd]]$df$end <- end
  l[[pd]]$df$period <- as.factor(paste(c(start,end-1),collapse='-'))

  ## Matrix covariates ----------------------------------------------------------
  cat('computing covariates...\n')
  ## Distances to/from companies in this period subgraph if they are in list of this period acquisitions
  gx.sub.idx <- which(V(gx.sub)$name %in% gx.names)
  dis <- igraph::distances(gx.sub, 
                                      v  = V(gx.sub)[gx.sub.idx], 
                                      to = V(gx.sub)[gx.sub.idx] )
  # dis[dis == Inf ] <- 100
  l[[pd]]$dist_d <- as.vector(dis[lower.tri(dis)])
  l[[pd]]$df$dist_d <- as.vector(dis[lower.tri(dis)])
  ## MMC
  gx.tmp <- induced.subgraph(gx, vids = V(gx)[which(V(gx)$name %in% gx.names)])
  mmc.mat <- getMultiMarketContact(co_br, V(gx.tmp)$name, end)
  l[[pd]]$mmc_d <- as.vector(mmc.mat[lower.tri(mmc.mat)])
  l[[pd]]$df$mmc_d <- as.vector(mmc.mat[lower.tri(mmc.mat)])
  
  ## Age diff
  l[[pd]]$age <- (end-1) - V(gx)[which(V(gx)$name %in% gx.names)]$founded_year
  l[[pd]]$age_d <- as.vector(dist(l[[pd]]$age))
  l[[pd]]$df$age_d <- as.vector(dist(l[[pd]]$age))
  
  ## Comp size (count) diff
  l[[pd]]$degree <- igraph::degree(gx, v = V(gx)[which(V(gx)$name %in% gx.names)])
  l[[pd]]$degree_d <- as.vector(dist(l[[pd]]$degree))
  l[[pd]]$df$degree_d <- as.vector(dist(l[[pd]]$degree))
  
  ## Constraint Diff
  l[[pd]]$constraint <- igraph::constraint(gx, nodes = V(gx)[which(V(gx)$name %in% gx.names)])
  l[[pd]]$constraint_d <- as.vector(dist(l[[pd]]$constraint))
  l[[pd]]$df$constraint_d <- as.vector(dist(l[[pd]]$constraint))
  
  ## Acquisition Experience
  acq.exp <- co_acq[which(co_acq$acquired_year < end), ]
  acq.cnt.i <- plyr::count(acq.exp$acquirer_name_unique)
  names(acq.cnt.i) <- c('company_name_unique','acq_exp_i')
  acq.cnt.j <- acq.cnt.i
  names(acq.cnt.j) <- c('company_name_unique','acq_exp_j')
  ## merge acqusition experience by row (firm_i) and column (firm_j)
  l[[pd]]$df <- merge(l[[pd]]$df, acq.cnt.i, by.x = 'firm_i', by.y = 'company_name_unique', all.x = T)
  l[[pd]]$df <- merge(l[[pd]]$df, acq.cnt.j, by.x = 'firm_j', by.y = 'company_name_unique', all.x = T)
  l[[pd]]$df$acq_exp_i[is.na(l[[pd]]$df$acq_exp_i)] <- 0
  l[[pd]]$df$acq_exp_j[is.na(l[[pd]]$df$acq_exp_j)] <- 0
  l[[pd]]$df$acq_exp <- l[[pd]]$df$acq_exp_i + l[[pd]]$df$acq_exp_j
  
  ## Centrality Diff
  # between <- igraph::betweenness(gx)
  
  ## local density diff (?)
  
  ## FACTORS / HOMOPHILY TERMS ---------------------------------------------- 
  cat('computing factor / homophily terms...\n')
  ## Same community
  mem <- igraph::multilevel.community(gx)$membership
  mem <- mem[ which(V(gx)$name %in% gx.names) ]
  l[[pd]]$community <- mem
  md <- dist(mem)
  dvec <- as.vector(md)
  dvec[dvec>0] <- 1  ## diff community == 1
  l[[pd]]$community_d <- 1-dvec     ## 1=same; 0=not same
  l[[pd]]$df$community_d <- 1-dvec     ## 1=same; 0=not same
  
  ## Same HQ region
  vert.sub <- V(gx)[which(V(gx)$name %in% gx.names)]
  l[[pd]]$region <- sapply(seq_along(vert.sub), function(x){
    ct <- V(gx)[x]$country_code
    st <- V(gx)[x]$state_code
    if(is.na(st) | st == 'NA')
      return(ct)
    else 
      return(paste(c(ct,st),collapse = "_"))
  })
  rd <- abs(diff(as.numeric(as.factor(l[[pd]]$region))))
  dvec <- as.vector(rd)
  dvec[dvec>0] <- 1 
  l[[pd]]$region_d <- 1-dvec ## 1=same; 0=not same
  l[[pd]]$df$region_d <- 1-dvec ## 1=same; 0=not same
  
  ## same category
  vert.sub <- V(gx)[which(V(gx)$name %in% gx.names)]
  l[[pd]]$category <- as.factor(sapply(seq_along(vert.sub), function(x) {
    split <- str_split(V(gx)[vert.sub]$category_group_list[x], '[|]')
    return(split[[1]][1]) 
  }))
  cd <- abs(dist(as.numeric(l[[pd]]$category)))
  dvec <- as.vector(cd)
  dvec[dvec>0] <- 1   ## x > 1 == not same
  l[[pd]]$category_d <- 1-dvec  ## 1=same; 0=not same
  l[[pd]]$df$category_d <- 1-dvec  ## 1=same; 0=not same
  
  ## IPO Status
  iposub <- co_ipo[which(co_ipo$company_name_unique %in% gx.names
                      & co_ipo$went_public_year < end), ]
  l[[pd]]$ipo <- as.factor(sapply(gx.names, function(x) {
    ifelse(x %in% iposub$company_name_unique, 'public', 'private')
  }))
  l[[pd]]$ipo_d <- 1 - abs(dist(as.numeric(l[[pd]]$ipo)))  ## 1=same; 0=not same
  l[[pd]]$df$ipo_d <- 1 - abs(dist(as.numeric(l[[pd]]$ipo)))  ## 1=same; 0=not same
  
  #------------------------- END Dyadic predictors ----------------------------------
}
  
save(l, file="acquisitions_l_13-15-17.RData")






