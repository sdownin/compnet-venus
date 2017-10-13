#############################################################################################
#
#  Competition Networks and Acquisition Activity
#
#############################################################################################
setwd("C:/Users/sdowning/Google Drive/PhD/Dissertation/competition networks/compnet")

library(plyr)
library(reshape2)
library(lattice); library(latticeExtra)
library(ggplot2)
library(igraph)
library(stringr)

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
## Add group category
V(g.full)$category <- sapply(V(g.full)$category_group_list, function(x)str_split(x, '[|]')[[1]][1]
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
plot(yc$x, yc$freq, main=sprintf('n=%d',nrow(acq.r)), log='y')

if( !('l' %in% ls()) ) l <- list()   ## coariates list
start <- 1997 ## including
end <- 1999   ## exluding
pd <- as.character(end)
gx <- makePdSubgraph(g.full, start, end)
gx.sub <- igraph::induced.subgraph(gx, vids=V(gx)[which(igraph::degree(gx)>0)])

l[[pd]] <- list()
## Matrix covariates ----------------------------------------------------------

## Distances
l[[pd]]$dist_d <- igraph::distances(gx, 
                         v = V(g.full)[which(V(g.full)$name %in% acq.names)], 
                         to = V(g.full)[which(V(g.full)$name %in% acq.names)])

## MMC
gx.tmp <- induced.subgraph(gx, vids = V(gx)[which(V(gx)$name %in% acq.names)])
l[[pd]]$mmc_e <- getMultiMarketContact(br_co, V(gx.tmp)$name, end)

## Age diff
l[[pd]]$age <- (end-1) - V(gx)[which(V(gx)$name %in% acq.names)]$founded_year
l[[pd]]$age_d <- dist(l[[pd]]$age)

## Comp size (count) diff
l[[pd]]$degree <- igraph::degree(gx)
l[[pd]]$degree_d <- dist(l[[pd]]$degree)

## Constraint Diff
l[[pd]]$constraint <- igraph::constraint(gx)
l[[pd]]$constraint_d <- dist(l[[pd]]$constraint)

## Same community
mem <- igraph::multilevel.community(gx)$membership
idx <- which(V(gx)$name %in% acq.names)
mem <- mem[idx]
l[[pd]]$community <- as.factor(mem)
md <- dist(mem)
dvec <- as.vector(md)
dvec[dvec>0] <- 1  ## diff community == 1
l[[pd]]$community_d <- 1-dvec     ## same community == 1

## Same HQ region
l[[pd]]$region_d

## same category

## Centrality Diff
# between <- igraph::betweenness(gx)

## local density diff (?)

##------------------------ --- Firm-level covariates -------------------------------














