##--------------------------------------------------------------
##
##        Make Full Global Competition Netowrk
##
##--------------------------------------------------------------

setwd("C:/Users/T430/Google Drive/PhD/Dissertation/competition networks/compnet2")
# .libPaths('C:/Users/T430/Documents/R/win-library/3.2')

library(igraph)

data_dir <- "C:/Users/T430/Google Drive/PhD/Dissertation/crunchbase/"
img_dir  <- "C:/Users/T430/Google Drive/PhD/Dissertation/competition networks/envelopment/img"

source(file.path(getwd(),'R','amj_awareness_function.R'))
source(file.path(getwd(),'R','cb_data_prep.R'))
source(file.path(getwd(),'R','cb_sic_codes.R'))


cat('\nmaking full graph...\n')

max.year <- 2016

## delete edges at or later than this date (the year after max.year)
exclude.date <- sprintf('%d-01-01', max.year+1)

## make graph
g.full <- makeGraph(comp = co_comp, vertdf = co)

## cut out confirmed dates >= 2016
g.full <- igraph::induced.subgraph(g.full, vids=V(g.full)[which(V(g.full)$founded_year <= max.year
                                                                | is.na(V(g.full)$founded_year)
                                                                | V(g.full)$founded_year=='' ) ] )
g.full <- igraph::delete.edges(g.full, E(g.full)[which(E(g.full)$relation_created_at >= exclude.date)])

## SIMPLIFY
g.full <- igraph::simplify(g.full, remove.loops=T,remove.multiple=T,
                           edge.attr.comb = list(weight='sum',
                                                 relation_began_on='min',
                                                 relation_ended_on='min'))

## save graph file
igraph::write.graph(graph = g.full, file="g_full.graphml", format = 'graphml')

cat('done.\n')
