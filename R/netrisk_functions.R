##########################################################################################
#
# NETWORK RISK FUNCTION
# 
# @author   Stephen Downing <sdowning.bm02g@nctu.edu.tw>
# @article  "Network Risk: Assessing the threat of envelopment"
#
##########################################################################################

##
# handmade degree function
##
mat.degree<-function(x)
{
  return( as.vector( rowSums(as.matrix(x)) + colSums(as.matrix(x)) ) )
} 

##
# Convert dataframe to lowcase column-wise
##
df2lower <- function(df)
{
  for(col in names(df)) {
    df[,col] <- stringr::str_to_lower(df[,col])
  }
  return(df)
}

##
# Gets an Igraph object from a network object
# WARNING: DOES NOT HANDLE BIPARTITE OR LOOPY NETWORKS
##
getIgraphFromNet <- function(net)
{
  net.mat <- network::as.matrix.network.adjacency(net)
  mode <- ifelse(network::get.network.attribute(net, 'directed'), 'directed','undirected')
  diag <- network::get.network.attribute(net,'loops')
  ig <- igraph::graph_from_adjacency_matrix(adjmatrix = net.mat, mode = mode, diag = diag)
  
  ## add vertex attributes
  vertAttrs <- network::list.vertex.attributes(net)
  for (attr in vertAttrs) {
    values <- network::get.vertex.attribute(x=net, attrname = attr)
    ig <- igraph::set.vertex.attribute(graph=ig, name=attr, index=V(ig), value=values)
  }
  
  ## add edge attributes
  edgeAttrs <- network::list.edge.attributes(net)
  for (attr in edgeAttrs) {
    values <- network::get.edge.attribute(el=net$mel, attrname = attr)
    ig <- igraph::set.edge.attribute(graph=ig, name=attr, index=E(ig), value=values)
  }
  
  return(ig)
}

##
#  Gets a Network object from an igraph object
#  WARNING: DOES NOT HANDLE BIPARTITE OR LOOPY NETWORKS
#  WARNING:  MUST USE matrix.type='edgelist'; adjacency & incidence not working
##
getNetFromIgraph <- function(ig, add.vertex.name=FALSE, matrix.type='edgelist', 
                             vertex.pid.string='vertex.names')
{
  if (matrix.type=='adjacency') {
    mat <- igraph::as_adjacency_matrix(ig, type='both',names=T, sparse=F)
  } else if (matrix.type == 'edgelist') {
    mat <- igraph::as_edgelist(ig, names=T)
  } else if (matrix.type == 'incidence') {
    # mat <- igraph::as_incidence_matrix(ig, types='both', names=T, sparse=F)
    stop(sprintf('Conversion from incidence matrix currently broken.'))
  } else {
    stop(sprintf('Invalid matrix.type given: %s.\n',matrix.type))
  }
  
  if (add.vertex.name) {
    vertAttrList <- igraph::vertex.attributes(ig)
  } else {
    vertAttrList <- igraph::vertex.attributes(ig)[which( names(igraph::vertex.attributes(ig))!='name' ) ]
  }
  
  net <- network::network(mat, matrix.type=matrix.type,
                          vertex.attr = vertAttrList, 
                          directed=igraph::is.directed(ig),
                          loops = any(igraph::is.loop(ig)),
                          bipartite = igraph::is.bipartite(ig))
  
  if (vertex.pid.string %in% network::list.vertex.attributes(net))
      net <- set.network.attribute(net, 'vertex.pid', vertex.pid.string)

  ##---------------------------
  if (ecount(ig) != length(net$mel))
    stop(sprintf('igraph edges (%d) not equal to network edges (%d)', ecount(ig), length(net$mel)))
  
  ## add vertex attributes
  vertAttrs <- names(igraph::vertex.attributes(ig))
  verts <- V(ig) %>% as.vector()
  for (attr in vertAttrs) {
    ## handle vertex name attribute (to add or not to add to network object)
    if (attr!='name' | add.vertex.name) {
      values <- igraph::get.vertex.attribute(graph=ig, name=attr, index=verts)
      net <- network::set.vertex.attribute(x=net,attrname=attr, value=values)
    }
  }
  
  ## add edge attributes
  edgeAttrs <- names(igraph::edge.attributes(ig))
  edges <- E(ig) %>% as.vector()
  for (attr in edgeAttrs) {
    values <- igraph::get.edge.attribute(graph=ig, name=attr, index=edges)
    net <- network::set.edge.attribute(x=net, attrname=attr, value=values)
  }
  return(net)
}

##
# Get Multi-Product firm combined ego network
##
getMultiProdEgoNet <- function(g, firms, k=1, include.competitor.egonet=TRUE) 
{
  ## direct competitors
  v.subset <- V(g)[V(g)$name %in% firms]
  ## k-th order ego net of each company in set of {multi-product firms, direct competitors}
  if (include.competitor.egonet)
      v.subset <- unique(c(v.subset, igraph::neighbors(g, v = v.subset, mode ='all')))
  ego.list <- igraph::ego(g, order=k, nodes=v.subset, mode='all', mindist = 0)
  nodes <- c(unlist(ego.list))
  output <- list(names=unique(names(nodes)), vids=unique(nodes))
  return(output)
}

##
# Plot Competition Network coloring the Multi-Product firms in red
##
plotCompNet <- function(gs,multi.prod=NA, vertex.log.base=exp(1),label.log.base=10, ...) 
{
  par(mar=c(.1,.5,.1,.5))
  d <- igraph::degree(gs)
  if (!all(is.na(multi.prod)))
    vertcol <- ifelse(V(gs)$name %in% multi.prod, rgb(.8,.2,.2,.8), rgb(.5,.5,.7,.8))
  else
    vertcol <- rgb(.5,.5,.7,.8)
  set.seed(1111)
  plot.igraph(gs
              , layout=layout.kamada.kawai
              , vertex.size=log(d,base=vertex.log.base)*2 + 2
              , vertex.color=vertcol
              , vertex.label.cex=log(d,base=label.log.base)/2 + .01
              , vertex.label.color='black'
              , vertex.label.font = 2
              , vertex.label.family = 'sans'
              , ...
  )
  par(mar=c(4.5,4.5,3.5,1))
}

##
# Plot an igraph object with base network object style
##
plotIgraphInNetworkStyle <- function(g, ...)
{
  plot.igraph(g
              , vertex.color='red'
              , vertex.label.color='black'
              , vertex.size=5,edge.color='black'
              , edge.arrow.size=.35
              , ...
  )
}

##
#
##
getNetSizeChange <- function(l, showPlot=TRUE)
{
  g.max <- l[[ which.max(sapply(l,ecount)) ]]
  net.size.full <- data.frame(vertices=vcount(g.max),edges=ecount(g.max))
  #
  tmp <- sapply(X = l, FUN = function(x)c(vcount(x),ecount(x)))
  net.size <- data.frame(t(tmp))
  names(net.size) <- c('vertices','edges')
  net.size.pct <- data.frame(vertices=net.size$vertices/net.size.full$vertices, edges=net.size$edges/net.size.full$edges)
  net.size.diff <- data.frame(vertices=diff(net.size$vertices), edges=diff(net.size$edges))
  net.size.diff <- rbind(data.frame(vertices=NA,edges=NA), net.size.diff)
  if(showPlot) {
    matplot(as.numeric(names(l)),cbind(net.size$edges,net.size.diff$edges),pch=16:17, type='o', xlab='Period',ylab='Competitive Relations')
    legend(x='topleft',legend=c('Total','New'),lty=1:2,col=1:2,pch=16:17)
  }
  return(list(size=net.size, diff=net.size.diff, pct=net.size.pct))
}


###
# Sets active edges for the period in a network object 
# using attributes from an igraph object
##
setPdActivity <- function(net, g, start, end, 
                           pdAttr='founded_at',acquiredPdAttr='acquired_at',
                           edgeCreatedAttr='relation_created_at',
                           edgeClosedAttr='competitor_closed_on',
                           edgeAcquiredAttr='acquired_at')
{
  cat('collecting edges to filter...\n')
  vertexAttrs <- names(igraph::vertex.attributes(g))
  edgeAttrs <- names(igraph::edge.attributes(g))
  inactiveEdges <- c()
  inactiveVerts <- c()
  ##------------------ COLLECT VERTICES ------------------ 
  ##  REMOVE EDGES ADJACENT TO VERTICES founded_at > `end`
  if(pdAttr %in% vertexAttrs) {
    tmp <- igraph::get.vertex.attribute(g,pdAttr) 
    vids <- V(g)[which(tmp > end)]
    eids <- E(g)[ from(vids) & to(vids) ]
    inactiveVerts <- c(inactiveVerts, vids)
    inactiveEdges <- c(inactiveEdges, eids)
  }
  ##------------------ COLLECT EDGES ---------------------
  # ##  REMOVE EDGES with relation_created_at > `end`
  # if(edgeCreatedAttr %in% edgeAttrs) {
  #   tmp <- igraph::get.edge.attribute(g,edgeCreatedAttr) 
  #   eids <- E(g)[which(tmp > end)]
  #   removeEdges <- c(removeEdges, eids)
  # }
  ##  REMOVE EDGES competitor_closed_on < `start`
  if(edgeClosedAttr %in% edgeAttrs) {
    tmp <- igraph::get.edge.attribute(g,edgeClosedAttr) 
    eids <- E(g)[which(tmp < start)]
    el <- igraph::get.edges(g, eids)
    vids <- unique(c(el))
    inactiveVerts <- c(inactiveVerts, vids)
    inactiveEdges <- c(inactiveEdges, eids)
  }
  ##  REMOVE EDGES acquired_at < `start`
  if(edgeAcquiredAttr %in% vertexAttrs) {
    tmp <- igraph::get.vertex.attribute(g,edgeAcquiredAttr) 
    eids <- E(g)[which(tmp < start)]
    el <- igraph::get.edges(g, eids)
    vids <- unique(c(el))
    inactiveVerts <- c(inactiveVerts, vids)
    inactiveEdges <- c(inactiveEdges, eids)
  }
  ##------------------UNIQUES--------------------------------
  inactiveVerts <- unique(inactiveVerts)
  inactiveEdges <- unique(inactiveEdges)
  ## ---------------GET ACTIVE FROM INACTIVE ----------------
  activeVerts <- unique(  V(g)[ !(V(g)%in%inactiveVerts) ]  )
  activeEdges <- unique(  E(g)[ !(E(g)%in%inactiveEdges) ]  )
  ##-----------------REMOVE EDGES ---------------------------
  #cat(sprintf('removing %d edges of %d (%.2f%s)\n', length(removeEdgesUnique), length(edges), 100*length(removeEdgesUnique)/length(edges), '%'))
  #g.sub <- igraph::delete_edges(graph=g,edges = removeEdgesUnique)
  ##------------------- networkDynamic ---------------------
  ## deactivate inactive edges
  # net <- networkDynamic::deactivate.edges(x = net, onset = start, terminus = end, e = as.vector(inactiveEdges) )
  # net <- networkDynamic::deactivate.vertices(x = net, onset = start, terminus = end, v = as.vector(inactiveVerts) )
  
  ## set active edges
  net <- networkDynamic::activate.edges(x = net, onset = start, terminus = end, e = as.vector(activeEdges) )
  # net <- networkDynamic::activate.vertices(x = net, onset = start, terminus = end, v = as.vector(activeVerts) )

  ## REturn dynamic network
  return(net)
}


###
# Sets active edges for the period in a network object 
# using attributes from an igraph object
##
makeIgraphPdSubgraphKeepNA <- function(g, start, end, 
                          vertFoundedAttr='founded_at',
                          vertClosedAttr='company_closed_on',
                          vertAcquiredAttr='acquired_at',
                          edgeCreatedAttr='relation_created_at',
                          acq=NA,rou=NA,br=NA)
{
  cat('collecting vertices to remove...\n')
  vertAttrs <- names(igraph::vertex.attributes(g))
  edgeAttrs <- names(igraph::edge.attributes(g))
  inactiveEdges <- c()
  inactiveVerts <- c()
  ##------------------ COLLECT VERTICES TO REMOVE ------- 
  ##  REMOVE VERTICES founded_on > `end`
  if(vertFoundedAttr %in% vertAttrs) {
    tmp <- igraph::get.edge.attribute(g,vertFoundedAttr) 
    vids <- V(g)[which(tmp > end)]
    inactiveVerts <- c(inactiveVerts, vids)
  }
  ##  REMOVE VERTICES closed_on < `start`
  if(vertClosedAttr %in% vertAttrs) {
    tmp <- igraph::get.edge.attribute(g,vertClosedAttr) 
    vids <- V(g)[which(tmp < start)]
    inactiveVerts <- c(inactiveVerts, vids)
  }
  ##  REMOVE VERTICES acquired_at < `start`
  if(vertAcquiredAttr %in% vertAttrs) {
    tmp <- igraph::get.vertex.attribute(g,vertAcquiredAttr) 
    vids <- V(g)[which(tmp < start)]
    inactiveVerts <- c(inactiveVerts, vids)
  }
  ## ---------------GET UNIQUE ACTIVE VERTICES ----------------
  activeVerts <- unique(  V(g)[ !(V(g)%in%inactiveVerts) ]  )
  ##-----------------MAKE SUBGRAPH ---------------------------
  cat('inducing subgraph...\n')
  g <- igraph::induced.subgraph(g, vids = activeVerts)
  ##----------------------DYNAMIC ATTRS-----------------------
  ## AGE
  agediff <- end - V(g)$founded_year
  V(g)$age <- ifelse( agediff >= 0, agediff, NA)
  ## funding rounds
  if(any( !is.na(rou) )) {
    cat('optional: adding funding rounds...\n')
    V(g)$funding_total_usd <- sapply(V(g)$name,function(x)getTotalRaised(x))
    V(g)$log_funding_total_usd <- log(V(g)$funding_total_usd + 1)
    V(g)$has_fund <- ifelse(V(g)$funding_total_usd>0,1,0)
  }
  ## acquisitions
  if(any( !is.na(acq) )) {
    cat('optional: adding acquisitions...\n')
    V(g)$acquisitions <- sapply(V(g)$name,function(x)getAcqsConcat(x))
    V(g)$acquisitions_count <- sapply(V(g)$name,function(x)getAcqsCount(x) )   
  }
  ## branches
  if(any( !is.na(br) )) {
    cat('optional: adding branches...\n')
    V(g)$branches <- sapply(V(g)$name,function(x)getBrsConcat(x))
    V(g)$branches_count <- sapply(V(g)$name,function(x)getBrsCount(x))
  }
  return(g)
} 

#-------- Dependend Functions -----
getRaisedAmts <- function(name_i)
{
  subset(rou, subset=(company_name_unique==name_i & funded_year <= end), select = 'raised_amount_usd')
}
getTotalRaised <- function(name_i) 
{
  amts <- getRaisedAmts(name_i)
  if(nrow(amts)>0) {
    return(sum(amts,na.rm = T))
  }
  return(0)
}

getAcqs <- function(name_i)
{
  subset(acq, subset=(company_name_unique==name_i & acquired_year <= end) )
}
getAcqsCount <- function(name_i)
{
  acqsSubset <- getAcqs(name_i)
  return(nrow(acqsSubset))
}
getAcqsConcat <- function(name_i)
{
  acqsSubset <- getAcqs(name_i)
  if(nrow(acqsSubset)>0) {
    return( paste(acqsSubset$acquired_name_unique,collapse = "|"))
  }
  return("")
}

getBrs <- function(name_i)
{
  subset(br, subset=(company_name_unique==name_i & created_year <= end))
}
getBrsConcat <-function(name_i)
{
  brs <- getBrs(name_i)
  brsMarket2 <- brs$market2
  index <- which(brsMarket2=="" | is.na(brsMarket2))
  brs[index] <- 'NA'
  return(paste(brsMarket2, collapse="|"))
}
getBrsCount <- function(name_i)
{
  brs <- getBrs(name_i)
  brsCount <- nrow(brs)
  brsCount[brsCount < 1] <- 1
  return(brsCount)
}
#----------------------------------


# ###
# # Sets active edges for the period in a network object 
# # using attributes from an igraph object
# ##
# makeIgraphPdSubgraphDropNA <- function(g, start, end, 
#                                        vertFoundedAttr='founded_at',
#                                        vertClosedAttr='company_closed_on',
#                                        vertAcquiredAttr='acquired_at',
#                                        edgeCreatedAttr='relation_created_at',
#                                        rou=NA,br=NA)
# {
#   vertAttrs <- names(igraph::vertex.attributes(g))
#   edgeAttrs <- names(igraph::edge.attributes(g))
#   activeEdges <- c()
#   activeVerts <- c()
#   ##------------------ COLLECT VERTICES TO KEEP ------- 
#   ##  KEEP VERTICES founded_on < `end`
#   if(vertFoundedAttr %in% vertAttrs) {
#     tmp <- igraph::get.edge.attribute(g,vertFoundedAttr) 
#     vids <- V(g)[which(tmp < end)]
#     activeVerts <- c(activeVerts, vids)
#   }
#   ##  KEEP VERTICES closed_on > `start`
#   if(vertClosedAttr %in% vertAttrs) {
#     tmp <- igraph::get.edge.attribute(g,vertClosedAttr) 
#     vids <- V(g)[which(tmp > start)]
#     activeVerts <- c(activeVerts, vids)
#   }
#   ##  KEEP VERTICES acquired_at > `start`
#   if(vertAcquiredAttr %in% vertAttrs) {
#     tmp <- igraph::get.vertex.attribute(g,vertAcquiredAttr) 
#     vids <- V(g)[which(tmp > start)]
#     activeVerts <- c(activeVerts, vids)
#   }
#   ## ---------------GET UNIQUE ACTIVE VERTICES ----------------
#   activeVerts <- unique(  activeVerts  )
#   ##-----------------MAKE SUBGRAPH ---------------------------
#   g <- igraph::induced.subgraph(g, vids = activeVerts)
#   ##----------------------DYNAMIC ATTRS-----------------------
#   ## AGE
#   agediff <- end - V(g)$founded_year
#   V(g)$age <- ifelse( agediff >= 0, agediff, NA)
#   
#   return(g)
# }


# ###
# # Sets active edges for the period in a network object 
# # using attributes from an igraph object
# ##
# setPdActiveEdges <- function(net, g, start, end, 
#                              pdAttr='founded_at',acquiredPdAttr='acquired_at',
#                              edgeCreatedAttr='relation_created_at',
#                              edgeClosedAttr='competitor_closed_on',
#                              edgeAcquiredAttr='acquired_at')
# {
#   cat('collecting edges to filter...\n')
#   vertexAttrs <- names(igraph::vertex.attributes(g))
#   edgeAttrs <- names(igraph::edge.attributes(g))
#   inactiveEdges <- c()
#   inactiveVerts <- c()
#   ##------------------ COLLECT VERTICES ------------------ 
#   ##  REMOVE EDGES ADJACENT TO VERTICES founded_at > `end`
#   if(pdAttr %in% vertexAttrs) {
#     tmp <- igraph::get.vertex.attribute(g,pdAttr) 
#     vids <- V(g)[which(tmp > end)]
#     eids <- E(g)[ from(vids) & to(vids) ]
#     inactiveVerts <- c(inactiveVerts, vids)
#     inactiveEdges <- c(inactiveEdges, eids)
#   }
#   ##------------------ COLLECT EDGES ---------------------
#   # ##  REMOVE EDGES with relation_created_at > `end`
#   # if(edgeCreatedAttr %in% edgeAttrs) {
#   #   tmp <- igraph::get.edge.attribute(g,edgeCreatedAttr) 
#   #   eids <- E(g)[which(tmp > end)]
#   #   removeEdges <- c(removeEdges, eids)
#   # }
#   ##  REMOVE EDGES competitor_closed_on < `start`
#   if(edgeClosedAttr %in% edgeAttrs) {
#     tmp <- igraph::get.edge.attribute(g,edgeClosedAttr) 
#     eids <- E(g)[which(tmp < start)]
#     el <- igraph::get.edges(g, eids)
#     vids <- unique(c(el))
#     inactiveVerts <- c(inactiveVerts, vids)
#     inactiveEdges <- c(inactiveEdges, eids)
#   }
#   ##  REMOVE EDGES acquired_at < `start`
#   if(edgeAcquiredAttr %in% vertexAttrs) {
#     tmp <- igraph::get.vertex.attribute(g,edgeAcquiredAttr) 
#     eids <- E(g)[which(tmp < start)]
#     el <- igraph::get.edges(g, eids)
#     vids <- unique(c(el))
#     inactiveVerts <- c(inactiveVerts, vids)
#     inactiveEdges <- c(inactiveEdges, eids)
#   }
#   ##------------------UNIQUES--------------------------------
#   inactiveVerts <- unique(inactiveVerts)
#   inactiveEdges <- unique(inactiveEdges)
#   ## ---------------GET ACTIVE FROM INACTIVE ----------------
#   activeVerts <- V(g)[ !(V(g)%in%inactiveVerts) ]
#   activeEdges <- E(g)[ !(E(g)%in%inactiveEdges) ]
#   ##-----------------REMOVE EDGES ---------------------------
#   #cat(sprintf('removing %d edges of %d (%.2f%s)\n', length(removeEdgesUnique), length(edges), 100*length(removeEdgesUnique)/length(edges), '%'))
#   #g.sub <- igraph::delete_edges(graph=g,edges = removeEdgesUnique)
#   ##------------------- networkDynamic ---------------------
#   # net <- getNetFromIgraph(g, matrix.type = 'edgelist')  ## change to passing in `net` as variable
#   ## initiate all active edges and verts
#   # net <- networkDynamic::activate.edges(x = net, onset = start, terminus = end, e = seq_len(length(net$mel)) )
#   # net <- networkDynamic::activate.vertices(x = net,onset = start,terminus = end, v = seq_len(length(net$gal$n)) )
#   
#   ## set inactive edges
#   net <- networkDynamic::activate.edges(x = net, onset = start, terminus = end, e = as.vector(activeEdges) )
#   ## deactivate verts causes time range -Inf to Inf ??????????????
#   
#   # net <- networkDynamic::deactivate.vertices(x = net, onset = start, terminus = end, v = inactiveVerts)
#   
#   ## REturn dynamic network
#   return(net)
# }


###
# Remove edges between companies that weren't created yet
# and after being closed/acquired
##
getPdActiveEdges <- function(net, g, start, end, 
                             pdAttr='founded_at',acquiredPdAttr='acquired_at',
                             edgeCreatedAttr='relation_created_at',
                             edgeClosedAttr='competitor_closed_on',
                             edgeAcquiredAttr='acquired_at')
{
  cat('collecting edges to filter...\n')
  vertexAttrs <- names(igraph::vertex.attributes(g))
  edgeAttrs <- names(igraph::edge.attributes(g))
  edges <- E(g)
  inactiveEdges <- c()
  verts <- V(g)
  inactiveVerts <- c()
  ##------------------ COLLECT VERTICES ------------------ 
  ##  REMOVE EDGES ADJACENT TO VERTICES founded_at > `end`
  if(pdAttr %in% vertexAttrs) {
    tmp <- igraph::get.vertex.attribute(g,pdAttr) 
    vids <- V(g)[which(tmp > end)]
    eids <- E(g)[ from(vids) & to(vids) ]
    inactiveVerts <- c(inactiveVerts, vids)
    inactiveEdges <- c(inactiveEdges, eids)
  }
  ##------------------ COLLECT EDGES ---------------------
  # ##  REMOVE EDGES with relation_created_at > `end`
  # if(edgeCreatedAttr %in% edgeAttrs) {
  #   tmp <- igraph::get.edge.attribute(g,edgeCreatedAttr) 
  #   eids <- E(g)[which(tmp > end)]
  #   removeEdges <- c(removeEdges, eids)
  # }
  ##  REMOVE EDGES competitor_closed_on < `start`
  if(edgeClosedAttr %in% edgeAttrs) {
    tmp <- igraph::get.edge.attribute(g,edgeClosedAttr) 
    eids <- E(g)[which(tmp < start)]
    el <- igraph::get.edges(g, eids)
    vids <- unique(c(el))
    inactiveVerts <- c(inactiveVerts, vids)
    inactiveEdges <- c(inactiveEdges, eids)
  }
  ##  REMOVE EDGES acquired_at < `start`
  if(edgeAcquiredAttr %in% vertexAttrs) {
    tmp <- igraph::get.vertex.attribute(g,edgeAcquiredAttr) 
    eids <- E(g)[which(tmp < start)]
    el <- igraph::get.edges(g, eids)
    vids <- unique(c(el))
    inactiveVerts <- c(inactiveVerts, vids)
    inactiveEdges <- c(inactiveEdges, eids)
  }
  ##------------------UNIQUES--------------------------------
  inactiveVerts <- unique(inactiveVerts)
  inactiveEdges <- unique(inactiveEdges)
  ##-----------------REMOVE EDGES ---------------------------
  #cat(sprintf('removing %d edges of %d (%.2f%s)\n', length(removeEdgesUnique), length(edges), 100*length(removeEdgesUnique)/length(edges), '%'))
  #g.sub <- igraph::delete_edges(graph=g,edges = removeEdgesUnique)
  ##------------------- networkDynamic ---------------------
  # net <- getNetFromIgraph(g, matrix.type = 'edgelist')  ## change to passing in `net` as variable
  ## initiate all active edges and verts
  # net <- networkDynamic::activate.edges(x = net, onset = start, terminus = end, e = seq_len(length(net$mel)) )
  # net <- networkDynamic::activate.vertices(x = net,onset = start,terminus = end, v = seq_len(length(net$gal$n)) )

  ## set inactive edges
  net <- networkDynamic::deactivate.edges(x = net, onset = start, terminus = end, e = inactiveEdges)
  ## deactivate verts causes time range -Inf to Inf ??????????????
  
  # net <- networkDynamic::deactivate.vertices(x = net, onset = start, terminus = end, v = inactiveVerts)

  ## REturn dynamic network
  return(net)
}

#
#  continue here...
#
# > activate.edges(triangle,at=1) # turn on all edges at time 1 only
# > activate.edges(triangle,onset=2, terminus=3, e=get.edgeIDs(triangle,v=1,alter=2))
# > add.edges.active(triangle,onset=4, length=2,tail=3,head=1)
#
## GET network object FROM igraph object

## active all edges not in the removeEdges set
# g.netdyn <- network.initialize(vcount(g),directed=FALSE)
# activate.vertices(g.netdyn, onset=0, terminus = 10)
# add.edges.active(g.netdyn,tail=1:2,head=2:3,onset=start,terminus=end)

## Activete vertices
#> activate.vertices(triangle,onset=1,terminus=5,v=1)
#> activate.vertices(triangle,onset=1,terminus=10,v=2)
#> activate.vertices(triangle,onset=4,terminus=10,v=3)




# ###
# # Remove edges between companies that weren't created yet
# # and after being closed/acquired
# ##
# setEdgeActiveState <- function(g,start,end,pdAttr='founded_at',acquiredPdAttr='acquired_at',
#                                edgeCreatedAttr='relation_created_at',
#                                edgeClosedAttr='competitor_closed_on',
#                                edgeAcquiredAttr='acquired_at')
# {
#   cat('collecting edges to filter...\n')
#   E(g)$active <- TRUE
#   vertexAttrs <- names(igraph::vertex.attributes(g))
#   edgeAttrs <- names(igraph::edge.attributes(g))
#   edges <- E(g)
#   removeEdges <- c()
#   ##------------------ from VERTEX properties ------------ 
#   ##  REMOVE EDGES ADJACENT TO VERTICES founded_at > `end`
#   if(pdAttr %in% vertexAttrs) {
#     tmp <- igraph::get.vertex.attribute(g,pdAttr) 
#     vids <- V(g)[which(tmp > end)]
#     eids <- E(g)[ from(vids) & to(vids) ]
#     # for (v in vids) {
#     #   removeEdges <- c(removeEdges, E(g)[ from(v) & to(v) ])
#     # }
#     removeEdges <- c(removeEdges, eids)
#   }
#   ##------------------ from EDGES properties----------------
#   # ##  REMOVE EDGES with relation_created_at > `end`
#   # if(edgeCreatedAttr %in% edgeAttrs) {
#   #   tmp <- igraph::get.edge.attribute(g,edgeCreatedAttr) 
#   #   eids <- E(g)[which(tmp > end)]
#   #   removeEdges <- c(removeEdges, eids)
#   # }
#   ##  REMOVE EDGES competitor_closed_on < `start`
#   if(edgeClosedAttr %in% edgeAttrs) {
#     tmp <- igraph::get.edge.attribute(g,edgeClosedAttr) 
#     eids <- E(g)[which(tmp < start)]
#     removeEdges <- c(removeEdges, eids)
#   }
#   ##  REMOVE EDGES acquired_at < `start`
#   if(edgeAcquiredAttr %in% vertexAttrs) {
#     tmp <- igraph::get.vertex.attribute(g,edgeAcquiredAttr) 
#     eids <- E(g)[which(tmp < start)]
#     removeEdges <- c(removeEdges, eids)
#   }
#   ##-----------------REMOVE EDGES ---------------------------
#   removeEdgesUnique <- unique(removeEdges)
#   cat(sprintf('removing %d edges of %d (%.2f%s)\n', length(removeEdgesUnique), length(edges), 100*length(removeEdgesUnique)/length(edges), '%'))
#   E(g)[which(E(g) %in% removeEdgesUnique)]$active <- FALSE
#   return(g)
# }


#--------------------------------------------------------------------------
## Distance weighted reach function
distWeightReach <- function(g,
                            mode='in',
                            weights=NA
) {
  D <- c()
  #for each vertex
  for (k in 1:vcount(g)) {
    if (degree(g, v=k)>0) {
      #find vertex subcomponent
      vec <- subcomponent(graph = g,v = V(g)[k],mode = mode)
      vec <- vec[order(vec)]
      
      d <- numeric(length(vec)-1)
      #for each other vertex l in subcomponent of vertex k
      for (l in 1:(length(vec))) {
        # excluding when k=l (which would make -Inf length)
        if (k!=vec[l]) {
          # vertex path of geodesic from k to l
          dp <-  unlist(get.shortest.paths(graph = g, from = k, to = vec[l],
                                           mode=mode, weights=weights,
                                           output="vpath")$vpath) #directed graph
          # inverse of length of geodesic from k to l
          #  -1 to subtract the origin vertex
          d[l] <- 1 / ( length(dp)-1 )
        }
      }
      D[k] <- sum(d)
    }
  } #end vertex loop
  
  R <- sum(D) / vcount(g)
  return(R)
}#end function
