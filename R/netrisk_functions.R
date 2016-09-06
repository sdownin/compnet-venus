##########################################################################################
#
# NETWORK RISK FUNCTION
# 
# @author   Stephen Downing <sdowning.bm02g@nctu.edu.tw>
# @article  "Network Risk: Assessing the threat of envelopment"
#
##########################################################################################

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
##
getNetFromIgraph <- function(ig, add.vertex.name=FALSE)
{
  adjmat <- igraph::as_adjacency_matrix(ig, type='both',names=T, sparse=F)
  if (add.vertex.name) {
    vertAttrList <- igraph::vertex.attributes(ig)
  } else {
    vertAttrList <- igraph::vertex.attributes(ig)[which( names(igraph::vertex.attributes(ig))!='name' ) ]
  }
  net <- network::network(adjmat, vertex.attr = vertAttrList, 
                          directed=igraph::is.directed(ig),
                          loops = any(igraph::is.loop(ig)),
                          bipartite = igraph::is.bipartite(ig))
  
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
getMultiProdEgoNet <- function(g, firms, k=1) 
{
  ego.list <- igraph::ego(g, order=k, nodes=V(g)[V(g)$name %in% multi.prod], mode='all', mindist = 0)
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
  d <- degree(gs)
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
# Remove edges between companies that weren't created yet
# and after being closed/acquired
##
createPdNet <- function(g,start,end,pdAttr='founded_at',acquiredPdAttr='acquired_at',
                        edgeCreatedAttr='relation_created_at',
                        edgeClosedAttr='competitor_closed_on',
                        edgeAcquiredAttr='acquired_at')
{
  cat('collecting edges to filter...\n')
  vertexAttrs <- names(igraph::vertex.attributes(g))
  edgeAttrs <- names(igraph::edge.attributes(g))
  edges <- E(g)
  removeEdges <- c()
  ##------------------ COLLECT VERTICES ------------------ 
  ##  REMOVE EDGES ADJACENT TO VERTICES founded_at > `end`
  if(pdAttr %in% vertexAttrs) {
    tmp <- igraph::get.vertex.attribute(g,pdAttr) 
    vids <- V(g)[which(tmp > end)]
    eids <- E(g)[ from(vids) & to(vids) ]
    # for (v in vids) {
    #   removeEdges <- c(removeEdges, E(g)[ from(v) & to(v) ])
    # }
    removeEdges <- c(removeEdges, eids)
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
    removeEdges <- c(removeEdges, eids)
  }
  ##  REMOVE EDGES acquired_at < `start`
  if(edgeAcquiredAttr %in% vertexAttrs) {
    tmp <- igraph::get.vertex.attribute(g,edgeAcquiredAttr) 
    eids <- E(g)[which(tmp < start)]
    removeEdges <- c(removeEdges, eids)
  }
  ##-----------------REMOVE EDGES ---------------------------
  removeEdgesUnique <- unique(removeEdges)
  cat(sprintf('removing %d edges of %d (%.2f%s)\n', length(removeEdgesUnique), length(edges), 100*length(removeEdgesUnique)/length(edges), '%'))
  #g.sub <- igraph::delete_edges(graph=g,edges = removeEdgesUnique)
  ##------------------- networkDynamic ---------------------
  g.netdyn <- network.initialize(vcount(g),directed=FALSE)
  activate.vertices(g.netdyn, onset=0, terminus = 10)
  add.edges.active(g.netdyn,tail=1:2,head=2:3,onset=start,terminus=end)
  return(g.netdyn)
}


###
# Remove edges between companies that weren't created yet
# and after being closed/acquired
##
setEdgeActiveState <- function(g,start,end,pdAttr='founded_at',acquiredPdAttr='acquired_at',
                               edgeCreatedAttr='relation_created_at',
                               edgeClosedAttr='competitor_closed_on',
                               edgeAcquiredAttr='acquired_at')
{
  cat('collecting edges to filter...\n')
  E(g)$active <- TRUE
  vertexAttrs <- names(igraph::vertex.attributes(g))
  edgeAttrs <- names(igraph::edge.attributes(g))
  edges <- E(g)
  removeEdges <- c()
  ##------------------ COLLECT VERTICES ------------------ 
  ##  REMOVE EDGES ADJACENT TO VERTICES founded_at > `end`
  if(pdAttr %in% vertexAttrs) {
    tmp <- igraph::get.vertex.attribute(g,pdAttr) 
    vids <- V(g)[which(tmp > end)]
    eids <- E(g)[ from(vids) & to(vids) ]
    # for (v in vids) {
    #   removeEdges <- c(removeEdges, E(g)[ from(v) & to(v) ])
    # }
    removeEdges <- c(removeEdges, eids)
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
    removeEdges <- c(removeEdges, eids)
  }
  ##  REMOVE EDGES acquired_at < `start`
  if(edgeAcquiredAttr %in% vertexAttrs) {
    tmp <- igraph::get.vertex.attribute(g,edgeAcquiredAttr) 
    eids <- E(g)[which(tmp < start)]
    removeEdges <- c(removeEdges, eids)
  }
  ##-----------------REMOVE EDGES ---------------------------
  removeEdgesUnique <- unique(removeEdges)
  cat(sprintf('removing %d edges of %d (%.2f%s)\n', length(removeEdgesUnique), length(edges), 100*length(removeEdgesUnique)/length(edges), '%'))
  E(g)[which(E(g) %in% removeEdgesUnique)]$active <- FALSE
  return(g)
}


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
