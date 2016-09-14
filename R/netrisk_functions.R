##########################################################################################
#
# NETWORK RISK FUNCTION
# 
# @author   Stephen Downing <sdowning.bm02g@nctu.edu.tw>
# @article  "Network Risk: Assessing the threat of envelopment"
#
##########################################################################################

##
# Adjacency matrix degree function
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
# Coleman Theil inequality index [0,1] as measure of Hierarchy ( Burt, 1992:70ff )
# equals 0 if all contact-specific constraints equal the avg.
#     -- that is, no contacts are more connected than others
# approaches 1.0 to the extent that all constraint is from one contact
##
burt4inequality <- function(g)
{
  cg <- constraint(g)
  r <- cg / mean(cg)
  numer <- sum( r * log(r) )
  denom <- vcount(g)*log(vcount(g))
  return(numer / denom)
}
##
#
##
burt4effectsize <- function(g)
{
  vcount(g)-2*ecount(g)/vcount(g)
}
##
#
##
burt4efficiency <- function(g)
{
  ecount(g) - burt4effectsize(g)
}

##
# Burt's 4 related social capital aspects of a network 
#   density, hierarchy, effectsize, efficiency
##
burt4summary <- function(g.list, plotting=TRUE)
{
  df <- data.frame(
    density=sapply(g.list, igraph::graph.density),
    hierarchy=sapply(g.list, burt4inequality),
    effectsize=sapply(g.list, burt4effectsize),
    efficiency=sapply(g.list, burt4efficiency)
  )
  if(plotting)
    pairsDetail(df)
  return(df)
}

##
#
##
getEgoGraph <- function(graph.list, name, order=3, safe=FALSE)
{
  if(!safe) 
      return(sapply(graph.list, function(g)igraph::make_ego_graph(g, k, V(g)[V(g)$name==name_i],mode = 'all'), simplify = T))    

  out.list <- sapply(graph.list, function(g)igraph::make_ego_graph(g, k, V(g)[V(g)$name==name_i],mode = 'all'), simplify = T)
  for (i in 1:length(out.list)) {
      g_i <- out.list[[i]]
      if (length(g_i)==0)
        out.list[[i]] <- NA
      else if( class(g_i)=='igraph' )
        out.list[[i]] <- g_i
      else if ( class(g_i[[1]])=='igraph'  )
        out.list[[i]] <- g_i[[1]]
      else if ( class(g_i[[1]][[1]])=='igraph'  )
        out.list[[i]] <- g_i[[1]][[1]]
      else
        out.list[[i]] <- NA
  }
  return(out.list)
}

# tmp.list <- sapply(tmp.list, function(x){
#   if(length(x)==0) 
#     return(NA)
#   return( ifelse(class(x)=='igraph',x,
#                  ifelse(class(x[[1]])=='igraph',x[[1]],
#                         ifelse(class(x[[1]][[1]])=='igraph',x[[1]][[1]],
#                                NA))) )
# }, simplify = T)

##
# Get Igraph from list of igraphs that contain empty|NA elements
#     controlls for sapply() returning a list of 1 igraph instead of the igraph itself
# @param list   x    A list of igraphs
# @param string FUN  The name of igrpah function to apply
##
safeIgraphApply <- function(x, FUN, args.list=list())
{
  g <- NA
  if(length(x)==0)
    return(NA)
  else if(class(x)=='igraph')
    g <- x
  else if(class(x[[1]])=='igraph')
    g <- x[[1]]
  else if(class(x[[1]][[1]])=='igraph')
    g <- x[[1]][[1]]
  else
    return(NA)
  ##
  if(any( !is.na(g) )) {
    args.list <- c(args.list, list(graph=g))
    return(do.call(what = FUN, args = args.list))    
  }
  else
    return(NA)
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
#  WARNING:  MUST USE matrix.type='edgelist'; !!adjacency & incidence not working
##
getNetFromIgraph <- function(ig, add.vertex.name=FALSE, matrix.type='edgelist', 
                             vertex.pid.string='vertex.names')
{
  ##  INPUTS CHECK
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
  
  ## remove null edge attrs first
  tmp <- names(edge.attributes(ig))
  edgeAttrs <- tmp[which( !(tmp%in%'weight') )]
  for (attr in edgeAttrs) {
    if (all(is.null(igraph::get.edge.attribute(ig,attr))) ) {
      ig <- remove.edge.attribute(ig, attr)      
    } else if (all(is.na(igraph::get.edge.attribute(ig,attr))) ) {
      ig <- remove.edge.attribute(ig, attr)      
    }
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
plotCompNet <- function(gs, membership=NA, focal.firm=NA, focal.color=TRUE, multi.prod=NA, vertex.log.base=exp(1),label.log.base=10,margins=NA, ...) 
{
  if(all(is.na(margins)))
      margins <- c(.1,.1,.1,.1)
  ##
  par(mar=margins)
  d <- igraph::degree(gs)
  vertshape <- rep('circle',vcount(gs))
  vertcol <-  rgb(.3,.3,.6,.4)
  ##
  if (!all(is.na(membership)))
    vertcol <- rainbow(length(unique(membership)), alpha=.7)[ membership ]
  ##
  if (!all(is.na(multi.prod)))
    vertcol <- ifelse(V(gs)$name %in% multi.prod, rgb(.8,.2,.2,.8), vertcol)
  ##
  if(!all(is.na(focal.firm))) {
    if(focal.color) 
        vertcol <- ifelse(V(gs)$name %in% focal.firm, rgb(.15,.15,.7,.8), vertcol )
    vertshape <- ifelse(V(gs)$name %in% focal.firm, 'square', 'circle' )
  }
  ##
  set.seed(1111)
  plot.igraph(gs
              , layout=layout.kamada.kawai
              , vertex.size=log(d,base=vertex.log.base)*2 + 2
              , vertex.color=vertcol
              , vertex.label.cex=log(d,base=label.log.base)/2 + .01
              , vertex.label.color='black'
              , vertex.label.font = 2
              , vertex.label.family = 'sans'
              , vertex.shape = vertshape
              , ...
  )
  par(mar=c(4.5,4.5,3.5,1))
}


##
# Plot Competition Network ONE COLOR
##
plotCompNetOneColor <- function(gs, vertex.color=NA, focal.firm=NA, vertex.log.base=exp(1),label.log.base=10,margins=NA, ...) 
{
  if(all(is.na(margins)))
    margins <- c(.1,.1,.1,.1)
  ##
  par(mar=margins)
  d <- igraph::degree(gs)
  vertshape <- rep('circle',vcount(gs))
  vertcol <-  rgb(.3,.3,.6,.4)
  if(!all(is.na(vertex.color)))
    vertcol <- vertex.color

  if(!all(is.na(focal.firm))) {
    if(focal.color) 
    vertshape <- ifelse(V(gs)$name %in% focal.firm, 'square', 'circle' )
  }
  ##
  set.seed(1111)
  plot.igraph(gs
              , layout=layout.kamada.kawai
              , vertex.size=log(d,base=vertex.log.base)*2 + 2
              , vertex.color=vertcol
              , vertex.label.cex=log(d,base=label.log.base)/2 + .01
              , vertex.label.color='black'
              , vertex.label.font = 2
              , vertex.label.family = 'sans'
              , vertex.shape = vertshape
              , ...
  )
  par(mar=c(4.5,4.5,3.5,1))
}


##
#
##
plotRingWithIsolates <- function(gs, vert.size=20, label.cex=1.6, edge.width=2, mar=c(.1,.1,.1,.1), ...) 
{
  par(mar=mar)
  d <- igraph::degree(gs)
  set.seed(1111)
  plot.igraph(gs
              , layout=layout.circle
              , vertex.size=vert.size
              , vertex.color='gray'
              , vertex.label.cex=label.cex
              , vertex.label.color='black'
              , vertex.label.font = 2
              , vertex.label.family = 'sans'
              , edge.width = edge.width
              , ...
  )
  par(mar=c(4.5,4.5,3.5,1))
}

##
plotPretty <- function(gs, vert.size=20, label.cex=1.6, edge.width=1.5, mar=c(.1,.1,.1,.1),...) 
{
  par(mar=mar)
  d <- igraph::degree(gs)
  set.seed(1111)
  plot.igraph(gs
              , layout=layout.kamada.kawai
              , vertex.size=vert.size
              , vertex.color='gray'
              , vertex.label.cex=label.cex
              , vertex.label.color='black'
              , vertex.label.font = 2
              , vertex.label.family = 'sans'
              , edge.width = edge.width
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
    tmp <- igraph::get.vertex.attribute(g,vertFoundedAttr) 
    vids <- V(g)[which(tmp > end)]
    inactiveVerts <- c(inactiveVerts, vids)
  }
  ##  REMOVE VERTICES closed_on < `start`
  if(vertClosedAttr %in% vertAttrs) {
    tmp <- igraph::get.vertex.attribute(g,vertClosedAttr) 
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
    cat('optional: aggregating funding rounds...\n')
    x <- ply.vert.attr(rou, 'funded_year', 'ply.rou', start, end)
    check <- nrow(x) > 0 & all(c('funding_total_usd') %in% names(x))
    if(check) {
        V(g)$funding_total_usd <- ifelse(check, ifelse( is.na(x$funding_total_usd), 0, x$funding_total_usd), 0)
    } else {
        V(g)$funding_total_usd <- 0
    }
    V(g)$log_funding_total_usd <- log(V(g)$funding_total_usd + 1)
    V(g)$has_fund <- ifelse(V(g)$funding_total_usd>0,1,0)
  }
  ## acquisitions
  if(any( !is.na(acq) )) {
    cat('optional: aggregating acquisitions...\n')
    x <-  ply.vert.attr(acq, 'acquired_year', 'ply.acq', start, end)
    check <- nrow(x) > 0 & all(c('acquisitions_count','acquisitions_concat') %in% names(x))
    if(check) {
        V(g)$acquisitions <- x$acquisitions_concat
        V(g)$acquisitions_count <- ifelse( is.na(x$acquisitions_count)|x$acquisitions_count=="", 0, x$acquisitions_count)
    } else {
        V(g)$acquisitions <- NA
        V(g)$acquisitions_count <- 0
    }
  }
  ## branches
  if(any( !is.na(br) )) {
    cat('optional: aggregating branches...\n')
    x <- ply.vert.attr(br, 'created_year', 'ply.br', start, end)
    check <- nrow(x) > 0 & all(c('branches_count','branches_concat') %in% names(x))
    if(check) {
        V(g)$branches <- x$branches_concat
        V(g)$branches_count <- ifelse(x$branches_count<1,1,0)      
    } else {
        V(g)$branches <- NA
        V(g)$branches_count <- 0 
    }
  }
  
  return(g)
} 
##----- dependent functions in makeIgraphPdSubgraphKeepNA() --------
ply.vert.attr <- function(df, filterPdVar, FUN, start, end)
{
  if( !('company_name_unique'%in% names(df)) )
    stop('df must have name column (`company_name_unique`)')
  if( !(filterPdVar %in% names(df)) )
    stop(sprintf('filterPdVar `%s` must be a column in `df`',filterPdVar))
  df.sub <- df[which( df[,filterPdVar] <= end), ]
  tmp.name <- data.frame(company_name_unique=V(g)$name, stringsAsFactors = F)
  tmp.ply <- do.call(what = FUN, args = list(df.sub=df.sub))
  tmp.merge <- merge(tmp.name, tmp.ply, by='company_name_unique', all.x=T, all.y=F)
  # for(column in 2:ncol(tmp.merge))
  #   tmp.merge[is.na(tmp.merge[,column]), column] <- 0
  return(tmp.merge)
}
ply.rou <- function(df.sub)  # 'funded_year'
{
  plyr::ddply(df.sub, .(company_name_unique), .progress='text', summarise,
              funding_total_usd=sum(raised_amount_usd, na.rm=TRUE))
}
ply.acq <- function(df.sub)  # 'acquired_year'
{
  plyr::ddply(df.sub, .(company_name_unique), .progress='text', summarise,
              acquisitions_count=length(company_name_unique),
              acquisitions_concat=paste(acquired_name_unique, collapse = "|") )
}
ply.br <- function(df.sub)   # 'created_year
{
  plyr::ddply(df.sub, .(company_name_unique), .progress='text', summarise,
              branches_count=length(company_name_unique),
              branches_concat=paste(market2, collapse = "|") )
}
#----// dependent functions -----------



#-------- Dependend Functions  Previous versions -----
# getRaisedAmts <- function(name_i)
# {
#   subset(rou, subset=(company_name_unique==name_i & funded_year <= end), select = 'raised_amount_usd')
# }
# getTotalRaised <- function(name_i) 
# {
#   amts <- getRaisedAmts(name_i)
#   if(nrow(amts)>0) {
#     return(sum(amts,na.rm = T))
#   }
#   return(0)
# }
# getAcqs <- function(name_i)
# {
#   subset(acq, subset=(company_name_unique==name_i & acquired_year <= end) )
# }
# getAcqsCount <- function(name_i)
# {
#   acqsSubset <- getAcqs(name_i)
#   return(nrow(acqsSubset))
# }
# getAcqsConcat <- function(name_i)
# {
#   acqsSubset <- getAcqs(name_i)
#   if(nrow(acqsSubset)>0) {
#     return( paste(acqsSubset$acquired_name_unique,collapse = "|"))
#   }
#   return("")
# }
# getBrs <- function(name_i)
# {
#   subset(br, subset=(company_name_unique==name_i & created_year <= end))
# }
# getBrsConcat <-function(name_i)
# {
#   brs <- getBrs(name_i)
#   brsMarket2 <- brs$market2
#   index <- which(brsMarket2=="" | is.na(brsMarket2))
#   brs[index] <- 'NA'
#   return(paste(brsMarket2, collapse="|"))
# }
# getBrsCount <- function(name_i)
# {
#   brs <- getBrs(name_i)
#   brsCount <- nrow(brs)
#   brsCount[brsCount < 1] <- 1
#   return(brsCount)
# }
#------------------------------------------------------


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
                            mode='in'
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
      D[k] <- sum(d)r
    }
  } #end vertex loop
  
  R <- sum(D) / vcount(g)
  cat(sprintf('nodes %d edges %d reach: %.3f\n',vcount(g),ecount(g),R))
  return(R)
}#end function



distWeightReachPerNode <- function(g, mode='all')
{
  D <- c()

  # for one node

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
    } ## end other vertex l loop
    D[k] <- sum(d)r

  R <- sum(D) / vcount(g)
  cat(sprintf('nodes %d edges %d reach: %.3f\n',vcount(g),ecount(g),R))
  return(R)
}

## UPPER ZEROS
distWeightReachByNode <- function(g, alpha=1)
{
  rmat <- dmat <- distances(g)
  dmat[lower.tri(dmat,diag = T)] <- 0
  dmat.vec <- dmat[upper.tri(dmat)]
  # rmat.vec <- sapply(dmat.vec, function(x)sum(1/1:x))
  rmat.vec <- sapply(dmat.vec, function(x){
    return(sum( sapply(seq_len(x),function(x_i)sum(1/(1:x_i)^alpha)) ))
  })
  rmat[upper.tri(rmat, diag = F)] <- rmat.vec
  r <- sapply(1:nrow(rmat), function(x)sum(rmat[x, ],rmat[ ,x]))
  names(r) <- row.names(rmat)  
  return(r)
}

#BOTH SIDES
distWeightReachByNode_other <- function(g)
{
  rmat <- dmat <- distances(g)
  dmat[lower.tri(dmat,diag = T)] <- 0
  dmat.vec <- dmat[upper.tri(dmat)]
  # rmat.vec <- sapply(dmat.vec, function(x)sum(1/1:x))
  rmat.vec <- sapply(dmat.vec, function(x)sum(1/(1:x)^3))
  rmat[upper.tri(rmat, diag = F)] <- rmat.vec
  r <- sapply(1:nrow(rmat), function(x)sum(rmat[x, ],rmat[ ,x]))
  names(r) <- row.names(rmat)  
  return(r)
}




##
#
##
df.cent.norm <- function(g.tmp)
{
  be <- igraph::betweenness(g.tmp,normalized = F)
  cl <- igraph::closeness(g.tmp, normalized = F)
  ei <- igraph::eigen_centrality(g.tmp)$vector
  po <- igraph::power_centrality(g.tmp,exp=2)
  al <- igraph::alpha_centrality(g.tmp,alpha = 2)
  re <- distWeightReachByNode(g.tmp, alpha = 1)
  df <- data.frame(
    between=100*be/sum(be),
    close=100*cl/sum(cl),
    eigen=100*ei/sum(ei),
    power=100*po/sum(po),
    alpha=100*al/sum(al),
    reach=100*re/sum(re)
  )
  return(df)
}

##
#
##
df.cent <- function(g.tmp)
{
  df <- data.frame(
    between= igraph::betweenness(g.tmp,normalized = T),
    close= igraph::closeness(g.tmp, normalized = T),
    eigen= igraph::eigen_centrality(g.tmp)$vector,
    negpower= -igraph::power_centrality(g.tmp,exp=2),
    negalpha= -igraph::alpha_centrality(g.tmp,alpha = 2),
    normreach= 10*distWeightReachByNode(g.tmp)/sum(distWeightReachByNode(g.tmp))
  )
  return(df)
}



