##########################################################################################
#
# NETWORK RISK FUNCTION
# 
# @author   Stephen Downing <sdowning.bm02g@nctu.edu.tw>
# @article  "Network Risk: Assessing the threat of envelopment"
#
##########################################################################################


##
#
##
filterModels <- function(l)
{
  return(l[ sapply(l, function(m) !any(is.na(m@coef))) ])
}

##
#
##
write.regtable <- function(l, screen=TRUE, html=FALSE, filename=NA, ...)
{
  if(is.na(filename))
    filename <- deparse(substitute(fit))
  isText <- grepl(pattern = '.txt',x = filename,ignore.case = T)
  if(!isText) {
    name <- stringr::str_split(filename, "[.]")[[1]][1]
    stamp <- gsub("\\D", "", Sys.time(), perl = T)
    filename <- sprintf('%s_%s.txt',name,stamp )
  }
  if(screen) texreg::screenreg(l, file = filename,  single.row = T, ci.force = T, ...)
  if(html) texreg::htmlreg(l, file = filename,  single.row = T, ci.force = T, ...)
}

##
# Summary(fit) output to text file
##
write.summary <- function(fit, filename=NA, append=FALSE, show=FALSE, split=TRUE)
{
  if(is.na(filename))
    filename <- deparse(substitute(fit))
  isText <- grepl(pattern = '.txt',x = filename,ignore.case = T)
  if(!isText) {
    name <- stringr::str_split(filename, "[.]")[[1]][1]
    stamp <- gsub("\\D", "", Sys.time(), perl = T)
    filename <- sprintf('%s_%s.txt',name,stamp )
  }
  con <- file(description = filename, open="w" )
  sink(con, append = append, split=split)
  print(summary(fit))
  sink()
  close(con)
  if(show)
    file.show(filename)
}


##
# Adjacency matrix degree function
##
mat.degree<-function(x)
{
  return( as.vector( rowSums(as.matrix(x)) + colSums(as.matrix(x)) ) )
} 

##
# get the coefficient by name from a model fit summary
##
getCoef <- function(fit, coef=NA)
{
  m <- summary(fit)
  if (is.na(coef)) {
    coef <- row.names(m$coefs)
  }
  cvec <- sapply(coef, function(coef_i){
    m$coefs$Estimate[row.names(m$coefs)==coef_i]
  })
  names(cvec) <- coef
  return(cvec)
}

##
#  remove ergm terms from coeff names
##
removeErgmTerms <- function(x, add.terms=NA)
{
  patterns <- c('nodecov','nodematch','absdiff','gwesp','fixed','edges','[.]','0','alpha')
  if(any( !is.na(add.terms)))
    patterns <- c(patterns, add.terms)
  sapply(patterns, function(pattern){
    x <<- str_replace_all(x, pattern, "")
  })
  return(x)
}

##
# Convenience function: Print out sumary of logit model results
##
explainLogit <- function(b)
{
  for (i in 1:length(b)){
    b_i <- b[i]
    cat(sprintf('%s:\n',names(b)[i]))
    cat(sprintf('            b:     log-odds: %.4f\n',    b_i))
    cat(sprintf('       exp(b):         odds: %.4f\n',    exp(b_i)))
    cat(sprintf('1/(1+exp(-b)):  probability: %.4f\n',    1/(1+exp(-b_i))))
    cat(sprintf('                    percent: %.2f%s\n\n',100/(1+exp(-b_i)),'%'))
  }
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
#  get ordinal number suffix
##
th <- function(x)
{
  if(! is.numeric(x) )
    return(x)
  if(x==1)
    return(paste0(x,'st'))
  if(x==2)
    return(paste0(x,'nd'))
  if(x==3)
    return(paste0(x,'rd'))
  if(x)
    return(paste0(x,'th'))
  return(x)
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
getEgoGraphList <- function(graph.list, name, order=3, safe=TRUE)
{
  if(!safe) 
      return(sapply(graph.list, function(g)igraph::make_ego_graph(g, order, V(g)[V(g)$name==name],mode = 'all'), simplify = T))    

  out.list <- sapply(graph.list, function(g)igraph::make_ego_graph(g, order, V(g)[V(g)$name==name],mode = 'all'), simplify = T)
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
  
  ## add network|graph attributes
  netAttrs <- network::list.network.attributes(net)
  for (attr in netAttrs) {
    values <- network::get.network.attribute(x = net, attrname = attr)
    ig <- igraph::set.graph.attribute(graph=ig, name=attr, value=values)
  }
  
  return(ig)
}

##
#  Gets a Network object from an igraph object
#  WARNING: DOES NOT HANDLE BIPARTITE OR LOOPY NETWORKS
#  WARNING:  MUST USE matrix.type='edgelist'; !!adjacency & incidence not working
##
getNetFromIgraph <- function(ig, add.vertex.name=FALSE, matrix.type='adjacency', 
                             vertex.pid.string='vertex.names')
{
  ##  INPUTS CHECK
  if (matrix.type=='adjacency') {
    mat <- igraph::as_adjacency_matrix(ig, type='both',names=T, sparse=F)
  } else if (matrix.type == 'edgelist') {
    stop(sprintf('conversion from edgelist misses isolate edges'))
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
  net %v% 'id' <- seq_len(net$gal$n)
  
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
  
  ## add network|graph attributes
  netAttrs <- names(igraph::graph.attributes(ig))
  for (attr in netAttrs) {
    values <- igraph::get.graph.attribute(graph=ig, name=attr)
    net <- network::set.network.attribute(x=net, attrname=attr, value=values)
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
# Plot Competition Network coloring the Multi-Product firms in red
##
plotCompNetRisk <- function(gs, membership=NA, focal.firm=NA, focal.color=TRUE, multi.prod=NA, margins=NA, ...) 
{
  if(all(is.na(margins)))
    margins <- c(.1,.1,.1,.1)
  ##
  par(mar=margins)
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
              , vertex.color=vertcol
              , vertex.label.color='black'
              , vertex.label.font = 2
              , vertex.label.family = 'sans'
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

##
# Filter a network object
##
filterNet <- function(filtered, attr='id')
{
  return(unname(unlist(sapply(filtered, function(x){
    x[attr]
  }, simplify=TRUE))))
}

##
# INITIALIZE NetworkDynamic object
##
initNetworkDynamic <- function(net, start, end, create.TEAs=FALSE) {
  net.obs.period <- list(observations=list( c(start,end)), 
                         mode="discrete", time.increment=1,time.unit="step")
  nd <- networkDynamic::networkDynamic(network.list = list(net), 
                                       net.obs.period = net.obs.period,
                                       vertex.pid = 'vertex.names', 
                                       create.TEAs = create.TEAs )
  nd <- networkDynamic::deactivate.edges(nd, onset=start, terminus = end, e = seq_len(length(net$mel)))
  nd <- networkDynamic::deactivate.vertices(nd, onset=start, terminus = end, v = seq_len(net$gal$n))
  return(nd)
}

#***************************************************
### ***********NETWORK PERIOD FUNCTION**************
# FOR USE IN TERMG (STERGM) where node set must remain constant
# here we only remove the edges going backward in time
# Sets active edges for the period in a network object 
# using attributes from an igraph object
##
updateNetworkDynamicPdActivateEdges <- function(net, # [[network]]
                                              nd=NA,  # [[networkDynamic]]
                                              start=NA, end=NA, 
                                              lagStart=NA, lagEnd=NA,
                                              vertActiveAttrs=c('category_list','city','country_code','state_code','region'),
                                              edgeActiveAttrs=c('relation_began_on','relation_ended_on','weight'),
                                              vertFoundedAttr='founded_year',
                                              vertClosedAttr='closed_year',
                                              vertAcquiredAttr='acquired_year',
                                              edgeCreatedAttr='relation_began_on',
                                              edgeRemovedAttr='relation_ended_on',
                                              dynamic.only=TRUE,
                                              deactivateVertices=FALSE,
                                              acq=NA,rou=NA,br=NA,ipo=NA)
{
  cat('collecting edges to remove...\n')
  vertAttrs <- network::list.vertex.attributes(net)
  edgeAttrs <- network::list.edge.attributes(net)
  inactiveEdges <- c(); inactiveVertsEdges <- c();
  inactiveVerts <- c()
  ##------------------ COLLECT EDGES TO REMOVE -----------
  ## REMOVE EDGES CREATED AT
  if (edgeCreatedAttr %in% edgeAttrs) {
    tmp <- network::get.edge.attribute(net, edgeCreatedAttr)
    eids <- which(tmp > end)
    inactiveEdges <- c(inactiveEdges, eids)
  }
  # ## ---------------GET UNIQUE ACTIVE EDGES ----------------
  activeEdges <- unique(  which(!(net %v% 'id' %in% inactiveEdges))  )
  nd <- networkDynamic::deactivate.edges(x = nd, onset = start, terminus = end, e = inactiveEdges )
  nd <- networkDynamic::activate.edges(x = nd, onset = start, terminus = end, e = activeEdges )
  ##------------------ COLLECT VERTICES TO REMOVE ------- 
  ##  REMOVE VERTICES founded_on > `end`
  if(vertFoundedAttr %in% vertAttrs) {
    tmp <- network::get.vertex.attribute(net, vertFoundedAttr)
    vids <- which(tmp > end) #(g)[which(tmp > end)]
    inactiveVerts <- unique( c(inactiveVerts, vids) )
  }
  ##  REMOVE VERTICES closed_on < `start`
  if(vertClosedAttr %in% vertAttrs) {
    tmp <- network::get.vertex.attribute(net, vertClosedAttr)
    vids <- which( tmp < start )  # V(g)[which(tmp < start)]
    inactiveVerts <- unique( c(inactiveVerts, vids) )
  }
  ##  REMOVE VERTICES acquired_at < `start`
  if(vertAcquiredAttr %in% vertAttrs) {
    tmp <- network::get.vertex.attribute(net, vertAcquiredAttr)
    vids <- which( tmp < start )  # V(g)[which(tmp < start)]
    inactiveVerts <- unique( c(inactiveVerts, vids) )
  }
  # ## ---------------GET UNIQUE ACTIVE VERTICES ----------------
  activeVerts <- unique(  which(!(net %v% 'id' %in% inactiveVerts))  )
  if (deactivateVertices) {
      nd <- networkDynamic::deactivate.vertices(x = nd, onset = start, terminus = end, v = inactiveVerts )
  }
  nd <- networkDynamic::activate.vertices(x = nd, onset = start, terminus = end, v = activeVerts)
  # ##---------- GET EDGES FOR WHICH VERTICES ARE INACTIVES -------
  el <- network::as.edgelist(net)
  inactiveVertsEdges <-  which( el[,1] %in% inactiveVerts | el[,2] %in% inactiveVerts )
  nd <- networkDynamic::deactivate.edges(x = nd, onset = start, terminus = end, e = inactiveVertsEdges )
  #----------------------------------------------------------------
  #                            DYNAMIC TEAs
  #----------------------------------------------------------------
  if( length(vertActiveAttrs) > 0) {
    for (attr in vertActiveAttrs) {
      val <- net %v% attr
      activate.vertex.attribute(x = nd, prefix = attr, value = val, onset=start, terminus = end)
    }
  }
  if( length(edgeActiveAttrs) > 0) {
    for (attr in edgeActiveAttrs) {
      val <- net %v% attr
      activate.edge.attribute(x = nd, prefix = attr, value = val, onset=start, terminus = end)
    }
  } 
  # ## ------------------HYPOTHESIS PREDICTORS------------------
  pred = list()
  cat('\ncomputing hypothesis predictor variables...\n')
  net.t <- network.extract(nd, onset=start, terminus = end)
  net.t.lag <- network.extract(nd, onset=lagStart, terminus = lagEnd)
  # 0. ENV RISK -- VERTEX ATTRIBUTE
  prefix <- 'env_risk';    cat('\ncomputing envelopment risk...\n')
  val <- envRisk(getIgraphFromNet(net.t), out.envrisk = TRUE)
  pred[[prefix]] <- val
  activate.vertex.attribute(nd, prefix, val,v = seq_len(net.t$gal$n), onset=start, terminus = end, dynamic.only = dynamic.only)
  # 1. MMC - Branches   (does NOT include market weight (revenue, customers, etc.), just binary overlap or not)
  prefix <- 'mmc';    cat('\ncomputing multi-market contact...\n')
  brsub <- br[which(br$company_name_unique %in% (net.t %v% 'vertex.names') & br$created_year < end), ]
  val <- sapply(1:length(brsub$company_name_unique), function(i) {
    if(i %% 50 == 0) cat(sprintf('\nMMC for firm i = %s of %s\n',i,length(brsub$company_name_unique)))
    sapply(1:length(brsub$company_name_unique), function(j) {
      if (i == j) {
        return(0)
      } else  {
        subi <- brsub[which(brsub$company_name_unique == brsub$company_name_unique[i] & brsub$company_name_unique != 'NA'),]
        subi <- subi[!duplicated(subi),]
        subj <- brsub[which(brsub$company_name_unique == brsub$company_name_unique[j] & brsub$company_name_unique != 'NA'),]
        subj <- subj[!duplicated(subj),]
        ni <- sum(subi$mmc_code %in% subj$mmc_code)
        nj <- sum(subj$mmc_code %in% subi$mmc_code)
        mmc <- (ni / length(subi$mmc_code)) * (nj / length(subj$mmc_code))
        return(mmc)
      } 
    })
  })
  pred[[prefix]] <- val
  activate.edge.attribute(nd, prefix, val, e=seq_along(net.t$mel), onset=start, terminus = end, dynamic.only = dynamic.only)
  # 2. Dist lag - (competition edges)  ## NETWORK OR EDGE PROPERTY? ??
  prefix <- 'dist_lag';    cat('\ncomputing distances lag contact...\n')
  val <- igraph::distances(getIgraphFromNet(network.extract(nd, onset = lagStart, terminus = lagEnd)))
  pred[[prefix]] <- val
  activate.edge.attribute(nd, prefix, val, e=seq_along(net.t.lag$mel), onset=lagStart, terminus = lagEnd, dynamic.only = dynamic.only)
  prefix <- 'de_alio_entry'
  val[(!is.na(val) & val > -Inf & val < Inf)] <- 1
  val[(is.na(val) | val == -Inf | val == Inf)] <- 0
  pred[[prefix]] <- val
  activate.edge.attribute(nd, prefix, val,e=seq_along(net.t$mel), onset=lagStart, terminus = lagEnd, dynamic.only = dynamic.only)
  # 3. IPO STATUS -- VERTEX ATTRIBUTE
  prefix <- 'ipo_status'; cat('\ncomputing IPO status contact...\n')
  iposub <- ipo[which(ipo$company_name_unique %in% (net.t %v% 'vertex.names')
                   & ipo$went_public_year < end), ]
  val <- ifelse((net %v% 'vertex.names') %in% iposub$company_name_unique, 1, 0)
  pred[[prefix]] <- val
  activate.vertex.attribute(nd, prefix, val, v=seq_len(net.t$gal$n), onset=start, terminus = end, dynamic.only = dynamic.only)
  # 4. Customer Status -- EDGE ATTRIBUTE
  return(list(nd=nd, pred=pred))
} 

#*******************************************
### ********IGRAPH PERIOD FUNCTION**********
# Sets active edges for the period in a network object 
# using attributes from an igraph object
##
makeIgraphPdSubgraphKeepNA <- function(g, start, end, 
                          vertFoundedAttr='founded_on',
                          vertClosedAttr='closed_on',
                          vertAcquiredAttr='acquired_on',
                          edgeCreatedAttr='relation_began_on',
                          edgeRemovedAttr='relation_ended_on',
                          removeIsolates=TRUE,
                          acq=NA,rou=NA,br=NA)
{
  tryCatch(cat(sprintf('\nmaking period %s-%s:\n', start,end)))
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
  ##------------------ COLLECT EDGES TO REMOVE -----------
  ## REMOVE EDGES CREATED AT
  if (edgeCreatedAttr %in% edgeAttrs) {
    tmp <- igraph::get.edge.attribute(g,edgeCreatedAttr)
    eids <- E(g)[which(tmp > end)]
    inactiveEdges <- c(inactiveEdges, eids)
    
  }
  ## ---------------GET UNIQUE ACTIVE VERTICES ----------------
  # activeEdges <- unique(  E(g)[ !(E(g)%in%inactiveEdges) ]  )
  ## -------------- UPDATE SUBGRAPH PRUNING EDGES -------------
  g <- igraph::delete.edges(g, edges=inactiveEdges)
  #-----------------------------------------------------------
  if(removeIsolates)
    g <- igraph::induced.subgraph(graph=g, vids=V(g)[which(igraph::degree(g) > 0)] )
  #------------------------ENV-RISK--------------------------
  g <- igraph::set.graph.attribute(g, 'distances', igraph::distances(g, mode='all'))
  g <- envRisk(g, out.graph = T)
  ##----------------------DYNAMIC ATTRS-----------------------
  ## AGE
  agediff <- end - V(g)$founded_year
  V(g)$age <- ifelse( agediff >= 0, agediff, NA)
  ## funding rounds
  if(any( !is.na(rou) )) 
    g <- setRouAttrs(g, rou, start, end)
  ## acquisitions
  if(any( !is.na(acq) )) 
    g <- setAcqAttrs(g, acq, start, end)
  ## branches
  if(any( !is.na(br) )) 
    g <- setBrAttrs(g, br, start, end)
  return(g)
} 
##----- dependent functions in makeIgraphPdSubgraphKeepNA() --------
setAcqAttrs <- function(g, acq, start, end)
{
  cat('optional: aggregating acquisitions...\n')
  x <-  ply.vert.attr(acq, g, 'acquired_year', 'ply.acq', start, end)
  check <- nrow(x) > 0 & all(c('acquisitions_count','acquisitions_concat') %in% names(x))
  if(check) {
    V(g)$acquisitions <- x$acquisitions_concat
    V(g)$acquisitions_count <- x$acquisitions_count
  } else {
    V(g)$acquisitions <- NA
    V(g)$acquisitions_count <- 0
  }
  V(g)[which(is.na(V(g)$acquisitions_count) | V(g)$acquisitions=="" )]$acquisitions_count <- 0
  return(g)
}
setRouAttrs <- function(g, rou, start, end)
{
  cat('optional: aggregating funding rounds...\n')
  x <- ply.vert.attr(rou, g, 'funded_year', 'ply.rou', start, end)
  check <- nrow(x) > 0 & all(c('funding_total_usd') %in% names(x))
  if(check)
    V(g)$funding_total_usd <- x$funding_total_usd
  else
    V(g)$funding_total_usd <- 0
  V(g)[is.na(V(g)$funding_total_usd)]$funding_total_usd <- 0
  V(g)$log_funding_total_usd <- log(V(g)$funding_total_usd + 1)
  V(g)$has_fund <- ifelse(V(g)$funding_total_usd>0,1,0)
  return(g)
}
setBrAttrs <- function(g, br, start, end)
{
  cat('optional: aggregating branches...\n')
  x <- ply.vert.attr(br, g, 'created_year', 'ply.br', start, end)
  check <- nrow(x) > 0 & all(c('branches_count','branches_concat') %in% names(x))
  if(check) {
    V(g)$branches <- x$branches_concat
    V(g)$branches_count <- x$branches_count      
  } else {
    V(g)$branches <- NA
    V(g)$branches_count <- 0 
  }
  V(g)[which(is.na(V(g)$branches_count) 
             | V(g)$branches=="" 
             | V(g)$branches=="_" )]$branches_count <- 0
  return(g)
}
##
ply.vert.attr <- function(df, g, filterPdVar, FUN, start, end)
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
              acquisitions_concat=paste(acquiree_name_unique, collapse = "|") )
}
ply.br <- function(df.sub)   # 'created_year
{
  plyr::ddply(df.sub, .(company_name_unique), .progress='text', summarise,
              branches_count=length(company_name_unique),
              branches_concat=paste(mmc_code, collapse = "|") )
}
#----// dependent functions -----------



###
# FOR USE IN TERMG (STERGM) where node set must remain constant
# here we only remove the edges going backward in time
# Sets active edges for the period in a network object 
# using attributes from an igraph object
##
# makeIgraphPdSubgraphAllNDeleteEdges <- function(g, start, end, 
#                                        vertFoundedAttr='founded_at',
#                                        vertClosedAttr='company_closed_on',
#                                        vertAcquiredAttr='acquired_at',
#                                        edgeCreatedAttr='relation_created_at',
#                                        acq=NA,rou=NA,br=NA)
# {
#   cat('collecting edges to remove...\n')
#   vertAttrs <- names(igraph::vertex.attributes(g))
#   edgeAttrs <- names(igraph::edge.attributes(g))
#   inactiveEdges <- c()
#   inactiveVerts <- c()
#   ##------------------ COLLECT VERTICES TO REMOVE ------- 
#   ##  REMOVE VERTICES founded_on > `end`
#   if(vertFoundedAttr %in% vertAttrs) {
#     tmp <- igraph::get.vertex.attribute(g,vertFoundedAttr) 
#     vids <- V(g)[which(tmp > end)]
#     inactiveVerts <- c(inactiveVerts, vids)
#   }
#   ##  REMOVE VERTICES closed_on < `start`
#   if(vertClosedAttr %in% vertAttrs) {
#     tmp <- igraph::get.vertex.attribute(g,vertClosedAttr) 
#     vids <- V(g)[which(tmp < start)]
#     inactiveVerts <- c(inactiveVerts, vids)
#   }
#   ##  REMOVE VERTICES acquired_at < `start`
#   if(vertAcquiredAttr %in% vertAttrs) {
#     tmp <- igraph::get.vertex.attribute(g,vertAcquiredAttr) 
#     vids <- V(g)[which(tmp < start)]
#     inactiveVerts <- c(inactiveVerts, vids)
#   }
#   # ## ---------------GET UNIQUE ACTIVE VERTICES ----------------
#   # activeVerts <- unique(  V(g)[ !(V(g)%in%inactiveVerts) ]  )
#   # ##-----------------MAKE SUBGRAPH ---------------------------
#   # cat('inducing subgraph...\n')
#   # g <- igraph::induced.subgraph(g, vids = activeVerts)
#   #------------------ REMOVE EDGES TO INACTIVE VERTICES ------------
#   el <- igraph::get.edgelist(g,names=F)
#   inactiveVertsEdges <- which(el[,1]  %in% inactiveVerts | el[,1]  %in% inactiveVerts )
#   g <- igraph::delete.edges(g, edges = inactiveVertsEdges)
#   ##------------------ COLLECT EDGES TO REMOVE -----------
#   ## REMOVE EDGES CREATED AT
#   if (edgeCreatedAttr %in% edgeAttrs) {
#     tmp <- igraph::get.edge.attribute(g,edgeCreatedAttr)
#     eids <- E(g)[which(tmp > end)]
#     inactiveEdges <- c(inactiveEdges, eids)
#     
#   }
#   ## ---------------GET UNIQUE ACTIVE VERTICES ----------------
#   # activeEdges <- unique(  E(g)[ !(E(g)%in%inactiveEdges) ]  )
#   ## -------------- UPDATE SUBGRAPH PRUNING EDGES -------------
#   g <- igraph::delete.edges(g, edges=inactiveEdges)
#   ##----------------------DYNAMIC ATTRS-----------------------
#   ## AGE
#   agediff <- end - V(g)$founded_year
#   V(g)$age <- ifelse( agediff >= 0, agediff, NA)
#   ## funding rounds
#   if(any( !is.na(rou) )) {
#     cat('optional: aggregating funding rounds...\n')
#     x <- ply.vert.attr(rou, g, 'funded_year', 'ply.rou', start, end)
#     check <- nrow(x) > 0 & all(c('funding_total_usd') %in% names(x))
#     if(check) {
#       V(g)$funding_total_usd <- ifelse(check, ifelse( is.na(x$funding_total_usd), 0, x$funding_total_usd), 0)
#     } else {
#       V(g)$funding_total_usd <- 0
#     }
#     V(g)$log_funding_total_usd <- log(V(g)$funding_total_usd + 1)
#     V(g)$has_fund <- ifelse(V(g)$funding_total_usd>0,1,0)
#   }
#   ## acquisitions
#   if(any( !is.na(acq) )) {
#     cat('optional: aggregating acquisitions...\n')
#     x <-  ply.vert.attr(acq, g, 'acquired_year', 'ply.acq', start, end)
#     check <- nrow(x) > 0 & all(c('acquisitions_count','acquisitions_concat') %in% names(x))
#     if(check) {
#       V(g)$acquisitions <- x$acquisitions_concat
#       V(g)$acquisitions_count <- ifelse( is.na(x$acquisitions_count)|x$acquisitions_count=="", 0, x$acquisitions_count)
#     } else {
#       V(g)$acquisitions <- NA
#       V(g)$acquisitions_count <- 0
#     }
#   }
#   ## branches
#   if(any( !is.na(br) )) {
#     cat('optional: aggregating branches...\n')
#     x <- ply.vert.attr(br, g, 'created_year', 'ply.br', start, end)
#     check <- nrow(x) > 0 & all(c('branches_count','branches_concat') %in% names(x))
#     if(check) {
#       V(g)$branches <- x$branches_concat
#       V(g)$branches_count <- ifelse(x$branches_count<1,1,0)      
#     } else {
#       V(g)$branches <- NA
#       V(g)$branches_count <- 0 
#     }
#   }
#   
#   return(g)
# } 
# 

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
      D[k] <- sum(d)
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
    D[k] <- sum(d)

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



# ##
# #
# ##
# .getMMCmatrix <- function(brsub, firms, firmNameVar, marketVar, is.symmetric)
# {
#   n <- length(firms)
#   if (is.symmetric) 
#   {
#     out <- sapply(1:n, function(j) { ## columns
#       if(j %% 5 == 0) cat(sprintf('MMC for firm j = %s of %s\n',j,length(brsub[,firmNameVar])))
#       sapply(1:n, function(i) {      ## rows
#         if (i <= j) {
#           ## upper triange + diagonal
#           return(0)
#         } else  { 
#           ## lower triange fill in...
#           mi <- brsub[which(brsub[,firmNameVar] == firms[i] & brsub[,firmNameVar] != 'NA'), marketVar]
#           mj <- brsub[which(brsub[,firmNameVar] == firms[j] & brsub[,firmNameVar] != 'NA'), marketVar]
#           if (length(mi)==0 | length(mj)==0) {
#             return(0)
#           } else {
#             ni <- sum(mi %in% mj)
#             nj <- sum(mj %in% mi)
#             mmc <- (ni / length(mi)) * (nj / length(mj))
#             return(mmc)
#           }
#         } 
#       })
#     }) 
#     t.out <- t(out)
#     out[upper.tri(out, diag = FALSE)] <- t.out[upper.tri(t.out, diag = FALSE)]
#   } 
#   else 
#   {
#     out <- sapply(1:n, function(j) { ## columns
#       if(j %% 5 == 0) cat(sprintf('MMC for firm j = %s of %s\n',j,length(brsub[,firmNameVar])))
#       sapply(1:n, function(i) {      ## rows
#         if (i == j) {
#           return(0)
#         } else  {
#           ## off-diagonal fill in..
#           subi <- brsub[which(brsub[,firmNameVar] == firms[i] & brsub[,firmNameVar] != 'NA'),]
#           subi <- subi[!duplicated(subi),]
#           subj <- brsub[which(brsub[,firmNameVar] == firms[j] & brsub[,firmNameVar] != 'NA'),]
#           subj <- subj[!duplicated(subj),]
#           if (nrow(subi)==0 & nrow(subj)==0) {
#             return(0)
#           } else {
#             ni <- sum(subi[,marketVar] %in% subj[,marketVar])
#             nj <- sum(subj[,marketVar] %in% subi[,marketVar])
#             mmc <- (ni / length(subi[,marketVar])) * (nj / length(subj[,marketVar]))
#             return(mmc)
#           }
#         } 
#       })
#     })   
#   }
# }
# 
# ##
# #
# ##
# getMultiMarketContact <- function(brsub, firms, firmNameVar='company_name_unique',marketVar='mmc_code', is.symmetric=TRUE)
# {
#   if( !('company_name_unique' %in% names(brsub)) 
#       | !('mmc_code' %in% names(brsub)) )
#     stop('data.frame must contains columns `company_name_unique` and `mmc_code`')
#   if (all(is.na(firms)) | all(is.nan(firms)) | 
#       all(is.null(firms)) | length(firms)==0) {
#     return(matrix(0))
#   }
#   if (nrow(brsub)==0) {
#     out <- matrix(0)
#   } else {
#     out <- .getMMCmatrix(brsub, firms, firmNameVar, marketVar, is.symmetric)
#     out[is.na(out)] <- 0
#     out[is.nan(out)] <- 0
#   }
#   return(out)
# }

# ##
# #
# ##
# makePdNetworkSetCovariates <- function(net, start, end, 
#                                        edgeCreatedAttr='relation_began_on',
#                                        edgeDeletedAttr='relation_ended_on',
#                                        vertFoundedAttr='founded_year',
#                                        vertClosedAttr='closed_year',
#                                        vertAcquiredAttr='acquired_year',
#                                        downweight.env.risk=FALSE,
#                                        netRiskCommunityAlgo='multilevel.community',
#                                        acq=NA,rou=NA,br=NA,ipo=NA)
# {
#   cat('collecting edges to remove...\n')
#   vertAttrs <- network::list.vertex.attributes(net)
#   edgeAttrs <- network::list.edge.attributes(net)
#   inactiveEdges <- c(); inactiveVertsEdges <- c(); inactiveVerts <- c()
#   ## ------------REMOVE EDGES FROM ABSENT VERTICES--------------------------
#   #
#   ##------------------ COLLECT EDGES TO REMOVE -----------
#   ## Get EDGES CREATED AT ___ to be removed
#   if (edgeCreatedAttr %in% edgeAttrs) {
#     tmp <- network::get.edge.attribute(net, edgeCreatedAttr)
#     eids <- which(tmp > end)
#     inactiveEdges <- c(inactiveEdges, eids)
#   }
#   ##------------------ COLLECT VERTICES TO REMOVE ------- 
#   ##  REMOVE VERTICES founded_on > `end`
#   if(vertFoundedAttr %in% vertAttrs) {
#     tmp <- network::get.vertex.attribute(net, vertFoundedAttr)
#     vids <- which(tmp > end) #(g)[which(tmp > end)]
#     inactiveVerts <- unique( c(inactiveVerts, vids) )
#   }
#   ##  REMOVE VERTICES closed_on < `start`
#   if(vertClosedAttr %in% vertAttrs) {
#     tmp <- network::get.vertex.attribute(net, vertClosedAttr)
#     vids <- which( tmp < start )  # V(g)[which(tmp < start)]
#     inactiveVerts <- unique( c(inactiveVerts, vids) )
#   }
#   ##  REMOVE VERTICES acquired_at < `start`
#   if(vertAcquiredAttr %in% vertAttrs) {
#     tmp <- network::get.vertex.attribute(net, vertAcquiredAttr)
#     vids <- which( tmp < start )  # V(g)[which(tmp < start)]
#     inactiveVerts <- unique( c(inactiveVerts, vids) )
#   }
#   # ##---------- GET EDGES FOR WHICH VERTICES ARE INACTIVES -------
#   el <- network::as.edgelist(net)
#   inactiveVertsEdges <-  which( el[,1] %in% inactiveVerts | el[,2] %in% inactiveVerts )
#   inactiveEdges <- unique(c(inactiveEdges, inactiveVertsEdges))
#   ##------------- DELTE EDGES --------------------------------------
#   net <- network::delete.edges(net, inactiveEdges)
#   #----------------------------------------------------------------
#   #
#   # ## ------------------HYPOTHESIS PREDICTORS------------------
#   g.net <- getIgraphFromNet(net)
#   ## # 1. ENV RISK -- VERTEX ATTRIBUTE
#   cat('\ncomputing risk measure...\n')
#   if (downweight.env.risk) {
#     rl <- envRisk(g.net) 
#     prefix <- 'env_risk'
#   } else {
#     rl <- netRisk(g.net, community.type = netRiskCommunityAlgo)
#     prefix <- 'net_risk'
#   }
#   net %v% prefix <- rl$r
#   net %v% 'npm' <- V(rl$g)$community
#   ## # 2. MMC - Branches   (does NOT include market weight (revenue, customers, etc.), just binary overlap or not)
#   cat('\ncomputing multi-market contact...\n')
#   mmc <- getMultiMarketContact(br, net%v%'vertex.names', end)
#   net %n% 'mmc' <- as.matrix(mmc)
#   ## # 3. Dist lag - (competition edges)  ## NETWORK OR EDGE PROPERTY? ??
#   cat('\ncomputing distances lag contact...\n')
#   D <- as.matrix(igraph::distances(g.net))
#   rownames(D) <- NULL;  colnames(D) <- NULL
#   D[D==0] <- 1e-16
#   net %n% 'dist' <- D
#   #
#   D[(!is.na(D) & D > -Inf & D < Inf)] <- 1
#   D[(is.na(D) | D == -Inf | D == Inf)] <-0
#   net %n% 'de_alio_entry' <- D
#   ## # 4. IPO STATUS -- VERTEX ATTRIBUTE
#   cat('\ncomputing IPO status contact...\n')
#   iposub <- ipo[which(ipo$company_name_unique %in% (net %v% 'vertex.names')
#                       & ipo$went_public_year < end), ]
#   net %v% 'ipo_status' <- ifelse((net %v% 'vertex.names') %in% iposub$company_name_unique, 1, 0)
#   ## # 4. Customer Status (coopetition) -- EDGE ATTRIBUTE
#   return(net)
# }

##
#
##
envRisk <- function(g,  community.type='multilevel.community',
                    risk.center = FALSE, risk.scale=FALSE,
                    out.envrisk=FALSE,out.dist=FALSE, 
                    out.graph=FALSE, out.df=FALSE)
{
  v <- ifelse(class(g)=='igraph', vcount(g),1)
  r <- rep(0, v)
  Z <- matrix(0,v,v)
  npms <- matrix(0)
  cat(sprintf('%s nodes  %s edges\n',vcount(g),ecount(g)))
  if(class(g)=='igraph')
    V(g)$community <- 0
  if(class(g)=='igraph' & ecount(g) > 0 )
  {
    ## ------------ NOISY PRODUCT MARKETS --------------------------
    com.ml <- do.call(community.type, list(graph=g))
    V(g)$community <- com.ml$membership
    ## npms
    coms <- unique(com.ml$membership)
    npms <- list()
    for (c_i in coms) {
      com.names <-com.ml$names[which(com.ml$membership==c_i)]
      npms[[c_i]] <- igraph::induced.subgraph(g, vids=V(g)[V(g)$name %in% com.names])
    }
    vcs <- sapply(npms, igraph::vcount)         #vertices
    ecs <- sapply(npms, igraph::ecount)         #edges
    dens <- sapply(npms,igraph::graph.density)  #densities
    ## find CROSS MARKET DENSITIES   (set diagonals to densities)
    cmd <- matrix(0, nrow=length(coms), ncol=length(coms))
    cmd <- sapply(1:length(coms), function(i){
      sapply(1:length(coms), function(j){
        if ( i != j ) {
          el <- get.edgelist(g, names=F)
          el <- el[which( (el[,1] %in% V(npms[[i]]) & el[,2] %in% V(npms[[j]]))
                          | (el[,1] %in% V(npms[[j]]) & el[,2] %in% V(npms[[i]])) ) ,  ]
          crossden_ij <- nrow(el) / (vcs[i] * vcs[j])
          cmd[i,j] <- ifelse(length(crossden_ij)>0, crossden_ij, 0)
        } else if ( i == j ) {
          cmd[i,j] <- dens[i]
        }
      })
    })
    if(is.null(dim(cmd)))  cmd <- matrix(cmd)
    cmd[is.na(cmd)] <- 0
    cmd[is.nan(cmd)] <- 0
    ##------------------ RISK COMPUTATION ------------------------------
    ## 1. DISTANCES Matrix excluding current competition (-1)
    D <- igraph::distances(g, mode="all") - 1
    diag(D) <- 0
    ## 2. COMMUNITY WEIGHT:  set same NPM (community) risk to 0
    W <- sapply(V(g)$community, function(x){
      sapply(V(g)$community, function(y){
        cmd[x,y]
      })
    })
    W[is.na(W)] <- 0
    ## 3. Z is down-weighted distance:  W  already has same-market density on diags, cross-market density off-diag
    Z <- D * (1-W)
    ## 4. risk [r] is inverse of sum of distances (of firms outside focal firm's NPM) ## scaled by (N-1) competitors for inter-network comparison
    Z.finiteColSum <- apply(Z,1,function(x){
      if ( length(x[x>-Inf & x<Inf]) == 1 ) { # isolate
        return(Inf)
      } else {
        sum(x[(x<-Inf & x<Inf)])
      }
    })
    r <- (vcount(g.sub)-1) / Z.finiteColSum   ## only counting non-isolate vertices
    r[r==Inf | r == -Inf] <- 0
    if (risk.center)
      r <- r - mean(r, na.rm = T)
    if (risk.scale)
      r <- r / sd(r, na.rm = T)
    names(r) <- V(g)$name
  }
  #-------------------------------------------------------------------
  V(g)$envrisk <- r
  if(out.envrisk)
    return(r)
  if(out.dist)
    return(Z)
  g <- igraph::set.graph.attribute(g, 'NpmWeightDist', Z)
  if(out.graph)
    return(g)
  df.r <- data.frame(name=V(g)$name,envrisk=V(g)$envrisk)
  df.r <- df.r[order(df.r$envrisk, decreasing=T),]
  if(out.df)
    return(df.r)
  return(list(g=g, r=r, npms=npms, df.r=df.r))
  
}


##
#
##
netRisk <- function(g,  community.type='multilevel.community',
                    risk.center = FALSE, risk.scale=FALSE,
                    out.netrisk=FALSE,out.dist=FALSE, 
                    out.graph=FALSE, out.df=FALSE)
{
  cat(sprintf('(community.type = %s) ',community.type))
  v <- ifelse(class(g)=='igraph', vcount(g),1)
  r <- rep(0, v); Z <- matrix(0,v,v); npms <- matrix(0)
  cat(sprintf('%s nodes  %s edges\n',vcount(g),ecount(g)))
  if(class(g)=='igraph') 
    V(g)$community <- 0
  if(class(g)=='igraph' & ecount(g) > 0 )
  {
    ## ------------ NOISY PRODUCT MARKETS --------------------------
    ## Subgraph
    V(g)$degree <- igraph::degree(g, v=V(g), mode='all', loops=F)
    g.sub <- igraph::induced.subgraph(g, V(g)[which(V(g)$degree > 0)])
    ## Communities on subgraph
    cat('communities = ')
    com.ml <- do.call(community.type, list(graph=g.sub))
    V(g.sub)$community <- com.ml$membership
    cat(sprintf('%s...',max(com.ml$membership)))
    ## npms on subgraph
    coms <- sort(unique(com.ml$membership))
    npms <- list()
    cat('subgraphs...')
    for (c_i in coms) {
      com.names <- com.ml$names[which(com.ml$membership==c_i)]
      npms[[c_i]] <- igraph::induced.subgraph(g.sub, vids=V(g.sub)[V(g.sub)$name %in% com.names])
    }
    vcs <- sapply(npms, igraph::vcount)         #vertices
    ecs <- sapply(npms, igraph::ecount)         #edges
    dens <- sapply(npms,igraph::graph.density)  #densities
    dens[is.nan(dens) | is.na(dens)] <- 0           ## fix NaN,NA
    ## find CROSS MARKET DENSITIES  on subgraph  (set diagonals to densities)
    cat('cross-NPM densities...')
    ## assign lower triangle elements
    cmd <- matrix(0, nrow=length(coms), ncol=length(coms))
    sapply( 1:length(coms), function(j){
      sapply( j:length(coms), function(i){
        if ( i != j ) {
          el <- get.edgelist(g.sub, names=F)
          el.sub <- el[which( (el[,1] %in% V(npms[[i]]) & el[,2] %in% V(npms[[j]]))
                          | (el[,1] %in% V(npms[[j]]) & el[,2] %in% V(npms[[i]])) ) ,  ]
          crossden_ij <- length(c(el.sub)) / (vcs[i] * vcs[j])
          cmd[i,j] <<- ifelse(length(crossden_ij)>0, crossden_ij, 0)  ## *BREAKS SCOPE*
        } else if ( i == j ) {
          cmd[i,j] <<- dens[i]   ## *BREAKS SCOPE*
        }
      })
    })
    if(is.null(dim(cmd)))  cmd <- matrix(cmd);  cmd[is.na(cmd)] <- 0; cmd[is.nan(cmd)] <- 0
    ## fill in upper triangle
    tcmd <- t(cmd)
    cmd[upper.tri(cmd)] <- tcmd[upper.tri(tcmd)]
    ## g.sub communities -> g
    V(g)$community <- 0
    for (i in V(g.sub)) {
      V(g)[ which( V(g)$name == V(g.sub)$name[i] ) ]$community <- V(g.sub)$community[i]
    }
    ##------------------ RISK COMPUTATION ------------------------------
    ## 1. DISTANCES Matrix excluding current competition (-1)
    cat('distances...')
    D <- igraph::distances(g, mode="all") # - 1     # ??? subtract 1 or not ???
    diag(D) <- 0                            ### remove ego-loops
    D[D == Inf | D == -Inf] <- 0           ### remove Inf before summing distances D
    ## 2. COMMUNITY WEIGHT:  set same NPM (community) risk to 0  get from (g.sub) set in to W of full (g)
    cat('weights...')
    W <- matrix(0, vcount(g),vcount(g))
    ### LOWER TRIANGLE
    sapply( 1:vcount(g.sub), function(j) {  #cat(sprintf('i  %s ',i))
      sapply( (j+1):vcount(g.sub), function(i) {  #cat(sprintf('i  %s\n',j))
        ## full graph node indices for W
        y <- which(V(g)$name == V(g.sub)$name[j])
        x <- which(V(g)$name == V(g.sub)$name[i])
        ## NPM indices
        b <- V(g.sub)$community[j]
        a <- V(g.sub)$community[i]
        ## assign weights # cat(sprintf('i %s j %s cmd[a,b]=%s\n',i,j,cmd[a,b]))
        W[x,y] <<- cmd[a,b]  ## *BREAKS SCOPE*: assign value to W in parent function scope
      })
    })
    # Assign lower.tri to upper.tri
    tW <- t(W)
    W[upper.tri(W, diag = F)] <- tW[upper.tri(tW, diag = F)]
    W[is.na(W) | is.nan(W) ] <- 0  ### will divide by W later --> 1/Inf = 0
    ## 3. Z = Distances (D) / Cross-Densities (W) [density on diags of W, cross-market density off-diags]
    Z <- D * (2 - W)
    ## 4. risk [r] sum over N firms of (density/distances) [Z]  (of firms outside focal firm's NPM) ## scaled by (N-1) competitors for inter-network comparison
    # # -- get subcomponent for each vert in g.sub
    # noniso.vcount <- sapply(V(g.sub)$name, function(name) {
    #   length(igraph::subcomponent(g.sub, v=which(V(g.sub)$name==name))) - 1
    # })
    # # -- scale each nonisolate's sum of weighted distances by its subcomponent vcount
    # sumZ <- colSums(Z)
    # r <- noniso.vcount / sumZ[which(names(sumZ) %in% names(noniso.vcount))]
    # # -- scale each nonisolate's sum of weighted distances by its subcomponent vcount [[length()/sum()]]
    sumZ <-  apply(X = Z, MARGIN = 1, FUN = function(x) {
      ifelse(sum(x)==0, Inf, length(x[x>0])/sum(x, na.rm = T) ) 
    })
    # -- risk = inverse sum pf distance
    r <- 1 / sumZ
    # -- correct Infinite values
    r[r==Inf | r == -Inf] <- 0              ### remove Inf from final risk value
    #--------------end risk computation------------------------
    if (risk.center)
      r <- r - mean(r, na.rm = T)
    if (risk.scale)
      r <- r / sd(r, na.rm = T)
    names(r) <- V(g)$name
  } 
  #-------------------------------------------------------------------
  V(g)$netrisk <- r
  cat('done.\n')
  if(out.netrisk)
    return(r)
  if(out.dist)
    return(Z)
  g <- igraph::set.graph.attribute(g, 'NpmWeightDist', Z)
  if(out.graph)
    return(g)
  df.r <- data.frame(name=V(g)$name,netrisk=V(g)$netrisk)
  df.r <- df.r[order(df.r$netrisk, decreasing=T),]
  if(out.df)
    return(df.r)
  return(list(g=g, r=r, npms=npms, df.r=df.r))
  
}

# ##------------------ RISK COMPUTATION ------------------------------
# ## 1. DISTANCES Matrix excluding current competition (-1)
# cat('distances...')
# D <- igraph::distances(g, mode="all")
# diag(D) <- 0
# ## 2. COMMUNITY WEIGHT:  set same NPM (community) risk to 0  get from (g.sub) set in to W of full (g)
# cat('weights...')
# tol <- 1e-3
# W <- matrix(tol, vcount(g),vcount(g))
# ### LOWER TRIANGLE
# sapply( 1:vcount(g.sub), function(j) {  #cat(sprintf('i  %s ',i))
#   sapply( (j+1):vcount(g.sub), function(i) {  #cat(sprintf('i  %s\n',j))
#     ## full graph node indices for W
#     y <- which(V(g)$name == V(g.sub)$name[j])
#     x <- which(V(g)$name == V(g.sub)$name[i])
#     ## NPM indices
#     b <- V(g.sub)$community[j]
#     a <- V(g.sub)$community[i]
#     ## assign weights
#     # cat(sprintf('i %s j %s cmd[a,b]=%s\n',i,j,cmd[a,b]))
#     W[x,y] <<- cmd[a,b]  ## *BREAKS SCOPE*: assign value to W in parent function scope
#   })
# })
# # Assign t(lower.tri) to upper.tri
# tW <- t(W)
# W[upper.tri(W, diag = F)] <- tW[upper.tri(tW, diag = F)]
# W[is.na(W)] <- 0
# ## 3. Z is down-weighted distance:  W  already has same-market density on diags, cross-market density off-diag
# Z <- D * W
# ## 4. risk [r] is inverse of sum of distances (of firms outside focal firm's NPM) ## scaled by (N-1) competitors for inter-network comparison
# cat('risk measure...')
# Z.finiteColSum <- apply(Z,1,function(x) {
#   if ( length(x[x>-Inf & x<Inf]) <= 1 | is.na(x) ) { # isolate
#     return(Inf)
#   } else {
#     sum(x[(x<-Inf & x<Inf)])
#   }
# })
# Z.finiteColSum[is.na(Z.finiteColSum)] <- Inf
# ## isolates = Inf --> r = 0
# r <- (vcount(g.sub)-1) / Z.finiteColSum   ## only counting non-isolate vertices
# r[r==Inf | r == -Inf] <- 0
# #--------------end risk computation------------------------

##
#
##
.getMarketsDf <- function(df, pd.end, drop=FALSE, ...)
{
  if( !('company_name_unique' %in% names(df)))
    stop('df must contain `company_name_unique` column')
  if( !('mmc_code' %in% names(df)))
    stop('df must contain `mmc_code` column')
  plyr::ddply(df, .variables = .(company_name_unique),.drop = drop,
              summarise,
              concat = paste(mmc_code,collapse = "|"),
              .progress = 'text', ...)
}

# outer(V(g)$community, V(g)$community,
#       Vectorize(function(x,y){paste(c(x,y),collapse="_")}))

##
# x string  Concatenated markets, eg, 'USA_CA|USA_NY|ARG_9'
# y == x
##
.getMMCfromMarketConcat <- function(x,y)
{
  mx <- c(stringr::str_split(x, '[|]', simplify = T))
  my <- c(stringr::str_split(y, '[|]', simplify = T))
  if (length(mx)==0 | length(my)==0)
    return(0)
  nx <- sum(mx %in% my)
  ny <- sum(my %in% mx)
  mmc <- (nx / length(mx)) * (ny / length(my))
  mmc[diag(mmc)] <- 0
  return(mmc)
}

##
#
##
getMultiMarketContact <- function(br, firms, end, ...)
{
  brsub <- br[which(br$company_name_unique %in% firms & br$created_year < end), ]
  cat('concatenating firm branch markets...\n')
  df <- .getMarketsDf(brsub, end, ...)
  df.m <- merge(data.frame(company_name_unique=firms,stringsAsFactors = F),
                df, by = 'company_name_unique', all.x=T, all.y=F)
  cat('computing MMC outer product matrix...')
  mmc <- outer(df.m$concat, df.m$concat, Vectorize(.getMMCfromMarketConcat))
  cat('done.\n')
  return( mmc )
}


##
#
##
makePdNetwork <- function(net, start, end, 
                          edgeCreatedAttr='relation_began_on',
                          edgeDeletedAttr='relation_ended_on',
                          vertFoundedAttr='founded_year',
                          vertClosedAttr='closed_year',
                          vertAcquiredAttr='acquired_year')
{
  cat('collecting edges to remove...\n')
  vertAttrs <- network::list.vertex.attributes(net)
  edgeAttrs <- network::list.edge.attributes(net)
  inactiveEdges <- c(); inactiveVertsEdges <- c(); inactiveVerts <- c()
  ##------------------ COLLECT EDGES TO REMOVE -----------
  ## Get EDGES CREATED AT ___ to be removed
  if (edgeCreatedAttr %in% edgeAttrs) {
    tmp <- network::get.edge.attribute(net, edgeCreatedAttr)
    eids <- which(tmp > end)
    inactiveEdges <- c(inactiveEdges, eids)
  }
  ##------------------ COLLECT VERTICES TO REMOVE ------- 
  ##  REMOVE VERTICES founded_on > `end`
  if(vertFoundedAttr %in% vertAttrs) {
    tmp <- network::get.vertex.attribute(net, vertFoundedAttr)
    vids <- which(tmp > end) #(g)[which(tmp > end)]
    inactiveVerts <- unique( c(inactiveVerts, vids) )
  }
  ##  REMOVE VERTICES closed_on < `start`
  if(vertClosedAttr %in% vertAttrs) {
    tmp <- network::get.vertex.attribute(net, vertClosedAttr)
    vids <- which( tmp < start )  # V(g)[which(tmp < start)]
    inactiveVerts <- unique( c(inactiveVerts, vids) )
  }
  ##  REMOVE VERTICES acquired_at < `start`
  if(vertAcquiredAttr %in% vertAttrs) {
    tmp <- network::get.vertex.attribute(net, vertAcquiredAttr)
    vids <- which( tmp < start )  # V(g)[which(tmp < start)]
    inactiveVerts <- unique( c(inactiveVerts, vids) )
  }
  # ##---------- GET EDGES FOR WHICH VERTICES ARE INACTIVES -------
  el <- network::as.edgelist(net)
  inactiveVertsEdges <-  which( el[,1] %in% inactiveVerts | el[,2] %in% inactiveVerts )
  inactiveEdges <- unique(c(inactiveEdges, inactiveVertsEdges))
  ##------------- DELTE EDGES --------------------------------------
  net <- network::delete.edges(net, inactiveEdges)
  ##
  return(net)
}

##
#
##
setCovariates <- function(net, start, end,
                        downweight.env.risk=FALSE,
                        netRiskCommunityAlgo='multilevel.community',
                        acq=NA,rou=NA,br=NA,ipo=NA)
{ 
  if( network::network.edgecount(net) > 0 ) {
    g.net <- getIgraphFromNet(net)
    ##------------------------------------
    ## # 0. AGE
    ##------------------------------------
    year <- net %v% 'founded_year'
    year[is.na(year) | is.nan(year)] <- median(year, na.rm = T)
    age <- end - year
    age[age < 0] <- 0
    net %v% 'age' <- age
    ##------------------------------------
    ## # 1. ENV RISK -- VERTEX ATTRIBUTE
    ##------------------------------------
    cat('\ncomputing risk measure...\n')
    if (downweight.env.risk) {
      rl <- envRisk(g.net) 
      prefix <- 'env_risk'
    } else {
      rl <- netRisk(g.net, community.type = netRiskCommunityAlgo)
      prefix <- 'net_risk'
    }
    net %v% prefix <- rl$r
    net %v% 'npm' <- V(rl$g)$community
    ##------------------------------------
    ## # 2. MMC - Branches   (does NOT include market weight (revenue, customers, etc.), just binary overlap or not)
    ##------------------------------------
    cat('computing multi-market contact...\n')
    mmc <- getMultiMarketContact(br, net%v%'vertex.names', end)
    net %n% 'mmc' <- as.matrix(mmc)
    ##------------------------------------
    ## # 3. Dist lag - (competition edges)  ## NETWORK OR EDGE PROPERTY? ??
    ##------------------------------------
    cat('computing distances lag contact...\n')
    D <- as.matrix(igraph::distances(g.net))
    rownames(D) <- NULL;  colnames(D) <- NULL
    D[D==0] <- 1e-16
    net %n% 'dist' <- D
    #
    D[(!is.na(D) & D > -Inf & D < Inf)] <- 1
    D[(is.na(D) | D == -Inf | D == Inf)] <- 0
    net %n% 'de_alio_entry' <- D
    ##------------------------------------
    ## # 4. IPO STATUS -- VERTEX ATTRIBUTE
    ##------------------------------------
    cat('computing IPO status contact...\n')
    iposub <- ipo[which(ipo$company_name_unique %in% (net %v% 'vertex.names')
                        & ipo$went_public_year < end), ]
    net %v% 'ipo_status' <- ifelse((net %v% 'vertex.names') %in% iposub$company_name_unique, 1, 0)
    ##------------------------------------
    ## # 5. Constraint -- NODE ATTR
    ##------------------------------------
    cat('computing constraint...\n')
    cons <-  igraph::constraint(g.net)    ### isolates' constraint = NaN
    cons[is.nan(cons) | is.na(cons)] <- 0 ### ?Is this theoretically OK?
    net %v% 'constraint' <- cons
    ##------------------------------------
    ## # 6. Similarity 
    ##------------------------------------
    cat('computing inv.log.w.similarity...\n')
    sim <- igraph::similarity(g.net,vids = V(g.net), 
                              mode = "all", method = "invlogweighted" )
    sim[is.nan(sim) | is.na(sim)] <- 0
    net %n% 'similarity' <- sim
    ##------------------------------------
    ## # 7. Centrality (betweenness)
    ##------------------------------------
    cat('computing betweenness...\n')
    betw <- igraph::betweenness(g.net)
    net %v% 'betweenness' <- betw
    net %v% 'betweenness_log' <- log(betw + .001) 
    ##------------------------------------
    ## # 8. Customer Status (coopetition) -- EDGE ATTRIBUTE
    ##------------------------------------
  } else {
    cat('zero edges, skipping attributes.\n')
  }
  
  return(net)
}

##
#
##
getNetEcount <- function(net, symmetric=TRUE, upper.tri.diag=FALSE)
{
  if(symmetric)  {
    return(sum(net[upper.tri(net, diag = upper.tri.diag)]))
  } else {
    return(sum(net[,]))
  }
}



