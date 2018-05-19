##
# amj_functions.R
##

library(igraph)
library(network)
library(intergraph)
library(xergm)



###
# Create igraph from competitive relation in comp dataframe
#  with optional vertex attributes in vertdf dataframe
# @param [dataframe] comp               The edgelist of competitive relations
# @param [dataframe] vertdf             The vertex attributes dataframe
# @param [character] name               The company's name attribute
# @param [character] compName           The compeitor's name attribute
# @param [character] relationStartCol   The competitive relation's starting date attribute
# @param [character] relationEndCol     The competitive relation's ending date attribute
# @param [character[]] vertAttrs        The names of vertex attributes from vertdf to include in the graph
# @return [igraph]
##
makeGraph <- function(comp,vertdf,name='company_name_unique', 
                      compName='competitor_name_unique', 
                      relationStartCol='relation_began_on',
                      relationEndCol='relation_ended_on',
                      vertAttrs=NA )
{
  if(is.na(vertAttrs)) {
    vertAttrs <- c('company_name','founded_on','founded_year','closed_on','closed_year','category_list',
                   'category_group_list','state_code','country_code','region','city','acquired_on')
  }
  el <- data.frame(source=comp[,name], 
                   target=comp[,compName],
                   relation_began_on=comp[,relationStartCol], 
                   relation_ended_on=comp[,relationEndCol], 
                   stringsAsFactors = F)
  ## remove missing names
  el <- el[which(el$source!="" & el$target!=""), ]
  ## make vertex df
  verts <- data.frame(company_name_unique=unique(c(el$source,el$target)), stringsAsFactors = F)
  verts <- merge(x=verts,y=vertdf[,c(name,vertAttrs[vertAttrs%in%names(vertdf)])],
                 by=name,all.x=T,all.y=F)  
  ## make graph
  g <- igraph::graph.data.frame(d = el, directed = F, vertices = verts)
  E(g)$weight <- 1
  V(g)$weight <- 1
  V(g)$orig.vid <- as.integer(V(g))
  return(g)
}



##
# Returns age covariate
# @see setCovariates()
# @param [network] net     The network object
# @param [integer] end     The ending year (excluded)
# @return [integer[]] 
##
.cov.age <- function(net, end)
{
  year <- net %v% 'founded_year'
  year[is.na(year) | is.nan(year)] <- median(year, na.rm = T)
  age <- end - year
  age[age < 0] <- 0
  return(age)
}


##
# Returns the vector of firm-branch regions, each concatenated as a character string (separated by pipes "|")
# @see setCovariates(), .covMmc()
# @param [dataframe] df   The firm-branch dataframe
# @param [boolean] drop   A flag to drop fields in the plyr::ddply() call
# @param [dataframe] 
# 
##
.cov.mmcMarketsDf <- function(df, drop=FALSE, ...)
{
  if( !('company_name_unique' %in% names(df)))
    stop('df must contain `company_name_unique` column')
  if( !('mmc_code' %in% names(df)))
    stop('df must contain `mmc_code` column')
  return(plyr::ddply(df, .variables = .(company_name_unique),.drop = drop,
                      summarise,
                      concat = paste(mmc_code,collapse = "|"),
                      .progress = 'text', ...))
}

##
# Returns the MMC value between two firms based on their firm branch regions
# @see setCovariates(), .covMmc()
# @param [character] x   The concatenated markets, eg, 'USA_CA|USA_NY|ARG_9'
# @param [character] y   The concatenated markets, eg, 'USA_CA|USA_NY|ARG_9'
# @return [float]
##
.cov.mmcfromMarketConcat <- function(x,y)
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
# Returns firm-branch geogrpahic overlap (proxying in this case firm-branch Multimarket contact)
# @see setCovariates()
# @param [dataframe] br       
# @param [character[]] firms  The vector of firm names (company_name_unique)
# @param [integer] end        The ending year (excluded)
# @return [matrix]
##
.cov.mmc <- function(br, firms, end, ...)
{
  cols <- c('company_name_unique','created_year')
  for (col in cols){
    if (col %in% names(br)) stop(sprintf('br dataframe missing attribute `%s`', col))
  }
  brsub <- br[which(br$company_name_unique %in% firms & br$created_year < end), ]
  cat('concatenating firm branch markets...\n')
  df <- .cov.mmcMarketsDf(brsub, ...)
  tmp <- data.frame(company_name_unique=firms,stringsAsFactors = F)
  df.m <- merge(x=tmp, y=df, by = 'company_name_unique', all.x=T, all.y=F)
  cat('computing MMC outer product matrix...')
  mmc <- outer(df.m$concat, df.m$concat, Vectorize(.cov.mmcfromMarketConcat))
  cat('done.\n')
  return(as.matrix(mmc))
}

##
# Returns the dyadic distances of nodes in g.net
# @see setCovariates()
# @param [igraph] g.net  The igraph object
# @return [matrix]
##
.cov.dist <- function(g.net)
{
  D <- as.matrix(igraph::distances(g.net))
  rownames(D) <- NULL
  colnames(D) <- NULL
  D[D==0] <- 1e-16
  return(D)
}

##
# Returns a binary vector of 1
# @see setCovariates()
# @param [network] net  The network object
# @return [matrix]
##
.cov.ipo <- function(net, ipo, end, nameAttr='vertex.names')
{
  cols <- c('company_name_unique','went_public_year')
  for (col in cols){
    if (col %in% names(br)) stop(sprintf('ipo dataframe missing attribute `%s`', col))
  }
  idx <- which(ipo$company_name_unique %in% (net %v% 'vertex.names') & ipo$went_public_year < end)
  iposub <- ipo[idx, ]
  names <- net %v% nameAttr
  return(ifelse(names %in% iposub$company_name_unique, 1, 0))
}

##
# Returns the vector of constraint values for each node
# @see setCovariates()
# @param [igraph] g.net  The igraph object 
# @return [float[]]
##
.cov.constraint <- function(g.net)
{
  cons <-  igraph::constraint(g.net)    ### isolates' constraint = NaN
  cons[is.nan(cons) | is.na(cons)] <- 0 ### ?Is this theoretically OK?
  return(cons)
}

##
# Returns the vector of similarity scores for each node
# @see setCovariates()
# @param [igraph] g.net      The igraph object 
# @param [character] method  The similarity compuation method (see igraph docs)
# @return [float[]]
##
.cov.similarity <- function(g.net, method='invlogweighted')
{
  sim <- igraph::similarity(g.net, vids = V(g.net), mode = "all", method = method)
  sim[is.nan(sim) | is.na(sim)] <- 0
  return(sim)
}

##
# Assigns multiple versions of centrality scores to the given network object
#   and returns the updated network object
# @see setCovariates()
# @param [network] net   The network object 
# @return [network] 
##
.cov.centrality(net)
{
  # cat('computing betweenness...\n')
  # betw <- igraph::betweenness(g.net)
  # net %v% 'betweenness' <- betw
  # net %v% 'betweenness_log' <- log(betw + .001) 
  net %v% 'cent_deg' <- igraph::degree(g.net)
  net %v% 'cent_eig' <- igraph::eigen_centrality(g.net)$vector
  ## larger exp (Bonacich "beta") increase sensitivity to effects from distant node
  pcn0.0 <- tryCatch(tmpn0.0<- igraph::power_centrality(g.net,exp= 0), error = function(e)e)
  pcn0.1 <- tryCatch(tmpn0.1<- igraph::power_centrality(g.net,exp=-0.1), error = function(e)e)
  pcn0.2 <- tryCatch(tmpn0.2<- igraph::power_centrality(g.net,exp=-0.2), error = function(e)e)
  pcn0.3 <- tryCatch(tmpn0.3<- igraph::power_centrality(g.net,exp=-0.3), error = function(e)e)
  pcn0.4 <- tryCatch(tmpn0.4<- igraph::power_centrality(g.net,exp=-0.4), error = function(e)e)
  pcn0.5 <- tryCatch(tmpn0.5<- igraph::power_centrality(g.net,exp=-0.5), error = function(e)e)
  
  if (!inherits(pcn0.0, "error")) net %v% 'cent_pow_n0_0' <- pcn0.0
  if (!inherits(pcn0.1, "error")) net %v% 'cent_pow_n0_1' <- pcn0.1
  if (!inherits(pcn0.2, "error")) net %v% 'cent_pow_n0_2' <- pcn0.2
  if (!inherits(pcn0.3, "error")) net %v% 'cent_pow_n0_3' <- pcn0.3
  if (!inherits(pcn0.4, "error")) net %v% 'cent_pow_n0_4' <- pcn0.4
  if (!inherits(pcn0.5, "error")) net %v% 'cent_pow_n0_5' <- pcn0.5
  
  return(net)
}


## 
# Assigns multiple versions of generalistIndex to the given network object
#   and returns the updated network object
# @see setCovariates(), generalistIndex()
# @param [network] net       The network object 
# @return [network] 
## 
.cov.generalistIndex <- function(net)
{
  ## Community membership
  g.net <- intergraph::asIgraph(net)
  ##igraph::optimal.community()  ## too slow
  ##igraph::spinglass.community() ## too slow
  net %v% 'com_multilevel'  <- igraph::multilevel.community(g.net)$membership
  net %v% 'com_infomap'     <- igraph::infomap.community(g.net)$membership
  net %v% 'com_walktrap'    <- igraph::walktrap.community(g.net)$membership
  net %v% 'com_fastgreedy'  <- igraph::fastgreedy.community(g.net)$membership
  net %v% 'com_edgebetween' <- igraph::edge.betweenness.community(g.net)$membership
  net %v% 'com_labelprop'   <- igraph::label.propagation.community(g.net)$membership
  # net %v% 'com_eigenvector' <- igraph::leading.eigenvector.community(g.net)$membership  ## Arpack solver error
  ## Generalist Index
  net %v% 'genidx_multilevel'  <- generalistIndex(g.net, net %v% 'com_multilevel' )
  net %v% 'genidx_infomap'     <- generalistIndex(g.net, net %v% 'com_infomap' )
  net %v% 'genidx_walktrap'    <- generalistIndex(g.net, net %v% 'com_walktrap' )
  net %v% 'genidx_fastgreedy'  <- generalistIndex(g.net, net %v% 'com_fastgreedy' )
  net %v% 'genidx_edgebetween' <- generalistIndex(g.net, net %v% 'com_edgebetween' )
  net %v% 'genidx_labelprop'   <- generalistIndex(g.net, net %v% 'com_labelprop' )
  return(net)
}


##
# Computes the Generalist (vs Specialist) Index
# for an igraph with given cluster memberships
# values bounded between 0 and K (for K clusters in the memberships)
# @param [igraph] g   The igraph object
# @param [int[]] memberships  The vector of integers representing cluster memberships
# @return [float[]] The generalist index vector [float[]]
##
generalistIndex <- function(g, memberships)
{
  if (class(g) != 'igraph') stop("g must be an igraph object")
  clusters <- unique(memberships)
  K <- length(clusters)
  adj <- igraph::get.adjacency(g, sparse = F)
  ## apply GI algorithm to adjacency matrix of graph g
  gen.idx <- sapply(1:nrow(adj), function(i){
    x <- adj[i,]
    val <- 0 ## reset value for next node
    for (k in clusters) {
      ## get index of vertices in k'th cluster
      v.in.k <- which(memberships == k)
      ## exclude vertex i from computation of it's own generalist index
      v.in.k <- v.in.k[v.in.k != i]
      ## filter to members in cluster k (cols) for this row x
      sub.k.x <- x[v.in.k]
      ## sum edges from focal node i to members of cluster k
      cnt.e.k <- sum(sub.k.x)
      ## total possible edges node i to cluster k
      max.e.k <- length(sub.k.x)
      ## increment value by cluster ratio
      ## define ratio as 0 if empty denominator (max.e.k=0)
      val <- val + ifelse(max.e.k > 0, (cnt.e.k / max.e.k), 0)
    }
    return(val)
  })
  return(gen.idx)
}

##
# Set network covariates
# @param [network] net          The network object
# @param [integer] start        The starting year (included)
# @param [integer] end          The ending year (excluded)
# @param [character[]] covlist  The names of covariates to add to the network (vertices or edges), including 'age','mmc','dist','similarity','ipo_status','centrality','generalist'
# @param [dataframe] acq        The acquisitions dataframe
# @param [dataframe] rou        The investment|funding roundings dataframe
# @param [dataframe] br         The firm branches dataframe 
# @param [dataframe] ipo        The firm IPO listing dataframe
# @return [network]
##
setCovariates <- function(net, start, end,
                          covlist=c('age','mmc','dist','similarity','ipo_status','centrality','generalist'),
                          acq=NA,rou=NA,br=NA,ipo=NA)
{ 
  if( network::network.edgecount(net) > 0 ) {
    g.net <- intergraph::asIgraph(net)
    if ('age' %in% covlist) {
      cat('\ncomputing age ...\n')
      net %v% 'age' <- .cov.age(net, end)
    }
    if ('mmc' %in% covlist) {
      cat('computing multi-market contact (branch geographic overlap)...\n')
      net %n% 'mmc' <- .cov.mmc(br, (net %v% 'vertex.names'), end)
    }
    if ('dist' %in% covlist) {
      cat('computing distances lag contact...\n')
      net %n% 'dist' <- .cov.dist(g.net)
    }
    if ('ipo_status' %in% covlist) {
      cat('computing IPO status contact...\n')
      net %v% 'ips_status' <- .cov.ipo(net, ipo, end) 
    }
    if ('constraint' %in% covlist) {
      cat('computing constraint...\n')
      net %v% 'constraint' <- .cov.constraint(g.net)
    }
    if ('similarity' %in% covlist) {
      cat('computing inv.log.w.similarity...\n')
      net %n% 'similarity' <- .cov.similarity(g.net)
    }
    if ('centrality' %in% covlist) {
      cat('computing centralities...\n')
      net <- .cov.centrality(net)
    }
    if ('generalist' %in% covlist) {
      cat('computing Generalist (vs Specialist) Index...')
      net <- .cov.generalistIndex(net)
    }
    cat('...done\n')
    
  } else {
    
    cat('zero edges, skipping attributes.\n')
    
  }
  
  return(net)
}




##
# Update Graph collapsing nodes by acquisitions mapping
# NOTE: add 'weight' and 'acquisitions' attributes to graph before start
# @param [igraph] g      The igraph object 
# @param [dataframe] df  The dataframe of acquisitions
# @param [bool] verbose  A flag to echo stutus updates
# @return [igraph] The node-collapsed igraph object
##
nodeCollapseGraph <- function(g, acquisitions, verbose = FALSE)
{
  if (class(g) != 'igraph') stop("g must be an igraph object")
  if (class(acquisitions) != 'data.frame') stop("acquisitions must be a data frame")
  
  ##--------------------- Acquisitions Mapping --------------------------
  ## acqs = c(1,4,3,3,1,4,...)
  ## {acquired} index --> {acquirer} acqs[index]  WHEN BOTH IN NETWORK
  acqs.sub <- acquisitions[which(acquisitions$acquirer_vid %in% V(g)$orig.vid
                                 & acquisitions$acquiree_vid %in% V(g)$orig.vid ), ]
  cat(sprintf('processing acquisitions: %s ...', nrow(acqs.sub)))
  if (nrow(acqs.sub) == 0) {
    
    g.acq.s <- g
    
  } else {
    
    # acqMapping <- V(g)$orig.vid
    acqMapping <- as.integer( V(g) )
    if (verbose) cat('updating acq mapping i: ')
    for(i in 1:length(unique(acqs.sub$acquirer_vid))) {
      if (verbose) cat(sprintf(" %s ",i))
      acqr.i <- unique(acqs.sub$acquirer_vid)[i] ##  acquirer vid
      acqe.sub.i <- acqs.sub[acqs.sub$acquirer_vid == acqr.i, 'acquiree_vid'] ## acquiree's vids
      acqe.vids <- acqe.sub.i[which(acqe.sub.i %in% V(g)$orig.vid)] ## filter acquiree's vids to those in subgraph
      if (length(acqe.vids) > 0) { ##replace current g vid of acquiree with current graph vid of acquirer by orig.vid property
        acqr.g.i <- as.integer( V(g)[ which(V(g)$orig.vid == acqr.i) ] )
        acqe.g.vids <- sapply(acqe.vids, function(x)as.integer(V(g)[which(V(g)$orig.vid == x)]))
        acqMapping[  which( as.integer(V(g)) %in% acqe.g.vids ) ] <- as.integer(acqr.g.i)  ## assign acquirer's vid to value in acquiree's spots
      }
    }
    
    if (verbose) cat('reindexing mapping ')
    ## change orig.vid to current subgraph vids (reindexing)
    # acqMappingSub <- sapply(1:length(acqMapping), function(i){ 
    #   if (verbose & (i %% 500 == 0)) cat(sprintf(" %s ",i))
    #   x <- acqMapping[i]
    #   return( as.integer(V(g)[which(x==V(g)$orig.vid)]) )
    # })
    acqMappingSub <- acqMapping ## no need to reindex here bc already using current subgraph vids
    if (verbose) cat('finished reindexing  ')
    
    ##-------------- CONFIG GRAPH ATTRIBUTES COMBINATIONS ---------------------
    ## build vertex attr comb list
    vertex.attr.comb <- list(weight=function(x)sum(x),
                             tmp.name=function(x)x[1],
                             tmp.orig.vid=function(x)x[1],
                             absorbed=function(x)paste(x,collapse="|") ) ## paste(x,collapse="|")
    attrs <- igraph::list.vertex.attributes(g)
    skipAttrs <- c('name','weight',names(vertex.attr.comb))
    if (verbose) cat('finished reindexing  ')
    for (attr in attrs[which(!(attrs %in% skipAttrs))]) {
      vertex.attr.comb[[attr]] <- function(x)paste(unique(x),collapse="|")
    }
    if (verbose) cat('finished adding attrs ')
    
    ## temporary attrs used to concat in mapping
    V(g)$absorbed <- V(g)$name
    V(g)$tmp.name <- V(g)$name[acqMappingSub]
    V(g)$tmp.orig.vid <- V(g)$orig.vid[acqMappingSub]
    
    ##---------------------- COLLAPSE NODES ---------------------------------
    if (verbose) cat('contracting vertices ')
    g.acq <- igraph::contract.vertices(g, acqMappingSub, vertex.attr.comb=vertex.attr.comb)
    
    ## remove nodes that were acquired (had no remaining edges : degree=0)
    if (verbose) cat('inducing subgraph ')
    g.acq <- igraph::induced.subgraph(g.acq,vids = which(igraph::degree(g.acq)>0))
    V(g.acq)$name <- V(g.acq)$tmp.name
    V(g.acq)$orig.vid <- V(g.acq)$tmp.orig.vid
    
    ## contract edges
    if (verbose) cat('simplifying edges ')
    edge.attr.comb = list(weight="sum",relation_began_on="min",relation_ended_on="min")
    g.acq.s <- igraph::simplify(g.acq, remove.multiple=T, remove.loops=T, edge.attr.comb=edge.attr.comb)
    
  } 
  
  cat('done.\n')
  return(g.acq.s)
}



##
# Makes period graph 
# - remove missing edges and nodes
# - transfers edges and apply node collapse on acquisitions
# @see Hernandez & Menon 2017  Network Revolution
##
makePdGraph <- function(g, start, end,
                        isolates.remove = FALSE,
                        edgeCreatedAttr='relation_began_on',
                        edgeDeletedAttr='relation_ended_on',
                        vertFoundedAttr='founded_on',
                        vertClosedAttr='closed_on',
                        vertAcquiredAttr='acquired_on')
{
  if (class(g) != 'igraph') stop("g must be an igraph object")
  ## start INCLUSIVE : end EXCLUSIVE
  ## start <= {date} < end
  vertAttrs <- igraph::list.vertex.attributes(g)
  edgeAttrs <- igraph::list.edge.attributes(g)
  inactiveEdges <- c(); inactiveVertsEdges <- c(); inactiveVerts <- c()
  ##------------------ REMOVE EDGES ----------- 
  cat('collecting edges to remove...')
  ## REMOVE EDGES CREATED AT  >= end  (exclude end day)  OR UNKNOWN CREATED DATE
  if (edgeCreatedAttr %in% edgeAttrs) {
    tmp <- igraph::get.edge.attribute(g, edgeCreatedAttr) 
    # eids <- which(tmp >= end | tmp == "NA") ## created after OR unknown start
    eids <- which(tmp >= end & tmp != "NA") ## created after OR unknown start
    inactiveEdges <- c(inactiveEdges, eids) 
  }  
  ## REMOVE EDGES ENDED AT < start  AND ENDED AT DATE IS NOT UNKNOWN
  if (edgeDeletedAttr %in% edgeAttrs) { 
    tmp <- igraph::get.edge.attribute(g, edgeDeletedAttr)
    # eids <- which(tmp < start & tmp != "NA") ## created before AND not NA (NA means not yet ended)
    eids <- which(tmp < start & tmp != "NA") ## created before AND not NA (NA means not yet ended)
    inactiveEdges <- c(inactiveEdges, eids)
  }
  g <- igraph::delete.edges(g, inactiveEdges)
  ##------------------ REMOVE VERTICES  ------- 
  cat('done.\ncollecting vertices to remove...')
  ##  REMOVE VERTICES founded_on >= END OR UNKONW FOUNDED ON DATE
  if(vertFoundedAttr %in% vertAttrs) {
    tmp <- igraph::get.vertex.attribute(g, vertFoundedAttr)
    # vids <- which(tmp >= end | tmp == "NA") 
    vids <- which(tmp >= end & tmp != "NA") 
    inactiveVerts <- unique( c(inactiveVerts, vids) )
  }
  ##  REMOVE VERTICES closed_on < START AND CLOSED DATE IS NOT UNKOWN
  if(vertClosedAttr %in% vertAttrs) {
    tmp <- igraph::get.vertex.attribute(g, vertClosedAttr)
    vids <- which(tmp < start & tmp != "NA")  
    inactiveVerts <- unique( c(inactiveVerts, vids) )
  }
  ##  REMOVE VERTICES acquired_on < `start`
  if(vertAcquiredAttr %in% vertAttrs) {
    tmp <- igraph::get.vertex.attribute(g, vertAcquiredAttr)
    vids <- which( tmp < start & tmp != "NA" )  # V(g)[which(tmp < start)]
    inactiveVerts <- unique( c(inactiveVerts, vids) )
  }
  ## GET active VERTICES
  activeVerts <- which( !(V(g) %in% inactiveVerts) )
  ## SUBGRAPH OF ONLY ACTIVE VERTICES
  g <- igraph::induced.subgraph(g, activeVerts)
  ## remove isolates
  if (isolates.remove)
    g <- igraph::induced.subgraph(g,vids = which(igraph::degree(g)>0))
  cat('done.\n')
  return(g)
}
