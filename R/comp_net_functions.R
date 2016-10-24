##########################################################################################
#
# COMPETITION NETWORKS FUNCTION
#
# @author   Stephen Downing <sdowning.bm02g@nctu.edu.tw>
# @date     May 2015
#
##########################################################################################
#.libPaths('C:/Users/sdowning/Documents/R/win-library/3.2')
library(igraph)

#---------- convenience function ------------
va <- function(g){
  vertex.attributes(g)
}
gdf <- function(g)
{
  get.data.frame(x=g, what = 'vertices')
}
spl <- function(x)
{
  str_split(x, "[|]")[[1]]
}
jn <- function(x)
{
  paste(x, collapse = "|")
}
catList <- function(net, attr)
{
  out <- net %v% attr
  names(out) <- net %v% 'vertex.names'
  lapply(out, function(x) spl(x))
}

#--------------------------------------------

### 
# KEEP NAME OF AQUIRER WHEN CONTRACTING VERTICES FOR ACQUISITION `vertex.attr.comb`
##
vertCombNames.orig <- function(x, acquirer, concat=FALSE)
{
  if(concat) {
    out <- ifelse(length(x)==1,x,sprintf('%s (%s)',x[which(x==acquirer)],paste(x[which(x!=acquirer)],collapse=','))) 
  } else {
    out <- ifelse(length(x)==1,x,x[which(x==acquirer)])
  }
  return(out)
}
##
vertCombNames <- function(x, acquirer, concat=FALSE)
{
  if(length(x)==1)
    return(x)
  if (concat) {
    out <- sprintf('%s (%s)', x[which(x==acquirer)], paste(x[which(x!=acquirer)],collapse=',') )
  } else {
    out <- unlist(x[which(x==acquirer)])
  }
  return(out)
}

###
#
#
##
collapseAcquisitions <- function(x, acquirer)
{
  out <- ifelse(length(x)==1,"",paste(x[which(x!=acquirer)],collapse='|'))
  return(out)
}

###
#
#
##
getVertAttrList <- function(g, acquirer, target, attrNames=NA)
{
  if(any(is.na(attrNames)))
    attrNames <- names(igraph::vertex.attributes(g))
  ## GET LIST OF VERTEX ATTRIBUTES TO COMPUTE WHEN CONTRACTING THE ACQUISITION GRAPH
  vertAttrList <- list()
  for(attr in attrNames) {
    if(attr %in% c('name','founded_year','age')) 
      next
    if(attr %in% names(igraph::vertex.attributes(g))) {
      tryDatum <- na.omit(igraph::get.vertex.attribute(g,attr))[1]
      tryResult <- try(do.call('sum',list(tryDatum)),silent=T)      
    }
    if(inherits(tryResult,'try-error'))
      next
    vertAttrList[[attr]] <- 'sum'
  }
  ## MANUALLY ADD FUNCTION TO COMBINE ACQUISITION NAMES
  vertAttrList <- c(vertAttrList, list(
    acquisitions= function(x)paste(x, collapse='|')#function(x)ifelse(length(x)==1,"",paste(x[which(x!=acquirer)],collapse='|'))
  ))
  return(vertAttrList)
}

###
# Get vector of node indices mapping acquirer to aquired nodes
#
##
getContractionMapping <- function(g, acquirer, target)
{
  lenAcq = length(acquirer)
  if(lenAcq != length(target))
    stop('acquirer and target must be same length')
  vertNames <- V(g)$name  
  mapping <- V(g)$name %>% as.factor() %>% as.numeric()
  for(i in 1:lenAcq) {
    a_i <- acquirer[i]
    t_i <- target[i]
    if( t_i%in%vertNames & a_i%in%vertNames ){
      targetIndex <- which(vertNames==t_i)
      acquirerIndex <- which(vertNames==a_i)
      mapping[targetIndex] <- acquirerIndex
    }    
  }
  return(mapping)
}


###
#  Get the list of attributes which are to be added to contracted graph before deleting isolate nodes
#
##
getAttributeMappingList <- function(g, mapping)
{
  attrList <- list()
  attrs <- c('name','founded_at','founded_month','founded_quarter','founded_year','acquired_at','age')
  for(attr in attrs){
    if(attr %in% names(vertex.attributes(g))) {
      attrList[[attr]] <- sapply(mapping, function(x)get.vertex.attribute(graph=g, name=attr, index=x)) 
    }
  }
  return(attrList)
}

###
#  Add the attributes from the attribute list back to the contracted graph before deleting isolate nodes
#
##
applyAttributeMapping <- function(g, attrMapList)
{
  for(attr in names(attrMapList)){
    if(attr %in% names(vertex.attributes(g))) {
      g <- set.vertex.attribute(graph=g, name=attr, value=attrMapList[[attr]])
    }
  }
  return(g)
}

###
# Removes nodes with no edges remaining after contraction
#
##
deleteIsolates <- function(g)
{
  tryDeleteVertices <- try(delete.vertices(g, which(degree(g)==0)), silent=TRUE)
  if (inherits(tryDeleteVertices, 'try-error')) {
    cat(tryDeleteVertices)
    stop("deleteIsolates() function error:")
  } else {
    g <- tryDeleteVertices    
  }
  return(g)
}

###
# CREATE ACQUISITION CONTRACTED GRAPH
#
##
getAcquisitionContractedGraph <- function(g,acquirer,target,attrNames=NA,deleteIsolates=TRUE)
{
  if(!('acquisitions'%in% names(igraph::vertex.attributes(g))))
    V(g)$acquisitions <- V(g)$name %>% as.vector()
  ## GET LIST OF VERTEX ATTRIBUTES TO COMPUTE WHEN CONTRACTING THE ACQUISITION GRAPH
  vertAttrList <- getVertAttrList(g, acquirer, target, attrNames)
  ## GET MAPPING OF ACQUIRER TO TARGET INDICES FOR CONTRACTION (if both in current subgraph)
  mapping <- getContractionMapping(g, acquirer, target)
  attrMapList <- getAttributeMappingList(g, mapping)
  ## DO GRAPH CONTRACTION (if any acquisition pair [acquirer, target] exists in current subgraph)
  g <- igraph::contract( g, mapping = mapping,  vertex.attr.comb=vertAttrList )
  g <- simplify(g, remove.multiple=T,remove.loops=T,edge.attr.comb=list(weight='sum' ))
  ## MANUALLY UPDATE GRAPH ATTRIBUTES
  g <- applyAttributeMapping(g, attrMapList)
  ## REMOVE NODES WITHOUT EDGES
  if(deleteIsolates)
    g <- deleteIsolates(g)
  cat('\nvcount: ',vcount(g),'\n')
  return(g)
}


###
# EXAMPLE DYNAMIC COMPETITION NETWORK WITH ACQUISITIONS
#
##
getExampleDynamicCompNet <- function(output=TRUE)
{  
  w.l  <- list()
  w <- graph.data.frame(d=data.frame(
      s=c(1,1,1,2,3,3,4,4,5)
    , t=c(2,3,5,4,2,4,6,5,6)
    ), directed = F)
  V(w)$name <- c('A','B','C','D','E','F')
  set.seed(1111)
  V(w)$patents <- sample(seq(0,5),vcount(w),replace = T)
  V(w)$funding_total_usd <- sample(c(rep(0,3),2^seq(14,24)),vcount(w),replace = T)
  V(w)$founded_at <- sprintf('%s-01-01',sample(seq(2005,2015),size=6,replace = T))
  V(w)$age <- sample(1:5,vcount(w),replace = T)
  E(w)$weight <- 1

  ## MAIN LOOP
  acqs <- data.frame(acquirer = c('F','F','F'), 
                     target = c('A','C','B'),
                     stringsAsFactors = F)
  w.l[[1]] <- w
  for (i in 1:nrow(acqs)) {
    w.l[[1+i]] <- getAcquisitionContractedGraph(w.l[[i]], acquirer = acqs$acquirer[i], target = acqs$target[i])
  }
  
  par(mfrow=c(2,2),mar=c(.5,.5,1,1))
  plotNet(w.l[[1]], vertexSizeAttr='patents',legendText='t = 1')
  for(i in 2:length(w.l)) {
    al <- sapply(V(w.l[[i]])$acquisitions, function(x)strsplit(x,split = '[|]')[[1]])
    acquirer.name <- acquired.names <- NULL
    if(length(al)>0){
      anames <- al[[which.max(sapply(al,length))]]
      acquirer.name <- anames[which(anames %in% V(w.l[[i]])$name)]
      acquired.names <- anames[which( !(anames %in% V(w.l[[i]])$name) )]      
    }
    plotNet(w.l[[i]], 
      vertexSizeAttr='patents',
      legendText=paste0(
          't = ',i,'\n',paste0(acquirer.name,'-->(',paste(acquired.names,collapse=','),')')
      )
    )
  }
  if(output)
    return(w.l)
}

###
#  WRAPPER FOR igraph PLOTTING
#
#  [label.font]    1 plain text
#                  2 bold face
#                  3 italic
#                  4 bold and italic
#                  5 (symbol font)
#  [label.family]   serif, sans, mono
##
plotNet <- function(g,vertexSizeAttr='funding_total_usd',legendText=NA, vertScale=18, logSize=TRUE, ...)
{
  set.seed(1111)
  igraph::plot.igraph( g
                       , layout=layout.circle
                       , vertex.color='steelblue'
                       , vertex.label= V(g)$name  #paste0(V(g)$name,'\n',V(g)$funding_total_usd)
                       , vertex.label.color='white'
                       , vertex.label.font = 2
                       , vertex.label.family = 'sans'
                       , vertex.size=15+log(1+V(g)$funding_total_usd)
                       , edge.label=E(g)$weight
                       , edge.label.color='black'
                       , edge.label.cex=1.3
                       , edge.label.family='sans'
                       , edge.width=E(g)$weight*2
                       , edge.curved=F
  )
  if(!is.na(legendText))
    legend('topright',legend=legendText,bty='n')
}

###
# MAP A VECTOR X PROPORTIONALLY TO NEW RANGE (Y1, Y2)
##
map <- function(x,y1,y2)
{
  dist <- diff(range(x))
  ymin <- min(y1,y2)
  ymax <- max(y1,y2)
  p <- (x - min(x))/dist
  ydist <- ymax - ymin
  y <- ymin + p*ydist
  return(y)
}

###
# GET ZERO, NON-ZERO PROPORTIONS
##
zprop <- function(x,digits=3)
{
  z <- ifelse(x>0,1,0)
  s <- sum(z)
  l <- length(z)
  df <- data.frame( p1=s/l, p0=1-(s/l) )
  return(round(df,digits))
}

###
# PAIRS PLOT HELPER FUNCTIONS:
#     `cor`
#     `smooth`
#     `hist`
##
panel.cor <- function(x, y, digits=2, cex.cor)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  #txt <- format(c(r, 0.123456789), digits=digits)[1]
  test <- cor.test(x,y)
  Signif <- ifelse(round(test$p.value,3)<0.001,"p<0.001",paste("p=",round(test$p.value,3)))
  text(0.5, 0.25, sprintf('r = %.2f',r), cex=1.4)
  text(.5, .75, Signif, cex=1.4)
}
panel.smooth<-function (x, y, col = "steelblue", bg = NA, pch = 18,cex = 0.8, 
                        col.smooth = "red", span = 2/3, iter = 3)
{
  points(x, y, pch = pch, col = col, bg = bg, cex = cex)
  ok <- is.finite(x) & is.finite(y)
  if (any(ok))
    lines(stats::lowess(x[ok], y[ok], f = span, iter = iter),
          col = col.smooth)
}
panel.hist <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col="steelblue", ...)
}

###
# ENHANCED pairs() FUNCTION
# SAMPLE, LOG TRANSFORM, DROP FACTOR VARS, OMIT NA, PLOT SMOOTH,HIST,CORR
##
pairsMod <- function(x,yName='acq_count',sampSize=2000,output=FALSE,naOmit=FALSE,dropNames=c('company_name_unique','period'),seed=1111)
{
  yIndex <- which(names(x)==yName)
  x <- x[,which(!(names(x)%in%dropNames))]
  x[,yName] <- log(x[,yName]+1e-6)
  names(x)[which(names(x)==yName)] <- sprintf('ln_%s',yName)
  if(naOmit)
    x <- na.omit(x)
  set.seed(seed)
  samp <- sample(seq_len(nrow(df.panel.reg)),sampSize)
  if(length(nrow(x))==0)
    return('zero non-NA data to plot')
  pairs(x[samp,],lower.panel=panel.smooth,
        diag.panel=panel.hist,upper.panel=panel.cor)
  if(output) 
    return(x)
}

pairsDetail <- function(x)
{
  pairs(x,lower.panel=panel.smooth,diag.panel=panel.hist,upper.panel=panel.cor)
}

###
# MONEY STRING (WITH UNITs ABBREVIATIONS) TO INTEGER
##
cleanMoney <- function(values)
{
  x <- gsub('[$]','',values)
  cleaned   <- ifelse(
    grepl('B',x), as.numeric(gsub('B','',x))*1e9, ifelse(
      grepl('M',x),as.numeric(gsub('M','',x))*1e6, ifelse(
        grepl('k',x),as.numeric(gsub('k','',x))*1e3, 
        as.numeric(x) 
      ) 
    ) 
  )
  return(cleaned)
}

###
# UNIX EPOCH TO DATE (posix.ct)
##
timestamp2date <- function(stamp, asDate=FALSE)
{
  posix <- as.POSIXct(stamp, origin="1970-01-01")
  if(asDate) 
    return(as.date(posix))
  return(posix)
}

###
# CONCATENATE LIST OF DFs into PANEL DATA DF
##
list2paneldf <- function(li)
{
  return(do.call('rbind', li))
}

###
# ADD ACQUISITION COUNTS PER PERIOD (from `count.df`)  TO COMPANY DATAFRAME `to.merge.df`
##
getAcqCountsByPeriod <- function(acq, start, end, to.merge.df, 
                                 count.name='company_name_unique', new.count.field='acq_count',
                                 pdName='acquired_at')
{
  if(!(count.name%in%names(acq))) {
    # warning(sprintf('count.name "%s" not in count.df names',count.name))
    stop(sprintf('count.name "%s" not in acq names',count.name))
  }
  ## PERIOD COUNTS
  df.sub <- acq[which( acq[,pdName]>=start & acq[,pdName]<end ), ]
  df.sub.count <- plyr::count(df.sub[,count.name])
  names(df.sub.count) <- c(count.name,new.count.field)
  # MERGE
  to.merge.df <- merge(to.merge.df,df.sub.count,by=count.name,all.x=T)
  fillIndices <- which( is.na(to.merge.df[,new.count.field]) | to.merge.df[,new.count.field]=='' )
  to.merge.df[fillIndices, new.count.field] <- 0
  return(to.merge.df)
}

###
# CREATE GRAPH FROM COMPETITIVE RELATIONS IN `comp` DATAFRAME
##
makeGraph <- function(comp,vertdf,name='company_name_unique', 
                      compName='competitor_name_unique', relationPdName='relation_created_at',
                      competitorFoundedName='competitor_founded_on',
                      competitorClosedName='competitor_closed_on',
                      vertAttrs=c('founded_at','founded_month','founded_quarter',
                                  'founded_year','acquired_at','company_closed_on','status',
                                  'category_list','country_code','state_code','city','market2') )
{
  el <- data.frame(source=comp[,name], 
                   target=comp[,compName],
                   relation_created_at=comp[,relationPdName], 
                   stringsAsFactors = F)
  ## build edge list
  if (competitorFoundedName %in% names(comp))
      el[,competitorFoundedName] <- comp[,competitorFoundedName]
  if (competitorClosedName %in% names(comp))
      el[,competitorClosedName] <- comp[,competitorClosedName]
  ## remove missing names
  el <- el[which(el$source!="" & el$target!=""), ]
  ## make vertex df
  verts <- data.frame(company_name_unique=unique(c(el$source,el$target)), stringsAsFactors = F)
  verts <- merge(x=verts,y=vertdf[,c(name,vertAttrs[vertAttrs%in%names(vertdf)])],
                 by=name,all.x=T,all.y=F)  
  ## make graph
  g <- igraph::graph.data.frame(d = el, directed = F, vertices = verts)
  E(g)$weight <- 1
  return(g)
}

###
# CREATE SUBGRAPH WITH VERTICES EXISTING 
#       & NOT REMOVED (closed,acquired) 
#       BEFORE `end` DATE
#       FROM  GRAPH BUILT WITH makeGraph()
##
makePdSubgraph <- function(g,acq,start,end,pdAttr='founded_at',closedPdAttr='closed_at')
{
  vertexAttrs <- names(igraph::vertex.attributes(g))
  keepVids <- c();   removeVids <- c()
  # g.sub <- igraph::delete_edges(graph=g,edges = E(g)[which(E(g)$relation_created_at>end )])
  ##  KEEP VERTICES (EDGES) IF   FOUNDED BEFORE `end`
  if(pdAttr %in% vertexAttrs){
    tmp <- igraph::get.vertex.attribute(g,pdAttr)
    pdVids <- V(g)[which( !is.na(tmp) & tmp < end)]
    keepVids <- c(keepVids, pdVids )
  }
  ##  REMOVE VERTICES (EDGES) IF   CLOSED BEFORE `end`
  if(closedPdAttr %in% vertexAttrs) {
    tmp <- igraph::get.vertex.attribute(g,closedPdAttr) 
    closedPdVids <- V(g)[which( !is.na(tmp) & tmp < end)]
    removeVids <- c(removeVids, closedPdVids )    
  }
  ## INDUCE SUBGRAPH
  uKeepVids <- unique(keepVids)
  uRemoveVids <- unique(removeVids)
  vids <- uKeepVids[which( !(uKeepVids %in% uRemoveVids) )]
  g.sub <- igraph::induced.subgraph(graph=g,vids=vids)
  return(g.sub)
}

###
# Remove edges between companies that weren't created yet
# and after being closed/acquired
##
filterPdEdges <- function(g,start,end,pdAttr='founded_at',acquiredPdAttr='acquired_at',
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
  g.sub <- igraph::delete_edges(graph=g,edges = removeEdgesUnique)
  return(g.sub)
}

###
# GET LARGEST CONNECTED COMPONENT FROM LIST OF DECOMPOSED SUBGRPAHS
##
getLcc <- function(g)
{
  comps <- igraph::decompose.graph(g)
  lcc <- comps[[which.max(sapply(comps,vcount))]]
  return(lcc)
}

###
# GET GLOBAL NETWORK VERTEX PROPERTIES 
##
getNetworkProperties <- function(lcc)
{
  # V(lcc)$alpha <- igraph::alpha.centrality(lcc)
  V(lcc)$authority <- igraph::authority.score(lcc)$vector
  V(lcc)$between <- igraph::betweenness(lcc,directed=F,normalized = F)
  V(lcc)$lnbetween <- log(V(lcc)$between)
  # V(lcc)$bonpow <- igraph::bonpow(lcc)
  V(lcc)$closeness <- igraph::closeness(lcc)
  V(lcc)$constraint <- igraph::constraint(lcc)
  V(lcc)$eccentricity <- igraph::eccentricity(lcc)
  V(lcc)$strength <- igraph::graph.strength(lcc)
  V(lcc)$eigen <- igraph::eigen_centrality(lcc)$vector
  # V(lcc)$edgecomm <- igraph:edge.betweenness.community(lcc)$membership
  V(lcc)$labelcomm <- igraph::label.propagation.community(lcc)$membership
  V(lcc)$walkcomm <- igraph::walktrap.community(lcc)$membership
  ## EGO NET PROPERTIES
  l.ego <- igraph::make_ego_graph(lcc,order = 1)
  V(lcc)$ego_size <- laply(l.ego,vcount)
  V(lcc)$ego_density <- laply(l.ego,function(x) ecount(x)/(vcount(x)*(vcount(x)-1)) )
  V(lcc)$ego_clustering <- laply(l.ego,function(x)igraph::transitivity(x,type='globalundirected'))
  #
  sprintf('diameter = %d; avg.path = %.3f',diameter(lcc),average.path.length(lcc))
  return(lcc)
}

###
# GET DATAFRAME OF NETWORK VERTEX ATTRIBUTES FROM IGRAPH OBJECT `lcc` (largest component)
##
getNetworkPropertiesDataframe <- function(lcc, name='company_name_unique',what='vertices', 
                                          date.fields.exclude=c('founded_at','founded_month',
                                                                'founded_quarter','founded_year')) 
{
  df.net <- igraph::get.data.frame(x=lcc,what=what)
  df.net[,name] <- row.names(df.net)
  df.net <- df.net[ ,which(!(names(df.net)%in%date.fields.exclude))]
  return(df.net)
}


