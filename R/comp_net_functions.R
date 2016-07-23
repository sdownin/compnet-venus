##########################################################################################
#
# COMPETITION NETWORKS FUNCTION
#
# @author   Stephen Downing <sdowning.bm02g@nctu.edu.tw>
# @date     May 2015
#
##########################################################################################
.libPaths('C:/Users/sdowning/Documents/R/win-library/3.2')
library(igraph)

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

collapse <- function(x)
{
  return(paste(x,collapse='|'))
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
    if(attr == 'name' | attr == 'founded_year') 
      next
    if(attr %in% names(igraph::vertex.attributes(g))) {
      tryDatum <- na.omit(igraph::get.vertex.attribute(g,attr))[1]
      tryResult <- try(do.call('sum',list(tryDatum)),silent=T)      
    }
    if(inherits(tryResult,'try-error'))
      next
    vertAttrList[[attr]] <- 'sum'
  }
  
  vertAttrList <- c(vertAttrList, list(
    acquisitions=function(x)ifelse(length(x)==1,"",paste(x[which(x!=acquirer)],collapse='|'))
    #name= function(x)vertCombNames(x, acquirer, concat=FALSE)
  ))
  
  return(vertAttrList)
}

###
# UPDATE GRAPH FROM ONE ACQUISITION: 
#     ACQUIRER ABSORBS TIES TO TARGET'S COMPETITORS
##
getOneAcquisitionContractedGraph <- function(g,acquirer,target,attrNames=NA)
{
  #cat('\nvcount: ',vcount(g))
  if(any(length(acquirer)>1|length(target)>1))
    stop('acquirer and target must be individual nodes')

  if(!('acquisitions'%in% names(igraph::vertex.attributes(g))))
     V(g)$acquisitions <- V(g)$name %>% as.vector()
  
  ## GET LIST OF VERTEX ATTRIBUTES TO COMPUTE WHEN CONTRACTING THE ACQUISITION GRAPH
  vertAttrList <- getVertAttrList(g, acquirer, target, attrNames)
  
  ## GET MAPPING OF ACQUIRER TO TARGET INDICES FOR CONTRACTION (if both in current subgraph)
  vertNames <- V(g)$name  
  mapping <- V(g)$name %>% as.factor() %>% as.numeric()
  doContract  <- FALSE

  if( target%in%vertNames & acquirer%in%vertNames ){
    doContract <- TRUE
    targetIndex <- which(vertNames==target)
    acquirerIndex <- which(vertNames==acquirer)
    mapping[targetIndex] <- acquirerIndex
    ## MANUALLY TRACK VERTEX ATTRUBUTES FOR UPDATING
    mappedNames <- sapply(mapping, function(x)V(g)$name[x])
    if('founded_at' %in% names(vertex.attributes(g)))
      mappedFoundedAt <- sapply(mapping, function(x)V(g)$founded_at[x])
    if('founded_month' %in% names(vertex.attributes(g)))
      mappedFoundedMonth <- sapply(mapping, function(x)V(g)$founded_month[x])
    if('founded_quarter' %in% names(vertex.attributes(g)))
      mappedFoundedQuarter <- sapply(mapping, function(x)V(g)$founded_quarter[x])
    if('founded_year' %in% names(vertex.attributes(g)))
      mappedFoundedYear <- sapply(mapping, function(x)V(g)$founded_year[x])
    if('acquired_at' %in% names(vertex.attributes(g)))
      mappedAcquiredAt <- sapply(mapping, function(x)V(g)$acquired_at[x])
  }

  # cat('\n',mapping)
  ## DO GRAPH CONTRACTION (if any acquisition pair [acquirer, target] exists in current subgraph)
  if(doContract) {
    g <- igraph::contract( g, mapping = mapping,  vertex.attr.comb=vertAttrList )
    ## UPDATE VERTEX ATTRIBUTES MANUALLY
    V(g)$name <- mappedNames
    if(exists('mappedFoundedAt'))
      V(g)$founded_at <- mappedFoundedAt
    if(exists('mappedFoundedMonth'))
      V(g)$founded_month <- mappedFoundedMonth
    if(exists('mappedFoundedQuarter'))
      V(g)$founded_quarter <- mappedFoundedQuarter
    if(exists('mappedFoundedYear'))
      V(g)$founded_year <- mappedFoundedYear
    if(exists('mappedAcquiredAt'))
      V(g)$acquired_at <- mappedAcquiredAt

    g <- simplify(g, remove.multiple=T,remove.loops=T,edge.attr.comb=list(weight='sum' ))
    ##
    tryDeleteVertices <- try(delete.vertices(g, V(g)[which(degree(g)==0)]), silent=TRUE)
    if (!inherits(tryDeleteVertices, 'try-error'))
      g <- tryDeleteVertices
  }
  cat('\nvcount: ',vcount(g),'\n')
  return(g)
}

###
# LOOP THROUGH ACQUISITIONS AND RETURN FULLY CONTRACTED GRAPH
#
##
getFullAcquisitionContractedGraph <- function(g, acquirer, target, fullGraphList=FALSE)
{
  l <- list()
  l[[1]] <- g
  lenAcq <- length(acquirer)
  if (lenAcq != length(target))
    stop('acquirer and target must be same length')
  
  for(i in 1:lenAcq) {
    l[[i+1]] <- getOneAcquisitionContractedGraph( l[[i]]
      , acquirer = acquirer[i]
      , target = target[i]
    )
  }
  if(fullGraphList)
    return(l)
  else
    return(l[[length(l)-1]])
}

###
# EXAMPLE DYNAMIC COMPETITION NETWORK WITH ACQUISITIONS
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
  w.l <- getFullAcquisitionContractedGraph(w, acquirer = acqs$acquirer, target = acqs$target, fullGraphList = TRUE)
  
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
getAcqCountsByPeriod <- function(count.df, start, end, to.merge.df, 
                                 count.name='company_name_unique', new.count.field='acq_count',
                                 pdName='acquired_year')
{
  if(!(count.name%in%names(count.df))) {
    # warning(sprintf('count.name "%s" not in count.df names',count.name))
    stop(sprintf('count.name "%s" not in count.df names',count.name))
  }
  ## PERIOD COUNTS
  df.sub <- acq[which( acq[,pdName]>=start & acq[,pdName]<end ), ]
  df.sub.count <- count(df.sub[,count.name])
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
                      vertAttrs=c('founded_at','founded_month','founded_quarter',
                                  'founded_year','acquired_at','closed_at','age','funding_total_usd') )
{
  el <- data.frame(source=comp[,name], 
                   target=comp[,compName],
                   relation_created_at=comp[,relationPdName], 
                   stringsAsFactors = F)
  verts <- data.frame(company_name_unique=unique(c(el$source,el$target)), stringsAsFactors = F)
  verts <- merge(x=verts,y=vertdf[,c(name,vertAttrs[vertAttrs%in%names(vertdf)])],
                 by=name,all.x=T,all.y=F)  
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
makePdSubgraph <- function(g,acq,end,pdAttr='founded_at',closedPdAttr='closed_at')
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


