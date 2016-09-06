##
#
#  Functions used in the analysis of 
#  Network Risk: Estimating the risk of envelopment
#
##

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
#  Get a Network object from an igraph object
##
getNetFromIgraph <- function(ig)
{
  g.tmp <- ig
  adjmat <- igraph::as_adjacency_matrix(g.tmp, type='both',names=T, sparse=F)
  net <- network::network(adjmat, vertex.attr = vertex.attributes(g.tmp), directed=F)
  for (edgeAttrName in names(igraph::edge.attributes(g.tmp))) {
    edgeAttr <- igraph::get.edge.attribute(g.tmp, edgeAttrName)
    net <- network::set.edge.attribute(net, attrname=edgeAttrName, value=edgeAttr)
  }
  return(net)
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
