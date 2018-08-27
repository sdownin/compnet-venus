library(igraph)
library(Matrix)

##
#
##
plot2 <- function(gx, layout=layout.random, vertex.size=15, focal.firm=NA, fam='sans', seed=11111, ...)
{
  vAttrs = names(igraph::vertex.attributes(gx))
  vcolors <- sapply(V(gx)$type, function(x)ifelse(x, "SkyBlue2", "gray"))
  lcolors <-  sapply(V(gx)$type, function(x)ifelse(x, "darkblue", "black"))
  if(!is.na(focal.firm)) {
    vcolors[V(gx)$name==focal.firm] <- 'darkblue'
    lcolors[V(gx)$name==focal.firm] <- 'white'
  }
  set.seed(seed)
  plot(gx, 
       layout = layout, 
       layout.par = list(), 
       labels = NULL, 
       label.color = lcolors, 
       label.font = NULL, 
       label.degree = -pi/4, 
       label.dist = 0, 
       vertex.label=sapply(1:vcount(gx), function(x) ifelse("name" %in% vAttrs, V(gx)$name[x], x)),
       vertex.color = vcolors, 
       vertex.shape = sapply(1:vcount(gx),function(x)ifelse(V(gx)$type[x], "circle", "square")),
       vertex.size = vertex.size, 
       vertex.frame.color="black", 
       vertex.label.family=fam,  # Font family of the label (e.g."Times", "Helvetica")
       vertex.label.font=1,  # Font: 1 plain, 2 bold, 3, italic, 4 bold italic, 5 symbol
       vertex.label.color=lcolors,
       edge.color = "darkgrey", 
       edge.width = E(gx)$weight,
       edge.labels = NA, 
       edge.lty=1, 
       margin=0,
       loop.angle=0, 
       axes = FALSE, 
       xlab = "", 
       ylab = "",
       xlim=c(-1,1), 
       ylim=c(-1,1), 
       ...)
}


##
# Bipartite Graph Acquisition
##
biAcq <- function(gi, acquirer, target, project=T)
{
  if (project) {
    gi <- bipartite.projection(gi, remove.type = F)$proj2
  }
  
  tdf <- as_data_frame(gi, what='vertices')
  tdf$before <- power_centrality(gi, exponent = -.1)
  tdf$after <- NA
  
  vnamemap <- names(V(gi))
  vmap <- as.integer(V(gi))
  
  revord <- which(vnamemap==target) < which(vnamemap==acquirer)
  comb.func <- ifelse(revord, 'last', 'first')
  
  vmap[vnamemap==target] <- vmap[vnamemap==acquirer]
  
  vertex.attr.comb <- list(type=ifelse(revord, 'last', 'first'),
                           name=ifelse(revord, 'last', 'first'))

  gi.2 <- igraph::contract.vertices(gi, vmap, vertex.attr.comb = vertex.attr.comb)
  gi.2 <- igraph::simplify(gi.2, remove.multiple = T, remove.loops = T, edge.attr.comb = list(weight='sum'))
  gi.2 <- igraph::induced.subgraph(gi.2, V(gi.2)[igraph::degree(gi.2)>0])
  
  tdf$after[tdf$name!=target] <- power_centrality(gi.2, exponent = -.2)
  tdf$delta <- tdf$after - tdf$before
  
  print(tdf)

  return(gi.2)
}

##
#
##
mapTo <- function(x,   #vector of degrees
                   minmax=c(9,20),  # vector of length 2: min and max
                   log=F           # logicial for log transform
) { 
  if (any(minmax < 0)) stop ("Negative output range is not allowed.\nPlease assign minmax argument as vector of 2 non-negative values (x>=0) and rerun function.")
  n <- length(x)
  dist <- max(x) - min(x)  #scalar
  #output
  M <- max(minmax)
  m <- min(minmax)
  range <- M - m   # interval to be mapped to
  scale <- (x-min(x)) / dist 
  
  if(log) {
    if(all(x>=1 | x==0)) {
      lx <- log(x)
      maxlx <- max(lx[lx<Inf & lx>-Inf])
      scale <- lx / maxlx
      y <- m + range*scale
      y[is.na(y)|is.nan(y)|y==-Inf|y==Inf] <- m
    } else {
      #augment x proportions to > 1 yielding values suitable for log transform
      scalepos <- (scale+1)/min(scale+1)
      #account for log transform while still proportional to elements of x 
      # by normalizing with the log of the maximum
      scale <- log(scalepos) / log(max(scalepos))
      y <- m + range*scale
    }
  } else {
    y <- m + range*scale
  }
  return(y)
}

centPow <- function(gx, beta=-0.2)
{
  return(power_centrality(gx, exponent = beta))
}

df.pow <- function(gx, betas=c(-.3,-.2,-.1,-.01,0))
{
  df <- data.frame(name=V(gx)$name)
  for (beta in betas) df[ , as.character(beta)] <- centPow(gx, beta)
  return(df)
}

##-----------------------------------------------------------------------------------




.par = par()

n1 <- 5
n2 <- 10
focal.firm <- as.character(4)
target.firm <- as.character(1)

## CREATE RANDOM BIPARTITE FIRM_MARKET
set.seed(11111)
gx=sample_bipartite(n1,n2,'gnp',.6)
V(gx)$name <- c(LETTERS[1:n1], 1:n2)
E(gx)$weight <- 1

## BIMODAL FIRM_MARKET PLOT
vshapes <- sapply(V(gx)$type,function(x)ifelse(x,'circle','square'))
par(mar=c(.1,.1,.1,.1), mfrow=c(1,2))
plot2(gx, 
     layout=layout.bipartite,
     vertex.shape=vshapes,
     vertex.size=18,
     focal.firm=focal.firm)
plot2(gx, 
      layout=layout.kamada.kawai,
      vertex.shape=vshapes,
      vertex.size=18,
      focal.firm=focal.firm)
par(mfrow=c(1,1))

## PLOT UNIMODAL FIRM_FIRM
gx.ff <- bipartite.projection(gx, remove.type = F)$proj2
vshapes <- sapply(V(gx.ff)$type,function(x)ifelse(x,'circle','square'))
plot2(gx.ff, 
      layout=layout.fruchterman.reingold,
      vertex.shape=vshapes,
      vertex.size=1.1*mapTo(centPow(gx.ff, beta = -.01)),
      focal.firm=focal.firm
)

## UNIMODAL FIRM_FIRM ADJACENCY MATRIX
print(as_adjacency_matrix(gx.ff, attr = 'weight', sparse = F))


## ACQUISITION UNIMODAL FIRM_FIRM
gx2 <- biAcq(gx, focal.firm, target.firm, project = T)
vshapes <- sapply(V(gx)$type,function(x)ifelse(x,'circle','square'))
plot2(gx2, 
      layout=layout.fruchterman.reingold,
      vertex.shape=vshapes,
      vertex.size=1.1*mapTo(centPow(gx2, beta = -.1)),
      focal.firm=focal.firm
)


as_adjacency_matrix(bipartite.projection(gx2)$proj2, attr = 'weight', sparse = F)

power_centrality(gx, exponent = -0.2, nodes = V(gx)$type)

biAcq(gi, '1', '2', T)



plot2(gx,vertex.shape=vshapes, layout=layout.kamada.kawai)
# plot(gx,vertex.shape=vshapes, layout=layout.fruchterman.reingold)

E(gx)$weight <- 1
gx.bp <- bipartite.projection(gx)
plot(gx.bp$proj1, edge.width=E(gx.bp$proj1)*.3, vertex.shape='square')
plot(gx.bp$proj2, edge.width=E(gx.bp$proj2)*.025, vertex.shape='circle', 
     layout=layout.fruchterman.reingold)

as_data_frame(gx, what='vertices')

biAcq(gi, '1', '2', T)


##--------------------------------------------------------------------------------------

gx=sample_bipartite(5,5,'gnp',.5)
vshapes <- sapply(V(gx)$type,function(x)ifelse(x,'circle','square'))
plot(gx, 
     layout=layout.bipartite,
     vertex.shape=vshapes)

as_data_frame(gx, what='vertices')


## BIPARTITE EDGE DATAFRAME
df <- data.frame(
  market = c('A','A','A','A', 'B','B','B', 'C','C','C'),
  firm =   c(1,  2,  3,  4,   3,  4,  5,   4,  5,  6)
)

## SPARSE INCIDENCE MATRIX
R <- spMatrix(nrow=length(unique(df$firm)),
              ncol=length(unique(df$market)),
              i = as.numeric(factor(df$firm)),
              j = as.numeric(factor(df$market)),
              x = rep(1, length(as.numeric(df$firm))) )
row.names(R) <- levels(factor(df$firm))
colnames(R) <- levels(factor(df$market))
R

## FIRM_FIRM MATRIX
Rrow <- tcrossprod(R)

## t(R) 
## MODE1::MARKETS
## MODE2: FIRMS
gi <- graph.incidence(t(R))
vshapes <- sapply(V(gi)$type, function(x)ifelse(x,'circle','square'))

plot(gi,  vertex.shape=vshapes)

plot(gi, 
     layout=layout.bipartite,
     vertex.shape=vshapes,
     vertex.size=power_centrality(gi, exponent = -0.2)*30
     )

df <- as_data_frame(gi, what='vertices')
for (i in 1:6) {
  for (j in 1:6) {
    if (i != j) {
      df[ ,paste0(i,j)] <- biAcq(gi, as.character(i), as.character(j))$delta
    }
  }
}

## WHICH MIN
apply(df[df$type==T,3:ncol(df)], 2, which.min)

biAcq(gi, '1', '2', T)

## FIRM-FIRM ADJACENCY
ga <- graph.adjacency(tcrossprod(R), diag = F, mode = 'undirected')
E(ga)$weight <- 1
ga <- simplify(ga, remove.multiple = T, remove.loops = T, edge.attr.comb = list(weight='sum'))
set.seed(2)
plot(ga, edge.width=E(ga)$weight^2)




