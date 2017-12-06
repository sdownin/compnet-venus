setwd("C:/Users/T430/Google Drive/PhD/Dissertation/competition networks/compnet2")
img_dir <-"C:/Users/T430/Google Drive/PhD/Dissertation/competition networks/compnet2/img"
#.libPaths('C:/Users/sdowning/Documents/R/win-library/3.2')
library(igraph)
library(RColorBrewer)
library(ggplot2)
library(reshape2)

##=============================================================
##              Helper Functions
##-------------------------------------------------------------
## wrapper for igraph power centrality function
power <- function(graph, beta){
  if (class(graph) != 'igraph') 
    stop("graph must be an igraph object.")
  return(igraph::power_centrality(graph = graph, 
                                  nodes = V(graph), 
                                  exponent = beta, 
                                  rescale = F,
                                  loops = F))
}

## map a vector of power centralities to values between [min,max] node sizes for plotting
map <- function(x, min=5, max=23) {
  xmin <- x[which.min(x)]
  xmax <- x[which.max(x)]
  maxdiff <- abs(xmax - xmin)
  scalediff <- max - min
  return(sapply(x, function(i) (scalediff*(i - xmin)/maxdiff) + min ))
}

##=============================================================
##              Make Competition Network
##-------------------------------------------------------------

## Make graph by edges
g1 <- igraph::graph.data.frame(data.frame(
  from=c('Bill','Bill','Bill','Fred','Fred','Fred','4','4','4', '8','8','8',   '13','13','13', 'Linda','Linda','Linda'), 
  to  =c('1',   '2',   '3',   '4',   '13',  '8',   '5','6','7', '9','10','11', '14','15','12', '16',   '17',   '18')      
), directed=F)

gbf <- igraph::graph.data.frame(data.frame(
  from=c('Bill', 'Bill','Bill','Bill','Fred','Fred','Fred','4','4','4', '8','8','8',   '13','13','13', 'Linda','Linda','Linda'), 
  to  =c('Fred', '1',   '2',   '3',   '4',   '13',  '8',   '5','6','7', '9','10','11', '14','15','12', '16',   '17',   '18')      
), directed=F)

gbl <- igraph::graph.data.frame(data.frame(
  from=c('Bill', 'Bill','Bill','Bill','Fred','Fred','Fred','4','4','4', '8','8','8',   '13','13','13', 'Linda','Linda','Linda'), 
  to  =c('Linda', '1',   '2',   '3',   '4',   '13',  '8',   '5','6','7', '9','10','11', '14','15','12', '16',   '17',   '18')      
), directed=F)

##=============================================================
##            Power Centrality
##-------------------------------------------------------------
## compute power centralities for range of beta values
# betas <- c('-3'=-3,'-2'=-2,'-1.5'=-1.5,'-0.5'=-0.5,'0.5'=0.5,'1.5'=1.5,'2'=2,'3'=3)
# betas <- c('-0.2'=-0.2,'0.2'=0.2)
betas <- c('-0.4'=-0.4,'-0.3'=-0.3,'-0.2'=-0.2,'-0.1'=-0.1,
           '0.1'=0.1,'0.2'=0.2,'0.3'=0.3,'0.4'=0.4)

df.pow <- as.data.frame(sapply(betas, function(x)power(g1, x)))
print(df.pow[which(rownames(df.pow) %in% c('Bill','Fred','Linda')), c('-0.1','0.1')])

df.pow.bf <- as.data.frame(sapply(betas, function(x)power(gbf, x)))
print(df.pow.bf[which(rownames(df.pow.bf) %in% c('Bill','Fred','Linda')), c('-0.1','0.1')])

df.pow.bl <- as.data.frame(sapply(betas, function(x)power(gbl, x)))
print(df.pow.bl[which(rownames(df.pow.bl) %in% c('Bill','Fred','Linda')), c('-0.1','0.1')])


##=============================================================
##            Visualize
##-------------------------------------------------------------
gx <- gbl
df.pow <- as.data.frame(sapply(betas, function(x)power(gx, x)))
print(df.pow[which(rownames(df.pow) %in% c('Bill','Fred','Linda')), c('-0.1','0.1')])
par(mfrow=c(1,1))
m <- ncol(df.pow)
n <- nrow(df.pow)
SEED <- 333
font <- 17
legcex <- 1
nodeNames <- c('Bill','Fred','Linda')
layoutFunction <- layout.kamada.kawai
beta <- '0.1'
plot.igraph(gx, layout=layoutFunction,
            vertex.size = map(df.pow[,beta]),
            vertex.label.cex = map(df.pow[,beta],min = .8,max = 1.6),
            main=expression('Adverse Power Centrality ('*beta*' = -0.1)'),
            vertex.color=sapply(rownames(df.pow),function(name)ifelse(name %in% nodeNames, 'pink', 'gray')),
            vertex.label.color=sapply(rownames(df.pow),function(name)ifelse(name %in% nodeNames, 'darkred','black')),
            vertex.label.font=font)



