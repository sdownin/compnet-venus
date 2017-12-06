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
  from=c('a','a','b' , 'b','d','d','d','d','e','e','f','f','g','g','j' , 'u','u','u','u','t','t','s','r' , 'u','p','p','i' , 'o','n','n','l' , 'l','k'),                
  to  =c('b','c','c' , 'd','e','f','g','h','g','h','h','g','h','j','u' , 't','s','r','q','s','q','r','q' , 'p','i','o','o' , 'n','l','m','m' , 'k','b')      
), directed=F)


##=============================================================
##            Power Centrality
##-------------------------------------------------------------
## compute power centralities for range of beta values
betas <- c('-3'=-3,'-2'=-2,'-1.5'=-1.5,'-0.5'=-0.5,'0.5'=0.5,'1.5'=1.5,'2'=2,'3'=3)
df.pow <- as.data.frame(sapply(betas, function(x)power(g1, x)))
print(df.pow)

## check centrality
a <- 1.1962  ## approximated, computed implicitly so that [sum of squares of centralities] = N
b <- -1.5
Y <- igraph::as_adjacency_matrix(g1, sparse=F)
I <- diag(nrow(adjmat))
ones <- rep(1,nrow(adjmat))
pow.cent.manual <-  a * solve(I - b*Y) %*% (Y %*% ones)
print(pow.cent.manual)

## save CSV of power values
write.csv(df.pow,file = "power_centrality_comparison.csv",row.names = T)

## long-form dataframe
tmp <- cbind(node=row.names(df.pow),df.pow)
df.pow.l <- melt(tmp,id.vars=c('node'), value.name = 'centrality')

## plot heatmap of power values by beta
ggplot(df.pow.l, aes(node, variable)) + 
  geom_tile(aes(fill=centrality), colour='white') +
  scale_fill_gradient(low=rgb(0.9,0.2,0.2,.7),high=rgb(0.2,0.9,0.2,.7)) +
  ylab('beta value')

# ## check correlations with number of firms in d'th order neighborhood
# pws <- c(-3,-1.5,-.5,.5,1.5,3)
# data.frame(power=pws,
#            neighborhood_d1=sapply(pws,function(pwr)cor(neighborhood.size(g1,1),power(g1,pwr))),
#            neighborhood_d2=sapply(pws,function(pwr)cor(neighborhood.size(g1,2),power(g1,pwr))),
#            neighborhood_d3=sapply(pws,function(pwr)cor(neighborhood.size(g1,3),power(g1,pwr))),
#            neighborhood_d4=sapply(pws,function(pwr)cor(neighborhood.size(g1,4),power(g1,pwr)))
#            )


##=============================================================
##            Plot Network with Power Centrality
##-------------------------------------------------------------
## save plot to file
filename <- file.path(img_dir, sprintf('power_centrality_compare_7.png'))
png(filename,height=5.5,width=9,units='in',res=250)
    ##--------------- begin plot --------------------------------------
    par(mfrow=c(2,3))
    m <- ncol(df.pow)
    n <- nrow(df.pow)
    SEED <- 333
    font <- 17
    legcex <- 1 
    nodeNames <- c('i','j','k')
    layoutFunction <- layout.kamada.kawai
    ##---------------------------------- DISTANT NETWORK EFFECT ----------------------------------
    ###### ADVERSARIAL NETWORK 
    beta <- '-1.5'
    par(mar=c(0,0,1.2,0)); set.seed(SEED)
    plot.igraph(g1,  layout=layoutFunction,
                vertex.size = map(df.pow[,beta]), 
                vertex.label.cex = map(df.pow[,beta],min = .8,max = 1.6),
                main=expression('Adversarial Power Centrality ('*beta*' = -1.5)'),
                vertex.color=sapply(rownames(df.pow),function(name)ifelse(name %in% c('i','j','k'), 'pink', 'gray')), 
                vertex.label.color=sapply(rownames(df.pow),function(name)ifelse(name %in% c('i','j','k'), 'darkred', 'black')),
                vertex.label.font=font)
    ####### COLLABORATIVE NETWORK 
    par(mar=c(0,0,1.2,0)); set.seed(SEED)
    beta <- '1.5'
    plot.igraph(g1, layout=layoutFunction,
                vertex.size = map(df.pow[,beta]), 
                vertex.label.cex = map(df.pow[,beta],min = .8,max = 1.6),
                main=expression('Collaborative Power Centrality ('*beta*' = 1.5)'),
                vertex.color=sapply(rownames(df.pow),function(name)ifelse(name %in% nodeNames, 'pink', 'gray')), 
                vertex.label.color=sapply(rownames(df.pow),function(name)ifelse(name %in% nodeNames, 'darkred','black')),
                vertex.label.font=font)
    ######  BARPLOT 
    par(mar=c(4.2,4.2,2.5,2))
    colors <- RColorBrewer::brewer.pal(n = 8, name = 'Paired')[1:2]
    betaNames <- c('-1.5','1.5')
    data <- as.matrix(t(df.pow[nodeNames,betaNames]))
    midpoints <- barplot(height=data, 
                         beside = T, col=rep(colors,3), ylim=c(-1.7,4),
                         xlab='Node',ylab='Power Centrality')
    text(midpoints[1,], 
         sapply(data[1,],function(x)ifelse(x>0,x+.2,x-.2)),
         labels=round(data[1,],2))
    text(midpoints[2,],
         sapply(data[2,],function(x)ifelse(x>0,x+.2,x-.2)),
         labels=round(data[2,],2))
    legend('topright',legend=betaNames, title="Beta",fill=rep(colors,3), cex=legcex)
    ###---------------------------------- LOCAL NETWORK EFFECT ----------------------------------
    ###### ADVERSARIAL NETWORK 
    beta <- '-0.5'
    par(mar=c(0,0,1.2,0)); set.seed(SEED)
    plot.igraph(g1,  layout=layoutFunction,
                vertex.size = map(df.pow[,beta]), 
                vertex.label.cex = map(df.pow[,beta],min = .8,max = 1.6),
                main=expression('Adversarial Power Centrality ('*beta*' = -0.5)'),
                vertex.color=sapply(rownames(df.pow),function(name)ifelse(name %in% nodeNames, 'pink', 'gray')), 
                vertex.label.color=sapply(rownames(df.pow),function(name)ifelse(name %in% nodeNames, 'darkred', 'black')),
                vertex.label.font=font)
    ####### COLLABORATIVE NETWORK 
    par(mar=c(0,0,1.2,0)); set.seed(SEED)
    beta <- '0.5'
    plot.igraph(g1, layout=layoutFunction,
                vertex.size = map(df.pow[,beta]), 
                vertex.label.cex = map(df.pow[,beta],min = .8,max = 1.6),
                main=expression('Collaborative Power Centrality ('*beta*' = 0.5)'),
                vertex.color=sapply(rownames(df.pow),function(name)ifelse(name %in% nodeNames, 'pink', 'gray')), 
                vertex.label.color=sapply(rownames(df.pow),function(name)ifelse(name %in% nodeNames, 'darkred','black')),
                vertex.label.font=font)
    ######  BARPLOT 
    par(mar=c(4.2,4.2,2,2))
    colors <- RColorBrewer::brewer.pal(n = 8, name = 'Paired')[3:4]
    betaNames <- c('-0.5','0.5')
    data <- as.matrix(t(df.pow[nodeNames,betaNames]))
    midpoints <- barplot(height=data, 
            beside = T, col=rep(colors,3), ylim=c(-1.7,2),
            xlab='Node',ylab='Power Centrality')
    text(midpoints[1,], 
         sapply(data[1,],function(x)ifelse(x>0,x+.15,x-.15)),
         labels=round(data[1,],2))
    text(midpoints[2,],
         sapply(data[2,],function(x)ifelse(x>0,x+.15,x-.15)),
         labels=round(data[2,],2))
    legend('topright',legend=betaNames, title="Beta",fill=rep(colors,3), cex=legcex)
    ##--------------------------- end plot -------------------------------------
dev.off()
    
    


