setwd("C:/Users/sdowning/Google Drive/PhD/Dissertation/competition networks/compnet")
.libPaths('C:/Users/sdowning/Documents/R/win-library/3.2')
library(igraph)

if( !('gl1' %in% ls()) )
  load('netrisk_sms_full_pd_graph.RData')



#
tg1 <- igraph::graph.data.frame(data.frame(
  from=c(1,2,3,4,5,6),
  to  =c(2,3,4,5,6,7)
), directed=F)
n <- 7
igmat <- matrix(rep(1,n*n),nrow=n)
igmat[upper.tri(igmat,diag=T)] <- 0
#
tg2 <- igraph::graph.adjacency(igmat, mode='undirected')
#
tg3 <- igraph::graph.data.frame(data.frame(
  from=c(2,1,3,4,3,5,7,7),
  to  =c(3,3,4,5,5,6,5,6)
), directed=F)


plot(tg1, vertex.size=4*distWeightReachByNode(tg1), vertex.label.cex=1.3)
plot(tg1, vertex.size=100*closeness(tg1), vertex.label.cex=1.3)
plot(tg1, vertex.size=100*closeness(tg1), vertex.label.cex=1.3)
plot(tg1, vertex.size=100*closeness(tg1), vertex.label.cex=1.3)

plot(tg1, vertex.size=10*distWeightReachByNode(tg1)*betweenness(tg1,directed = F, normalized = T),
     vertex.label.cex=1.3)
plot(tg2, vertex.size=10*distWeightReachByNode(tg2)*betweenness(tg2,directed = F, normalized = T),
     vertex.label.cex=1.3)

###############################################################################
f.cent <- 'df.cent.norm'

filename <- file.path(img_dir, sprintf('risk_metric_centrality_comparisons.png'))
png(filename,height=5,width=9,units='in',res=250)

par(mfrow=c(2,3))
m <- 6
SEED <- 11111
vertex.size <- 25; vertex.label.cex <- 1.3
colors <- rainbow(m,s=.7,v=.7)
legcex <- .8
#######
par(mar=c(0,0,0,0))
set.seed(SEED)
plot.igraph(tg2,vertex.color='gray',vertex.size=vertex.size, vertex.label.cex=vertex.label.cex)
#
par(mar=c(0,0,0,0))
set.seed(SEED)
plot.igraph(tg1,vertex.color='gray',vertex.size=vertex.size, vertex.label.cex=vertex.label.cex)
#
par(mar=c(0,0,0,0))
set.seed(SEED)
plot.igraph(tg3,vertex.color='gray',vertex.size=vertex.size, vertex.label.cex=vertex.label.cex)
######
par(mar=c(4.2,4.2,1,1))
df.c2 <- do.call(f.cent, list(g.tmp=tg2))
matplot(df.c2, type='b',pch=15:(12+m), col=colors,lty=1:m,
        xlab='Node',ylab='Risk Approx. (Centrality)')
legend('topleft',legend=names(df.c2),pch=15:(12+m),col=colors,lty=1:m, cex=legcex)
#
par(mar=c(4.2,4.2,1,1))
df.c1 <- do.call(f.cent, list(g.tmp=tg1))
matplot(df.c1, type='b',pch=15:(12+m), col=colors,lty=1:m,
        xlab='Node',ylab='Risk Approx. (Centrality)')
legend('topleft',legend=names(df.c1),pch=15:(12+m),col=colors,lty=1:m, cex=legcex)
#
par(mar=c(4.2,4.2,1,1))
df.c3 <- do.call(f.cent, list(g.tmp=tg3))
matplot(df.c3, type='b',pch=15:(12+m), col=colors,lty=1:m,
        xlab='Node',ylab='Risk Approx. (Centrality)')
legend('topleft',legend=names(df.c3),pch=15:(12+m),col=colors,lty=1:m, cex=legcex)
#######
dev.off()

#####################################################################################







#####################################################################################
#   Community Risk Measurement
#####################################################################################

G <- graph.edgelist(matrix(c(1,2, 2,3, 1,3,
                      2,4, 3,6,
                      4,5, 4,6, 4,7, 5,6, 6,7),
                   ncol=2, byrow=T), directed=F)  #  5,7 missing 
er <- envRisk(G, c(1,2), c(6))





