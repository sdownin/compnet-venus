setwd("C:/Users/sdowning/Google Drive/PhD/Dissertation/competition networks/compnet")
.libPaths('C:/Users/sdowning/Documents/R/win-library/3.2')
library(igraph)
library(network)
library(networkDynamic)
library(tergm)
if( !('gl1' %in% ls()) )
  load('netrisk_sms_full_pd_graph.RData')
img_dir  <- "C:/Users/sdowning/Google Drive/PhD/Dissertation/competition networks/envelopment"

lcex <- 2
filename <- file.path(img_dir, 'envel_risk_modularity_compare_1.png')
png(filename, height=4, width=12, units='in', res=250)
  par(mfrow=c(1,3), mar=c(.1,3,.1,1))
  ##------------------- 0 ---------------------
  G <- graph.edgelist(matrix(c(1,2, 2,3, 1,3,
                               4,5, 4,6, 5,6, 6,7, 5,7),
                             ncol=2, byrow=T), directed=F)  #  5,7 missing 
  V(G)$name <- as.character(1:7)
  er <- envRisk(G, c(1,2), c(6))
  G <- envRisk(G, c(1,2), c(6), out.graph = T)
  
  cols <- rainbow(length(unique(V(G)$community)), s = .8, v = .8, alpha=.7)
  colslight <- rainbow(length(unique(V(G)$community)), s = .8, v = .8, alpha=.35)
  vertcols <- cols[V(G)$community]
  set.seed(1111)
  plot.igraph(G, layout=layout.kamada.kawai
              , vertex.label = round(V(G)$envrisk, 2)
              , vertex.size = 8
              , vertex.color = vertcols
              , vertex.label.color='black'
              , vertex.label.font = 2
              , vertex.label.family = 'sans'
              , edge.width = 3
              , edge.color = 'darkgrey'
              , mark.groups = list(V(er$npms[[1]])$name,V(er$npms[[2]])$name)
              , mark.col = colslight
  )
  legend('topleft', bty='n', cex=lcex, legend = sprintf('Q = %.1f', modularity(G, c(1,1,1,2,2,2,2))))
  
  #---------------------- 0 < Q < 1--------------
  G <- graph.edgelist(matrix(c(1,2, 2,3, 1,3,
                               2,4, 3,6,
                               4,5, 4,6, 5,6, 6,7, 5,7),
                             ncol=2, byrow=T), directed=F)  #  5,7 missing 
  V(G)$name <- as.character(1:7)
  er <- envRisk(G, c(1,2), c(6))
  G <- envRisk(G, c(1,2), c(6), out.graph = T)
  
  cols <- rainbow(length(unique(V(G)$community)), s = .8, v = .8, alpha=.7)
  colslight <- rainbow(length(unique(V(G)$community)), s = .8, v = .8, alpha=.35)
  vertcols <- cols[V(G)$community]
  set.seed(1111)
  plot.igraph(G, layout=layout.kamada.kawai
              , vertex.label = round(V(G)$envrisk, 2)
              , vertex.size = V(G)$envrisk * 35
              , vertex.color = vertcols
              , vertex.label.color='black'
              , vertex.label.font = 2
              , vertex.label.family = 'sans'
              , edge.width = 3
              , edge.color = 'darkgrey'
              , mark.groups = list(V(er$npms[[1]])$name,V(er$npms[[2]])$name)
              , mark.col = colslight
  )
  legend('topleft', bty='n',cex=lcex, legend = sprintf('Q = %.1f',modularity(G, V(G)$community)))
  
  
  #-------------------------------- Q= 1-----------------------
  G <- graph.edgelist(matrix(c(1,4, 1,5, 2,6, 2,7, 3,4, 3,5, 3,7),
                             ncol=2, byrow=T), directed=F)  #  5,7 missing 
  V(G)$name <- c('A','B','C','D','E','F','G')
  er <- envRisk(G, c(1,2), c(6))
  G <- envRisk(G, c(1,2), c(6), out.graph = T)
  
  cols <- rainbow(length(unique(V(G)$community)), s = .8, v = .8, alpha=.7)
  colslight <- rainbow(length(unique(V(G)$community)), s = .8, v = .8, alpha=.35)
  vertcols <- cols[c(1,1,1,2,2,2,2)]
  set.seed(1111)
  plot.igraph(G, layout=layout.circle
              , vertex.label = round(V(G)$envrisk, 2)
              # , vertex.label = V(G)$name
              , vertex.size = V(G)$envrisk * 35
              , vertex.color = vertcols
              , vertex.label.color='black'
              , vertex.label.font = 2
              , vertex.label.family = 'sans'
              , edge.width = 3
              , edge.color = 'darkgrey'
              , mark.groups = list(c(1,2,3),c(4,5,6,7))
              , mark.col = colslight
  )
  legend('topleft', bty='n', cex=lcex, legend = sprintf('Q = %.1f',modularity(G, c(1,1,1,2,2,2,2))))
#-------------------------------
dev.off()
  





############################################################################



name_i <- 'medallia'
k <- 3
afterlag <- 2
g.list <-  getEgoGraphList(gl1[6:length(gl1)], name =  name_i, order = k, safe = TRUE)
nl <- list()
for (i in afterlag:length(g.list)) {
  g <- g.list[[i]]
  #V(g)$type <- factor(ifelse(V(g)$name %in% multi.prod, 'MultiProd', 'SingleProd'))
  er <- envRisk(g, single.prod.names = single.prod, multi.prod.names = multi.prod)
  g <- er$g
  ##
  nl[[i]] <- getNetFromIgraph(g)
  nl[[i]] <- network::set.network.attribute(nl[[i]], 'envrisk', value = get.graph.attribute(g, 'envrisk'))
  founded_year_filled <- nl[[i]] %v% 'founded_year'
  founded_year_filled[is.na(founded_year_filled)] <- median(founded_year_filled, na.rm = T)
  nl[[i]] %v% 'founded_year_filled' <-  founded_year_filled
  nl[[i]] %v% 'type' <- ifelse(nl[[i]] %v% 'vertex.names' %in% multi.prod, 'MultiProd', 'SingleProd')
  rdf <-  envRisk(g.list[[i-1]], single.prod.names = single.prod, multi.prod.names = multi.prod, out.df = T)
  nl[[i]]  %v% 'envrisk_lag1' <- rdf$envrisk
}; nl <- nl[afterlag:length(g.list)]

nd <- networkDynamic::networkDynamic(network.list=nl, vertex.pid = 'vertex.names', create.TEAs = T,
                                     start = as.numeric(names(g.list)[afterlag]), 
                                     end= as.numeric(names(g.list)[length(g.list)]))

fn1 <- stergm(nd,
              formation= ~nodecov("envrisk_lag1") 
              + edges + nodematch("market2", diff=F) ,
              dissolution= ~ edges ,
              estimate="CMLE",times = 2015:2016)



f4 <- ergm(nl[[i]]  ~ nodecov("envrisk") + nodematch("type", diff=TRUE)
           + edges + nodematch("market2", diff=FALSE) 
           + absdiff("founded_year_filled")
)
f5 <- ergm(net ~ nodecov("envrisk") + nodecov("envrisk_lag1") 
           + nodematch("type", diff=TRUE)
           + edges + nodematch("market2", diff=FALSE) 
           + absdiff("founded_year_filled")
)
f5g <- ergm(net ~ nodecov("envrisk") + nodecov("envrisk_lag1") + nodecov("envrisk_lag2") 
            + nodematch("type", diff=TRUE) + gwesp(0.1, fixed=F)
            + edges + nodematch("market2", diff=FALSE) 
            + absdiff("founded_year_filled")
)
f6 <- ergm(net ~ nodecov("envrisk") + nodecov("envrisk_lag1") + nodecov("envrisk_lag2") 
           + nodematch("type", diff=TRUE)
           + edges + nodematch("market2", diff=FALSE) 
           + absdiff("founded_year_filled")
)
screenreg(list(f1,f2,f3,f4,f5,f5g,f6))

dev.off(); plot.new();par(mfrow=c(2,2))
plot(gof(f6))
