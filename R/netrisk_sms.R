setwd("C:/Users/sdowning/Google Drive/PhD/Dissertation/competition networks/compnet")
.libPaths('C:/Users/sdowning/Documents/R/win-library/3.2')
library(ergm)
library(tergm)
library(network)
library(igraph)
library(plyr)
library(dplyr)
library(stringr)
data_dir <- "C:/Users/sdowning/Google Drive/PhD/Dissertation/crunchbase"
#
source(file.path(getwd(),'R','comp_net_functions.R'))
source(file.path(getwd(),'R','cb_data_prep.R'))

# FUNCTIONS

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
getNetFromIgraph <- function(igraph)
{
  g.tmp <- igraph
  adjmat <- igraph::as_adjacency_matrix(g.tmp, type='both',names=T, sparse=F)
  net <- network::network(adjmat, vertex.attr = vertex.attributes(g.tmp), directed=F)
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
# END FUNCTIONs ---------------------
#View(el[grep(pattern = '([Oo]racle)', x=el[,2], ignore.case = T, perl = T),])


## Competitors 
file <- "C:\\Users\\sdowning\\Google Drive\\PhD\\Dissertation\\crunchbase\\cb_export_with_competitors_20160106_competitors_edgelist.csv"
el <- read.csv(file, header = T, sep = ',', quote = c("'",'"'), stringsAsFactors = F)
el <- df2lower(el)
# drop 4info
el <- el[which(el[,1]!='4info' & el[,2]!='4info'), ]
head(el)
el.all <- data.frame(company_name_unique=c(el[,1],el[,2]))
el.cnt <- plyr::count(el.all, vars='company_name_unique')
el.cnt <- el.cnt[ order(el.cnt$freq, decreasing = T), ]
View(head(el.cnt, 30))



## Acquisitions
file <- "C:\\Users\\sdowning\\Google Drive\\PhD\\Dissertation\\crunchbase\\cb_export_with_competitors_20160106_acquisitions_edgelist.csv"
acq <- read.csv(file, header = T, sep = ',', quote = c("'",'"'), stringsAsFactors = F)
acq <- df2lower(acq)
head(acq)
acq.cnt <- plyr::count(acq, vars='company_name_unique')
acq.cnt <- acq.cnt[ order(acq.cnt$freq, decreasing = T), ]
View(head(acq.cnt, 30))

## MERGE acq and comp
elacq <- merge(el.cnt, acq.cnt, by='company_name_unique', all=T)
names(elacq) <- c('company_name_unique', 'comp','acq')
elacq <- elacq[order(elacq$acq, decreasing=T),]
View(head(elacq, 100))


##--------------------- MAKE NETRISK SUBGRAPH ALGORITHM -------------------------
## 1. MAKE COMPETITOR GRAPH
g <- graph.data.frame(el, directed = F)
V(g)$degree <- degree(g)
deg <- degree(g) %>% sort('freq',decreasing=T)
net.size.full <- data.frame(vertices=vcount(g),edges=ecount(g))


## 2. SELECT n Multi-Product Firms  (serial acquirers)
top.acquirers <- c("cisco","google","microsoft","ibm","yahoo","oracle","hewlett-packard","intel","aol","apple",
                      "emc","facebook","amazon","ebay","twitter","adobe-systems","nokia","dell","electronicarts",
                      "riverside","groupon","autodesk","salesforce","thoma-bravo","iac","quest-software","zayo-group",
                      "zynga","qualcomm","blackberry","ca","berkshire-hathaway-corp","carlyle-group","intuit",
                      "symantec","broadcom","kkr","medtronic","dropbox","vmware")
# multi.prod <- c("cisco","google","microsoft","ibm","yahoo","oracle","facebook","amazon","adobe-systems","nokia",
#                "dell","groupon","salesforce","blackberry")
multi.prod <- c("google","microsoft","ibm","oracle","amazon")

## 4. GET k-th order competition competition network
##        union of kth-order ego networks for each n multi-product firms

l <- list()
for (k in 1:11) {
  mpen <- getMultiProdEgoNet(g, multi.prod, k=k)
  l[[k]] <- igraph::induced.subgraph(g, vids=mpen$vids)
}

tmp <- sapply(X = l, FUN = function(x)c(vcount(x),ecount(x)))
net.size <- data.frame(t(tmp))
names(net.size) <- c('vertices','edges')
net.size.pct <- data.frame(vertices=net.size$vertices/net.size.full$vertices, edges=net.size$edges/net.size.full$edges)
net.size.diff <- data.frame(vertices=diff(net.size$vertices), edges=diff(net.size$edges))
net.size.diff <- rbind(data.frame(vertices=NA,edges=NA), net.size.diff)
#plotCompNet(gs, multi.prod)

##------------------- PLOTS----------------------------------------------
# Plot comp net size cumulative distribution
png('comp_net_3by1_size_k_order_ego_net.png', width = 5, height=7, units = 'in', res = 200)
par(mfrow=c(3,1),mar=c(4.5,4.5,1.5,1))
  matplot(net.size, pch=16:17, type='o',xlab = expression(k^'th'~Order~Ego~Net),ylab=expression('Comp Net Size'))
  legend(x='topleft',legend=names(net.size),pch=16:17,col=1:2,lty=1:2)
  #
  matplot(net.size.pct, pch=16:17, type='o',xlab = expression(k^'th'~Order~Ego~Net),
          ylab=expression('Comp Net Proportion of Total'),  ylim=c(0,1))
  legend(x='topleft',legend=names(net.size),pch=16:17,col=1:2,lty=1:2)
  #
  matplot(net.size.diff, pch=16:17, type='o',xlab = expression(k^'th'~Order~Ego~Net),
          ylab=expression('Change in Comp Net Size by Order (k)'))
  legend(x='topright',legend=names(net.size),pch=16:17,col=1:2,lty=1:2)
dev.off()

# Plot comp net k=1, k=2
png('comp_net_3by1_ego_net_plot_k1_k2.png', width = 14, height=8, units = 'in', res = 200)
par(mfrow=c(1,2),mar=c(.1,.1,.1,.1))
  plotCompNet(l[[1]],multi.prod)
  legend(x='topright',legend='k = 1',bty='n')
  #
  plotCompNet(l[[2]],multi.prod, label.log.base = 20)
  legend(x='topright',legend='k = 2',bty='n')
dev.off()
## -------------------END PLOTS----------------------------------------

## Make Network Object
gt <- l[[1]]
adjmat <- igraph::as_adjacency_matrix(gt, type='both',names=T, sparse=F)
net <- network::network(adjmat, vertex.attr = vertex.attributes(gt), directed=F)

# Estimte ERGM models
f1 <- net ~ edges + nodecov("degree") + gwdegree(.5)
m1 <- ergm(formula = f1)
summary(m1)

f2 <- net ~ edges + nodecov("degree") + triangle()
m2 <- ergm(formula = f2)
summary(m2)

f3 <- net ~ edges + nodecov("degree") + dsp(1:3)
m3 <- ergm(formula = f3)
summary(m3)


#-----------------------------------------
#
#
#
#
#
#
#     CONTINUE HERE
#
#        TO DO:  1. comp_net_function.R::filterPdEdges()  
#                2. pd loop to make networkDynamic()
#                3. stergm()
#
#
#
#
#
#--------------------------------------------


##--------------- Make Period Subgraph -----------------------------------
yrpd <- 2
startYr <- 1999
endYr <- 2015
periods <- seq(startYr,endYr,yrpd)
company.name <- 'company_name_unique'
verbose <- TRUE
df.in <- co.acq

# subset kth order compnet
k <- 2
g.full <- makeGraph(comp = comp, vertdf = co)
mpen <- getMultiProdEgoNet(g.full, multi.prod, k=k)
g <- igraph::induced.subgraph(g.full, vids=mpen$vids)

## t main period loop  
gl <- list()
for(t in 2:length(periods)) {
  cat(sprintf('\nmaking period %s-%s:\n', periods[t-1],periods[t]))
  start <- periods[t-1]
  end <- periods[t]
  gl[[t-1]] <- filterPdEdges(g=g,start=start,end=end) 
}; names(gl) <- periods[-1]

# get Net Size Change (with plot)
net.size <- getNetSizeChange(gl)

plotCompNet(gl[[1]],multi.prod)

##------------------- plot period change ---------------------
png("comp_net_4_pd_subgraph_change_k2.png",width=10, height=12, units='in', res=200)
par(mfrow=c(2,2),mar=c(.2,.2,.2,.2))
  for (t in 1:length(gl)) {
    gx <- gl[[t]]
    yr <- names(gl)[t]
    plotCompNet(gx, multi.prod, vertex.log.base = 20, label.log.base = 20)
    legend(x='topright',legend=sprintf('Yr = %s',yr),bty='n')
  }
dev.off()
-------------------------------------------------------------

  
  
  
# Make Network Dynamic
net.list <- lapply(X = lcc.l, FUN = function(x)getNetFromIgraph(x))
networkDynamic(network.list = net.list)

stergm()
  
  
  
  
  
  
  
  