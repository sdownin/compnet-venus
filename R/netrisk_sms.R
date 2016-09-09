setwd("C:/Users/sdowning/Google Drive/PhD/Dissertation/competition networks/compnet")
.libPaths('C:/Users/sdowning/Documents/R/win-library/3.2')
library(statnet, quietly = T)
library(network, quietly = T)
library(ergm, quietly = T)
library(tergm, quietly = T)
library(btergm, quietly = T)
library(xergm, quietly = T)  ## includes rem, tnam, GERGM
library(texreg, quietly = T)
library(igraph, quietly = T)
library(plyr, quietly = T)
library(dplyr, quietly = T)
library(stringr, quietly = T)
library(ndtv, quietly = T)
library(visNetwork, quietly = T)
library(scatterplot3d, quietly = T)
data_dir <- "C:/Users/sdowning/Google Drive/PhD/Dissertation/crunchbase"
img_dir  <- "C:/Users/sdowning/Google Drive/PhD/Dissertation/competition networks/envelopment"
#
source(file.path(getwd(),'R','comp_net_functions.R'))
source(file.path(getwd(),'R','netrisk_functions.R'))
source(file.path(getwd(),'R','cb_data_prep.R'))


##  SELECT n Multi-Product Firms  (serial acquirers)
# top.acquirers <- c("cisco","google","microsoft","ibm","yahoo","oracle","hewlett-packard","intel","aol","apple",
#                    "emc","facebook","amazon","ebay","twitter","adobe-systems","nokia","dell","electronicarts",
#                    "riverside","groupon","autodesk","salesforce","thoma-bravo","iac","quest-software","zayo-group",
#                    "zynga","qualcomm","blackberry","ca","berkshire-hathaway-corp","carlyle-group","intuit",
#                    "symantec","broadcom","kkr","medtronic","dropbox","vmware")
# multi.prod <- c("cisco","google","microsoft","ibm","yahoo","oracle","facebook","amazon","adobe-systems","nokia",
#                "dell","groupon","salesforce","blackberry")


##------------------------------------------------------------------------------
## MAKE FULL COMP NET OF ALL RELATIONS IN DB 
##------------------------------------------------------------------------------
g.full <- makeGraph(comp = comp, vertdf = co)
## fill in missing founded_year values from founded_at year
V(g.full)$founded_year <- ifelse( (!is.na(V(g.full)$founded_year)|V(g.full)$founded_year==''), V(g.full)$founded_year, as.numeric(substr(V(g.full)$founded_at,1,4)))
## cut out confirmed dates >= 2016
g.full <- igraph::induced.subgraph(g.full, vids=V(g.full)[which(V(g.full)$founded_year <= 2015 
                                                                | is.na(V(g.full)$founded_year)
                                                                | V(g.full)$founded_year=='' ) ] )
g.full <- igraph::delete.edges(g.full, E(g.full)[which(E(g.full)$relation_created_at >= '2016-01-01')])
## change NA funding to 0
V(g.full)[is.na(V(g.full)$funding_total_usd)]$funding_total_usd <- 0
## reduce repeated edges
to.min <- c('relation_created_at','competitor_founded_on','competitor_closed_on','founded_at','founded_month','founded_quarter','founded_year','acquired_at')
to.collapse <- c('funding_total_usd','category_list','country_code','state_code','city')
edgeAttrCombList <- list(weight='sum')
gAttrList <- names(igraph::vertex.attributes(g.full))
for (i in 1:length(to.min)) {
  if (to.min[i] %in% gAttrList) {
    edgeAttrCombList[[ to.min[i] ]] <- 'min'
  }
}
for (i in 1:length(to.collapse))  {
  if (to.collapse[i] %in% gAttrList) {
    edgeAttrCombList[[ to.collapse[i] ]] <- function(x)paste(x, collapse = "||||")
  }
}
g.full <- igraph::simplify(g.full, remove.loops=T,remove.multiple=T, 
                          edge.attr.comb = edgeAttrCombList)

##-----------------------------------------------------------------------
## make regression predictors
##-----------------------------------------------------------------------
# V(g.full)$log_funding <- log( V(g.full)$funding_total_usd + 1 )
# V(g.full)$has_funding <- ifelse(V(g.full)$funding_total_usd>0,"YES","NO")
#_------------------------------------------------------------------------


# ##-----------------------------------------------------------------------
# ## example company ego plots by k order
# ##-----------------------------------------------------------------------
# #####
# ## Netflix
# png(file.path(img_dir,'netflix_ego_plot.png'), height=6, width=18, units='in',res=350)
# par(mfrow=c(1,3))
# for (k in 1:3) {
#   g.netflix <- igraph::make_ego_graph(graph = g.full,order=k,nodes=V(g.full)[V(g.full)$name=='netflix'])[[1]]
#   plotCompNet(g.netflix, multi.prod = 'netflix')
#   legend('topright',legend=sprintf('k=%s',k),bty='n')
# }
# dev.off()
# ## Medallia
# png(file.path(img_dir,'medallia_ego_plot.png'), height=12, width=12, units='in',res=350)
# par(mfrow=c(2,2))
# for (k in 1:4) {
#   g.medallia <- igraph::make_ego_graph(graph = g.full,order=k,nodes=V(g.full)[V(g.full)$name=='medallia'])[[1]]
#   plotCompNet(g.medallia, multi.prod = 'medallia')
#   legend('topright',legend=sprintf('k=%s',k),bty='n')
# }
# dev.off()
# ## surveymonkey
# png(file.path(img_dir,'surveymonkey_ego_plot.png'), height=12, width=12, units='in',res=350)
# par(mfrow=c(2,2))
# for (k in 1:4) {
#   g.sm <- igraph::make_ego_graph(graph = g.full,order=k,nodes=V(g.full)[V(g.full)$name=='surveymonkey'])[[1]]
#   plotCompNet(g.sm, multi.prod = 'surveymonkey',vertex.log.base = 2^k, label.log.base = 3*2^k)
#   legend('topright',legend=sprintf('k=%s',k),bty='n')
# }
# dev.off()
# 
# ## shortest paths
# gx <- g.sm
# igraph::get.shortest.paths(graph=gx, 
#                            from = V(gx)[V(gx)$name=='surveymonkey'], 
#                            to=V(gx)[V(gx)$name=='google'],output = 'both')
# #####


##################################################################################
#                        MAIN NETWORK COMPUTATION
##################################################################################
## starting values and params
yrpd <- 1
startYr <- 1996
endYr <- 2000
periods <- seq(startYr,endYr,yrpd)
company.name <- 'company_name_unique'
verbose <- TRUE
df.in <- co

##--------------------- MAKE NETRISK SUBGRAPH ALGORITHM -------------------------
## ## 1.  Choose Multi-Product firms
multi.prod <- c("google","microsoft") #,"ibm","oracle","amazon")
## ## 2. choose kth order for ego-net
k <- 1
## ## 3. get ego net from multi-product firms chosen above
mp.egonet <- getMultiProdEgoNet(g.full, multi.prod, k=k, include.competitor.egonet = FALSE)

V(g.full)$firm_type <- ifelse(V(g.full)$name%in%multi.prod,'MP',
                                        ifelse( V(g.full)$name%in%mp.egonet$names,'SP',
                                                'Other') )
## ## 4. get 2015 ego net igraph object
g <- igraph::induced.subgraph(g.full, vids=mp.egonet$vids)

print(g)

##--------------------- MAIN DYNAMIC NETWORK CREATION -----------------------------  
## make network object
net <- getNetFromIgraph(g, matrix.type = 'edgelist', vertex.pid.string='vertex.names') 
# net <- networkDynamic::activate.edges(x = net, onset = start, terminus = end, e = seq_len(length(net$mel)) )
# net <- networkDynamic::activate.vertices(x = net,onset = start,terminus = end, v = seq_len(length(net$gal$n)) )

## loop through periods and ACTIVATE edges that exist in the period
for(t in 2:length(periods)) {
  cat(sprintf('\nmaking period %s-%s:\n', periods[t-1],periods[t]))
  net <- setPdActivity(net=net, g=g, start=periods[t-1], end=periods[t]) 
}
## END MAIN LOOP 


# ## get Net Size Change (with plot)
# net.size <- getNetSizeChange(gl)

# ## plot comp net subgraphs
# plotCompNet(gl[[1]],multi.prod)

# ## plot filmstrip
# filmstrip( net, displaylabels=F, frames = 2)

## PLOT ACTIVE EDGES BY YEAR
ec <- c()
for ( t in 2:length(periods) ) {
  ec[t] <- sum(network.extract(net, onset=periods[t-1], terminus=periods[t], rule = 'any')[,]) / 2
};  plot(periods,ec,type='o',pch=16,log='',ylab="Active Competitive Relations")


## CMLE
## EGMME
## CMPLE
sfit1 <- stergm(net, 
                formation= ~edges + nodefactor("firm_type"), 
                dissolution= ~edges + nodefactor("firm_type"),
                estimate="CMLE", times = periods[c(1,length(periods))])

summary(sfit1)

sfit1b <- stergm(net, 
                formation= ~edges + nodefactor("firm_type"), 
                dissolution= ~density  + triangle,
                estimate="CMLE", times = periods[c(1,length(periods))])

summary(sfit1b)

sfit2 <- stergm(net, 
                formation= ~edges + nodefactor("firm_type") , 
                dissolution= ~density + triangle + nodefactor("has_funding"),  #localtriangle
                estimate="CMLE", times = periods[c(1,length(periods))])
summary(sfit2)

sfit5 <- stergm(net, 
                formation= ~edges + nodefactor("firm_type") , 
                dissolution= ~ density + triangle + nodefactor("has_funding"),  #localtriangle
                estimate="CMLE", times = periods[c(1,5)])
summary(sfit5)
gf5 <- gof(sfit5)
plot(gf5)

sfit6 <- stergm(net, 
                formation= ~edges + nodefactor("firm_type") , 
                dissolution= ~ triangle + nodefactor("has_funding"),  #localtriangle
                estimate="CMLE", times = periods[2:4])
summary(sfit6)
gf6 <- gof(sfit6)
plot(gf6)

sfit7 <- stergm(net, 
                formation= ~edges + nodefactor("firm_type") , 
                dissolution= ~edges + nodefactor("has_funding"),  #localtriangle
                estimate="CMLE", times = periods[2:4])
summary(sfit7)
gf7 <- gof(sfit7)
plot(gf7)

sfit8 <- stergm(net, 
                formation= ~edges + nodefactor("firm_type")  + gwesp(0, fixed=T), 
                dissolution= ~edges,  #localtriangle
                estimate="CMLE", times = periods[2:4])
summary(sfit8)
gf8 <- gof(sfit8)
plot(gf8)


png('test_heatmap_1.png',height=15,width=15,units='in',res=250)
  heatmap(net[,])
dev.off()


sfit9 <- stergm(ss, 
                formation= ~ kstar(3), 
                dissolution= ~edges,  #localtriangle
                estimate="CMLE", times = 10:12)
summary(sfit9)
gf9 <- gof(sfit9)
plot(gf9)


################################################################################
#                       // end main network computation
################################################################################


# ##------------------- PLOTS----------------------------------------------
# gk <- list()
# for (k in 1:12) {
#   mp.egonet <- getMultiProdEgoNet(g.full, multi.prod, k=k, include.competitor.egonet = T)
#   gk[[k]] <- igraph::induced.subgraph(g.full, vids=mp.egonet$vids)
# }
# net.size <- getNetSizeChange(gk, showPlot = F)
#
# # Plot comp net size cumulative distribution
# png('comp_net_3by1_size_k_order_ego_net.png', width = 5, height=7, units = 'in', res = 200)
# par(mfrow=c(3,1),mar=c(4.5,4.5,1.5,1))
# matplot(net.size, pch=16:17, type='o',xlab = expression(k^'th'~Order~Ego~Net),ylab=expression('Comp Net Size'))
# legend(x='topleft',legend=names(net.size),pch=16:17,col=1:2,lty=1:2)
# #
# matplot(net.size.pct, pch=16:17, type='o',xlab = expression(k^'th'~Order~Ego~Net),
#         ylab=expression('Comp Net Proportion of Total'),  ylim=c(0,1))
# legend(x='topleft',legend=names(net.size),pch=16:17,col=1:2,lty=1:2)
# #
# matplot(net.size.diff, pch=16:17, type='o',xlab = expression(k^'th'~Order~Ego~Net),
#         ylab=expression('Change in Comp Net Size by Order (k)'))
# legend(x='topright',legend=names(net.size),pch=16:17,col=1:2,lty=1:2)
# dev.off()
# 
# # Plot comp net k=1, k=2
# png('comp_net_3by1_ego_net_plot_k1_k2.png', width = 14, height=8, units = 'in', res = 200)
# par(mfrow=c(1,2),mar=c(.1,.1,.1,.1))
#   plotCompNet(l[[1]],multi.prod)
#   legend(x='topright',legend='k = 1',bty='n')
#   #
#   plotCompNet(l[[2]],multi.prod, label.log.base = 20)
#   legend(x='topright',legend='k = 2',bty='n')
# dev.off()
##

# ##------------------- PLOTTING PERIOD SUGRAPHS ---------------------
# png("comp_net_4_pd_subgraph_change_k2.png",width=10, height=12, units='in', res=200)
# par(mfrow=c(2,2),mar=c(.2,.2,.2,.2))
#   for (t in 1:length(gl)) {
#     gx <- gl[[t]]
#     yr <- names(gl)[t]
#     plotCompNet(gx, multi.prod, vertex.log.base = 20, label.log.base = 20)
#     legend(x='topright',legend=sprintf('Yr = %s',yr),bty='n')
#   }
# dev.off()
# ##-------------------------------------------------------------

  
  
  
# Make Network Dynamic
net.list <- lapply(X = gl, FUN = function(x)getNetFromIgraph(x))
nd <- networkDynamic(network.list = net.list, onsets = periods[1:(length(periods)-1)], termini = periods[-1])

# save.image(file = 'netrisk_sms_image_with_tergm_fit.RData')

formation <- ~ edges
dissolution <- ~ edges
sm1 <- stergm(net,
              formation=formation, 
              dissolution=dissolution, 
              estimate="CMLE", 
              times=2001:2002)
  

  
  



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

  


## ------------------- NETWORKDYNAMIC PLOTS -----------------------------
data("short.stergm.sim")
ss <- short.stergm.sim
ndtv::timeline(ss)
ndtv::timePrism(ss, at=seq(1,10,by=3))
ndtv::filmstrip(ss, frames=9, mfrow=c(3,3))
ndtv::render.d3movie(ss, filename = )
  






g1 <- igraph::barabasi.game(8, power = 1.1, m=2)
g2 <- igraph::barabasi.game(8, power = 3, m=4)
  


#-----------------------------------------------------------------
#
#   igraph MAIN LOOP
#
#-----------------------------------------------------------------
yrpd <- 2
startYr <- 1999
endYr <- 2015
periods <- seq(startYr,endYr,yrpd)
company.name <- 'company_name_unique'
verbose <- TRUE
#k <- 3
#-----------------------------------
#g.base <- igraph::make_ego_graph(g.full,order=k,nodes=V(g.full)[V(g.full)$name=='surveymonkey'])[[1]]
g.base <- g.full
gl <- list()
for(t in 2:length(periods)) {
  cat(sprintf('\nmaking period %s-%s:\n', periods[t-1],periods[t]))
  gl[[t]] <- makeIgraphPdSubgraphKeepNA(g.base, start=periods[t-1], end=periods[t],acq=acq,rou=rou,br=br)
}; names(gl) <- periods;  gl <- gl[2:length(gl)]

# save.image(file='netrisk_sms_full_pd_graph.RData')
# load('netrisk_sms_full_pd_graph.RData')

#--------------------------------------------------------------------



sapply(gl1, ecount)
sapply(gl1, vcount)
sapply(gl1, function(g)max(na.omit(V(g)$founded_at)))

periods <- seq(2000,2015,1)
gl1 <- gl1[ which( !(names(gl1)%in%c(2016)) ) ]

name_i <- 'surveymonkey'
filename <- file.path(img_dir,sprintf('%s_k_period_size_avg_path.png',name_i))
png(filename,height=7,width=11,units='in',res=250)
  par(mfrow=c(2,4),mar=c(4,4,2,1))
  for (k in 1:4) {
    gs.sm <- sapply(gl1, function(g)igraph::make_ego_graph(g, k, V(g)[V(g)$name==name_i],mode = 'all'))
    df.sm <- data.frame(v=sapply(gs.sm,vcount),e=sapply(gs.sm,ecount))
    matplot(periods,df.sm,type='o',pch=16:17,ylim=c(5,2000),log='y',ylab='Comp Net Size (log scale)',main=sprintf('k=%d',k))
    legend('topleft',legend=c('firms','competitive relations'),col=1:2,lty=1:2,pch=16:17)
  }
  for (k in 1:4) {
    gs.sm <- sapply(gl1, function(g)igraph::make_ego_graph(g, k, V(g)[V(g)$name==name_i],mode = 'all'))
    plot(names(gs.sm),sapply(gs.sm,function(g)igraph::average.path.length(g)/k),
         ylab='Avg Path Length / k',xlab='Period',type='o',pch=16,main=sprintf('k=%d',k),ylim=c(.6,1.9))  
  }
dev.off()

## plot k-th order ego net time slices
par(mfrow=c(2,3),mar=c(.1,.1,.1,.1))
for(i in (1+3*c(0,1,2,3,4,5)) ) {
  set.seed(1111)
  plotCompNet(gs.sm[[i]], multi.prod = c('surveymonkey'), vertex.log.base = 1+exp(k)/11, label.log.base = 1+exp(k)/1.6 )
  legend('topright',legend=sprintf('t=%d',periods[i]),bty='n')
}

sapply(gs.sm, function(g)V(g)[V(g)$name=='surveymonkey']$funding_total_usd)







