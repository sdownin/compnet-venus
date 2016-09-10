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
library(lattice, quietly = T)
library(latticeExtra, quietly = T)
library(ggplot2, quietly = T)
data_dir <- "C:/Users/sdowning/Google Drive/PhD/Dissertation/crunchbase"
img_dir  <- "C:/Users/sdowning/Google Drive/PhD/Dissertation/competition networks/envelopment"
#
if( !('gl1' %in% ls()) )
  load('netrisk_sms_full_pd_graph.RData')

source(file.path(getwd(),'R','comp_net_functions.R'))
source(file.path(getwd(),'R','netrisk_functions.R'))
source(file.path(getwd(),'R','cb_data_prep.R'))


lattice::trellis.par.set(strip.background=list(col="lightgrey"))

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
  






####################################################################
####################################################################
####################################################################
####################################################################
####################################################################
####################################################################
####################################################################



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
# 

#--------------------------------------------------------------------


###########################
#  Subset Competition Net List
###########################
periods <- seq(2001,2015,1)
gl1.bak <- gl1
gl1 <- gl1.bak[ which( names(gl1.bak)%in%periods)  ]
###########################

previous.base.plotting.ego.nets <- function() 
{
# name_i <- 'surveymonkey'
# 
# filename <- file.path(img_dir,sprintf('%s_k_period_size_avg_path.png',name_i))
# png(filename,height=7,width=11,units='in',res=250)
#   par(mfrow=c(2,4),mar=c(4,4,2,1))
#   for (k in 1:4) {
#     gs.sm <- sapply(gl1, function(g)igraph::make_ego_graph(g, k, V(g)[V(g)$name==name_i],mode = 'all'))
#     df.sm <- data.frame(Period=periods,k=k,v=sapply(gs.sm,vcount),e=sapply(gs.sm,ecount))
#     matplot(periods,df.sm,type='o',pch=16:17,ylim=c(5,2000),log='y',ylab='Comp Net Size (log scale)',main=sprintf('k=%d',k))
#     legend('topleft',legend=c('firms','competitive relations'),col=1:2,lty=1:2,pch=16:17)
#   }
#   for (k in 1:4) {
#     gs.sm <- sapply(gl1, function(g)igraph::make_ego_graph(g, k, V(g)[V(g)$name==name_i],mode = 'all'))
#     plot(names(gs.sm),sapply(gs.sm,function(g)igraph::average.path.length(g)/k),
#          ylab='Avg Path Length / k',xlab='Period',type='o',pch=16,main=sprintf('k=%d',k),ylim=c(.6,1.9))  
#   }
# dev.off() 
}




##  --------------------latice extra-------------------------------------
names <- c('google','microsoft','oracle','adobe-systems','amazon','ibm','facebook','sap',
           'dropbox','surveymonkey','medallia','clarabridge','netflix')

## PLOT LOOP
for (name_i in names) {
    # name_i <- 'dropbox'
    
    df.sm.tot <- data.frame();  df.av.tot <- data.frame()
    for (k in 1:4) {
      gs.sm <- sapply(gl1, function(g)igraph::make_ego_graph(g, k, V(g)[V(g)$name==name_i],mode = 'all'), simplify = T)
      ## size growth
      df.sm <- data.frame(Period=periods, k=sprintf('k=%d',k),
                          v=sapply(gs.sm, function(x)safeIgraphApply(x, 'vcount')),
                          e=sapply(gs.sm, function(x)safeIgraphApply(x, 'ecount')) )
      df.sm.tot <- rbind(df.sm.tot, df.sm)
      ## avg. path length
      df.av <- data.frame(Period=periods, k=sprintf('k=%d',k),
                          l=sapply(gs.sm, function(x)safeIgraphApply(x,'average.path.length')/k) )
      df.av.tot <- rbind(df.av.tot, df.av)
    }
    df.sm.melt <- reshape2::melt(data = df.sm.tot, id.vars=c('k','Period'), value.name="Network Size")
    df.av.melt <- reshape2::melt(data = df.av.tot, id.vars=c('k','Period'), value.name="Avg Path Length / k")
    
    ## SIZE GROWTH PLOT
    trel1 <- lattice::xyplot(`Network Size` ~ Period | k, groups=variable, data=df.sm.melt, 
                main=sprintf("%s: Competition Network Growth by Ego Network Order (k)", stringr::str_to_title(name_i)),
                type='b', pch=16:17, col=1:2, lty=1:2, layout=c(4,1), na.rm=FALSE,
                scales = list(y = list(log = 10)),
                par.settings=list(strip.background=list(col='lightgrey')),
                key=list(text=list(c("Firms","Competitive Relations")),
                       points=list(type='b',pch=16:17,col=1:2,lty=1:2),
                       space="top", columns=2)
                )
    ## AVG PATH LENGTH
    trel2 <- lattice::xyplot(`Avg Path Length / k` ~ Period | k, data=df.av.melt, 
                 main=sprintf("%s: Avg Competitive Distance Decrease with Ego Network Order (k)", stringr::str_to_title(name_i)),
                 type='b', pch=16, col=1, lty=1, layout=c(4,1), na.rm=FALSE,
                 scales = list(y = list(log = 10)),
                 par.settings=list(strip.background=list(col='lightgrey')),
                 key=list(text=list(c("Avg Path Length / Ego Network Order (k)")),
                          points=list(type='b',pch=16,col=1,lty=1),
                          space="top", columns=1)
    )
    ## SAVE PLOT SIZE
    filename <- file.path(img_dir,sprintf('lattice_%s_k_net_size_growth.png',name_i))
    lattice::trellis.device(device = 'png', filename=filename, height=4.5, width=9, units='in', res=200)
    print(trel1);  
    dev.off()  
    ## SAVE PLOT LENGTH
    filename <- file.path(img_dir,sprintf('lattice_%s_k_avg_path_len.png',name_i))
    lattice::trellis.device(device = 'png', filename=filename, height=4.5, width=9, units='in', res=200)
    print(trel2);  
    dev.off()  
 
}


##-------------------------------------------------------------------------

multi.prod <- c("cisco","google","microsoft","ibm","yahoo","oracle","hewlett-packard","intel","aol","apple",
                  "facebook","amazon","adobe-systems","nokia","dell",
                  "groupon","autodesk","salesforce","iac","quest-software","zayo-group",
                  "qualcomm","blackberry","berkshire-hathaway-corp","carlyle-group","intuit",
                  "symantec","broadcom","kkr","medtronic","vmware")


single.prod <- c('dropbox','surveymonkey','netflix','medallia')
k.vec <- c(3,3,3,4)

for (i in 1:length(single.prod)) {
  name_i <- single.prod[i]
  k <- k.vec[i]
  
  gs.sm <- sapply(gl1, function(g)igraph::make_ego_graph(g, k, V(g)[V(g)$name==name_i],mode = 'all'), simplify = T)
  
  ## plot k-th order ego net time slices
  filename <- file.path(img_dir,sprintf('ego_compnet_%s_k%d_timeslices.png',name_i,k))
  png(filename, height=8,width=14,units='in',res=350)
    par(mfrow=c(2,3),mar=c(.1,.1,.1,.1))
    for(i in (5+2*c(0,1,2,3,4,5)) ) {
      set.seed(1111)
      if (length(gs.sm[[i]])==0)
        g_i <- NA
      else if( class(gs.sm[[i]])=='igraph' )
        g_i <- gs.sm[[i]]
      else if ( class(gs.sm[[i]][[1]])=='igraph'  )
        g_i <- gs.sm[[i]][[1]]
      else if ( class(gs.sm[[i]][[1]][[1]])=='igraph'  )
        g_i <- gs.sm[[i]][[1]][[1]]
      else
        g_i <- NA
      ##
      if (all( !is.na(g_i) )) {
        plotCompNet(g_i, focal.firm = name_i, multi.prod=multi.prod, vertex.log.base = 1+exp(k)*.05, label.log.base = 1+exp(k)*.4 )
      } else {
        plot.new()
      }
      legend('topright',legend=sprintf('t=%d (k=%d)',periods[i], k),bty='n', cex=1.6)
    }
  dev.off()
}



## jaccard similarity
js <- similarity(g.sub, method='jaccard')
heatmap(js)

## cliques are markets
cl <- igraph::cliques(gs.sm[[1]], min = 2)
cl.cnt <- plyr::count(sapply(cl,length))
barplot(height = cl.cnt$freq, names.arg = cl.cnt$x)



## network distance
name_i <- 'surveymonkey'
gx <- gl1[[14]]
sp <- igraph::get.shortest.paths(gx, output = 'both', mode='all',
                                 from=V(gx)[V(gx)$name==name_i],
                                 to=V(gx)[V(gx)$name %in% c(multi.prod)]  )

sp <- igraph::get.shortest.paths(gx, to=V(gx)[V(gx)$name %in% c(multi.prod,name_i)] )

dis <- lapply(single.prod, function(name_i){
  sapply(gl1[3:5], function(gx){
    print(name_i)
    tmp <- igraph::shortest.paths(gx, 
                           v=V(gx)[V(gx)$name==name_i], 
                           to=V(gx)[V(gx)$name %in% c(multi.prod)] ) 
    row.names(tmp) <- multi.prod
    return(tmp)
  }, simplify=T)
})


############################################################################################
###_------------------------- NETWORK DISTANCE PLOTS -----------------------------------------
ls.dist <- list()
for (name_i in single.prod) {
  ls.dist[[name_i]] <- ldply(gl1, function(gx){
      igraph::shortest.paths(gx, 
                          v=V(gx)[V(gx)$name==name_i], 
                          to=V(gx)[V(gx)$name %in% c(multi.prod)] ) 
    })
}

df2 <- data.frame()
for (i in 1:length(ls.dist)) {
  ls.dist[[i]]$single.prod <- names(ls.dist)[i]
  df2 <- rbind(df2, ls.dist[[i]])  
}



l <- 1
df2[df2==Inf|df2==-Inf] <- NA

ls.dist.melt <- reshape2::melt(df2, id.vars=c('.id','single.prod'))

# lattice::dotplot(value ~ .id | single.prod , groups=variable, type='b', data=ls.dist.melt, na.rm=T)

trel3 <- lattice::xyplot(value ~ as.numeric(.id) | single.prod , groups=variable, 
                data=ls.dist.melt, na.rm=T,
                type='b',  pch=16, col=1:14, lty=1:6, layout=c(4,1), na.rm=T,
                xlab='Period', ylab='Network Distance',
                # scales = list(y = list(log = 10)),
                par.settings=list(strip.background=list(col='lightgrey')),
                key=list(text=list( as.character(unique(droplevels(na.omit(ls.dist.melt)$variable)))),
                         points=list(type='b',pch=16,col=1:14,lty=1:6),
                         space="top", columns=6)
                )

ls.dist.ply.avg <- plyr::ddply(ls.dist.melt, .variables = .(.id, single.prod), summarise,
            meandist = mean(value, na.rm = T))
igraph::leading.eigenvector.community(gs.sm[[1]])
trel4 <- lattice::xyplot(meandist ~ as.numeric(.id) | single.prod, data = ls.dist.ply.avg, 
                type=c('b','r'), lty=1, pch=16, col='black',size=5,
                mean = "Envelopment Risk: Avg Network Distance Over Time",
                xlab='Period', ylab='Mean Network Distance',
                par.settings=list(strip.background=list(col='lightgrey'))
                )

filename <- file.path(img_dir,sprintf('all_multiprod_net_dist_change_time.png'))
lattice::trellis.device(device = 'png', filename=filename, height=4.5, width=9, units='in', res=200)
print(trel3);  
dev.off()  
filename <- file.path(img_dir,sprintf('avg_net_dist_change_time.png'))
lattice::trellis.device(device = 'png', filename=filename, height=4.5, width=9, units='in', res=200)
print(trel4);  
dev.off()  
##################################################################


##----------------------- Community -----------------------
g <- gs.sm[[15]][[1]]
com <- list()
com$infomap<- igraph::infomap.community(g)
com$walktrap <- igraph::walktrap.community(g)
##com3 <- igraph::spinglass.community(g)
com$fastgreedy <- igraph::fastgreedy.community(g)
com$multilevel <- igraph::multilevel.community(g)
com$edgebet <- igraph::edge.betweenness.community(g)
com$labelprop <- igraph::label.propagation.community(g)
com$eigen <- igraph::leading.eigenvector.community(g)

par(mfrow=c(3,3))
for(i in 1:length(com)) {
  cnt <- plyr::count(com[[i]]$membership)
  barplot(height = cnt$freq, names.arg = cnt$x, main=names(com)[i], col='lightgray')
}

## Single Medallia plot-----------------------------------
name_i <- 'medallia'
membership <- com$multilevel$membership
k <- 4
gx <- gl1[[length(gl1)]]
g.sub <- igraph::make_ego_graph(gx, k, V(gx)[V(gx)$name==name_i],mode = 'all')[[1]]
filename <- file.path(img_dir,sprintf('single_ego_compnet_%s_k%d.png',name_i,k))
png(filename, height=5.5,width=6,units='in',res=350)
  par(mfrow=c(1,1), mar=c(.1,.1,0,0))
  plotCompNet(g.sub, focal.firm = name_i, membership = membership, vertex.log.base = 1+exp(k)*.05, label.log.base = 1+exp(k)*.4 )
  legend('topright',legend=sprintf('t=%d (k=%d)',periods[length(periods)], k),bty='n', cex=1.3)
dev.off()


## compare community membership algorithm loop 
par(mfrow=c(3,3), mar=c(.1,.1,0,0))
for(i in 1:length(com)) {
  plotCompNet(g.sub, focal.firm = name_i, membership = com[[i]]$membership, vertex.log.base = 1+exp(k)*.04, label.log.base = 1+exp(k)*.4 )
  legend('topright',legend=sprintf('%s',stringr::str_to_title(names(com)[i])),bty='n', cex=1.3)
}



## medallia community
c_i <- com$multilevel$membership[which(com$multilevel$names==name_i)]
com$multilevel$names[which(com$multilevel$membership==c_i)]

##
coms <- unique(com$multilevel$membership)
com.subg <- list()
for (c_i in coms) {
  com.names <- com$multilevel$names[which(com$multilevel$membership==c_i)]
  com.subg[[c_i]] <-  igraph::induced.subgraph(g, vids=V(g)[V(g)$name %in% com.names])
}
par(mfrow=c(3,3), mar=c(.1,.1,2,.1))
sapply(com.subg, function(x)plotCompNet(x,margins=c(0,0,2,.1), vertex.log.base = 1.5, label.log.base = 3,main=sprintf('%.2f',graph.density(x))))


## Noisy Product Markets 
n <- 10
par(mfrow=c(2,4), mar=c(2,0,4,0))
## ERDOS RENYI GAME
erl <- lapply(X=c(0, 1/3,2/3,1),FUN = function(x)igraph::erdos.renyi.game(n,x,directed = F))
sapply(erl,function(x)plotRingWithIsolates(x, main=sprintf('        p = %.2f\n    rho = %.2f\np+rho = %.2f',x$p,1-graph.density(x),x$p+1-graph.density(x))))
##  BARABASI GAME PREFERENTIAL ATTACHMENT
bgl <- lapply(X=c(0.01,1.5,2,4.5), function(x)as.undirected(barabasi.game(n, power=2.5, m=x)))
sapply(bgl,function(x)plotRingWithIsolates(x, main=sprintf('    rho = %.2f',1-graph.density(x))))


##_--------------------------------------------------------



##---------------------------------------------------------


##---------------------------------------------------------


###-------------------------------------------------------------------


dis.df <- data.frame(dropbox=dis[[l]]$dropbox,
                    surveymonkey=dis[[l]]$surveymonkey,
                    netflix=dis[[l]]$netflix,
                    medallia=dis[[l]]$medallia)
do.call(rbind, lapply(dis, data.frame, stringsAsFactors=FALSE))


for(i in (5+2*c(0,1,2,3,4,5)) ) {
  set.seed(1111)
  gs.sm <- sapply(gl1, function(g)igraph::make_ego_graph(g, k, V(g)[V(g)$name==name_i],mode = 'all'), simplify = T)
  g_i <- ifelse(class(gs.sm[[i]])=='igraph', gs.sm[[i]],  gs.sm[[i]][[1]] )

  print(g_i)
}


    g_i <- ifelse(class(gs.sm[[i]])=='igraph', gs.sm[[i]], 
                ifelse(class(gs.sm[[i]][[1]])=='igraph', gs.sm[[i]][[1]],
                       NA))



