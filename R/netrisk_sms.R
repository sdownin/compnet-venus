setwd("C:/Users/sdowning/Google Drive/PhD/Dissertation/competition networks/compnet")
.libPaths('C:/Users/sdowning/Documents/R/win-library/3.2')
library(statnet, quietly = T)
library(network, quietly = T)
library(ergm, quietly = T)
library(tergm, quietly = T)
library(btergm, quietly = T)
library(hergm, quietly = T)
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
library(directlabels, quietly = T)
library(ggplot2, quietly = T)
data_dir <- "C:/Users/sdowning/Google Drive/PhD/Dissertation/crunchbase"
img_dir  <- "C:/Users/sdowning/Google Drive/PhD/Dissertation/competition networks/envelopment/img"
#
if( !('gl1' %in% ls()) )
  load('netrisk_sms_full_pd_graph.RData')
#  
#
source(file.path(getwd(),'R','comp_net_functions.R'))
source(file.path(getwd(),'R','netrisk_functions.R'))
source(file.path(getwd(),'R','cb_data_prep.R'))
#
par.default <- par()
lattice::trellis.par.set(strip.background=list(col="lightgrey"))
###########################################################################
## Firm type global variables
multi.prod <- c("cisco","google","microsoft","ibm","yahoo","oracle","hewlett-packard","intel",
                "aol","apple","facebook","amazon","adobe-systems","nokia","dell","sap","motorola",
                "groupon","autodesk","salesforce","iac","quest-software","zayo-group","sas",
                "qualcomm","blackberry","berkshire-hathaway-corp","carlyle-group","intuit",
                "symantec","broadcom","kkr","medtronic","vmware","sony","lg","motorola-mobility",
                "motorola-solutions")

single.prod <- c('netflix','medallia','dropbox','surveymonkey')  #
###########################################################################

##  SELECT n Multi-Product Firms  (serial acquirers)
# top.acquirers <- c("cisco","google","microsoft","ibm","yahoo","oracle","hewlett-packard","intel","aol","apple",
#                    "emc","facebook","amazon","ebay","twitter","adobe-systems","nokia","dell","electronicarts",
#                    "riverside","groupon","autodesk","salesforce","thoma-bravo","iac","quest-software","zayo-group",
#                    "zynga","qualcomm","blackberry","ca","berkshire-hathaway-corp","carlyle-group","intuit",
#                    "symantec","broadcom","kkr","medtronic","dropbox","vmware")
# multi.prod <- c("cisco","google","microsoft","ibm","yahoo","oracle","facebook","amazon","adobe-systems","nokia",
#                "dell","groupon","salesforce","blackberry")


#####################################################################################
## MAKE FULL COMP NET OF ALL RELATIONS IN DB 
#####################################################################################
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
## Make list of attributes functions to simplify duplicate edges
# to.min <- c('relation_created_at','competitor_founded_on','competitor_closed_on','founded_at','founded_month','founded_quarter','founded_year','acquired_at')
# to.collapse <- c('funding_total_usd','category_list','country_code','state_code','city')
# edgeAttrCombList <- list(weight='sum')
# gAttrList <- names(igraph::vertex.attributes(g.full))
# for (i in 1:length(to.min)) {
#   if (to.min[i] %in% gAttrList) {
#     edgeAttrCombList[[ to.min[i] ]] <- function(x){return(min(na.omit(x)))}
#   }
# }
# for (i in 1:length(to.collapse))  {
#   if (to.collapse[i] %in% gAttrList) {
#     edgeAttrCombList[[ to.collapse[i] ]] <- function(x)paste(na.omit(x), collapse = "||||")
#   }
# }
## SIMPLIFY
g.full <- igraph::simplify(g.full, remove.loops=T,remove.multiple=T, 
                          edge.attr.comb = list(weight='sum', 
                                                relation_created_at='min',
                                                competitor_founded_on='min',
                                                competitor_closed_on='min'))

##-----------------------------------------------------------------------
## make regression predictors
##-----------------------------------------------------------------------
# V(g.full)$log_funding <- log( V(g.full)$funding_total_usd + 1 )
# V(g.full)$has_funding <- ifelse(V(g.full)$funding_total_usd>0,"YES","NO")
#_------------------------------------------------------------------------



####################################################################
####################################################################
####################################################################



####################################################################
####################################################################
#-----------------------------------------------------------------
#
#                 Create Dynamic igraph MAIN LOOP
#
#-----------------------------------------------------------------
yrpd <- 1
startYr <- 2006
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
####################################################################
####################################################################


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



others <- c('oracle','adobe-systems','facebook','sap')


## ------------------------PLOT SIZE AND DISTANCE COMPARISONS_-----------------------------------

mp.names <- c('google','amazon','microsoft','sap')
sp.names <- c('dropbox','netflix','surveymonkey','medallia')

## for each pair in the list of pairs
for (i in 1:length(mp.names)) {
    df.sm.tot <- data.frame();  df.av.tot <- data.frame()
    sp.name_i <- sp.names[i]
    mp.name_i <- mp.names[i]
    # for each name in the pair
    for(name_i in c(sp.name_i,mp.name_i)) {
      for (k in 1:4) { ## consider up to 4th order ego network
        #gs.sm <- sapply(gl1, function(g)igraph::make_ego_graph(g, k, V(g)[V(g)$name==name_i],mode = 'all'), simplify = T)
        gs.sm <- getEgoGraphList(graph.list = gl1, name =  name_i, order = k, safe = TRUE)
        periods <- names(gs.sm)
        ## size growth
        df.sm <- data.frame(Period=periods, k=sprintf('k=%d',k),
                            v=sapply(gs.sm, function(x)safeIgraphApply(x, 'vcount')),
                            e=sapply(gs.sm, function(x)safeIgraphApply(x, 'ecount')),
                            type=ifelse(name_i%in%sp.names,str_to_title(sp.name_i),str_to_title(mp.name_i)))
        df.sm.tot <- rbind(df.sm.tot, df.sm)
        ## avg. path length
        df.av <- data.frame(Period=periods, k=sprintf('k=%d',k),
                            l=sapply(gs.sm, function(x)safeIgraphApply(x,'average.path.length')/k),
                            type=ifelse(name_i%in%sp.names,'Single-Product','Multi-Product') )
        df.av.tot <- rbind(df.av.tot, df.av)
      }    
    }
    df.sm.melt <- reshape2::melt(data = df.sm.tot, id.vars=c('k','Period','type'), value.name="Network Size")
    df.av.melt <- reshape2::melt(data = df.av.tot, id.vars=c('k','Period','type'), value.name="Avg Path Length / k")
    ##----------------------------- PLOTTING --------------------------
    ## SIZE GROWTH PLOT
    trel1 <- lattice::xyplot(`Network Size` ~ Period | type + k, groups=variable, data=df.sm.melt, 
                main=sprintf("%s vs %s: Competition Network Growth by Ego Network Order (k)", 
                             stringr::str_to_title(sp.name_i), stringr::str_to_title(mp.name_i)),
                type='b', pch=16:17, col=1:2, lty=1:2, layout=c(8,1), na.rm=FALSE,
                ylab= expression('Network Size (log10 scale)'),
                scales = list(y = list(log = 10)),
                par.settings=list(strip.background=list(col='lightgrey')),
                key=list(text=list(c("Firms","Competitive Relations")),
                       points=list(type='b',pch=16:17,col=1:2,lty=1:2),
                       space="top", columns=2)
                )
    ## AVG PATH LENGTH
    trel2 <- lattice::xyplot(`Avg Path Length / k` ~ Period | type + k, data=df.av.melt, 
                 main=sprintf("%s vs %s: Avg Competitive Distance Decrease with Ego Network Order (k)", 
                              stringr::str_to_title(sp.name_i), stringr::str_to_title(mp.name_i)),
                 type='b', pch=16, col=1, lty=1, layout=c(8,1), na.rm=FALSE,
                 # scales = list(y = list(log = 10)),
                 par.settings=list(strip.background=list(col='lightgrey')),
                 key=list(text=list(c("Avg Path Length / Ego Network Order (k)")),
                          points=list(type='b',pch=16,col=1,lty=1),
                          space="top", columns=1)
    )
    ## SAVE PLOT SIZE
    filename <- file.path(img_dir,sprintf('lattice_%s_vs_%s_k_net_size_growth.png',sp.name_i,mp.name_i))
    lattice::trellis.device(device = 'png', filename=filename, height=3.5, width=14, units='in', res=200)
    print(trel1);  
    dev.off()  
    ## SAVE PLOT LENGTH
    filename <- file.path(img_dir,sprintf('lattice_%s_vs_%s_k_avg_path_len.png',sp.name_i,mp.name_i))
    lattice::trellis.device(device = 'png', filename=filename, height=3.5, width=14, units='in', res=200)
    print(trel2);  
    dev.off()  
 
} ## -----------END SP-MP NETOWRK SIZE and DISTANCE COMPARISON -------------------------------




###############################################################################
##--------------------- PLOT COMPETITION NETWORK SLICES --------------------------------------
###############################################################################

k.vec <- c(3,4,3,3)
krisk <- 6
for (i in 1:length(single.prod)) {
  name_i <- single.prod[i]
  k <- k.vec[i]
  gs.sm <- getEgoGraphList(gl1, name_i, k, safe=TRUE)
  gs.sm.r <- getEgoGraphList(gl1, name_i, krisk, safe=TRUE)
  ## plot k-th order ego net time slices
  filename <- file.path(img_dir,sprintf('ego_compnet_%s_k%s_timeslices_r.png',name_i,k))
  png(filename, height=11,width=14,units='in',res=350)
  par(mfrow=c(3,3),mar=c(.1,.1,.1,.1))
  # for(i in (5+2*c(0,1,2,3,4,5)) ) {
  tstart <- which(as.numeric(names(gs.sm))==2007)
  tend <- which(as.numeric(names(gs.sm))==2015)
  yrs <- seq(tstart,tend,by=2)
  for(i in 1:length(gs.sm) ) {
    g_i <- gs.sm[[i]]; g_i_r <- gs.sm.r[[i]]
    g_i_r <- envRisk(g_i_r, single.prod.names=single.prod,multi.prod.names=multi.prod,
                   risk.center=F,risk.scale=F,out.graph=T)
    set.seed(1111)
    if (all( !is.na(g_i) ) & class(g_i)=='igraph') {
      if(ecount(g_i)>0)
        plotCompNet(g_i, focal.firm = name_i, multi.prod=multi.prod, vertex.log.base = 1+exp(k)*.1, label.log.base = 1+exp(k)*.5 )
      else
        plot.new()
    } else {
      plot.new()
    }
    r <- ifelse(class(g_i_r)=='igraph',V(g_i_r)[V(g_i_r)$name==name_i]$envrisk,NA)
    legend.text <- sprintf('t = %s\nr(k=6) = %s',names(gs.sm)[i],ifelse(!is.na(r),round(r,2),''))
    legend('topright',legend=legend.text,bty='n', cex=1.6)
  }
  dev.off()
}; dev.off()



## cliques are markets
cl <- igraph::cliques(gs.sm[[1]], min = 2)
cl.cnt <- plyr::count(sapply(cl,length))
barplot(height = cl.cnt$freq, names.arg = cl.cnt$x)


############################################################################
## network distance
############################################################################
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
      igraph::shortest.paths(gx, v=V(gx)[V(gx)$name==name_i], 
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
nColors <- length(unique(droplevels(na.omit(ls.dist.melt)$variable)))
# lattice::dotplot(value ~ .id | single.prod , groups=variable, type='b', data=ls.dist.melt, na.rm=T)

trel3 <- lattice::xyplot(value ~ as.numeric(.id) | single.prod , groups=variable, 
                data=ls.dist.melt, na.rm=T,
                type='b',  pch=16, col=rainbow(n = nColors,alpha=.75), lty=1:6, layout=c(4,1), na.rm=T,
                xlab='Period', ylab='Network Distance',
                # scales = list(y = list(log = 10)),
                main="Distance from Serial Acquirers & Multi-Product Firms",
                par.settings=list(strip.background=list(col='lightgrey')),
                key=list(text=list( as.character(unique(droplevels(na.omit(ls.dist.melt)$variable)))),
                         points=list(type='b',pch=16,col=rainbow(n = nColors,alpha=.75),lty=1:6),
                         space="top", columns=6)
                )

trel4bw <- lattice::bwplot(value ~ factor(.id) | single.prod, 
                         data=ls.dist.melt, na.rm=T,
                         type='b',  pch=16, col='steelblue', fill='gray', lty=1:6, layout=c(4,1), na.rm=T,
                         xlab='Period', ylab='Network Distance',
                         scales = list(x = list(rot = 90)),
                         par.settings=list(strip.background=list(col='lightgrey'))
                )

ls.dist.ply.avg <- plyr::ddply(ls.dist.melt, .variables = .(.id, single.prod), summarise,
            meandist = mean(value, na.rm = T))

trel4 <- lattice::xyplot(meandist ~ as.numeric(.id) | single.prod, data = ls.dist.ply.avg, 
                type=c('b','r'), lty=1, pch=16, col='black',size=5,
                mean = "Envelopment Risk: Avg Network Distance Over Time",
                xlab='Period', ylab='Mean Network Distance',
                par.settings=list(strip.background=list(col='lightgrey'))
                )

filename <- file.path(img_dir,sprintf('all_multiprod_net_dist_change_time_rainbow_col.png'))
lattice::trellis.device(device = 'png', filename=filename, height=5, width=12, units='in', res=200)
print(trel3);  
dev.off()  
filename <- file.path(img_dir,sprintf('bwplot_net_dist_change_time.png'))
lattice::trellis.device(device = 'png', filename=filename, height=4, width=14, units='in', res=200)
print(trel4bw);  
dev.off()  
filename <- file.path(img_dir,sprintf('avg_net_dist_change_time.png'))
lattice::trellis.device(device = 'png', filename=filename, height=4, width=14, units='in', res=200)
print(trel4);  
dev.off()  
##################################################################

##################################################################
##----------------------- Community -----------------------
##################################################################
name_i <- 'netflix'
k <- 3
#
g.list <- getEgoGraphList(gl1, name_i, k, safe=TRUE)
g <- g.list[[length(g.list)]]
com <- list()
com$infomap<- igraph::infomap.community(g)
com$walktrap <- igraph::walktrap.community(g)
##com3 <- igraph::spinglass.community(g)
com$fastgreedy <- igraph::fastgreedy.community(g)
com$multilevel <- igraph::multilevel.community(g)
com$edgebet <- igraph::edge.betweenness.community(g)
com$labelprop <- igraph::label.propagation.community(g)
com$eigen <- igraph::leading.eigenvector.community(g)

com.name.list <- c('Infomap','Walk Trap','Fast Greedy','Multi-level',
                   'Edge Betweenness','Label Propagation','Leading Eigenvector')

## compare NPM distribution BARPLOT
par(mfrow=c(3,3), mar=c(3,3,3,1))
for(i in 1:length(com)) {
  cnt <- plyr::count(com[[i]]$membership)
  barplot(height = cnt$freq, names.arg = cnt$x, main=com.name.list[i], col='lightgray')
}

## SINGLE FOCAL FIRM RAINBOW PLOT COMMUNITY MEMBERSHIP-----------------------------
membership <- com$multilevel$membership
gx <- gl1[[length(gl1)]]
g.sub <- igraph::make_ego_graph(gx, k, V(gx)[V(gx)$name==name_i],mode = 'all')[[1]]
filename <- file.path(img_dir,sprintf('single_ego_compnet_nofocalcolor_%s_k%s.png',name_i,k))
png(filename, height=5.5,width=6,units='in',res=350)
  par(mfrow=c(1,1), mar=c(.1,.1,0,0))
  plotCompNet(g.sub, focal.firm = name_i, focal.color=FALSE, membership = membership, vertex.log.base = 1+exp(k)*.8, label.log.base = 1+exp(k)*1.2 )
  legend('topright',legend=sprintf('t=%s (k=%s)',periods[length(periods)], k),bty='n', cex=1.3)
dev.off()


## compare community membership algorithm loop 
filename <- file.path(img_dir,sprintf('compare_community_membership_alorithms_%s_k%d.png',name_i,k))
png(filename, height=7, width=12, units='in', res=300)
par(mfrow=c(3,3), mar=c(.1,.1,0,0))
for(i in 1:length(com)) {
  plotCompNet(g.sub, focal.firm = name_i, focal.color=FALSE,  membership = com[[i]]$membership, vertex.log.base = 1+exp(k)*.05, label.log.base = 1+exp(k)*.4 )
  legend('topright',legend=sprintf('%s',stringr::str_to_title(names(com)[i])),bty='n', cex=1.3)
}
dev.off()

c_i <- com$multilevel$membership[which(com$multilevel$names==name_i)]
com$multilevel$names[which(com$multilevel$membership==c_i)]

## BREAK APART COMMUNITY STRUCTURE
coms <- unique(com$multilevel$membership)
ncoms <- length(coms)
mf.col = ceiling(sqrt(ncoms))
mf.row = floor(sqrt(ncoms))
if(mf.col*mf.row < ncoms)
    mf.row <- ceiling(sqrt(ncoms))
com.subg <- list()
for (c_i in coms) {
  com.names <- com$multilevel$names[which(com$multilevel$membership==c_i)]
  com.subg[[c_i]] <-  igraph::induced.subgraph(g, vids=V(g)[V(g)$name %in% com.names])
}
## PLOT NOISY PRODUCT MARKET DENSITY
filename <- file.path(img_dir,sprintf('noisy_prod_market_densities_%s_k%d.png',name_i,k))
png(filename, height=3.25*mf.row, width=3.5*mf.col, units='in', res=300)
par(mfrow=c(mf.row,mf.col), mar=c(.1,.1,2,.1))
sapply(seq_len(length(com.subg)), function(x){
  g <- com.subg[[x]]
  col_i <- rainbow(length(com.subg), alpha=.7)[x]
  plotCompNetOneColor(g, vertex.color = col_i, vertex.log.base = 1.5,label.log.base = 6,margins = c(.1,.1,2,.1),main=sprintf('Density = %.2f',graph.density(g)))
})
dev.off()



################ EXAMPLE RANDOM SAMPLE PLOTS ####################################
## Noisy Product Markets -- ALL ER BINOMIAL MODELS
n <- 10
filename <- file.path(img_dir,'noisy_prod_market_density_epsilon_erd_renyi.png')
png(filename,height=3, width=8, units='in',res=200)
  par(mfrow=c(1,4), mar=c(2,0,4,0))
  ## ERDOS RENYI GAME
  erl <- lapply(X=c(0, 1/3,2/3,1),FUN = function(x)igraph::erdos.renyi.game(n,x,directed = F))
  sapply(erl,function(x)plotRingWithIsolates(x, vert.size=30, main=sprintf('        p = %.2f\n    eps = %.2f\np+eps = %.2f',x$p,1-graph.density(x),x$p+1-graph.density(x))))
dev.off()

##  BARABASI GAME PREFERENTIAL ATTACHMENT
bgl <- lapply(X=c(0.01,1.5,2,4.5), function(x)as.undirected(barabasi.game(n, power=2.5, m=x)))
sapply(bgl,function(x)plotRingWithIsolates(x, main=sprintf('    eps = %.2f',1-graph.density(x))))



################### NEW SIMULATION EXAMPLE NPM ##############
n <- 10
SEED <- 11111
filename <- file.path(img_dir,'noisy_prod_market_density_epsilon_erd_renyi_barabasi_game_2.png')
png(filename,height=3, width=8, units='in',res=200)
  par(mfrow=c(1,4), mar=c(.1,0,.1,0))
  ## ERDOS RENYI GAME
  set.seed(SEED)
  erl <- lapply(X=c(.28,.6),FUN = function(x)igraph::erdos.renyi.game(n,x,directed = F))
  sapply(erl,function(x)plotPretty(x, vert.size=23, mar=c(1,.4,4,.4),
                                             main=sprintf('ER\nepsilon = %.2f',1-graph.density(x))))
  ##  BARABASI GAME PREFERENTIAL ATTACHMENT
  set.seed(SEED)
  bgl <- lapply(X=c(1.5,3), function(x)as.undirected(barabasi.game(n, power=3, m=x)))
  sapply(bgl,function(x)plotPretty(x, vert.size=23, mar=c(1,.4,4,.4), main=sprintf('AB\nepsilon = %.2f',1-graph.density(x))))
dev.off()

##_--------------------------------------------------------


################################################################
##   ENVELOPMENT RISK
################################################################

name_i <- 'medallia'
krisk <- 6
kplot <- 1
tmp.list <- getEgoGraphList(gl1, name_i, kplot, safe=T)
plotnames <- V(tmp.list[[length(tmp.list)]])$name
g.list <- getEgoGraphList(gl1, name_i, krisk, safe=T)
# quick look
print(vcount(g.list[[length(g.list)]]))
. <- sapply(1:12, function(x){
  . <-  make_ego_graph(g.full,x,nodes = V(g.full)[V(g.full)$name==name_i])
  return(c(e=ecount(.[[1]]),v=vcount(.[[1]])))
}); matplot(rbind(c(e=NA,v=NA),diff(t(.))), type='b',pch=16:17, main=str_to_title(name_i), xlab='k', ylab='Size Diff')
abline(v=6,lty=3); legend('topright',legend=c('relations','firms'),col=1:2,lty=1:2,pch=16:17)
###
## YEARLY LIST OF RISK DATAFRAME
risk.std <- FALSE
r.list <- lapply(g.list,function(g){
  envRisk(g, single.prod.names = single.prod, multi.prod.names = multi.prod,
          risk.center=risk.std, risk.scale=risk.std, out.df = TRUE)
})
## MERGE COMBINED LIST OF RISK
df.r.m <- data.frame(name=NA)
for (i in 1:length(r.list)) {
  tmp <- r.list[[i]]
  if(any( !is.na(tmp) )) {
    names(tmp) <- c(names(tmp)[1], names(r.list)[i])
    df.r.m <- merge(df.r.m, tmp, by='name', all=T )    
  }
}
## ADD FACTOR VARS
df.r.m$type <- ifelse(df.r.m$name%in%multi.prod, 'Multi-Product','Single-Product')
df.tmp <- df.r.m[,!(names(df.r.m)%in%c('name','type','risk'))]
#
ncols <- ncol(df.tmp)
split <- ceiling(ncols/2)
risk.check <- rowMeans(df.tmp[,(split+1):ncols], na.rm = T) - rowMeans(df.tmp[,1:split], na.rm = T) > 0
#
# lastyear <- names(g.list)[length(g.list)]
# risk.check <- rowMeans(df.tmp[,(ncol(df.tmp)-1):ncol(df.tmp)]) > 0
#
df.r.m$risk <- ifelse( risk.check, 'Increasing', 'Decreasing')
df.r.melt <- reshape2::melt(data = df.r.m, id.vars=c('name','type','risk'))

## ------------PLOTTING--------------------------------
X <- subset(df.r.melt, subset=( name %in% plotnames & df.r.melt$type=='Single-Product' ))
# X <- df.r.melt
cols <- rainbow(length(unique(X[!is.na(X$risk),'name'])), s=.7, v=.7, alpha=.95)
trel.r1 <- direct.label( xyplot(
  value ~ variable | risk, groups= name, data=X,
  type='b', pch=16, na.rm=T, col=cols,
  ylab='Envelopement Risk', xlab='Period',
  main=sprintf('%s\'s %s-Order Network (showing %s of %s firms)',str_to_title(name_i),th(krisk),length(cols),vcount(g.list[[length(g.list)]]) ),
  par.settings=list(strip.background=list(col=c('lightgrey','darkgrey'))),
  panel=function(x,y,groups,...){
    panel.xyplot(x,y,groups,...);
    panel.lines(x=0:(length(unique(X$variable))*2),y=0,col='black',lty=2);
  }), 'last.polygons'); #print(trel.r1)

filename <- file.path(img_dir, sprintf('risk_timeseries_lattice_%s_k%s_k%s_risk_%s_2.png',name_i,krisk,kplot,risk.std))
lattice::trellis.device(device='png', filename=filename, height=5, width=10, units='in', res=200)
print(trel.r1)
dev.off()


bwplot(value ~ variable | risk, groups= name, data=X,
       panel=function(x,y,groups,...){
         panel.bwplot(x,y,...);
         panel.lines(x=0:(length(unique(X$variable))*2),y=0,lty=2,col='black');
       })
##############################################################################
#                   end envelopment risk computation
##############################################################################


##############################################################################
#----------------------------- ERGM (1 year) ---------------------------------
##############################################################################

name_i <- 'netflix'
fl <- list()
counter <- 1
kvec <- 2:5
for (k in kvec) {
  g.list <-  getEgoGraphList(graph.list = gl1, name =  name_i, order = k, safe = TRUE)
  g <- g.list[[length(g.list)]]
  g.lag1 <- g.list[[length(g.list)-1]]
  # g.lag2 <- g.list[[length(g.list)-2]]
  er <- envRisk(g, single.prod.names = single.prod, multi.prod.names = multi.prod)
  g <- er$g
  ## ------------- Add node attrs to network object ------------------------------
  net <- getNetFromIgraph(g)
  net <- network::set.network.attribute(net, 'envrisk', value = get.graph.attribute(g, 'envrisk'))
  age_filled <- net %v% 'age'
  age_filled[is.na(age_filled)] <- median(age_filled, na.rm = T)
  net %v% 'age_filled' <-  age_filled
  net %v% 'type' <- ifelse(net%v%'vertex.names' %in% multi.prod, 'MultiProd', 'SingleProd')
  rdf <-  envRisk(g.lag1, single.prod.names = single.prod, multi.prod.names = multi.prod, out.df = T)
  net %v% 'envrisk_lag1' <- rdf$envrisk
  # rdf <-  envRisk(g.lag2, single.prod.names = single.prod, multi.prod.names = multi.prod, out.df = T)
  # net %v% 'envrisk_lag2' <- rdf$envrisk
  ## --------------- fit model (depending on number of firm type) ----------------------------
  if ( sum(net%v%'type'=='MultiProd') < 5 ) {
    fit <- ergm(net ~ nodecov("envrisk") 
      + nodecov("envrisk_lag1") 
      # + nodecov("envrisk_lag2") 
      + edges + gwesp(0, fixed=T) # + degree(c(2,4,8))
      + nodematch("market2", diff=FALSE) + absdiff("age_filled")
    )
  } else {
    fit <- ergm(net ~ nodecov("envrisk") 
      + nodecov("envrisk_lag1") 
      # + nodecov("envrisk_lag2") 
      + nodematch("type", diff=TRUE)
      + edges + gwesp(0, fixed=T) # + degree(c(2,4,8))
      + nodematch("market2", diff=FALSE) + absdiff("age_filled")
    )
  }
  fl[[counter]] <- fit
  counter <- counter + 1 
}; names(fl) <- sapply(kvec,function(k)sprintf('k=%s(N=%s)',k,vcount(g.list[[k]])))

## compare models
screenreg(fl)
## plot model diagnostics (simulate nets from model results; compare median to observed)
#dev.off();plot.new();par(mfrow=c(2,2));plot(gof(fl[[length(fl)]],GOF = ~ model + degree + espartners + distance))
dev.off();plot.new();par(mfrow=c(2,2));plot(gof(fl[[length(fl)]],GOF = ~ degree + espartners + distance))

## interpret model results
for (i in 1:length(fl))
    explainLogit(getCoef(fl[[i]]))

screenreg(fl, custom.coef.names = c('Env.Risk',
                                'Env.Risk:Lag(1)',
                                'Edges',
                                'GWESP(fixed)',
                                'Homophily(Region)',
                                'Age Diff',
                                'Homophily(Multi-Prod)',
                                'Homophily(Single-Prod)'))
htmlreg(fl, custom.coef.names = c('Env.Risk',
                                  'Env.Risk:Lag(1)',
                                  'Edges',
                                  'GWESP(fixed)',
                                  'Homophily(Region)',
                                  'Age Diff',
                                  'Homophily(Multi-Prod)',
                                  'Homophily(Single-Prod)'))

## SAVE EFFECT PLOTS
filename <- file.path(img_dir,sprintf('%s_ergm_enrisk_effect_probability_plots.png',name_i))
png(filename, height=8, width=8, units='in',res=200)
par(mfrow=c(2,1), mar=c(4.2,4.2,3.5,1))
b <- getCoef(fl[[2]])
predictors <- c('envrisk','envrisk_lag1')  ## have to be in order of predictors in model
predictor.names <- c('Env. Risk', 'Env. Risk Lag 1')
for (i in 1:length(predictors)) {
  predictor <- predictors[i]
  predictor.name <- predictor.names[i]
  #--------------- BUILD MEDIAN OBSERVATION ------------
  net <- net
  mu <- list(); var.check <- c()
  net.g <- getIgraphFromNet(net)
  for (var in removeErgmTerms(names(b),c('MultiProd','SingleProd'))) {
    if (var != "" &  !(var %in% var.check)) {
      x <- na.omit( net %v% var )
      if (is.character(x)) {
        cnt <- plyr::count(x)
        mu[[var]] <- as.character(cnt[which.max(cnt$freq),'x'])      
      } else {
        mu[[var]] <- median(x, na.rm = T)      
      }
    }  
    var.check <- c(var.check, var)
  }
  mu$edges <- median(igraph::degree(net.g), na.rm = T)
  medtri <- median(igraph::count_triangles(net.g), na.rm=T)
  mu$gwesp <- ifelse(medtri>0,medtri,max(1,mean(igraph::count_triangles(net.g), na.rm=T)/2))
  ###----------------------------------------------------------
  X1 <- c(mu$envrisk, mu$envrisk_lag1,        
         0, 1, # no multi-prod, yes single-prod
         mu$edges, mu$gwesp,
         1, # same market
         2 # age diff median
  )
  b <- getCoef(fl[[4]])
  samp <- net %v% predictor
  r.range <- range(samp)
  r <- seq(r.range[1],r.range[2],length.out = 100)
  X2 <- t(sapply(r, function(x)c(x,X1[-i])))
  Xb <- X2%*%b
  eff.med <- 1 / (1 + exp( - c(median(samp), X1[-i]) %*% b ) )
  eff.med.p1 <- 1 / (1 + exp( - c(median(samp)+sd(samp), X1[-i]) %*% b ) )
  eff.diff <- eff.med.p1 - eff.med
  eff.pct <- (eff.diff / eff.med) * 100
  effect <- median(samp) + sd(samp)
  plot(r,1/(1+exp(-Xb)), main=sprintf('Effect on Relation Probability of 1 StDev Increase above Median %s:\n%e (+%.1f%s)',
                                      predictor.names[i],eff.diff,eff.pct,'%'), 
       type='l', lty=1,col='darkred',lwd=2, ylab='Probability',xlab='Envelopment Risk\n(dotted lines mark sample quantiles 0,25,50,75,100)')
  abline(v=quantile(samp, probs = c(0,.25,.5,.75,1)), lty=2)  
}
dev.off()





# b <- getCoef(fl[[4]],'nodecov.envrisk_lag1')
# samp <- net %v% 'envrisk_lag1'
# r.range <- range(samp, na.rm = T)
# #x <- seq(min(r.range),max(r.range), length.out = 100)
# x <- seq(-1,1,length.out = 100)
# plot(x,1/(1+exp(-b*x)), type='l', lty=1,col='darkred',lwd=2,ylim=c(0,1),ylab='Probability',xlab='Envelopment Risk Lag(1)\n(dotted lines mark sample quantiles 0,25,50,75,100)')
# abline(v=quantile(samp, probs = c(0,.25,.5,.75,1)), lty=2)




dev.off(); plot.new();par(mfrow=c(2,2))
plot(gof(fl[length(fl)]))

# fl2
# fl.good
# fl.good.deg
# fl.use
# fl.gwesp0 <- fl[2:4]

# > mean(degree(getIgraphFromNet(net)))
# [1] 3.621362
# > mean(net %v% 'envrisk')
# [1] 0.3835154
# > mean(net %v% 'envrisk_lag1')
# [1] 0.3619397

mu1 <-  c( mean((net %v% 'envrisk')), 
          mean((net %v% 'envrisk_lag1')), 
    1, 0, mean(degree(getIgraphFromNet(net))),
    0, # ??
    0,
    0)
mu1std <-  c( mean((net %v% 'envrisk')) + sd((net %v% 'envrisk')), 
           mean((net %v% 'envrisk_lag1')), 
           1, 0, mean(degree(getIgraphFromNet(net))),
           0, # ??
           0,
           0)
b1 <- c(3.631853, 0.130217,1.683311,-0.728211, -9.887887, 
        2.028685, 0.537109, 0.006603 )

1 / (1 + exp(- b1 %*% mu1))
1 / (1 + exp(- b1 %*% mu1std))


tmpf <- function(x)
{
  mu1 <-  c( x, 
             mean((net %v% 'envrisk_lag1')), 
             1, 0, mean(degree(getIgraphFromNet(net))),
             0, # ??
             0,
             0)
  b1 <- c(3.631853, 0.130217,1.683311,-0.728211, -9.887887, 
          2.028685, 0.537109, 0.006603 )
  
  return(1 / (1 + exp(- b1 %*% mu1)))
}

f4 <- ergm(net ~ nodecov("envrisk") + nodematch("type", diff=TRUE)
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



############################################################################
#--------------------- DYNAMIC NETWORK------------------------------
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



## ------------------- NETWORKDYNAMIC PLOTS -----------------------------
data("short.stergm.sim")
ss <- short.stergm.sim
ndtv::timeline(ss)
ndtv::timePrism(ss, at=seq(1,10,by=3))
ndtv::filmstrip(ss, frames=9, mfrow=c(3,3))
ndtv::render.d3movie(ss, filename = )










