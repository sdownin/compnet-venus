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
if( !('gl1' %in% ls()) )
  load('netrisk_sms_full_pd_graph.RData')
###
# save.image('netrisk_sms_full_pd_graph.RData')
###
source(file.path(getwd(),'R','comp_net_functions.R'))
source(file.path(getwd(),'R','netrisk_functions.R'))
source(file.path(getwd(),'R','cb_data_prep.R'))
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
    g_i_r <- envRisk(g_i_r, risk.center=F,risk.scale=F,out.graph=T)
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
    tmp <- igraph::shortest.paths(gx, v=V(gx)[V(gx)$name==name_i], to=V(gx)[V(gx)$name %in% c(multi.prod)] ) 
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