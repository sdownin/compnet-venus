setwd("C:/Users/T430/Google Drive/PhD/Dissertation/competition networks/compnet2")
# .libPaths('C:/Users/T430/Documents/R/win-library/3.2')
library(parallel)
library(network, quietly = T)
library(texreg, quietly = T)
library(igraph, quietly = T)
library(plyr, quietly = T)
library(lattice, quietly = T)
library(latticeExtra, quietly = T)
library(ggplot2, quietly = T)
library(reshape2)

data_dir <- "C:/Users/T430/Google Drive/PhD/Dissertation/crunchbase/"
## FUNCTIONS
source(file.path(getwd(),'R','comp_net_functions.R'))
source(file.path(getwd(),'R','netrisk_functions.R'))
## DATA
source(file.path(getwd(),'R','cb_data_prep.R'))

is.missing <- function(x)
{
  if(is.null(x)) 
    return(TRUE)
  return(is.na(x) | is.nan(x) | x == '')
}

#####################################################################################
## MAKE FULL COMP NET OF ALL RELATIONS IN DB 
#####################################################################################
# g.full <- makeGraph(comp = co_comp, vertdf = co)
# ## cut out confirmed dates >= 2016
# g.full <- igraph::induced.subgraph(g.full, vids=V(g.full)[which(V(g.full)$founded_year <= 2016
#                                                                 | is.na(V(g.full)$founded_year)
#                                                                 | V(g.full)$founded_year=='' ) ] )
# g.full <- igraph::delete.edges(g.full, E(g.full)[which(E(g.full)$relation_created_at >= '2017-01-01')])
# ## SIMPLIFY
# g.full <- igraph::simplify(g.full, remove.loops=T,remove.multiple=T,
#                            edge.attr.comb = list(weight='sum',
#                                                  relation_began_on='min',
#                                                  relation_ended_on='min'))
# ## save
# igraph::write.graph(graph = g.full, file="g_full.graphml", format = 'graphml')
######################################################################################

## SORT CO_ACQ BY acquisition date
co_acq <- co_acq[order(co_acq$acquired_on, decreasing = F), ]

## comptetition network
## 37828
g.full <- read.graph('g_full.graphml', format='graphml')

## add comp net vertex IDs to acquisitions dataframe
gdf <- data.frame(acquirer_vid=as.integer(V(g.full)), acquirer_name_unique=V(g.full)$name)
co_acq <- merge(co_acq, gdf, by='acquirer_name_unique')
gdf <- data.frame(acquiree_vid=as.integer(V(g.full)), acquiree_name_unique=V(g.full)$name)
co_acq <- merge(co_acq, gdf, by='acquiree_name_unique')

## only acquisitions in recent period
## 20999 (of 29805)
co_acq_d <- co_acq[which(co_acq$acquired_on >= '2008-01-01'), ]

## acquisition filtered by comp net firms union (either aquirer or acquired)
## 14208
co_acq_g_u <- co_acq[which(co_acq$acquirer_name_unique %in% V(g.full)$name
                           | co_acq$acquiree_name_unique %in% V(g.full)$name), ]
## acquisition filtered by comp net firms intersection (both aquirer and acquired)
## 3442
co_acq_g_i <- co_acq[which(co_acq$acquirer_name_unique %in% V(g.full)$name
                           & co_acq$acquiree_name_unique %in% V(g.full)$name), ]
## acquisition filtered by comp net firms intersection, recent date (>=2010)
## 2786
co_acq_g_i_d <- co_acq_g_i[which(co_acq_g_i$acquired_on >= '2010-01-01'), ]

## check top filtered acquirers
cnt <- plyr::count(co_acq_g_i_d$acquirer_name_unique)
cnt <- cnt[order(cnt$freq, decreasing = T),]
head(cnt, 20)
# Top Acquirers:             x   freq
# 496                   google   73
# 544                      ibm   46
# 414                 facebook   32
# 847                   oracle   30
# 1358                   yahoo   29
# 80                     apple   28
# 740                microsoft   26
# 1002              salesforce   26
# 1229                 twitter   23
# 58                    amazon   20
# 240                    cisco   18
# 368                     ebay   17
# 505                  groupon   17
# 581                    intel   16
# 68                       aol   15
# 385  endurance-international   15
# 945                  rakuten   14
# 243           citrix-systems   13
# 323                     dell   12
# 379                      emc   12

## Check Acquisitions distribution by network distance included
df.ego <- data.frame()
name_i <- 'ibm'
for (d in 1:6) {
  g.ego <- igraph::make_ego_graph(graph = g.full, 
                                  nodes = V(g.full)[V(g.full)$name==name_i], 
                                  order = d, mode = 'all')[[1]]
  mem <- igraph::multilevel.community(g.ego)$membership
  mem.cnt <- plyr::count(mem)
  mem.cnt <- mem.cnt[order(mem.cnt$freq, decreasing = T), ]
  dim(mem.cnt)
  ##  
  acq.src <- co_acq_d[which(co_acq_d$acquirer_name_unique %in% V(g.ego)$name), ]
  acq.src.trg <- co_acq_d[which(co_acq_d$acquirer_name_unique %in% V(g.ego)$name
                                & co_acq_d$acquiree_name_unique %in% V(g.ego)$name), ]
  ##
  df.tmp <- data.frame(d=d,v=vcount(g.ego),e=ecount(g.ego),
                       acq.src=nrow(acq.src),acq.src.trg=nrow(acq.src.trg),
                       r.in.out=round(nrow(acq.src.trg)/(nrow(acq.src)+nrow(acq.src.trg)),3),
                       first.comp=min(E(g.ego)$relation_began_on))
  df.ego <- rbind(df.ego, df.tmp)
}; df.ego 


##--------------------------------------------------------------
##--------------------------------------------------------------
##--------- CREATE FIRM NETWORK PERIOD LISTS  ------------------
##--------------------------------------------------------------
##--------------------------------------------------------------


name_i <- 'ibm'
d <- 2
times <- sapply(2014:2017, function(x)paste0(x,'-01-01'))

g.ego <- igraph::make_ego_graph(graph = g.full,
                                nodes = V(g.full)[V(g.full)$name==name_i],
                                order = d, mode = 'all')[[1]]
g.ego.pd.tmp <- makePdGraph(g.ego, times[1], times[length(times)], isolates.remove=TRUE)
c(v=vcount(g.ego.pd.tmp),e=ecount(g.ego.pd.tmp))
sapply(2:length(times), function(i){gi=makePdGraph(g.ego, times[i-1], times[i], TRUE); return(c(e=ecount(gi),v=vcount(gi)))})

## YEAR PERIODS: DEFINE NICHE CLUSTERS
l <- list()
df.mmc <- data.frame()
df.rem <- data.frame()
df.reg <- data.frame()
lidx <- 0
timeval <- timeval.last <- 0

## all event vertices
acq.src <- co_acq[which(co_acq$acquirer_name_unique %in% V(g.ego.pd.tmp)$name), ]
acq.src.allpd <- acq.src[which(acq.src$acquired_on >= times[1] & acq.src$acquired_on < times[length(times)]) , ]
acq.src.allpd <- acq.src.allpd[order(acq.src.allpd$acquired_on, decreasing = F), ]
## check number of acquisitions to counts
acq.src.trg.allpd <- acq.src.allpd[which(acq.src.allpd$acquirer_name_unique %in% V(g.ego)$name 
                                       & acq.src.allpd$acquiree_name_unique %in% V(g.ego)$name), ]
dim(acq.src.trg.allpd)
## get all verts in the period (first either acquired or acquirer)
acq.verts <- unique(c(as.character(acq.src.allpd$acquirer_name_unique), 
                      as.character(acq.src.allpd$acquiree_name_unique)))
df.verts <- data.frame(id=1:length(acq.verts), name=acq.verts, stringsAsFactors = F)
yrs <- co$founded_year[which(co$company_name_unique %in% df.verts$name & !is.na(co$founded_year))]
df.verts$founded_year <- sapply(1:nrow(df.verts), function(x) {
    year <- co$founded_year[which(co$company_name_unique == df.verts$name[x])]
    year <- year[1]
    return(ifelse(is.na(year)|length(year)<1|class(year)=='list', median(yrs,na.rm = T), year))
  }, simplify = T)
df.verts$age <- 2018 - df.verts$founded_year
##
# p <- 18 ## i.mmc, i.mmc^2, num.mkts, deg, power
# m <- nrow(acq.src.allpd)
# n <- nrow(df.verts)
# ar.cov <- array(dim=c(m,p,n))

# nEventCov <- 4
# ar.cov.rec <- array(dim=c(m,nEventCov,n,n))

start <- times[1]
end <- times[length(times)]
## make period graph
g.pd <- makePdGraph(g.ego, start, end, isolates.remove=TRUE)
g.full.pd <- makePdGraph(g.full, start, end, isolates.remove=TRUE)
## period NC
V(g.pd)$nc <- as.integer(igraph::multilevel.community(g.pd)$membership)
## keep original pd graph
g.pd.orig <- g.pd
g.full.pd.orig <- g.full.pd
## filter acquisitions made by firms in this period graph
acq.src <- co_acq[which(co_acq$acquirer_name_unique %in% V(g.pd)$name), ]
## filter acquisitions made during this period
acq.src.pd <- acq.src[which(acq.src$acquired_on >= start & acq.src$acquired_on < end) , ]
acq.src.pd <- acq.src.pd[order(acq.src.pd$acquired_on, decreasing = F), ]



## ACQUISITION EVENTS:  UPDATE MMC & DYNAMIC EFFs
for (j in 67:nrow(acq.src.allpd)) {
  date_j <- acq.src.allpd$acquired_on[j]
  ## g.pd            d2 updated each acquisition
  ## g.pd.orig       d2 original
  ## g.full.pd.orig  global network within period start, end
  
  cat(sprintf('\n\nstart %s end %s : acquisition %s\n\n',start,end,j))
  if ( !(acq.src.allpd$acquiree_name_unique[j] %in% V(g.full.pd.orig)$name) ) 
    next
  if ( !(acq.src.allpd$acquirer_name_unique[j] %in% V(g.pd)$name) ) 
    next

  lidx <- lidx + 1
  l[[lidx]] <- list()
  
  ## Update MMC after acquisition
  l[[lidx]]$mmc <- getFmMmc(g.pd, as.integer(V(g.pd)$nc))
  
  ## SUM FM MMC over markets  ??????
  V(g.pd)$fm.mmc.sum <- rowSums(l[[lidx]]$mmc)
  V(g.pd)$num.mkts <- apply(l[[lidx]]$mmc, MARGIN=1, FUN=function(x){
    return(length(x[x>0]))
  })
  
  ## GET  DATAFRAME VARS
  
  ## Acquirer d2 original org.vid
  xi.orig.vid <- V(g.pd.orig)$orig.vid[which(acq.src.allpd$acquirer_name_unique[j] == V(g.pd.orig)$name)]
  ## target d2 original org.vid
  xj.orig.vid <- V(g.pd.orig)$orig.vid[which(acq.src.allpd$acquiree_name_unique[j] == V(g.pd.orig)$name)]
  xj.orig.vid <- ifelse(length(xj.orig.vid) > 1, xj.orig.vid, NA)
  ## acquirer  d2 t=j id
  xi <- which(V(g.pd)$name==acq.src.allpd$acquirer_name_unique[j])
  ## target  d2 t=j id
  xj <- which(V(g.pd)$name==acq.src.allpd$acquirer_name_unique[j])
  # acquirer id in original graph (at start of period)
  xi.orig <- as.integer(V(g.pd.orig)[V(g.pd.orig)$name==acq.src.allpd$acquirer_name_unique[j]])
  xi.nc <- as.integer(V(g.pd.orig)$nc[xi.orig]) ## original nc for the period
  #
  xi.mmc.sum <-  V(g.pd)$fm.mmc.sum[xi]
  xi.num.mkts <-  V(g.pd)$num.mkts[xi]
  ##
  xj <- as.integer(V(g.pd)[V(g.pd)$name==acq.src.allpd$acquiree_name_unique[j]])
  xj.orig <- ifelse( !is.na(xj.orig.vid), as.integer(V(g.pd.orig)[V(g.pd.orig)$orig.vid==xj.orig.vid]), NA)
  xj.orig <- ifelse(length(xj.orig) > 1, xj.orig, NA)
  xj.nc <- ifelse(length(xj)==0,NA,  V(g.pd.orig)$nc[xj.orig] )  ## original nc for the period
  
  ##--------------------------------------
  ## TARGET ALTERNATIVES SET
  ##--------------------------------------
  cat('target set ...')
  ## target alternative set vids
  targ.id <- which(V(g.full.pd)$name == acq.src.allpd$acquiree_name_unique[j])
  targ.vids.d2 <- igraph::neighborhood(graph = g.full.pd, order = 2, nodes = targ.id)[[1]]
  targ.vids.d2 <- targ.vids.d2[which( !(names(targ.vids.d2) %in% V(g.full.pd)$name[targ.id]))]
  targ.vids.d1 <- igraph::neighborhood(graph = g.full.pd, order = 1, nodes = targ.id)[[1]]
  targ.vids.d1 <- targ.vids.d1[which( !(names(targ.vids.d1) %in% V(g.full.pd)$name[targ.id]))]
  ## Target alternatives dataframe
  df.targ.alt <- co[which(co$company_name_unique %in% c(names(targ.vids.d1),names(targ.vids.d2),V(g.full.pd)$name[targ.id])), ]
  df.targ.alt$d <- sapply(df.targ.alt$company_name_unique, function(x){ return(
      ifelse(x ==  V(g.full.pd)$name[targ.id], 0, 
             ifelse(x %in% names(targ.vids.d1), 1,   2))
    )})
  ## ipo status
  df.targ.alt$is.public <- sapply(1:nrow(df.targ.alt), function(x){
    isNotOperating <- df.targ.alt$status[x] != 'operating'
    ipo.date <- co_ipo$went_public_on[which(co_ipo$company_name_unique == df.targ.alt$company_name_unique[x])]
    if (length(ipo.date)<1) 
      return(0)
    return(ifelse( isNotOperating & ipo.date <= date_j, 1, 0))
  })
  ## target had IPO
  df.targ <- df.targ.alt[which(df.targ.alt$company_name_unique == V(g.full.pd)$name[targ.id]), ]
  
  if (nrow(df.targ) == 0)
    next
  
  tmp <- df.targ.alt[df.targ.alt$company_name_unique %in% names(targ.vids.d2), ]
  ## select based on ownership status
  if (df.targ$is.public == 1) {
    tmp <- tmp[tmp$is.public == 1, ]
  } else {
    tmp <- tmp[tmp$is.public == 0, ] 
  }
  tmp.alt <- tmp[sample(1:nrow(tmp),size = min(9,nrow(tmp)),replace = F), ]
  ## combine target and alternatives for target set
  df.targ.alt <- rbind(tmp.alt, df.targ)
  ## add MMC
  df.targ.alt$fm.mmc.sum <- sapply(df.targ.alt$company_name_unique, function(name){
    ifelse(name %in% V(g.pd)$name, V(g.pd)$fm.mmc.sum[which(V(g.pd)$name == name)] , NA)
  })
  df.targ.alt$num.mkts <- sapply(df.targ.alt$company_name_unique, function(name){
    ifelse(name %in% V(g.pd)$name, V(g.pd)$num.mkts[which(V(g.pd)$name == name)] , NA)
  })
  cat('done.\n')
  ##--------------------------------------
  
  ##--------------------------------------
  ## ACQUIRER ALTERNATIVES SET
  ##--------------------------------------
  cat('acquirer set ...')
  ## acquirer alternative set vids
  acq.id <- which(V(g.pd)$name == acq.src.allpd$acquirer_name_unique[j])
  acq.vids.d2 <- igraph::neighborhood(graph = g.pd, order = 2, nodes = acq.id)[[1]]
  acq.vids.d2 <- acq.vids.d2[which( !(names(acq.vids.d2) %in% V(g.pd)$name[acq.id]))]
  acq.vids.d1 <- igraph::neighborhood(graph = g.pd, order = 1, nodes = acq.id)[[1]]
  acq.vids.d1 <- acq.vids.d2[which( !(names(acq.vids.d1) %in% V(g.pd)$name[acq.id]))]
  ## acquirer alternatives dataframe
  # length(acq.vids.d1)
  df.acq.alt <- co[which(co$company_name_unique %in% c(names(acq.vids.d1),names(acq.vids.d2),V(g.pd)$name[acq.id])), ]
  df.acq.alt$d <- sapply(df.acq.alt$company_name_unique, function(x){ return(
    ifelse(x ==  V(g.pd)$name[acq.id], 0, 
           ifelse(x %in% names(acq.vids.d1), 1,   2))
  )})
  ## ipo status
  df.acq.alt$is.public <- sapply(1:nrow(df.acq.alt), function(x){
    isNotOperating <- df.acq.alt$status[x] != 'operating'
    ipo.date <- co_ipo$went_public_on[which(co_ipo$company_name_unique == df.acq.alt$company_name_unique[x])]
    if (length(ipo.date)<1) 
      return(0)
    return(ifelse( isNotOperating & ipo.date <= date_j, 1, 0))
  })
  ## target had IPO
  df.acq <- df.acq.alt[which(df.acq.alt$company_name_unique == V(g.pd)$name[acq.id]), ]
  
  if (nrow(df.acq) == 0)
    next
  
  tmp <- df.acq.alt[df.acq.alt$company_name_unique %in% names(acq.vids.d2), ]
  ## select based on ownership status
  if (df.acq$is.public == 1) {
    tmp <- tmp[tmp$is.public == 1, ]
  } else {
    tmp <- tmp[tmp$is.public == 0, ] 
  }
  tmp.acq.alt <- tmp[sample(1:nrow(tmp),size = min(9,nrow(tmp)),replace = F), ]
  ## combine target and alternatives for target set
  df.acq.alt <- rbind(tmp.acq.alt, df.acq)
  ## set MMC
  df.acq.alt$fm.mmc.sum <- sapply(df.acq.alt$company_name_unique, function(name){
      ifelse(name %in% V(g.pd)$name, as.numeric(V(g.pd)$fm.mmc.sum[which(V(g.pd)$name == name)]) , NA)
    })
  df.acq.alt$num.mkts <- sapply(df.acq.alt$company_name_unique, function(name){
      ifelse(name %in% V(g.pd)$name, as.numeric(V(g.pd)$num.mkts[which(V(g.pd)$name == name)]) , NA)
    })
  cat('done.\n')
  # cat('acquirer set ...')
  # ## acquirer alternative set vids
  # acq.id <- which(V(g.full.pd)$name == acq.src.allpd$acquirer_name_unique[j])
  # acq.vids.d2 <- igraph::neighborhood(graph = g.full.pd, order = 2, nodes = acq.id)[[1]]
  # acq.vids.d2 <- acq.vids.d2[which( !(names(acq.vids.d2) %in% V(g.full.pd)$name[acq.id]))]
  # acq.vids.d1 <- igraph::neighborhood(graph = g.full.pd, order = 1, nodes = acq.id)[[1]]
  # acq.vids.d1 <- acq.vids.d2[which( !(names(acq.vids.d1) %in% V(g.full.pd)$name[acq.id]))]
  # ## acquirer alternatives dataframe
  # # length(acq.vids.d1)
  # df.acq.alt <- co[which(co$company_name_unique %in% c(names(acq.vids.d1),names(acq.vids.d2),V(g.full.pd)$name[acq.id])), ]
  # df.acq.alt$d <- sapply(df.acq.alt$company_name_unique, function(x){ return(
  #   ifelse(x ==  V(g.full.pd)$name[acq.id], 0, 
  #          ifelse(x %in% names(acq.vids.d1), 1,   2))
  # )})
  # ## ipo status
  # df.acq.alt$is.public <- sapply(1:nrow(df.acq.alt), function(x){
  #   isNotOperating <- df.acq.alt$status[x] != 'operating'
  #   ipo.date <- co_ipo$went_public_on[which(co_ipo$company_name_unique == df.acq.alt$company_name_unique[x])]
  #   if (length(ipo.date)<1) 
  #     return(0)
  #   return(ifelse( isNotOperating & ipo.date <= date_j, 1, 0))
  # })
  # ## target had IPO
  # df.acq <- df.acq.alt[which(df.acq.alt$company_name_unique == V(g.full.pd)$name[acq.id]), ]
  # tmp <- df.acq.alt[df.acq.alt$company_name_unique %in% names(acq.vids.d2), ]
  # if (df.acq$is.public == 1) {
  #   tmp <- tmp[tmp$is.public == 1, ]
  # } else {
  #   tmp <- tmp[tmp$is.public == 0, ] 
  # }
  # tmp.acq.alt <- tmp[sample(1:nrow(tmp),size = min(9,nrow(tmp)),replace = F), ]
  # ## combine target and alternatives for target set
  # df.acq.alt <- rbind(tmp.acq.alt, df.acq)
  # cat('done.\n')
  ##--------------------------------------

  ##--------------------------------------
  ##  NETWORK COVARIATES
  ##--------------------------------------
  cat('computing network covariates...')
  df.acq.alt$set <- 'acquirer'
  df.acq.alt$event <- sapply(df.acq.alt$d, function(d)ifelse(as.integer(d)==0, 1, 0))
  df.targ.alt$set <- 'target'
  df.targ.alt$event <- sapply(df.targ.alt$d, function(d)ifelse(as.integer(d)==0, 1, 0))
  df.alt <- rbind(df.acq.alt, df.targ.alt)
  df.alt$t <- j ## acquisition index
  df.alt <- df.alt[order(which(V(g.full.pd.orig)$name %in% df.alt$company_name_unique )), ] ## confirm ascencing order
  
  # ## Create Diff Graph (removed|acquired nodes are represented as isolates)
  vids <- which( V(g.full.pd)$name %in% df.alt$company_name_unique )
  vids.orig <- which( V(g.full.pd.orig)$name %in% df.alt$company_name_unique )
  vids.orig.rm <- vids[which( !(vids.orig %in% vids))]
  mapping <- V(g.full.pd.orig)[which(V(g.full.pd.orig)$orig.vid %in% V(g.full.pd)$orig.vid) ]
  g.diff <- igraph::contract.vertices(g.full.pd, mapping = mapping)
  V(g.diff)$name <- V(g.full.pd.orig)$name
  vids.diff <- as.integer( V(g.diff)[which( V(g.diff)$name %in% df.alt$company_name_unique )] )
  
  ## global covars
  tmp.cov <- data.frame(
    company_name_unique = unlist(V(g.diff)$name[vids.diff]),
    pow.n2 = unname(unlist(igraph::power_centrality(g.diff, nodes = vids.diff, exponent = -0.2))),
    pow.n1 = unname(unlist(igraph::power_centrality(g.diff, nodes = vids.diff, exponent = -0.1))),
    deg = unname(igraph::degree(g.diff, v = vids.diff)),
    constraint = unname(unlist(igraph::constraint(g.diff, nodes = vids.diff)))
  )
  df.alt <- merge(df.alt, tmp.cov, by = 'company_name_unique', all.x = T, all.y = F)
  
  df.alt$acq.experience <- unlist(sapply(1:nrow(df.alt), function(x){ return(
    nrow(acq.src.allpd[which(acq.src.allpd$acquirer_name_unique == df.alt$company_name_unique[x]
                             & acq.src.allpd$acquired_on <= date_j), ]) / j ## scale experience to num observed acquisitions
  )}))
  ## local covars in pd graph
  df.alt$fm.mmc.sum <- sapply(df.alt$company_name_unique, function(name){
    ifelse(name %in% V(g.pd)$name, as.numeric(V(g.pd)$fm.mmc.sum[which(V(g.pd)$name == name)]), NA)
  })
  df.alt$num.mkts   <-  sapply(df.alt$company_name_unique, function(name){
    ifelse(name %in% V(g.pd)$name, as.numeric(V(g.pd)$num.mkts[which(V(g.pd)$name == name)]), NA)
  })
  
  cat('done.\n')
  ##---------------------------------------------
  
  l[[lidx]]$df.alt <- df.alt
  
  ##---------------------------------------------
  cat('appending dyadic regression dataframe...')
  
  for (k in 1:nrow(df.alt[df.alt$set=='acquirer', ])) {
    for (r in 1:nrow(df.alt[df.alt$set=='target', ])) {
      ix <- which( df.alt$company_name_unique == df.alt[df.alt$set=='acquirer', ][k, ]$company_name_unique )
      jx <- which( df.alt$company_name_unique == df.alt[df.alt$set=='target', ][r, ]$company_name_unique )
      
      if (df.alt$company_name_unique[ix] != df.alt$company_name_unique[jx]) {
        ij.dist <- igraph::distances(g.full.pd, 
                                     v = V(g.full.pd)[which(V(g.full.pd)$name == df.alt$company_name_unique[ix])], 
                                     to =V(g.full.pd)[which(V(g.full.pd)$name == df.alt$company_name_unique[jx])] )
        df.tmp.dyad <- data.frame(
          # event features 
          y = ifelse(as.integer(df.alt$event[ix]) & as.integer(df.alt$event[jx]), 1, 0),
          t = j,
          date = date_j,
          i = df.alt$company_name_unique[ix], 
          j = df.alt$company_name_unique[jx], 
          # acquirer covars 
          i.pow.n1 = df.alt$pow.n1[ix],
          i.pow.n2 = df.alt$pow.n2[ix],
          i.deg = df.alt$deg[ix],
          i.fm.mmc.sum = ifelse(is.missing(df.alt$fm.mmc.sum[ix]), NA, df.alt$fm.mmc.sum[ix]),
          i.num.mkts = ifelse(is.missing(df.alt$num.mkts[ix]), NA, df.alt$num.mkts[ix]),
          i.constraint = df.alt$constraint[ix],
          i.acq.experience = df.alt$acq.experience[ix],
          # target covars 
          j.pow.n1 = df.alt$pow.n1[jx],
          j.pow.n2 = df.alt$pow.n2[jx],
          j.deg = df.alt$deg[jx],
          j.fm.mmc.sum = ifelse(is.missing(df.alt$fm.mmc.sum[jx]), NA, df.alt$fm.mmc.sum[jx]),
          j.num.mkts = ifelse(is.missing(df.alt$num.mkts[jx]), NA, df.alt$num.mkts[jx]),
          j.constraint = df.alt$constraint[jx],
          j.acq.experience = df.alt$acq.experience[jx],
          # dyadic covars 
          ij.same.region = ifelse(df.alt$region[ix] == df.alt$region[jx], 1, 0),
          ij.same.state = ifelse(df.alt$state_code[ix] == df.alt$state_code[jx], 1, 0),
          ij.same.country = ifelse(df.alt$country_code[ix] == df.alt$country_code[jx], 1, 0),
          ij.same.employee.range = ifelse(df.alt$employee_count[ix] == df.alt$employee_count[jx], 1, 0),
          ij.dist = ifelse( class(ij.dist)=='matrix' & nrow(ij.dist)>0 & ncol(ij.dist)>0, ij.dist[1,1], Inf),
          ij.diff.pow.n1 = as.numeric(df.alt$pow.n1[ix]) - as.numeric(df.alt$pow.n1[jx]),
          ij.diff.pow.n2 = as.numeric(df.alt$pow.n2[ix]) - as.numeric(df.alt$pow.n2[jx]),
          ij.diff.deg = as.numeric(df.alt$deg[ix]) - as.numeric(df.alt$deg[jx]),
          ij.diff.fm.mmc.sum = ifelse(any(is.missing(df.alt$fm.mmc.sum[ix]),is.missing(df.alt$fm.mmc.sum[jx])), NA, as.numeric(df.alt$fm.mmc.sum[ix]) - as.numeric(df.alt$fm.mmc.sum[jx])),
          ij.diff.num.mkts = ifelse(any(is.missing(df.alt$num.mkts[ix]), is.missing(df.alt$num.mkts[jx])), NA, as.numeric(df.alt$num.mkts[ix]) - as.numeric(df.alt$num.mkts[jx])),
          ij.diff.constraint = as.numeric(df.alt$constraint[ix]) - as.numeric(df.alt$constraint[jx]),
          ij.diff.acq.experience = as.numeric(df.alt$acq.experience[ix]) - as.numeric(df.alt$acq.experience[jx])
        )
        df.reg <- rbind(df.reg, df.tmp.dyad)
      }
    }
  }
  
  cat('done.\n')
  ##---------------------------------------

  
  ##--------------------------------------------------------------------------    
  ## NODE COLLAPSE update network
  g.pd <- nodeCollapseGraph(g.pd, acq.src.allpd[j,])
  g.full.pd <- nodeCollapseGraph(g.full.pd, acq.src.allpd[j,])
  
  ## save incrementally
  if (lidx %% 10 == 0) {   
    saveRDS(list(l=l,df.reg=df.reg), file = sprintf("acqlogit_covs_list_%s.rds",name_i))
  }
  
  gc()
}


saveRDS(list(l=l,df.reg=df.reg), file = sprintf("acqlogit_covs_list_%s.rds",name_i))



tmp <- readRDS(sprintf("acqlogit_covs_list_%s.rds",name_i))
l <- tmp$l
df.reg <- tmp$df.reg

write.csv(df.reg, file = sprintf("acqlogit_covs_df_%s.csv",name_i), row.names = F)




