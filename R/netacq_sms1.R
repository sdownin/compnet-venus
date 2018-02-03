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
times <- sapply(2013:2017, function(x)paste0(x,'-01-01'))

g.ego <- igraph::make_ego_graph(graph = g.full,
                                nodes = V(g.full)[V(g.full)$name==name_i],
                                order = d, mode = 'all')[[1]]
sapply(2:length(times), function(i){gi=makePdGraph(g.ego, times[i-1], times[i], TRUE); return(c(e=ecount(gi),v=vcount(gi)))})

## YEAR PERIODS: DEFINE NICHE CLUSTERS
l <- list()
df.mmc <- data.frame()
df.rem <- data.frame()
lidx <- 0
timeval <- timeval.last <- 0

## all event vertices
acq.src <- co_acq[which(co_acq$acquirer_name_unique %in% V(g.ego)$name), ]
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
  for (j in 1:nrow(acq.src.allpd)) {
    date_j <- acq.src.allpd$acquired_on[j]
    ## g.pd            d2 updated each acquisition
    ## g.pd.orig       d2 original
    ## g.full.pd.orig  global network within period start, end
    
    cat(sprintf('\n\nstart %s end %s : acquisition %s\n\n',start,end,j))
    if ( !(acq.src.allpd$acquiree_name_unique[j] %in% V(g.full.pd.orig)$name) ) 
      next

    lidx <- lidx + 1
    l[[lidx]] <- list()
    l[[lidx]]$acq.exper <- plyr::count(acq.src.allpd$acquirer_name_unique[1:j])
    
    ## Update MMC after acquisition
    l[[lidx]]$mmc <- getFmMmc(g.pd, as.integer(V(g.pd)$nc))
    
    ## SUM FM MMC over markets  ??????
    V(g.pd)$fm.mmc.sum <- rowSums(l[[lidx]]$mmc)
    V(g.pd)$num.mkts <- apply(l[[lidx]]$mmc,MARGIN=1,FUN=function(x){
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
    tmp <- df.targ.alt[df.targ.alt$company_name_unique %in% names(targ.vids.d2), ]
    if (df.targ$is.public == 1) {
      tmp <- tmp[tmp$is.public == 1, ]
    } else {
      tmp <- tmp[tmp$is.public == 0, ] 
    }
    tmp.alt <- tmp[sample(1:nrow(tmp),size = min(9,nrow(tmp)),replace = F), ]
    ## combine target and alternatives for target set
    df.targ.alt <- rbind(tmp.alt, df.targ)
    ##--------------------------------------
    
    ##--------------------------------------
    ## ACQUIRER ALTERNATIVES SET
    ##--------------------------------------
    ## acquirer alternative set vids
    acq.id <- which(V(g.full.pd)$name == acq.src.allpd$acquirer_name_unique[j])
    acq.vids.d2 <- igraph::neighborhood(graph = g.full.pd, order = 2, nodes = acq.id)[[1]]
    acq.vids.d2 <- acq.vids.d2[which( !(names(acq.vids.d2) %in% V(g.full.pd)$name[acq.id]))]
    acq.vids.d1 <- igraph::neighborhood(graph = g.full.pd, order = 1, nodes = acq.id)[[1]]
    acq.vids.d1 <- acq.vids.d2[which( !(names(acq.vids.d1) %in% V(g.full.pd)$name[acq.id]))]
    ## acquirer alternatives dataframe
    # length(acq.vids.d1)
    df.acq.alt <- co[which(co$company_name_unique %in% c(names(acq.vids.d1),names(acq.vids.d2),V(g.full.pd)$name[acq.id])), ]
    df.acq.alt$d <- sapply(df.acq.alt$company_name_unique, function(x){ return(
      ifelse(x ==  V(g.full.pd)$name[acq.id], 0, 
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
    df.acq <- df.acq.alt[which(df.acq.alt$company_name_unique == V(g.full.pd)$name[acq.id]), ]
    tmp <- df.acq.alt[df.acq.alt$company_name_unique %in% names(acq.vids.d2), ]
    if (df.acq$is.public == 1) {
      tmp <- tmp[tmp$is.public == 1, ]
    } else {
      tmp <- tmp[tmp$is.public == 0, ] 
    }
    tmp.acq.alt <- tmp[sample(1:nrow(tmp),size = min(9,nrow(tmp)),replace = F), ]
    ## combine target and alternatives for target set
    df.acq.alt <- rbind(tmp.acq.alt, df.acq)
    ##--------------------------------------
  
    ##--------------------------------------
    ##  NETWORK COVARIATES
    ##--------------------------------------
    df.acq.alt$set <- 'acquirer'
    df.acq.alt$event <- sapply(df.acq.alt$d, function(d)ifelse(as.integer(d)==0, 1, 0))
    df.targ.alt$set <- 'target'
    df.targ.alt$event <- sapply(df.targ.alt$d, function(d)ifelse(as.integer(d)==0, 1, 0))
    df.alt <- rbind(df.acq.alt, df.targ.alt)
    
    # ## Create Diff Graph (removed|acquired nodes are represented as isolates)
    vids <- which( V(g.full.pd)$name %in% df.alt$company_name_unique )
    vids.orig <- which( V(g.full.pd.orig)$name %in% df.alt$company_name_unique )
    vids.orig.rm <- vids[which( !(vids.orig %in% vids))]
    mapping <- V(g.full.pd.orig)[which(V(g.full.pd.orig)$orig.vid %in% V(g.full.pd)$orig.vid) ]
    g.diff <- igraph::contract.vertices(g.full.pd, mapping = mapping)
    V(g.diff)$name <- V(g.full.pd.orig)$name
    vids.diff <- as.integer( V(g.diff)[which( V(g.diff)$name %in% df.alt$company_name_unique )] )
    
    cat('computing network covariates...')
    ## set covars
    df.alt$pow.n3 <- unname(unlist(igraph::power_centrality(g.diff, nodes = vids.diff, exponent = -0.3)))
    df.alt$pow.n2 <- unname(unlist(igraph::power_centrality(g.diff, nodes = vids.diff, exponent = -0.2)))
    df.alt$pow.n1 <- unname(unlist(igraph::power_centrality(g.diff, nodes = vids.diff, exponent = -0.1)))
    df.alt$deg <- igraph::degree(g.diff, v = vids.diff)
    df.alt$fm.mmc.sum <- V(g.diff)$fm.mmc.sum[vids.diff]
    df.alt$num.mkts <- V(g.diff)$num.mkts[vids.diff]
    
    cat('done.\n')
    
    ## save current data in dataframe
    df.pd <- data.frame(fm.mmc.sum=V(g.pd)$fm.mmc.sum, 
                        num.mkts=V(g.pd)$num.mkts,
                        nc=V(g.pd)$nc)
    df.mmc <- rbind(df.mmc, df.pd)
    

    
    ##  compute degree and selected power centralities for vids.diff from full graph g.diff
    .v1 <- unname(unlist(V(g.diff)[vids.diff]$name))
    .v2 <- unname(unlist(igraph::degree(g.diff, v = vids.diff)))
    .v3 <- unname(unlist(igraph::power_centrality(g.diff, nodes = vids.diff, exponent = -0.3)))
    .v4 <- unname(unlist(igraph::power_centrality(g.diff, nodes = vids.diff, exponent = -0.2)))
    .v5 <- unname(unlist(igraph::power_centrality(g.diff, nodes = vids.diff, exponent = -0.1)))
    df.diff.cov <- data.frame(name=.v1, deg = .v2, pow.n3=.v3, pow.n2=.v4, pow.n1=.v5)
    df.pd.cov <- merge(x = df.pd.cov, y = df.diff.cov, by='name', all.x=T, all.y=F)

    date_j <- acq.src.allpd$acquired_on[j]
    df.verts$is.public <- sapply(1:nrow(df.verts), function(x){
      isNotOperating <- co$status[which(co$company_name_unique == df.verts$name[x])] != 'operating'
      ipo.date <- co_ipo$went_public_on[which(co_ipo$company_name_unique == df.verts$name[x])]
      if (length(ipo.date)<1) 
        return(0)
      return(ifelse( isNotOperating & ipo.date <= date_j, 1, 0))
    })
    
    ##--------------------------------------------------------------------------    
    ## NODE COLLAPSE update network
    g.pd <- nodeCollapseGraph(g.pd, acq.src.allpd[j,])
    g.full.pd <- nodeCollapseGraph(g.full.pd, acq.src.allpd[j,])
    
    if (lidx %% 20 == 0) {  ## save incrementally 
      saveRDS(list(l=l, g.full.pd.orig=g.full.pd.orig), file = sprintf("acqlogit_covs_list_%s.rds",name_i))
    }
  }
  
  saveRDS(list(l=l, g.full.pd.orig=g.full.pd.orig), file = sprintf("acqlogit_covs_list_%s.rds",name_i))
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ## ACQUISITION EVENTS:  UPDATE MMC & DYNAMIC EFFs
  for (j in 1:nrow(acq.src.allpd)) {
    cat(sprintf('\n\nstart %s end %s : acquisition %s\n\n',start,end,j))
    
    # if (acq.src.allpd$acquiree_name_unique[j] %in% V(g.pd.orig)$name) {
        lidx <- lidx + 1
        l[[lidx]] <- list()
        ## Update MMC after acquisition
        l[[lidx]]$mmc <- getFmMmc(g.pd, as.integer(V(g.pd)$nc))
    
        ## SUM FM MMC over markets  ??????
        V(g.pd)$fm.mmc.sum <- rowSums(l[[lidx]]$mmc)
        V(g.pd)$num.mkts <- apply(l[[lidx]]$mmc,MARGIN=1,FUN=function(x){
          return(length(x[x>0]))
        })
        ## save current data in dataframe
        df.pd <- data.frame(fm.mmc.sum=V(g.pd)$fm.mmc.sum, 
                            num.mkts=V(g.pd)$num.mkts,
                            nc=V(g.pd)$nc)
        df.mmc <- rbind(df.mmc, df.pd)
        ## GET REM DATAFRAME VARS
        # xi.orig.vid <- acq.src.allpd$acquirer_vid[j]
        # xj.orig.vid <- acq.src.allpd$acquiree_vid[j]
        xi.orig.vid <- V(g.pd.orig)$orig.vid[which(acq.src.allpd$acquirer_name_unique[j] == V(g.pd.orig)$name)]
        xj.orig.vid <- V(g.pd.orig)$orig.vid[which(acq.src.allpd$acquiree_name_unique[j] == V(g.pd.orig)$name)]
        xj.orig.vid <- ifelse(length(xj.orig.vid) > 1, xj.orig.vid, NA)
        xi <- as.integer(V(g.pd)[V(g.pd)$name==acq.src.allpd$acquirer_name_unique[j]])
        
        if (length(xi) >0 ) {
            xi.orig <- as.integer(V(g.pd.orig)[V(g.pd.orig)$name==acq.src.allpd$acquirer_name_unique[j]])
            xi.nc <- as.integer(V(g.pd.orig)$nc[xi.orig]) ## original nc for the period
            xi.mmc.sum <-  V(g.pd)$fm.mmc.sum[xi]
            xi.num.mkts <-  V(g.pd)$num.mkts[xi]
            xi.deg <- igraph::degree(g.pd)[xi]
            xi.pow <- igraph::power_centrality(g.pd, exponent = -0.3, sparse = T)[xi]
            ##
            xj <- as.integer(V(g.pd)[V(g.pd)$name==acq.src.allpd$acquiree_name_unique[j]])
            xj.orig <- ifelse( !is.na(xj.orig.vid), as.integer(V(g.pd.orig)[V(g.pd.orig)$orig.vid==xj.orig.vid]), NA)
            xj.orig <- ifelse(length(xj.orig) > 1, xj.orig, NA)
            xj.nc <- ifelse(length(xj)==0,NA,  V(g.pd.orig)$nc[xj.orig] )  ## original nc for the period
            xj.mmc.sum <- ifelse(length(xj)==0,NA,  V(g.pd)$fm.mmc.sum[xj] )
            xj.num.mkts <- ifelse(length(xj)==0,NA,  V(g.pd)$num.mkts[xj] )
            xj.deg <- ifelse(length(xj)==0,NA,  igraph::degree(g.pd)[xj] )
            xj.pow <- ifelse(length(xj)==0,NA,  igraph::power_centrality(g.pd, exponent = -0.3, sparse = T)[xj] )
            src <- ifelse(length(acq.src.allpd$acquirer_name_unique[j])>0, 
                          acq.src.allpd$acquirer_name_unique[j], NA)
            trg <- ifelse(length(acq.src.allpd$acquiree_name_unique[j])>0, 
                          acq.src.allpd$acquiree_name_unique[j], NA)
            datestr <- acq.src.allpd$acquired_on[j]
            timeval.last <- timeval
            timeval <- as.integer(ymd(datestr))
            timeval <- ifelse(timeval%in%df.rem$t, timeval.last + 0.01, timeval)
            ##
            # ## Create Diff Graph (removed nodes are isolates)
            vids <- as.integer( V(g.full.pd)[which( df.verts.pd.cov$name %in% V(g.full.pd)$name )] )
            vids.orig <- as.integer( V(g.full.pd.orig)[which( df.verts.pd.cov$name %in% V(g.full.pd.orig)$name )] )
            vids.orig.rm <- vids[which( !(vids.orig %in% vids))]
            mapping <- V(g.full.pd.orig)[which(V(g.full.pd.orig)$orig.vid %in% V(g.full.pd)$orig.vid) ]
            g.diff <- igraph::contract.vertices(g.full.pd, mapping = mapping)
            V(g.diff)$name <- V(g.full.pd.orig)$name
            vids.diff <- as.integer( V(g.diff)[which( V(g.diff)$name %in% df.verts$name )] )
            # MAKE REM DATAFRAME 
            df.rem.pd <- data.frame(t=timeval, src=src, trg=trg, 
                                    time=datestr, idx=lidx,
                                    i.orig.vid=xi.orig.vid,
                                    j.orig.vid=xj.orig.vid,
                                    i=xi, 
                                    i.nc=xi.nc, i.mmc.sum=xi.mmc.sum, i.num.mkts=xi.num.mkts, 
                                    i.deg=xi.deg,i.pow=xi.pow,
                                    j=ifelse(length(xj)==0,NA,xj), 
                                    j.nc=xj.nc, j.mmc.sum=xj.mmc.sum, j.num.mkts=xj.num.mkts, 
                                    j.deg=xj.deg, j.pow=xj.pow
            )
            df.rem <- rbind(df.rem, df.rem.pd)
            cat(sprintf("df.rem dim:  %s\n",paste(dim(df.rem),collapse=", ")))
            ## SAVE COVARIATE ARRAY [m,p,n] for m times, p covars, n actors
            df.pd.cov <- data.frame(name=V(g.pd)$name, 
                                    mmc.sum=as.numeric(V(g.pd)$fm.mmc.sum), 
                                    mmc.sum.sq=as.numeric(V(g.pd)$fm.mmc.sum)^2,
                                    num.mkts=as.numeric(V(g.pd)$num.mkts),
                                    pow.n4=igraph::power_centrality(g.pd, exponent = -0.4), 
                                    pow.n3=igraph::power_centrality(g.pd, exponent = -0.3), 
                                    pow.1 =igraph::power_centrality(g.pd, exponent = 0.1),
                                    pow.2 =igraph::power_centrality(g.pd, exponent = 0.2),
                                    pow.3 =igraph::power_centrality(g.pd, exponent = 0.3),
                                    pow.4 =igraph::power_centrality(g.pd, exponent = 0.4),
                                    betweenness=igraph::betweenness(g.pd),
                                    constraint=igraph::constraint(g.pd)  )
            eig <- igraph::eigen_centrality(g.pd)
            if (length(eig$vector)>0)
              df.pd.cov$eig  <- eig$vector
            ##  compute degree and selected power centralities for vids.diff from full graph g.diff
            .v1 <- unname(unlist(V(g.diff)[vids.diff]$name))
            .v2 <- unname(unlist(igraph::degree(g.diff, v = vids.diff)))
            .v3 <- unname(unlist(igraph::power_centrality(g.diff, nodes = vids.diff, exponent = -0.2)))
            .v4 <- unname(unlist(igraph::power_centrality(g.diff, nodes = vids.diff, exponent = -0.1)))
            df.diff.cov <- data.frame(name=.v1, deg = .v2, pow.n2=.v3, pow.n1=.v4)
            df.pd.cov <- merge(x = df.pd.cov, y = df.diff.cov, by='name', all.x=T, all.y=F)
            ##
            date_j <- acq.src.allpd$acquired_on[j]
            df.verts$is.public <- sapply(1:nrow(df.verts), function(x){
              ipo.date <- co_ipo$went_public_on[which(co_ipo$company_name_unique == df.verts$name[x])]
              if (length(ipo.date)<1) 
                return(0)
              return(ifelse( ipo.date <= date_j, 1, 0))
            })
            dim(co_ipo[which(co_ipo$company_name_unique %in% acq.src.allpd$acquirer_name_unique), ])
            df.verts.pd.cov <- merge(df.verts, df.pd.cov, by = 'name', all.x = T)
            df.verts.pd.cov <- df.verts.pd.cov[order(df.verts.pd.cov$id),]
            ## sender covariate array
            l[[lidx]]$cov <- df.verts.pd.cov
            ar.cov[lidx, 1, ] <- df.verts.pd.cov$mmc.sum
            ar.cov[lidx, 2, ] <- df.verts.pd.cov$mmc.sum.sq
            ar.cov[lidx, 3, ] <- df.verts.pd.cov$num.mkts
            ar.cov[lidx, 4, ] <- df.verts.pd.cov$deg
            ar.cov[lidx, 5, ] <- df.verts.pd.cov$pow.n4
            ar.cov[lidx, 6, ] <- df.verts.pd.cov$pow.n3
            ar.cov[lidx, 7, ] <- df.verts.pd.cov$pow.n2
            ar.cov[lidx, 8, ] <- df.verts.pd.cov$pow.n1
            ar.cov[lidx, 9, ] <- df.verts.pd.cov$pow.1
            ar.cov[lidx,10, ] <- df.verts.pd.cov$pow.2
            ar.cov[lidx,11, ] <- df.verts.pd.cov$pow.3
            ar.cov[lidx,12, ] <- df.verts.pd.cov$pow.4
            ar.cov[lidx,13, ] <- df.verts.pd.cov$betweenness
            ar.cov[lidx,14, ] <- df.verts.pd.cov$constraint
            ar.cov[lidx,15, ] <- df.verts.pd.cov$age
            ar.cov[lidx,16, ] <- df.verts.pd.cov$is.public
            ar.cov[lidx,17, ] <- df.verts.pd.cov$mmc.sum.sq * df.verts.pd.cov$pow.n1
            if ('eig' %in% names(df.verts.pd.cov))
              ar.cov[lidx,18, ] <- df.verts.pd.cov$eig
            ## Event covariate array
            ar.cov.rec[lidx,1, , ] <- outer(df.verts.pd.cov$age, df.verts.pd.cov$age, '-')
            ar.cov.rec[lidx,2, , ] <- as.matrix(dist(df.verts.pd.cov$is.public, df.verts.pd.cov$is.public, method = 'manhattan', diag = T, upper = T))
            ### vids from diff graph
            dists <- as.matrix( igraph::distances(g.diff, v = vids.diff, to = vids.diff) )
            dists <- max(dists[dists < Inf]) * (1 / dists)
            diag(dists) <- 0
            ar.cov.rec[lidx,3, , ] <- dists
            ar.cov.rec[lidx,4, , ] <- dists * df.verts.pd.cov$mmc.sum.sq
        }
    # }
    
    ## NODE COLLAPSE update network
    g.pd <- nodeCollapseGraph(g.pd, acq.src.allpd[j,])
    g.full.pd <- nodeCollapseGraph(g.full.pd, acq.src.allpd[j,])
    
    if (lidx %% 20 == 0) {
      saveRDS(list(df.rem=df.rem, ar.cov=ar.cov), file = sprintf("acquisitions_rem_covs_%s.rds",name_i))
      saveRDS(l, file = sprintf("acquisitions_cov_list_%s.rds",name_i))
      saveRDS(list(df.verts=df.verts, acq.src.allpd=acq.src.allpd), file = sprintf("acquisitions_verts_df_%s.rds",name_i))
    }
  }
  
  ## remove missing observations (skipped in loop)
  acq.src.allpd <- acq.src.allpd[order(acq.src.allpd$acquired_on, decreasing = F), ]
  df.rem <- df.rem[order(df.rem$t, decreasing = F), ]
  df.verts <- df.verts[which(df.verts$name %in% c(as.character(df.rem$src),as.character(df.rem$trg))),]
  df.verts$id <- 1:nrow(df.verts)
  ar.cov <- ar.cov[1:nrow(df.rem), , 1:nrow(df.verts) ]
  
  saveRDS(list(df.rem=df.rem, ar.cov=ar.cov), file = sprintf("acquisitions_rem_covs_%s.rds",name_i))
  saveRDS(l, file = sprintf("acquisitions_cov_list_%s.rds",name_i))
  saveRDS(list(df.verts=df.verts, acq.src.allpd=acq.src.allpd), file = sprintf("acquisitions_verts_df_%s.rds",name_i))
  
  # # CovRec <- sapply(df.verts.pd.cov$name, function(name) ifelse(name %in% V(g.pd.orig)$name, 1, 0))
  # tmp <- sapply(1:nrow(df.verts),function(x)as.integer(V(g.pd.orig)[which(V(g.pd.orig)$name==df.verts$name[x])]$nc))
  # CovRec <- as.matrix(dist(x = tmp,method = 'manhattan',diag = T, upper = T))
  # CovRec[is.na(CovRec)] <- 0
  # CovRec[CovRec > 0] <- -1 ## not same nc
  # CovRec[CovRec == 0] <- 1 ## same nc
  # CovRec[CovRec < 0] <- 0
  saveRDS(list(ar.cov.rec=ar.cov.rec), file = sprintf("acquisitions_cov_rec_%s.rds",name_i))
  
  
  
#-----------------------------------------------------------------------
l1 <- readRDS(sprintf("acquisitions_verts_df_%s.rds",name_i))
l2 <- readRDS(sprintf("acquisitions_rem_covs_%s.rds",name_i))
l3 <- readRDS(sprintf("acquisitions_cov_list_%s.rds",name_i))
l4 <- readRDS(sprintf("acquisitions_cov_rec_%s.rds",name_i))
df.verts <- l1$df.verts
acq.src.allpd <- l1$acq.src.allpd
df.rem <- l2$df.rem
ar.cov <- l2$ar.cov
df.verts.pd.cov <- l3[[1]]$cov
ar.cov.rec <- l4$ar.cov.rec
#----------------------- RELATIONAL EVENT MODEL ------------------------

## ----------------- Correlations ---------------------------------------
dim(ar.cov)

cov.idx <-  c(1,2,3,4, 7, 15,16,17)
ar.cov.na0 <- ar.cov[ , cov.idx, ]
ar.cov.na0[is.na(ar.cov.na0)] <- 0

# dms <- dim(ar.cov.na0)
# ar.cov.2 <- array(dim=c(dms[1],length(iv.idx)+1,dms[3]))
# ar.cov.2[ , 1:length(iv.idx), ] <-ar.cov.na0
# ar.cov.2[ , 8, ] <- ar.cov.na0[ , 1, ] * ar.cov.na0[ , 5, ]
ar.cov.2 <- ar.cov.na0

df.cor <- data.frame()
for (i in 1:dim(ar.cov.2)[2]) {
  df.cor <- rbind(df.cor, c(ar.cov.2[ , i, ]))  #flatten time x node covariates matrix for each predictor
}
df.cor <- t(df.cor)

library(psych)


df.desc <- psych::describe(df.cor)
write.csv(df.desc, file = 'acqrem_summary_stats.csv', row.names = F)

m.cor <- cor(df.cor)
write.csv(m.cor, file = 'acqrem_cor.csv', row.names = F)

df.cor.test <- psych::corr.test(as.data.frame(df.cor) )
write.csv(df.cor.test$p, file = 'acqrem_cor_pval.csv', row.names = F)


## PLOT TIMESERIES
firm.names <- c('ibm','microsoft','oracle','salesforce','opentext','adobe-systems')
matplot(ar.cov[,1,which(df.verts$name %in% firm.names)], 
        xlab='Acquisition Order (2014 - 2017)', 
        lwd=2, ylab='Total Firm-to-Market MMC',
        type='l', col=1:6, lty = 1:6)
legend('left',legend = firm.names, lty=1:6, col=1:6, lwd=2)


clrs <- rainbow(n=length(unique(V(g.pd.orig)$nc)), s=.7, v=.75)[V(g.pd.orig)$nc]
set.seed(14380)
png("ibm_comp_net_1.png", height = 6.5, width=7, units = 'in', res=200)
par(mar=c(.1,.1,.1,.1))
plot(g.pd.orig, 
     vertex.size=4, 
     vertex.color=clrs, 
     vertex.label='',
     layout=layout.kamada.kawai)
dev.off()



tmp1 <- readRDS('acq_rem_m1.rds')
tmp2 <- readRDS('acq_rem_fit_m4.rds')
tmp3 <- readRDS('acq_rem_fit_m3n1.rds')
tmp4 <- readRDS('acq_rem_fit_m8.rds')

fits <- list(tmp1$fit,tmp2$fit,tmp3$fit,tmp4$fit)

coef.name = c('Acquisition Experience', 'Num. Markets', 
              'Degree Centrality','Founded Year','Ownership Status (1=public)')
htmlreg(tmp1, digits = 3, file = "acq_rem_results_m1.html", custom.coef.names = coef.name)

coef.name = c('Acquisition Experience', 'Firm-to-market MMC', 'Firm-to-market MMC Squared',
               'Num. Markets',  'Degree Centrality','Founded Year','Ownership Status (1=public)')
htmlreg(tmp2, digits = 3, file = "acq_rem_results_m2.html",  custom.coef.names = coef.name)

coef.name = c('Acquisition Experience', 'Power Centrality',
              'Num. Markets',   'Degree Centrality','Founded Year','Ownership Status (1=public)')
htmlreg(tmp3, digits = 3, file = "acq_rem_results_m3.html",  custom.coef.names = coef.name)

coef.name = c('Acquisition Experience', 'Firm-to-market MMC', 'Firm-to-market MMC Squared', 'Power Centrality',
              'Num. Markets', 'Degree Centrality','Founded Year','Ownership Status (1=public)')
htmlreg(tmp4, digits = 3, file = "acq_rem_results_m4.html",  custom.coef.names = coef.name)


#----------------------- RELATIONAL EVENT MODEL ------------------------

# rem.dyad(edgelist, n, effects = NULL, ordinal = TRUE, acl = NULL,
#          cumideg = NULL, cumodeg = NULL, rrl = NULL, covar = NULL, ps = NULL,
#          tri = NULL, optim.method = "BFGS", optim.control = list(), 
#          coef.seed = NULL, hessian = FALSE, sample.size = Inf, verbose = TRUE, 
#          fit.method = c("BPM", "MLE", "BSIR"), conditioned.obs = 0, 
#          prior.mean = 0, prior.scale = 100, prior.nu = 4, sir.draws = 500, 
#          sir.expand = 10, sir.nu = 4, gof = TRUE)

# effects <- c('NODSnd','NODRec', 
#              'PSA0-X0','PSA0-XA','PSA0-XY','PSAB-X0','PSAB-XA',
#              'PSAB-XY','PSA0-AY','PSAB-A0','PSAB-AY')  ## 'CovSnd','CovEvent'

# for (col in c('i.mmc.sum','i.num.mkts','j.mmc.sum','j.num.mkts'))
#   df.rem[ , col] <- as.numeric(df.rem[, col])

df.rem$is.nc <- sapply(1:nrow(df.rem), function(x)all(!is.na(df.rem$j.nc[x]) && df.rem$i.nc[x] == df.rem$j.nc[x]))

effects <- c('CovSnd')  ## 'CovSnd','CovEvent'
covar <- list(CovSnd=ar.cov)

# df.rem$s.f <- sapply(acq.src.allpd$acquirer_name_unique, function(x)df.verts$id[which(x==df.verts$name)])
# df.rem$t.f <- sapply(acq.src.allpd$acquiree_name_unique, function(x)df.verts$id[which(x==df.verts$name)])
el <- data.frame(
  t = 1:nrow(acq.src.allpd),
  s.f = sapply(acq.src.allpd$acquirer_name_unique, function(x)df.verts$id[which(x==df.verts$name)]),
  t.f = sapply(acq.src.allpd$acquiree_name_unique, function(x)df.verts$id[which(x==df.verts$name)]),
  stringsAsFactors = F
)

# rf4 <- rem.dyad(el, n = n, effects = effects, covar=covar,
#                 ordinal = F, hessian = T)

el <- ldply(1:nrow(acq.src.allpd))

summary(rf4)




data("atus80ord", package="informR")
head(atus80ord)
data("atus80int", package = "informR")
head(atus80int)
atus80ord[which(is.na(atus80ord[, "Activities"])), "Activities"] <- "MISSING"
rawevents <- cbind(atus80ord$Activities, atus80ord$TUCASEID)
evls <- gen.evl(rawevents, null.events = "MISSING")
alpha.ints <- gen.intercepts(evls, basecat = "Sleeping")


ev.types <-  sapply(1:nrow(df.rem),function(i) {
  ifelse(is.na(df.rem$j.nc[i]), 'diverge-global',
         ifelse(df.rem$i.nc[i] != df.rem$j.nc[i], 'diverge-local',
                'converge'))
})
rawevents1 <- cbind(ev.types, as.character(df.rem$i.nc))

evls1 <- gen.evl(rawevents1)
alpha.ints1 <- gen.intercepts(evls1, basecat = 'diverge-global') ## 2= local

alpha.fit1 <- rem(eventlist = evls1, statslist = alpha.ints1, 
                 estimator = "BPM", prior.param = list(mu = 0, sigma = 100 , nu = 4))
summary(alpha.fit1)


a1 <- paste(evls$event.key[-9, 1], evls$event.key[-9, 1], sep = "")
beta.sforms <- gen.sformlist(evls, a1)


# el <- data.frame(
#   t = df.rem$t,
#   s.f = sapply(df.rem$src, function(x)df.verts$id[which(x==df.verts$name)]),
#   t.f = sapply(df.rem$trg, function(x)df.verts$id[which(x==df.verts$name)]),
#   stringsAsFactors = F
# )
# el <- data.frame(
#   t = df.rem$t,
#   s.f = sapply(acq.src.allpd$acquirer_name_unique, function(x)df.verts$id[which(x==df.verts$name)]),
#   t.f = sapply(acq.src.allpd$acquiree_name_unique, function(x)df.verts$id[which(x==df.verts$name)]),
#   stringsAsFactors = F
# )
# acq.src.allpd <- acq.src.allpd[order(acq.src.allpd$acquired_on), ]
# ts <- sapply(acq.src.allpd$acquired_on, function(x)as.numeric(ymd(x)))
el <- data.frame(
  t = df.rem$t,
  s.f = sapply(as.character(df.rem$src), function(x)df.verts$id[which(x==df.verts$name)]),
  t.f = sapply(as.character(df.rem$trg), function(x)df.verts$id[which(x==df.verts$name)]),
  stringsAsFactors = F
)
effects <- c('NODSnd', 'CovSnd', 'PSAB-XA','PSAB-XY','PSAB-AY')
ar.cov.na0 <- ar.cov 
ar.cov.na0[is.na(ar.cov.na0)] <- 0 
covar <- list(CovSnd=ar.cov.na0)
fit.rem.bpm.1 <- rem.dyad(edgelist = el, n = nrow(df.verts), effects = effects, ordinal = T, 
                    covar = covar, fit.method = "BPM", gof=F, hessian = T, verbose = T)
summary(fit.rem.bpm.1)
saveRDS(list(fit.rem.bpm.1=fit.rem.bpm.1), file = "acquisitions_fit_rem_245.rds")

screenreg(list(fit.rem.bpm.1), digits=3, 
          custom.coef.names = c('Acquisition Experience', 'FM-MMC','FM-MMC Squared',
                                'Num. Markets','Degree Centrality','Power Centrality'))




# ##
# tmp.nc <- data.frame(name=df.rem$src,nc=df.rem$i.nc)
# tmp.nc <- unique(rbind(tmp.nc, data.frame(name=df.rem$trg,nc=df.rem$j.nc)))
# tmp.nc <- tmp.nc[which(tmp.nc$name %in% df.verts.pd.cov$name), ]
# 
# df.nc <- merge(df.verts, tmp.nc, by='name', all.x = T, all.y=F)
# df.nc[is.na(df.nc)] <- 9999
# m.nc <- as.matrix(dist(df.nc$nc, method = 'manhattan',diag = T, upper = T))
# m.nc[m.nc > 0] <- -99 ## mark diff nc
# m.nc[m.nc == 0] <- 1  ## set same nc to 1
# m.nc[m.nc < 0] <- 0   ## set diff nc to 0
# CovRec <- m.nc


#------------------------- MODEL 1 - Add receiver ---------------------------------------

el <- data.frame(
  t = df.rem$t,
  s.f = sapply(as.character(df.rem$src), function(x)df.verts$id[which(x==df.verts$name)]),
  t.f = sapply(as.character(df.rem$trg), function(x)df.verts$id[which(x==df.verts$name)]),
  stringsAsFactors = F
)

effects <- c('NODSnd', 'CovSnd', 'CovRec')
##  [1] mmc.sum,     mmc.sum.sq,   num.mkts,   deg,      pow.n5,  
##  [6] pow.n3,      pow.n1,       pow.1,      pow.3,    pow.5,      
## [11] betweenness, constraint,   eig
cov.idx <- c(1,2,3,4,  6)
ar.cov.na0 <- ar.cov[ , cov.idx, ]
ar.cov.na0[is.na(ar.cov.na0)] <- 0
##
covar <- list(CovSnd=ar.cov.na0, CovRec=CovRec)
fit.rem.bpm.1 <- rem.dyad(edgelist = el, n = nrow(df.verts), effects = effects, ordinal = F, 
                    covar = covar, fit.method = "BPM", gof=F, hessian = T, verbose = T)
summary(fit.rem.bpm.1)
saveRDS(list(fit.rem.bpm.1=fit.rem.bpm.1), file = "acquisitions_fit_rem_rec_pshift_245_13_1.rds")

#---------------------------------------------------------------------------

screenreg(list(fit.rem.bpm.1), digits=3, 
          custom.coef.names = c('Acquisition Experience', 'FM-MMC','FM-MMC Squared',
                                'Num. Markets','Degree Centrality',
                                'Power Centrality (b=-0.3)','Converging Acq. ("local" target)'))


#------------------------- MODEL 1B - no sender stat ---------------------------------------
effects <- c('CovSnd', 'CovRec')
##  [1] mmc.sum,     mmc.sum.sq,   num.mkts,   deg,      pow.n5,  
##  [6] pow.n3,      pow.n1,       pow.1,      pow.3,    pow.5,      
## [11] betweenness, constraint,   eig
cov.idx <- c(1,2,3,4,  6)
ar.cov.na0 <- ar.cov[ , cov.idx, ]
ar.cov.na0[is.na(ar.cov.na0)] <- 0
##
covar <- list(CovSnd=ar.cov.na0, CovRec=CovRec)
fit.rem.bpm.1b <- rem.dyad(edgelist = el, n = nrow(df.verts), effects = effects, ordinal = F, 
                          covar = covar, fit.method = "BPM", gof=F, hessian = T, verbose = T)
summary(fit.rem.bpm.1b)
saveRDS(list(fit.rem.bpm.1b=fit.rem.bpm.1b), file = "acquisitions_fit_rem_rec_pshift_245_13_1b.rds")

#------------------------- MODEL 2 - All Sender (not receiver)  ------------------------------

effects <- c('NODSnd', 'CovSnd')
##  [1] mmc.sum,     mmc.sum.sq,   num.mkts,   deg,      pow.n5,  
##  [6] pow.n3,      pow.n1,       pow.1,      pow.3,    pow.5,      
## [11] betweenness, constraint,   eig
cov.idx <- c(1,2,3,4,  6,  11,12)
ar.cov.na0 <- ar.cov[ , cov.idx, ]
ar.cov.na0[is.na(ar.cov.na0)] <- 0
##
covar <- list(CovSnd=ar.cov.na0)
fit.rem.bpm.2 <- rem.dyad(edgelist = el, n = nrow(df.verts), effects = effects, ordinal = F, 
                          covar = covar, fit.method = "BPM", gof=F, hessian = T, verbose = T)
summary(fit.rem.bpm.2)
saveRDS(list(fit.rem.bpm.2=fit.rem.bpm.2), file = "acquisitions_fit_rem_rec_pshift_245_13_2.rds")
#---------------------------------------------------------------------------


#------------------------- MODEL 3 ----------------------------------------------

effects <- c('NODSnd', 'CovSnd', 'CovRec')
##  [1] mmc.sum,     mmc.sum.sq,   num.mkts,   deg,      pow.n5,  
##  [6] pow.n3,      pow.n1,       pow.1,      pow.3,    pow.5,      
## [11] betweenness, constraint,   eig
cov.idx <- c(1,2,3,4,  6,  11,12)
ar.cov.na0 <- ar.cov[ , cov.idx, ]
ar.cov.na0[is.na(ar.cov.na0)] <- 0
##
covar <- list(CovSnd=ar.cov.na0, CovRec=CovRec)
fit.rem.bpm.3 <- rem.dyad(edgelist = el, n = nrow(df.verts), effects = effects, ordinal = F, 
                          covar = covar, fit.method = "BPM", gof=F, hessian = T, verbose = T)
summary(fit.rem.bpm.3)
saveRDS(list(fit.rem.bpm.3=fit.rem.bpm.3), file = "acquisitions_fit_rem_rec_pshift_245_13_3.rds")
#---------------------------------------------------------------------------



































# for (i in 2:length(times)) {
#   start <- times[i-1]
#   end <- times[i]
#   ## make period graph
#   g.pd <- makePdGraph(g.ego, start, end, isolates.remove=TRUE)
#   ## period NC
#   V(g.pd)$nc <- as.integer(igraph::multilevel.community(g.pd)$membership)
#   ## keep original pd graph
#   g.pd.orig <- g.pd
#   ## filter acquisitions made by firms in this period graph
#   acq.src <- co_acq[which(co_acq$acquirer_name_unique %in% V(g.pd)$name), ]
#   ## filter acquisitions made during this period
#   acq.src.pd <- acq.src[which(acq.src$acquired_on >= start & acq.src$acquired_on < end) , ]
#   acq.src.pd <- acq.src.pd[order(acq.src.pd$acquired_on, decreasing = F), ]
#   
#   ## ACQUISITION EVENTS:  UPDATE MMC & DYNAMIC EFFs
#   for (j in 1:nrow(acq.src.pd)) {
#     cat(sprintf('\n\nstart %s end %s : acquisition %s\n\n',start,end,j))
#     lidx <- lidx + 1
#     l[[lidx]] <- list()
#     ## Update MMC after acquisition
#     l[[lidx]]$mmc <- getFmMmc(g.pd, as.integer(V(g.pd)$nc))
#     
#     ## SUM FM MMC over markets  ??????
#     V(g.pd)$fm.mmc.sum <- rowSums(l[[lidx]]$mmc)
#     V(g.pd)$num.mkts <- apply(l[[lidx]]$mmc,MARGIN=1,FUN=function(x){
#       return(length(x[x>0]))
#     })
#     ## save current data in dataframe
#     df.pd <- data.frame(fm.mmc.sum=V(g.pd)$fm.mmc.sum, 
#                         num.mkts=V(g.pd)$num.mkts,
#                         nc=V(g.pd)$nc)
#     df.mmc <- rbind(df.mmc, df.pd)
#     ## GET REM DATAFRAME VARS
#     xi.orig.vid <- acq.src.pd$acquirer_vid[j]
#     xj.orig.vid <- acq.src.pd$acquiree_vid[j]
#     xi <- as.integer(V(g.pd)[V(g.pd)$name==acq.src.pd$acquirer_name_unique[j]])
#     xi.nc <- as.integer(V(g.pd.orig)$nc[xi]) ## original nc for the period
#     xi.mmc.sum <-  V(g.pd)$fm.mmc.sum[xi]
#     xi.num.mkts <-  V(g.pd)$num.mkts[xi]
#     xi.deg <- igraph::degree(g.pd)[xi]
#     xi.pow <- igraph::power_centrality(g.pd, exponent = -0.5)[xi]
#     xj <- as.integer(V(g.pd)[V(g.pd)$orig.vid==xj.orig.vid])
#     xj.nc <- ifelse(length(xj)==0,NA,  V(g.pd.orig)$nc[xj] )  ## original nc for the period
#     xj.mmc.sum <- ifelse(length(xj)==0,NA,  V(g.pd)$fm.mmc.sum[xj] )
#     xj.num.mkts <- ifelse(length(xj)==0,NA,  V(g.pd)$num.mkts[xj] )
#     xj.deg <- ifelse(length(xj)==0,NA,  igraph::degree(g.pd)[xj] )
#     xj.pow <- ifelse(length(xj)==0,NA,  igraph::power_centrality(g.pd, exponent = -0.5)[xj] )
#     src <- ifelse(length(acq.src.pd$acquirer_name_unique[j])>0, 
#                   acq.src.pd$acquirer_name_unique[j], NA)
#     trg <- ifelse(length(acq.src.pd$acquiree_name_unique[j])>0, 
#                   acq.src.pd$acquiree_name_unique[j], NA)
#     datestr <- acq.src.pd$acquired_on[j]
#     timeval.last <- timeval
#     timeval <- as.integer(ymd(datestr))
#     timeval <- ifelse(timeval%in%df.rem$t, timeval.last + 0.01, timeval)
#     # MAKE REM DATAFRAME
#     df.rem.pd <- data.frame(t=timeval, src=src, trg=trg, 
#                             time=datestr, idx=lidx,
#                             i.orig.vid=xi.orig.vid,
#                             j.orig.vid=xj.orig.vid,
#                             i=xi, 
#                             i.nc=xi.nc, i.mmc.sum=xi.mmc.sum, i.num.mkts=xi.num.mkts, 
#                             i.deg=xi.deg,i.pow=xi.pow,
#                             j=ifelse(length(xj)==0,NA,xj), 
#                             j.nc=xj.nc, j.mmc.sum=xj.mmc.sum, j.num.mkts=xj.num.mkts, 
#                             j.deg=xj.deg, j.pow=xj.pow
#     )
#     df.rem <- rbind(df.rem, df.rem.pd)
#     ## SAVE COVARIATE ARRAY [m,p,n] for m times, p covars, n actors
#     df.pd.cov <- data.frame(name=V(g.pd)$name, 
#                             mmc.sum=as.numeric(V(g.pd)$fm.mmc.sum), 
#                             mmc.sum.sq=as.numeric(V(g.pd)$fm.mmc.sum)^2,
#                             num.mkts=as.numeric(V(g.pd)$num.mkts),
#                             deg=igraph::degree(g.pd),
#                             pow=igraph::power_centrality(g.pd, exponent = -0.5))
#     df.verts.pd.cov <- merge(df.verts, df.pd.cov, by = 'name', all.x = T)
#     df.verts.pd.cov <- df.verts.pd.cov[order(df.verts.pd.cov$id),]
#     l[[lidx]]$cov <- df.verts.pd.cov
#     ar.cov[j, 1, ] <- df.verts.pd.cov$mmc.sum
#     ar.cov[j, 2, ] <- df.verts.pd.cov$mmc.sum.sq
#     ar.cov[j, 3, ] <- df.verts.pd.cov$num.mkts
#     ar.cov[j, 4, ] <- df.verts.pd.cov$deg
#     ar.cov[j, 5, ] <- df.verts.pd.cov$pow
#     ## NODE COLLAPSE update network
#     g.pd <- nodeCollapseGraph(g.pd, acq.src.pd[j,])
#   }
#   saveRDS(list(df.rem=df.rem, ar.cov=ar.cov), file = "acquisitions_rem_covs.rds")
# }
# saveRDS(l, file = "acquisitions_cov_list.rds")
















# ## cache original
# firm.nets.orig <- firm.nets



## run main network period creation loop
firms.todo <- 'ibm'
i <- 1
# for (i in 1:length(firms.todo)) {
## -- settings --
d <- 2
yrpd <- 1
startYr <- 2007
endYr <- 2017
## --------------
name_i <- firms.todo[i]
cat(sprintf('\n---------%s----------\n',name_i))
periods <- seq(startYr,endYr,yrpd)
company.name <- 'company_name_unique'
verbose <- TRUE
#
#g.base <- igraph::make_ego_graph(g.full,order=k,nodes=V(g.full)[V(g.full)$name=='surveymonkey'])[[1]]
g.base <- g.full
g.k.sub <- igraph::make_ego_graph(graph = g.base, 
                                  nodes = V(g.full)[V(g.full)$name==name_i], 
                                  order = d, mode = 'all')[[1]]
net.k.sub <- asNetwork(g.k.sub)
net.k.sub %n% 'ego' <- name_i
net <- net.k.sub
#----------------Network List-------------------
nl <- list()
for (t in 2:length(periods)) {
  cat(sprintf('\nmaking period %s-%s:\n', periods[t-1],periods[t]))
  tmp.net <- makePdNetworkTransfer(net.k.sub,
                                   start=periods[t-1], end=periods[t])
  nl[[t]] <- setCovariates(tmp.net, periods[t-1], periods[t],
                           netRiskCommunityAlgo='multilevel.community',
                           downweight.env.risk=FALSE,
                           acq=co_acq,br=co_br,rou=co_rou,ipo=co_ipo)
}
nl.bak <- nl
nl <- nl[which(sapply(nl, length)>0)]
names(nl) <- periods[2:length(periods)]
## ---------- add LAGS ----------------
for (t in 2:length(nl)) {
  nl[[t]] %n% 'DV_lag' <- nl[[t-1]][,]
  nl[[t]] %n% 'dist_lag' <- nl[[t-1]] %n% 'dist'
  ##-------------------------------------------
  # nl[[t]] %v% 'net_risk_lag' <- nl[[t-1]] %v% 'net_risk'
  # nl[[t]] %v% 'cent_deg_lag' <- nl[[t-1]] %v% 'cent_deg'
  # nl[[t]] %v% 'genidx_multilevel_lag' <- nl[[t-1]] %v% 'genidx_multilevel'
  # nl[[t]] %v% 'cent_pow_n0_5_lag' <- nl[[t-1]] %v% 'cent_pow_n0_5'
  # nl[[t]] %v% 'cent_pow_n0_1_lag' <- nl[[t-1]] %v% 'cent_pow_n0_1'
  # nl[[t]] %v% 'cent_eig_lag' <- nl[[t-1]] %v% 'cent_eig'
  # nl[[t]] %v% 'cent_deg_lag' <- nl[[t-1]] %v% 'cent_deg'
  # g.tmp <- getIgraphFromNet(nl[[t]])
  # if (vcount(g.tmp)>0 & ecount(g.tmp)>0) {
  #   nl[[t]] %v% 'constraint' <- igraph::constraint(g.tmp)
  # }
}
##--------------- GET TERGM NETS LIST -----------
## only nets with edges > 0
nets.all <- nl[2:length(nl)]
nets <- nets.all[ which(sapply(nets.all, getNetEcount) > 0) ]
#-------------------------------------------------

# ## SAVE variable in image
# # firm.nl <- list()
# firm.nets[[net_group]][[name_i]] <- nets

# ## CAREFUL TO OVERWRITE
# file.name <- sprintf('tergm_firm_nets_1yr_6pd_v4_%s.rds',net_group)
# saveRDS(firm.nets, file=file.name)
## CAREFUL TO OVERWRITE
saveRDS(nets, file=sprintf('firm_nets_cem/%s_d%s.rds', name_i, d))
gc()
}



##--------------------------------------------------------------
##--------------------------------------------------------------
##--------- CREATE FIRM NETWORK PERIOD LISTS  ------------------
##--------- SAVE EACH PD NET SEPARATELY       ------------------
##--------------------------------------------------------------
##--------------------------------------------------------------
firms.todo <- 'qualtrics'

## -- settings --
d <- 2   ## large graph > 1000 nodes, save years separately for one firm
yrpd <- 1
startYr <- 2010
endYr <- 2017

## --------------
i <- 1
name_i <- firms.todo[i]
cat(sprintf('\n---------%s----------\n',name_i))
periods <- seq(startYr,endYr,yrpd)
company.name <- 'company_name_unique'
verbose <- TRUE
#
#g.base <- igraph::make_ego_graph(g.full,order=k,nodes=V(g.full)[V(g.full)$name=='surveymonkey'])[[1]]
g.base <- g.full
g.k.sub <- igraph::make_ego_graph(graph = g.base, nodes = V(g.full)[V(g.full)$name==name_i], 
                                  order = d, mode = 'all')[[1]]
net.k.sub <- getNetFromIgraph(g.k.sub)
net.k.sub %n% 'ego' <- name_i
net <- net.k.sub
#----------------Network List-------------------
for (t in 2:length(periods)) {
  nl <- list()
  pd <- as.character(periods[t])
  cat(sprintf('\nmaking period %s-%s:\n', periods[t-1],periods[t]))
  tmp.net <- makePdNetwork(net.k.sub,
                           start=periods[t-1], end=periods[t])
  cat(sprintf("\n nodes : %s \n",nrow(tmp.net[,])))
  nl[[pd]] <- setCovariates(tmp.net, periods[t-1], periods[t],
                            netRiskCommunityAlgo='multilevel.community',
                            downweight.env.risk=FALSE,
                            acq=co_acq,br=co_br,rou=co_rou,ipo=co_ipo)
  
  ## CAREFUL TO OVERWRITE
  saveRDS(nl, file=sprintf('firm_nets_cem/%s_d%s_pd%s.rds', name_i, d, periods[t]))
  rm(nl)
  gc()
}
################################################################
################################################################
