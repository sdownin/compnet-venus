setwd("C:/Users/T430/Google Drive/PhD/Dissertation/competition networks/compnet2")
# .libPaths('C:/Users/T430/Documents/R/win-library/3.2')
library(parallel)
library(statnet, quietly = T)
library(network, quietly = T)
library(xergm, quietly = T)  ## includes rem, tnam, GERGM
library(texreg, quietly = T)
library(igraph, quietly = T)
library(plyr, quietly = T)
library(lattice, quietly = T)
library(latticeExtra, quietly = T)
library(ggplot2, quietly = T)
library(reshape2)
library(relevent)
library(informR)

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
# ## set default attrs for contractions
# V(g.full)$orig.vid <- as.integer(V(g.full))
# V(g.full)$weight <- 1
# E(g.full)$weight <- 1
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
name_i <- 'google'
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

##----------------------------------------------------------------------
## Make Ego Net to use
##_---------------------------------------------------------------------
d <- 2
name_i <- 'yahoo'
g.ego <- igraph::make_ego_graph(graph = g.full, 
                                nodes = V(g.full)[V(g.full)$name==name_i], 
                                order = d, mode = 'all')[[1]]
print(c(v=vcount(g.ego),e=ecount(g.ego)))
# print(c(range=range(E(g.ego)$relation_began_on)))
## comp relations
plyr::count(sapply(E(g.ego)$relation_began_on,function(x)year(ymd(x))))
# g.ego <- induced.subgraph(g.ego, V(g.ego)[igraph::degree(g.ego)>1])
mem <- igraph::multilevel.community(g.ego)$membership
mem.cnt <- plyr::count(mem)
mem.cnt <- mem.cnt[order(mem.cnt$freq, decreasing = T), ]
dim(mem.cnt)
##  
acq.src <- co_acq_d[which(co_acq_d$acquirer_name_unique %in% V(g.ego)$name), ]
acq.src.trg <- co_acq_d[which(co_acq_d$acquirer_name_unique %in% V(g.ego)$name
                              & co_acq_d$acquiree_name_unique %in% V(g.ego)$name), ]
## plot 
V(g.ego)$color <- rainbow(length(unique(mem)))[mem]
par(mar=c(.1,.1,.1,.1))
plot(g.ego, vertex.size=log(1+igraph::degree(g.ego))*2, 
     vertex.label.cex=log(1+igraph::degree(g.ego))*.01)
set.seed(111)
plot(g.ego, vertex.size=3, vertex.label.cex=.06, layout=layout.fruchterman.reingold)
## contracted NC graph
V(g.ego)$weight <- 1
E(g.ego)$weight <- 1
V(g.ego)$mem <- mem
g.ego.m <- igraph::contract.vertices(g.ego, mem, vertex.attr.comb = list(weight="sum",
                                                                         name=function(x)x[1],
                                                                         mem=function(x)x[1]))
g.ego.m <- igraph::contract.vertices(g.ego, mem)
g.ego.m <- igraph::simplify(g.ego.m,remove.multiple = T, remove.loops = T, edge.attr.comb = list(weight="sum"))
plot(g.ego.m, edge.width=E(g.ego.m)$weight * .9, vertex.label=V(g.ego.m)$mem)

##----------------------------------------------------------------------
##_---------------------------------------------------------------------



#plot activity:
n=100
idx <- (nrow(acq.src)-n+1):nrow(acq.src)
x <- ymd(acq.src$acquired_on[idx])
y <- as.factor(acq.src$acquirer_name[idx])
plot(x,y,pch=16); abline(h=y)



# # Top Acquirers:         x  freq
# # 5343              Google  204
# # 2656               Cisco  197
# # 8070           Microsoft  186
# # 6040                 IBM  170
# # 13985             Yahoo!  120
# # 9234  Oracle Corporation  113
# # 5779     Hewlett-Packard   99
# # 941                Apple   82
# # 6449               Intel   82
# # 9435     Parker Hannifin   74
# # 4093                 EMC   73
# # 890                  AOL   68
# # 712               Amazon   66
# # 48                    3M   65
# # 4528            Facebook   64
# # 11968           Symantec   60
# # 3923                eBay   59
# # 1338               Avnet   54
# # 12908            Twitter   48
# # 1596  Berkshire Hathaway   46
# 
# ## Competition Network (d=2) Size:
# # apple      1101 2187
# # google     2100 4598
# # IBM         828 1710 
# # Microsoft  1873 3925
# # cisco       220  406
# # oracle     1248 2456
# 
# ## FIND FOCAL FIRM 
# name_i <- 'cisco'
# d <- 2
# 
# g.ego <- igraph::make_ego_graph(graph = g.full, 
#                                 nodes = V(g.full)[V(g.full)$name==name_i], 
#                                 order = d, mode = 'all')[[1]]
# g.ego
# 
# par(mar=c(.1,.1,.1,.1))
# deg=log(1+igraph::degree(g.ego))
# plot(g.ego, vertex.size=deg*4, vertex.label.cex=deg*.3)

##--------------------------------------------------------------
##--------------------------------------------------------------
##--------- CREATE FIRM NETWORK PERIOD LISTS  ------------------
##--------------------------------------------------------------
##--------------------------------------------------------------


name_i <- 'dell'
d <- 3
times <- sapply(2014:2017, function(x)paste0(x,'-01-01'))

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
## check number of acquisitions to counts
acq.src.trg.allpd <- acq.src.allpd[which(acq.src.allpd$acquirer_name_unique %in% V(g.ego)$name 
                                       & acq.src.allpd$acquiree_name_unique %in% V(g.ego)$name), ]
dim(acq.src.trg.allpd)
## get all verts in the period (first either acquired or acquirer)
acq.verts <- unique(c(as.character(acq.src.trg.allpd$acquirer_name_unique), 
                      as.character(acq.src.trg.allpd$acquiree_name_unique)))
df.verts <- data.frame(id=1:length(acq.verts), name=acq.verts, stringsAsFactors = F)
##
p <- 15 ## i.mmc, i.mmc^2, num.mkts, deg, power
m <- nrow(acq.src.trg.allpd)
n <- nrow(df.verts)
ar.cov <- array(dim=c(m,p,n))
    

  start <- times[1]
  end <- times[length(times)]
  ## make period graph
  g.pd <- makePdGraph(g.ego, start, end, isolates.remove=TRUE)
  ## period NC
  V(g.pd)$nc <- as.integer(igraph::multilevel.community(g.pd)$membership)
  ## keep original pd graph
  g.pd.orig <- g.pd
  ## filter acquisitions made by firms in this period graph
  acq.src <- co_acq[which(co_acq$acquirer_name_unique %in% V(g.pd)$name), ]
  ## filter acquisitions made during this period
  acq.src.pd <- acq.src[which(acq.src$acquired_on >= start & acq.src$acquired_on < end) , ]
  acq.src.pd <- acq.src.pd[order(acq.src.pd$acquired_on, decreasing = F), ]
  
  ## ACQUISITION EVENTS:  UPDATE MMC & DYNAMIC EFFs
  for (j in 1:nrow(acq.src.allpd)) {
    cat(sprintf('\n\nstart %s end %s : acquisition %s\n\n',start,end,j))
    
    if (acq.src.allpd$acquiree_name_unique[j] %in% V(g.pd.orig)$name) {
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
        # xi.orig.vid <- acq.src.pd$acquirer_vid[j]
        # xj.orig.vid <- acq.src.pd$acquiree_vid[j]
        xi.orig.vid <- V(g.pd.orig)$orig.vid[which(acq.src.allpd$acquirer_name_unique[j] == V(g.pd.orig)$name)]
        xj.orig.vid <- V(g.pd.orig)$orig.vid[which(acq.src.allpd$acquiree_name_unique[j] == V(g.pd.orig)$name)]
        xi <- as.integer(V(g.pd)[V(g.pd)$name==acq.src.pd$acquirer_name_unique[j]])
        xi.orig <- as.integer(V(g.pd.orig)[V(g.pd.orig)$name==acq.src.pd$acquirer_name_unique[j]])
        xi.nc <- as.integer(V(g.pd.orig)$nc[xi.orig]) ## original nc for the period
        xi.mmc.sum <-  V(g.pd)$fm.mmc.sum[xi]
        xi.num.mkts <-  V(g.pd)$num.mkts[xi]
        xi.deg <- igraph::degree(g.pd)[xi]
        xi.pow <- igraph::power_centrality(g.pd, exponent = -0.3, sparse = T)[xi]
        ##
        xj <- as.integer(V(g.pd)[V(g.pd)$orig.vid==xj.orig.vid])
        xj.orig <- as.integer(V(g.pd.orig)[V(g.pd.orig)$orig.vid==xj.orig.vid])
        xj.nc <- ifelse(length(xj)==0,NA,  V(g.pd.orig)$nc[xj.orig] )  ## original nc for the period
        xj.mmc.sum <- ifelse(length(xj)==0,NA,  V(g.pd)$fm.mmc.sum[xj] )
        xj.num.mkts <- ifelse(length(xj)==0,NA,  V(g.pd)$num.mkts[xj] )
        xj.deg <- ifelse(length(xj)==0,NA,  igraph::degree(g.pd)[xj] )
        xj.pow <- ifelse(length(xj)==0,NA,  igraph::power_centrality(g.pd, exponent = -0.3, sparse = T)[xj] )
        src <- ifelse(length(acq.src.pd$acquirer_name_unique[j])>0, 
                      acq.src.pd$acquirer_name_unique[j], NA)
        trg <- ifelse(length(acq.src.pd$acquiree_name_unique[j])>0, 
                      acq.src.pd$acquiree_name_unique[j], NA)
        datestr <- acq.src.pd$acquired_on[j]
        timeval.last <- timeval
        timeval <- as.integer(ymd(datestr))
        timeval <- ifelse(timeval%in%df.rem$t, timeval.last + 0.01, timeval)
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
                                deg=igraph::degree(g.pd),
                                pow.n4=igraph::power_centrality(g.pd, exponent = -0.4),
                                pow.n3=igraph::power_centrality(g.pd, exponent = -0.3),
                                pow.n2=igraph::power_centrality(g.pd, exponent = -0.2),
                                pow.n1=igraph::power_centrality(g.pd, exponent = -0.1),
                                pow.1=igraph::power_centrality(g.pd, exponent = 0.1),
                                pow.2=igraph::power_centrality(g.pd, exponent = 0.2),
                                pow.3=igraph::power_centrality(g.pd, exponent = 0.3),
                                pow.4=igraph::power_centrality(g.pd, exponent = 0.4),
                                betweenness=igraph::betweenness(g.pd),
                                constraint=igraph::constraint(g.pd)  )
        eig <- igraph::eigen_centrality(g.pd)
        if (length(eig$vector)>0)
            df.pd.cov$eig  <- eig$vector
        df.verts.pd.cov <- merge(df.verts, df.pd.cov, by = 'name', all.x = T)
        df.verts.pd.cov <- df.verts.pd.cov[order(df.verts.pd.cov$id),]
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
        if ('eig' %in% names(df.verts.pd.cov))
            ar.cov[lidx,15, ] <- df.verts.pd.cov$eig
    }
    
    ## NODE COLLAPSE update network
    g.pd <- nodeCollapseGraph(g.pd, acq.src.allpd[j,])
    
    if (lidx %% 20 == 0) {
      saveRDS(list(df.rem=df.rem, ar.cov=ar.cov), file = sprintf("acquisitions_rem_covs_%s.rds",name_i))
      saveRDS(l, file = sprintf("acquisitions_cov_list_%s.rds",name_i))
      saveRDS(list(df.verts=df.verts, acq.src.allpd=acq.src.allpd), file = sprintf("acquisitions_verts_df_%s.rds",name_i))
    }
  }
  
  saveRDS(list(df.rem=df.rem, ar.cov=ar.cov), file = sprintf("acquisitions_rem_covs_%s.rds",name_i))
  saveRDS(l, file = sprintf("acquisitions_cov_list_%s.rds",name_i))
  saveRDS(list(df.verts=df.verts, acq.src.allpd=acq.src.allpd), file = sprintf("acquisitions_verts_df_%s.rds",name_i))
  
  # CovRec <- sapply(df.verts.pd.cov$name, function(name) ifelse(name %in% V(g.pd.orig)$name, 1, 0))
  CovRec <- sapply(1:nrow(df.rem), function(i) ifelse(df.rem$i.nc[i] == df.rem$j.nc[i], 1, 0))
  saveRDS(list(CovRec=CovRec), file = sprintf("acquisitions_cov_rec_%s.rds",name_i))
  
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
CovRec <- l4$CovRec
#----------------------- RELATIONAL EVENT MODEL ------------------------

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
