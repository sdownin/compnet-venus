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


name_i <- 'google'
d <- 2
times <- sapply(2013:2016, function(x)paste0(x,'-01-01'))

g.ego <- igraph::make_ego_graph(graph = g.full, 
                                nodes = V(g.full)[V(g.full)$name==name_i], 
                                order = d, mode = 'all')[[1]]
## YEAR PERIODS: DEFINE NICHE CLUSTERS
for (i in 2:length(times)) {
  start <- times[i-1]
  end <- times[i]
  ## make period graph
  g.ego.pd <-   makePdGraph(g.ego, start, end)
  ## period NC
  V(g.ego.pd)$nc <- igraph::multilevel.community(g.ego.pd)$membership
  ## filter acquisitions made by firms in this period graph
  acq.src <- co_acq[which(co_acq$acquirer_name_unique %in% V(g.ego.pd)$name), ]
  ## filter acquisitions made during this period
  acq.src.pd <- acq.src[which(acq.src$acquired_on >= start & acq.src$acquired_on < end) , ]
  ## INITIAL FM MMC
  V(g.ego.pd)$fm.mmc <- fmMmc(g.ego.pd.j, 'nc')
  df.pd <- data.frame(fm.mmc=V(g.ego.pd)$fm.mmc, 
                      nc=V(g.ego.pd)$nc)
  ## ACQUISITION EVENTS:  UPDATE MMC & DYNAMIC EFFs
  for (j in 1:nrow(acqs.src.pd)) {
    g.ego.pd.j <- nodeCollapseGraph(g.ego.pd, acq.src.pd[j,])
    V(g.ego.pd.j)$fm.mmc <- fmMmc(g.ego.pd.j, V(g.ego.pd.j)$nc)
    df.tmp <- data.frame(fm.mmc=V(g.ego.pd.j)$fm.mmc, 
                         nc=V(g.ego.pd.j)$nc)
    df.pd <- rbind(df.pd, df.tmp)
  }
}


##
# Computes the firm-to-market MMC for firms in 
# competition network g with niche clusters defined in 'memberships'
# @param {igraph}       g
# @param {array int[]}  membership
# @return {data.frame [N x M]} The FM-MMC for N firms in M markets
##
fmMmc <- function(g, membership)
{
  markets <- unique(membership)
  markets <- markets[order(markets)]
  adj <- igraph::as_adjacency_matrix(g, sparse = F)
  
  #### 1. 
  ## Create [Firm x Market] incidence matrix
  ## markets of each firm (which markets they are in based on NC membership of their rivals)
  df.ms <- ldply(1:nrow(adj), function(i){
    i.nbr <- unname(which( adj[i, ] == 1 ))  ## rivals (neighbors in the network) ## i.nbr <- as.integer(igraph::neighbors(g, V(g)[i]))  
    i.ms <- unique(membership[i.nbr])  ## markets of firm i (the markets of the rivals of firm i)
    i.ms.row <- rep(0, length(markets)) ## dummy row for [Firm x Market] matrix
    i.ms.row[ i.ms ] <- 1 ## assign [Firm x Market] matrix row value to 1
    names(i.ms.row) <- sapply(1:length(markets),function(x)paste0('m',x))
    return(i.ms.row)
  })
  ## convert df to matrix
  m.ms <- as.matrix(df.ms)
  
  #### 2. 
  ## Create [Firm x Firm] MMC matrix (each M[i,j] = count of markets in contact)
  m.mmc <- m.ms %*% t(m.ms)
  ## set diagonals to 0 (firms don't compete with themselves)
  diag(m.mmc) <- 0
  
  #### 3. 
  ## Get MMC competitors list {j} for each firm i (the competitors with which they have MMC)
  js <- lapply(1:nrow(m.mmc), FUN = function(i)which(m.mmc[i, ] > 1))
  
  #### 4. 
  ## numerator computation:  Sum_m{Sum_j{D_imt * D_jmt}}
  numerator <- ldply(1:nrow(df.ms), function(i){
    tmp <- rep(0, ncol(df.ms))
    ## firm i market indices
    i.ms <- which(df.ms[i,] > 0)
    ## comps j in markets m that i is in 
    js.i.ms <- df.ms[ js[[i]] , i.ms ]
    if (length(js.i.ms) > 0) {
      ## the MMC counts of compj in each market that j is in & i is in
      js.i.ms.cnt <- js.i.ms * m.mmc[i, js[[i]] ]
      js.i.ms.sum <- colSums(js.i.ms.cnt) 
      ## assign the MMC iteraction sums in i's markets with js who are MMC comps
      tmp[ i.ms ] <- js.i.ms.sum
    }
    return(tmp)
  })
  
  #### 5. 
  ## denominator computation:  Sum_m{D_imt * N_mmc_t}
  denominator <- ldply(1:nrow(df.ms), function(i){
    tmp <- rep(0, ncol(df.ms))
    ## firm i market indices
    i.ms <- which(df.ms[i,] > 0)
    D_imt <- length(i.ms)
    ## comps j in markets m that i is in 
    js.i.ms <- df.ms[ js[[i]] , i.ms ]
    if (length(js.i.ms) > 0) {
      ## the MMC counts of compj in each market m that j is in & i is in
      js.i.ms.cnt <- js.i.ms * m.mmc[i, js[[i]] ]
      N.mmc.m <- apply(js.i.ms.cnt, MARGIN = 2, FUN = function(x){
        return( length(x[x > 0]) * D_imt )
      })
      ## assign the MMC iteraction sums in i's markets with js who are MMC comps
      tmp[ i.ms ] <- N.mmc.m
    }
    return(tmp)
  })
  
  ## compute FM-MMC elementwise by component dfs
  fm.mmc <- numerator / denominator
  m.fm.mmc <- as.matrix(fm.mmc)
  ## replace 
  m.fm.mmc[(is.na(m.fm.mmc) | is.nan(m.fm.mmc))] <- 0
  ## convert to dataframe
  fm.mmc <- as.data.frame(m.fm.mmc)
  
  return(as.data.frame(m.fm.mmc))
}

##
##
## TODO
##
##







#### 4. 
## NUMERATOR
## 
numerator <- sapply(1:nrow(m.mmc), FUN=function(i){
  i.sum.j.m <- sum(m.mmc[ i , js[[i]] ]) # firm j in market m subset (where j is a MMC firm for i)
  return(i.sum.j.m)
})
# numerator.i <- ldply(1:nrow(df.ms), function(i){
#   ## rows are firms j that have MMC to firm i
#   ## cols are markets i is in
#   jm.sub <- df.ms[ js[[i]] , which(df.ms[i, ] == 1) ] # firm j in market m subset (where j is a MMC firm for i)
#   return(sum(jm.sub))
# })

#### 4. 
## N_MMC[t] term: 
## number of firms j that contact i in market m that are multimarket contacts
denominator <- ldply(1:nrow(df.ms), function(i){
  ## sum over markets (cols)
  tmp <- df.ms[ js[[i]] , which(df.ms[i, ] == 1) ]
  return(ifelse(nrow(tmp)==0, 0, rowSums(tmp)))
})
## sum over markets m (columns) to compute denominator
sum_m_D_imt.N_mmc <- rowSums(D_imt.N_mmc)

#### 5.
## sum MM interactions (numerator of equation)
sum.ijm <- ldply(1:nrow(df.ms), function(i){
  f.m.mmc <- df.ms[ c(i, js[[i]]), ]
  f.m.mmc.s <-  colSums(f.m.mmc)
  
})


## [Firm x Market] MMC
fm.mmc <- ldply(1:nrow(adj), function(i){
  return(sapply(1:ncol(adj), function(j){
    if ( i == j | !(j %in% js[[i]]) ) {
      return(0); ## if j not MM competitor (or is i), then zero
    } else {
      
    }
  }))
})


## competitors j of firm i who have multimarket contact




## list of competitors
i.m.comp <- lapply(1:length(markets), function(m) {
  m.co <- which(membership == markets[m])      ## firms in market m
  i.nbr.m <- i.nbr[ which(i.nbr %in% m.co) ]   ## rivals who are in market m
  return(i.nbr.m)
})


# n <- vcount(g)
# markets <- unique(membership)
# # adj <- igraph::as_adjacency_matrix(g, sparse = F)
# for (j in 1:(n-1)) {
#   for (i in (j+1):n) {  ## lower triangle, columnwise (loop over rows per column)
#     if (i != j) {
#       ## rivals (neighbors in the network)
#       i.nbr <- as.integer(igraph::neighbors(g, V(g)[i]))
#       j.nbr <- as.integer(igraph::neighbors(g, V(g)[j]))
#       # i.ncs <- which(membership == V(g)$nc[i])
#       # j.ncs <- which(membership == V(g)$nc[j])
#       for (m in 1:length(markets)) {
#         ## m'th market member firms
#         m.co <- which(membership == markets[m])
#         ## rivals in market m
#         i.nbr.m <- i.nbr[ which(i.nbr %in% m.co) ]
#         j.nbr.m <- j.nbr[ which(j.nbr %in% m.co) ]
#       }
#     }
#   }
# }












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
