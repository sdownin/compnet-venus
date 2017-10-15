#---------------------------------------------------------------------
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
img_dir  <- "C:/Users/T430/Google Drive/PhD/Dissertation/competition networks/envelopment/img"
# if( !('net' %in% ls()) )
#   load('netrisk_dynamic_2.RData')
###
# save.image('netrisk_dynamic_2.RData')
###
source(file.path(getwd(),'R','comp_net_functions.R'))
source(file.path(getwd(),'R','netrisk_functions.R'))
# source(file.path(getwd(),'R','cb_data_prep.R'))

# load('netrisk_dynamic_firm_nets_1yr_v3_misc.RData')
# load('tergm_firm_nets_6pd_1yr.RData')
# firm.nets <- readRDS('tergm_firm_nets_6pd_1yr.rds')
firm.nets <- readRDS('tergm_firm_nets_1yr_6pd_v4_cem.rds')

par.default <- par()
lattice::trellis.par.set(strip.background=list(col="lightgrey"))
###########################################################################


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
# #igraph::write.graph(graph = g.full, file="g_full.graphml", format = 'graphml')

g.full <- read.graph('g_full.graphml', format='graphml')



#----------------------------------------------------------------
##-------------------FIND MARKET of suitable size --------------
# #----------------------------------------------------------------
# firms <- V(g.full)$name
# deg <- igraph::degree(g.full)
# firms.sub <- firms[which(deg > 8 & deg < 13)]
# #View(data.frame(name=firms.sub)) 
# ##
# name_i <- 'medallia'   ## check companies
# k <- 2
# (nbs <- neighbors(g.full, v = V(g.full)[which(V(g.full)$name==name_i)]))
# g.ego <- make_ego_graph(g.full, order=k, 
#                         nodes = V(g.full)[which(V(g.full)$name==name_i)] )[[1]]
# cat(vcount(g.ego))
# ##
# egodeg <- igraph::degree(g.ego, normalized = F)^.4
# plot(g.ego, layout=layout.kamada.kawai,
#      vertex.size=egodeg*1.0,vertex.label.cex=egodeg*.2)
# 
# View(head(co[grep('biotec',
#                   co$short_description,
#                   ignore.case = T,perl=T) & 
#                co$company_name_unique %in% firms,],100))

#-----------------------------------------------------------------
# ## EFM / CEM 
# firms.todo <-  c('medallia','clarabridge','qualtrics','satmetrix','confirmit',
#                  'empathica','allegiance','hybris','customergauge',
#                  'mindshare-technologies','markettools')

##--------------------------------------------------------------
##--------------------------------------------------------------
##--------- CREATE FIRM NETWORK PERIOD LISTS  ------------------
##--------------------------------------------------------------
##--------------------------------------------------------------

# ## cache original
# firm.nets.orig <- firm.nets


## creat list if not exists
if( !('firm.nets' %in% ls()) ) firm.nets <- list()

## set market group of firms
net_group <- 'cem'
if( !(net_group %in% names(firm.nets)) ) firm.nets[[net_group]] <- list()

## set firms to create networks
name_i <- 'clarabridge'
nbhd <- neighborhood(g.full, order=1, nodes=V(g.full)[V(g.full)$name==name_i])[[1]]
gsub <- induced.subgraph(g.full, vids = nbhd)
firms.todo <- names(nbhd)
firms.todo <- rev(names(nbhd)[ !(names(nbhd) %in% names(firm.nets$cem)) ])

## run main network period creation loop
for (i in 1:length(firms.todo)) {
  ## -- settings --
  k <- 3
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
  g.k.sub <- igraph::make_ego_graph(graph = g.base, nodes = V(g.full)[V(g.full)$name==name_i], order = k, mode = 'all')[[1]]
  net.k.sub <- getNetFromIgraph(g.k.sub)
  net <- net.k.sub
  net %n% 'ego' <- name_i
  #----------------Network List-------------------
  nl <- list()
  for (t in 2:length(periods)) {
    cat(sprintf('\nmaking period %s-%s:\n', periods[t-1],periods[t]))
    tmp.net <- makePdNetwork(net.k.sub,
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
    nl[[t]] %v% 'net_risk_lag' <- nl[[t-1]] %v% 'net_risk'
    nl[[t]] %v% 'cent_deg_lag' <- nl[[t-1]] %v% 'cent_deg'
    nl[[t]] %n% 'DV_lag' <- nl[[t-1]][,]
    nl[[t]] %n% 'dist_lag' <- nl[[t-1]] %n% 'dist'
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

  ## SAVE variable in image
  # firm.nl <- list()
  firm.nets[[net_group]][[name_i]] <- nets

  ## CAREFUL TO OVERWRITE
  file.name <- sprintf('tergm_firm_nets_1yr_6pd_v4_%s.rds',net_group)
  saveRDS(firm.nets, file=file.name)

}

# load('netrisk_dynamic_firm_nets.RData')

# ## FIX NaN in genidx_multilevel
# algos <- c('genidx_multilevel','genidx_infomap',
#            'genidx_walktrap', 'genidx_fastgreedy',
#            'genidx_edgebetween','genidx_labelprop')
# for (i in 1:length(firm.nets$cem)) {
#   for (j in 1:length(firm.nets$cem[[i]])) {
#       for (k in 1:length(algos)) {
#         tmp <- firm.nets$cem[[i]][[j]] %v% algos[k]
#         tmp[is.na(tmp) | is.null(tmp) | is.nan(tmp)] <- 0
#         firm.nets$cem[[i]][[j]] %v% algos[k] <- tmp
#       }
#   }
# }
# saveRDS(firm.nets, file='tergm_firm_nets_1yr_6pd_v4_cem.rds')



## # ADD SIMILARITY NETWORK PROPERTY TO FIRM.NETS
# for (i in seq_along(firm.nets)) {
#   firm.list <- firm.nets[[i]]
#   for (j in seq_along(firm.list)) {
#     net <- firm.list[[j]]
#     g.net <- getIgraphFromNet(net)
#     sim <- igraph::similarity(g.net,vids = V(g.net), 
#                               mode = "all", method = "invlogweighted" )
#     sim[is.nan(sim) | is.na(sim)] <- 0
#     firm.nets[[i]][[j]] %n% 'similarity' <- sim
#   }
# }

# # ADD CONSTRAINT NODE PROPERTY
# for (t in 1:length(nets)) {
#   g.tmp <- getIgraphFromNet(nets[[t]])
#   if (vcount(g.tmp)>0 & ecount(g.tmp)>0) {
#     cons <-  igraph::constraint(g.tmp)
#     cons[is.nan(cons) | is.na(cons)] <- 0 ### ???
#     nets[[t]] %v% 'constraint' <- cons
#   }
# }

# # # ADD DISTANCE NETWORK PROPERTY TO FIRM.NETS
# firm.nets.bak <- firm.nets
# for (i in seq_along(firm.nets)) {
#   firm.list <- firm.nets[[i]]
#   for (j in seq_along(firm.list)) {
#     firm.years <- firm.list[[j]]
#     years <- sapply(names(firm.years),as.numeric)
#     for (k in seq_along(firm.years)) {
#       firm.nets[[i]][[j]][[k]] %n% 'inv_dist' <- 1 / (firm.nets[[i]][[j]][[k]] %n% 'dist')
#       firm.nets[[i]][[j]][[k]] %n% 'exp_inv_dist' <- exp(1 / (firm.nets[[i]][[j]][[k]] %n% 'dist'))
#       if (k > 1) {
#         firm.nets[[i]][[j]][[k]] %n% 'inv_dist_lag' <- firm.nets[[i]][[j]][[k-1]] %n% 'inv_dist'
#         firm.nets[[i]][[j]][[k]] %n% 'exp_inv_dist_lag' <- firm.nets[[i]][[j]][[k-1]] %n% 'exp_inv_dist'
#       }
#     }
#   }
# }
# 
# for (i in seq_along(firm.nets$test$clarabridge)) {
#   if (i > 1) {
#     firm.nets$test$clarabridge[[i]] %n% 'DV_lag' <- firm.nets$test$clarabridge[[i-1]][,]
#   }
# }

# #------------------------------------------------------
# #              Predictors Diagnostics
# #------------------------------------------------------
# ## Plot density
# n <- ceiling(sqrt(length(firm.nets)))
# m <- ifelse(n*(n-1) >= length(firm.nets), n-1, n)
# par(mfrow=c(m,n), mar=c(2.5,2.5,2,1))
# for (firm_i in names(firm.nets)) {
#   nets <- firm.nets[[firm_i]]
#   plot(as.numeric(names(nets))-1,   
#        sapply(nets,function(net) {
#          sum(net[lower.tri(net)])/(nrow(net[,])*nrow(net[,]-1)/2) 
#        }), 
#        ylab='density', xlab='year', type='b', main=firm_i)
# }
# 
# ## Net Risk
# par(mfrow=c(3,3), mar=c(2.5,2.5,2,1))
# for (firm_i in names(firm.nets)) {
#   nets <- firm.nets[[firm_i]]
#   sapply(seq_along(nets), function(j) {
#     hist(nets[[j]] %v% 'net_risk', breaks=25, main=sprintf('%s %s',firm_i,names(nets)[j]))
#   })
# }


#---------------------------------------------------------
# --------- BTERGM HYPOTHESES MODEL  COMPARE -------------
#---------------------------------------------------------
nPeriods <- 6
R <- 200
net_group <- 'cem'
firm_i <- 'clarabridge'

nets.sub <- firm.nets[[net_group]][[firm_i]]
nets.sub <- nets.sub[(length(nets.sub)-nPeriods+1):(length(nets.sub))]
mmc <- lapply(nets.sub, function(net) as.matrix(net %n% 'mmc'))
smt <- lapply(nets.sub, function(net) as.matrix(net %n% 'similarity'))
ldv <- lapply(nets.sub, function(net) as.matrix(net %n% 'DV_lag'))

## Models
m0 <-   nets.sub ~ edges + gwesp(0, fixed=T) + 
  nodematch('ipo_status', diff=TRUE) +
  nodematch('state_code', diff=F) +
  nodecov('age') +  absdiff('age') + 
  edgecov(mmc)  + # edgecov(ldv) +  edgecov(smt)
  memory(type="stability",lag=1)

m1 <-   nets.sub ~ edges + gwesp(0, fixed=T) + 
  nodematch('ipo_status', diff=TRUE) +
  nodematch('state_code', diff=F) +
  nodecov('age') +    absdiff('age') + 
  edgecov(mmc)  + # edgecov(ldv) +   edgecov(smt)  +
  memory(type="stability",lag=1) + 
  nodecov('cent_pow_1_5') 
m1d <-   nets.sub ~ edges + gwesp(0, fixed=T) + 
  nodematch('ipo_status', diff=TRUE) +
  nodematch('state_code', diff=F) +
  nodecov('age') +    absdiff('age') + 
  edgecov(mmc)  + # edgecov(ldv) +   edgecov(smt)  +
  memory(type="stability",lag=1) + ## replace ldv
  nodecov('cent_pow_1_5') + absdiff('cent_pow_1_5')
m1nd <-   nets.sub ~ edges + gwesp(0, fixed=T) + 
  nodematch('ipo_status', diff=TRUE) +
  nodematch('state_code', diff=F) +
  nodecov('age') +    absdiff('age') + 
  edgecov(mmc)  + # edgecov(ldv) +   edgecov(smt)  +
  memory(type="stability",lag=1) + ## replace ldv
  nodecov('cent_pow_n1_5') + absdiff('cent_pow_n1_5')

m2 <-   nets.sub ~ edges + gwesp(0, fixed=T) + 
  nodematch('ipo_status', diff=TRUE) +
  nodematch('state_code', diff=F) +
  nodecov('age') +    absdiff('age') + 
  edgecov(mmc)  + #edgecov(ldv) +   edgecov(smt)  +
  memory(type="stability",lag=1) + 
  nodecov('cent_deg_lag') 
m2d <-   nets.sub ~ edges + gwesp(0, fixed=T) + 
  nodematch('ipo_status', diff=TRUE) +
  nodematch('state_code', diff=F) +
  nodecov('age') +    absdiff('age') + 
  edgecov(mmc)  + # edgecov(ldv) + edgecov(smt)  +
  memory(type="stability",lag=1) + ## replace ldv
  nodecov('cent_deg_lag') + absdiff('cent_deg_lag')

m3 <- nets.sub ~ edges + gwesp(0, fixed=T)   +
  nodematch('ipo_status', diff=TRUE)  +
  nodematch('state_code', diff=F) +
  nodecov('age') +    absdiff('age') + 
  edgecov(mmc)  + # edgecov(ldv) +  edgecov(smt)  +
  memory(type="stability",lag=1) + 
  cycle(3) + cycle(4) + cycle(5)

m4 <-   nets.sub ~ edges + gwesp(0, fixed=T) + 
  nodematch('ipo_status', diff=TRUE) +
  nodematch('state_code', diff=F) +
  nodecov('age') +    absdiff('age') + 
  edgecov(mmc)  + # edgecov(ldv) +   edgecov(smt)  +
  memory(type="stability",lag=1) + 
  nodecov('genidx_multilevel')  + absdiff('genidx_multilevel')

m5 <-   nets.sub ~ edges + gwesp(0, fixed=T) + 
  nodematch('ipo_status', diff=TRUE) +
  nodematch('state_code', diff=F) +
  nodecov('age') +    absdiff('age') + 
  edgecov(mmc)  + # edgecov(ldv) +   edgecov(smt)  +
  memory(type="stability",lag=1) + 
  nodecov('cent_pow_n1_5') + absdiff('cent_pow_n1_5') +
  cycle(3) + cycle(4) + cycle(5) + 
  nodecov('genidx_multilevel')  + absdiff('genidx_multilevel')
m5wt <-   nets.sub ~ edges + gwesp(0, fixed=T) + 
  nodematch('ipo_status', diff=TRUE) +
  nodematch('state_code', diff=F) +
  nodecov('age') +    absdiff('age') + 
  edgecov(mmc)  + # edgecov(ldv) +   edgecov(smt)  +
  memory(type="stability",lag=1) + 
  nodecov('cent_pow_n1_5') + absdiff('cent_pow_n1_5') +
  cycle(3) + cycle(4) + cycle(5) + 
  nodecov('genidx_walktrap')  + absdiff('genidx_walktrap')
m5im <-   nets.sub ~ edges + gwesp(0, fixed=T) + 
  nodematch('ipo_status', diff=TRUE) +
  nodematch('state_code', diff=F) +
  nodecov('age') +    absdiff('age') + 
  edgecov(mmc)  + # edgecov(ldv) +   edgecov(smt)  +
  memory(type="stability",lag=1) + 
  nodecov('cent_pow_n1_5') + absdiff('cent_pow_n1_5') +
  cycle(3) + cycle(4) + cycle(5) + 
  nodecov('genidx_infomap')  + absdiff('genidx_infomap')
m5fg <-   nets.sub ~ edges + gwesp(0, fixed=T) + 
  nodematch('ipo_status', diff=TRUE) +
  nodematch('state_code', diff=F) +
  nodecov('age') +    absdiff('age') + 
  edgecov(mmc)  + # edgecov(ldv) +   edgecov(smt)  +
  memory(type="stability",lag=1) + 
  nodecov('cent_pow_n1_5') + absdiff('cent_pow_n1_5') +
  cycle(3) + cycle(4) + cycle(5) + 
  nodecov('genidx_fastgreedy')  + absdiff('genidx_fastgreedy')
m5eb <-   nets.sub ~ edges + gwesp(0, fixed=T) + 
  nodematch('ipo_status', diff=TRUE) +
  nodematch('state_code', diff=F) +
  nodecov('age') +    absdiff('age') + 
  edgecov(mmc)  + # edgecov(ldv) +   edgecov(smt)  +
  memory(type="stability",lag=1) + 
  nodecov('cent_pow_n1_5') + absdiff('cent_pow_n1_5') +
  cycle(3) + cycle(4) + cycle(5) + 
  nodecov('genidx_edgebetween')  + absdiff('genidx_edgebetween')
m5lp <-   nets.sub ~ edges + gwesp(0, fixed=T) + 
  nodematch('ipo_status', diff=TRUE) +
  nodematch('state_code', diff=F) +
  nodecov('age') +    absdiff('age') + 
  edgecov(mmc)  + # edgecov(ldv) +   edgecov(smt)  +
  memory(type="stability",lag=1) + 
  nodecov('cent_pow_n1_5') + absdiff('cent_pow_n1_5') +
  cycle(3) + cycle(4) + cycle(5) + 
  nodecov('genidx_labelprop')  + absdiff('genidx_labelprop')

f0  <- btergm(m0,  R=R, parallel = "multicore", ncpus = detectCores()); summary(f0)
f1  <- btergm(m1,  R=R, parallel = "multicore", ncpus = detectCores()); summary(f1)
f1d <- btergm(m1d, R=R, parallel = "multicore", ncpus = detectCores()); summary(f1d)
f1nd<- btergm(m1nd,R=R, parallel = "multicore", ncpus = detectCores()); summary(f1nd)
f2  <- btergm(m2,  R=R, parallel = "multicore", ncpus = detectCores()); summary(f2)
f2d <- btergm(m2d, R=R, parallel = "multicore", ncpus = detectCores()); summary(f2d)
f3  <- btergm(m3,  R=R, parallel = "multicore", ncpus = detectCores()); summary(f3)
f4  <- btergm(m4,  R=R, parallel = "multicore", ncpus = detectCores()); summary(f4)

f5  <- btergm(m5,  R=R, parallel = "multicore", ncpus = detectCores()); summary(f5)
f5wt  <- btergm(m5wt,  R=R, parallel = "multicore", ncpus = detectCores()); summary(f5wt)
f5im  <- btergm(m5im,  R=R, parallel = "multicore", ncpus = detectCores()); summary(f5im)
f5fg  <- btergm(m5fg,  R=R, parallel = "multicore", ncpus = detectCores()); summary(f5fg)
f5eb  <- btergm(m5eb,  R=R, parallel = "multicore", ncpus = detectCores()); summary(f5eb)
f5lp  <- btergm(m5lp,  R=R, parallel = "multicore", ncpus = detectCores()); summary(f5lp)


# fits <- list(f0=f0,f1=f1,f1d=f1d,f1nd=f1nd,f2=f2,f2d=f2d,f3=f3,f4=f4,f5=f5)  #f3=f3,f4=f4,f5=f5
# saveRDS(fits, file='sms_tergm_new_gi_cem_clarabridge_fits.rds')

fits5 <- list(f5=f5,f5wt=f5wt,f5im=f5im,f5fg=f5fg,f5eb=f5eb,f5lp=f5lp)
saveRDS(fits5, file='sms_tergm_new_gi_cem_clarabridge_fits5_algos.rds')

screenreg(fits5,digits = 3)




#---------------------------------------------------------
#             REMOVE BOOTSAMP OUTLIERS
#---------------------------------------------------------

l.fix = list()
l.fix$misc = list()
l.fix$misc$clarabridge = list()
for (model in c('f0', 'f1', 'f2', 'f3', 'f4')) {
  fit <- l.hyp$misc$clarabridge[[model]]
  ol.list <- apply(fit@bootsamp, 2, function(x) which(x > median(x, na.rm = T) + 1.5*IQR(x, na.rm = T) 
                                                      | x < median(x, na.rm = T) - 1.5*IQR(x, na.rm = T)) )
  ol <- unique(unlist(ol.list))
  keep <- seq_len(nrow(fit@bootsamp))[ !( seq_len(nrow(fit@bootsamp)) %in% ol) ]
  fit@bootsamp <- fit@bootsamp[keep, ]
  l.fix$misc$clarabridge[[model]] <- fit
}

l.fix$misc$clarabridge$f3 = f3

#----------------------------------------------------------
#                MCMLE TERGM
##--------------------------------------------------------
# load('netrisk_dynamic_netflix_2y.RData')

net_group <- 'misc'
nets.group <- firm.nets[[net_group]]
firms.todo <- names(firm.nets[[net_group]])
#####
if ( !('l.fit.m' %in% ls()) ) l.fit.m <- list()
if ( !(net_group %in% names(l.fit.m)) ) l.fit.m[[net_group]] <- list()
nPeriods <- min(sapply(nets.group,function(net)length(net))) 
resamp <- 100
yrpd <- 1
if ('tmp.npds' %in% ls()) rm(tmp.npds)
for (i in 1:length(firms.todo)) {
  firm_i <- firms.todo[i]; cat(sprintf('---------%s---------\n',firm_i))
  nets <- nets.group[[firm_i]]
  nets.sub <- nets[ (length(nets)-nPeriods+1):length(nets) ]
  if ( !('tmp.npds' %in% ls()) )   tmp.npds <- length(nets.sub)
  mmc <- lapply(nets.sub, function(net) as.matrix(net %n% 'mmc'))
  sim <- lapply(nets.sub, function(net) as.matrix(net %n% 'similarity'))
  fit <- mtergm(nets.sub ~ edges + gwesp(0, fixed=T)  + cycle(3:6) + 
                nodefactor('state_code') + nodematch('state_code', diff=F) +
                nodecov('age') +   edgecov(mmc)  +
                nodecov('net_risk') + nodecov('net_risk_lag') +
                nodematch('npm',diff=F) + 
                nodematch('ipo_status', diff=TRUE)  +
                nodecov('constraint') + absdiff('constraint') + 
                edgecov(sim)  #+
              # nodecov('betweenness') + absdiff('betweenness')
              ,  parallel = "multicore", ncpus = detectCores())
  l.fit.m[[net_group]][[firm_i]] <- fit
  file.name <- sprintf('fit_list_mtergm_%syr_%spd_%s-grp.RData', yrpd, tmp.npds,net_group)
  save.image(file.name) # save.image('fit_list_btergm_med_clar_qual_1yr_.RData')
}

write.regtable(list(mt6), filename='fit_mtergm_clar')



##-----------------------------------------------------------
#            TEST MTERGM MCMLE
#----------------------------------------------------------
load('netrisk_dynamic_firm_nets_1yr_v3_misc.RData')

nets.sub <- firm.nets$test$clarabridge
mmc <- lapply(nets.sub, function(net) as.matrix(net %n% 'mmc'))
sim <- lapply(nets.sub, function(net) as.matrix(net %n% 'similarity'))

fm.c5 <- mtergm(  nets.sub ~ edges + gwesp(0, fixed=T) + 
             # nodefactor('state_code') +
             # nodematch('state_code', diff=F) +
             nodecov('age') +   # edgecov(mmc)  + # edgecov(ldv) +
             nodematch('npm',diff=F) + 
             edgecov(sim)  +
             #nodematch('ipo_status', diff=TRUE)  +
             nodecov('net_risk') +
             nodecov('constraint') + absdiff('constraint') + 
             cycle(3) + cycle(4) + cycle(5) 
       ,  parallel = "multicore", ncpus = detectCores())


