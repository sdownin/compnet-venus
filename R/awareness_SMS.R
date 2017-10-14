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
source(file.path(getwd(),'R','cb_data_prep.R'))

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
## Forrester research competitors
forr.comp.names <- c('satmetrix','empathica','medallia','verint',
                     'qualtrics','maritzcx','smg','nice-systems')
##
nbhd <- neighborhood(g.full, order=1, nodes=V(g.full)[V(g.full)$name==name_i])[[1]]
gsub <- induced.subgraph(g.full, vids = nbhd)
firms.todo <- unique(c(names(nbhd),forr.comp.names))
firms.todo <- rev(firms.todo[ !(firms.todo %in% names(firm.nets$cem)) ])

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

############################################################################
#--------------------- BTERGM BY INDUSTRY ------------------------------
############################################################################

R <- 200
nPeriods <- 6
net_group <- 'cem'
firms <- which(sapply(firm.nets$cem,function(x)length(x)>=nPeriods))

if (!("fits" %in% ls())) fits <- list()
if (!(net_group %in% names(fits))) fits[[net_group]] <- list()

for (firm_i in firms) {
  nets.sub <- firm.nets[[net_group]][[firm_i]]
  nets.sub <- nets.sub[(length(nets.sub)-nPeriods+1):(length(nets.sub))]
  
  mmc <- lapply(nets.sub, function(net) as.matrix(net %n% 'mmc'))
  smt <- lapply(nets.sub, function(net) as.matrix(net %n% 'similarity'))
  ldv <- lapply(nets.sub, function(net) as.matrix(net %n% 'DV_lag'))
  for (index in 1:length(nets.sub)) {
    tmp <- nets.sub[[index]] %v% 'genidx_multilevel'
    tmp[is.na(tmp) | is.null(tmp) | is.nan(tmp)] <- 0
    nets.sub[[index]] %v% 'genidx_multilevel_narm' <- tmp
  }
  
  m5 <-   nets.sub ~ edges + gwesp(0, fixed=T) + 
    nodematch('ipo_status', diff=TRUE) +
    nodematch('state_code', diff=F) +
    nodecov('age') + absdiff('age') +   
    edgecov(mmc)  + # edgecov(ldv) +   edgecov(smt)  +
    memory(type="stability",lag=1) + 
    nodecov('cent_pow_1_5')  + absdiff('cent_pow_1_5') +
    cycle(3) + cycle(4) + cycle(5) + 
    nodecov('genidx_multilevel_narm')  + absdiff('genidx_multilevel_narm')
  
  ## RUN Bootstrap MPLE
  fits[[net_group]][[firm_i]] <- btergm(m5, R=R, parallel = "multicore", ncpus = detectCores()); summary(fits[[net_group]][[firm_i]])
  
  ## save serialized object
  saveRDS(fits, file=sprintf('tergm_fits_new_m5_pow_%s_pd%s_R%s.rds',net_group,nPeriods,R))
}







#------------------------------------------------------
############################################################################
#--------------------- BTERGM ------------------------------
############################################################################
R <- 200
nPeriods <- 6
net_group <- 'misc'
firm_i <- 'clarabridge'

nets.sub <- firm.nets[[net_group]][[firm_i]]
nets.sub <- nets.sub[(length(nets.sub)-nPeriods+1):(length(nets.sub))]

mmc <- lapply(nets.sub, function(net) as.matrix(net %n% 'mmc'))
sim <- lapply(nets.sub, function(net) as.matrix(net %n% 'similarity'))
ldv <- lapply(nets.sub, function(net) as.matrix(net %n% 'DV_lag'))
# lid <- lapply(nets.sub, function(net) as.matrix(net %n% 'inv_dist_lag'))

# tc. <- lapply(nets.sub, function(net) as.matrix(net %n% 'DV_lag') as.matrix(net %n% 'DV_lag') )


m5 <- nets.sub ~ edges + edgecov(mmc)  + edgecov(sim)  +
  #nodefactor('state_code') + 
  gwesp(0, fixed=T) + 
  nodecov('age') +  
  nodematch('state_code', diff=F) +
  nodematch('npm',diff=F) +   
  nodematch('ipo_status', diff=TRUE)  +
  nodecov('net_risk') +
  nodecov('constraint') + absdiff('constraint') + 
  cycle(3:5) + 
  memory(type="stability", lag=1) +
  timecov(transform=function(t)t)#  + cycle(6)

m6 <- nets.sub ~ edges + gwesp(0, fixed=T) + 
  #nodefactor('state_code') + 
  nodematch('state_code', diff=F) +
  nodecov('age') +  
  nodematch('npm',diff=F) + 
  edgecov(mmc)  + edgecov(sim)  + edgecov(lid)  +   #edgecov(ldv) +
  nodematch('ipo_status', diff=TRUE)  +
  nodecov('net_risk') +
  nodecov('constraint') + absdiff('constraint') + 
  cycle(3) + cycle(4) + # cycle(5) #  + cycle(6)
  memory(type="stability") 

m7 <- nets.sub ~ edges + gwesp(0, fixed=T) + 
  #nodefactor('state_code') + 
  nodematch('state_code', diff=F) +
  nodecov('age') +  
  nodematch('npm',diff=F) + 
  edgecov(mmc)  + edgecov(sim)  + edgecov(lid)  +   #edgecov(ldv) +
  nodematch('ipo_status', diff=TRUE)  +
  nodecov('net_risk') +
  nodecov('constraint') + absdiff('constraint') + 
  cycle(3) + cycle(4) + # cycle(5) #  + cycle(6)
  memory(type="stability") +
  timecov(transform=function(t)t)

m8 <- nets.sub ~ edges + gwesp(0, fixed=T) + 
  #nodefactor('state_code') + 
  nodematch('state_code', diff=F) +
  nodecov('age') +  
  nodematch('npm',diff=F) + 
  edgecov(mmc)  + edgecov(sim)  + edgecov(lid)  +   #edgecov(ldv) +
  nodematch('ipo_status', diff=TRUE)  +
  nodecov('net_risk') +
  nodecov('constraint') + absdiff('constraint') + 
  cycle(3) + cycle(4) + cycle(5)  + # cycle(6)
  memory(type="stability", lag=1) 

  
m9 <- nets.sub ~ edges + gwesp(0, fixed=T) + 
    #nodefactor('state_code') + 
    nodematch('state_code', diff=F) +
    nodecov('age') +  
    nodematch('npm',diff=F) + 
    edgecov(mmc)  + edgecov(sim)  + edgecov(lid)  +   #edgecov(ldv) +
    nodematch('ipo_status', diff=TRUE)  +
    nodecov('net_risk') +
    nodecov('constraint') + absdiff('constraint') + 
    cycle(3) + cycle(4) + cycle(5)  + # cycle(6)
    memory(type="stability", lag=1) +
    timecov(transform=function(t)t)
    
m10 <- nets.sub ~ edges + gwesp(0, fixed=T) + 
  #nodefactor('state_code') + 
  nodematch('state_code', diff=F) +
  nodecov('age') +  
  nodematch('npm',diff=F) + 
  edgecov(mmc)  + edgecov(sim)  + edgecov(lid)  +   #edgecov(ldv) +
  nodematch('ipo_status', diff=TRUE)  +
  nodecov('net_risk') +
  nodecov('constraint') + absdiff('constraint') + 
  cycle(3) + cycle(4) + cycle(5)  + # cycle(6)
  memory(type="stability", lag=1) +
  timecov(transform=function(t)log(t))

m12 <- nets.sub ~ edges + gwesp(0, fixed=T) + 
  #nodefactor('state_code') + 
  nodematch('state_code', diff=F) +
  nodecov('age') +  
  nodematch('npm',diff=F) + 
  edgecov(mmc)  + edgecov(sim)  + edgecov(lid)  +   #edgecov(ldv) +
  nodematch('ipo_status', diff=TRUE)  +
  nodecov('net_risk') +
  nodecov('constraint') + absdiff('constraint') + 
  cycle(3) + cycle(4) + cycle(5)  + # cycle(6)
  memory(type="stability", lag=1) +
  timecov(transform=function(t)1/(1+exp(-(2*t-6))) )


ncpus
# f4
f5 <- btergm(m5, R=R, parallel = "multicore", ncpus = detectCores())

f6 <- btergm(m6, R=R, parallel = "multicore", ncpus = ncpus)
f7 <- btergm(m7, R=R, parallel = "multicore", ncpus = ncpus)

f8 <- btergm(m8, R=R, parallel = "multicore", ncpus = ncpus)
f9 <- btergm(m9, R=R, parallel = "multicore", ncpus = ncpus)

f10 <- btergm(m10, R=R, parallel = "multicore", ncpus = ncpus)
f12 <- btergm(m12, R=R, parallel = "multicore", ncpus = ncpus)



# screenreg(list(m4=f4,m5=f5,m6=f6,m7=f7))
screenreg(list(f8, f10,f12), digits = 3, single.row = F)

save.image('netrisk_dynamic_timecov.RData')


## VISUALIZE INV DIST LAG
net <- nets.sub$`2017`
df.lid <- data.frame(y=net[,][lower.tri(net[,])],lid=(net %n% 'inv_dist_lag')[lower.tri(net[,])])

xyplot(y ~ lid, data=df.lid )

## logistic trend line
logit.model <- glm(y ~ lid, data=df.lid, family=binomial(link = "logit"))
tmp.data <- data.frame(lid=seq(0,1,.001))
pred.data <- as.data.frame(
  predict(logit.model, 
  newdata = tmp.data,
  type="link",se=T)
  )

pred.data <- cbind(pred.data, tmp.data)
std <- qnorm(0.95 / 2 + 0.5)
pred.data$fit <- logit.model$family$linkinv(pred.data$fit)
# pred.data$min <- logit.model$family$linkinv(pred.data$fit - std * pred.data$se.fit)
# pred.data$max <- logit.model$family$linkinv(pred.data$fit + std * pred.data$se.fit)
pred.data$min <- pred.data$fit - std * pred.data$se.fit
pred.data$max <- pred.data$fit + std * pred.data$se.fit


ggplot(df.lid, aes(lid, y, size=..n..)) + 
  stat_sum()+ scale_size_area() #+ 
  #loess(model = 'logistic')

# p <- ggplot(df.lid, aes(x=lid, y=y)) 
p <- ggplot(df.lid, aes(x=lid, y=y)) +  geom_point()
p  <- p + geom_line(data=pred.data, aes(y=fit))
p <- p + geom_ribbon(data=pred.data, aes(y=fit,ymin=min,ymax=max),alpha=.4)
p

# nets <- list()
# pds <- 2007:2016
# for (i in 2:length(pds)) {
#   nets[[i]] <- network(network.extract(nd, onset=pds[i-1], terminus=pds[i])[,], directed = F, hyper = F, multiple = F, loops = F, bipartite = F)
# }; names(nets) <- pds; nets <- nets[-1]

fb0 <- btergm(nets.sub ~ edges + gwesp(0, fixed=T) + nodecov('age') + scale_size_area()
                nodematch('state_code', diff=F) +  
                 nodecov('net_risk'),
              R = 5, parallel = "multicore", ncpus = detectCores())

fb1 <- btergm(nets.sub ~ edges + gwesp(0, fixed=T) + nodecov('age') + 
                nodematch('state_code', diff=F) +  
                nodecov('net_risk') + edgecov(mmc),
              R = 5, parallel = "multicore", ncpus = detectCores())

fb3 <- btergm(nets.sub ~ edges + gwesp(0, fixed=T)  + cycle(3:4) + 
                nodematch('state_code', diff=F) +
                nodecov('age') +   edgecov(mmc)  +
                nodecov('net_risk') + nodecov('net_risk_lag') +
                nodematch('npm',diff=F)
               ,
              R = 20, parallel = "multicore", ncpus = detectCores())


fb4 <- btergm(nets.sub ~ edges + gwesp(0, fixed=T)  + cycle(3:4) + 
                nodematch('state_code', diff=F) +
                nodecov('age') +   edgecov(mmc)  +
                nodecov('net_risk') + nodecov('net_risk_lag') +
                nodematch('npm',diff=F) + 
                nodematch('ipo_status', diff=TRUE) 
              ,
              R = 5, parallel = "multicore", ncpus = detectCores())


nets.sub <- nets
mmc <- lapply(nets.sub,function(net) as.matrix(net %n% 'mmc'))
fb5clar <- btergm(nets.sub ~ edges + gwesp(0, fixed=T)  + cycle(3:5) + 
                nodematch('state_code', diff=F) +
                nodecov('age') +   edgecov(mmc)  +
                nodecov('net_risk') + nodecov('net_risk_lag') +
                nodematch('npm',diff=F) + 
                nodematch('ipo_status', diff=TRUE) 
              ,
              R = 30, parallel = "multicore", ncpus = detectCores())
fb6clar <- btergm(nets.sub ~ edges + gwesp(0, fixed=T)  + 
                    cycle(3:5) + 
                    nodematch('state_code', diff=F) +
                    nodecov('age') +   
                    edgecov(mmc)  +
                    nodecov('net_risk') + 
                    nodematch('npm',diff=F) + 
                    nodematch('ipo_status', diff=TRUE) +
                    nodecov('constraint') + 
                    absdiff('constraint')
                 ,
                 R = 5, parallel = "multicore", ncpus = detectCores())

(l <- list(fb0, fb5clar, fb6clar))
texreg::screenreg(l, single.row = T)
write.regtable(filterModels(l), filename = "clarabridge_3k_2y_btergm_constraint", digits=3)

#-------------- ADD NETWORK STATS NOT YET COMPUTED ----------------------
# load('netrisk_dynamic_firm_nets.RData')
# firm.nets.bak <- firm.nets
# for (i in 1:length(firm.nets)) {
#   nets <- firm.nets[[i]]
#   for (t in 1:length(nets)) {
#     g.tmp <- getIgraphFromNet(nets[[t]])
#     if (vcount(g.tmp)>0 & ecount(g.tmp)>0) {
#       cons <-  igraph::constraint(g.tmp)
#       cons[is.nan(cons) | is.na(cons)] <- 0 ### ???
#       nets[[t]] %v% 'constraint' <- cons
#       betw <- igraph::betweenness(g.tmp)
#       nets[[t]] %v% 'betweenness' <- betw
#       nets[[t]] %v% 'betweenness_log' <- log(betw + .001) 
#     }
#   }
#   firm.nets[[i]] <- nets
# }
# save.image('netrisk_dynamic_firm_nets_constr_betw.RData')

#------------------------------------------------------------------
#------------- CEM Industry btergm firm MODEL FIT LIST --------------------------
#-----------------------------------------------------------------
firms.todo <- names(firm.nets)
#####
if ( !('l.fit.b' %in% ls()) ) l.fit.b <- list()
nPeriods <- min(sapply(firm.nets,function(net)length(net))) 
resamp <- 1000
yrpd <- 1
if ('tmp.npds' %in% ls()) rm(tmp.npds)
for (i in 1:length(firms.todo)) {
  firm_i <- firms.todo[i]; cat(sprintf('---------%s---------\n',firm_i))
  nets <- firm.nets[[firm_i]]
  nets.sub <- nets[ (length(nets)-nPeriods+1):length(nets) ]
  if ( !('tmp.npds' %in% ls()) )   tmp.npds <- length(nets.sub)
  mmc <- lapply(nets.sub, function(net) as.matrix(net %n% 'mmc'))
  sim <- lapply(nets.sub, function(net) as.matrix(net %n% 'similarity'))
  fit <- btergm(nets.sub ~ edges + gwesp(0, fixed=T)  + cycle(3:6) + 
                 nodefactor('state_code') + nodematch('state_code', diff=F) +
                 nodecov('age') +   edgecov(mmc)  +
                 nodecov('net_risk') + nodecov('net_risk_lag') +
                 nodematch('npm',diff=F) + 
                 nodematch('ipo_status', diff=TRUE)  +
                 nodecov('constraint') + absdiff('constraint') +
                 edgecov(sim) #+
                # nodecov('betweenness') + absdiff('betweenness')
               , R = resamp, parallel = "multicore", ncpus = detectCores())
  l.fit.b[[firm_i]] <- fit
  file.name <- sprintf('fit_list_btergm_%syr_%spd_%sR_.RData', yrpd, tmp.npds, resamp)
  save.image(file.name) # save.image('fit_list_btergm_med_clar_qual_1yr_.RData')
}


#------------------------------------------------------------------
#------------- MISCELLANEOUS markets btergm firm MODEL FIT LIST ---
#-----------------------------------------------------------------
net_group <- 'misc'
nets.group <- firm.nets[[net_group]]
# firms.todo <- names(firm.nets[[net_group]])
firms.todo <- c("medallia","clarabridge", "satmetrix")
#####
if ( !('l.fit.b' %in% ls()) ) l.fit.b <- list()
if ( !(net_group %in% names(l.fit.b)) ) l.fit.b[[net_group]] <- list()
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
  fit <- btergm(nets.sub ~ edges + gwesp(0, fixed=T)  + cycle(3:6) + 
                  nodefactor('state_code') + nodematch('state_code', diff=F) +
                  nodecov('age') +   edgecov(mmc)  +
                  nodecov('net_risk') + nodecov('net_risk_lag') +
                  nodematch('npm',diff=F) + 
                  nodematch('ipo_status', diff=TRUE)  +
                  nodecov('constraint') + absdiff('constraint') + 
                  edgecov(sim)  #+
                # nodecov('betweenness') + absdiff('betweenness')
                , R = resamp, parallel = "multicore", ncpus = detectCores())
  l.fit.b[[net_group]][[firm_i]] <- fit
  file.name <- sprintf('fit_list_btergm_%syr_%spd_%sR_%s-grp.RData', yrpd, tmp.npds, resamp,net_group)
  save.image(file.name) # save.image('fit_list_btergm_med_clar_qual_1yr_.RData')
}


##_---------------------------------------------------------
load('fit_list_btergm_med_clar_qual_.RData')
co.str <- names(firm.nets)

l <- list(satmetrix=l.fit.b$satmetrix,
          empathica=l.fit.b$empathica,confirmit=l.fit.b$confirmit,
          clarabridge=l.fit.b$clarabridge,medallia=l.fit.b$medallia,
          qualtrics=l.fit.b$qualtrics, allegiance=l.fit.b$allegiance)
# l <- list(clarabridge=l.fit.b$clarabridge,
#           satmetrix=l.fit.b$satmetrix,
#           empathica=l.fit.b$empathica)
screenreg(l, single.row = T)
write.regtable(l, filename='fit_list_btergm_1yr_6pd_')

write.regtable(l.fit.b$misc, filename='fit_list_btergm_1yr_5pd_misc_efm', ci.force.level=0.01)
#----------------------------------------------


nr <- c(.38,.47,.29,.5,.4,.51,.45,.46,.32)

#-------------------------------------------------------
#  PLOT COEFS DISTRIBUTION
#-------------------------------------------------------
l.fit.b.sub <- l.fit.b$misc[ which(names(l.fit.b$misc) != 'medallia') ]
if ('df.coefs' %in% ls()) rm(df.coefs) 
for (i in seq_along(l.fit.b.sub)) {
  firm_i <- names(l.fit.b.sub)[i]
  coefs <- unlist(l.fit.b.sub[[firm_i]]@coef)
  coefs <- coefs[!grepl('state_code[.]',names(coefs),ignore.case=T,perl=T)]
  tmp <- data.frame(name=coefs)
  names(tmp)[1] <- firm_i
  if ( !('df.coefs' %in% ls()) ) 
    df.coefs <- data.frame(coefs=names(coefs))
  df.coefs <- cbind(df.coefs, tmp)  
}

## Melt
df.coefs.m <- melt(df.coefs, id.vars = 'coefs')

## Add market category
# df.coefs.m$market <- sprintf('EFM (n=%s)',length(unique(df.coefs.m$variable)))
df.coefs.m$market <- NA
markets <- list(FIoT=c('fitbit','runtastic'),
                P2PT=c('zipcar','ridejoy'),
                PCI=c('visa','mastercard'),
                EFM=c('clarabridge','satmetrix'))
for (i in seq_along(markets)) {
  df.coefs.m[which(df.coefs.m$variable %in% markets[[i]]), 'market'] <- names(markets)[i]
}

# bwplot(value ~ coefs, data=df.coefs.m)

##RENAME
sw <- function(x){
  switch(x,
    'absdiff.constraint' = '2. Abs. Diff. Constraint',
    'nodematch.ipo_status.1' = '1. Size Homophily (IPO)',
    'nodematch.ipo_status.0' = '1. Size Homophily (Private)',
    'nodecov.net_risk' = '0. Net Risk',
    'cycle3' = '3. cycle3',
    'cycle4' = '3. cycle4',
    'cycle5' = '3. cycle5',
    'cycle6' = '3. cycle6'
  )
}

## HYPOTHESES
hyp <- c('absdiff.constraint', 'nodematch.ipo_status.1',
         'nodematch.ipo_status.0','nodecov.net_risk','cycle3','cycle4',
         'cycle5','cycle6')
df.sub.hyp <- subset(df.coefs.m, subset= (coefs %in% hyp) )
df.sub.hyp$coefs <- sapply(df.sub.hyp$coefs, function(x) sw(as.character(x)))
## CONTROLS
df.sub.con <- subset(df.coefs.m, subset= !(coefs %in% hyp) )
## BW PLOT of COEFFICIENTS
ggplot(data=df.sub.hyp, aes(x=coefs, y=value, fill=market)) + ## fill=industry
  geom_boxplot() +
  scale_fill_brewer(type = 'qual') + 
  geom_hline(yintercept = 0) + coord_flip() +
  ylab('Point Estimate') + xlab('Variable') + 
  # scale_y_log10() +
  theme_bw() + theme(legend.position="top")
ggsave('btergm_coef_bwplot_compare_misc_HYP.png',width = 8, height = 6, units = 'in', dpi = 250)

ggplot(data=df.sub.con, aes(x=coefs, y=value, fill=market)) + ## fill=industry
  geom_boxplot() +
  scale_fill_brewer(type = 'qual') + 
  geom_hline(yintercept = 0) + coord_flip() +
  ylab('Point Estimate') + xlab('Variable') + 
  # scale_y_log10() +
  theme_bw() + theme(legend.position="top")
ggsave('btergm_coef_bwplot_compare_misc_CONTR.png',width = 8, height = 6, units = 'in', dpi = 250)


#---------------------------------------------------------
# --------- BTERGM HYPOTHESES MODEL  COMPARE -------------
#---------------------------------------------------------
nPeriods <- 5
resamp <- 100
net_group <- 'misc'
firm_i <- 'clarabridge'
if ( !('l.hyp' %in% ls()) ) l.hyp <- list()
if ( !(net_group %in% names(l.hyp)) ) l.hyp[[net_group]] <- list()
if ( !(firm_i %in% names(l.hyp[[net_group]])) ) l.hyp[[net_group]][[firm_i]] <- list()
nets.sub <- firm.nets[[net_group]][[firm_i]]
nets.sub <- nets.sub[(length(nets.sub)-nPeriods+1):(length(nets.sub))]
mmc <- lapply(nets.sub, function(net) as.matrix(net %n% 'mmc'))
sim <- lapply(nets.sub, function(net) as.matrix(net %n% 'similarity'))

l.hyp[[net_group]][[firm_i]]$fbc <- btergm(
                nets.sub ~ edges + gwesp(0, fixed=T) + 
                nodefactor('state_code') + nodematch('state_code', diff=F) +
                nodecov('age') +   edgecov(mmc)  +
                nodematch('npm',diff=F) + 
                edgecov(sim)  #+
              , R = resamp, parallel = "multicore", ncpus = detectCores())

l.hyp[[net_group]][[firm_i]]$fb0 <- btergm(
                nets.sub ~ edges + gwesp(0, fixed=T) + 
                nodefactor('state_code') + nodematch('state_code', diff=F) +
                nodecov('age') +   edgecov(mmc)  +
                nodematch('npm',diff=F) + 
                edgecov(sim)  +
                nodecov('net_risk') + nodecov('net_risk_lag') 
              , R = resamp, parallel = "multicore", ncpus = detectCores())

l.hyp[[net_group]][[firm_i]]$fb1 <- btergm(
                nets.sub ~ edges + gwesp(0, fixed=T) + 
                nodefactor('state_code') + nodematch('state_code', diff=F) +
                nodecov('age') +   edgecov(mmc)  +
                nodematch('npm',diff=F) + 
                edgecov(sim)  +
                nodematch('ipo_status', diff=TRUE)
              , R = resamp, parallel = "multicore", ncpus = detectCores())

l.hyp[[net_group]][[firm_i]]$fb2 <- btergm(
                nets.sub ~ edges + gwesp(0, fixed=T) + 
                nodefactor('state_code') + nodematch('state_code', diff=F) +
                nodecov('age') +   edgecov(mmc)  +
                nodematch('npm',diff=F) + 
                edgecov(sim)  +
                nodecov('constraint') + absdiff('constraint') 
              , R = resamp, parallel = "multicore", ncpus = detectCores())

l.hyp[[net_group]][[firm_i]]$fb3 <- btergm(
                nets.sub ~ edges + gwesp(0, fixed=T)   +
                nodefactor('state_code') + nodematch('state_code', diff=F) +
                nodecov('age') +   edgecov(mmc)  +
                nodematch('npm',diff=F) + 
                edgecov(sim)  +
                cycle(3:6)
              , R = resamp, parallel = "multicore", ncpus = detectCores())

l.hyp[[net_group]][[firm_i]]$fb4 <- btergm(
                nets.sub ~ edges + gwesp(0, fixed=T) + 
                nodefactor('state_code') + nodematch('state_code', diff=F) +
                nodecov('age') +   edgecov(mmc)  +
                nodematch('npm',diff=F) + 
                edgecov(sim)  +
                nodecov('net_risk') + nodecov('net_risk_lag') +
                nodematch('ipo_status', diff=TRUE)  +
                nodecov('constraint') + absdiff('constraint') + 
                cycle(3:6)
              , R = resamp, parallel = "multicore", ncpus = detectCores())

save.image(sprintf('btergm_fit_HYP_%s_%s_%sR.RData',net_group,firm_i,resamp))

write.regtable(l.hyp$misc$clarabridge, html = T, filename = 'btergm_HYP_model_compare_clarabridge')

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


