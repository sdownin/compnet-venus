setwd("C:/Users/sdowning/Google Drive/PhD/Dissertation/competition networks/compnet")
library(btergm)
library(texreg)
library(lattice)
library(latticeExtra)
library(ggplot2)
library(reshape2)
library(plyr)
library(sna)

data_dir  <- "C:/Users/sdowning/Google Drive/PhD/Dissertation/competition networks/compnet"
data_file <- 'run_pmle_hyp1000_OUT.RData'
load(file.path(data_dir,data_file))

source(file.path(getwd(),'R','comp_net_functions.R'))
source(file.path(getwd(),'R','netrisk_functions.R'))

par.default <- par()
lattice::trellis.par.set(strip.background=list(col="lightgrey"))

y <- l.hyp$misc$clarabridge$f3@bootsamp
apply(l.hyp$misc$clarabridge$f4@bootsamp, 2, function(x) {
  any(is.na(x)) | any(is.nan(x)) | any(x==Inf) | any(x==-Inf)
})


#---------------------------------------------------------
#   Init fixed object
#_--------------------------------------------------------
l.fix <- l.hyp
net_group <- 'misc'
firm_i <- 'clarabridge'

#--------------------------------------------------------
#                REPLACE NA WITH MEDIAN
#----------------------------------------------------------
for (model in c('f0','f1','f2','f3','f4')) {
    fit <- l.fix[[net_group]][[firm_i]][[model]]
    fit@bootsamp <- apply(fit@bootsamp, 2, function(x) as.numeric(x, silent=T))
    for (i in 1:ncol(fit@bootsamp)) {
        fit@bootsamp[is.na(fit@bootsamp[,i]), i] <-  median(fit@bootsamp[,i], na.rm = T) 
    }
    l.fix[[net_group]][[firm_i]][[model]]@bootsamp <- fit@bootsamp
}

#---------------------------------------------------------
#             REMOVE BOOTSAMP OUTLIERS
#---------------------------------------------------------
for (model in c('f0', 'f1', 'f2', 'f3', 'f4')) {
  fit <- l.fix[[net_group]][[firm_i]][[model]]
  ol.list <- apply(fit@bootsamp, 2, function(x) which(x > median(x, na.rm = T) + 1.5*IQR(x, na.rm = T) 
                                                      | x < median(x, na.rm = T) - 1.5*IQR(x, na.rm = T)) )
  ol <- unique(unlist(ol.list))
  keep <- seq_len(nrow(fit@bootsamp))[ !( seq_len(nrow(fit@bootsamp)) %in% ol) ]
  fit@bootsamp <- fit@bootsamp[keep, ]
  l.fix[[net_group]][[firm_i]][[model]]@bootsamp <- fit@bootsamp
}

## Check visually
par(mfrow=c(3,4))
sapply(1:12, function(i)qqnorm(l.fix$misc$clarabridge$f3@bootsamp[,i], 
                               main=names(l.fix$misc$clarabridge$f0@coef)[i]))
sapply(1:12, function(i)hist(l.fix$misc$clarabridge$f3@bootsamp[,i],  breaks=19,
                               main=names(l.fix$misc$clarabridge$f0@coef)[i]))

## check remaining
remains <- sapply(l.fix$misc$clarabridge, function(m) nrow(m@bootsamp))

## sample 500 or lowest common count
samp2 <- ifelse(max(remains) < 500, max(remains), 500)
for (model in c('f0', 'f1', 'f2', 'f3', 'f4')) {
  fit <- l.fix[[net_group]][[firm_i]][[model]]
  keep <- sample(seq_len(nrow(fit@bootsamp)), size = samp2, replace = F)
  l.fix[[net_group]][[firm_i]][[model]]@bootsamp <- fit@bootsamp[keep, ]
}

#------------------------------------------------------------


#------------------------------------------------------------
#               Goodness of Fit
#------------------------------------------------------------

## CREATE TIME SERIES GOODNESS OF FIT NETWORK SAMPLES
# 2,4,6 == 2012,14,16
model <- 'f4'
net_group <- 'misc'
firm_i <- 'clarabridge'
indices <- c(3,6) #c(2,4,6)

l.g2 <- lapply(indices, function(i) {
  btergm::gof(l.fix$misc$clarabridge$f4, target= nets.sub[[i]],
              statistics=c(esp,dsp,deg,geodesic), nsim=50)
})
l.g3 <- c(list(g1tmp),l.g2)
# l.g4 <- lapply(indices, function(i) {
#   btergm::gof(l.fix$misc$clarabridge$f4, target= nets.sub[[i]],
#               statistics=c(esp,dsp,deg,geodesic), nsim=50)
# })

## PLOT time series GOF to file
gof.tmp <- l.g3
indices <- c(1,3,6)
net.years <- as.numeric(names(nets.sub)[indices]) - 1
n <- length(gof.tmp)
png('gof_f4_1000_v3.png', height=n*2.67, width=12, units='in', res=500)
  par(mfrow=c(n,4), mar=c(3,4,4.5,1))
  for (i in 1:n) {
    gi <- gof.tmp[[i]]
    for (j in 1:length(gi)) {
      btergm::plot(gi[[j]], main=paste(net.years[i], names(gi)[j]))
    }
  }
dev.off()


#-----------------------------------------------------------------#
#            GET DISTANCES
#_-----------------------------------------------------------------
firm_i <- 'clarabridge'
nets.sub <- firm.nets$misc[[firm_i]][-1]
dl <- sapply(nets.sub, function(net){
  g <- getIgraphFromNet(net)
  igraph::distances(g, v = V(g)[V(g)$name==firm_i])
}); rownames(dl) <- nets.sub[[length(nets.sub)]] %v% 'vertex.names'

## 'mopinion', 'converseon', 'lexalytics'
View(dl[which(dl[,'2017']==1),])

View(dl[which(dl[,'2017']==2),])

# 'ibm': 3->2; 2013-14
# 'satmetrix': 3->2; 2013-14
# 'networked-insighted' : 4->3->2; 2013-14-15
# 

#-----------------------------------------------------------------#
#                  PLOTS NETWORK SLICE
#-----------------------------------------------------------------#
focal.firm <- 'clarabridge'
competitors <- c('ibm','networked-insights','satmetrix', 'mopinion')
years <- c('2014', '2017')
png(sprintf('%s_net_time_slice_colored_dists_c42_s11.png',focal.firm), width=6*length(years), height=7.5, units='in', res=300)
  par(mfrow=c(1,length(years)))
  for (year_i in years) {
    ## max colors -------------------
    maxn <- 0
    for (tmp in years) {
      net <- firm.nets$misc$clarabridge[[tmp]]
      g <- getIgraphFromNet(net)
      g <- induced.subgraph(g, vids=V(g)[igraph::degree(g)>0])
      ngroups <- length(unique(multilevel.community(g)$membership))
      maxn <- max(maxn, ngroups)
    }
    rcolors <- rainbow(maxn, s=.8, alpha=.35)
    set.seed(11)
    rcolors <- rcolors[sample(seq_along(rcolors),size = length(rcolors),replace = F)]
    ##-------------------------------
    ##  Get period network
    net <- firm.nets$misc$clarabridge[[year_i]]
    g <- getIgraphFromNet(net)
    g <- induced.subgraph(g, vids=V(g)[igraph::degree(g)>0])
    V(g)$mem <- igraph::multilevel.community(g)$membership
    ## Plot
    plotCompNetAOMequalSize(g,
                            rcolors=rcolors,
                            competitors= competitors, 
                            focal.firm = focal.firm, is.focal.color = F,
                            layout.algo = layout.fruchterman.reingold,
                            seed=1111, margins = c(.1,.5,.1,2.5))
    ds <- igraph::distances(g, v = V(g)[V(g)$name==focal.firm], to=V(g)[V(g)$name%in%competitors] ) 
    leg <- sprintf('Yr = %s\nE = %s\nV = %s\nDistances:\n    %s = %s\n    %s = %s\n    %s = %s\n    %s = %s',
                   as.numeric(year_i)-1,ecount(g),vcount(g),
                   'IBM',ds[,competitors[1]],
                   'NI',ds[,competitors[2]],
                   "SM",ds[,competitors[3]],
                   'MO',ds[,competitors[4]])
    legend('topright', legend=leg, bty='n', cex = .8)
  }
dev.off()


## reference plot
ref.year <- '2014'
net <- firm.nets$misc$clarabridge[[ref.year]]
g <- getIgraphFromNet(net)
g <- induced.subgraph(g, vids=V(g)[igraph::degree(g)>0])
mem <- igraph::multilevel.community(g)$membership
png(sprintf('%s_%s_labeled_reference_CNG_plot.png',focal.firm,ref.year), height=20, width = 20, units='in', res=400)
  plotCompNetAOMequalSizeRefLabel(g, rcolors=rcolors, competitors= competitors, 
                        focal.firm = focal.firm,  layout.algo = layout.fruchterman.reingold,
                        seed=1111)
dev.off()
#-----------------------------------------------------------------
#              Plotting Predictors over time
#-----------------------------------------------------------------

year.names <- as.numeric(names(nets.sub)) - 1
probs <- c(.25,.5,.75)

fit <- l.fix$misc$clarabridge$f4

# qt <- apply(fit@effects, 2, function(x) quantile(x, probs = c(.25,.5,.75), na.rm = T))

## NET RISK
net_risk <- sapply(nets.sub, function(net) quantile(net %v% 'net_risk', probs = probs))
matplot(year.names, t(net_risk), type='o', pch=1:3, lty=1:3, col=c(1,2,4))

## CONSTRAINT
const <- sapply(nets.sub, function(net) quantile(net %v% 'constraint', probs = probs))
matplot(year.names, t(const), type='o', pch=1:3, lty=1:3, col=c(1,2,4))


# ## ABS DIFF CONSTRAINT
# abs_diff_const <- sapply(nets.sub, function(net) {
#   const <- net %v% 'constraint'
#   dm <- outer(const, const, "-")
#   x <- dm[lower.tri(dm, diag=F)]
#   quantile(x, probs = c(.05,.5,.95))
# })
# matplot(year.names, t(abs_diff_const), type='o', pch=1:3, lty=1:3, col=c(1,2,4))

## Cycles
## 3
cycle3 <- sapply(nets.sub, function(net) {
  k <- 3
  km <- as.data.frame(kcycle.census(net[,], maxlen = k, mode='graph'))
  tkm <- t(km)
  cnt <- tkm[, as.character(k)]
  quantile(cnt, probs = probs)
})
matplot(year.names, t(cycle3), type='o', pch=1:3, lty=1:3, col=c(1,2,4))

## 4
cycle4 <- sapply(nets.sub, function(net) {
  k <- 4
  km <- as.data.frame(kcycle.census(net[,], maxlen = k, mode='graph'))
  tkm <- t(km)
  cnt <- tkm[, as.character(k)]
  quantile(cnt, probs = probs)
})
matplot(year.names, t(cycle4), type='o', pch=1:3, lty=1:3, col=c(1,2,4))
## 5
cycle5 <- sapply(nets.sub, function(net) {
  k <- 5
  km <- as.data.frame(kcycle.census(net[,], maxlen = k, mode='graph'))
  tkm <- t(km)
  cnt <- tkm[, as.character(k)]
  quantile(cnt, probs = probs)
})
matplot(year.names, t(cycle5), type='o', pch=1:3, lty=1:3, col=c(1,2,4))

indices <- c(2,3,4,5)
## plot GRAPH TIME SLICE -- COLOR BY COMMUNITY
par(mfrow=c(2,2))
for (i in indices) {
  net <- nets.sub[[i]]
  g.tmp <- getIgraphFromNet(net)
  g.sub <- igraph::induced.subgraph(g.tmp, vids=V(g.tmp)[igraph::degree(g.tmp)>0])
  mem <- igraph::multilevel.community(g.sub)$membership
  plotCompNetAOM(g.sub, membership = mem, is.focal.color = T, focal.firm = 'clarabridge', seed=12)
}


##-----------------------------------------------------------------
#             probability interactions
#
#_-----------------------------------------------------------------
sig <- function(a) (1/(1+exp(-a)))[1]

fit <- l.fix$misc$clarabridge$f4

bnames <- names(fit@coef)
b <- fit@coef
xbar <- apply(fit@effects, 2, function(x) quantile(x, probs=.5, na.rm = T))

quantile(fit@effects$nodecov.net_risk)

sig(b%*%xbar)

predict.tie <- function(model,firm_j,firm_i='clarabridge',periods=NA) 
{
  net <- firm.nets$misc$clarabridge$`2017`
  p <- sapply(firm_j, function(fj){
    cat(sprintf('interpreting %s\n',fj))
    vi <- which(net %v% 'vertex.names' == firm_i )
    vj <- which(net %v% 'vertex.names'  == fj )
    return(interpret(model, type='tie', i=vi, j=vj))
  })
  n <- ifelse(length(firm_j) > 8, 8, length(firm_j))
  if(any(is.na(periods)))
    x <- seq_len(model@time.steps)
  matplot(x=x, y=p, xlab='Period',ylab='Conditional Tie Probability', type='b', lty=1:n, pch=1:n)
  legend('topleft', legend=firm_j[1:n], lty=1:n,pch=1:n, col=1:n)
  return(p)
}

periods <- 2011:2016
firms1 <- c('satmetrix','networked-insights','mopinion')
firms2 <- c('satmetrix','networked-insights','mopinion','ibm')
n1 <- ifelse(length(firms1) > 8, 8, length(firms1))
n2 <-  ifelse(length(firms2) > 8, 8, length(firms2))

## INTERPRET ( slow )
pred.1 <- predict.tie(fit, firms1, periods=periods)
pred.2 <- predict.tie(fit, firms2, periods=periods)

## PLOT
png('predicted_tie_probs_sp_vs_gen.png',height=6.5,width=6.5,units='in',res=200)
  par(mfrow=c(2,1), mar=c(4,4,3,1))
  matplot(2011:2016, pred.1,  main='Specialists',
          xlab='Period',ylab='Conditional Tie Probability',
          ylim=c(0,.0009), type='b', lty=1:n1, pch=1:n1, lwd=2)
  legend('topleft', legend=stringr::str_to_title(firms1), lty=1:n1,pch=1:n1, col=1:n1, lwd=2)
  ##
  matplot(2011:2016, pred.2, main="Specialists vs. Generalist",
          xlab='Period',ylab='Conditional Tie Probability', 
          ylim=c(0,.0085), type='b', lty=1:n2, pch=1:n2, lwd=2)
  legend('topleft', legend=stringr::str_to_title(firms2), lty=1:n2, pch=1:n2, col=1:n2, lwd=2)
dev.off()

png('predicted_tie_probs_ln.png',height=5,width=7,units='in',res=200)
matplot(2011:2016, pred.2,
        xlab='Period',ylab='Conditional Tie Probability (Ln scale)', log='y',
        type='b', lty=1:n2, pch=1:n2, lwd=2)
abline(v=2015,lty=3,col=3)
legend('topleft', legend=c(stringr::str_to_title(firms1),'IBM'), lty=1:n2, pch=1:n2, col=1:n2, lwd=2)
dev.off()


#-----------------------------------------------------------------
#  Predicting Edge probabilities
#-----------------------------------------------------------------

mmc <- lapply(nets.sub, function(net) as.matrix(net %n% 'mmc'))
sim <- lapply(nets.sub, function(net) as.matrix(net %n% 'similarity'))
ldv <- lapply(nets.sub, function(net) as.matrix(net %n% 'DV_lag'))
## (long running time; parallelization doesn't work on Windows)
ep <- btergm::edgeprob(l.fix$misc$clarabridge$f4)

#-----------------------------------------------------------------
# Correlations & Stats
#_---------------------------------------------------------------
skip <- c('nodematch.ipo_status.1','nodematch.ipo_status.0','edges', 'gwesp.fixed.0')
eff.sub <- fit@effects[, which( !(colnames(fit@effects) %in% skip))]
cr <- round(cor(eff.sub), 2)
cr[upper.tri(cr, diag=T)] <- NA
colnames(cr) <- seq_len(ncol(cr))
write.table(cr, file = 'f4_corr_mat.csv',sep=',') 


## summary stat
write.table(psych::describe(eff.sub), file = 'f4_summary_stats.csv',sep=',') 

## regression outpu
write.regtable(l.fix$misc$clarabridge, html = T, single.row = F, digits=3)




##_-------------------------------------------------------------------
#  Out of Sample GOF
#----------------------------------------------------------------------

ix <- 1:(length(nets.sub)-1)
mmc <- lapply(nets.sub[ix], function(net) as.matrix(net %n% 'mmc'))
sim <- lapply(nets.sub[ix], function(net) as.matrix(net %n% 'similarity'))
ldv <- lapply(nets.sub[ix], function(net) as.matrix(net %n% 'DV_lag'))

osfit <- btergm(
  nets.sub[ix] ~ edges + gwesp(0, fixed=T) + 
    #nodefactor('state_code') + 
    nodematch('state_code', diff=F) +
    nodecov('age') +   edgecov(mmc)  + edgecov(ldv) +
    nodematch('npm',diff=F) + 
    edgecov(sim)  +
    nodematch('ipo_status', diff=TRUE)  +
    nodecov('net_risk') +
    nodecov('constraint') + absdiff('constraint') + 
    cycle(3) + cycle(4) + cycle(5) 
  , R=100, parallel = "multicore", ncpus = detectCores())

osgof <- btergm::gof(osfit, target=nets.sub[length(nets.sub)], nsim=50, statistics=c(dsp,esp,deg,geodesic))

par(mfrow=c(2,2))
btergm::plot(osgof)
