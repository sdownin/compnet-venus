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


net_group <- 'cem'
firm_i <- 'clarabridge'
pd <- 6
R <- 2000

fits <- readRDS(sprintf("fits_cem_%s/%s_pd%s_R%s.rds", R, firm_i, pd, R))
f4 <- fits[[net_group]][[firm_i]]$m4


## UNCOMMENT this section to run Goodness of Fit and Predict Ties
# ## Goodness of Fit
# options(error=function() dump.frames(to.file=TRUE))
# f4.gof1 <- gof(f4,formula=m4,nsim=100,
#                statistics = c(esp, dsp, geodesic,deg, triad.undirected, 
#                               walktrap.modularity))
# plot(f4.gof1)
# saveRDS(f4.gof1, file="tergm_f4_gof1.rds")


## TERGM Micro-interpretation
## Predict Ties i--j (for i:=clarabridge and all j!=i) at period 6 (2016)
N <- nrow(f4@data$networks$`2017`[,])
df4.pij <- data.frame(i=rep(NA,N),
                     j=rep(NA,N),
                     firm_i=rep(NA,N),
                     t1=rep(NA,N),
                     t2=rep(NA,N),
                     t3=rep(NA,N),
                     t4=rep(NA,N),
                     t5=rep(NA,N)
                     # t6=rep(NA,N)
                     )

numPds <- length(f4@data$networks)
ts <- 1:numPds
netLatest <- f4@data$networks[[numPds]]
gLatest <- getIgraphFromNet(netLatest)
ego.j <- which(netLatest %v% 'vertex.names' == firm_i)
idx <- 1

for (j in ego.j) { ## ego firm column j
  for (i in 1:N) { ## alter firm row i
    if (i != j) {
      for (t in ts) { ## time periods
        df4.pij$i[i] <- i
        df4.pij$j[i] <- j
        df4.pij$firm_i[i] <- (f4@data$networks[[t]] %v% 'vertex.names')[i]
        colt <- paste0("t",t)
        df4.pij[i,colt] <- btergm::interpret(f4,i=i,j=j,t=t)
        cold <- paste0("d",t)
        df4.pij[i,cold] <- igraph::distances(getIgraphFromNet(f4@data$networks[[t]]), v = i, to = j)
        cat(sprintf("t:%s i = %s --> j = %s",t,i,j),'\n')
      }
    }
  }
  cat(paste0("finished j =",j))
}; df4.pij[is.na(df4.pij)] <- 0

saveRDS(df4.pij, file=sprintf("tergm_f4_R%s_pd%s_interpret_%s_pij.rds", R, pd, firm_i))


##-------------------- PLOT NETWORK in 2016 ----------------------------
cutoff <- .93

nbr.idx <- as.numeric(igraph::neighbors(gLatest, v=ego.j))
nbr.name <- names(igraph::neighbors(gLatest, v=ego.j))
awares <- which(df4.pij$t5 > quantile(df4.pij$t5, cutoff, na.rm = T)  &  !(df4.pij$i %in% nbr.idx) )
#awares <- which( df4.pij$i %in% nbr.idx )

## plot 1 x 2 probabilities and network
png('tergm_pij_cem_clarabridge_2016_scatter_graph_rivals.png', height=5, width = 10, units = 'in', res = 250)
    par(mfrow=c(1,2))
    par(mar=c(4.2,4.2,1,1))
    plot(log10(df4.pij$t5), 
         ylab='Log Probability of Tie (i,j)', xlab='Firm',
         col=sapply(1:N,function(x)ifelse(x %in% awares, 'darkred', rgb(.7,.7,.1,.7))), 
         pch=16 #sapply(1:N,function(x)ifelse(x %in% awares, 16, 1))
         )
    text(awares, log10(df4.pij$t5[awares]), pos = 3,
         labels = (f4@data$networks[[1]] %v% 'vertex.names')[awares],
         col='darkred', cex=.7)
    par(mar=c(.1,.1,.1,.1))
    plotCompNetColPredict(gLatest, df4.pij$t5, focal.firm = 'clarabridge', cutoff=cutoff )
    #plotCompNetColRivals(gLatest, df4.pij$t5, focal.firm = 'clarabridge' )
dev.off()

## Side histograph of probabilities 
png('tergm_pij_cem_clarabridge_2016_hist.png', height=3.5, width = 6, units = 'in', res = 250)
  hist(log10(df4.pij$t5), breaks=50, xlab="Log Probability of Tie (i,j)", 
       main="")
dev.off()



# par(mfrow=c(1,1),mar=c(.5,.5,1,.5))
# plotCompNetAOMequalSize(gs=gLatest, is.focal.color = F,
#                         focal.firm = firm_i)

par(mfrow=c(1,2))
plotCompNetColRivals(gLatest, df4.pij$t5, focal.firm = 'clarabridge')
plotCompNetColPredict(gLatest, df4.pij$t5, focal.firm = 'clarabridge', cutoff=.9 )


dat <- df4.pij[,c('t1','t2','t3','t4','t5')]
rownames(dat) <- df4.pij$firm_i
trend <- as.data.frame(t(apply(dat, MARGIN = 1, FUN = function(y){
  lm(y ~ x, data=data.frame(y=y,x=1:length(y)))$coef
})))

df4.pij <- merge(df4.pij, data.frame(firm_i=rownames(trend),trend=trend$x), by = 'firm_i')

trends <- t(df4.pij[ , c('t1','t2','t3','t4','t5')])
cols <- sapply(1:N,function(x)ifelse(x %in% awares, 'darkred', rgb(.7,.7,.1,.5)))
par(mfrow=c(1,1))
png('tergm_pij_cem_clarabridge_2012_2016_matplot.png', height=5, width = 4.5, units = 'in', res = 250)
  matplot(x=2012:2016,trends, type='o', pch=20, lty=1, 
        col=cols,  main="Prob. of Competitive Tie (i,j) by Year",
        xlab='Year',ylab="Log Probability of Tie (i,j)",
        log='y')
dev.off()

