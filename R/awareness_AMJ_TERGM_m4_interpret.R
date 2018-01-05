#---------------------------------------------------------------------
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
library(dplyr)

img_dir <- "C:/Users/T430/Google Drive/PhD/Dissertation/competition networks/envelopment"
proj_dir <- "C:/Users/T430/Google Drive/PhD/Dissertation/competition networks/compnet2"
data_dir <- file.path(proj_dir, "firm_nets_cem")
results_dir <- file.path(proj_dir, "results")
R_dir <-  file.path(proj_dir, "R")

setwd(proj_dir)
source(file.path(R_dir,'comp_net_functions.R'))
source(file.path(R_dir,'netrisk_functions.R'))


focal.firm <- 'qualtrics'
net_group <- 'cem'
d <- 3
pd <- 7
mx <- 4
R <- 2000
firm_i <- focal.firm

fit_file <- sprintf("%s/fit_winlocal_%s_pd%s_d%s_R%s_m%s.rds", proj_dir, firm_i, pd, d, R, mx)
f4 <- readRDS(fit_file)

mmc.all <- lapply(f4@data$networks, function(net) as.matrix(net %n% 'mmc'))

data_file <- sprintf("%s/%s_d%s.rds", data_dir, firm_i,d)
nets <- readRDS(data_file)
if (pd < length(nets)) nets <- nets[(length(nets)-pd+1):length(nets)]



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
R <- nrow(f4@boot$t)
df4.pij <- data.frame(i=rep(NA,N),
                     j=rep(NA,N),
                     firm_i=rep(NA,N),
                     t1=rep(NA,N),
                     t2=rep(NA,N),
                     t3=rep(NA,N),
                     t4=rep(NA,N),
                     t5=rep(NA,N),
                     t6=rep(NA,N)  )

numPds <- length(f4@data$networks)
netLatest <- f4@data$networks[[numPds]]
gLatest <- getIgraphFromNet(netLatest)
ego.j <- which(netLatest %v% 'vertex.names' == firm_i)
idx <- 1
ts <- 1:5  ## 1:numPds  ## only last period

for (j in ego.j) { ## ego firm column j
  for (i in 1:N) { ## alter firm row i
    if (i != j) {
      for (t in ts) { ## time periods
        mmc <- mmc.all[[t]]
        df4.pij$i[i] <- i
        df4.pij$j[i] <- j
        df4.pij$firm_i[i] <- (f4@data$networks[[t]] %v% 'vertex.names')[i]
        colt <- paste0("t",t)
        df4.pij[i,colt] <- btergm::interpret(f4,target=nets,i=i,j=j,t=t)
        cold <- paste0("d",t)
        df4.pij[i,cold] <- c(igraph::distances(getIgraphFromNet(f4@data$networks[[t]]), v = i, to = j))[1]
        cat(sprintf("t:%s i = %s --> j = %s",t,i,j),'\n')
      }
    }
  }
  cat(paste0("finished j =",j))
}; df4.pij[is.na(df4.pij)] <- 0
df4.pij$d6 <- as.vector(df4.pij$d6)

interp.file.path <- sprintf("%s/tergm_interpret_winlocal_all_%s_m4_R%s_pd%s_d%s_pij.rds", proj_dir, firm_i, R, pd, d)
saveRDS(df4.pij, file=interp.file.path)

##
# READ IN Pij data
##
df4.pij <- readRDS(interp.file.path)
df4.pij$d6mat <- NULL

##-------------------------- check rivals in top pij ------------------
vs <- igraph::neighbors(getIgraphFromNet(nets$`2017`), v = 416)
top <- df4.pij[df4.pij$t6 > quantile(df4.pij$t6, .963), 'firm_i']
all(top %in% names(vs))



## plot segmentation of rivals, awareness set, 
# plot(density(log10(df4.pij$t6), bw = .11))
# hist(log10(df4.pij$t6), breaks=24)

## hist + density
par(mfrow=c(1,1))
hist(log(df4.pij$t6), breaks=23, prob=T, col='gray', main="", xlab="Ln Probability of Tie (i,j)")
abline(v=log(quantile(df4.pij$t6, c(.785, .9618))), col='black', lty=3)
lines(density(log(df4.pij$t6), bw = .24), col='darkred', lwd=2)

##
# [1] 0.005516564
# > quantile(df4.pij$t6, c(.785, .9618))
# 78.5%      96.18% 
#   0.005963329 0.524079281 
awr <- df4.pij[df4.pij$t6 < quantile(df4.pij$t6, .963) 
               & df4.pij$t6 > quantile(df4.pij$t6, .79), 'firm_i']

##--------------------------Box Plot by Distance -----------------------
df4.pij$lnt6p1 <- log(1 + df4.pij$t6)
df4.pij$lnt6 <- log(df4.pij$t6)

df.tmp <- df4.pij[df4.pij$d6 != 0 & df4.pij$d6 != '0', ]
df.tmp$d6f <- as.factor(df.tmp$d6)
droplevels(df.tmp$d6f)

ggplot(aes(x=d6f, y=lnt6p1), data=df.tmp) + 
  geom_jitter(width = 0.3, col=rgb(.01,.01,.01,.25), pch=16) +
  # geom_boxplot(outlier.colour = "darkred", outlier.shape = 16) + 
  scale_y_log10() + 
  ylab("Conditional Probability of Tie (i,j)") +
  xlab("Competitive Distance (i,j)") + 
  ggtitle("Competitive Distance and Competition Formation") +
  theme_bw()

ggplot(aes(x=d6f, y=lnt6p1), data=df.tmp) + 
  # geom_jitter(width = 0.3, col=rgb(.01,.01,.01,.25), pch=16) +
  geom_boxplot(outlier.colour = "darkred", outlier.shape = 1, fill='gray') + 
  scale_y_log10() + 
  ylab("Conditional Probability of Tie (i,j)") +
  xlab("Competitive Distance (i,j)") + 
  ggtitle("Competitive Distance and Competition Formation") +
  theme_bw()

## LONG DF MULTI_PERIOD PLOT
dfl <- data.frame(firm_i=NA, i=NA, j=NA, year=NA, d=NA, p=NA)
years <- c('2011','2012','2013','2014','2015','2016')
for (x in 1:6) {
  p <- paste0('t',x)
  d <- paste0('d',x)
  yr <- years[x]
  tmp <- df4.pij[,c('firm_i', 'i', 'j', d, p)]
  tmp$year <- yr
  names(tmp) <- c('firm_i', 'i', 'j', 'd', 'p', 'year')
  dfl <- rbind(dfl, tmp)
}
dfl$d0 <- dfl$d
dfl$d <- sapply(dfl$d,function(x)ifelse(as.character(x) %in% c('1','2','3','4'), x, '5+'))
dfl <- na.omit(dfl)
dfl$year <- as.factor(dfl$year)
dfl$d <- as.factor(dfl$d)
dfl$i <- as.factor(dfl$i)

dfl2 <-
  dfl[dfl$d != 0 & dfl$d !='0', ] %>%
  group_by(d) %>%
  mutate(outlier = log10(p) > median(log10(p)) + IQR(log10(p)) * 2, 
         low.otl = log10(p) < median(log10(p)) - IQR(log10(p)) * 2,
         aware = p > quantile(p, .785),
         period= ifelse(year %in% c('2014','2015','2016'),'2014-16','2011-13'),
         Period= ifelse(year %in% c('2015','2016'),'2015-16',
                        ifelse(year %in% c('2013','2014'),'2013-14','2011-12'))) %>%
  ungroup
dfl2$period <- factor(dfl2$period, levels = c("2014-16", "2011-13"))
dfl2$aware.all <- sapply(dfl2$p, function(x) x > 0.00596)


dfl2 <- dfl2[ dfl2$year != "2016" | dfl2$d != Inf, ]

## SUBSET (not grouped) JITERRED OUTLIERS **************************
dfl3 <- dfl2[dfl2$year %in% c('2011','2012','2013','2014','2015','2016'), ]
dfl3 <- dfl3[ dfl3$d != Inf | dfl3$year != '2016' ,]
ggplot(dfl3) + aes(x = d, y = p) +
  geom_point(pch=16, col=rgb(0.105, 0.247, 0.074), 
             data=function(x)dplyr::filter_(x, ~ outlier),
             alpha=.9, lwd=2, position = 'jitter') +
  scale_fill_manual(values=c("#999999", "#ffffff", 'steelblue')) +
  scale_y_log10() + 
  ylab("Conditional Probability of Competitive Encounter") +
  xlab("Competitive Distance") + 
  # ggtitle("Competitive Distance and Competition Formation (2011-2016)") +
  theme_bw()
ggsave('qualtrics_microinter_pij_by_distances_outliers_2_opaque.png', 
       width = 5.5, height=5.5, units = 'in', dpi=200)

## NEW PLOT AWARENESS SET
dfl3 <- dfl2[dfl2$year %in% c('2011','2012','2013','2014','2015','2016'), ]
dfl3 <- dfl3[ dfl3$d != Inf | dfl3$year != '2016' ,]
ggplot(dfl3) + aes(x = d, y = p) +
  geom_point(pch=16, col='darkred', alpha=.8, lwd=2, position = 'jitter',
             data=function(x)dplyr::filter_(x, ~ (aware.all ))) +
  scale_fill_manual(values=c("#999999", "#ffffff", 'steelblue')) +
  scale_y_log10(c(0.099,1.001)) + 
  ylab("Conditional Probability of Competitive Encounter") +
  xlab("Competitive Distance (i,j)") + 
  # ggtitle("Competitive Distance and Competition Formation (2011-2016)") +
  theme_bw()
ggsave('qualtrics_microinter_pij_by_distances_outliers_2_opaque_5-8Inf.png', 
       width = 5.5, height=5.5, units = 'in', dpi=200)


## NEW PLOT AWARENESS SET
dfl3 <- dfl2[dfl2$year %in% c('2011','2012','2013','2014','2015','2016'), ]
dfl3 <- dfl3[ dfl3$d != Inf | dfl3$year != '2016' ,]
set.seed(269957)
ggplot(dfl3) + aes(x = d, y = log(p)) +
  # geom_point(pch=16, col='darkred', alpha=.8, lwd=2, position = 'jitter',
  #            data=function(x)dplyr::filter_(x, ~ (aware.all ))) +
  geom_jitter(width = 0.3, height = 0, pch=16, col='red', alpha=.8, lwd=2,
              data=function(x)dplyr::filter_(x, ~ (aware.all & !low.otl))) +
  scale_fill_manual(values=c("#999999", "#ffffff", 'steelblue')) +
  ylab("Conditional Ln Probability of Competitive Encounter") +
  xlab("Competitive Distance") + 
  ylim(-5.2,1.5) +
  # coord_trans(y="log10") +
  theme_bw() 
ggsave('qualtrics_microinter_pij_by_distances_outliers_3_logprob_5plus.png', 
       width = 5.5, height=5.5, units = 'in', dpi=200)

# 
# ## SUBSET group by  YEAR
# dfl3 <- dfl2[dfl2$year %in% c('2013','2014','2015','2016'), ]
# ggplot(dfl3) + aes(x = d, y = p, fill=year) +
#   geom_boxplot(aes(fill=year), outlier.shape = 1, outlier.alpha = .7) +
#   # geom_boxplot(aes(fill=factor(d)), outlier.colour = NA) +
#   # geom_point(aes(pch=21, position_jitterdodge(jitter.width = 0))) +
#   # geom_point(aes(color=year, pch=year), data=function(x)dplyr::filter_(x, ~ outlier), position = 'jitter') +
#   scale_fill_manual(values=c("#666666", "#ffffff", '#aaaaaa','steelblue')) +
#   scale_y_log10() + 
#   ylab("Conditional Probability of Tie (i,j)") +
#   xlab("Competitive Distance (i,j)") + 
#   ggtitle("Competitive Distance and Competition Formation") +
#   theme_bw()



# ## SUBSET (not grouped) Jittered
# dfl3 <- dfl2[dfl2$year %in% c('2011','2012','2013','2014','2015','2016'), ]
# dfl3 <- dfl3[ dfl3$d != Inf | dfl3$year != '2016' ,]
# ggplot(dfl3) + aes(x = d, y = p) +
#   # geom_boxplot(outlier.shape = 1, outlier.color = 'darkred', outlier.alpha = .6, fill='gray') +
#   geom_boxplot(aes(fill=period), outlier.colour = NA) +
#   #geom_point(aes(pch=21, position_jitterdodge(jitter.width = 0))) +
#   #geom_point(aes(color=year, pch=year), data=function(x)dplyr::filter_(x, ~ outlier), position = 'jitter') +
#   scale_fill_manual(values=c("#999999", "#ffffff", 'steelblue', 'red','yellow','green')) +
#   scale_y_log10() + 
#   ylab("Conditional Probability of Tie (i,j)") +
#   xlab("Competitive Distance (i,j)") + 
#   ggtitle("Competitive Distance and Competition Formation (2011-2016)") +
#   theme_bw()

# ## BY HALF PERIOD
# dfl3 <- dfl2[dfl2$year %in% c('2011','2012','2013','2014','2015','2016'), ]
# dfl3$period <- as.factor(dfl3$period)
# ggplot(dfl3) + aes(x = d, y = p, fill=period) +
#   geom_boxplot(aes(fill=period), outlier.shape = 1, outlier.color = 'darkred', outlier.alpha = .6, fill='gray') +
#   # geom_boxplot(aes(fill=factor(d)), outlier.colour = NA) +
#   # geom_point(aes(pch=21, position_jitterdodge(jitter.width = 0))) +
#   # geom_point(aes(color=year, pch=year), data=function(x)dplyr::filter_(x, ~ outlier), position = 'jitter') +
#   scale_fill_manual(values=c("#999999", "#ffffff", 'steelblue')) +
#   scale_y_log10() + 
#   ylab("Conditional Probability of Tie (i,j)") +
#   xlab("Competitive Distance (i,j)") + 
#   ggtitle("Competitive Distance and Competition Formation (2011-2016)") +
#   theme_bw()


# dfl3 <- dfl2[dfl2$year %in% c('2014','2016'), ]
# ggplot(dfl3) + aes(x = d, y = p, color = year) +
#   geom_boxplot(aes(color=year), outlier.shape = NA) + 
#   geom_point(aes(color=year, pch=year), data=function(x)dplyr::filter_(x, ~ outlier), position = 'jitter') +
#   scale_y_log10() + 
#   ylab("Conditional Probability of Tie (i,j)") +
#   xlab("Competitive Distance (i,j)") + 
#   ggtitle("Competitive Distance and Competition Formation") +
#   theme_bw()

##----------------------------------------------------------------------

## -------------------- AWARENESS DENSITY BY YEAR -----------------------
file.name <- file.path(img_dir,"qualtrics_tergm_microinterp_aware_density_period_bottom_space_hr.png")
png(file.name, height = 4.5, width = 6.5, units = 'in', res = 250)
ggplot(aes(x=p, fill=period, lty=period), data=dfl3) + 
  # geom_jitter(width = 0.3, col=rgb(.01,.01,.01,.25), pch=16) +
  geom_density(aes(fill=period, lty=period), alpha=.7, lwd=1.05) + 
  scale_x_log10() +
  xlab("Conditional Probability of Tie (i,j)") + 
  ggtitle("Interdependent Awareness Distribution over Time") +
  scale_fill_manual(values=c("steelblue", "#e5e5e5")) +
  ylim(c(-1.5,4)) +
  theme_bw()
dev.off()


##-------------------- PLOT NETWORK in 2016 ----------------------------
cutoff <- .8
# quant <- 0.00596

nbr.idx <- as.numeric(igraph::neighbors(gLatest, v=ego.j)) 
nbr.name <- names(igraph::neighbors(gLatest, v=ego.j)) 
# awares <- which(df4.pij$t6 > quantile(df4.pij$t6, cutoff, na.rm = T)  &  !(df4.pij$i %in% nbr.idx) ) 
awares <- which(df4.pij$t6 > quant) 
#awares <- which( df4.pij$i %in% nbr.idx ) 
df4.pij$t6[which(df4.pij$t6 <= 1e-6 )] <- 1e-3 


png('qualtrics_microinterp_ln_prob_two_color_lightred_NETWORK.png', height=8.5, width=8.5, units='in', res = 200)
  par(mar=c(.1,.1,.1,.1), mfrow=c(1,1))
  plotCompNetColPredict(gLatest, df4.pij$t6, focal.firm = 'qualtrics', cutoff=cutoff, layout.algo = layout.fruchterman.reingold )
dev.off()


# ## plot 1 x 2 probabilities and network
# png('tergm_pij_cem_qualtrics_winlocal_2016_scatter_graph_rivals_lightred.png', height=5, width = 10, units = 'in', res = 250)
#     plot(density(log(df4.pij$t6))); par(xpd=F) ;abline(v=-5.1 )
#     cat(sprintf("n > q: %s", length(df4.pij$t6[df4.pij$t6 > quantile(df4.pij$t6, .8)])))
#     ##  
#     png('qualtrics_microinterp_ln_prob_index_two_color_narrow.png', height=6.5, width=3.5, units='in', res = 200)
#       par(mfrow=c(1,1), mar=c(4.2,4.2,1,1))
#       plot(log(df4.pij$t6), 
#            ylab='Ln Probability of Tie (i,j)', xlab='Firm Index',
#            col=sapply(1:N,function(x)ifelse(x %in% awares, 'darkred', rgb(.3,.3,.3,1))), 
#            pch=sapply(1:N,function(x)ifelse(x %in% awares, 16, 1))
#            # pch=16 #sapply(1:N,function(x)ifelse(x %in% awares, 16, 1)),
#            # log='y'
#            )
#     dev.off()
#     # col2 <- c(rgb(.7,.7,.7,.6), 'darkred')  ## (low,  high)
#     # text(awares, log(df4.pij$t6[awares]), pos = 3,
#     #      labels = V(gLatest)$name[awares],
#     #      col='darkred', cex=.7)
#     ##
#     png('qualtrics_microinterp_ln_prob_two_color_lightred_NETWORK.png', height=8.5, width=8.5, units='in', res = 200)
#       par(mar=c(.1,.1,.1,.1), mfrow=c(1,1))
#       plotCompNetColPredict(gLatest, df4.pij$t6, focal.firm = 'qualtrics', cutoff=cutoff, layout.algo = layout.fruchterman.reingold )
#     dev.off()
#     # plotCompNetColPredict(gLatest, df4.pij$t6, focal.firm = 'qualtrics', cutoff=cutoff, layout.algo = layout.kamada.kawai )
#     #plotCompNetColRivals(gLatest, df4.pij$t6, focal.firm = 'clarabridge' )
# dev.off()

## Side histograph of probabilities 
png('tergm_pij_cem_qualtrics_winlocal_2016_hist.png', height=3.5, width = 6, units = 'in', res = 250)
  hist(log10(df4.pij$t6), breaks=50, xlab="Log Probability of Tie (i,j)", 
       main="")
dev.off()



# par(mfrow=c(1,1),mar=c(.5,.5,1,.5))
# plotCompNetAOMequalSize(gs=gLatest, is.focal.color = F,
#                         focal.firm = firm_i)

par(mfrow=c(1,2))
plotCompNetColRivals(gLatest, df4.pij$t6, focal.firm = focal.firm)
plotCompNetColPredict(gLatest, df4.pij$t6, focal.firm = focal.firm, cutoff=.9 )


dat <- df4.pij[,c('t1','t2','t3','t4','t5','t6')]
rownames(dat) <- df4.pij$firm_i
trend <- as.data.frame(t(apply(dat, MARGIN = 1, FUN = function(y){
  lm(y ~ x, data=data.frame(y=y,x=1:length(y)))$coef
})))

df4.pij <- merge(df4.pij, data.frame(firm_i=rownames(trend),trend=trend$x), by = 'firm_i')

trends <- t(df4.pij[ , c('t1','t2','t3','t4','t5','t6')])
cols <- sapply(1:N,function(x)ifelse(x %in% awares, 'darkred', rgb(.7,.7,.1,.5)))
par(mfrow=c(1,1))
png('tergm_pij_cem_qualtrics_winlocal_2012_2016_matplot.png', height=5, width = 4.5, units = 'in', res = 250)
  matplot(x=2012:2016,trends, type='o', pch=20, lty=1, 
        col=cols,  main="Prob. of Competitive Tie (i,j) by Year",
        xlab='Year',ylab="Log Probability of Tie (i,j)",
        log='y')
dev.off()

