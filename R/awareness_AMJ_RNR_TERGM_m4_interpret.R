library(igraph)
library(intergraph)
library(btergm)
library(xergm)
library(parallel)
library(texreg)
library(stringr)
library(reshape2)
library(dplyr)
library(lattice)
library(ggplot2)

## DIRECTORIES

work_dir <- "C:/Users/T430/Google Drive/PhD/Dissertation/competition networks/compnet2"
data_dir <- "C:/Users/T430/Google Drive/PhD/Dissertation/competition networks/compnet2/firm_nets_rnr"
results_dir <- "C:/Users/T430/Google Drive/PhD/Dissertation/competition networks/compnet2/amj_rnr_results"
img_dir  <- "C:/Users/T430/Google Drive/PhD/Dissertation/competition networks/envelopment/img"


## set woring dir
setwd(work_dir)

## -----------Model Results Settings-----
name_i <- 'qualtrics'
d <- 3
R <- 500
nPeriods <- 11
m_x <- 'm4'
##----------------------------------------

## results
fits.file <- sprintf('%s/fit_winlocal_%s_pd%s_d%s_R%s_%s.rds', 
                     results_dir, name_i, nPeriods, d, R, m_x)
fits <- readRDS(fits.file)

##  network data
nets.file <- file.path(data_dir,sprintf('%s_d%s.rds',firm_i,d))
nets <- readRDS(nets.file)

## create igraph lists
gs <- lapply(nets, asIgraph)


## visualize effects bootstrap distributions
effects <- names(fits@effects)
par(mfrow=c(3,3))
for (effect in effects) {
  qqnorm(fits@boot$t[,effect], main=effect)
}
par(mfrow=c(3,3))
for (effect in effects) {
  x <- fits@boot$t[,effect]
  dr <- diff(range(x))
  xl1 <- min(0-(.1*dr), min(x))
  xl2 <- max(.1*dr, max(x))
  ci <- quantile(x, c(.025,.975))
  hist(x, main=effect, breaks=13, col=rgb(.2,.2,.2,.2), xlim=c(xl1,xl2))
  abline(v=ci, col='red',lwd=2)
  abline(v=0, col='black',lwd=1, lty=2)
  segments(x0=ci[1],x1=ci[2],y0=0,y1=0,col='red',lwd=2)
}

# library()
# par(mfrow=c(1,1))
# ci95 <- c(.025,.5,.975)
# coef.mat <- t(apply(fits@boot$t,2,function(x)quantile(x,ci95)))
# matplot(x=df.coef, y=1:nrow(coef.mat))


##===============================================
## interpreation data frame i-->j (for all t)
##-----------------------------------------------
g <- asIgraph(nets[[length(nets)]])
vcnt <- vcount(g)
time.steps <- fits@time.steps
firm.names <-  V(g)$vertex.names
v.focal <- as.integer( V(g)[V(g)$vertex.names==name_i] )

## make data frame of predicted probabilities
# for (t in 1:time.steps)
#   idf[,paste0('t',t)]<- NA
# ## time columns
# tcolnames <- names(idf)[grep(names(idf), pattern = 't\\d{1,3}', perl = T)]

## competitor interpretation loop
interp.df.file <- sprintf('%s/interpret_winlocal_%s_pd%s_d%s_R%s_%s.csv', 
                     results_dir, name_i, nPeriods, d, R, m_x)
idf <- data.frame()
for (j in 1:vcount(g)) {
  cat(sprintf('%s:  %s\n',j,V(g)$vertex.names[j]))
  j.name <- V(g)$vertex.names[j]
  if (j == v.focal) {
    probs <- rep(0, time.steps)
  } else {
    probs <- btergm::interpret(fits, type='tie', i=v.focal, j=j)
  }
  for (t in 1:length(probs)) {
    d <- igraph::distances(gs[[t+1]], v = v.focal, to = j)[1,1]
    tmp <- data.frame(i=v.focal, j=j, i.name=name_i, j.name=j.name, t=t, d=d, p=probs[t])
    idf <- rbind(idf, tmp)
  }
  if (j %% 10 == 0)  write.csv(x = idf, file = interp.df.file)
}
write.csv(x = idf, file = interp.df.file)

## add covariates
idf$genidx_multilevel <- NA
idf$njobs_multilevel <- NA
idf$cent_pow_n0_4 <- NA
idf$absdiff_pow_n0_4 <- NA
idf$year <- NA
for (row in 1:nrow(idf)) {
  t <- idf$t[row]
  i <- idf$i[row]
  j <- idf$j[row]
  # idf$d[row] <- igraph::distances(gs[[t+1]], v = i, to = j)[1,1]
  idf$year[row] <- 2006 + as.integer(t)
  idf$genidx_multilevel[row] <- (nets[[t+1]] %v% 'genidx_multilevel')[j]
  idf$njobs_multilevel[row] <- (nets[[t+1]] %v% 'njobs_multilevel')[j]
  idf$cent_pow_n0_4[row] <- (nets[[t+1]] %v% 'cent_pow_n0_4')[j]
  idf$absdiff_pow_n0_4[row] <- abs((nets[[t+1]] %v% 'cent_pow_n0_4')[j] - (nets[[t+1]] %v% 'cent_pow_n0_4')[i])
  if(row %% 300 == 0) cat(sprintf('row %s\n',row))  
}

## distance category
idf$d_cat <- as.factor(sapply(idf$d,function(x)ifelse(as.character(x) %in% c('1','2','3','4'), as.character(x), '5+')))
idf$i <- as.factor(idf$i)
idf$year <- as.factor(idf$year)

## aware.all.cutoff
aware.all.cutoff <- 0.00205

## mutate group-period factors
idf2 <-
  idf[idf$d != 0 & idf$d !='0', ] %>%
  group_by(d) %>%
  mutate(
    outlier = log10(p) > median(log10(p)) + IQR(log10(p)) * 2, 
    low.otl = log10(p) < median(log10(p)) - IQR(log10(p)) * 2,
    aware = p > quantile(p, .5),
    h1 = ifelse(genidx_multilevel >= quantile(genidx_multilevel,.75), 'Q4', ifelse(genidx_multilevel >= quantile(genidx_multilevel,.5), 'Q3', ifelse(genidx_multilevel >= quantile(genidx_multilevel,.25), 'Q2','Q1'))),
    Diversification = ifelse(genidx_multilevel >= quantile(genidx_multilevel,.5), 'High', 'Low'),
    `Competitive Asymmetry` = ifelse(absdiff_pow_n0_4 >= quantile(absdiff_pow_n0_4,.5), 'High', 'Low'),
    h2 = ifelse(absdiff_pow_n0_4 >= quantile(absdiff_pow_n0_4,.75), 'Q4', ifelse(absdiff_pow_n0_4 >= quantile(absdiff_pow_n0_4,.5), 'Q3', ifelse(absdiff_pow_n0_4 >= quantile(absdiff_pow_n0_4,.25), 'Q2','Q1')))
  ) %>% 
  ungroup
## add aware.all boolean
idf2$aware.all <- sapply(idf2$p, function(x) x > aware.all.cutoff)

## manual colors
colors2 <- c( "#333333", '#aaaaaa')
colors4 <- c( '#222222', '#aaaaaa', '#555555', '#888888')

## Data for hypotheses interaction plots
idf3 <- idf2[idf2$year %in% c('2007','2008','2009','2010','2011','2012','2013','2014','2015','2016'), ]
idf3 <- idf3[ idf3$d != Inf | idf3$year != '2016', ]

##====================================================
## Hostility Profile (awareness set) Scatter Facet plot
##----------------------------------------------------
set.seed(269951)
dodge.width <- .73
## plot H1 Interaction with distance
ggplot(idf3) + aes(x = d_cat, y = log(p), color=Diversification, pch=Diversification) +
  geom_point(position=position_jitterdodge(dodge.width=dodge.width), lwd=2.5,
             data=function(x)dplyr::filter_(x, ~ (aware.all & !low.otl))) +
  scale_color_manual(values=colors2) +
  ylab("Conditional Ln Probability of Competitive Encounter") +
  xlab("Competitive Distance") + 
  ylim(-6.1,1.5) +
  theme_classic() +  theme(legend.position="top")
## plot H2 Interaction with distance
ggplot(idf3) + aes(x = d_cat, y = log(p), color=`Competitive Asymmetry`, pch=`Competitive Asymmetry`) +
  geom_point(position=position_jitterdodge(dodge.width=dodge.width), lwd=2.5,
             data=function(x)dplyr::filter_(x, ~ (aware.all & !low.otl))) +
  scale_color_manual(values=colors2) +
  ylab("Conditional Ln Probability of Competitive Encounter") +
  xlab("Competitive Distance") + 
  ylim(-6.1,1.5) +
  theme_classic() + theme(legend.position="top")


##==========================================
## ALL OBSERVATIONS Box & Whisker + DOT plot
##------------------------------------------
set.seed(26995)
dodge.width <- .8
## plot H1 Interaction with distance
ggplot(idf3) + aes(x = d_cat, y = log(p), color=Diversification, pch=Diversification) +
  geom_point(position=position_jitterdodge(dodge.width=dodge.width), lwd=2.5,
             data=function(x)dplyr::filter_(x, ~ (!low.otl))) +  #aware.all & 
  geom_boxplot(fill='white', outlier.colour = NA, position = position_dodge(width = dodge.width), lwd=1.1) +
  scale_color_manual(values=c( "#333333", '#aaaaaa')) +
  ylab("Conditional Ln Probability of Competitive Encounter") +
  xlab("Competitive Distance") + 
  theme_classic() +  theme(legend.position="top")
## plot H2 Interaction with distance
ggplot(idf3) + aes(x = d_cat, y = log(p), color=`Competitive Asymmetry`, pch=`Competitive Asymmetry`) +
  geom_point(position=position_jitterdodge(dodge.width=dodge.width), lwd=2.5,
             data=function(x)dplyr::filter_(x, ~ (!low.otl))) +  # aware.all & 
  geom_boxplot(fill='white', outlier.colour = NA, position = position_dodge(width = dodge.width), lwd=1.1) +
  scale_color_manual(values=c( "#333333", '#aaaaaa')) +
  ylab("Conditional Ln Probability of Competitive Encounter") +
  xlab("Competitive Distance") + 
  theme_classic() + theme(legend.position="top")

##=============================================================
## ALL OBSERVATIONS Box & Whisker + DOT plot + hotility profile cutoff line  
##----------------------------------------------------------
set.seed(26995)
dodge.width <- .8
## plot H1 Interaction with distance
ggplot(idf3) + aes(x = d_cat, y = log(p), color=Diversification, pch=Diversification) +
  geom_point(position=position_jitterdodge(dodge.width=dodge.width), lwd=2.5,
             data=function(x)dplyr::filter_(x, ~ (!low.otl & aware.all))) +  # & aware.all 
  geom_boxplot(fill='white', outlier.colour = NA, position = position_dodge(width = dodge.width), lwd=1.1) +
  geom_hline(yintercept=log(aware.all.cutoff), lty=2, lwd=1.5, col='darkred') +
  scale_color_manual(values=c( "#333333", '#aaaaaa')) +
  ylab("Conditional Ln Probability of Competitive Encounter") +
  xlab("Competitive Distance") + 
  ylim(-10.8, 2.5) +
  theme_classic() +  theme(legend.position="top")
## plot H2 Interaction with distance
ggplot(idf3) + aes(x = d_cat, y = log(p), color=`Competitive Asymmetry`, pch=`Competitive Asymmetry`) +
  geom_point(position=position_jitterdodge(dodge.width=dodge.width), lwd=2.5,
             data=function(x)dplyr::filter_(x, ~ (!low.otl & aware.all))) +  # & aware.all  
  geom_boxplot(fill='white', outlier.colour = NA, position = position_dodge(width = dodge.width), lwd=1.1) +
  geom_hline(yintercept=log(aware.all.cutoff), lty=2, lwd=1.5, col='darkred') +
  scale_color_manual(values=c( "#333333", '#aaaaaa')) +
  ylab("Conditional Ln Probability of Competitive Encounter") +
  xlab("Competitive Distance") + 
  ylim(-10.8, 2.5) +
  theme_classic() + theme(legend.position="top")

##==========================================
## ALL OBSERVATIONS Box & Whisker only
##------------------------------------------
set.seed(26995)
dodge.width <- .8
## plot H1 Interaction with distance
ggplot(idf3) + aes(x = d_cat, y = log(p), color=Diversification, pch=Diversification) +
  geom_boxplot(fill='white', outlier.colour = NA, position = position_dodge(width = dodge.width), lwd=1.1) +
  scale_color_manual(values=c( "#333333", '#aaaaaa')) +
  ylab("Conditional Ln Probability of Competitive Encounter") +
  xlab("Competitive Distance") + 
  theme_classic() +  theme(legend.position="top")
## plot H2 Interaction with distance
ggplot(idf3) + aes(x = d_cat, y = log(p), color=`Competitive Asymmetry`, pch=`Competitive Asymmetry`) +
  geom_boxplot(fill='white', outlier.colour = NA, position = position_dodge(width = dodge.width), lwd=1.1) +
  scale_color_manual(values=c( "#333333", '#aaaaaa')) +
  ylab("Conditional Ln Probability of Competitive Encounter") +
  xlab("Competitive Distance") + 
  theme_classic() + theme(legend.position="top")





##==========================================
## ALL OBSERVATIONS Median Ribbon line:  H1
##------------------------------------------
idfh1 <- plyr::ddply(.data = idf3, .variables = c('d_cat','h1'), summarise,
                     p_l95=quantile(p,.025),
                     p_u95=quantile(p,.975),
                     p_mean=mean(p), 
                     p_median=median(p) )
dodge.width <- 0
ggplot(idfh1) + aes(x=as.integer(d_cat), y=log(p_median), color=h1, pch=h1) +
  geom_point(lwd=4, position = position_dodge(width = dodge.width)) +
  geom_line(lwd=1.3, position = position_dodge(width = dodge.width)) +
  # geom_ribbon(aes(ymin=log(p_l95), ymax=log(p_u95), x=as.integer(d_cat)), alpha = 0.1) +
  scale_color_manual(values=colors4) +
  ylab("Conditional Ln Probability of Competitive Encounter") +
  xlab("Competitive Distance") + 
  # ylim(-6.1,1.5) +
  theme_classic() + theme(legend.position="top")


##==========================================
## ALL OBSERVATIONS Median Ribbon line:  H2
##------------------------------------------
idfh2 <- plyr::ddply(.data = idf3, .variables = c('d_cat','h2'), summarise,
                     p_l95=quantile(p,.025),
                     p_u95=quantile(p,.975),
                     p_mean=mean(p), 
                     p_median=median(p) )
dodge.width <- 0
ggplot(idfh2) + aes(x=as.integer(d_cat), y=log(p_median), color=h2, pch=h2) +
  geom_point(lwd=4, position = position_dodge(width = dodge.width)) +
  geom_line(lwd=1.3, position = position_dodge(width = dodge.width)) +
  # geom_ribbon(aes(ymin=log(p_l95), ymax=log(p_u95), x=as.integer(d_cat)), alpha = 0.1) +
  scale_color_manual(values=colors4) +
  ylab("Conditional Ln Probability of Competitive Encounter") +
  xlab("Competitive Distance") + 
  # ylim(-6.1,1.5) +
  theme_classic() + theme(legend.position="top")








ggplot(aes(x=t, y=p, color=as.factor(j)), data=idf) + 
  geom_line() + 
  geom_point() + 
  scale_y_log10() +
  theme(legend.position="none")



##########################################################################


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

##########################################################################




data.sub <- idf[idf$d<Inf,]
xyplot(p ~ d , groups=j.name, type='b', data=data.sub, auto.key = T)

xyplot(log(p) ~ t, groups=j.name, type='b', data=data.sub, auto.key = T)


## GOF
gf <- gof(fits, target=nets.all[length(nets.all)],
          nsim=50, 
          statistics=c(dsp, esp, deg, ideg, 
                       geodesic, rocpr, walktrap.modularity))
par(mfrow=c(1,5))
plot(gf)


## Compare to GLM
X <- cbind(fits@response,fits@effects)
fixnames <- unname(sapply(names(fits@effects),function(x)str_replace_all(x,"[\\[i\\]]","")))
names(X) <- c('y',fixnames)

formula1 <- as.formula(sprintf('y ~ %s',paste(fixnames,collapse=" + ")))
glfit <- glm(formula1 ,
             data=X,
             family=binomial(link = "logit"))

screenreg(list(glm=glfit,tergm=fits), ci.force = T, digits=3)


fitm <- mtergm(get(m_x))
plot(mcmc.diagnostics(fitm@ergm))

screenreg(list(glm=glfit,btergm=fits,mtergm=fitm), digits=3)



m0 <-   nets ~ edges + gwesp(0, fixed = T) +
  nodematch("ipo_status", diff = F) +
  nodematch("state_code", diff = F) +
  nodecov("age") + absdiff("age") +
  # edgecov(mmc) + 
  # edgecov(cpp) + #edgecov(cpc) +
  nodecov("njobs_multilevel") + #absdiff("njobs_multilevel") +
  nodecov("cent_pow_n0_2") + absdiff("cent_pow_n0_2") +
  cycle(3)  #+ #cycle(4) + cycle(5) + 




##
# DEFINE MODEL and MODEL NAME TO COMPUTE
##
m_x <- 'm0'
##
# SET RESAMPLES
##
R <- 30


## RUN TERGM
fits0 <- btergm(get(m_x), R=R, parallel = "multicore", ncpus = detectCores())

saveRDS(fits0, 'test_nets_qualtrics_d2.rds')

screenreg(fits0)

gf0 <- gof(fits0, nsim=30, statistics=c(dsp, esp, deg, ideg, geodesic, rocpr, 
                                      walktrap.modularity))
plot(gf0)
