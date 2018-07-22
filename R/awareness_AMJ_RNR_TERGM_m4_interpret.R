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
firm_i <- name_i
d <- 3
R <- 500
nPeriods <- 11
m_x <- 'm4'
##----------------------------------------

## FUNCTIONS FOR NAs REMOVED
.med <- function(x){return(median(x, na.rm=TRUE))}
.min <- function(x){return(min(x, na.rm=TRUE))}
.max <- function(x){return(max(x, na.rm=TRUE))}
.avg <- function(x){return(mean(x, na.rm=TRUE))}
.std <- function(x){return(sd(x, na.rm=TRUE))}
.qtl <- function(x, probs){return(quantile(x, probs=probs, na.rm=TRUE))}
.iqr <- function(x){return(IQR(x, na.rm=TRUE))}

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

if (file.exists(interp.df.file)) {
  ## READ IN PREDICTED PROBS
  idf <- read.csv(interp.df.file)
} else {
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
}



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

## remove probabilities and covariates (set NA) for Inf distance (firm's not in component that pd)
idf$p[idf$d==Inf] <- NA
idf$genidx_multilevel[idf$d==Inf] <- NA
idf$njobs_multilevel[idf$d==Inf] <- NA
idf$cent_pow_n0_4[idf$d==Inf] <- NA
idf$absdiff_pow_n0_4[idf$d==Inf] <- NA

## distance category
idf$d_cat <- as.factor(sapply(idf$d,function(x){
  xstr <- as.character(x)
  ifelse(xstr %in% c('1','2','3','4'), xstr, ifelse(x==Inf, NA, '5+'))
  }))
idf$i <- as.factor(idf$i)
idf$year <- as.factor(idf$year)

## aware.all.cutoff
aware.all.cutoff <- 0.001

## mutate group-period factors
idf2 <-
  idf[idf$d != 0 & idf$d !='0', ] %>%
  group_by(d) %>%
  mutate(
    outlier = log10(p) > .med(log10(p)) + .iqr(log10(p)) * 2, 
    low.otl = log10(p) < .med(log10(p)) - .iqr(log10(p)) * 2,
    aware.med = p > .qtl(p, .5),
    aware.cutoff = p > aware.all.cutoff,
    Diversification = ifelse(is.na(genidx_multilevel), NA, 
                             ifelse(genidx_multilevel >= .qtl(genidx_multilevel,.5), 'High', 
                                    'Low')),
    `Competitive Asymmetry` = ifelse(is.na(absdiff_pow_n0_4), NA, 
                                     ifelse(absdiff_pow_n0_4 >= .qtl(absdiff_pow_n0_4,.5), 'High',
                                            'Low')),
    h1 = ifelse(is.na(genidx_multilevel), NA, 
                ifelse(genidx_multilevel >= .qtl(genidx_multilevel,.75), 'Q4', 
                       ifelse(genidx_multilevel >= .qtl(genidx_multilevel,.5), 'Q3', 
                              ifelse(genidx_multilevel >= .qtl(genidx_multilevel,.25), 'Q2',
                                     'Q1')))),
    h2 = ifelse(is.na(absdiff_pow_n0_4), NA, 
                      ifelse(absdiff_pow_n0_4 >= .qtl(absdiff_pow_n0_4,.75), 'Q4', 
                             ifelse(absdiff_pow_n0_4 >= .qtl(absdiff_pow_n0_4,.5), 'Q3', 
                                    ifelse(absdiff_pow_n0_4 >= .qtl(absdiff_pow_n0_4,.25), 'Q2',
                                           'Q1'))))
  ) %>% 
  ungroup
## add aware.all boolean
idf2$aware.all <- sapply(idf2$p, function(x) x > aware.all.cutoff)

## manual colors
colors2rw <- c('black','red')
colors2 <- c( "#333333", '#aaaaaa')
colors4 <- c( '#222222', '#aaaaaa', '#555555', '#888888')
colors5 <- c( '#111111', '#333333','#555555', '#777777','#999999')


## Data for hypotheses interaction plots
# idf3 <- idf2[idf2$year %in% c('2007','2008','2009','2010','2011','2012','2013','2014','2015','2016'), ]
idf3 <- idf2[idf2$year %in% c('2007','2016'), ]
#
idf3 <- idf3[ idf3$d != Inf | idf3$year != '2016', ]

##====================================================
## Hostility Profile (awareness set) Scatter Facet plot
##----------------------------------------------------
set.seed(269951)
dodge.width <- 0
##
ggplot(idf3) + aes(x = d_cat, y = log(p), color=aware.all, pch=aware.all) +
  geom_point(position=position_jitterdodge(dodge.width=dodge.width), lwd=2.5,
             data=function(x)dplyr::filter_(x, ~ !low.otl)) +
  scale_color_manual(values=colors2rw) +
  scale_shape_manual(values=c(1,16)) +
  facet_wrap(~ year) +
  ylab("Conditional Ln Probability of Competitive Encounter") +
  xlab("Competitive Distance") + 
  ylim(-10.5,1.5) +
  geom_hline(yintercept = log(aware.all.cutoff), lty=3) +
  theme_bw() +  theme(legend.position='none')


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
  facet_wrap(~ year) +
  ylab("Conditional Ln Probability of Competitive Encounter") +
  xlab("Competitive Distance") + 
  ylim(-10.5,1.5) +
  theme_classic() +  theme(legend.position="top")
## plot H2 Interaction with distance
ggplot(idf3) + aes(x = d_cat, y = log(p), color=`Competitive Asymmetry`, pch=`Competitive Asymmetry`) +
  geom_point(position=position_jitterdodge(dodge.width=dodge.width), lwd=2.5,
             data=function(x)dplyr::filter_(x, ~ (aware.all & !low.otl))) +
  scale_color_manual(values=colors2) +
  facet_wrap(~ year) +
  ylab("Conditional Ln Probability of Competitive Encounter") +
  xlab("Competitive Distance") + 
  ylim(-10,1.5) +
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
## ALL OBSERVATIONS Median  line:  Diversification & Competitive Asymmetry -- 2 lines
##------------------------------------------
## H1
idfh1 <- plyr::ddply(.data = idf3, .variables = c('d_cat','Diversification'), summarise,
                     p_l95=.qtl(p,.025),
                     p_u95=.qtl(p,.975),
                     p_mean=.avg(p), 
                     p_median=.med(p) )
dodge.width <- 0
ggplot(idfh1) + aes(x=as.integer(d_cat), y=log(p_median), color=Diversification, pch=Diversification) +
  geom_point(lwd=4, position = position_dodge(width = dodge.width)) +
  geom_line(lwd=1.3, position = position_dodge(width = dodge.width)) +
  # geom_ribbon(aes(ymin=log(p_l95), ymax=log(p_u95), x=as.integer(d_cat)), alpha = 0.1) +
  scale_color_manual(values=colors4) +
  ylab("Conditional Ln Probability of Competitive Encounter") +
  xlab("Competitive Distance") + 
  # ylim(-6.1,1.5) +
  theme_classic() + theme(legend.position="top")
## H2
idfh2 <- plyr::ddply(.data = idf3, .variables = c('d_cat','`Competitive Asymmetry`'), summarise,
                     p_l95=.qtl(p,.025),
                     p_u95=.qtl(p,.975),
                     p_mean=.avg(p), 
                     p_median=.med(p) )
dodge.width <- 0
ggplot(idfh2) + aes(x=as.integer(d_cat), y=log(p_median), color=`Competitive Asymmetry`, pch=`Competitive Asymmetry`) +
  geom_point(lwd=4, position = position_dodge(width = dodge.width)) +
  geom_line(lwd=1.3, position = position_dodge(width = dodge.width)) +
  # geom_ribbon(aes(ymin=log(p_l95), ymax=log(p_u95), x=as.integer(d_cat)), alpha = 0.1) +
  scale_color_manual(values=colors4) +
  ylab("Conditional Ln Probability of Competitive Encounter") +
  xlab("Competitive Distance") + 
  # ylim(-6.1,1.5) +
  theme_classic() + theme(legend.position="top")


##==========================================
## ALL OBSERVATIONS Median  line:  H1 & H2 -- 4 lines
##------------------------------------------
## H1
idfh1 <- plyr::ddply(.data = idf3, .variables = c('d_cat','h1'), summarise,
                     p_l95=.qtl(p,.025),
                     p_u95=.qtl(p,.975),
                     p_mean=.avg(p), 
                     p_median=.med(p) )
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
## H2
idfh2 <- plyr::ddply(.data = idf3, .variables = c('d_cat','h2'), summarise,
                     p_l95=.qtl(p,.025),
                     p_u95=.qtl(p,.975),
                     p_mean=.avg(p), 
                     p_median=.med(p) )
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






##==========================================
## Prob X Covariate: ALL OBSERVATIONS Median line -- DISTANCE INTERACTION -- ALL
##------------------------------------------
leg.title <- "Competitive Distance"
## H1
idfh1 <- plyr::ddply(.data = idf3, .variables = c('d_cat','h1'), summarise,
                     p_l95=.qtl(p,.025),
                     p_u95=.qtl(p,.975),
                     p_mean=.avg(p), 
                     p_median=.med(p) )
idfh1$Q <- as.integer(str_sub(idfh1$h1,2,2))
dodge.width <- 0
ggplot(idfh1) + aes(x=Q, y=log(p_median), color=d_cat, pch=d_cat) +
  geom_point(lwd=4, position = position_dodge(width = dodge.width)) +
  geom_line(lwd=1.3, position = position_dodge(width = dodge.width)) +
  # geom_ribbon(aes(ymin=log(p_l95), ymax=log(p_u95), x=as.integer(d_cat)), alpha = 0.1) +
  scale_color_manual(values=colors5) +
  ylab("Conditional Ln Probability of Competitive Encounter") +
  xlab("Diversification Quartile (H1)") + 
  # ylim(-6.1,1.5) +
  theme_classic() + theme(legend.position="top") +
  guides(color=guide_legend(leg.title),pch=guide_legend(leg.title))
## H2
idfh2 <- plyr::ddply(.data = idf3, .variables = c('d_cat','h2'), summarise,
                     p_l95=.qtl(p,.025),
                     p_u95=.qtl(p,.975),
                     p_mean=.avg(p), 
                     p_median=.med(p) )
idfh2$Q <- as.integer(str_sub(idfh2$h2,2,2))
dodge.width <- 0
ggplot(idfh2) + aes(x=Q, y=log(p_median), color=d_cat, pch=d_cat) +
  geom_point(lwd=4, position = position_dodge(width = dodge.width)) +
  geom_line(lwd=1.3, position = position_dodge(width = dodge.width)) +
  # geom_ribbon(aes(ymin=log(p_l95), ymax=log(p_u95), x=as.integer(d_cat)), alpha = 0.1) +
  scale_color_manual(values=colors5) +
  ylab("Conditional Ln Probability of Competitive Encounter") +
  xlab("Competitive Asymmetry Quartile (H2)") + 
  # ylim(-6.1,1.5) +
  theme_classic() + theme(legend.position="top") + 
  guides(color=guide_legend(leg.title),pch=guide_legend(leg.title))



##==========================================
## Prob X Covariate: ALL OBSERVATIONS Median line -- DISTANCE INTERACTION -- NON-RIVALS (D > 1)
##------------------------------------------
leg.title <- "Competitive Distance (not current rivals)"
## H1
idfh1 <- plyr::ddply(.data = idf3, .variables = c('d_cat','h1'), summarise,
                     p_l95=.qtl(p,.025),
                     p_u95=.qtl(p,.975),
                     p_mean=.avg(p), 
                     p_median=.med(p) )
idfh1$Q <- as.integer(str_sub(idfh1$h1,2,2))
idfh1 <- idfh1[idfh1$d_cat != '1', ]
idfh1$d_cat <- droplevels(idfh1$d_cat)
dodge.width <- 0
ggplot(idfh1) + aes(x=Q, y=log(p_median), color=d_cat, pch=d_cat) +
  geom_point(lwd=4, position = position_dodge(width = dodge.width)) +
  geom_line(lwd=1.3, position = position_dodge(width = dodge.width)) +
  # geom_ribbon(aes(ymin=log(p_l95), ymax=log(p_u95), x=as.integer(d_cat)), alpha = 0.1) +
  scale_color_manual(values=colors5) +
  ylab("Conditional Ln Probability of Competitive Encounter") +
  xlab("Diversification Quartile (H1)") + 
  # ylim(-6.1,1.5) +
  theme_classic() + theme(legend.position="top") +
  guides(color=guide_legend(leg.title),pch=guide_legend(leg.title))
## H2
idfh2 <- plyr::ddply(.data = idf3, .variables = c('d_cat','h2'), summarise,
                     p_l95=.qtl(p,.025),
                     p_u95=.qtl(p,.975),
                     p_mean=.avg(p), 
                     p_median=.med(p) )
idfh2$Q <- as.integer(str_sub(idfh2$h2,2,2))
idfh2 <- idfh2[idfh2$d_cat != '1', ]
idfh2$d_cat <- droplevels(idfh2$d_cat)
dodge.width <- 0
ggplot(idfh2) + aes(x=Q, y=log(p_median), color=d_cat, pch=d_cat) +
  geom_point(lwd=4, position = position_dodge(width = dodge.width)) +
  geom_line(lwd=1.3, position = position_dodge(width = dodge.width)) +
  # geom_ribbon(aes(ymin=log(p_l95), ymax=log(p_u95), x=as.integer(d_cat)), alpha = 0.1) +
  scale_color_manual(values=colors5) +
  ylab("Conditional Ln Probability of Competitive Encounter") +
  xlab("Competitive Asymmetry Quartile (H2)") + 
  # ylim(-6.1,1.5) +
  theme_classic() + theme(legend.position="top") + 
  guides(color=guide_legend(leg.title),pch=guide_legend(leg.title))




##==========================================
## Prob X Covariate: ALL OBSERVATIONS Median line -- DISTANCE INTERACTION -- UNEXPECTED INDIRECT COMPETITORS (D > 2)
##------------------------------------------
leg.title <- "Competitive Distance (unexpected indirect competitors)"
## H1
idfh1 <- plyr::ddply(.data = idf3, .variables = c('d_cat','h1'), summarise,
                     p_l95=.qtl(p,.025),
                     p_u95=.qtl(p,.975),
                     p_mean=.avg(p), 
                     p_median=.med(p) )
idfh1$Q <- as.integer(str_sub(idfh1$h1,2,2))
idfh1 <- idfh1[ !idfh1$d_cat  %in% c('1','2'), ]
idfh1$d_cat <- droplevels(idfh1$d_cat)
dodge.width <- 0
ggplot(idfh1) + aes(x=Q, y=log(p_median), color=d_cat, pch=d_cat) +
  geom_point(lwd=4, position = position_dodge(width = dodge.width)) +
  geom_line(lwd=1.3, position = position_dodge(width = dodge.width)) +
  # geom_ribbon(aes(ymin=log(p_l95), ymax=log(p_u95), x=as.integer(d_cat)), alpha = 0.1) +
  scale_color_manual(values=colors5) +
  ylab("Conditional Ln Probability of Competitive Encounter") +
  xlab("Diversification Quartile (H1)") + 
  # ylim(-6.1,1.5) +
  theme_classic() + theme(legend.position="top") +
  guides(color=guide_legend(leg.title),pch=guide_legend(leg.title))
## H2
idfh2 <- plyr::ddply(.data = idf3, .variables = c('d_cat','h2'), summarise,
                     p_l95=.qtl(p,.025),
                     p_u95=.qtl(p,.975),
                     p_mean=.avg(p), 
                     p_median=.med(p) )
idfh2$Q <- as.integer(str_sub(idfh2$h2,2,2))
idfh2 <- idfh2[ !idfh2$d_cat %in% c('1','2'), ]
idfh2$d_cat <- droplevels(idfh2$d_cat)
dodge.width <- 0
ggplot(idfh2) + aes(x=Q, y=log(p_median), color=d_cat, pch=d_cat) +
  geom_point(lwd=4, position = position_dodge(width = dodge.width)) +
  geom_line(lwd=1.3, position = position_dodge(width = dodge.width)) +
  # geom_ribbon(aes(ymin=log(p_l95), ymax=log(p_u95), x=as.integer(d_cat)), alpha = 0.1) +
  scale_color_manual(values=colors5) +
  ylab("Conditional Ln Probability of Competitive Encounter") +
  xlab("Competitive Asymmetry Quartile (H2)") + 
  # ylim(-6.1,1.5) +
  theme_classic() + theme(legend.position="top") + 
  guides(color=guide_legend(leg.title),pch=guide_legend(leg.title))






##=============================================================
##
##  TEMPORAL TREND -- AWARENESS TRAJECTORY
##
##--------------------------------------------------------------

## mutate group-period factors
tdf <-
  idf[idf$d != 0 & idf$d !='0', ] %>%
  group_by(year) %>%
  mutate(
    p_log = log(p),
    p_log_z = (log(p) - mean(log(p))) / sd(log(p))
  ) %>% 
  ungroup

mcis <- plyr::ddply(.data = idf3, .variables = c('year'), summarise,
                    i=v.focal,
                    j=9999,
                    j.name='MEDIAN',
                    p_log_l95=.qtl(log(p),.025),
                    p_log_u95=.qtl(log(p),.975),
                    p_log_l75=.qtl(log(p),.125),
                    p_log_u75=.qtl(log(p),.875),
                    # p_log_mean=mean(log(p)), 
                    p_log=.med(log(p))
                    )
mcis$year <- as.integer(as.character(mcis$year))
tdf$year <- as.integer(as.character(tdf$year))
# sub.j.name <- tdf$j.name[tdf$year %in% c('2008','2012','2016') & tdf$p_log_z > 1.69 & tdf$d != 1 & tdf$d < Inf]
# grp <- 61
# j.idx <- 1:6 + 6 * grp
# print(j.idx)
## K means cluster

g <- asIgraph(nets[[t+1]])

yrtest <- '2016'
qt <- .qtl(tdf$p[tdf$year==yrtest], c(.86,.90))
mid.names <- as.character(tdf$j.name[!is.na(tdf$p) & tdf$p > qt[1] & tdf$p < qt[2] & tdf$year==yrtest])
print(mid.names)

plotNames <- c(  # 'clarabridge',# 'adobe', # 'hootsuite','sprinklr',  # 'satmetrix',#'sap','hybris',
  'palantir-technologies',
  'ibm', # 'oracle'  # 'salesforce', 'freshdesk',
  'delighted', 
  'promoter-io',
  # 'ideascale', #'askyourtargetmarket',
  'medallia',  'kampyle'  #'surveyrock'#,  'qriously'
  )
# plotNames <- mid.names
j.idx <- which(V(g)$vertex.names %in% plotNames)
sub.j.name <- unique(as.character(tdf$j.name[tdf$j %in% j.idx]))
tdf.sub <- tdf[tdf$j.name %in% sub.j.name, ]
leg.title <- "Potential / Current\nRival"
ggplot(tdf.sub) + aes(x=year,y=p_log) + 
  geom_ribbon(aes(ymin=p_log_l95, ymax=p_log_u95, x=year, linetype=NA), col='black', alpha = 0.085, data=mcis) +
  geom_ribbon(aes(ymin=p_log_l75, ymax=p_log_u75, x=year, linetype=NA), col='black', alpha = 0.11, data=mcis) +
  geom_line(aes(y=p_log, x=year), data=mcis, color='darkgray', lwd=2, lty=2) + 
  geom_point(aes(color=j.name, pch=j.name), lwd=3.5) + 
  geom_line(aes(color=j.name), lwd=1.1) + 
  ylab('Conditoinal Ln Probabilitiy of Competitive Encounter') +
  theme_classic() + theme(legend.position="top") + 
  guides(color=guide_legend(leg.title),pch=guide_legend(leg.title))



## STL DECOMPOSITION DATAFRAME
idf3.sub <- idf3[ , c('j.name','year','p','t')]
jnu <- unique(idf3.sub$j.name)
idf3.stl <- data.frame()
for (i in 1:length(jnu)) {
  x <- idf3.sub[idf3.sub$j.name==jnu[i],]
  ps <- x$p
  tmp <- data.frame(j.name=jnu[i],
                    t1=ps[1],t2=ps[2],t3=ps[3],t4=ps[4],t5=ps[5],
                    t6=ps[6],t7=ps[7],t8=ps[8],t9=ps[9],t10=ps[10])
  idf3.stl <- rbind(idf3.stl, tmp)
}
## STL
cols <- names(idf3.stl)[2:ncol(idf3.stl)]
stl(idf3$p, s.window = 'periodic', na.action='na.rm')


j.all <- unique(idf2$j)
j.na <- unique(idf2$j[is.na(idf2$p)])
j.nona <- j.all[ !j.all %in% j.na]
idf2.na.p <- idf2$p[ !idf2$j %in% j.na]
ts1 <- ts(idf2.na.p, frequency=10,  start=c(1,1), names=j.nona)
ts1.stl <- stl(ts1, s.window = 'periodic')
plot(ts1.stl)















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
