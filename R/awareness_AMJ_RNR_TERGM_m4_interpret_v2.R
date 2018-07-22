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

source(file.path(getwd(),'R','amj_awareness_functions.R'))

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

##====================================================
## Hostility Profile (awareness set) Scatter Facet plot
##----------------------------------------------------
## set years
yrs <- c('2007','2016')
##
idf3 <- idf2[idf2$year %in% yrs, ]
idf3 <- idf3[ idf3$d != Inf | idf3$year != '2016', ]
set.seed(269951)
dodge.width <- 0
##
##
ggplot(idf3) + aes(x = d_cat, y = log(p), color=aware.all, pch=aware.all) +
  geom_point(position=position_jitterdodge(dodge.width=dodge.width), lwd=2.5,
             data=function(x)dplyr::filter_(x, ~ !low.otl)) +
  scale_color_manual(values=colors2rw) +
  scale_shape_manual(values=c(1,16)) +
  facet_wrap(~ year) +
  ylab("Conditional Ln Probability of Competitive Encounter") +
  xlab("Degree of Separation From Focal Firm") + 
  ylim(-10.5,1.5) +
  geom_hline(yintercept = log(aware.all.cutoff), lty=3) +
  theme_classic() +  theme(legend.position='none')
fig3.scatter.name <- sprintf('interpret_%s_Fig3_scatter_cut_classic_%.4f_y%s-%s.png', name_i, aware.all.cutoff, yrs[1],yrs[2])
ggsave(filename = fig3.scatter.name)




##-----------------------------------------------------------
##
##     AERIAL MAPS
##
##-----------------------------------------------------------
# quantile.cutoff <- .8
# cutoff <- quantile(idf$p[idf$year==2016], quantile.cutoff, na.rm = T)
for (year in 2007:2016) {
  gLatest <- asIgraph(nets[[ which(names(nets)==as.character(year+1)) ]])
  #
  idf.sub <- idf[idf$year==as.character(year), ]
  idf.sub[is.na(idf.sub$p), 'p'] <- 0
  #
  ## COMPUTE CUTOFF FRACTION FROM aware.all.cutoff SPECIFIED ABOVE
  quantile.cutoff <- length(idf.sub$p[idf.sub$p >= aware.all.cutoff] ) / length(idf.sub$p)
  #
  aerial.map.name <- sprintf('interpret_%s_Fig3_aerial_map_cut%.4f_quant%s_y%s.png', name_i, aware.all.cutoff, round(100*quantile.cutoff), year)
  png(aerial.map.name, height=8.5, width=8.5, units='in', res = 200)
  par(mar=c(.1,.1,.1,.1), mfrow=c(1,1))
  V(gLatest)$prob <- idf.sub$p
  aaf$plotCompNetColPredict(gs = gLatest,
                            focal.firm = name_i,
                            cutoff=cutoff,
                            probAttrName='prob',
                            layout.algo = layout.fruchterman.reingold,
                            seed=1111)
  dev.off()
}
###--------------












##=============================================================
##
##  TEMPORAL TREND -- AWARENESS TRAJECTORY
##
##--------------------------------------------------------------

idf4 <- idf2
#
idf4 <- idf4[ idf4$d != Inf | idf4$year != '2016', ]


## mutate group-period factors
tdf <-
  idf[idf$d != 0 & idf$d !='0', ] %>%
  group_by(year) %>%
  mutate(
    p_log = log(p),
    p_log_z = (log(p) - mean(log(p))) / sd(log(p))
  ) %>% 
  ungroup

mcis <- plyr::ddply(.data = idf4, .variables = c('year'), summarise,
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
  # 'palantir-technologies',
  # 'ibm', # 'oracle'  # 'salesforce', 'freshdesk',
  'uservoice',
  'delighted', 
  'promoter-io',
  # 'ideascale', #'askyourtargetmarket',
  'medallia',  'kampyle',  #'surveyrock'#,  'qriously',
  'corporate-executive-board-ceb','tableau-software','cvent','comscore'
)
# plotNames <- mid.names
j.idx <- which(V(g)$vertex.names %in% plotNames)
sub.j.name <- unique(as.character(tdf$j.name[tdf$j %in% j.idx]))
tdf.sub <- tdf[tdf$j.name %in% sub.j.name, ]
leg.title <- "Potential / Current\nRival"
ggplot(tdf.sub) + aes(x=year,y=p_log) + 
  # geom_ribbon(aes(ymin=p_log_l95, ymax=p_log_u95, x=year, linetype=NA), col='black', alpha = 0.085, data=mcis) +
  # geom_ribbon(aes(ymin=p_log_l75, ymax=p_log_u75, x=year, linetype=NA), col='black', alpha = 0.11, data=mcis) +
  geom_line(aes(y=p_log, x=year), data=mcis, color='darkgray', lwd=2, lty=2) + 
  geom_point(aes(color=j.name, pch=j.name), lwd=3.5) + 
  geom_line(aes(color=j.name), lwd=1.1) + 
  ylab('Conditoinal Ln Probabilitiy of Competitive Encounter') +
  theme_classic() + theme(legend.position="top") + 
  guides(color=guide_legend(leg.title),pch=guide_legend(leg.title))



