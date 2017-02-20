#############################################################################################
#
#  Competition Networks and Acquisition Activity
#
#  GLMM Regression
#
#############################################################################################
setwd("C:/Users/sdowning/Google Drive/PhD/Dissertation/competition networks/compnet")

library(lme4)
library(texreg)

# source(file.path(getwd(),'R','comp_net_functions.R'))
# source(file.path(getwd(),'R','cb_data_prep.R'))
# source(file.path(getwd(),'R','acquisitions_data.R'))

data_dir <- "C:/Users/sdowning/Google Drive/PhD/Dissertation/crunchbase/"
img_dir  <- "C:/Users/sdowning/Google Drive/PhD/Dissertation/competition networks/acquisitions/img"
par.default <- par()
lattice::trellis.par.set(strip.background=list(col="lightgrey"))
#---------------------------------------------------------------------------------------------------

# load(file="acquisitions_l_with_mmc_13-15-17.RData")
# 
# ldf <- lapply(l, FUN=function(x)x$df)
# df.panel <- list2paneldf(list(l$`2013`$df, l$`2015`$df, l$`2017`$df))
# 
# df.sub <- na.omit(df.panel)
# df.sub <- df.sub[which(df.sub$dist_d < Inf),]
# dim(df.sub)

# write.table(df.sub, 'acquisitions_l_df_with_mmc_15-15-17.csv', sep=",", na = 'NA', row.names = F, col.names = T)

df.sub <- read.table('acquisitions_l_df_with_mmc_15-15-17.csv', header = T, sep = ',',na.strings = c('NA'), stringsAsFactors = F)

##  transform skewed covariates 
df.sub$age_d_log <-  log(df.sub$age_d + .001)
df.sub$acq_exp_d_log <- log(df.sub$acq_exp_d + .001)
df.sub$degree_d_log <- log(df.sub$degree_d + .001)
df.sub$closeness_d_log <- log(df.sub$closeness_d + .001)

## propotional sample:  all y=1; mult*x as many y=0
mult <- 1
y1_idx <- which(df.sub$Y == 1)
y0_idx <- which(df.sub$Y == 0)
set.seed(1111)
y0_idx.samp <- sample(y0_idx,size = mult*length(y1_idx), replace = F)
df.sub.samp <- df.sub[c(y1_idx,y0_idx.samp), ]

# ## simple random sampling
# y.samp <- sample(seq_len(nrow(df.sub)),size = 2*sum(df.sub$Y), replace = F)
# df.sub.samp <- df.sub[y.samp, ]

#------------------------------------------------------------------------------------------
#                                      ESTIMATION 
#------------------------------------------------------------------------------------------
glmctrl <- glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 20000))  # 'bobyqa'
#------------------------------------------------------------------------------------------

# form1 <- Y ~ dist_d + age_d + acq_exp_d + (1|firm_i/firm_j) + (1|period)
# fit1 <- glmer(formula = form1, data = df.sub.samp, family = binomial(link='logit'), verbose = T, control = glmctrl)
# 
# form2 <- Y ~ dist_d + I(dist_d^2) + age_d + acq_exp_d + (1|firm_i/firm_j) + (1|period)
# fit2 <- glmer(formula = form2, data = df.sub.samp, family = binomial(link='logit'), verbose = T, control = glmctrl)
# 
# form3 <- Y ~ dist_d + community_d + ipo_d  + acq_exp_d_log + age_d_log + closeness_d_log +
#   degree_d_log + constraint_d + region_d + category_d + 
#   (1|firm_i/firm_j) + (1|period)
# fit3 <- glmer(formula = form3, data = df.sub.samp, family = binomial(link='logit'), verbose = T, control = glmctrl)


form1 <- Y ~  
  degree_d_log + region_d + category_d + ipo_d + age_d_log + community_d + closeness_d_log + mmc_d +
  (1|firm_i) + (1|firm_j) + (1|period)
fit1 <- glmer(formula = form1, data = df.sub.samp, family = binomial(link='logit'), verbose = T, control = glmctrl)

form2 <- Y ~ dist_d  +
  degree_d_log + region_d + category_d + ipo_d + age_d_log + community_d + closeness_d_log + mmc_d +
  (1|firm_i) + (1|firm_j) + (1|period)
fit2 <- glmer(formula = form2, data = df.sub.samp, family = binomial(link='logit'), verbose = T, control = glmctrl)

form3 <- Y ~ acq_exp_d_log +  dist_d  + dist_d:acq_exp_d_log +
  degree_d_log + region_d + category_d + ipo_d + age_d_log + community_d + closeness_d_log + mmc_d +
  (1|firm_i) + (1|firm_j) + (1|period)
fit3 <- glmer(formula = form3, data = df.sub.samp, family = binomial(link='logit'), verbose = T, control = glmctrl)

form4 <- Y ~  constraint_d + 
  degree_d_log + region_d + category_d + ipo_d + age_d_log + community_d + closeness_d_log + mmc_d +
  (1|firm_i) + (1|firm_j) + (1|period)
fit4 <- glmer(formula = form4, data = df.sub.samp, family = binomial(link='logit'), verbose = T, control = glmctrl)

# form5 <- Y ~ dist_d + I(dist_d^2)  +
#   degree_d_log + region_d + category_d + ipo_d + age_d_log + community_d + closeness_d_log + mmc_d +
#   (1|firm_i) + (1|firm_j) + (1|period)
# fit5 <- glmer(formula = form5, data = df.sub.samp, family = binomial(link='logit'), verbose = T, control = glmctrl)

# form6 <- Y ~ dist_d + I(dist_d^2) + acq_exp_d_log + constraint_d + 
#   degree_d_log + region_d + category_d + ipo_d + age_d_log + community_d + closeness_d_log + mmc_d +
#   (1|firm_i) + (1|firm_j) + (1|period)
# fit6 <- glmer(formula = form6, data = df.sub.samp, family = binomial(link='logit'), verbose = T, control = glmctrl)

form7 <- Y ~ acq_exp_d_log + dist_d +  dist_d:acq_exp_d_log + constraint_d + 
  degree_d_log + region_d + category_d + ipo_d + age_d_log + community_d + closeness_d_log + mmc_d +
  (1|firm_i) + (1|firm_j) + (1|period)
fit7 <- glmer(formula = form7, data = df.sub.samp, family = binomial(link='logit'), verbose = T, control = glmctrl)

# form8 <- Y ~ dist_d + I(dist_d^2) + acq_exp_d_log + dist_d:acq_exp_d_log + constraint_d +
#   degree_d_log + region_d + category_d + ipo_d + age_d_log + community_d + closeness_d_log + mmc_d +
#   (1|firm_i) + (1|firm_j) + (1|period)
# fit8 <- glmer(formula = form8, data = df.sub.samp, family = binomial(link='logit'), verbose = T, control = glmctrl)


fitlist <- list(fit2,fit3,fit4,fit7)
save(fitlist, file = 'acquisitions_fitlist_mmc_12347.RData')

screenreg(fitlist, digits = 3)
write.regtable(fitlist, html=T, filename='acquisitions_fitlist_mmc_12347', digits=3, single.row = F)



fitlist2 <- list(fit3,fit4,fit5,fit6,fit7,fit8)
screenreg(fitlist2, digits = 3)





##-------------------------------------------------------------
# interactions
#--------------------------------------------------------------
df.sub.samp$acq_exp_d_fac <- factor(ifelse(df.sub.samp$acq_exp_d > median(df.sub.samp$acq_exp_d), 'High','Low'))
df.sub.samp$dist_d_fac <- factor(ifelse(df.sub.samp$dist_d > quantile(df.sub.samp$dist_d,probs = 2/3), 'High',
                                        ifelse(df.sub.samp$dist_d > quantile(df.sub.samp$dist_d,probs = 1/3), 'Mid',
                                               'Low'
                                               )
                                        ), levels=c('Low','Mid','High'), ordered=T)
df.sub.samp$dist_d_fac <- factor(ifelse(df.sub.samp$dist_d > quantile(df.sub.samp$dist_d,probs = 1/2), 'High','Low'))

tmp <- df.sub.samp[which(df.sub.samp$period=='2011-2012'), ]
interaction.plot(tmp$dist_d,tmp$acq_exp_d_fac, tmp$Y,
                 col=c('darkred','steelblue'),lty=1:2,pch=16:17,
                 ylab='Mean Response',xlab='Competitive Distance',
                 type='b',trace.label = 'Acquisition\nExperience')



bwplot(mean(Y) ~ dist_d | period, data=df.sub.samp)


