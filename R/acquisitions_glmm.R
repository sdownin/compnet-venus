#############################################################################################
#
#  Competition Networks and Acquisition Activity
#
#  GLMM Regression
#
#############################################################################################
setwd("C:/Users/sdowning/Google Drive/PhD/Dissertation/competition networks/compnet")

library(lme4)

# source(file.path(getwd(),'R','comp_net_functions.R'))
# source(file.path(getwd(),'R','cb_data_prep.R'))
# source(file.path(getwd(),'R','acquisitions_data.R'))

data_dir <- "C:/Users/sdowning/Google Drive/PhD/Dissertation/crunchbase/"
img_dir  <- "C:/Users/sdowning/Google Drive/PhD/Dissertation/competition networks/acquisitions/img"
par.default <- par()
lattice::trellis.par.set(strip.background=list(col="lightgrey"))

df.sub <- na.omit(df)
df.sub <- df.sub[which(df.sub$dist_d < Inf),]

form1 <- Y ~ dist_d + age_d + acq_exp + (1|firm_i) + (1|firm_j)
fit1 <- glmer(formula = form1, data = df.sub, family = binomial(link='logit'), verbose = T)

form2 <- Y ~ dist_d + I(dist_d^2) + age_d + acq_exp + (1|firm_i) + (1|firm_j)
fit2 <- glmer(formula = form2, data = df.sub, family = binomial(link='logit'), verbose = T)

form3 <- Y ~ dist_d + ipo_d  + acq_exp + mmc_d + age_d + 
  degree_d + constraint_d + region_d + category_d + 
  (1|firm_i) + (1|firm_j) + (1|period)
fit3 <- glmer(formula = form3, data = df.sub, family = binomial(link='logit'), verbose = T)


