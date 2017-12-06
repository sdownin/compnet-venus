#############################################################################################
#
#  Competition Networks and Acquisition Activity
#
#  GLMM Regression
#
#############################################################################################
setwd("C:/Users/T430/Google Drive/PhD/Dissertation/competition networks/compnet2")

library(texreg)
library(relevent)
library(informR)

source(file.path(getwd(),'R','comp_net_functions.R'))
source(file.path(getwd(),'R','cb_data_prep.R'))
source(file.path(getwd(),'R','acquisitions_data.R'))

data_dir <- "C:/Users/sdowning/Google Drive/PhD/Dissertation/crunchbase/"
img_dir  <- "C:/Users/sdowning/Google Drive/PhD/Dissertation/competition networks/acquisitions/img"

par.default <- par()
lattice::trellis.par.set(strip.background=list(col="lightgrey"))
#---------------------------------------------------------------------------

## EXAMPLE
data("atus80ord", package = "informR")
data("atus80int", package = "informR")
atus80ord[1:5, ]
##
atus80ord[which(is.na(atus80ord[, "Activities"])), "Activities"] <- "MISSING"
rawevents <- cbind(atus80ord$Activities, atus80ord$TUCASEID)
evls <- gen.evl(rawevents, null.events = "MISSING")
names(evls)
alpha.ints <- gen.intercepts(evls, basecat = "Sleeping")
alpha.ints[[1]][[1]][1, , ]

alpha.fit <- rem(eventlist = evls$eventlist, statslist = alpha.ints,
                 estimator = "BPM", 
                 prior.param = list(mu = 0, sigma = 100 , nu = 4))
summary(alpha.fit)



x = seq(-20,20,.01)
y = -1*x^3 + -20*x^2 -1
y2 = -95*x^3 + 53*x^2 -19
matplot(x,1/(1+exp(-cbind(y,y2))), type='l')

