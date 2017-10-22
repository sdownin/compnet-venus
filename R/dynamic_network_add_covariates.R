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

###########################################################################

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

# # ## cache original
# firm.nets.orig <- firm.nets
# 
# ## set market group of firms
# net_group <- 'cem'
# if( !(net_group %in% names(firm.nets)) ) firm.nets[[net_group]] <- list()
# 
# ## set firms to create networks
# firms.todo <- names(firm.nets$cem)

## run main network period creation loop
files <- dir(path = 'firm_nets_cem')

for (i in 1:length(files)) {
  nets <- readRDS(paste0('firm_nets_cem/',files[i]))
  name_i <- stringr::str_split(files[i], ".rds")[[1]][1]
  ## -- settings --
  yrpd <- 1
  endYr <- 2017
  startYr <- endYr - length(nets)
  ## --------------
  cat(sprintf('\n---------%s----------\n',name_i))
  periods <- seq(startYr,endYr,yrpd)
  company.name <- 'company_name_unique'
  verbose <- TRUE
  #----------------Network List-------------------
  for (t in 2:length(nets)) {
    cat(sprintf('\nsetting covariates for period %s-%s:\n', periods[t-1],periods[t]))
    nets[[t]] <- setCovariates( nets[[t]], 
         periods[t-1], periods[t],
         c('dist', 'similarity', 'centrality','generalist', 'constraint'),
         netRiskCommunityAlgo='multilevel.community',
         downweight.env.risk=FALSE #,
         # acq=co_acq,br=co_br,rou=co_rou,ipo=co_ipo
    )
  }
  ## ---------- add LAGS ----------------
  for (t in 2:length(nets)) {
    net <- nets[[t]]
    netlag <- nets[[t-1]]
    net %n% 'DV_lag' <- netlag[,]
    net %n% 'dist_lag' <- netlag %n% 'dist'
    # net %v% 'net_risk_lag' <- netlag %v% 'net_risk'
    net %v% 'cent_eig_lag' <- netlag %v% 'cent_deg'
    net %v% 'cent_deg_lag' <- netlag %v% 'cent_deg'
    net %v% 'cent_pow_n1_5_lag' <- netlag %v% 'cent_pow_n1_5'
    net %v% 'cent_pow_n2_0_lag' <- netlag %v% 'cent_pow_n2_0'
    net %v% 'cent_pow_n3_0_lag' <- netlag %v% 'cent_pow_n3_0'
    net %v% 'genidx_multilevel_lag' <- netlag %v% 'genidx_multilevel'
    nets[[t]] <- net
  }

  ## CAREFUL TO OVERWRITE
  file.name <- sprintf('firm_nets_cem/%s', files[i])
  saveRDS(nets, file=file.name)
  
}


# names <- names(firm.nets$cem)
# for (i in 1:length(names)) {
#   ## CAREFUL TO OVERWRITE
#   name <- names[i]
#   net <- firm.nets$cem[[name]]
#   file.name <- sprintf('firm_nets_cem/%s.rds',name)
#   saveRDS(net, file=file.name)
#   rm(net)
#   firm.nets$cem[[name]] <- NULL
# }


