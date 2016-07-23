##########################################################################################
#
# COMPETITION NETWORKS AND ACQUISITIONS ACTIVITY ANALYSIS
#
# @author   Stephen Downing <sdowning.bm02g@nctu.edu.tw>
# @date     May 2016
#
#
##########################################################################################
setwd("C:/Users/sdowning/Google Drive/PhD/Dissertation/competition networks/acquisitions")
.libPaths('C:/Users/sdowning/Documents/R/win-library/3.2')
#
library(plyr)
# library(dplyr)
library(magrittr)
library(texreg)
library(coefplot2)
# library(devtools)
# library(rcrunchbase)
library(reshape2)
library(ggplot2)
# library(xlsx)
library(igraph)
# library(sna)
# library(network)
library(stringr)
library(MASS)
library(memisc)
# library(pscl)
# library(AER)
library(psych)
library(glmmADMB)
data_dir <- "C:/Users/sdowning/Google Drive/PhD/Dissertation/crunchbase"
#
source(file.path(getwd(),'comp_net_functions.R'))
#
load('acquisitions_data_analysis.RData')
# save.image('acquisitions_data_analysis.RData')


##--------------------------------------------------------
## LOAD DATA; CLEAN DATA
##---------------------------------------------------------
csv.companies <- 'cb_export_with_competitors_20160106_companies.csv'
csv.acquisitions <- 'cb_export_with_competitors_20160106_acquisitions.csv'
csv.competitors <- 'cb_export_with_competitors_20160106_competitors.csv'

co <- read.table(file.path(data_dir, csv.companies), sep=",",header=T, quote='"', stringsAsFactors = F, fill=T)
acq <- read.table(file.path(data_dir,csv.acquisitions), sep=",",header=T, quote='"', stringsAsFactors = F, fill=T)
comp <- read.table(file.path(data_dir,csv.competitors), sep=",",header=T, quote='"', stringsAsFactors = F, fill=T)


## convert funding value strings to numbers
co$funding_total_usd <- as.numeric(gsub('[-]','0',gsub('[, ]','',co$funding_total_usd)))

## assign name company_name_unique same as other data.frame
names(acq)[which(names(acq)=='name')] <- 'company_name_unique'

## convert timestamp (epoch) to date
comp$relation_updated_at_date <- timestamp2date(as.numeric(comp$relation_updated_at))

## convert factors
co$status <- as.factor(co$status)
co$country_code <- as.factor(co$country_code)
co$state_code <- as.factor(co$state_code)
co$region <- as.factor(co$region)

##-----------------------------------------------------------
## COMPUTE REGRESSION VARIABLES
##------------------------------------------------------------
co$age <- 2016 - co$founded_year


# ##--------------------------------------------------------
# ## SUBSET COMPANIES (regression df) THAT HAVE AT LEAST ONE COMPETITIVE TIE
# ## WILL NEED TO SELECT ONLY LARGEST CONNECTED COMPONENT
# ##--------------------------------------------------------
# names.u <- unique(c(comp$company_name_unique,comp$competitor_name_unique))
# co.c <- co[which(co$company_name_unique%in%names.u), ]


# ##-------------------------------------------------------
# ##  ADD ACQUIRED DATES TO REMOVE COMPANIES FROM COMPETITION NETWORK
##-------------------------------------------------------
tmp <- acq[,c('acquired_name_unique','acquired_at','company_name_unique')]
names(tmp) <- c('company_name_unique','acquired_at','acquirer_company_name_unique')
# head(co[, c('company_name_unique')])
co.acq.dup <- merge(co,tmp, by='company_name_unique', all.x=T,all.y=F)
## examine duplicates
co.acq.dup[duplicated(co.acq.dup$company_name_unique),]
## remove duplicates
co.acq <- co.acq.dup[!duplicated(co.acq.dup$company_name_unique),]

## check REMOVED DUPLICATES
cnt <- count(co.acq$company_name_unique)
cnt <- cnt[order(cnt$freq, decreasing = T), ]
dups <- cnt$x[cnt$freq>1]
length(dups) == 0




##------------------------------------
##
## NEED CLOSED_ON DATES HERE...
##
##------------------------------------
##-------------------------------------------------------
##  ADD ClOSED DATES TO REMOVE COMPANIES FROM COMPETITION NETWORK
##-------------------------------------------------------

#co$closed_at  <- '' ###







