##########################################################################################
#
# COMPETITION NETWORKS AND ACQUISITIONS ACTIVITY ANALYSIS
#
# @author   Stephen Downing <sdowning.bm02g@nctu.edu.tw>
# @date     May 2016
#
#
##########################################################################################
setwd("C:/Users/sdowning/Google Drive/PhD/Dissertation/competition networks/compnet")
.libPaths('C:/Users/sdowning/Documents/R/win-library/3.2')
#
library(plyr)
# library(dplyr)
library(magrittr)
library(texreg)
library(reshape2)
library(lubridate)
library(stringr)
data_dir <- "C:/Users/sdowning/Google Drive/PhD/Dissertation/crunchbase"
#
#source(file.path(getwd(),'R','comp_net_functions.R'))
#
#load(file.path(getwd(),'R','acquisitions_data_analysis.RData'))
# save.image('acquisitions_data_analysis.RData')

##--------- Conversion functions---------------

##
# convert 12/31/2013 --> 2013-12-31 (only if input contains "/")
##
convertMdySlashToYmdDash <- function(df, cols.to.fix)
{
  for (i in 1:length(cols.to.fix)) {
    col <- cols.to.fix[i] 
    if ( col %in% names(df)) {
      test.val <- na.omit(df[which(df[,col]!=""),col])[1]
      if( grepl("[/]", test.val, ignore.case = T,perl = T) ) {
        df[,col] <- as.character( lubridate::mdy(df[, col]) )
      }      
    }
  }
  return(df)
}
##_--------------------------------------------

##--------------------------------------------------------
## LOAD DATA; CLEAN DATA
##---------------------------------------------------------
csv.companies <- 'cb_export_with_competitors_20160106_companies.csv'
csv.acquisitions <- 'cb_export_with_competitors_20160106_acquisitions.csv'
csv.competitors <- 'cb_export_with_competitors_20160106_competitors.csv'
csv.funding <- 'cb_export_with_competitors_20160106_rounds.csv'
csv.branches <- 'cb_export_with_competitors_20160725_branches.csv'

co <- read.table(file.path(data_dir, csv.companies), sep=",",header=T, quote='"', stringsAsFactors = F, fill=T)
acq <- read.table(file.path(data_dir,csv.acquisitions), sep=",",header=T, quote='"', stringsAsFactors = F, fill=T)
comp <- read.table(file.path(data_dir,csv.competitors), sep=",",header=T, quote='"', stringsAsFactors = F, fill=T)
rou <- read.table(file.path(data_dir,csv.funding), sep=",",header=T, quote='"', stringsAsFactors = F, fill=T)
br <- read.table(file.path(data_dir,csv.branches), sep=",",header=T, quote='"', stringsAsFactors = F, fill=T)


# xlsx.FINAL <- 'cb_export_with_competitors_20160725_FINAL.xlsx'
# 
# co   <- read_excel(file.path(data_dir,xlsx.FINAL), sheet = "companies")
# acq  <- read_excel(file.path(data_dir,xlsx.FINAL), sheet = "acquisitions")
# comp <- read_excel(file.path(data_dir,xlsx.FINAL), sheet = "competitors")
# rou  <- read_excel(file.path(data_dir,xlsx.FINAL), sheet = "rounds")
# br   <- read_excel(file.path(data_dir,xlsx.FINAL), sheet = "branches")


# ##  DROP COMPANY row with mising name
co <- co[which(co$company_name_unique!="" & !is.na(co$company_name_unique)), ]


# reformat competitor relation dates
cols.to.fix <- c('relation_created_at','competitor_founded_on','competitor_closed_on','closed_on')
comp.bak <- comp
comp <- convertMdySlashToYmdDash(comp, cols.to.fix)


## reformat company datse
cols.to.fix <- c('founded_at','first_funding_at','last_funding_at')
co.bak <- co
co <- convertMdySlashToYmdDash(co, cols.to.fix)

## add acquisition date to competitor relation
tmp <- data.frame(company_name_unique=acq$acquired_name_unique, acquired_at=acq$acquired_at)
comp <- merge(comp, tmp, by='company_name_unique', all.x=T, all.y=F)
# tmp <- data.frame(company_name_unique=acq$company_name_unique, acquired_at=acq$acquired_at)
# comp <- merge(comp, tmp, by='company_name_unique',all.x=T, all.y=F)
# comp$acquired_at <- ifelse( !is.na(comp$acquired_at.x), comp$acquired_at.x, comp$acquired_at.y)

## remove specific competitors
comp <- comp[which(comp[,'competitor_name_unique']!='4info' & comp[,'company_name_unique']!='4info'), ]

## remove duplicate office|branche
br.r <- unique(br)  # with relation {office,headquarters}
br <- unique(br[, which( !(names(br) %in% 'relation') )])
## add market variable
br$market <- br$country_code3
br$market2 <- apply(X = br[,c('country_code3','region_code2')],
                   MARGIN = 1,
                   FUN =  function(x) paste(x, collapse='_'))

## add market to company df
co$market2 <- apply(X = co[,c('country_code','state_code')],
                    MARGIN = 1,
                    FUN =  function(x) paste(x, collapse='_'))

## add branch created_year
br$created_year <- year(br$created_at)

## drop unnecessary columns
co <- co[, !(names(co) %in% c('permalink','company_name','homepage_url'))]
acq <- acq[, !(names(acq) %in% c('acquired_permalink','acquired_name','company_permalink','company_name'))]
comp <- comp[, !(names(comp)%in%c('relation','competitor_api_path','competitor_homepage_url','competitor_company_name',
                                  'competitor_web_path','competitor_profile_image_url'))]
rou <- rou[, !(names(rou) %in% c('company_permalink','company_name','company_category_list',
                                 'company_country_code','company_state_code','company_region',
                                 'company_city','funding_round_permalink','X'))]
br <- br[, !(names(br) %in% c('country_web_path','region_web_path','city_web_path'))]

## combine street addresses
#br$street <- apply(X = br[,c('street_1','street_2')],MARGIN = 1, FUN = function(x)paste(x,collapse=', '))

## only keep relevant funding round columns
rou.cols <- c('company_name_unique','funding_round_type','funding_round_code','funded_at','funded_month',
              'funded_quarter','funded_year','raised_amount_usd')
rou <- rou[,rou.cols]

## convert funding value strings to numbers
co$funding_total_usd <- as.numeric(gsub('[-]','0',gsub('[, ]','',co$funding_total_usd)))
rou$raised_amount_usd <- as.numeric(gsub('[-]','0',gsub('[, ]','',rou$raised_amount_usd)))

## replace NAs in funding total with 0
co$funding_total_usd[is.na(co$funding_total_usd)] <- 0

## assign name company_name_unique same as other data.frame
names(acq)[which(names(acq)=='name')] <- 'company_name_unique'

## convert timestamp (epoch) to date
comp$relation_updated_at <- timestamp2date(as.numeric(comp$relation_updated_at))
br$created_at <- timestamp2date(as.numeric(br$created_at))
br$updated_at <- timestamp2date(as.numeric(br$updated_at))

## convert factors
co$status <- as.factor(co$status)
co$country_code <- as.factor(co$country_code)
co$state_code <- as.factor(co$state_code)
co$region <- as.factor(co$region)

##-----------------------------------------------------------
## COMPUTE REGRESSION VARIABLES
##------------------------------------------------------------
# co$age <- 2016 - co$founded_year  #move this computation to the period loop


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
co <- merge(co,tmp, by='company_name_unique', all.x=T,all.y=F)
## examine duplicates
#co.acq.dup[duplicated(co.acq.dup$company_name_unique),]
## remove duplicates
co <- co[!duplicated(co$company_name_unique),]

## check REMOVED DUPLICATES
cnt <- plyr::count(co$company_name_unique)
cnt <- cnt[order(cnt$freq, decreasing = T), ]
dups <- cnt$x[cnt$freq>1]
assertthat::are_equal(length(dups),0)
  
# ##-------------------------------------------------------
# ##  ADD CLOSED ON DATES TO REMOVE COMPANIES FROM COMPETITION NETWORK
##-------------------------------------------------------
tmp.closed <- data.frame(company_name_unique=comp$competitor_name_unique,company_closed_on=comp$competitor_closed_on)
co <- merge(co, tmp.closed, by = "company_name_unique", all.x=T, all.y=F)
co <- co[!duplicated(co$company_name_unique),]





# ## COMPARE competitors with closed|acquired dates of the competitor
# ## ****WRONG**** CANT DO THIS WAY BECAUSE THE ACQUISITION DATE RELATED TO ACQUISITION RELATION; NOT COMPETITON RELATION
# x <- merge(comp[,c('company_name_unique','competitor_name_unique','competitor_closed_on')], 
#            co[,c('company_name_unique','status')],
#            by='company_name_unique')
# x <- merge(x, 
#            acq[,c('company_name_unique','acquired_at')], 
#            by='company_name_unique')




