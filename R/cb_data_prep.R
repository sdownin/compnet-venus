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
#.libPaths('C:/Users/sdowning/Documents/R/win-library/3.2')
#
library(plyr, quietly = T)
library(magrittr, quietly = T)
library(reshape2, quietly = T)
library(lubridate, quietly = T)
library(stringr, quietly = T)
data_dir <- "C:/Users/sdowning/Google Drive/PhD/Dissertation/crunchbase/crunchbase_export_20161024"

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
## Sources
##---------------------------------------------------------
##---- entities
# csv.jobs <- 'jobs.csv'
# csv.ppl <- 'people.csv'
# csv.ppl_desc <- 'people_descriptions.csv'
csv.co <- 'organizations.csv'
csv.co_comp <- 'competitors.csv'
csv.co_cust <- 'customers.csv'
csv.co_parent <- 'org_parents.csv'
csv.co_prod <- 'products.csv'
csv.co_acq <- 'acquisitions.csv'
csv.co_br <- 'branches.csv'
csv.co_rou <- 'funding_rounds.csv'  ## company--funding_round
csv.co_ipo <- 'ipos.csv'
csv.fund <- 'funds.csv'
csv.inv <- 'investors.csv'
csv.inv_rou <- 'investments.csv'  ## investor--funding_round
csv.inv_part <- 'investment_partners.csv'
csv.ev <- 'events.csv'
csv.ev_rel <- 'event_relationships.csv'
csv.categ <- 'category_groups.csv'
#----------------------------------
#  Data import
#----------------------------------
cat('\nloading dataframes...\n')
co <- read.table(file.path(data_dir, csv.co), sep=",",header=T, quote='"' , stringsAsFactors = F, fill=T)
co_comp <- read.table(file.path(data_dir, csv.co_comp), sep=",",header=T, quote='"' , stringsAsFactors = F, fill=T)
co_cust <- read.table(file.path(data_dir, csv.co_cust), sep=",",header=T, quote='"' , stringsAsFactors = F, fill=T)
co_parent <- read.table(file.path(data_dir, csv.co_parent), sep=",",header=T, quote='"' , stringsAsFactors = F, fill=T)
co_prod <- read.table(file.path(data_dir, csv.co_prod), sep=",",header=T, quote='"' , stringsAsFactors = F, fill=T)
co_acq <- read.table(file.path(data_dir, csv.co_acq), sep=",",header=T, quote='"' , stringsAsFactors = F, fill=T)
co_br <- read.table(file.path(data_dir, csv.co_br), sep=",",header=T, quote='"' , stringsAsFactors = F, fill=T)
co_rou <- read.table(file.path(data_dir, csv.co_rou), sep=",",header=T, quote='"' , stringsAsFactors = F, fill=T)
co_ipo <- read.table(file.path(data_dir, csv.co_ipo), sep=",",header=T, quote='"' , stringsAsFactors = F, fill=T)
fund <- read.table(file.path(data_dir, csv.fund), sep=",",header=T, quote='"' , stringsAsFactors = F, fill=T)
inv <- read.table(file.path(data_dir, csv.inv), sep=",",header=T, quote='"' , stringsAsFactors = F, fill=T)
inv_rou <- read.table(file.path(data_dir, csv.inv_rou), sep=",",header=T, quote='"' , stringsAsFactors = F, fill=T)
inv_part <- read.table(file.path(data_dir, csv.inv_part), sep=",",header=T, quote='"' , stringsAsFactors = F, fill=T)
ev <- read.table(file.path(data_dir, csv.ev), sep=",",header=T, quote='"' , stringsAsFactors = F, fill=T)
ev_rel <- read.table(file.path(data_dir, csv.ev_rel), sep=",",header=T, quote='"' , stringsAsFactors = F, fill=T)
categ <- read.table(file.path(data_dir, csv.categ), sep=",",header=T, quote='"' , stringsAsFactors = F, fill=T)

# MULTI_MARKET_CONTACT CODE
co_br$mmc_code <- apply(co_br[,c('country_code3','region_code2')],1,function(x){
  paste(x, collapse="_")
})

# ##  DROP COMPANY row with mising name
co <- co[which(co$company_name_unique!="" & !is.na(co$company_name_unique)), ]

# ## ADD ACQUISITION DATE to COMPETITOR RELATION
# cat('\nadding acquisition date to competitor relation...\n')
# co.company <- co[,c('company_uuid','company_name_unique')]
# co.competitor <- data.frame(company_uuid=co$company_uuid, competitor_name_unique=co$company_name_unique,
#                             stringsAsFactors = F)
# co_comp <- merge(co_comp, co.company, by.x='entity_uuid', by.y='company_uuid', all.x=F, all.y=F )
# co_comp <- merge(co_comp, co.competitor, by.x='competitor_uuid', by.y='company_uuid', all.x=T, all.y=F)
# 
# co_comp_tmp <- merge(co_comp[,c('entity_uuid','competitor_uuid')], co_acq[,c('acquiree_uuid','acquired_on')], 
#                      by.x='entity_uuid', by.y='acquiree_uuid', all.x=T, all.y=F)
# co_comp_tmp <- merge(co_comp_tmp, co_acq[,c('acquiree_uuid','acquired_on')], 
#                      by.x='competitor_uuid', by.y='acquiree_uuid', all.x=T, all.y=F)
# # co_comp_tmp_sub <- subset(co_comp_tmp, subset=(competitor_uuid %in% co_comp$competitor_uuid
# #                                                |entity_uuid %in% co_comp$entity_uuid))
# ### check dups
# df.dup <- data.frame(x=apply(co_comp_tmp,1, function(x)paste(x,collapse="_")))
# df.dup$x <- as.character(df.dup$x)
# cnt <- plyr::count(df.dup$x)
# cnt <- cnt[order(cnt$freq, decreasing=T),]
# ###
# dups <- co_comp_tmp[duplicated(x = co_comp_tmp[,c('competitor_uuid','entity_uuid')]), ]
# co_comp$acquired_on <- apply(co_comp_tmp[,grep('acquired_on',x = names(co_comp_tmp),ignore.case = T)], 
#                              MARGIN = 1, FUN = function(x)min(x))
##
co_comp_acquired_on <- sapply(1:nrow(co_comp), function(i){
  index <- which(co_comp$entity_uuid[i] == co_acq$acquiree_uuid
                 | co_comp$competitor_uuid[i] == co_acq$acquiree_uuid)
  if(i%%1000==0) cat(sprintf('\ncompleted row %s',i))
  if (nrow(co_acq[index,]) > 0 ) {
    return(min(co_acq[index,'acquired_on']))
  }else{
    return(NA)
  }
})
co_comp$acquired_on <- co_comp_acquired_on

# tmp <- data.frame(company_name_unique=co_acq$acquired_name_unique, acquired_at=co_acq$acquired_at)
# co_comp2 <- merge(co_comp, tmp, by='company_name_unique', all.x=T, all.y=F)
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

## add branch created_year
br$created_year <- year(br$created_at)

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




