##########################################################################################
#
# AMJ 2018 SPECIAL ISSUE COMPETITION NETWORKS AND ACQUISITIONS ACTIVITY ANALYSIS
#
# @author   Stephen Downing <sdowning.bm02g@nctu.edu.tw>
# @date     May 2016
#
#
##########################################################################################
setwd("C:/Users/T430/Google Drive/PhD/Dissertation/competition networks/compnet2")
#.libPaths('C:/Users/sdowning/Documents/R/win-library/3.2')
#
library(plyr, quietly = T)
library(magrittr, quietly = T)
library(reshape2, quietly = T)
library(lubridate, quietly = T)
library(stringr, quietly = T)
data_dir <- "C:/Users/T430/Google Drive/PhD/Dissertation/crunchbase/crunchbase_export_20161024"

##
# Return vector of indices of  x which are in y
# @param [vector] x
# @param [vector] y
# @return [vector] length is length(intersect(x,y))
##
MATCH <- function (x,y)
{
  return(which(x %in% y))
}

##
# Returns vector of values from dataframe df 
#  by checking for non-NA values each row
#  in columns (old.col, new.col)
#  - if neither is non-NA, return default value provided (defaults to NA)
#  - if 1 is non-NA, return the non-NA value
#  - if both are non-NA, return the value of column `new.col`
# @param [dataframe] df
# @param [character] old.col  The old column name (to be replaced by new.col)
# @param [character] new.col  The new column name (to replace old.col)
# @return  [vector]
##
parseNonNa <- function(df, old.col, new.col, default=NA)
{
  return(unname(
    apply(df[ , c(old.col, new.col)], MARGIN = 1, FUN = function(x){
      idx <- which(!is.na(x))
      switch(as.character(length(idx)),
             '0'=default,## `default` if neither is non-NA
             '1'=x[idx], ## the non-NA value if only 1 is non-NA
             '2'=x[2]    ## the replacement column if both are non-NA
      )
    })
  ))
}

##
# Checks if the argument `x` is a falsy (null, <nan>, <na>, 'NA', or empty string)
# @param [any] x  The element to check
# @return [bool]  
##
isEmpty <- function(x)
{
  if (is.null(x)) 
    return(TRUE)
  return (is.nan(x) | is.na(x) | x=='NA' | x=='')
}

##
# Returns vector of competitive relation beginning date strings (or NA) by the following logic:
#  - take the latest founding date when both have valid founding dates
#  - else NA
# @param [dataframe] df  The dataframe to apply the relationBeganOn logic to
# @return [character[]]  The vector of beginning date strings
##
relationBeganOn <- function(df)
{
  cols <- c('company_founded_on','competitor_founded_on')
  for(col in cols){
    if (!(col %in% names(df))) stop(sprintf('dataframe missing column `%s`.',col))
  }
  return(unname(apply(X = df[,cols],MARGIN = 1,FUN = function(x){
    max(x)
  })))
}

##
# Returns vector of competitive relation ending date strings (or NA) by the following logic:
#  - take the earliest ending date (closed, acquired) if any (company,competitor) are not NA
#  - else NA
# @param [dataframe] df  The dataframe to apply the relationEndedOn logic to
# @return [character[]]  The vector of ending date strings
##
relationEndedOn <- function(df)
{
  cols <- c('company_closed_on','company_acquired_on','competitor_closed_on','competitor_acquired_on')
  for(col in cols){
    if (!(col %in% names(df))) stop(sprintf('dataframe missing column `%s`.',col))
  }
  return(unname(apply(X = df[,cols],MARGIN = 1,FUN = function(x){
    ifelse(any(!isEmpty(x)), min(x[!isEmpty(x)]), NA) 
  })))
}

##
# Loads and returns a dataframe from a CSV file
# @param [character] filename         The file name (absolute path or relative to current working directory)
# @param [boolean] header             A flag to include header row
# @param [character] quote            The quotation character
# @param [character] na.strings       The character strings to represent <NA> values
# @param [character] stringsAsFactors A flag to convert character strings to factor type
# @param [character] fill             A flag to fill missing columns per row
# @return [dataframe] filename
##
readCsv <- function(filename, header=T, quote='"', na.strings=c('NA',''), stringsAsFactors=F, fill=T)
{
  return(read.csv(file = filename, header=header, quote=quote, na.strings = na.strings, 
                  stringsAsFactors = stringsAsFactors, fill=fill))
}



##=================================
##  Sources
##----------------------------------
csv.co          <- 'organizations.csv'
csv.co_comp     <- 'competitors.csv'
csv.co_cust     <- 'customers.csv'
csv.co_parent   <- 'org_parents.csv'
csv.co_prod     <- 'products.csv'
csv.co_acq      <- 'acquisitions.csv'
csv.co_br       <- 'branches.csv'
csv.co_rou      <- 'funding_rounds.csv'  ## company--funding_round
csv.co_ipo      <- 'ipos.csv'
csv.fund        <- 'funds.csv'
csv.inv         <- 'investors.csv'
csv.inv_rou     <- 'investments.csv'  ## investor--funding_round
csv.inv_part    <- 'investment_partners.csv'
# csv.ev        <- 'events.csv'
# csv.ev_rel    <- 'event_relationships.csv'
# csv.categ     <- 'category_groups.csv'
# csv.jobs      <- 'jobs.csv'
# csv.ppl       <- 'people.csv'
# csv.ppl_desc  <- 'people_descriptions.csv'

##=================================
##  Data import
##----------------------------------
cat('\nloading dataframes...')

co        <- readCsv(file.path(data_dir, csv.co))
co_comp   <- readCsv(file.path(data_dir, csv.co_comp)) 
co_cust   <- readCsv(file.path(data_dir, csv.co_cust)) 
co_parent <- readCsv(file.path(data_dir, csv.co_parent))
co_prod   <- readCsv(file.path(data_dir, csv.co_prod)) 
co_acq    <- readCsv(file.path(data_dir, csv.co_acq)) 
co_br     <- readCsv(file.path(data_dir, csv.co_br)) 
co_rou    <- readCsv(file.path(data_dir, csv.co_rou)) 
co_ipo    <- readCsv(file.path(data_dir, csv.co_ipo)) 
fund      <- readCsv(file.path(data_dir, csv.fund)) 
inv       <- readCsv(file.path(data_dir, csv.inv)) 
inv_rou   <- readCsv(file.path(data_dir, csv.inv_rou)) 
inv_part  <- readCsv(file.path(data_dir, csv.inv_part))  
# ev        <- readCsv(file.path(data_dir, csv.ev))  
# ev_rel    <- readCsv(file.path(data_dir, csv.ev_rel))  
# categ     <- readCsv(file.path(data_dir, csv.categ))  
# job       <- readCsv(file.path(data_dir, csv.jobs))  
# ppl       <- readCsv(file.path(data_dir, csv.ppl))  
# ppl_desc  <- readCsv(file.path(data_dir, csv.ppl_desc))  


cat('done.\n')
cat('cleaning data...')


##==========================
## COMPANIES 
##--------------------------
## update founded_on,closed_on dates  - Jin-Su's Email 2018-04-23
co.date <- read.csv('founded_on_date_edit/missing_companies_20180330.csv', header = T, na.strings = c('','NA'), stringsAsFactors = F)
names(co.date) <- c('com_uuid','com_name','com_founded_on_UPDATE','com_closed_on_UPDATE','com_status','note')
## fix date formatting  '12/13/2011' ==> '2011-12-31'
co.date$com_founded_on_UPDATE <- sapply(co.date$com_founded_on_UPDATE, function(x){
  return(ifelse(isEmpty(x), NA,  as.character(mdy(x))))
})
co.date$com_closed_on_UPDATE <- sapply(co.date$com_closed_on_UPDATE, function(x){
  return(ifelse(isEmpty(x), NA,  as.character(mdy(x))))
})

## merge udpatese into company df
co <- merge(x=co,y=co.date,by.x='company_uuid',by.y='com_uuid',all.x=T,all.y=F)
## APPLY UPDATE OLD_COLUMN TO NEW_COLUMN  
## FASTER THAN FOR-LOOP WHEN DATAFRAME IS VERY LARGE
co$company_name <- parseNonNa(co, 'company_name','com_name')
co$status_update <- parseNonNa(co, 'status','com_status')
co$founded_on <- parseNonNa(co, 'founded_on','com_founded_on_UPDATE')
co$closed_on <- parseNonNa(co, 'closed_on','com_closed_on_UPDATE')
## DROP REPLACEMENT COLUMNS
co$com_name <- NULL
co$com_status <- NULL
co$com_founded_on_UPDATE <- NULL
co$com_closed_on_UPDATE <- NULL
co$note <- NULL

# ##  DROP COMPANY row with mising name
co <- co[which(co$company_name_unique!="" & !is.na(co$company_name_unique)), ]

## ONLY COMPANIES (no universities, etc)
co <- co[which(co$primary_role == 'company'), ]

## DROP DUPLICATES
co.cnt <- plyr::count(co$company_name_unique)
co.cnt <- co.cnt[order(co.cnt$freq, decreasing = T), ]
# co.cnt[co.cnt$freq > 1, ]
co.dups <- as.character(co.cnt$x[which(co.cnt$freq > 1)])
if(length(co.dups) > 0) {
  co <- co[which( !(co$company_name_unique %in% co.dups) ), ]
}

## ADD YEAR variables
co$founded_year <- as.numeric(stringr::str_sub( co$founded_on,1,4))
co$closed_year <-  as.numeric(stringr::str_sub( co$closed_on,1,4))
co$acquired_year <-  as.numeric(stringr::str_sub( co$acquired_on,1,4))


##==========================
## ACQUISITIONS
##--------------------------
## make unique acquired on dates for each company
##  - as min of acquired_on dates when multiple (acquired multiple times) 
cat('reshaping acquisitions dataframe:')
co_acq_acquired <- ddply(co_acq, 'acquiree_name_unique', .progress='text', summarize,
                         acquiree_uuid=paste(unique(acquiree_uuid),collapse="|"),
                         acquired_on=min(acquired_on),
                         acquired_on_concat=paste(acquired_on,collapse="|"))
co_acq_acquired <- co_acq_acquired[which(!isEmpty(co_acq_acquired$acquiree_name_unique) & !isEmpty(co_acq_acquired$acquired_on)),]
##
co_acq$company_name_unique <- co_acq$acquirer_name_unique
co_acq$acquired_year <- as.numeric(stringr::str_sub(co_acq$acquired_on,1,4))


##==========================
## COMPETITIVE RELATIONS
##--------------------------
## add dates to compute relation_began_on, relation_ended_on
## remove acquired_on|closed_on|founed_on dates from python pre-processing script
## to set them here:
drop.cols <- c('company_closed_on','competitor_closed_on','company_founded_on','competitor_founded_on',
               'relation_ended_on','max_founded_on','relation_began_on',
               'acquiree_name_unique_concat','acquired_on_concat','acquired_on')
for (col in drop.cols){
  if (col %in% names(co_comp)) {
    co_comp[,col] <- NULL
  }
}
## rename entity_uuid to company_uuid
names(co_comp)[which(names(co_comp)=='entity_uuid')] <- 'company_uuid'
##  - COMPANY FOUNDED | CLOSED
tmp <- co[,c('company_uuid','founded_on','closed_on')]
names(tmp) <- c('company_uuid','company_founded_on','company_closed_on')
co_comp <- merge(x=co_comp,y=tmp, by.x='company_uuid',by.y='company_uuid', all.x=T,all.y=F)
##  - COMPANY ACQUIRED  ## using co_acq_acquired for unique acquiree dates
tmp <- co_acq_acquired[,c('acquiree_uuid','acquired_on')]
names(tmp) <- c('acquiree_uuid','company_acquired_on')
co_comp <- merge(x=co_comp,y=tmp, by.x='company_uuid',by.y='acquiree_uuid', all.x=T,all.y=F)
##  - COMPETITOR FOUNDED | CLOSED
tmp <- co[,c('company_uuid','founded_on','closed_on')]
names(tmp) <- c('competitor_uuid','competitor_founded_on','competitor_closed_on')
co_comp <- merge(x=co_comp,y=tmp, by.x='company_uuid',by.y='competitor_uuid', all.x=T,all.y=F)
##  - COMPETITOR ACQUIRED  ## using co_acq_acquired for unique acquiree dates
tmp <- co_acq_acquired[,c('acquiree_uuid','acquired_on')]
names(tmp) <- c('acquiree_uuid','competitor_acquired_on')
co_comp <- merge(x=co_comp,y=tmp, by.x='company_uuid',by.y='acquiree_uuid', all.x=T,all.y=F)

##
## keep only intersection of competitive relations by the following conditions:
##
## 1. company or competitor names is missing
idx.name.exists <- which(co_comp$company_name_unique != '' & !is.na(co_comp$company_name_unique)
                         & co_comp$competitor_name_unique != '' & !is.na(co_comp$competitor_name_unique) )
## 2. only relations where both parties are in the company table 
idx.name.in.co <- which(co_comp$company_uuid %in% co$company_uuid
                        | co_comp$competitor_uuid %in% co$company_uuid)
## 3. only relations where founded_on date is not missing or "no exist"
idx.date.exists <- which( !isEmpty(co_comp$company_founded_on) 
                          & co_comp$company_founded_on != "no exist"
                          & !isEmpty(co_comp$competitor_founded_on) 
                          & co_comp$competitor_founded_on != "no exist" )
idx.all <- intersect( intersect(idx.name.exists,idx.name.in.co), idx.date.exists )
co_comp <- co_comp[idx.all, ]
## remove previous relation_began_on|relation_ended_on
co_comp$relation_began_year <- NULL ##  as.numeric(stringr::str_sub( co_comp$relation_began_on,1,4))
co_comp$relation_ended_year <- NULL ##  as.numeric(stringr::str_sub( co_comp$relation_ended_on,1,4))

## set relation_began_on|relation_ended_on by new rules use new founed_on dates


co_comp$relation_began_on <- relationBeganOn(co_comp)
co_comp$relation_ended_on <- relationEndedOn(co_comp)



##==========================
## MULTI_MARKET_CONTACT CODE
##--------------------------
co_br$mmc_code <- apply(co_br[,c('country_code3','region_code2')],1,function(x){
  paste(x, collapse="_")
})


##==========================
## FUNDING ROUND
##--------------------------
co_rou$funded_year <- as.numeric(stringr::str_sub(co_rou$announced_on,1,4))


##==========================
## BRANCHES
##--------------------------
co_br$created_year <- as.numeric(stringr::str_sub(co_br$created_at,1,4))


##==========================
## IPO
##--------------------------
co_ipo$went_public_year <- as.numeric(stringr::str_sub(co_ipo$went_public_on,1,4))


##==========================
## CUSTOMER
##--------------------------
co_cust$created_year <- as.numeric(stringr::str_sub(co_cust$created_at,1,4))


##==========================
## Unique Branches
##--------------------------
co_br <- unique(co_br)



cat('done.\n')














# ##-----------------------------------------
# ## Add founded_on dates manually checked
# ##-----------------------------------------
# acqs.data.fix <- read.csv("acquisitions_companies_ibm_d2_814_FIXED.csv", header = T, na.strings = c('NA'), stringsAsFactors = F)
# awar.data.fix <- read.csv("awareness_companies_qualtrics_d3_583_FIXED.csv", header = T, na.strings = c('NA'), stringsAsFactors = F)
# 
# isMissing <- function(x){
#   return(is.na(x) | is.nan(x)  | x == '' | x == 'NA')
# }
# 
# idx <- which( 
#   isMissing(co$founded_on)
#   & (co$company_name_unique %in% acqs.data.fix$company_name_unique 
#      | co$company_name_unique %in% awar.data.fix$company_name_unique)
# )
# 
# for(i in idx) {
#   acq.idx.i <- which(acqs.data.fix$company_name_unique == co$company_name_unique[i])
#   awr.idx.i <- which(awar.data.fix$company_name_unique == co$company_name_unique[i])
#   acq.date <- ifelse(length(acq.idx.i) > 0, acqs.data.fix$founded_on[acq.idx.i], NA)
#   awr.date <- ifelse(length(awr.idx.i) > 0, awar.data.fix$founded_on[awr.idx.i], NA)
#   # print(cat(sprintf('%s %s %s\n',i,acq.date,awr.date)))
#   co$founded_on[i] <- ifelse( !isMissing(acq.date), acq.date, 
#                               ifelse(!isMissing(awr.date), awr.date, 
#                                      NA  ))
#   # cat(sprintf('%s\n',co$founded_on[i]))
# }
# ##-----

# ## Firm age
# co$founded_year

# ## Add Acquried date to company df
# co.tmp <- merge(data.frame(company_name_unique=co$company_name_unique, stringsAsFactors = F), 
#                 co_acq[,c('acquiree_name_unique','acquired_on')], 
#                 by.x = 'company_name_unique', by.y = 'acquiree_name_unique', all.x = T, all.y=F)
# co.mer <- plyr::ddply(co.tmp, .(company_name_unique), summarise, 
#                       acquired_on_concat=paste(acquired_on,collapse="|"))







