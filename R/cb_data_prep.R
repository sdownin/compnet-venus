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

##--------------------------------------------------------
## Sources
##---------------------------------------------------------
##---- entities
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
csv.jobs <- 'jobs.csv'
csv.ppl <- 'people.csv'
csv.ppl_desc <- 'people_descriptions.csv'
#----------------------------------
#  Data import
#----------------------------------
cat('\nloading dataframes...\n')
co <- read.table(file.path(data_dir, csv.co), sep=",",header=T, quote='"' , na.strings = 'NA', stringsAsFactors = F, fill=T)
co_comp <- read.table(file.path(data_dir, csv.co_comp), sep=",",header=T, quote='"' , na.strings = 'NA', stringsAsFactors = F, fill=T)
co_cust <- read.table(file.path(data_dir, csv.co_cust), sep=",",header=T, quote='"' , na.strings = 'NA', stringsAsFactors = F, fill=T)
co_parent <- read.table(file.path(data_dir, csv.co_parent), sep=",",header=T, quote='"' , na.strings = 'NA', stringsAsFactors = F, fill=T)
co_prod <- read.table(file.path(data_dir, csv.co_prod), sep=",",header=T, quote='"' , na.strings = 'NA', stringsAsFactors = F, fill=T)
co_acq <- read.table(file.path(data_dir, csv.co_acq), sep=",",header=T, quote='"' , na.strings = 'NA', stringsAsFactors = F, fill=T)
co_br <- read.table(file.path(data_dir, csv.co_br), sep=",",header=T, quote='"' , na.strings = 'NA', stringsAsFactors = F, fill=T)
co_rou <- read.table(file.path(data_dir, csv.co_rou), sep=",",header=T, quote='"' , na.strings = 'NA', stringsAsFactors = F, fill=T)
co_ipo <- read.table(file.path(data_dir, csv.co_ipo), sep=",",header=T, quote='"' , na.strings = 'NA', stringsAsFactors = F, fill=T)
fund <- read.table(file.path(data_dir, csv.fund), sep=",",header=T, quote='"' , na.strings = 'NA', stringsAsFactors = F, fill=T)
inv <- read.table(file.path(data_dir, csv.inv), sep=",",header=T, quote='"' , na.strings = 'NA', stringsAsFactors = F, fill=T)
inv_rou <- read.table(file.path(data_dir, csv.inv_rou), sep=",",header=T, quote='"' , na.strings = 'NA', stringsAsFactors = F, fill=T)
inv_part <- read.table(file.path(data_dir, csv.inv_part), sep=",",header=T, quote='"' , na.strings = 'NA', stringsAsFactors = F, fill=T)
ev <- read.table(file.path(data_dir, csv.ev), sep=",",header=T, quote='"' , na.strings = 'NA', stringsAsFactors = F, fill=T)
ev_rel <- read.table(file.path(data_dir, csv.ev_rel), sep=",",header=T, quote='"' , na.strings = 'NA', stringsAsFactors = F, fill=T)
categ <- read.table(file.path(data_dir, csv.categ), sep=",",header=T, quote='"' , na.strings = 'NA', stringsAsFactors = F, fill=T)
##
job <- read.table(file.path(data_dir, csv.jobs), sep=",",header=T, quote='"' , na.strings = 'NA', stringsAsFactors = F, fill=T)
ppl <- read.table(file.path(data_dir, csv.ppl), sep=",",header=T, quote='"' , na.strings = 'NA', stringsAsFactors = F, fill=T)
ppl_desc <- read.table(file.path(data_dir, csv.ppl_desc), sep=",",header=T, quote='"' , na.strings = 'NA', stringsAsFactors = F, fill=T)


# MULTI_MARKET_CONTACT CODE
co_br$mmc_code <- apply(co_br[,c('country_code3','region_code2')],1,function(x){
  paste(x, collapse="_")
})

# ##  DROP COMPANY row with mising name
co <- co[which(co$company_name_unique!="" & !is.na(co$company_name_unique)), ]
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
co_comp$relation_began_year <-  as.numeric(stringr::str_sub( co_comp$relation_began_on,1,4))
co_comp$relation_ended_year <-  as.numeric(stringr::str_sub( co_comp$relation_ended_on,1,4))


# FUNDING ROUND
co_rou$funded_year <- as.numeric(stringr::str_sub(co_rou$announced_on,1,4))

# ACQUISITIONS ACQUIRER NAME
co_acq$company_name_unique <- co_acq$acquirer_name_unique
co_acq$acquired_year <- as.numeric(stringr::str_sub(co_acq$acquired_on,1,4))

# BRANCHES
co_br$created_year <- as.numeric(stringr::str_sub(co_br$created_at,1,4))

#IPO
co_ipo$went_public_year <- as.numeric(stringr::str_sub(co_ipo$went_public_on,1,4))

# CUSTOMER
co_cust$created_year <- as.numeric(stringr::str_sub(co_cust$created_at,1,4))

## Unique Branches
co_br <- unique(co_br)

# ## Firm age
# co$founded_year

# ## Add Acquried date to company df
# co.tmp <- merge(data.frame(company_name_unique=co$company_name_unique, stringsAsFactors = F), 
#                 co_acq[,c('acquiree_name_unique','acquired_on')], 
#                 by.x = 'company_name_unique', by.y = 'acquiree_name_unique', all.x = T, all.y=F)
# co.mer <- plyr::ddply(co.tmp, .(company_name_unique), summarise, 
#                       acquired_on_concat=paste(acquired_on,collapse="|"))







