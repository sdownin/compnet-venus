##########################################################################################
#
# AMJ 2018 SPECIAL ISSUE SIC Codes matching to public firms from CrunchBase
#
# Notes: 
#  - must load CrunchBase companies dataframe ("co") first -- use script cb_data_prep.R
#
#
##########################################################################################
setwd('C:\\Users\\T430\\Google Drive\\PhD\\Dissertation\\competition networks\\compnet2')
library(stringr)
library(stringi)
library(stringdist)


if(!('co' %in% ls())) 
  stop('must load `cb_data_prep.R` script first.')


cat('\nloading SIC data...')

## SIC CODE DESCRIPTIONS
sic <- read.csv('SIC/sic_4digit_classcodes_com.csv', stringsAsFactors = F, na.strings = c('','NA')) ## SIC_codes_description.csv
sic$description <- str_to_lower(sic$description)

## COMPUSTAT PUBLIC COMPANY SIC CODES
cs.all <- read.csv('SIC/compustat_public_sic.csv', stringsAsFactors = F, na.strings = c('','NA'))
## company name and SIC code sting separated by "|" example: "1040|1010"
cat('reshaping compustat firm SIC codes dataframe:')
cs <- ddply(cs.all, 'conml', .progress = 'text', summarize,
             gvkey =paste(unique(gvkey), collapse="|"),
             sic=paste(unique(sic), collapse="|") )


## find all public companies in CrunchBase that are public at any time (in co_ipo records)
pub.uuid <- unique(c(co$company_uuid[co$status=='ipo'], co_ipo$company_uuid))
co.pub <- co[which(co$company_uuid %in% pub.uuid), ]

##replace corporation ending terms 
names.pattern <- "\\s(ltd|co|co ltd|inc|corp|limited|incorporated|incorporation|corporation)$"
co.pub.names <- data.frame(name=co.pub$company_name, uuid=co.pub$company_uuid,
                           short=str_replace_all(str_to_lower(co.pub$company_name), names.pattern,""),
                           stringsAsFactors = F)
cs.names <- data.frame(name=cs$conml, gvkey=cs$gvkey,
                          short=str_replace_all(str_to_lower(cs$conml), names.pattern,""),
                          stringsAsFactors = F)

## match Compustat companies to CrunbhBase Companies
namemap <- data.frame()
for(i in seq_len(nrow(co.pub.names))) {
  if (i %% 50 == 0) {
    saveRDS(namemap, file = 'compustat_crunchbase_co_name_mapping_df.rds')
    cat(sprintf('i = %s\n',i))
  }
  x <- co.pub.names$short[i]
  edit <- stringdist(x, cs.names$short, method = 'osa') ## edit distance
  maxlen <- sapply(str_length(cs.names$short), function(j)max(j,str_length(x))) ## max string len
  dr <- edit/maxlen
  idx <- which.min(dr)
  tmpdf <- data.frame(co.pub.short=co.pub.names$short[i], 
                    co.pub.name=co.pub.names$name[i], 
                    co.pub.uuid=co.pub.names$uuid[i], 
                    cs.short=cs.names$short[idx], 
                    cs.name=cs.names$name[idx], 
                    cs.gvkey=cs.names$gvkey[idx], 
                    d=dr[idx], stringsAsFactors = F)
  namemap <- rbind(namemap, tmpdf)
};  saveRDS(namemap, file = 'compustat_crunchbase_co_name_mapping_df.rds')



cat('done.\n')
cat('cleaning data...')


# for(i in seq_along(pub.names)) {
#   x <- pub.names[i]
#   break
#   if (length(x)==1 && x=='NA')
#     lsic[[i]] <- data.frame(sic_code=NA, d=NA, code_len=NA, x=paste(x,collapse = "|"),xi=NA,sic_description=NA, stringsAsFactors = F)
#   mdf <- data.frame()
#   for (xi in x) {
#     edit <- stringdist(xi, sic$description, method = 'lcs') ## edit distance
#     maxlen <- sapply(siclen, function(j)max(j,str_length(xi))) ## max string len
#     df <- data.frame(sic_code=sic$code5, d=edit/maxlen, code_len=sic$code_len, x=paste(x,collapse = "|"),xi=xi,sic_description=sic$description, stringsAsFactors = F)
#     df <- df[df$code_len==max(df$code_len), ]
#     mdf <- rbind(mdf, df[which.min(df$d), ])
#   }
#   lsic[[i]] <- mdf[which.min(mdf$d),]
# }