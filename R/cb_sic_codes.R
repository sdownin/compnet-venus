##########################################################################################
#
# SIC Codes matching to public firms from CrunchBase
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
siclen <- str_length(sic$description)

## COMPUSTAT PUBLIC COMPANY SIC CODES
cs <- read.csv('SIC/compustat_public_sic.csv', stringsAsFactors = F, na.strings = c('','NA'))


lsic <- list()
for(i in seq_along(cat.split)) {
  x <- cat.split[[i]]
  if (i %% 100 == 0)  {
    cat(sprintf("i = %s\n",i))    
    saveRDS(list(lsic=lsic), file = "amj_sic_codes_list_parsed_lcs.rds")
  }
  if (length(x)==1 && x=='NA')
    lsic[[i]] <- data.frame(sic_code=NA, d=NA, code_len=NA, x=paste(x,collapse = "|"),xi=NA,sic_description=NA, stringsAsFactors = F)
  mdf <- data.frame()
  for (xi in x) {
    edit <- stringdist(xi, sic$description, method = 'lcs') ## edit distance
    maxlen <- sapply(siclen, function(j)max(j,str_length(xi))) ## max string len
    df <- data.frame(sic_code=sic$code5, d=edit/maxlen, code_len=sic$code_len, x=paste(x,collapse = "|"),xi=xi,sic_description=sic$description, stringsAsFactors = F)
    df <- df[df$code_len==max(df$code_len), ]
    mdf <- rbind(mdf, df[which.min(df$d), ])
  }
  lsic[[i]] <- mdf[which.min(mdf$d),]
}
saveRDS(list(lsic=lsic), file = "sic_codes_list_parsed_lcs.rds")



cat('done.\n')
cat('cleaning data...')