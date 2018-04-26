setwd('C:\\Users\\T430\\Google Drive\\PhD\\Dissertation\\competition networks\\compnet2\\founded_on_date_edit')
#read data files
library(dplyr)

##
# Return vector of indices of  x which are in y
# [vector] x
# [vector] y
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
# [dataframe] df
# [character] old.col  The old column name (to be replaced by new.col)
# [character] new.col  The new column name (to replace old.col)
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

mydata1=read.csv(file='organizations.csv', header=T, stringsAsFactors=F, na.strings = c('NA',''))
#length(unique(mydata1$com_ID)) -- # of companies in total: 433674

mydata2=read.csv(file='competitors.csv',header=T, stringsAsFactors=F, na.strings = c('NA',''))
mydata3=read.csv(file='acquisitions.csv', header=T, stringsAsFactors=F, na.strings = c('NA',''))
mydata4=read.csv(file='missing_companies_20180330.csv', header=T, stringsAsFactors=F, na.strings = c('NA',''))

#found many duplicates - remove duplicates
competition <- mydata2[-which(duplicated(mydata2)),]


######################################################
# 1. UPDATE VALUES IN ORGANIZATIONS DATAFRAME
######################################################
## MERGE IN UPDATED VALUES
tmp <- mydata4[,c("uuid", "com_name", "com_founded_on", "com_closed_on", "com_status")]  
org <- merge(x=mydata1, y=tmp, by.x='uuid', by.y='uuid', all.x=T, all.y=F)

## APPLY UPDATE OLD_COLUMN TO NEW_COLUMN  
## FASTER THAN FOR-LOOP WHEN DATAFRAME IS VERY LARGE
org$company_name <- parseNonNa(org, 'company_name','com_name')
org$founded_on <- parseNonNa(org, 'founded_on','com_founded_on')
org$closed_on <- parseNonNa(org, 'closed_on','com_closed_on')
org$status <- parseNonNa(org, 'status','com_status')

## DROP REPLACEMENT COLUMNS
org$com_name <- NULL
org$com_founded_on <- NULL
org$com_closed_on <- NULL
org$com_status <- NULL
## `org` dataframe is now the updated `mydata1` dataframe of organizations attributes

######################################################
# 2. MERGE UPDATED VALUES INTO COMPETITORS DATAFRAME
######################################################
# TEMP DF for merging in company and competitor attributes
tmp <- org[,c('uuid','company_name','founded_on','closed_on','status')]
# COMPANY NAME
names(tmp) <- c('uuid', "com_name", "com_founded_on", "com_closed_on", "com_status")
competition <- merge(x=competition, y=tmp, by.x = 'uuid', by.y='uuid', all.x = T, all.y=F)
# COMPETITOR NAME
names(tmp) <- c('uuid', "competitor_name", "competitor_founded_on", "competitor_closed_on", "competitor_status")
competition <- merge(x=competition, y=tmp, by.x = 'competitor_uuid', by.y='uuid', all.x = T, all.y=F)

#remove rows where com_name (ex. "NA") is not found from the organizations file
competition <- competition[!is.na(competition[,'com_name']),]
competition <- competition[!is.na(competition[,'competitor_name']),]

write.csv(competition, file = "competition_UPDATED_20180405.csv")

# m1 <- MATCH(competition$uuid, mydata1$uuid)
# m2l <- competition$uuid %in% mydata1$uuid
# m2n <- which(m2l)
# M <- MATCH(competition$uuid, mydata1$uuid)
# assertthat::are_equal(M, m2n)
# ##
# #      m1        m2l       m2n      
# # l  "73418"   "73418"   "71787"  
# # cl "integer" "logical" "integer"
# ##
# sapply(list(m1=m1,m2l=m2l,m2n=m2n,M=M),function(x)c(l=length(x),cl=class(x)))








































#Add missing founded_on info from missing data file
competition$com_founded_on[which(competition$com_founded_on=="")] <- NA  # | is.na(competition$com_founded_on)
sum(is.na(competition$com_founded_on))
NAs.a<-is.na(competition$com_founded_on)

competition$com_founded_on[NAs.a] = mydata4[MATCH(mydata4$uuid, competition$uuid[NAs.a]),]$founded_on



write.csv(competition, file = "compete_20180402.csv")

competition$com_closed_on[which(competition$com_closed_on=="")] <- "NA"
sum(competition$com_closed_on=="NA")
#is.na(competition$com_closed_on)
NAs1.a<-is.na(competition$com_closed_on)
competition$com_closed_on[NAs1.a] = mydata4[MATCH(competition$uuid[NAs1.a], mydata4$uuid),]$closed

competition$com_closed_on[NAs.a] = mydata3[MATCH(competition$uuid[NAs.a], mydata3$uuid),]$acquired_on
competition$com_status[NAs.a] = mydata4[MATCH(competition$uuid[NAs.a], mydata4$uuid),]$status

#Add competitors' information
competition$compet_founded_on = mydata1[MATCH(competition$competitor_uuid, mydata1$uuid),"founded_on"]
competition$compet_closed_on = mydata1[MATCH(competition$competitor_uuid, mydata1$uuid),"closed_on"]
competition$compet_status = mydata1[MATCH(competition$competitor_uuid, mydata1$uuid),"status"]

#Add competitors' missing founded_on
competition$compet_founded_on[which(competition$compet_founded_on=="")] <- "NA"
NAs1.a <-is.na(competition$compet_founded_on)
competition$compet_founded_on[NAs1.a] = mydata4[MATCH(competition$uuid[NAs1.a], mydata4$uuid),]$com_founded_on
competition$compet_closed_on[NAs1.a] = mydata4[MATCH(competition$uuid[NAs1.a], mydata4$uuid),]$closed
competition$compet_closed_on[NAs1.a] = mydata3[MATCH(competition$uuid[NAs1.a], mydata3$uuid),]$acquired_on
competition$compet_status[NAs1.a] = mydata4[MATCH(competition$uuid[NAs1.a], mydata4$uuid),]$status

#print a certain company to check if the data is joined properly or not
competition[which(competition$com_name=="ReputationManagement.net"),]
competition[which(competition$com_name=="Rif Mobile"),]

#--------------------------2018.3.31
#Add missing competitors' closed_on info from missing data file

#length(unique(mydata2_unique3$uuid)) -- # of unique companies: 24059

#create csv file for the current file to fill out missing information
write.csv(mydata2_unique3, file = "compete_20180324.csv")

mydata2_unique$com_country_code = mydata1[MATCH(mydata2_unique$uuid, mydata1$uuid),"country_code"]
mydata2_unique$com_state_code = mydata1[MATCH(mydata2_unique$uuid, mydata1$uuid),"state_code"]
mydata2_unique$com_status = mydata1[MATCH(mydata2_unique$uuid, mydata1$uuid),"status"]
mydata2_unique$com_category_list = mydata1[MATCH(mydata2_unique$uuid, mydata1$uuid),"category_list"]
mydata2_unique$com_category_group_list = mydata1[MATCH(mydata2_unique$uuid, mydata1$uuid),"category_group_list"]

mydata2_unique$com_closed_on = mydata1[MATCH(mydata2_unique$uuid, mydata1$uuid),"closed_on"]
mydata2_unique$com_employee_count = mydata1[MATCH(mydata2_unique$uuid, mydata1$uuid),"employee_count"]
mydata2_unique$com_created_at = mydata1[MATCH(mydata2_unique$uuid, mydata1$uuid),"created_at"]

#add more information about acquisition to the competition file
mydata2_unique$acquirer_uuid = mydata3[MATCH(mydata2_unique$uuid, mydata3$acquiree_uuid),"acquirer_uuid"]
mydata2_unique$acquirer_name = mydata3[MATCH(mydata2_unique$uuid, mydata3$acquiree_uuid),"acquirer_name"]
mydata2_unique$acquirer_country_code = mydata3[MATCH(mydata2_unique$uuid, mydata3$acquiree_uuid),"acquirer_country_code"]
mydata2_unique$acquirer_state_code = mydata3[MATCH(mydata2_unique$uuid, mydata3$acquiree_uuid),"acquirer_state_code"]
mydata2_unique$acquired_on = mydata3[MATCH(mydata2_unique$uuid, mydata3$acquiree_uuid),"acquired_on"]
mydata2_unique$price_usd = mydata3[MATCH(mydata2_unique$uuid, mydata3$acquiree_uuid),"price_usd"]

