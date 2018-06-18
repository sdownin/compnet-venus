#read data files
library(dplyr)

MATCH <- function (x,y)
{
  return(which(x %in% y))
}

mydata1=read.csv(file='organizations.csv', header=T, stringsAsFactors=FALSE, na.strings = c('NA',''))
#length(unique(mydata1$com_ID)) -- # of companies in total: 433674

mydata2=read.csv(file='competitors.csv',header=T, stringsAsFactors=FALSE, na.strings = c('NA',''))
mydata3=read.csv(file='acquisitions.csv', header=T, stringsAsFactors=FALSE, na.strings = c('NA',''))
mydata4=read.csv(file='missing_companies_20180330.csv', header=T, stringsAsFactors=FALSE, na.strings = c('NA',''))

#found many duplicates - remove duplicates
competition <- mydata2[-which(duplicated(mydata2)),]

#length(unique(mydata2_unique$uuid)) --# of companies in the competition: 25339

# add more information about companies to the competition file
competition$com_name = mydata1[MATCH(competition$uuid, mydata1$uuid),"company_name"]
competition$competitor_name = mydata1[MATCH(competition$competitor_uuid, mydata1$uuid),"company_name"]

#remove rows where com_name (ex. "NA") is not found from the organizations file
competition <- competition[!is.na(competition[,5]),]
competition <- competition[!is.na(competition[,6]),]

# Add information to "competition"
competition$com_founded_on = mydata1[MATCH(competition$uuid, mydata1$uuid),]$founded_on
competition$com_closed_on = mydata1[MATCH(competition$uuid, mydata1$uuid),]$closed_on
competition$com_status = mydata1[MATCH(competition$uuid, mydata1$uuid),]$status

competition_update <- left_join(competition, mydata4, by = c("uuid", "com_name", "com_founded_on", "com_closed_on", "com_status"))
competition_update2 <- merge(competition, mydata4, by=c("uuid", "com_name", "com_founded_on", "com_closed_on", "com_status"), all.y=T)

#length(unique(mydata2_unique2$uuid)) -- # of companies in the competition: 24294

#---------------------------------------------------------------------------------
########## deal with missing founded_on file
#temp =read.csv(file='founded_on_missing.csv', header=T)
#temp1 <- temp[-which(duplicated(temp)),]
#write.csv(temp1, file = "founded_on_missing_20180324.csv")
##########

m1 <- MATCH(competition$uuid, mydata1$uuid)
m2l <- competition$uuid %in% mydata1$uuid
m2n <- which(m2l)
M <- MATCH(competition$uuid, mydata1$uuid)
assertthat::are_equal(M, m2n)
##
#      m1        m2l       m2n      
# l  "73418"   "73418"   "71787"  
# cl "integer" "logical" "integer"
##
sapply(list(m1=m1,m2l=m2l,m2n=m2n,M=M),function(x)c(l=length(x),cl=class(x)))


# Add information to "competition"
compInMydata1 <- MATCH(competition$uuid, mydata1$uuid)
competition$com_founded_on[compInMydata1] = mydata1[compInMydata1,]$founded_on
competition$com_closed_on[compInMydata1] = mydata1[compInMydata1,]$closed_on
competition$com_status[compInMydata1] = mydata1[compInMydata1,]$status


#Add missing founded_on info from missing data file
competition$com_founded_on[which(competition$com_founded_on=="")] <- NA  # | is.na(competition$com_founded_on)
sum(is.na(competition$com_founded_on))
NAs.a<-is.na(competition$com_founded_on)
competition$com_founded_on[NAs.a] = mydata4[MATCH(competition$uuid[NAs.a], mydata4$uuid),]$founded_on

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

