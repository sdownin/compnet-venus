##########################################################################################
#
# AMJ 2018 SPECIAL ISSUE SDC COOPERATIVE RELATIONS DATA
#
# Notes: 
#  - must load 
#
#
##########################################################################################
library(stringr)
library(stringi)
library(stringdist)

## LOAD AND PREP CRUNCHBASE DATA IF NOT IN MEMORY
if(!('cb' %in% ls())) 
  source(file.path(getwd(),'R','amj_cb_data_prep.R'))

## CACHE ENVIRONMENT to keep when clearing tmp objects added here
.ls <- ls()

## DIRECTORIES
cb$data_dir <- "C:/Users/T430/Google Drive/PhD/Dissertation/crunchbase/crunchbase_export_20161024"
cb$work_dir <- "C:/Users/T430/Google Drive/PhD/Dissertation/competition networks/compnet2"
setwd(cb$work_dir)

## CrunchBase-Compustat name mapping
namemap.filename <- 'amj_cb_cs_name_map.csv'
cs.filename <- 'amj_cs_names.csv'

if(!('namemap' %in% names(cb)))  {
  if (namemap.filename %in% dir()) {
    cb$namemap <- cb$readCsv(namemap.filename)
    cs <- cb$readCsv(cs.filename)
    .ls <- c(.ls,'cs')
  } else {
    source(file.path(getwd(),'R','amj_cb_cs_name_map.R')) ## which loads amj_cb_cs_name_map.csv if exists, else creates it in amj_cb_cs_name_map.R
  }
  source(file.path(getwd(),'R','amj_cb_sic_codes.R'))
}


cat('\nloading SDC cooperative relations data...')

##===============================
## Functions
##-------------------------------





##===============================
## SDC cooperative relations 
##-------------------------------

sdc <- cb$readCsv('SDC_data/awareness_583_software_2008-2018_SIC_report_cusip_7-SPLIT1527140852.csv')
# cusip.cols <- names(sdc)[grep('cusip',names(sdc))]

## COMPUSTAT CUSIP CODE SUBSTRING 6 to MATHCH
cb$co$company_cusip_6 <- substr(cb$co$company_cusip, 1, 6)


## use only the crunchbase companies that have a matched cusip
tmp.co <- cb$co[!is.na(cb$co$company_cusip_6),c('company_uuid','company_cusip_6')]

names(tmp.co)[1] <- c('company_uuid_from_participant_parent_cusip')
# tmp.sdc <- sdc[,c('participant_parent_cusip','id')]
sdc <- merge(x=sdc, y=tmp.co, by.x='participant_parent_cusip', by.y='company_cusip_6', all.x=T, all.y=F)

names(tmp.co)[1] <- c('company_uuid_from_parti_cusip')
# tmp.sdc <- sdc[,c('parti_cusip','id')]
sdc <- merge(x=sdc, y=tmp.co, by.x='parti_cusip', by.y='company_cusip_6', all.x=T, all.y=F)

names(tmp.co)[1] <- c('company_uuid_from_ultimate_parent_cusip')
# tmp.sdc <- sdc[,c('ultimate_parent_cusip','id')]
sdc <- merge(x=sdc, y=tmp.co, by.x='ultimate_parent_cusip', by.y='company_cusip_6', all.x=T, all.y=F)

names(tmp.co)[1] <- c('company_uuid_from_participant_ultimate_parent_cusip')
# tmp.sdc <- sdc[,c('participant_ultimate_parent_cusip','id')]
sdc <- merge(x=sdc, y=tmp.co, by.x='participant_ultimate_parent_cusip', by.y='company_cusip_6', all.x=T, all.y=F)


uuid.col.names <- c('company_uuid_from_participant_parent_cusip',
                    'company_uuid_from_parti_cusip',
                    'company_uuid_from_ultimate_parent_cusip',
                    'company_uuid_from_participant_ultimate_parent_cusip')

sdc$company_uuid <- apply(sdc[,uuid.col.names],1,function(x){
    xna <- na.omit(x)
    if (length(xna)==0) return(NA)
    return(paste(unique(xna),collapse = "|"))
  })

sdc <- sdc[,names(sdc)[which(!(names(sdc)%in%uuid.col.names))]]
sdc <- sdc[order(sdc$coop_id, decreasing = F),]

## add SDC relations to environment exportable as: coop
coop <- sdc
.ls <- c(.ls,'coop')


##===============================
## CLEAR NAMESPACE 
##-------------------------------
## clear tmp objects in environment 
## from this script not to be exported
## when called from external script
.rm <- c('x')
for (x in setdiff(ls(), .ls)) .rm <- c(.rm, x)
rm(list=.rm)
gc()

cat('done.\n')








