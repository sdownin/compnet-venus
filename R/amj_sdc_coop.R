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

## DIRECTORIES
cb$data_dir <- "C:/Users/T430/Google Drive/PhD/Dissertation/crunchbase/crunchbase_export_20161024"
cb$work_dir <- "C:/Users/T430/Google Drive/PhD/Dissertation/competition networks/compnet2"
setwd(cb$work_dir)

## CrunchBase-Compustat name mapping
namemap.filename <- 'amj_cb_cs_name_map.csv'

if(!('namemap' %in% names(cb)))  {
  if (namemap.filename %in% dir()) {
    cb$namemap <- cb$readCsv(namemap.filename)
  } else {
    ## may take several hours to run; load from file instead, if possible
    source(file.path(getwd(),'R','amj_cb_cs_name_map.R'))
  }
}

## CACHE ENVIRONMENT to keep when clearing tmp objects added here
.ls <- ls()


cat('\nloading SDC cooperative relations data...')

##===============================
## Functions
##-------------------------------





##===============================
## SDC cooperative relations 
##-------------------------------

sdc <- cb$readCsv('SDC_data/awareness_583_software_2008-2018_SIC_report_cusip_7-SPLIT1526995992.csv')
# cusip.cols <- names(sdc)[grep('cusip',names(sdc))]
cols <- c('participant_parent_cusip','parti_cusip','ultimate_parent_cusip','participant_ultimate_parent_cusip')
View(sdc[,cols])









