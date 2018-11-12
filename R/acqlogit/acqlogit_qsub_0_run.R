##########################################################################################
#
# MMC Network & Acquisitions
#
# Run scripts to:
#  - create propensity scores for counterfactual acquisition pairings
#  - Process node collapse from acquisitions to compute acquisition pairing covariates
#
# @author   sdowning.bm02g@nctu.edu.tw
#
# @export [list] cb
#
#
# ## update founded_on,closed_on dates  - Jin-Su's Email 2018-04-23
# ## C:\\Users\\T430\\Google Drive\\PhD\\Dissertation\\competition networks\\compnet2\\founded_on_date_edit
# co.date <- cb$readCsv('founded_on_date_edit/missing_companies_20180330.csv')
#
##########################################################################################


##========================
## SETTINGS
##------------------------
name_i <- 'ibm'       ## focal firm name
d <- 3                ## distance threshold for focal firm ego network
years <- 2007:2017    ## years to subset
##------------------------
graph_file <- 'g_full.graphml'
##------------------------

## DIRECTORIES
.script_dir  <- '/home/sdowning/compnet/R/acqlogit'
.work_dir    <- '/home/sdowning/acqlogit'
.data_dir    <- file.path(.work_dir,'data')
.results_dir <- file.path(.work_dir,'results')

## DIRECTORIES
.script_dir  <- '/home/sdowning/compnet/R/acqlogit'

## LOAD Scripts and Data
acf <- source(file.path(.script_dir,'acqlogit_compnet_functions.R'))$value ## FUNCTIONS 
cb  <- source(file.path(.script_dir,'acqlogit_cb_data_prep.R'))$value      ## DATA 


is.missing <- function(x)
{
  if(is.null(x)) 
    return(TRUE)
  return(is.na(x) | is.nan(x) | x == '')
}

##=======================================
##  PREP DATA and LOAD GRAPH
##---------------------------------------
## comptetition network
g.full <- read.graph(file.path(.data_dir,graph_file), format='graphml')

## add comp net vertex IDs to acquisitions dataframe
co_acq <- cb$co_acq
gdf <- data.frame(acquirer_vid=as.integer(V(g.full)), 
                  acquirer_name_unique=V(g.full)$name,
                  acquirer_net_uuid=V(g.full)$company_uuid)
co_acq <- merge(co_acq, gdf, by='acquirer_name_unique', all.x = T, all.y = F)
gdf <- data.frame(acquiree_vid=as.integer(V(g.full)), 
                  acquiree_name_unique=V(g.full)$name,
                  acquiree_net_uuid=V(g.full)$company_uuid)
co_acq <- merge(co_acq, gdf, by='acquiree_name_unique', all.x=T, all.y = F)

## SORT CO_ACQ BY acquisition date
co_acq <- co_acq[order(co_acq$acquired_on, decreasing = F), ]


## 1. Propensity scores
source(file.path(.script_dir, 'acqlogit_qsub_1_propensity_scores.R'))

## 2. Node Collapse (compute covarites)
source(file.path(.script_dir, 'acqlogit_qsub_2_node_collapse.R'))

cat('completed successfully.\n')




