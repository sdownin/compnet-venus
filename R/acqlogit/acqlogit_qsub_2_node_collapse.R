##########################################################################################
#
# MMC Network & Acquisitions
#
# Create acquisition logit model covariates arrays
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

library(network, quietly = T)
library(texreg, quietly = T)
library(igraph, quietly = T)
library(plyr, quietly = T)
library(reshape2)
library(intergraph)

# ##========================
# ## SETTINGS
# ##------------------------
# name_i <- 'ibm'       ## focal firm name
# d <- 3                ## distance threshold for focal firm ego network
# years <- 2007:2017    ## years to subset
# ##------------------------
# graph_file <- 'g_full.graphml'
# ##------------------------


# ## DIRECTORIES
# .script_dir  <- '/home/sdowning/compnet/R/acqlogit'
# .work_dir    <- '/home/sdowning/acqlogit'
# .data_dir    <- file.path(.work_dir,'data')
# .results_dir <- file.path(.work_dir,'results')
.compustat_dir <- '/home/sdowning/data/compustat'

# ## LOAD Scripts and Data
# acf <- source(file.path(.script_dir,'acqlogit_compnet_functions.R'))$value ## FUNCTIONS 
# cb  <- source(file.path(.script_dir,'acqlogit_cb_data_prep.R'))$value      ## DATA 
# 
# is.missing <- function(x)
# {
#   if(is.null(x)) 
#     return(TRUE)
#   return(is.na(x) | is.nan(x) | x == '')
# }
# 
# ##=======================================
# ##  PREP DATA and LOAD GRAPH
# ##---------------------------------------
# ## comptetition network
# g.full <- read.graph(file.path(.data_dir,graph_file), format='graphml')
# 
# ## add comp net vertex IDs to acquisitions dataframe
# co_acq <- cb$co_acq
# gdf <- data.frame(acquirer_vid=as.integer(V(g.full)), 
#                   acquirer_name_unique=V(g.full)$name,
#                   acquirer_net_uuid=V(g.full)$company_uuid)
# co_acq <- merge(co_acq, gdf, by='acquirer_name_unique', all.x = T, all.y = F)
# gdf <- data.frame(acquiree_vid=as.integer(V(g.full)), 
#                   acquiree_name_unique=V(g.full)$name,
#                   acquiree_net_uuid=V(g.full)$company_uuid)
# co_acq <- merge(co_acq, gdf, by='acquiree_name_unique', all.x=T, all.y = F)
# 
# ## SORT CO_ACQ BY acquisition date
# co_acq <- co_acq[order(co_acq$acquired_on, decreasing = F), ]


##--------------------------------------------------------------
##
##            CREATE FIRM NETWORK PERIOD LISTS  
##
##--------------------------------------------------------------

## get date periods and ego network based on settings
times <- sapply(years, function(x)paste0(x,'-01-01'))
start <- times[1]
end <- times[length(times)]

## EGO NETWORK
g.ego <- igraph::make_ego_graph(graph = g.full,
                                nodes = V(g.full)[V(g.full)$name==name_i],
                                order = d, mode = 'all')[[1]]

## NETWORKS IN TIMEFRAME TO PROCESS NODE COLLAPSE AND POCESS COVARIATES
g.pd      <- acf$makePdGraph(g.ego, start, end, isolates.remove=TRUE)   ## ego network
g.full.pd <- acf$makePdGraph(g.full, start, end, isolates.remove=TRUE)  ## full network

## CHECK NETWORK PERIOD SIZES
sapply(2:length(times), function(i){gi=acf$makePdGraph(g.ego, times[i-1], times[i], TRUE); return(c(e=ecount(gi),v=vcount(gi)))})

##--------------------------------------------------
##   LOAD IN EGO NETWORK AND GLOBAL NETWORK
##--------------------------------------------------
g.pd.file <- file.path(.data_dir,sprintf('g_%s_d%s_NCINIT_%s_%s.graphml',name_i,d,start,end))
g.full.pd.file <- file.path(.data_dir,sprintf('g_full_NCINIT_%s_%s.graphml',start,end))

cat(sprintf('qsub_2: loading graphml files %s %s',g.pd.file, g.full.pd.file))
g.pd <- igraph::read.graph(g.pd.file, format='graphml')
g.full.pd <- igraph::read.graph(g.full.pd.file, format='graphml')


## Full timeframe Clusters
V(g.pd)$nc <- as.integer(igraph::multilevel.community(g.pd)$membership)
V(g.full.pd)$nc <- as.integer(igraph::multilevel.community(g.full.pd)$membership)

## keep original timeframe graph
g.pd.orig <- g.pd
g.full.pd.orig <- g.full.pd
##----------------------------------









##============================================
##
##  DATA FOR CONTROLS AND PROPENSITY SCORES
##
##--------------------------------------------

##--------------------------------------------
## Load updated compustat data
##--------------------------------------------
csfunda.file <- file.path(.compustat_dir,'fundamentals-annual-UPDATED.csv')
if (!file.exists(csfunda.file)) { ## if file not exists, then run script to create it
  stop(sprintf('stop: cannot find csfunda.file %s',csfunda.file))
  #source(file.path(.script_dir,'acqlogit_compustat_update.R'))
}
csa2.all <- cb$readCsv(csfunda.file)
minyr <- min(unique(csa2.all$fyear), na.rm = T)
csa2 <- csa2.all[which(csa2.all$fyear != minyr & !is.na(csa2.all$fyear)), ]

##--------------------------------------------
## LOAD SEGMENTS DATA FOR DIVERSIFICATION
##--------------------------------------------
seg <- read.csv(file.path(.compustat_dir,'segments.csv'), na=c(NA,'','NA'), stringsAsFactors = F, fill = T)
# segcus <- read.csv(file.path(getwd(),'compustat','segments-customer.csv'), na=c(NA,'','NA'), stringsAsFactors = F, fill = T)
col.seg <- c('conm','tic','datadate','srcdate','stype','snms','soptp1','geotp','sic','SICS1','SICS2','sales','revts','nis')
seg2 <- seg[seg$soptp1=='PD_SRVC',col.seg] ## exclude geographic MARKET segments; include PD_SRVC product/service segments
seg2$date <- sapply(seg2$datadate, function(x){
  x <- as.character(x)
  return(sprintf('%s-%s-%s',str_sub(x,1,4),str_sub(x,5,6),str_sub(x,7,8)))
})
seg2$year <- sapply(seg2$date, function(x)as.integer(str_sub(x,1,4)))

# ###
# print(head(seg2[which(seg2$tic=='GOOGL'),],12))
# View(seg2[which(seg2$tic=='GOOGL'),])

##--------------------------------------------
##  MERGE Compustat Data into Compnet Dataframe 
##--------------------------------------------
## EGO GRAPH VERTEX DATARAME MERGE WITH COMPUSTAT DATA
g.full.pd.df <- as_data_frame(g.full.pd, what = 'vertices')
## rename graph vertex name to company_name_unique
names(g.full.pd.df)[which(names(g.full.pd.df)=='name')] <- 'company_name_unique'
## crunchbase ipo data fields for crunchbase compnet firms
ipocols <- c('company_name_unique','stock_symbol','stock_exchange_symbol','went_public_on')
g.full.pd.df <- merge(g.full.pd.df, cb$co_ipo[,ipocols], by.x='company_name_unique', by.y='company_name_unique', all.x=T, all.y=F)


##============================================
## MANUAL CORRECTIONS (COMPUSTAT STOCK SYMBOLS CHANGED AFTER CRUNCHBASE DATA)
##--------------------------------------------
# ## SEARCH COMPUSTAT NAMES
# csa2[grep('SONY',csa2$conm),]
# ## SEARCH COMPUSTAT TICKER SYMBOLS
# csa2[grep('software',csa2$conm,ignore.case = T),c('conm','tic')]
# ## SEARCH CRUNCHBASE IPOS
# cb$co_ipo[grep('software-ag',cb$co_ipo$company_name_unique),c('company_name_unique','stock_symbol','stock_exchange_symbol')]
# # > unique(as.character(df.sub$i[is.na(df.sub$roa)]))
# # [1] "ask-com"  ??            "bazaarvoice"-         
# # [3] "bmc-software"-          "compuware"-           
# # [5] "csc"-[bought by DXC]    "forcepoint"-           
# # [7] "fujitsu"-               "google"-              
# # [9] "htc"  ??                "mcafee"-               
# # [11] "naspers"-              "netsuite"             
# # [13] "opera-software"-       "qlik-technologies"-    
# # [15] "responsys"-            "rightnow-technologies"-
# # [17] "samsung-electronics"?? "servicepower" ??        
# # [19] "siemens"-              "software-ag"          
# # [21] "solarwinds"-           "sony"-  

### MAP:  [COMPUSTAT]::CONM |--> [CrunchBase]::ipo.stock_symbol
###   to replace the COMPUSTAT `tic` value with the CrunchBase `stock_symbol`
###   in order to merge COMPUSTAT financials into CrunchBase firm data
cs.conm_cb.stock <- c(
  `ALPHABET INC`='GOOG',
  `SONY CORP`='6758',  ## Tokyo exchange symbol -- just using it here to map Compustat data to CrunchBase for Sony
  `BMC SOFTWARE INC`='BMC',
  `DXC TECHNOLOGY COMPANY`='csc',  ## CSC was acquired by DXC
  `FUJITSU LTD`='6702', ## Tokyo exchange symbol
  `NASPERS LTD`='NPN',  ## Johannesburg stock exchange
  `OPERA LTD -ADR`='OPESF',
  `RESPONSYS INC`='MKTG',
  `SIEMENS AG`='SIEMENS',
  `SOLARWINDS INC`='SWI',
  `SONY CORP SNE`='6758',
  `BAZAARVOICE INC`='BV',
  `COMPUWARE CORP`='CPWR',
  `WEBSENSE INC`='WBSN',  ##Forcepoint or Websense ? 
  `MCAFEE INC`='MFE',
  `QLIK TECHNOLOGIES INC`='QLIK',
  `RIGHTNOW TECHNOLOGIES INC`='RNOW'
)
csa2$stock_symbol <- csa2$tic
for (conm in names(cs.conm_cb.stock)) {
  csa2$stock_symbol[csa2$conm==conm] <- cs.conm_cb.stock[conm]   ## set the 
}


##===============================
## MERGE COMPUSTAT DATA INTO CRUNCHBASE DATA
##-------------------------------
## merge in COMPUSTAT data by stock_exchange symbol
csa2.tmp <- csa2[csa2$stock_symbol %in% g.full.pd.df$stock_symbol & !is.na(csa2$stock_symbol),]
df.cs <- merge(g.full.pd.df, csa2.tmp, by.x='stock_symbol',by.y='stock_symbol', all.x=T, all.y=F)

## SEGMENTS DATA company_name_unique to merge 
df.conm.u <- data.frame()
for (conm in unique(df.cs$conm)) {
  names <- df.cs[which(df.cs$conm==conm),'company_name_unique']
  df.conm.u <- rbind(df.conm.u, data.frame(conm=conm, company_name_unique=unique(names)[1]) )
}
## drop NA
df.conm.u <- na.omit(df.conm.u)
## MERGE IN company_name_unique
seg3 <- merge(seg2, df.conm.u, by.x='conm', by.y='conm', all.x=T, all.y=F)
## filter only segments data for firms with company_name_unique matched from CrunchBase data
seg4 <- seg3[which(!is.na(seg3$company_name_unique)),]


##===================================
##
##  AQUISITIONS FILTER
##
##-----------------------------------
## GET ALL ACQ EVENT VERTICES 
## keep only acquisitions with acquirer in ego network and target in global competition network
acq.src <- co_acq[ co_acq$acquirer_name_unique %in% V(g.pd)$name & co_acq$acquiree_name_unique %in% V(g.full.pd)$name, ]
acq.src.allpd <- acq.src[ acq.src$acquired_on >= start & acq.src$acquired_on < end , ]
acq.src.allpd <- acq.src.allpd[order(acq.src.allpd$acquired_on, decreasing = F), ]




##---------------------------------------------
## LOAD DATA AFTER PROPENSITIES ARE COMPUTED
##---------------------------------------------
l.prop <- readRDS(file.path(.data_dir, sprintf('acqlogit_propensity_score_comp_list_%s_d%s_ctrl.rds',name_i,d)))
g.prop <- l.prop$g.prop
g.full.prop <- l.prop$g.full.prop
# g.prop.nc <- l.prop$g.prop.nc ## ?
# g.full.prop.nc <- l.prop$g.full.prop.nc ## ?
a.df <- l.prop$a.df
a.df.ctrl <- l.prop$a.df.ctrl
t.df <- l.prop$t.df
a.prop <- l.prop$a.prop
t.prop <- l.prop$t.prop




##=============================================
## YEAR PERIODS: DEFINE NICHE CLUSTERS
##---------------------------------------------
l <- list()
df.mmc <- data.frame()
df.rem <- data.frame()
df.reg <- data.frame()
lidx <- 0  ## acquisition list index
timeval <- timeval.last <- 0

g.pd.nc <- g.pd.orig            ## ego network to node collapse for network covariates
g.full.pd.nc <- g.full.pd.orig  ## full netowrk to node collapse for network covariates



##===============================
##
##  MAIN LOOP: COMPUTE COVARIATES
##
##-------------------------------

## ACQUISITION EVENTS:  UPDATE MMC & DYNAMIC EFFs

do.node.collapse <- TRUE ## START WITH FALSE AND SET TO TRUE ON FIRST LOOP

for (j in 1:nrow(acq.src.allpd)) {
  
  df.acq.j <- acq.src.allpd[j,]  ## this acquisition row in the acquisition dataframe
  date_j <- acq.src.allpd$acquired_on[j]
  year_j <- as.integer(str_sub(date_j,1,4))
  ## g.pd            d2 updated each acquisition
  ## g.pd.orig       d2 original
  ## g.full.pd.orig  global network within timeframe start, end
  
  cat(sprintf('\n\n%s : acquisition %s (%.2f%s)\n\n',date_j,j,100*j/nrow(acq.src.allpd),'%'))
  
  ##-------------------------------------------
  ## NODE COLLAPSE PREVIOUS ACQUISITION IF IT WAS SKIPPED 
  ##-------------------------------------------
  if (do.node.collapse & j > 1) {
    cat(sprintf('node collapsing previous skipped acquisition %s:\n',(j-1)))
    g.pd.nc <- acf$nodeCollapseGraph(g.pd.nc, acq.src.allpd[(j-1),])
    g.full.pd.nc <- acf$nodeCollapseGraph(g.full.pd.nc, acq.src.allpd[(j-1),])
    ## FLAG TO NODE COLLAPSE NEXT LOOP
    do.node.collapse <- TRUE
  } else { ## DONT NODE COLLAPSE PREVIOUS ACQUISITION IF IT WAS ALREADY NODE COLLAPSED (NOT SKIPPED)
    ## FLAG TO NODE COLLAPSE NEXT LOOP
    do.node.collapse <- TRUE
  }
  
  ##------------------------------------------- 
  ## CHECKS TO SKIP THIS ACQUISITION
  ##------------------------------------------- 
  ## ACQUISITION MUST BE IN PROPENSITY SCORES DATAFRAMES
  if ( !(df.acq.j$acquisition_uuid %in% a.prop$acquisition_uuid) | 
       !(df.acq.j$acquisition_uuid %in% t.prop$acquisition_uuid) ) {
    next 
  }
  # SKIP IF ALL ACQUIRER ALTERNATIVES HAVE NO COMPUSTAT FINANICALS (check m2b all NA)
  if (all(is.na(a.prop$m2b[which(a.prop$acquisition_uuid==df.acq.j$acquisition_uuid)]))) 
    next
  ## SKIP IF EITHER ACQUIRER OR TARGET IS NOT IN NETWORK
  if ( !(acq.src.allpd$acquiree_name_unique[j] %in% V(g.full.pd.orig)$name) ) 
    next
  if ( !(acq.src.allpd$acquirer_name_unique[j] %in% V(g.pd)$name) ) 
    next
  ## SKIP IF ACQUIRER IS NOT PUBLIC
  isPublicAcq <- (acq.src.allpd$acquirer_name_unique[j] %in% cb$co_ipo$company_name_unique 
                  & cb$co_ipo$went_public_on[cb$co_ipo$company_name_unique==acq.src.allpd$acquirer_name_unique[j]] <= acq.src.allpd$acquired_on[j])
  if (length(isPublicAcq)==0)
    next
  if ( ! isPublicAcq)
    next

  
  lidx <- length(l) + 1
  l[[lidx]] <- list()
  

  ## Subset Year Period Network
  cat('  subsetting network edges for year period of acquisition...')
  ## Period network removes competitive relations ended < year_j OR started >= year_j+1
  g.pd <- asIgraph(acf$makePdNetwork(asNetwork(g.pd.nc), year_j, year_j+1, isolates.remove = F))
  g.full.pd <- asIgraph(acf$makePdNetwork(asNetwork(g.full.pd.nc), year_j, year_j+1, isolates.remove = F))
  V(g.pd)$name <- V(g.pd)$vertex.names
  V(g.full.pd)$name <- V(g.full.pd)$vertex.names
  cat('done.')
  
  ## GET FIRM x FRIM MMC MATRIX TO USE IN FM-MMC COMPUTATION
  m.mmc <- acf$getFirmFirmMmc(g.pd, as.integer(V(g.pd)$nc))
  
  ## Update MMC after acquisition
  l[[lidx]]$mmc <- acf$getFmMmc(g.pd, as.integer(V(g.pd)$nc))
  
  ## MMC degree: number of mmc dyads linked to each firm i
  V(g.pd)$num.mmc.comps <- acf$getNumMmcRivalsByMembership(g.pd, as.integer(V(g.pd)$nc), m.mmc)
  
  ## SUM FM MMC over markets  ??????
  V(g.pd)$fm.mmc.sum <- rowSums(l[[lidx]]$mmc)
  V(g.pd)$num.mkts <- apply(l[[lidx]]$mmc, MARGIN=1, FUN=function(x){
    return(length(x[x>0]))
  })
  
  ## GET  DATAFRAME VARS
  
  ## Acquirer d2 original org.vid
  xi.orig.vid <- V(g.pd.orig)$orig.vid[which(acq.src.allpd$acquirer_name_unique[j] == V(g.pd.orig)$name)]
  ## target d2 original org.vid
  xj.orig.vid <- V(g.pd.orig)$orig.vid[which(acq.src.allpd$acquiree_name_unique[j] == V(g.pd.orig)$name)]
  xj.orig.vid <- ifelse(length(xj.orig.vid) > 1, xj.orig.vid, NA)
  ## acquirer  d2 t=j id
  xi <- which(V(g.pd)$name==acq.src.allpd$acquirer_name_unique[j])
  ## target  d2 t=j id
  xj <- which(V(g.pd)$name==acq.src.allpd$acquiree_name_unique[j])
  # acquirer id in original graph (at start of period)
  xi.orig <- as.integer(V(g.pd.orig)[V(g.pd.orig)$name==acq.src.allpd$acquirer_name_unique[j]])
  xi.nc <- as.integer(V(g.pd.orig)$nc[xi.orig]) ## original nc for the period
  # 
  xi.mmc.sum <-  V(g.pd)$fm.mmc.sum[xi]
  xi.num.mkts <-  V(g.pd)$num.mkts[xi]
  num.mmc.comps <-  V(g.pd)$num.mmc.comps[xi]
  ## 
  xj.orig <- ifelse( !is.na(xj.orig.vid), as.integer(V(g.pd.orig)[V(g.pd.orig)$orig.vid==xj.orig.vid]), NA)
  xj.orig <- ifelse(length(xj.orig) > 1, xj.orig, NA)
  xj.nc <- ifelse(length(xj)==0,NA,  V(g.pd.orig)$nc[xj.orig] )  ## original nc for the period
  
  ##--------------------------------------
  ##
  ## TARGET ALTERNATIVES SET
  ##
  ##--------------------------------------
  ## SELECT FROM PROPENSITY SCORES (if alternatives more than 5)
  t.prop.j <- t.prop[which(t.prop$acquisition_uuid==df.acq.j$acquisition_uuid & !is.na(t.prop$pred)),]
  t.prop.j <- t.prop.j[order(t.prop.j$pred, decreasing = T), ]
  if (nrow(t.prop.j)==0) {
    next
  } else if (nrow(t.prop.j)>6) {
    idx.1 <- which(t.prop.j$y==1)
    idx.0 <- which(t.prop.j$y==0)
    idx.0.sample <- idx.0[1:min(5,length(idx.0))]
    alt.targ.names <- t.prop.j$company_name_unique[c(idx.1, idx.0.sample)]
  } else {
    alt.targ.names <- t.prop.j$company_name_unique
  }
  
  ## ACTUAL TARGET ID
  targ.id <- which(V(g.full.pd)$name == acq.src.allpd$acquiree_name_unique[j])
  ## START TARGET ALTERNATIVES DATAFRAME
  df.targ.alt <- cb$co[which(cb$co$company_name_unique %in% alt.targ.names),]
  ## MERGE IN y and d
  df.targ.alt <- merge(df.targ.alt, t.prop.j[,c('company_name_unique','y','d')], by.x='company_name_unique',by.y='company_name_unique',all.x=T,all.y=F)

  ## ipo status
  df.targ.alt$is.public <- sapply(1:nrow(df.targ.alt), function(x){
    isNotOperating <- df.targ.alt$status[x] != 'operating'
    ipo.date <- cb$co_ipo$went_public_on[which(cb$co_ipo$company_name_unique == df.targ.alt$company_name_unique[x])]
    ipo.date <- min(ipo.date, na.rm=TRUE)
    if (length(ipo.date)<1) 
      return(0)
    return(ifelse( isNotOperating & ipo.date <= date_j, 1, 0))
  })
  ## target had IPO
  df.targ <- df.targ.alt[which(df.targ.alt$company_name_unique == V(g.full.pd)$name[targ.id]), ]
  
  if (nrow(df.targ) == 0)
    next
  
  ## select based on ownership status  
  df.targ.alt <- df.targ.alt[which(df.targ.alt$is.public == df.targ$is.public),]

    ## add MMC
  df.targ.alt$fm.mmc.sum <- sapply(df.targ.alt$company_name_unique, function(name){
      ifelse(name %in% V(g.pd)$name, V(g.pd)$fm.mmc.sum[which(V(g.pd)$name == name)] , NA)
    })
  df.targ.alt$num.mkts <- sapply(df.targ.alt$company_name_unique, function(name){
      ifelse(name %in% V(g.pd)$name, V(g.pd)$num.mkts[which(V(g.pd)$name == name)] , NA)
    })
  df.targ.alt$num.mmc.comps <- sapply(df.targ.alt$company_name_unique, function(name){
      ifelse(name %in% V(g.pd)$name, V(g.pd)$num.mmc.comps[which(V(g.pd)$name == name)] , NA)
    })
  ## ACQUISITIONS
  df.targ.alt$acqs <- unname(sapply(df.targ.alt$company_name_unique, function(name){
      length(which(cb$co_acq$acquirer_name_unique==name & cb$co_acq$acquired_on <= date_j))
    }))
  ## VENTURE FUNDING
  df.targ.alt$fund.v.cnt <- unname(sapply(df.targ.alt$company_name_unique, function(name){
      length(which(cb$co_rou$company_name_unique==name & cb$co_rou$announced_on <= date_j & cb$co_rou$funding_round_type=='venture'))
    }))
  df.targ.alt$fund.v.amt <- unname(sapply(df.targ.alt$company_name_unique, function(name){
      idx <- which(cb$co_rou$company_name_unique==name & cb$co_rou$announced_on <= date_j & cb$co_rou$funding_round_type=='venture')
      return(sum(cb$co_rou$raised_amount_usd[idx], na.rm = T))
    }))
  ## ALL FUNDING
  df.targ.alt$fund.cnt <- unname(sapply(df.targ.alt$company_name_unique, function(name){
      length(which(cb$co_rou$company_name_unique==name & cb$co_rou$announced_on <= date_j))
    }))
  df.targ.alt$fund.amt <- unname(sapply(df.targ.alt$company_name_unique, function(name){
      idx <- which(cb$co_rou$company_name_unique==name & cb$co_rou$announced_on <= date_j)
      return(sum(cb$co_rou$raised_amount_usd[idx], na.rm = T))
    }))
  ## USE EGO and GLOBAL NETWORK for DEGREE
  df.targ.alt$deg <- sapply(df.targ.alt$company_name_unique, function(name){
      ifelse(name %in% V(g.pd)$name, igraph::degree(g.pd,which(V(g.pd)$name==name)) , NA)
    })
  df.targ.alt$deg.full <- sapply(df.targ.alt$company_name_unique, function(name){
      ifelse(name %in% V(g.full.pd)$name, igraph::degree(g.full.pd,which(V(g.full.pd)$name==name)) , NA)
    })
  
  ##----------------------------------------------------------
  ## DATA SAMPLE OF ALL ACQUIRERS (REAL + 5 ALTERNATIVES)
  l[[lidx]]$df.targ.alt <- df.targ.alt

  
  
  ##--------------------------------------
  ##
  ## ACQUIRER ALTERNATIVES SET
  ##
  ##--------------------------------------
  ## SELECT FROM PROPENSITY SCORES (if alternatives more than 5)
  a.prop.j <- a.prop[which(a.prop$acquisition_uuid==df.acq.j$acquisition_uuid & !is.na(a.prop$pred)),]
  a.prop.j <- a.prop.j[order(a.prop.j$pred, decreasing = T), ]
  if (nrow(a.prop.j)==0) {
    next
  } else if (nrow(a.prop.j)>6) {
    idx.1 <- which(a.prop.j$y==1)
    idx.0 <- which(a.prop.j$y==0)
    idx.0.sample <- idx.0[1:min(5,length(idx.0))]
    alt.acq.names <- a.prop.j$company_name_unique[c(idx.1, idx.0.sample)]
  } else {
    alt.acq.names <- a.prop.j$company_name_unique
  }
  
  ## ACTUAL TARGET ID
  acq.id <- which(V(g.full.pd)$name == acq.src.allpd$acquirer_name_unique[j])
  ## START TARGET ALTERNATIVES DATAFRAME
  df.acq.alt <- cb$co[which(cb$co$company_name_unique %in% alt.acq.names),]
  ## MERGE IN y and d
  df.acq.alt <- merge(df.acq.alt, a.prop.j[,c('company_name_unique','y','d')], by.x='company_name_unique',by.y='company_name_unique',all.x=T,all.y=F)
  
  
  ## ipo status
  df.acq.alt$is.public <- sapply(1:nrow(df.acq.alt), function(x){
    isNotOperating <- df.acq.alt$status[x] != 'operating'
    ipo.date <- cb$co_ipo$went_public_on[which(cb$co_ipo$company_name_unique == df.acq.alt$company_name_unique[x])]
    ipo.date <- min(ipo.date, na.rm=TRUE)
    if (length(ipo.date)<1) 
      return(0)
    return(ifelse( isNotOperating & ipo.date <= date_j, 1, 0))
  })
  ## target had IPO
  df.acq <- df.acq.alt[which(df.acq.alt$company_name_unique == V(g.full.pd)$name[acq.id]), ]
  
  if (nrow(df.acq) == 0)
    next
  if (nrow(df.acq) > 1)
    stop(sprintf('error: nrow df.acq %s > 1',nrow(df.acq)))
  

  ## select based on ownership status
  df.acq.alt <- df.acq.alt[which(df.acq.alt$is.public == df.acq$is.public), ]

  ## set MMC
  df.acq.alt$fm.mmc.sum <- sapply(df.acq.alt$company_name_unique, function(name){
      ifelse(name %in% V(g.pd)$name, as.numeric(V(g.pd)$fm.mmc.sum[which(V(g.pd)$name == name)]) , NA)
    })
  df.acq.alt$num.mkts <- sapply(df.acq.alt$company_name_unique, function(name){
      ifelse(name %in% V(g.pd)$name, as.numeric(V(g.pd)$num.mkts[which(V(g.pd)$name == name)]) , NA)
    })
  df.acq.alt$num.mmc.comps <- sapply(df.acq.alt$company_name_unique, function(name){
      ifelse(name %in% V(g.pd)$name, as.numeric(V(g.pd)$num.mmc.comps[which(V(g.pd)$name == name)]) , NA)
    })
  ## ACQUISITIONS
  df.acq.alt$acqs <- unname(sapply(df.acq.alt$company_name_unique, function(name){
    length(which(cb$co_acq$acquirer_name_unique==name & cb$co_acq$acquired_on <= date_j))
  }))
  ## USE EGO and GLOBAL NETWORK for DEGREE
  df.acq.alt$deg <- sapply(df.acq.alt$company_name_unique, function(name){
    ifelse(name %in% V(g.pd)$name, igraph::degree(g.pd,which(V(g.pd)$name==name)) , NA)
  })
  df.acq.alt$deg.full <- sapply(df.acq.alt$company_name_unique, function(name){
    ifelse(name %in% V(g.full.pd)$name, igraph::degree(g.full.pd,which(V(g.full.pd)$name==name)) , NA)
  })
  
  ## KEEP SAME DIMENSIONS AS TARGET DATAFRAME
  df.acq.alt$fund.v.cnt <- NA
  df.acq.alt$fund.v.amt <- NA
  df.acq.alt$fund.cnt <- NA
  df.acq.alt$fund.amt <- NA
  
  ##----------------------------------------------------------
  ## DATA SAMPLE OF ALL ACQUIRERS (REAL + 5 ALTERNATIVES)
  l[[lidx]]$df.acq.alt <- df.acq.alt


  
  
  
  ##--------------------------------------
  ##
  ##  NETWORK COVARIATES
  ##
  ##--------------------------------------
  cat('computing network covariates...')
  df.acq.alt$set <- 'acquirer'
  df.acq.alt$event <- sapply(df.acq.alt$d, function(d)ifelse(as.integer(d)==0, 1, 0))
  df.targ.alt$set <- 'target'
  df.targ.alt$event <- sapply(df.targ.alt$d, function(d)ifelse(as.integer(d)==0, 1, 0))
  df.alt <- rbind(df.acq.alt, df.targ.alt)
  df.alt$t <- j ## acquisition index
  df.alt <- df.alt[order(which(V(g.full.pd.orig)$name %in% df.alt$company_name_unique )), ] ## confirm ascencing order
  
  if (!all(count(df.alt$set)$freq>1)) {
    cat('missing alternative match. skipping.\n')
    next  ## SKIP IF NOT AT LEAST 1 ALTERNATIVE FOR ACQUIRER AND TARGET
  }

  # ## Create Diff Graph (removed|acquired nodes are represented as isolates)
  vids <- which( V(g.full.pd)$name %in% df.alt$company_name_unique )
  vids.orig <- which( V(g.full.pd.orig)$name %in% df.alt$company_name_unique )
  vids.orig.rm <- vids[which( !(vids.orig %in% vids))]
  mapping <- V(g.full.pd.orig)[which(V(g.full.pd.orig)$orig.vid %in% V(g.full.pd)$orig.vid) ]
  g.diff <- igraph::contract.vertices(g.full.pd, mapping = mapping)
  V(g.diff)$name <- V(g.full.pd.orig)$name
  vids.diff <- as.integer( V(g.diff)[which( V(g.diff)$name %in% df.alt$company_name_unique )] )
  
  ## global covars
  tmp.cov <- data.frame(
    company_name_unique = unlist(V(g.diff)$name[vids.diff]),
    closeness = unname(igraph::closeness(g.diff, vids = vids.diff,normalized = TRUE)),
    constraint = unname(unlist(igraph::constraint(g.diff, nodes = vids.diff)))
  )
  df.alt <- merge(df.alt, tmp.cov, by = 'company_name_unique', all.x = T, all.y = F)
  # ## acquisition experience
  # df.alt$acq.experience <- unlist(sapply(1:nrow(df.alt), function(x){ return(
  #   nrow(acq.src.allpd[which(acq.src.allpd$acquirer_name_unique == df.alt$company_name_unique[x]
  #                            & acq.src.allpd$acquired_on <= date_j), ]) / j ## scale experience to num observed acquisitions
  # )}))
  # ## local covars in pd graph
  # df.alt$fm.mmc.sum <- sapply(df.alt$company_name_unique, function(name){
  #   ifelse(name %in% V(g.pd)$name, as.numeric(V(g.pd)$fm.mmc.sum[which(V(g.pd)$name == name)]), NA)
  # })
  # df.alt$num.mkts  <-  sapply(df.alt$company_name_unique, function(name){
  #   ifelse(name %in% V(g.pd)$name, as.numeric(V(g.pd)$num.mkts[which(V(g.pd)$name == name)]), NA)
  # })
  # df.alt$num.mmc.comps  <-  sapply(df.alt$company_name_unique, function(name){
  #   ifelse(name %in% V(g.pd)$name, as.numeric(V(g.pd)$num.mmc.comps[which(V(g.pd)$name == name)]), NA)
  # })
  
  cat('done.\n')
  ##---------------------------------------------
  
  
  ##---------------------------------------------
  ## MERGE IN PUBLIC FIRM FINANCIAL CONTROLS
  ##---------------------------------------------
  cat('adding public firm financial controls...')
  ctrl.col <- c('company_name_unique','datayear','act','emp','ebitda','m2b','che')
  ctrl.idx <- which(df.cs$datayear == (df.acq.j$acquired_year-1) )
  if (length(ctrl.idx)==0)
    next
  df.alt <- merge(df.alt, df.cs[ctrl.idx,ctrl.col], by.x='company_name_unique',by.y='company_name_unique',all.x=T,all.y=F)
  df.alt$ln_asset <- log(df.alt$act)
  df.alt$ln_emp <- log(df.alt$emp)
  df.alt$roa <- df.alt$ebitda / df.alt$act
  df.alt$cash <- df.alt$che / df.alt$act
  cat('done.\n')

  
  ##---------------------------------------------
  ## RIVALS RECENT ACQUISITIONS
  ##---------------------------------------------
  cat('computing acquirers rivals recent acquisitions...')
  df.alt$rival.acq.1 <- NA
  df.alt$rival.acq.2 <- NA
  df.alt$rival.acq.3 <- NA
  for (ri in 1:nrow(df.alt)) 
  {
    x <- df.alt[ri,]
    xdate <- as.character(df.acq.j$acquired_on)  ## date of this acquitision j
    if(is.na(xdate)) next
    parts <- str_split(xdate,'[-]')[[1]]
    xdate1 <- sprintf('%04d-%s-%s',as.numeric(parts[1])-1,parts[2],parts[3])
    xdate2 <- sprintf('%04d-%s-%s',as.numeric(parts[1])-2,parts[2],parts[3])
    xdate3 <- sprintf('%04d-%s-%s',as.numeric(parts[1])-3,parts[2],parts[3])
    rivals <- names(neighbors(g.full.pd, v = which(V(g.full.pd)$name==x$company_name_unique)))
    df.alt$rival.acq.1[ri] <- sum(rivals %in% cb$co_acq$acquirer_name_unique[cb$co_acq$acquired_on < xdate & cb$co_acq$acquired_on >= xdate1])
    df.alt$rival.acq.2[ri] <- sum(rivals %in% cb$co_acq$acquirer_name_unique[cb$co_acq$acquired_on < xdate & cb$co_acq$acquired_on >= xdate2])
    df.alt$rival.acq.3[ri] <- sum(rivals %in% cb$co_acq$acquirer_name_unique[cb$co_acq$acquired_on < xdate & cb$co_acq$acquired_on >= xdate3])
  }
  cat('done.\n')
  
  
  ##---------------------------------------------
  ## COMPUTE PRODUCT SIMILARITY OF COUNTERFACTUAL acquirer|target TO ACTUAL target|acquirer
  ##---------------------------------------------
  cat('computing product similarity...')
  df.alt$ij.sim <- NA
  df.alt$ij.cossim <- NA
  for (si in 1:nrow(df.alt)) 
  {
    x <- df.alt[si,]
    if (x$set == 'target') {
      firm.i <- df.acq.j$acquirer_name_unique
      firm.j <- x$company_name_unique
    } else {
      firm.i <- x$company_name_unique
      firm.j <- df.acq.j$acquiree_name_unique
    }
    ## acquirer
    cats.i <- str_split(cb$co$category_list[cb$co$company_name_unique == firm.i], '[|]')[[1]]
    cg.i <- str_split(cb$co$category_group_list[cb$co$company_name_unique == firm.i], '[|]')[[1]]
    c.i <- unique(c(cats.i,cg.i))
    ## target
    cats.j <- str_split(cb$co$category_list[cb$co$company_name_unique == firm.j], '[|]')[[1]]
    cg.j <- str_split(cb$co$category_group_list[cb$co$company_name_unique == firm.j], '[|]')[[1]]
    c.j <- unique(c(cats.j,cg.j))
    ## similarity
    c.all <- unique(c(c.i,c.j))
    v.i <- as.integer(c.all %in% c.i)
    v.j <- as.integer(c.all %in% c.j)
    df.alt$ij.sim[si] <-  (v.i %*% v.j)[1,1]
    df.alt$ij.cossim[si] <-  (v.i %*% v.j)[1,1] / (sqrt(sum(v.i^2)) * sqrt(sum(v.j^2)))
  }
  cat('done.\n')
  
  
  ##---------------------------------------------
  ## Diversification Control
  ##---------------------------------------------
  df.alt$div <- NA
  for (di in 1:nrow(df.alt))
  {
    x = df.alt[di,]
    df.seg <- seg4[which(seg4$year==(df.acq.j$acquired_year-1) & seg4$company_name_unique==x$company_name_unique),]
    if (nrow(df.seg)==0) next
    sm <- sum(df.seg$sales)
    df.alt$div[di] <- sum(sapply(df.seg$sales, function(x) ifelse(x==0, 0, (x/sm) * log( 1/(x/sm) )) ))
  }
  
  
  ##---------------------------------------------
  ## CACHE ALTERNATIVES DATAFRAME
  ##---------------------------------------------
  l[[lidx]]$df.alt <- df.alt
  
  
  ##---------------------------------------------
  ## SYNERGIES
  ##---------------------------------------------
  cat('computing counterfactual networks for positional synergy:\n')
  ## ACQUIRER's POSITION
  acquirer <- acq.src.allpd$acquirer_name_unique[j]
  ##  Counterfactual target graphs
  g.cf <- lapply(df.targ.alt$company_name_unique, function(name){
    tmp.acq.df <- data.frame(
      acquirer_uuid = acq.src.allpd$acquirer_uuid[j],
      acquiree_uuid = cb$co$company_uuid[cb$co$company_name_unique==name],
      acquired_on = acq.src.allpd$acquired_on[j]
    )
    return(acf$nodeCollapseGraph(g.full.pd, tmp.acq.df))
  })
  names(g.cf) <- df.targ.alt$company_name_unique

  ##---------------------------------------------
  ##  APPEND PAIRING COVARIATES TO REGRESSION DATAFRAME
  ##---------------------------------------------
  cat('appending dyadic regression dataframe...\n')

  for (k in 1:nrow(df.alt[df.alt$set=='acquirer', ])) {
    ix <- which( df.alt$company_name_unique == df.alt[df.alt$set=='acquirer', ][k, ]$company_name_unique )

    if (length(df.alt$event[ix])==0)
      next

    for (r in 1:nrow(df.alt[df.alt$set=='target', ])) {
      jx <- which( df.alt$company_name_unique == df.alt[df.alt$set=='target', ][r, ]$company_name_unique )

      # cat(sprintf('k %s, ix %s, r %s, jx %s\n',k,ix,r,jx))
      
      ## skip pairing if neither acquirer nor target were in this actual event
      if (length(df.alt$event[jx])==0)
        next
      if ( !as.integer(df.alt$event[ix]) & !as.integer(df.alt$event[jx]) )
        next
      
      cat(sprintf('appending pairing %s-->%s\n',df.alt$company_name_unique[ix],df.alt$company_name_unique[jx]))
      
      if (df.alt$company_name_unique[ix] != df.alt$company_name_unique[jx]) {
        # cat(sprintf('ix %s jx %s\n',ix,jx))
        
        ## DISTANCE
        ij.dist <- igraph::distances(g.full.pd,
                                     v = which(V(g.full.pd)$name == df.alt$company_name_unique[ix]),
                                     to= which(V(g.full.pd)$name == df.alt$company_name_unique[jx]) )
        ## AQUIRER POSITION
        cat('  power centralities\n')
        pow.n1 <- unname(igraph::power_centrality(g.full.pd, nodes = which(V(g.full.pd)$name==df.alt$company_name_unique[ix]), exponent = -0.1))
        pow.n2 <- unname(igraph::power_centrality(g.full.pd, nodes = which(V(g.full.pd)$name==df.alt$company_name_unique[ix]), exponent = -0.2))
        pow.n3 <- unname(igraph::power_centrality(g.full.pd, nodes = which(V(g.full.pd)$name==df.alt$company_name_unique[ix]), exponent = -0.3))

        ## COUNTERFACTUAL NETWORK `r` for different target jx
        g.cf.r <- g.cf[[ df.alt$company_name_unique[jx] ]]
        
        ## COUNTERFACTUAL NETWORK COVARIATES
        cat('  network counterfactuals\n')
        # cf.m.mmc <- acf$getFirmFirmMmc(g.cf.r, as.integer(V(g.cf.r)$nc))
        # cf.num.mmc.comps <- acf$getNumMmcRivalsByMembership(g.cf.r, as.integer(V(g.cf.r)$nc), cf.m.mmc)
        cf.closeness <- igraph::closeness(g.cf.r, vids = which(V(g.cf.r)$name==df.alt$company_name_unique[ix]), normalized = TRUE)
        cf.degree <- igraph::degree(g.cf.r, v = which(V(g.cf.r)$name==df.alt$company_name_unique[ix]))
        cf.constraint <- igraph::constraint(g.cf.r, nodes = which(V(g.cf.r)$name==df.alt$company_name_unique[ix]))
        cf.pow.n1 <- unname(igraph::power_centrality(g.cf.r, nodes = which(V(g.cf.r)$name==df.alt$company_name_unique[ix]), exponent = -0.1))
        cf.pow.n2 <- unname(igraph::power_centrality(g.cf.r, nodes = which(V(g.cf.r)$name==df.alt$company_name_unique[ix]), exponent = -0.2))
        cf.pow.n3 <- unname(igraph::power_centrality(g.cf.r, nodes = which(V(g.cf.r)$name==df.alt$company_name_unique[ix]), exponent = -0.3))
        
        ## PAIRING DATAFRAME
        df.tmp.dyad <- data.frame(
          ###------  event metadata ------
          y = ifelse(as.integer(df.alt$event[ix]) & as.integer(df.alt$event[jx]), 1, 0),
          t = j,
          date = date_j,
          i = df.alt$company_name_unique[ix],
          j = df.alt$company_name_unique[jx],
          ###------  acquirer covars ------
          i.age = 2018 - df.alt$founded_year[ix],
          i.pow.n1 = pow.n1,
          i.pow.n2 = pow.n2,
          i.pow.n3 = pow.n3,
          i.closeness = df.alt$closeness[ix],
          i.deg = df.alt$deg[ix],
          i.deg.full = df.alt$deg.full[ix],
          i.fm.mmc.sum = ifelse(is.missing(df.alt$fm.mmc.sum[ix]), NA, df.alt$fm.mmc.sum[ix]),
          i.num.mkts = ifelse(is.missing(df.alt$num.mkts[ix]), NA, df.alt$num.mkts[ix]),
          i.num.mmc.comps = ifelse(is.missing(df.alt$num.mmc.comps[ix]), NA, df.alt$num.mmc.comps[ix]),
          i.constraint = df.alt$constraint[ix],
          i.acqs = df.alt$acqs[ix],
          i.rival.acq.1 = df.alt$rival.acq.1[ix],
          i.rival.acq.2 = df.alt$rival.acq.2[ix],
          i.rival.acq.3 = df.alt$rival.acq.3[ix],
          i.div = df.alt$div[ix],
          i.ij.sim = df.alt$ij.sim[ix],
          i.ij.cossim = df.alt$ij.cossim[ix],
          i.ln_asset = df.alt$ln_asset[ix],
          i.ln_emp = df.alt$ln_emp[ix],
          i.roa = df.alt$roa[ix],
          i.cash = df.alt$cash[ix],
          i.m2b = df.alt$m2b[ix],
          ###------  target covars ------ 
          j.age = 2018 - df.alt$founded_year[jx],
          j.deg = df.alt$deg[jx],
          j.deg.full = df.alt$deg.full[jx],
          j.fm.mmc.sum = ifelse(is.missing(df.alt$fm.mmc.sum[jx]), NA, df.alt$fm.mmc.sum[jx]),
          j.num.mkts = ifelse(is.missing(df.alt$num.mkts[jx]), NA, df.alt$num.mkts[jx]),
          j.num.mmc.comps = ifelse(is.missing(df.alt$num.mmc.comps[jx]), NA, df.alt$num.mmc.comps[jx]),
          j.constraint = df.alt$constraint[jx],
          j.acqs = df.alt$acqs[jx],
          j.rival.acq.1 = df.alt$rival.acq.1[jx],
          j.rival.acq.2 = df.alt$rival.acq.2[jx],
          j.rival.acq.3 = df.alt$rival.acq.3[jx],
          j.fund.v.cnt <- df.alt$fund.v.cnt[jx],
          j.fund.v.amt <- df.alt$fund.v.amt[jx],
          j.fund.cnt <- df.alt$fund.cnt[jx],
          j.fund.amt <- df.alt$fund.amt[jx],
          j.ij.sim = df.alt$ij.sim[jx],
          j.ij.cossim = df.alt$ij.cossim[jx],
          ###------ dyadic covars: acquisition pairing ------
          ij.same.region = ifelse(df.alt$region[ix] == df.alt$region[jx], 1, 0),
          ij.same.state = ifelse(df.alt$state_code[ix] == df.alt$state_code[jx], 1, 0),
          ij.same.country = ifelse(df.alt$country_code[ix] == df.alt$country_code[jx], 1, 0),
          ij.same.employee.range = ifelse(df.alt$employee_count[ix] == df.alt$employee_count[jx], 1, 0),
          ij.dist = ifelse( class(ij.dist)=='matrix' & nrow(ij.dist)>0 & ncol(ij.dist)>0, ij.dist[1,1], Inf),
          ij.diff.deg = as.numeric(df.alt$deg[ix]) - as.numeric(df.alt$deg[jx]),
          ij.diff.deg.full = as.numeric(df.alt$deg.full[ix]) - as.numeric(df.alt$deg.full[jx]),
          ij.diff.fm.mmc.sum = ifelse(any(is.missing(df.alt$fm.mmc.sum[ix]),is.missing(df.alt$fm.mmc.sum[jx])), NA, as.numeric(df.alt$fm.mmc.sum[ix]) - as.numeric(df.alt$fm.mmc.sum[jx])),
          ij.diff.num.mkts = ifelse(any(is.missing(df.alt$num.mkts[ix]), is.missing(df.alt$num.mkts[jx])), NA, as.numeric(df.alt$num.mkts[ix]) - as.numeric(df.alt$num.mkts[jx])),
          ij.diff.num.mmc.comps = ifelse(any(is.missing(df.alt$num.mmc.comps[ix]), is.missing(df.alt$num.mmc.comps[jx])), NA, as.numeric(df.alt$num.mmc.comps[ix]) - as.numeric(df.alt$num.mmc.comps[jx])),
          ij.diff.constraint = as.numeric(df.alt$constraint[ix]) - as.numeric(df.alt$constraint[jx]),
          ij.diff.acqs = as.numeric(df.alt$acqs[ix]) - as.numeric(df.alt$acqs[jx]),
          ###------  network synergies ------
          ij.syn.pow.n1 = (cf.pow.n1 - pow.n1) / pow.n1,
          ij.syn.pow.n2 = (cf.pow.n2 - pow.n2) / pow.n2,
          ij.syn.pow.n3 = (cf.pow.n3 - pow.n3) / pow.n3,
          # ij.syn.num.mmc.comps = (cf.num.mmc.comps - df.alt$num.mmc.comps[ix]) / df.alt$num.mmc.comps[ix],
          ij.syn.closeness = (cf.closeness - df.alt$closeness[ix]) / df.alt$closeness[ix],
          ij.syn.degree = (cf.degree - df.alt$deg[ix]) / df.alt$deg[ix],
          ij.syn.constraint = (cf.constraint  - df.alt$constraint[ix]) / df.alt$constraint[ix]
        )
        df.reg <- rbind(df.reg, df.tmp.dyad)
      }
    }
  }

  cat('done.\n')
  ##---------------------------------------

  
  ##=================================  
  ## NODE COLLAPSE update network
  ##---------------------------------
  cat(sprintf('node collapsing acquisition %s:\n',j))
  g.pd.nc <- acf$nodeCollapseGraph(g.pd.nc, acq.src.allpd[j,])
  g.full.pd.nc <- acf$nodeCollapseGraph(g.full.pd.nc, acq.src.allpd[j,])
  ## FLAG TO NOT RUN NODE COLLAPSE AT START OF NEXT LOOP SINCE IT WAS ALREADY PROCESSED HERE
  do.node.collapse <- FALSE
  
  ## save incrementally
  if (lidx %% 10 == 0) {   
    saveRDS(list(l=l,df.reg=df.reg), 
            file = file.path(.data_dir, sprintf("acqlogit_compnet_processed_acquisitions_synergies_list_%s_d%s.rds",name_i,d)))
  }
  
  gc()
  
} ## end loop



## final save
saveRDS(list(l=l,df.reg=df.reg), 
        file = file.path(.data_dir, sprintf("acqlogit_compnet_processed_acquisitions_synergies_list_%s_d%s.rds",name_i,d)))



## save regression dataframe to seprate table file
write.csv(df.reg, 
          file = file.path(.data_dir, sprintf("acqlogit_compnet_processed_acquisitions_synergies_df_%s_d%s.csv",name_i,d)),
          row.names = F)




