##########################################################################################
#
# MMC Network & Acquisitions
#
# Create propensity scores for alternative acquirer-target pairings
#
# @author   sdowning.bm02g@nctu.edu.tw
#
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

# ## DIRECTORIES
.compustat_dir <- '/home/sdowning/data/compustat'


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
g.pd      <- acf$makePdGraph(g.ego, start, end, isolates.remove=FALSE)   ## ego network
g.full.pd <- acf$makePdGraph(g.full, start, end, isolates.remove=FALSE)  ## full network

## CHECK NETWORK PERIOD SIZES
sapply(2:length(times), function(i){gi=acf$makePdGraph(g.ego, times[i-1], times[i], TRUE); return(c(e=ecount(gi),v=vcount(gi)))})

##--------------------------------------------------
## FIRST TIME: PROCESS NODE COLLAPSE OF ACQUISITIONS BEFORE START OF TIMEFRAME
##--------------------------------------------------
g.pd.file <- file.path(.data_dir,sprintf('g_%s_d%s_NCINIT_%s_%s.graphml',name_i,d,start,end))
g.full.pd.file <- file.path(.data_dir,sprintf('g_full_NCINIT_%s_%s.graphml',start,end))
acqs.init <- co_acq[co_acq$acquired_on < start, ]
if (!file.exists(file.path(g.pd.file))) {
  cat(sprintf('preprocessing ego net acquisition before start of timeframe: t < %s ...', start))
  g.pd <- acf$nodeCollapseGraph(g.pd, acqs.init)  #remove.isolates ?
  igraph::write.graph(g.pd, file=g.pd.file, format = 'graphml')
  cat('done.\n')
}
if (!file.exists(file.path(g.full.pd.file))) {
  cat(sprintf('preprocessing global net acquisition before start of timeframe: t < %s ...', start))
  g.full.pd <- acf$nodeCollapseGraph(g.full.pd, acqs.init, verbose = TRUE)  ## remove.isolates
  igraph::write.graph(g.full.pd, file=g.full.pd.file, format = 'graphml')
  cat('done.\n')
}

##--------------------------------------------------
## AFTER FIRST TIME:  LOAD IN EGO NETWORK AND GLOBAL NETWORK
##--------------------------------------------------
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
  cat(sprintf('stop: cannot find csfunda.file %s\n',csfunda.file))
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



##===================================
##
## COMPUTE PROPOENSITY SCORES
##
##    Yearly propensity scores computation
##    node collapse update network once for all acquisition per year
##
##    TODO ***
##
##-----------------------------------
## At this point all acquisitions preceding first year are already node-collapsed
acqyrs <- sort(unique(acq.src.allpd$acquired_year))

g.prop.nc <- g.pd.orig            ## ego network to node collapse for propensity scores
g.full.prop.nc <- g.full.pd.orig  ## full netowrk to node collapse for propensity scores

a.df <- data.frame() ## acquirers df
t.df <- data.frame() ## targets df


# ##---- RELOAD FROM SAVED -------------
# l.prop <- readRDS(sprintf('acqlogit_propensity_score_comp_list_%s.rds',name_i))
# g.prop <- l.prop$g.prop
# g.full.prop <- l.prop$g.full.prop
# a.df <- l.prop$a.df
# t.df <- l.prop$t.df
# years <- years[which(years >= 2009)]
# ##------------------------------------

for (year in acqyrs) 
{
  cat(sprintf('\nyear %s\n\n',year))
  acq.yr <- acq.src.allpd[which(acq.src.allpd$acquired_year == year),]
  
  ##====================================
  ## YEAR PERIOD SUBSET NETWORK  
  ##------------------------------------
  ## Period network removes competitive relations ended < year OR started >= year+1
  g.prop <- asIgraph(acf$makePdNetwork(asNetwork(g.prop.nc), year, year+1, isolates.remove = F))
  g.full.prop <- asIgraph(acf$makePdNetwork(asNetwork(g.full.prop.nc), year, year+1, isolates.remove = F))
  V(g.prop)$name <- V(g.prop)$vertex.names
  V(g.full.prop)$name <- V(g.full.prop)$vertex.names
  
  ## GET FIRM x FRIM MMC MATRIX TO USE IN FM-MMC COMPUTATION
  m.mmc <- acf$getFirmFirmMmc(g.prop, as.integer(V(g.prop)$nc))
  
  ## Update MMC after acquisition
  mmc <- acf$getFmMmc(g.prop, as.integer(V(g.prop)$nc))
  
  ## MMC degree: number of mmc dyads linked to each firm i
  V(g.prop)$num.mmc.comps <- acf$getNumMmcRivalsByMembership(g.prop, as.integer(V(g.prop)$nc), m.mmc)
  
  ## SUM FM MMC over markets  ??????
  V(g.prop)$fm.mmc.sum <- rowSums(mmc)
  V(g.prop)$num.mkts <- apply(mmc, 1, function(x)length(x[x>0]))

  
  
  ##===================================
  ## LOOP EACH ACQUISITION IN YEAR t
  ##-----------------------------------
  for (i in 1:nrow(acq.yr)) 
  {
    acq.yr.i <- acq.yr[i,]
    cat(sprintf(' %s: %s --> %s\n',i,acq.yr.i$acquirer_name_unique,acq.yr.i$acquiree_name_unique))
    
    if ( ! acq.yr.i$acquiree_name_unique %in% V(g.full.prop)$name)
      next
    
    ##=====================================
    ## TARGETS
    ##-------------------------------------
    ## target alternative set vids
    targ.id <- which(V(g.full.prop)$name == acq.yr.i$acquiree_name_unique)
    if (length(targ.id)==0 | is.na(targ.id)) {
      next
    }
    targ.vids.d2 <- igraph::neighborhood(graph = g.full.prop, order = 2, nodes = targ.id)[[1]]
    targ.vids.d2 <- targ.vids.d2[which( !(names(targ.vids.d2) %in% V(g.full.prop)$name[targ.id]))]
    targ.vids.d1 <- igraph::neighborhood(graph = g.full.prop, order = 1, nodes = targ.id)[[1]]
    targ.vids.d1 <- targ.vids.d1[which( !(names(targ.vids.d1) %in% V(g.full.prop)$name[targ.id]))]
    cat('     targ.vids|') ## DEBUG
    ## Target alternatives dataframe
    df.targ.alt <- cb$co[which(cb$co$company_name_unique %in% c(names(targ.vids.d1),names(targ.vids.d2),V(g.full.prop)$name[targ.id])), ]
    df.targ.alt <- df.targ.alt[!is.na(df.targ.alt$company_name_unique),]
    df.targ.alt$d <- sapply(df.targ.alt$company_name_unique, function(x){ return(
      ifelse(x ==  V(g.full.prop)$name[targ.id], 0, 
             ifelse(x %in% names(targ.vids.d1), 1,   2))
    )})
    ## ipo status
    df.targ.alt$is.public <- sapply(1:nrow(df.targ.alt), function(x){
      isNotOperating <- df.targ.alt$status[x] != 'operating'
      ipo.date <- cb$co_ipo$went_public_on[which(cb$co_ipo$company_name_unique == df.targ.alt$company_name_unique[x])]
      ipo.date <- min(ipo.date, na.rm=TRUE)
      if (length(ipo.date)<1) 
        return(0)
      return(ifelse( isNotOperating & ipo.date <= acq.yr.i$acquired_on, 1, 0))
    })
    ## set NAs in is.public =0
    df.targ.alt$is.public[is.na(df.targ.alt$is.public)] <- 0
    ## target had IPO
    df.targ <- df.targ.alt[which(df.targ.alt$company_name_unique == V(g.full.prop)$name[targ.id]), ]
    
    if (nrow(df.targ) == 0)
      next
    
    ## filter by target neighborhood
    tmp <- df.targ.alt[df.targ.alt$company_name_unique %in% names(targ.vids.d2), ]
    ## select based on ownership status
    tmp <- tmp[tmp$is.public == df.targ$is.public, ]
    # tmp.alt <- tmp[sample(1:nrow(tmp),size = min(9,nrow(tmp)),replace = F), ]
    tmp.alt <- tmp  ## keep all alternative targets
    ## combine target and alternatives for target set
    df.targ.alt <- rbind(tmp.alt, df.targ)
    ## add MMC
    df.targ.alt$fm.mmc.sum <- sapply(df.targ.alt$company_name_unique, function(name){
      ifelse(name %in% V(g.prop)$name, V(g.prop)$fm.mmc.sum[which(V(g.prop)$name == name)] , NA)
    })
    df.targ.alt$num.mkts <- sapply(df.targ.alt$company_name_unique, function(name){
      ifelse(name %in% V(g.prop)$name, V(g.prop)$num.mkts[which(V(g.prop)$name == name)] , NA)
    })
    df.targ.alt$num.mmc.comps <- sapply(df.targ.alt$company_name_unique, function(name){
      ifelse(name %in% V(g.prop)$name, V(g.prop)$num.mmc.comps[which(V(g.prop)$name == name)] , NA)
    })
    df.targ.alt$acqs <- unname(sapply(df.targ.alt$company_name_unique, function(name){
      x <- length(which(cb$co_acq$acquirer_name_unique==name & cb$co_acq$acquired_on <= acq.yr.i$acquired_on))
      n <- length(which(cb$co_acq$acquirer_name_unique %in% V(g.full.prop)$name & cb$co_acq$acquired_on <= acq.yr.i$acquired_on))
      return(x/n)
    }))
    ## VENTURE FUNDING
    df.targ.alt$fund.v.cnt <- unname(sapply(df.targ.alt$company_name_unique, function(name){
      length(which(cb$co_rou$company_name_unique==name & cb$co_rou$announced_on <= acq.yr.i$acquired_on & cb$co_rou$funding_round_type=='venture'))
    }))
    df.targ.alt$fund.v.amt <- unname(sapply(df.targ.alt$company_name_unique, function(name){
      idx <- which(cb$co_rou$company_name_unique==name & cb$co_rou$announced_on <= acq.yr.i$acquired_on & cb$co_rou$funding_round_type=='venture')
      return(sum(cb$co_rou$raised_amount_usd[idx], na.rm = T))
    }))
    ## ALL FUNDING
    df.targ.alt$fund.cnt <- unname(sapply(df.targ.alt$company_name_unique, function(name){
      length(which(cb$co_rou$company_name_unique==name & cb$co_rou$announced_on <= acq.yr.i$acquired_on))
    }))
    df.targ.alt$fund.amt <- unname(sapply(df.targ.alt$company_name_unique, function(name){
      idx <- which(cb$co_rou$company_name_unique==name & cb$co_rou$announced_on <= acq.yr.i$acquired_on)
      return(sum(cb$co_rou$raised_amount_usd[idx], na.rm = T))
    }))
    # ## USE GLOBAL NETWORK for DEGREE
    # df.targ.alt$deg <- sapply(df.targ.alt$company_name_unique, function(name){
    #   ifelse(name %in% V(g.prop)$name, igraph::degree(g.full.prop,which(V(g.full.prop)$name==name)) , NA)
    # })
    cat('end_targs|') ## DEBUG
    
    ##-------------------------------------
    ##  FILTER ALTERNATIVE TARGETS BY CONDITIONS
    ##-------------------------------------
    ## target's categories
    cats.j <- str_split(cb$co$category_group_list[cb$co$company_name_unique==acq.yr.i$acquiree_name_unique], pattern = '[|]')[[1]]
    ## CHECK MACHING CONDITIONS FOR ALTERNATIVE TARGETS (NOT ACTUAL)
    bool.t.j <- sapply(df.targ.alt$company_name_unique, function(xj){
      #if (is.na(xj)) {
      #  cat(sprintf('DEBUG qsub_1: target bool.t.j: xj=%s, acq.yr.i$acquiree_name_unique=%s\n',xj,acq.yr.i$acquiree_name_unique))
      #  cat(sprintf('company_name_unique string %s\n', paste(df.targ.alt$company_name_unique,collapse="|")))
      #  print(df.targ.alt)
      #}
      ## ACTUAL ACQUIRER
      if (is.na(xj) | length(xj)==0 | length(acq.yr.i$acquiree_name_unique)==0)
        return(FALSE)
      if (xj == acq.yr.i$acquiree_name_unique)
        return(FALSE)
      ## CHECK 0. ALREADY FILTERED df.targ.alt BY COMPETITION NETWORK NEIGHBORHOOD
      ## CHECK 1. AT LEAST ONE SAME CATEGORY
      cats.xj <- str_split(cb$co$category_group_list[cb$co$company_name_unique==xj], pattern = '[|]')[[1]]
      chk1 <- any(cats.xj %in% cats.j)
      ## CHECK 2. WAS ACQUISITION TARGET IN NEXT 5 YEARS
      dt0 <- as.character(acq.yr.i$acquired_on)
      parts <- str_split(dt0,'[-]')[[1]]
      dt5 <- sprintf('%04d-%s-%s',as.numeric(parts[1]) + 5,parts[2],parts[3])
      chk2 <- any(cb$co_acq$acquired_on >= dt0 & cb$co_acq$acquired_on < dt5 & cb$co_acq$acquiree_name_unique==xj)
      return(all(chk1,chk2))
    })
    if (length(bool.t.j)==0)
      next  ## SKIP IF NO ALTERNATIVES OPTIONS
    idx.t.j <- which(bool.t.j)
    if (length(idx.t.j) == 0)
      next  ## SKIP IF NO MATCHED ALTERNATIVES
    ## ADD ACTUAL TARGET INDEX
    idx.t.j <- c(idx.t.j, which(df.targ.alt$company_name_unique == acq.yr.i$acquiree_name_unique))
    ## ACQUISITION UUID
    df.targ.alt$acquisition_uuid <- acq.yr.i$acquisition_uuid

    cat('filter_targs|') ## DEBUG

    ##======================================
    ## ACQUIRER ALTERNATIVES SET
    ##--------------------------------------
    ## acquirer alternative set vids
    acq.id <- which(V(g.prop)$name == acq.yr.i$acquirer_name_unique)
    if (length(acq.id)==0 | is.na(acq.id)) {
      next
    }    
    cat(sprintf('|acq.id=%s|',paste(acq.id,collapse='_')))

    acq.vids.d2 <- igraph::neighborhood(graph = g.prop, order = 2, nodes = acq.id)[[1]]
    cat(sprintf('|acq.vids.d2=%s|',paste(acq.vids.d2,collapse='_')))

    acq.vids.d2 <- acq.vids.d2[which( !(names(acq.vids.d2) %in% V(g.prop)$name[acq.id]))]
    cat(sprintf('|acq.vids.d2r=%s|',paste(acq.vids.d2,collapse='_')))

    acq.vids.d1 <- igraph::neighborhood(graph = g.prop, order = 1, nodes = acq.id)[[1]]
    cat(sprintf('|acq.vids.d1=%s|',paste(acq.vids.d1,collapse='_')))

    acq.vids.d1 <- acq.vids.d1[which( !(names(acq.vids.d1) %in% V(g.prop)$name[acq.id]))]
    cat(sprintf('|acq.vids.d1r=%s|',paste(acq.vids.d1,collapse='_')))

    cat('|acq.vids|') ##DEBUG
    ## acquirer alternatives dataframe
    # length(acq.vids.d1)
    df.acq.alt <- cb$co[which(cb$co$company_name_unique %in% c(names(acq.vids.d1),names(acq.vids.d2),V(g.prop)$name[acq.id])), ]
    df.acq.alt <- df.acq.alt[!is.na(df.acq.alt$company_name_unique), ]
    df.acq.alt$d <- sapply(df.acq.alt$company_name_unique, function(x){ return(
      ifelse(x ==  V(g.prop)$name[acq.id], 0, 
             ifelse(x %in% names(acq.vids.d1), 1,   2))
    )})
    ## ipo status
    df.acq.alt$is.public <- sapply(1:nrow(df.acq.alt), function(x){
      isNotOperating <- df.acq.alt$status[x] != 'operating'
      ipo.date <- cb$co_ipo$went_public_on[which(cb$co_ipo$company_name_unique == df.acq.alt$company_name_unique[x])]
      ipo.date <- min(ipo.date, na.rm=TRUE)
      if (length(ipo.date)<1) 
        return(0)
      return(ifelse( isNotOperating & ipo.date <= acq.yr.i$acquired_on, 1, 0))
    })
    ## set NAs in is.public =0
    df.acq.alt$is.public[is.na(df.acq.alt$is.public)] <- 0
    ## target had IPO
    df.acq <- df.acq.alt[which(df.acq.alt$company_name_unique == V(g.prop)$name[acq.id]), ]
    
    
    ###DEBUG###
    cat('df.acq_saved_rds_debug\n') ##DEBUG
    saveRDS(list(df.acq=df.acq, df.targ.alt=df.targ.alt, df.acq.alt=df.acq.alt, acq.yr.i=acq.yr.i),
          file = file.path(.data_dir, sprintf('DEBUG_acqlogit_propensity_score_dfacq_list_%s_d%s.rds',name_i,d)))
    ###########   
 
    if (nrow(df.acq) == 0)
      next
    if (nrow(df.acq) > 1)
      stop(sprintf('error: nrow df.acq %s > 1',nrow(df.acq)))
    
    tmp <- df.acq.alt[df.acq.alt$company_name_unique %in% names(acq.vids.d2), ]
    ## select based on ownership status
    if (df.acq$is.public == 1) {tmp <- tmp[tmp$is.public == 1,]} else {tmp <- tmp[tmp$is.public == 0,]}
    # tmp.acq.alt <- tmp[sample(1:nrow(tmp),size = min(9,nrow(tmp)),replace = F), ]
    tmp.acq.alt <- tmp ## keep all
    ## combine target and alternatives for target set
    df.acq.alt <- rbind(tmp.acq.alt, df.acq)
    ## set MMC
    df.acq.alt$fm.mmc.sum <- sapply(df.acq.alt$company_name_unique, function(name){
      ifelse(name %in% V(g.prop)$name, as.numeric(V(g.prop)$fm.mmc.sum[which(V(g.prop)$name == name)]) , NA)
    })
    df.acq.alt$num.mkts <- sapply(df.acq.alt$company_name_unique, function(name){
      ifelse(name %in% V(g.prop)$name, as.numeric(V(g.prop)$num.mkts[which(V(g.prop)$name == name)]) , NA)
    })
    df.acq.alt$num.mmc.comps <- sapply(df.acq.alt$company_name_unique, function(name){
      ifelse(name %in% V(g.prop)$name, as.numeric(V(g.prop)$num.mmc.comps[which(V(g.prop)$name == name)]) , NA)
    })
    df.acq.alt$acqs <- unname(sapply(df.acq.alt$company_name_unique, function(name){
      x <- length(which(cb$co_acq$acquirer_name_unique==name & cb$co_acq$acquired_on <= acq.yr.i$acquired_on))
      n <- length(which(cb$co_acq$acquirer_name_unique %in% V(g.full.prop)$name & cb$co_acq$acquired_on <= acq.yr.i$acquired_on))
      return(x/n)
    }))
    # ## USE GLOBAL NETWORK
    # df.acq.alt$deg <- sapply(df.acq.alt$company_name_unique, function(name){
    #   ifelse(name %in% V(g.prop)$name, igraph::degree(g.full.prop,which(V(g.full.prop)$name==name)) , NA)
    # })
    
    ##-------------------------------------
    ##  FILTER ALTERNATIVE ACQUIRERS BY CONDITIONS
    ##-------------------------------------
    ## target's categories
    cats.i <- str_split(cb$co$category_group_list[cb$co$company_name_unique==acq.yr.i$acquirer_name_unique], pattern = '[|]')[[1]]
    ## CHECK MACHING CONDITIONS FOR ALTERNATIVE ACQUIRERS (NOT ACTUAL)
    bool.t.i <- sapply(df.acq.alt$company_name_unique, function(xi){
      if (is.na(xi))
        return(FALSE)
      ## ACTUAL ACQUIRER
      if (xi == acq.yr.i$acquirer_name_unique)
        return(FALSE)
      ## CHECK 0. ALREADY FILTERED df.targ.alt BY COMPETITION NETWORK NEIGHBORHOOD
      ## CHECK 1. AT LEAST ONE SAME CATEGORY
      cats.xi <- str_split(cb$co$category_group_list[cb$co$company_name_unique==xi], pattern = '[|]')[[1]]
      chk1 <- any(cats.xi %in% cats.i)
      ## CHECK 2. WAS ACQUISITION TARGET IN NEXT 5 YEARS
      dt0 <- as.character(acq.yr.i$acquired_on)
      parts <- str_split(dt0,'[-]')[[1]]
      dt5 <- sprintf('%04d-%s-%s',as.numeric(parts[1]) + 5,parts[2],parts[3])
      chk2 <- any(cb$co_acq$acquired_on >= dt0 & cb$co_acq$acquired_on < dt5 & cb$co_acq$acquirer_name_unique==xi)
      return(all(chk1,chk2))
    })
    if (length(bool.t.i)==0)
      next
    idx.t.i <- which(bool.t.i)
    if (length(idx.t.i)==0)
      next  ## SKIP IS NO ALTERNATIVE ACQUIRERS
    ## ADD ACTUAL ACQUIRER INDEX
    idx.t.i <- c(idx.t.i, which(df.acq.alt$company_name_unique == acq.yr.i$acquirer_name_unique))
    ## ACQUISITION UUID
    df.acq.alt$acquisition_uuid <- acq.yr.i$acquisition_uuid
    
    ## APPEND COUNTERFACTUALS FOR ACQUIRER AND TARGET SETS
    cat('   appending counterfactuals.\n')
    a.df <- rbind(a.df, df.acq.alt[idx.t.i,])
    t.df <- rbind(t.df, df.targ.alt[idx.t.j,])
    
  }

  ## NOTE: DO NOT NEED TO NODE COLLAPSE FOR PROPENSITY SCORE PREDICTORS (not using degree)
  ## ## NODE COLLAPSE  THE NC GRAPHS
  ## g.prop.nc <- acf$nodeCollapseGraph(g.prop.nc, acquisitions = acq.yr)
  ## g.full.prop.nc <- acf$nodeCollapseGraph(g.full.prop.nc, acquisitions = acq.yr)

  ## CACHE DATA
  saveRDS(list(g.prop=g.prop, g.full.prop=g.full.prop, 
               g.prop.nc=g.prop.nc, g.full.prop.nc=g.full.prop.nc,
               a.df=a.df, t.df=t.df),
          file = file.path(.data_dir, sprintf('acqlogit_propensity_score_comp_list_%s_d%s.rds',name_i,d)))
  
}


# ### PATCH: FILL IN full competition network competitors 
# t.df$deg2 <- NA
# for (i in 1:nrow(t.df)) {
#   t.df$deg2[i] <- unname(igraph::degree(g.full.pd.orig, which(V(g.full.pd.orig)$name==t.df$company_name_unique[i])))
# }
# a.df$deg2 <- NA
# for (i in 1:nrow(a.df)) {
#   a.df$deg2[i] <- unname(igraph::degree(g.full.pd.orig, which(V(g.full.pd.orig)$name==a.df$company_name_unique[i])))
# }
# ## PATCH TARGET FUNDING
# t.df$fund.v.cnt <- NA
# t.df$fund.v.amt <- NA
# uuids <- unique(t.df$acquisition_uuid)
# for (uuid in uuids) {
#   cat(sprintf('%s (%.2f%s)\n',uuid,100*which(uuids==uuid)/length(uuids),'%'))
#   tmp <- t.df[which(t.df$acquisition_uuid==uuid), ]
#   tmpdate <- tmp$acquired_on[which(tmp$d==0)]
#   for (i in which(t.df$acquisition_uuid == uuid)) {
#     name <- t.df$company_name_unique[i]
#     idx <- which(cb$co_rou$company_name_unique==name & cb$co_rou$announced_on <= tmpdate & cb$co_rou$funding_round_type=='venture')
#     t.df$fund.v.cnt[i] <- length(idx)
#     t.df$fund.v.amt[i] <- sum(cb$co_rou$raised_amount_usd[idx], na.rm = T)
#   }
# }
# 
# 
# saveRDS(list(g.prop=g.prop,g.full.prop=g.full.prop,a.df=a.df,t.df=t.df),
#         file = sprintf('acqlogit_propensity_score_comp_list_%s_PATCH.rds',name_i))


##=============================
## TARGET PROPENSITIES
##-----------------------------
## tmp
prop.data <- t.df
prop.data$y <- as.integer(prop.data$d == 0)
prop.data$age <- 2018 - prop.data$founded_year
# tmp <- cb$co_acq[,c('acquisition_uuid','acquired_on')]
# names(tmp) <- c('acquisition_uuid','acquisition_date')
# prop.data <- merge(prop.data, tmp, by='acquisition_uuid',all.x=T,all.y=F)
# prop.data$acquisition_year_f <- factor(sapply(prop.data$acquisition_date,function(x)str_sub(x,1,4)))
## fit
prop.fit <- glm(y ~  age + I(age^2) + acqs + fund.v.cnt + I(fund.v.amt/1e7), family=binomial(link='probit'), data=prop.data)
prop.data$pred <- predict.glm(prop.fit, prop.data,type = 'response')
## TARGET PROPENSITY SCORE DATA
t.prop <- prop.data

## check proportion of predicted values
dim(t.prop[which(!is.na(t.prop$pred)),])[1] / dim(t.prop)[1]

## CHECK TARGET PROPENSITY MODEL PERFORMANCE
check.prop <- t.prop
uuids <- unique(t.prop$acquisition_uuid) 
df.check <- data.frame()
for (uuid in uuids) {
  tmp <- check.prop[which(check.prop$acquisition_uuid==uuid),]
  tmp <- tmp[order(tmp$pred, decreasing = T), ]
  df.check <- rbind(df.check, data.frame(uuid=uuid,n=nrow(tmp),r=which(tmp$y==1)))
}
df.check$p <- (df.check$n - df.check$r + 1) / df.check$n 
# hist(df.check$p,main='1 = top propensity (good); 0 = bottom propensity (bad)')
cat(sprintf('Proportion of events in top 2:  %.3f\n',length(which(df.check$r<=2))/nrow(df.check)))
cat(sprintf('Proportion of events in top 3:  %.3f\n',length(which(df.check$r<=3))/nrow(df.check)))



##=============================
## ACQUIRER PROPENSITIES
##-----------------------------
## MERGE ACQUIRER COMPUSTAT FINANCIALS FOR YEAR (t-1)
ctrl.col <- c('company_name_unique','datayear','act','emp','ebitda','m2b','che')
a.df.ctrl <- data.frame()
for (year in acqyrs)
{
  yr.uuids <- cb$co_acq$acquisition_uuid[which(cb$co_acq$acquired_year==year)]
  df.yr <- a.df[which(a.df$acquisition_uuid %in% yr.uuids), ]
  ctrl.idx <- which(df.cs$datayear == year-1 )
  df.yr <- merge(df.yr, df.cs[ctrl.idx,ctrl.col], by.x='company_name_unique',by.y='company_name_unique',all.x=T,all.y=F)
  df.yr$ln_asset <- log(df.yr$act)
  df.yr$ln_emp <- log(df.yr$emp)
  df.yr$roa <- df.yr$ebitda / df.yr$act
  df.yr$cash <- df.yr$che / df.yr$act
  a.df.ctrl <- rbind(a.df.ctrl, df.yr)
}

# ## Propensity Score ranked selection
prop.data <- a.df.ctrl
prop.data$y <- as.integer(prop.data$d == 0)
prop.data$age <- 2018 - prop.data$founded_year
## FIT
prop.fit <- glm(y ~ ln_asset + roa + cash + m2b, 
                family=binomial(link='probit'), data=prop.data[which(!is.na(prop.data$m2b)),])
prop.data$pred <- predict.glm(prop.fit, prop.data, type = 'response')
## ACQUIRER PROPENSITY SCORE DATA
a.prop <- prop.data

## check proportion of predicted values
dim(a.prop[which(!is.na(a.prop$pred)),])[1] / dim(a.prop)[1]


## CHECK ACQUIRER PROPENSITY MODEL PERFORMANCE
check.prop <- a.prop
uuids <- unique(t.prop$acquisition_uuid) 
df.check <- data.frame()
for (uuid in uuids) {
  tmp <- check.prop[which(check.prop$acquisition_uuid==uuid),]
  tmp <- tmp[order(tmp$pred, decreasing = T), ]
  df.check <- rbind(df.check, data.frame(uuid=uuid,n=nrow(tmp),r=which(tmp$y==1)))
}
df.check$p <- (df.check$n - df.check$r + 1) / df.check$n 
# hist(df.check$p,main='1 = top propensity (good); 0 = bottom propensity (bad)')
cat(sprintf('Proportion of events in top 2:  %.3f\n',length(which(df.check$r<=2))/nrow(df.check)))
cat(sprintf('Proportion of events in top 3:  %.3f\n',length(which(df.check$r<=3))/nrow(df.check)))



## SAVE LIST OF PROPENSITY SCORE DATAFRAMES
saveRDS(list(g.prop=g.prop, g.full.prop=g.full.prop, 
             g.prop.nc=g.prop.nc, g.full.prop.nc=g.full.prop.nc,
             a.df=a.df, t.df=t.df, a.df.ctrl=a.df.ctrl, a.prop=a.prop, t.prop=t.prop),
        file = file.path(.data_dir, sprintf('acqlogit_propensity_score_comp_list_%s_d%s_ctrl.rds',name_i,d)))



