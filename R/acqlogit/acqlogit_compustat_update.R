
setwd("C:/Users/T430/Google Drive/PhD/Dissertation/competition networks/compnet2")
# .libPaths('C:/Users/T430/Documents/R/win-library/3.2')
library(parallel)
library(network, quietly = T)
library(texreg, quietly = T)
library(igraph, quietly = T)
library(plyr, quietly = T)
library(lattice, quietly = T)
library(latticeExtra, quietly = T)
library(ggplot2, quietly = T)
library(reshape2)

data_dir <- "C:/Users/T430/Google Drive/PhD/Dissertation/crunchbase/"

## LOAD Scripts and Data
acf <- source(file.path(getwd(),'R','acqlogit','acqlogit_compnet_functions.R'))$value ## FUNCTIONS 
cb  <- source(file.path(getwd(),'R','acqlogit','acqlogit_cb_data_prep.R'))$value      ## DATA 

is.missing <- function(x)
{
  if(is.null(x)) 
    return(TRUE)
  return(is.na(x) | is.nan(x) | x == '')
}

##====================
## SETTINGS
##--------------------
name_i <- 'ibm'
d <- 2
years <- 2007:2017


##=======================================
##  PREP DATA and LOAD GRAPH
##---------------------------------------
## comptetition network
g.full <- read.graph('g_full.graphml', format='graphml')

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


##==========================================
## CHECK EGO FIRM NETWORK PROPERTIES 
##------------------------------------------
# # 
# # ## filter acquisitions in recent period (if necessary)
# # co_acq_d <- co_acq[which(co_acq$acquired_on >= '1988-01-01'), ]
# # 
# # ## acquisition filtered by comp net firms union (either aquirer or acquired)
# # ## 14208
# # co_acq_g_u <- co_acq[which(co_acq$acquirer_name_unique %in% V(g.full)$name
# #                            | co_acq$acquiree_name_unique %in% V(g.full)$name), ]
# 
# ## acquisition filtered by comp net firms intersection (both aquirer and acquired)
# ## 3442
# co_acq_g_i <- co_acq[which(co_acq$acquirer_name_unique %in% V(g.full)$name
#                            & co_acq$acquiree_name_unique %in% V(g.full)$name), ]
# ## acquisition filtered by comp net firms intersection, recent date (>=2010)
# ## 2786
# co_acq_g_i_d <- co_acq_g_i[which(co_acq_g_i$acquired_on >= '1988-01-01'), ]
# 
# ## check top filtered acquirers
# cnt <- plyr::count(co_acq_g_i_d$acquirer_name_unique)
# cnt <- cnt[order(cnt$freq, decreasing = T),]
# head(cnt, 20)
# # Top Acquirers:             x   freq
# # 545                   google   88
# # 599                      ibm   58
# # 815                microsoft   47
# # 1496                   yahoo   46
# # 77                       aol   43
# # 944                   oracle   39
# # 64                    amazon   34
# # 88                     apple   32
# # 453                 facebook   31
# # 403                     ebay   28
# # 259                    cisco   27
# # 1109              salesforce   26
# # 1362                 twitter   24
# # 578          hewlett-packard   21
# # 414                      emc   19
# # 640                    intel   18
# # 556                  groupon   17
# # 421  endurance-international   16
# # 1250                symantec   15
# # 1347             tripadvisor   15
# 
# ## Check Acquisitions distribution by network distance included
# df.ego <- data.frame()
# name_i <- 'ibm'
# for (d in 1:6) {
#   g.ego <- igraph::make_ego_graph(graph = g.full, 
#                                   nodes = V(g.full)[V(g.full)$name==name_i], 
#                                   order = d, mode = 'all')[[1]]
#   mem <- igraph::multilevel.community(g.ego)$membership
#   mem.cnt <- plyr::count(mem)
#   mem.cnt <- mem.cnt[order(mem.cnt$freq, decreasing = T), ]
#   dim(mem.cnt)
#   ##  
#   acq.src <- co_acq_d[which(co_acq_d$acquirer_name_unique %in% V(g.ego)$name), ]
#   acq.src.trg <- co_acq_d[which(co_acq_d$acquirer_name_unique %in% V(g.ego)$name
#                                 & co_acq_d$acquiree_name_unique %in% V(g.ego)$name), ]
#   ##
#   df.tmp <- data.frame(d=d,v=vcount(g.ego),e=ecount(g.ego),
#                        acq.src=nrow(acq.src),acq.src.trg=nrow(acq.src.trg),
#                        r.in.out=round(nrow(acq.src.trg)/(nrow(acq.src)+nrow(acq.src.trg)),3),
#                        first.comp=min(E(g.ego)$relation_began_on))
#   df.ego <- rbind(df.ego, df.tmp)
# }; df.ego 


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


# ## FIRST TIME: PROCESS NODE COLLAPSE OF ACQUISITIONS BEFORE START OF TIMEFRAME
# acqs.init <- co_acq[co_acq$acquired_on < start, ]
# g.pd <- acf$nodeCollapseGraph(g.pd, acqs.init)  #remove.isolates ?
# g.full.pd <- acf$nodeCollapseGraph(g.full.pd, acqs.init, verbose = TRUE)  ## remove.isolates

# ## SAVE INITIALIZED EGO NETWORK AND GLOBAL NETWORK
# igraph::write.graph(g.pd, file=sprintf('g_%s_d%s_NCINIT_%s_%s.rds',name_i,d,start,end), format = 'graphml')
# igraph::write.graph(g.full.pd, file=sprintf('g_full_NCINIT_%s_%s.rds',start,end), format = 'graphml')

## NOT FIRST TIME:  LOAD IN EGO NETWORK AND GLOBAL NETWORK
g.pd <- igraph::read.graph(sprintf('g_%s_d%s_NCINIT_%s_%s.rds',name_i,d,start,end), format='graphml')
g.full.pd <- igraph::read.graph(sprintf('g_full_NCINIT_%s_%s.rds',start,end), format='graphml')


## Full timeframe Clusters
V(g.pd)$nc <- as.integer(igraph::multilevel.community(g.pd)$membership)
V(g.full.pd)$nc <- as.integer(igraph::multilevel.community(g.full.pd)$membership)

## keep original timeframe graph
g.pd.orig <- g.pd
g.full.pd.orig <- g.full.pd
##----------------------------------






##----------------------------------
## YEAR PERIODS: DEFINE NICHE CLUSTERS
l <- list()
df.mmc <- data.frame()
df.rem <- data.frame()
df.reg <- data.frame()
lidx <- 0  ## acquisition list index
timeval <- timeval.last <- 0



## GET ALL ACQ EVENT VERTICES 
## keep only acquisitions with acquirer in ego network and target in global competition network
acq.src <- co_acq[ co_acq$acquirer_name_unique %in% V(g.pd)$name & co_acq$acquiree_name_unique %in% V(g.full.pd)$name, ]
acq.src.allpd <- acq.src[ acq.src$acquired_on >= start & acq.src$acquired_on < end , ]
acq.src.allpd <- acq.src.allpd[order(acq.src.allpd$acquired_on, decreasing = F), ]


##===============================
##
##  UPDATE COMPUSTAT DATA FOR MARKET-to-BOOK VALUE
##
##-------------------------------


## 2. COMPUSTAT　　*** SLOW TO LOAD ***
csa <- cb$readCsv(file = file.path('compustat','fundamentals-annual.csv'), na.strings = c(NA,'','NA'))
dim(csa)
names(csa)


## SELECT COLUMNS FROM COMPUSTAT
cols <- c('conm','conml','gvkey','datadate','fyear','indfmt','consol','popsrc','tic','cusip',
          'act', ## total assets  (ln for size proxy)
          'che', ## cash and short term investments (scale by total assets for cash holdings)
          'emp', ## employees (ln employee size proxy) 
          'ebitda', ## ebidta (scale by total assets for ROA performance proxy)
          'prcc_c', ## close market price at calendar year
          'csho', ## shares outstanding  (PRCC_C x CSHO = market value of equity)
          'seq', ## stockholder equity
          'ceq', ## total common equity
          'pstkrv', ## preferred stock redemption value
          'pstkl', ## preferred stock liquidation 
          'pstk', ## preferred stock par value
          'lt', ## Total Liabilities
          'mib', ## Minority Interest
          'txditc' ## balance sheet deferred taxes
)
## SUBSET COMPUSTAT
csa2 <- csa[,cols]

## DATA YEAR
csa2$datayear <- as.integer(str_sub(csa2$datadate,1,4))
## DATA MONTH
csa2$datamonth <- as.integer(str_sub(csa2$datadate,5,6))
## YEAR FOR BOOK VALUE USED IN BOOK-to-MARKET RATIO
##    if fiscal year ends in Jan-May, use previous year book value, else use current year
csa2$bookyear <- apply(csa2[,c('datayear','datamonth')], 1, function(x){
  y <- as.numeric(x[1])
  m <- as.numeric(x[2])
  return(ifelse( m < 6, y-1, y ))
})

##===========================
##
## COMPUTE M/B RATIO
##  @see https://wrds-www.wharton.upenn.edu/pages/support/applications/risk-and-valuation-measures/market-book-mb-ratio/
##
##---------------------------

## GET MARKET VAL OF EQUITY BY FISCAL YEAR
csa2$prcc_c_f <- NA  ## init price by firm fiscal year (previous year price if datayear ends in Jan-May)
conms <- sort(unique(csa2$conm))
for (j in 1:length(conms)) {
  conm <- conms[j]
  years <- unique(csa2$datayear[which(csa2$conm==conm & csa2$datayear<=2017)])
  if (j %% 10 == 0) cat(sprintf('%s years %s-%s (%.2f%s)\n',conm,min(years),max(years),100*j/length(conms),'%'))
  for (year in years) {
    i_set <- which(csa2$conm==conm & csa2$datayear==year) ## set price index
    if (length(i_set)==0) next
    i_get <- which(csa2$conm==conm & csa2$datayear==csa2$bookyear[i_set]) ## get price index
    if (length(i_get)==0) next
    csa2$prcc_c_f[i_set] <- csa2$prcc_c[i_get]
  }
}
## MARKET VALUE OF EQUITY -- USING CORRECT PRICE BY FIRM FISCAL YEAR
csa2$mcap_c <- csa2$prcc_c_f * csa2$csho

## STOCK HOLDER EQUITY algorithm
##   SHE = 1. `seq` if available, else
##         2. `ceq`+`pstk` if available, else
##         3. `act`-(`lt`+`mib`)
csa2names <- names(csa2)
# count <- 0 ## DEBUG `she` computation
csa2$she <- apply(csa2, 1, function(x){
  # count <<- count+1 ## DEBUG
  seq  <- as.numeric(unlist(x[which(csa2names=='seq')]))
  if (!is.na(seq)) {
    # cat(sprintf('row %s: seq\n',count)) ## DEBUG
    return(seq)
  }
  ceq  <- as.numeric(unlist(x[which(csa2names=='ceq')]))
  pstk <- as.numeric(unlist(x[which(csa2names=='pstk')]))
  if (!is.na(ceq) & !is.na(pstk)) {
    # cat(sprintf('row %s: ceq+pstk\n',count)) ## DEBUG
    return(ceq + pstk)
  }
  act  <- as.numeric(unlist(x[which(csa2names=='act')]))
  lt   <- as.numeric(unlist(x[which(csa2names=='lt')]))
  mib  <- as.numeric(unlist(x[which(csa2names=='mib')]))
  if (!is.na(act) & !is.na(lt) & !is.na(mib)) {
    # cat(sprintf('row %s: act-(lt+mib)\n',count)) ## DEBUG
    # cat(sprintf('act %s, lt %s, mib %s\n',act,lt,mib))
    return(act - (lt + mib))
  }
  return(NA)
})

## BOOK VALUE OF EQUITY
csa2$bve <- apply(csa2[,c('she','pstkrv','pstkl','pstk')], 1, function(x){
  she <- x[1]
  ps <- as.numeric( if(!is.na(x[2])){ x[2] }else if(!is.na(x[3])){ x[3] }else if(!is.na(x[4])){ x[4] }else{ 0 } )
  return(ifelse( is.na(she), NA, she-ps ))
})

## MARKET-to-BOOK RATIO
csa2$m2b <- apply(csa2[,c('mcap_c','bve')], 1, function(x){
  if (any(is.na(x))) return(NA)
  return(ifelse( x[2]==0, NA, x[1]/x[2] ))
})


## SAVE updated compustat data
write.csv(csa2, file=file.path('compustat','fundamentals-annual-UPDATED.csv'), row.names = F)

