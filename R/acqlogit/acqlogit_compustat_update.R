###
##
##  Update Compustat Fundamentals Annual Data
##   to subset columns for controls and add Market-to-Book ratio
##
###
#setwd("C:/Users/T430/Google Drive/PhD/Dissertation/competition networks/compnet2")
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

.cs.update <- function()
{

data_dir <- "C:/Users/T430/Google Drive/PhD/Dissertation/crunchbase/"

is.missing <- function(x)
{
  if(is.null(x)) 
    return(TRUE)
  return(is.na(x) | is.nan(x) | x == '')
}



##===============================
##
##  UPDATE COMPUSTAT DATA FOR MARKET-to-BOOK VALUE
##
##-------------------------------


##  COMPUSTAT　　*** SLOW TO LOAD ***
csa <- read.csv(file.path('compustat','fundamentals-annual.csv'), na.strings = c(NA,'','NA'), stringsAsFactors = F, fill = T)
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


##===========================
##
## COMPUTE M/B RATIO
##  @see https://wrds-www.wharton.upenn.edu/pages/support/applications/risk-and-valuation-measures/market-book-mb-ratio/
##
##---------------------------

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

}

## run
.cs.update()
