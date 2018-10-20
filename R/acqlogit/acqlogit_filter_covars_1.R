##--------------------------------
## NETWORK MMC Acquisition FILTER COVARIATES
##
##-------------------------------

setwd("C:/Users/T430/Google Drive/PhD/Dissertation/competition networks/compnet2")
# .libPaths('C:/Users/T430/Documents/R/win-library/3.2')
library(texreg, quietly = T)
library(plyr, quietly = T)
library(lattice, quietly = T)
library(latticeExtra, quietly = T)
library(ggplot2, quietly = T)
library(reshape2)
library(mlogit)
library(mclogit)
library(lme4)
library(lmerTest)
library(lubridate)

data_dir <- "C:/Users/T430/Google Drive/PhD/Dissertation/crunchbase/"

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}




##============================================
## load data 
##--------------------------------------------
## CrunchBase data
cb  <- source(file.path(getwd(),'R','acqlogit','acqlogit_cb_data_prep.R'))$value      ## DATA 

## Regression dataframe
name_i <- 'ibm'
data.in <- readRDS(sprintf("acqlogit_compnet_covs_list_%s.rds",name_i))
l <- data.in$l
df.reg <- data.in$df.reg

##============================================
##  FILTER ALTERNATIVES 
##   1. Acquirer: same category, made acquisition within next 5 years
##   2. Target:   same category, was acquired within next 5 years 
##
##--------------------------------------------
cat('\nfiltering acglogit regression dataframe by matched sample conditions...\n')

df.filter <- data.frame()
ts <- unique(df.reg$t)

for (t in ts) 
{
  dft <- df.reg[df.reg$t==t,]
  ## Actual acquirer i and target j company_name_unique
  firm.i <- as.character(dft$i[dft$y==1])
  firm.j <- as.character(dft$j[dft$y==1]) 
  
  ## SKIP ACQUISITION IF ACTUAL EVENT IS MISSING FROM DATAFRAME
  if (length(firm.i)==0) next
  if (length(firm.j)==0) next
  
  ##-------------------------------------
  ## 1. filter ACQUIRER alternatives
  ##-------------------------------------
  ## alt acquirers (i != firm.i) for the real target
  dft.i <- dft[dft$i!=firm.i & dft$j==firm.j, ]
  ## acquirer's categories
  cats.i <- str_split(cb$co$category_group_list[cb$co$company_name_unique==firm.i], pattern = '[|]')[[1]]
  ## CHECK MACHING CONDITIONS FOR ALTERNATIVES
  bool.t.i <- sapply(as.character(dft.i$i), function(xi){
      ## CHECK 1. AT LEAST ONE SAME CATEGORY
      cats.xi <- str_split(cb$co$category_group_list[cb$co$company_name_unique==xi], pattern = '[|]')[[1]]
      chk1 <- any(cats.xi %in% cats.i)
      ## CHECK 2. MADE ACQUISITION IN NEXT 5 YEARS
      dt0 <- as.character(dft.i$date[dft.i$i==xi & dft.i$j==firm.j])
      parts <- str_split(dt0,'[-]')[[1]]
      dt5 <- sprintf('%04d-%s-%s',as.numeric(parts[1]) + 5,parts[2],parts[3])
      chk2 <- any(cb$co_acq$acquired_on >= dt0 & cb$co_acq$acquired_on < dt5 & cb$co_acq$acquirer_name_unique==xi)
      return(all(chk1,chk2))
    })
  if (length(bool.t.i)==0)
    next
  idx.t.i <- which(bool.t.i)
  ##----------------------------------------------------------
  ## RANDOMLY SAMPLE 5 alternatives 
  ##   TODO: REPLACE BY PROBIT MODEL FOR TOP PROPENSITY SCORES
  set.seed(1111)
  size <- min(5, length(idx.t.i))
  alt.firm.i <- as.character( dft.i$i[ sample(idx.t.i, size = size, replace=F) ] )
  ##----------------------------------------------------------
  ## DATA SAMPLE OF ALL ACQUIRERS (REAL + 5 ALTERNATIVES)
  dft.i.sub <- dft[dft$i %in% alt.firm.i & dft$j==firm.j, ]
  
  ## SKIP IS ALTERNATIVES SET HAS NO ALTERNTIVES
  if (nrow(dft.i.sub) < 1)
    next
  
  ##-------------------------------------
  ## 2. filter TARGET alternatives
  ##-------------------------------------
  ## alt targets (j != firm.j) for the real acquirer
  dft.j <- dft[dft$i==firm.i & dft$j!=firm.j, ]
  ## target's categories
  cats.j <- str_split(cb$co$category_group_list[cb$co$company_name_unique==firm.j], pattern = '[|]')[[1]]
  ## CHECK MACHING CONDITIONS FOR TARGETS
  bool.t.j <- sapply(as.character(dft.j$j), function(xj){
      ## CHECK 1. AT LEAST ONE SAME CATEGORY
      cats.xj <- str_split(cb$co$category_group_list[cb$co$company_name_unique==xj], pattern = '[|]')[[1]]
      chk1 <- any(cats.xj %in% cats.j)
      ## CHECK 2. WAS ACQUISITION TARGET IN NEXT 5 YEARS
      dt0 <- as.character(dft.j$date[dft.j$i==firm.i & dft.j$j==xj])
      parts <- str_split(dt0,'[-]')[[1]]
      dt5 <- sprintf('%04d-%s-%s',as.numeric(parts[1]) + 5,parts[2],parts[3])
      chk2 <- any(cb$co_acq$acquired_on >= dt0 & cb$co_acq$acquired_on < dt5 & cb$co_acq$acquiree_name_unique==xj)
      return(all(chk1,chk2))
    })
  if (length(bool.t.j)==0)
    next
  idx.t.j <- which(bool.t.j)
  ##----------------------------------------------------------
  ## RANDOMLY SAMPLE 5 alternatives 
  ##   TODO: REPLACE BY PROBIT MODEL FOR TOP PROPENSITY SCORES (???)
  set.seed(1111)
  size <- min(5, length(idx.t.j))
  alt.firm.j <- as.character( dft.j$j[ sample(idx.t.j, size = size, replace=F) ] )
  ##----------------------------------------------------------
  ## DATA SAMPLE OF ALL ACQUIRERS (REAL + 5 ALTERNATIVES)
  dft.j.sub <- dft[dft$i==firm.i & dft$j %in% alt.firm.j, ]
  
  ## SKIP IS EITHER ALTERNATIVES SET HAS NO ALTERNTIVES
  if (nrow(dft.j.sub) < 1)
    next
  
  ## COMBINE ALTERNATIVES AND ACTUAL PAIRING FOR ACQUISTION t WITH OTHER ACQUISITIONS DATA SAMPLE
  df.filter <- rbind(df.filter, dft.i.sub, dft.j.sub, dft[dft$y==1,])
  
  if (t %% 1 == 0) cat(sprintf('%s date %s\n',t,as.character(dft$date[dft$y==1])))
}

## ECHO FILTERED DATAFRAME SUMMARY
print(dim(df.filter))
print(rbind(head(df.filter,3),tail(df.filter,3)))

## save FILTERED regression dataframe to seprate table file
write.csv(df.filter, file = sprintf("acqlogit_compnet_covs_df_FILTERED_%s.csv",name_i), row.names = F)

cat('\ndone.\n')


