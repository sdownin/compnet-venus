##--------------------------------
## NETWORK MMC Acquisition selection rEGRESSION
#
## @see http://www.elff.eu/software/#mclogit
# The package 'mclogit' implements the estimation of mixed conditional logit models via the PQL method as used in my article published in Electoral Studies. It is published on CRAN. Development occurs on GitHub, where both releases and the development tree can be found.
# 
# The probability that individual i chooses alternative j from choice set Si is
# ??ij=expa(??ij)???k???Siexpa(??ik)
# with
# ??ij=??1x1ij+???+??qxqij+Uij
# where xhij are values of independent variables, ??h are parameters (coefficients), and Uij are random effects with a normal distribution.
# 
# The package allows to specify that random effects are equal for all individuals within clusters Cc, that is Ui1j=Ui2j for i1???Cc and i2???Cc, where such clusters also may be nested in a "multi-level" manner.
# 
# The "dependent variable" yij may be a "dummy variable" that is equal to 1 if individual i has chosen alternative j and 0 if s/he has chosen another alternative. For example, if all individuals i face the same set of five alternatives, then five values of the dependent variable would correspond to each individual with only one of the values being equal to one and the other four values being equal to zero. (This is sometimes called that the data are in "stacked" format.)
# 
# Also, if "covariate classes" of individuals are formed that share the same values of the independent variables and are members of the same cluster, and i indicates such a covariate class, yij may be the count of individuals from covariate class i that have chosen alternative j.
##-------------------------------

setwd("C:/Users/T430/Google Drive/PhD/Dissertation/competition networks/compnet2")
# .libPaths('C:/Users/T430/Documents/R/win-library/3.2')
library(texreg, quietly = T)
library(memisc, quietly = T)
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


## focal firm's competition network
name_i <- 'ibm'

##============================================
## load data 
##   **SLOW TO LOAD**
##--------------------------------------------
## 1. CrunchBase data
cb  <- source(file.path(getwd(),'R','acqlogit','acqlogit_cb_data_prep.R'))$value      ## DATA 

## 2. COMPUSTAT
csa <- read.csv(file = file.path('compustat','fundamentals-annual.csv'), na.strings = c(NA,'','NA'))
dim(csa)
names(csa)

## 3. load FILTERED regression dataframe to seprate table file
df.reg <- read.csv(sprintf("acqlogit_compnet_covs_df_FILTERED_%s.csv",name_i),na.strings = c(NA,'','NA'))


## SELECT COLUMNS FROM COMPUSTAT
cols <- c('conm','conml','gvkey','datadate','fyear','indfmt','consol','popsrc','tic','cusip',
  'act', ## total assets  (ln for size proxy)
  'che', ## cash and short term investments (scale by total assets for cash holdings)
  'emp', ## employees (ln employee size proxy) 
  'ebitda', ## ebidta (scale by total assets for ROA performance proxy)
  'prcc_c', ## close market price at calendar year
  # 'prccm', ## (monthly close price for december; use if prcc_c not available)
  'csho', ## shares outstanding  (PRCC_C x CSHO = market value of equity)
  'ceq' ## common/ordinary equity total
)
csa2 <- csa[,cols]

##==================================
## Public firms data frame (mapping company_name_unique --> stock_symbol)
##   used to merge in COMPUSTAT DATA with CrunchBase public firms
##----------------------------------
## public firms from crunbhbase regression data
tmpnames <- unique(c(as.character(df.reg$i),as.character(df.reg$j)))
df.cs <- data.frame(company_name_unique=tmpnames[tmpnames %in% cb$co_ipo$company_name_unique])
## crunchbase ipo data fields for crunchbase compnet firms
ipocols <- c('company_name_unique','stock_symbol','stock_exchange_symbol','went_public_on','country_code')
df.cs <- merge(df.cs, cb$co_ipo[,ipocols], by='company_name_unique', all.x=T, all.y=F)
## merge in COMPUSTAT data by stock_exchange symbole
df.cs <- merge(df.cs, csa2, by.x='stock_symbol',by.y='tic', all.x=T, all.y=F)
## merge in COMPUSTAT data with crunchbase ticker symbol and unique name into crunchbase regression data frame
df.reg$year <- as.integer(sapply(str_split(as.character(df.reg$date),'[-]'), function(x)x[1]))
yrs <- unique(df.reg$year)
df.reg.m <- data.frame()
for (yr in yrs) {
  df.reg.yr <- df.reg[df.reg$year==yr,]
  df.cs.yr <- df.cs[df.cs$fyear==yr,]
  df.reg.yr <- merge(df.reg.yr, df.cs.yr, by.x='i',by.y='company_name_unique', all.x=T, all.y=F)
  df.reg.m <- rbind(df.reg.m, df.reg.yr)
}


##=====================================
## Get diversification from segments
##-------------------------------------
##
##  TODO
##

##--------------------------------------------
df.reg <- df.reg.m
tmp <- cb$co[,c('company_name_unique','founded_on')]; 
tmpi <- tmp; names(tmpi) <- c('name','founded_on_i')
tmpj <- tmp; names(tmpj) <- c('name','founded_on_j')
df.reg <- merge(df.reg, tmpi, by.x='j',by.y='name',all.x=T,all.y=F)
df.reg <- merge(df.reg, tmpj, by.x='i',by.y='name',all.x=T,all.y=F)

df.reg$ij.age.diff <- as.numeric( (ymd(df.reg$founded_on_i) - ymd(df.reg$founded_on_j)) / 365.25 )

df.reg$ij.inv.dist <- 100 / df.reg$ij.dist  ## max(df.reg$ij.dist[df.reg$ij.dist < Inf ])

## cbind(y,t) ~ econ.left/class+welfare/class+auth/class,
df.sub <- df.reg
# df.sub$ij.dist[df.sub$ij.dist == Inf] <- median(df.reg$ij.dist[df.reg$ij.dist < Inf])
df.sub$ij.dist[df.sub$ij.dist == Inf] <- 1 + max(df.reg$ij.dist[df.reg$ij.dist < Inf])

## replace NA|missing discrete homophily terms with mode
df.sub$ij.same.state[is.na(df.sub$ij.same.state)] <- Mode(df.sub$ij.same.state[!is.na(df.sub$ij.same.state)])
df.sub$ij.same.country[is.na(df.sub$ij.same.country)] <- Mode(df.sub$ij.same.country[!is.na(df.sub$ij.same.country)])
df.sub$ij.same.region[is.na(df.sub$ij.same.region)] <- Mode(df.sub$ij.same.region[!is.na(df.sub$ij.same.region)])
df.sub$ij.same.employee.range[is.na(df.sub$ij.same.employee.range)] <- Mode(df.sub$ij.same.employee.range[!is.na(df.sub$ij.same.employee.range)])

# df.sub$i.acq.experience[is.na(df.sub$i.acq.experience)]
# df.sub$i.num.mkts[is.na(df.sub$i.num.mkts)]
# df.sub$i.deg[is.na(df.sub$i.deg)] 
# df.sub$ij.diff.deg[is.na(df.sub$ij.diff.deg)]

## COMPUTE PRODUCT DISSIMILARITY
df.sub$ij.dissim <- NA
df.sub$ij.discossim <- NA
for (i in 1:nrow(df.sub)) {
  firm.i <- as.character(df.sub$i[i])
  firm.j <- as.character(df.sub$j[i])
  cats.i <- str_split(cb$co$category_list[cb$co$company_name_unique == firm.i], '[|]')[[1]]
  cg.i <- str_split(cb$co$category_group_list[cb$co$company_name_unique == firm.i], '[|]')[[1]]
  c.i <- unique(c(cats.i,cg.i))
  cats.j <- str_split(cb$co$category_list[cb$co$company_name_unique == firm.j], '[|]')[[1]]
  cg.j <- str_split(cb$co$category_group_list[cb$co$company_name_unique == firm.j], '[|]')[[1]]
  c.j <- unique(c(cats.j,cg.j))
  c.all <- unique(c(c.i,c.j))
  v.i <- as.integer(c.all %in% c.i)
  v.j <- as.integer(c.all %in% c.j)
  df.sub$ij.dissim[i] <- -1 * (v.i %*% v.j)[1,1]
  df.sub$ij.discossim[i] <- -1 * (v.i %*% v.j)[1,1] / (sqrt(sum(v.i^2)) * sqrt(sum(v.j^2)))
  if (i %% 50 == 0) cat(sprintf('%s\n',i))
}

##====================================================
##  SUBSET
##----------------------------------------------------
# ##--------------------------------------------------
# acq.only <- df.sub[df.sub$y==1,c('t','i')]
# acq.only <- droplevels.data.frame(acq.only)
# #
# df.acq <- data.frame()
# df.cnt <- data.frame()
# for (i in 1:nrow(acq.only)) {
#   t <- as.numeric(acq.only$t[i])
#   firm_i <- as.character(acq.only$i[i])
#   tmp <- df.sub[as.numeric(df.sub$t) == t, ]
#   tmp <- tmp[as.character(tmp$i) == firm_i, ]
#   cat(sprintf("t %s : nrow %s\n",t,nrow(tmp)))
#   df.cnt <- rbind(df.cnt, data.frame(t=t,n=nrow(tmp)))
#   df.acq <- rbind(df.acq, tmp)
# }
# ## keep only nrow >= x
# t.keep.acq <- df.cnt$t[which(df.cnt$n >= 3)]
# df.acq <- df.acq[which(df.acq$t %in% t.keep.acq), ]
# 
# ##-----------------------------------------------------
# targ.only <- df.sub[df.sub$y==1,c('t','j')]
# targ.only <-droplevels.data.frame(targ.only)
# #
# df.targ <- data.frame()
# df.cnt <- data.frame()
# for (i in 1:nrow(targ.only)) {
#   t <- as.numeric(targ.only$t[i])
#   j <- as.character(targ.only$j[i])
#   tmp <- df.sub[as.numeric(df.sub$t) == t, ]
#   tmp <- tmp[as.character(tmp$j) == j, ]
#   cat(sprintf("t %s : nrow %s\n",t,nrow(tmp)))
#   df.cnt <- rbind(df.cnt, data.frame(t=t,n=nrow(tmp)))
#   df.targ <- rbind(df.targ, tmp)
# }
# ## keep only nrow >= x
# t.keep.targ <- df.cnt$t[which(df.cnt$n >= 3)]
# df.targ <- df.targ[which(df.targ$t %in% t.keep.targ), ]
# ##-------------------------------------------------------
# df.sub.0 <- df.sub
# df.sub <- df.sub[which(df.sub$t %in% t.keep.targ
#                        & df.sub$t %in% t.keep.acq), ]
# ##------------------------------------------------------

##================================
##
##  HOW MANY TARGETS ARE PRIVAET / PUBLIC?
##
##--------------------------------
#
# df.own <- df.sub[df.sub$y==1,c('j','date')]
# df.own$is_ipo <- apply(df.own,1,function(x){
#   any(cb$co_ipo$company_name_unique==x[1] & cb$co_ipo$went_public_on < x[2])
# })

## CHECK CRUNCHBASE ACQUISITIONS TARGETS OWNERSHIP STATUS AT TIME OF ACQUISITION
df.own <- cb$co_acq[cb$co_acq$acquired_on > '2000-01-01', 
                    c('acquirer_name_unique','acquiree_name_unique','acquired_on')]
df.own$acquirer_is_ipo <- apply(df.own,1,function(x){
  any(cb$co_ipo$company_name_unique==x[1] & cb$co_ipo$went_public_on < x[3])
})
df.own$target_is_ipo <- apply(df.own,1,function(x){
  any(cb$co_ipo$company_name_unique==x[2] & cb$co_ipo$went_public_on < x[3])
})
cnt.a <- plyr::count(df.own$acquirer_is_ipo)
cnt.t <- plyr::count(df.own$target_is_ipo)
print(sprintf('PUBLIC: %.2f%s aquirers, %.2f%s targets',
              100*cnt.a$freq[2]/sum(cnt.a$freq),'%',
              100*cnt.t$freq[2]/sum(cnt.t$freq),'%'))
print(sprintf('PRIVATE: %.2f%s aquirers, %.2f%s targets',
              100*cnt.a$freq[1]/sum(cnt.a$freq),'%',
              100*cnt.t$freq[1]/sum(cnt.t$freq),'%'))


# 'act', ## total assets  (ln for size proxy)
# 'che', ## cash and short term investments (scale by total assets for cash holdings)
# 'emp', ## employees (ln employee size proxy) 
# 'ebitda', ## ebidta (scale by total assets for ROA performance proxy)
# 'prcc_c', ## close market price at calendar year
# # 'prccm', ## (monthly close price for december; use if prcc_c not available)
# 'csho', ## shares outstanding  (PRCC_C x CSHO = market value of equity)
# 'ceq' ## common/ordinary equity total

## Precompute COMPUSTAT CONTROLS
df.sub$ln_asset <- log(df.sub$act)
df.sub$cash_holding <- 100 * df.sub$che / df.sub$act
df.sub$ln_employee <- log(df.sub$emp)
df.sub$roa <- 100 * df.sub$ebitda / df.sub$act
df.sub$me <- df.sub$prcc_c * df.sub$csho

###------------- SMS Proposal --------------------------------------
mc0 <- mclogit(
  cbind(y,t) ~ ln_asset + cash_holding + roa + ln_employee +
    ij.same.region  +  ij.diff.deg   +
    i.fm.mmc.sum    + I(i.acq.experience * 100),
  data = df.sub)
summary(mc0)

mc1 <- mclogit(
  cbind(y,t) ~  ln_asset + cash_holding + roa + ln_employee +
    ij.same.region +  ij.diff.deg   +
    i.fm.mmc.sum + I(i.acq.experience * 100) +
    j.constraint + i.pow.n2 +
    ij.inv.dist + I(100 * ij.discossim) ,
  data = df.sub)
summary(mc1)

mc2 <- mclogit(
  cbind(y,t) ~  ln_asset + cash_holding + roa + ln_employee +
    ij.same.region +  ij.diff.deg   +
    i.fm.mmc.sum + I(i.acq.experience * 100) +
    j.constraint + i.pow.n2 +
    I(i.fm.mmc.sum^2) ,
  data = df.sub)
summary(mc2)

mc3 <- mclogit(
  cbind(y,t) ~  ln_asset + cash_holding + roa + ln_employee +
    ij.same.region +  ij.diff.deg   +
    ij.inv.dist + I(100 * ij.discossim) + 
    i.fm.mmc.sum + I(i.acq.experience * 100) +
    j.constraint + i.pow.n2 +
    I(i.fm.mmc.sum^2) ,
  data = df.sub)
summary(mc3)

mc3b <- mclogit(
  cbind(y,t) ~  ln_asset + cash_holding + roa + ln_employee +
    ij.same.region +  ij.diff.deg   +
    ij.inv.dist + I(100 * ij.discossim) + 
    i.fm.mmc.sum + I(i.acq.experience * 100) +
    j.constraint + i.pow.n2 +
    I(i.fm.mmc.sum^2) +
    I(i.fm.mmc.sum^2):i.pow.n2,
  data = df.sub)
summary(mc3b)


mc4 <- mclogit(
  cbind(y,t) ~  ln_asset + cash_holding + roa + ln_employee +
    ij.same.region +  ij.diff.deg   +
    i.fm.mmc.sum + I(i.acq.experience * 100) +
    j.constraint + i.pow.n2 +
    ij.inv.dist + I(100 * ij.discossim) + 
    I(i.fm.mmc.sum^2) +
    I(i.fm.mmc.sum^2):ij.inv.dist , 
  data = df.sub)
summary(mc4)

mc5 <- mclogit(
  cbind(y,t) ~  ln_asset + cash_holding + roa + ln_employee +
    ij.same.region +  ij.diff.deg   +
    i.fm.mmc.sum + I(i.acq.experience * 100) +
    j.constraint + i.pow.n2 +
    ij.inv.dist + I(100 * ij.discossim) + 
    I(i.fm.mmc.sum^2) +
    I(i.fm.mmc.sum^2):j.constraint , 
  data = df.sub)
summary(mc5)

## SUMMARY TABLE
mtable(mc1,mc3,mc3b,mc4,mc5)

## SAVE FILE
mtab <- memisc::mtable(mc0,mc1,mc2,mc3,mc4, signif.symbols = "")
memisc::write.mtable(mtab, file = "acqlogit_reg_table_ibm.txt", 
                     signif.symbols = "*")





mtab <- memisc::mtable(mc0,mc1,mc2,mc3,mc4, signif.symbols = "")
memisc::write.mtable(mtab, file = "sms_acq_logit_reg_table_ibm.txt", signif.symbols = "")

# correlations
round(cor(mc4$model),2)

(smc0 <- mclogit::getSummary.mclogit(mc0, alpha=.05) )
(smc1 <- mclogit::getSummary.mclogit(mc1, alpha=.05) )
(smc2 <- mclogit::getSummary.mclogit(mc2, alpha=.05) )
(smc3 <- mclogit::getSummary.mclogit(mc3, alpha=.05) )
(smc4 <- mclogit::getSummary.mclogit(mc4, alpha=.05) )

smc4$coef

round(mc4$covmat, 3)

df.model <- cbind(predict(mc4),mc4$model)




## SANITY CHECK
mc0 <- mclogit(
  cbind(y,t) ~ i.fm.mmc.sum + I(i.fm.mmc.sum^2) + 
    ij.dist + I(i.fm.mmc.sum^2):ij.dist + 
    i.pow.n2 +  I(i.fm.mmc.sum^2):i.pow.n2,
  data = df.sub)
summary(mc0)

## add all controls to check
mc1 <- mclogit(
  cbind(y,t) ~ ij.same.state + ij.same.region + ij.same.country + ij.same.employee.range + ij.diff.deg + 
    i.num.mkts + i.deg  + 
    i.fm.mmc.sum + I(i.fm.mmc.sum^2) + 
    ij.dist + I(i.fm.mmc.sum^2):ij.dist + 
    i.pow.n2 +  I(i.fm.mmc.sum^2):i.pow.n2,
  data = df.sub)
summary(mc1)

## preferable model (only useful controls)
mc2 <- mclogit(
  cbind(y,t) ~ ij.same.region + ij.same.employee.range + ij.diff.deg +
    i.deg  +  
    i.fm.mmc.sum + I(i.fm.mmc.sum^2) + 
    ij.dist + I(i.fm.mmc.sum^2):ij.dist + 
    i.pow.n2 +  I(i.fm.mmc.sum^2):i.pow.n2,
  data = df.sub)
summary(mc2)

## period effect  acquirer
mc2r <- mclogit(
  cbind(y,t) ~ ij.same.region +  ij.same.employee.range + ij.diff.deg +
    i.deg  +  
    i.fm.mmc.sum + I(i.fm.mmc.sum^2) + 
    ij.dist + I(i.fm.mmc.sum^2):ij.dist + 
    i.pow.n2 +  I(i.fm.mmc.sum^2):i.pow.n2,
  random = ~ 1 | t,
  control = mclogit.control(epsilon=1e-08, maxit=2000, trace=TRUE),
  data = df.sub)
summary(mc2r)

mc3r <- mclogit(
  cbind(y,t) ~ ij.same.region + ij.same.employee.range + ij.diff.deg +
    i.deg  + 
    i.fm.mmc.sum + I(i.fm.mmc.sum^2) + 
    ij.dist + I(i.fm.mmc.sum^2):ij.dist + 
    i.pow.n2 +  I(i.fm.mmc.sum^2):i.pow.n2,
  random = ~ 1 | t,
  control = mclogit.control(epsilon=1e-08, maxit=2000, trace=TRUE),
  data = df.sub)
summary(mc3r)

## NESTED i in time t
mc4r <- mclogit(
  cbind(y,t) ~ ij.same.region + ij.same.employee.range + ij.diff.deg +
    i.deg  + 
    i.fm.mmc.sum + I(i.fm.mmc.sum^2) + 
    ij.dist + I(i.fm.mmc.sum^2):ij.dist + 
    i.pow.n2 +  I(i.fm.mmc.sum^2):i.pow.n2,
  random = ~ 1 | t / i,
  control = mclogit.control(epsilon=1e-08, maxit=2000, trace=TRUE),
  data = df.sub)
summary(mc4r)

mc4 <- mclogit(
  cbind(y,t) ~ ij.same.region + ij.same.employee.range + ij.diff.deg +
    i.deg  + 
    i.fm.mmc.sum + I(i.fm.mmc.sum^2) + 
    ij.dist + 
    i.pow.n2 ,
  control = mclogit.control(epsilon=1e-07, maxit=200, trace=TRUE),
  data = df.sub)
summary(mc4)



## preferable model (only useful controls)
cols <- c('i.deg','i.fm.mmc.sum','ij.dist','i.pow.n2')
df.sub.std <- df.sub
df.sub.std[,cols] <- scale(df.sub.std[,cols], center = T, scale = T)


mc2 <- mclogit(
  cbind(y,t) ~ ij.same.region + ij.same.employee.range + ij.diff.deg +
    log(1 + i.deg) + 
    i.fm.mmc.sum + I(i.fm.mmc.sum^2) + 
    ij.dist + I(i.fm.mmc.sum^2):ij.dist + 
    i.pow.n2 +  I(i.fm.mmc.sum^2):i.pow.n2,
  data = df.sub.std)
summary(mc2)







## >= 10 constraint , power 
## >= 9 constraint no, power no
## >= 8 constraint no, power no
## >= 7 constraint no, power no
## >= 6 constraint no, power   no
## >= 5 constraint no, power  no
## >= 4 constraint no, power yes
## >= 3 constraint yes, power yes
## >= 2 constraint yes, power no
mc3 <- mclogit(
  cbind(y,t) ~  ij.same.region + ij.same.employee.range + ij.diff.deg   +
    i.deg  +  i.fm.mmc.sum + I(i.acq.experience * 100) +
    i.pow.n2 + j.pow.n1 + ij.dist  +
    i.constraint + ij.diff.constraint +
    I(i.fm.mmc.sum^2) +
    i.fm.mmc.sum:i.pow.n2 +
    i.fm.mmc.sum:i.constraint +
    i.fm.mmc.sum:ij.dist  , 
  data = df.sub)
summary(mc3)

mc2 <- mclogit(
  cbind(y,t) ~  ij.same.region + ij.same.employee.range + ij.diff.deg   +
    i.deg  +  i.fm.mmc.sum + I(i.acq.experience * 100) +
    i.pow.n2 + j.pow.n1 + ij.dist  +
    i.constraint + 
    I(i.fm.mmc.sum^2) +
    i.fm.mmc.sum:i.pow.n2 +
    i.fm.mmc.sum:i.constraint +
    i.fm.mmc.sum:j.constraint +
    i.fm.mmc.sum:ij.dist  , 
  data = df.sub)
summary(mc2)


memisc::mtable(mc1,mc2,mc3)

mtab <- memisc::mtable(mc0,mc1,mc2,mc3,mc4, signif.symbols = "")
memisc::write.mtable(mtab, file = "sms_acq_logit_reg_table_ibm.txt", signif.symbols = "")

# correlations
round(cor(mc4$model),2)

(smc0 <- mclogit::getSummary.mclogit(mc0, alpha=.05) )
(smc1 <- mclogit::getSummary.mclogit(mc1, alpha=.05) )
(smc2 <- mclogit::getSummary.mclogit(mc2, alpha=.05) )
(smc3 <- mclogit::getSummary.mclogit(mc3, alpha=.05) )
(smc4 <- mclogit::getSummary.mclogit(mc4, alpha=.05) )

smc4$coef

round(mc4$covmat, 3)

df.model <- cbind(predict(mc4),mc4$model)
















## ----------------------------------------
##  Logit Model 
##------------------------------------------
mc2 <- mclogit(
  cbind(y,t) ~ ij.same.region + ij.same.employee.range + ij.diff.deg +
    log(1 + i.deg) + 
    i.fm.mmc.sum + I(i.fm.mmc.sum^2) + 
    ij.dist + I(i.fm.mmc.sum^2):ij.dist + 
    i.pow.n2 +  I(i.fm.mmc.sum^2):i.pow.n2,
  data = df.sub)
summary(mc2)

mc2.i <- mclogit(
  cbind(y,t) ~ ij.same.region + ij.same.employee.range + ij.diff.deg +
    log(1 + i.deg) + 
    i.fm.mmc.sum + I(i.fm.mmc.sum^2) + 
    ij.dist + I(i.fm.mmc.sum^2):ij.dist + 
    i.pow.n2 +  I(i.fm.mmc.sum^2):i.pow.n2,
  random = ~ 1 | i,
  data = df.sub)
summary(mc2.i)

## ----------------------------------------
##  MIXED EFFECTS LOGISTIC
##------------------------------------------
glm1 <- glmer(
  y ~ ij.same.region + ij.same.employee.range + ij.diff.deg  +
    log(1 + i.deg) +  i.fm.mmc.sum +
    I(i.fm.mmc.sum^2) +
    ij.dist + I(i.fm.mmc.sum^2):ij.dist + 
    i.pow.n2 +  I(i.fm.mmc.sum^2):i.pow.n2 +
    (1 | i) + (1 | t), 
  verbose = 9, 
  family=binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",
                       optCtrl=list(maxfun=1e4)),
  data=df.sub
)
summary(glm1)


##--------------------------------------
## only acquirer (focus on target selection)
##--------------------------------------
acq.only <- df.sub[df.sub$y==1,c('t','i')]
acq.only <-droplevels.data.frame(acq.only)
#
df.acq <- data.frame()
df.cnt <- data.frame()
for (i in 1:nrow(acq.only)) {
  t <- as.numeric(acq.only$t[i])
  firm_i <- as.character(acq.only$i[i])
  tmp <- df.sub[as.numeric(df.sub$t) == t, ]
  tmp <- tmp[as.character(tmp$i) == firm_i, ]
  cat(sprintf("t %s : nrow %s\n",t,nrow(tmp)))
  df.cnt <- rbind(df.cnt, data.frame(t=t,n=nrow(tmp)))
  df.acq <- rbind(df.acq, tmp)
}
## keep only nrow >= 3
t.keep <- df.cnt$t[which(df.cnt$n >= 3)]
df.acq <- df.acq[which(df.acq$t %in% t.keep), ]

acq1 <- mclogit(
  cbind(y,t) ~ ij.same.region + ij.same.employee.range + ij.diff.deg +
    ij.dist + ij.diff.pow.n1 +
    I(i.fm.mmc.sum^2):ij.dist ,
  data = df.acq)
summary(acq1)


glm.acq1 <- glmer(
  y ~ ij.same.region + ij.same.employee.range + ij.diff.deg +
    ij.dist + 
    I(i.fm.mmc.sum^2):ij.dist  +
    (1  | i) , 
  verbose = 9, family=binomial(link='logit'),
  control=glmerControl(optimizer="bobyqa",
                       optCtrl=list(maxfun=2e5)),
  data=df.acq
)
summary(glm.acq1)


##--------------------------------------
## only target (focus on MMC motivating acquirer)
##--------------------------------------
targ.only <- df.sub[df.sub$y==1,c('t','j')]
targ.only <-droplevels.data.frame(targ.only)
#
df.targ <- data.frame()
df.cnt <- data.frame()
for (i in 1:nrow(targ.only)) {
  t <- as.numeric(targ.only$t[i])
  j <- as.character(targ.only$j[i])
  tmp <- df.sub[as.numeric(df.sub$t) == t, ]
  tmp <- tmp[as.character(tmp$j) == j, ]
  cat(sprintf("t %s : nrow %s\n",t,nrow(tmp)))
  df.cnt <- rbind(df.cnt, data.frame(t=t,n=nrow(tmp)))
  df.targ <- rbind(df.targ, tmp)
}
## keep only nrow >= 3
t.keep <- df.cnt$t[which(df.cnt$n >= 4)]
df.targ <- df.targ[which(df.targ$t %in% t.keep), ]

write.csv(df.targ, file="sms_reg_df_targ_set_ibm.csv",row.names = F)

##-----------------------------------------------
## LOGIT Acquirer set only
##-----------------------------------------------


mc2t <- mclogit(
  cbind(y,t) ~ ij.same.region + ij.same.employee.range + ij.diff.deg +
    log(1 + i.deg) + 
    i.fm.mmc.sum + I(i.fm.mmc.sum^2) + 
    ij.dist + I(i.fm.mmc.sum^2):ij.dist + 
    i.pow.n2 +  I(i.fm.mmc.sum^2):i.pow.n2,
  data = df.targ)
summary(mc2t)

mc2tr.t <- mclogit(
  cbind(y,t) ~ ij.same.region + ij.same.employee.range + ij.diff.deg +
    log(1 + i.deg) + 
    i.fm.mmc.sum + I(i.fm.mmc.sum^2) + 
    ij.dist + I(i.fm.mmc.sum^2):ij.dist + 
    i.pow.n2 +  I(i.fm.mmc.sum^2):i.pow.n2,
  random =  ~ 1 | t,
  data = df.targ)
summary(mc2tr.t)

mc2tr.i <- mclogit(
  cbind(y,t) ~ ij.same.region + ij.same.employee.range + ij.diff.deg +
    log(1 + i.deg) + 
    i.fm.mmc.sum + I(i.fm.mmc.sum^2) + 
    ij.dist + I(i.fm.mmc.sum^2):ij.dist + 
    i.pow.n2 +  I(i.fm.mmc.sum^2):i.pow.n2,
  random =  ~ 1 | i,
  data = df.targ)
summary(mc2tr.i)


##--------------------------------------------
##
## SMS MIXED LOGIT REG TABLE
##
##--------------------------------------------
m0 <- glmer(
  y ~ ij.same.region + ij.same.employee.range + ij.diff.deg  +
    log(1 + i.deg) +  i.fm.mmc.sum +
    ij.dist +
    i.pow.n2 +  
    ( 1 | i ) + ( 1 | t ), 
  # verbose = 9, 
  family=binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",
                       optCtrl=list(maxfun=2e5)),
  data=df.targ
)
summary(m0)

m1 <- glmer(
  y ~ ij.same.region + ij.same.employee.range + ij.diff.deg  +
    log(1 + i.deg) +  i.fm.mmc.sum +
    I(i.fm.mmc.sum^2) +
    ij.dist +
    i.pow.n2 +  
    ( 1 | i ) + ( 1 | t ), 
  # verbose = 9, 
  family=binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",
                       optCtrl=list(maxfun=2e5)),
  data=df.targ
)
summary(m1)

m2 <- glmer(
  y ~ ij.same.region + ij.same.employee.range + ij.diff.deg  +
    log(1 + i.deg) +  i.fm.mmc.sum +
    I(i.fm.mmc.sum^2) +
    ij.dist + 
    i.pow.n2 +  I(i.fm.mmc.sum^2):i.pow.n2 +
    ( 1 | i ) + ( 1 | t ), 
  # verbose = 9, 
  family=binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",
                       optCtrl=list(maxfun=2e5)),
  data=df.targ
)
summary(m2)

m3 <- glmer(
  y ~ ij.same.region + ij.same.employee.range + ij.diff.deg  +
    log(1 + i.deg) +  i.fm.mmc.sum +
    I(i.fm.mmc.sum^2) +
    ij.dist + I(i.fm.mmc.sum^2):ij.dist + 
    i.pow.n2 + 
    ( 1 | i ) + ( 1 | t ), 
  # verbose = 9, 
  family=binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",
                       optCtrl=list(maxfun=2e5)),
  data=df.targ
)
summary(m3)

m4 <- glmer(
  y ~ ij.same.region + ij.same.employee.range + ij.diff.deg  +
    log(1 + i.deg) +  i.fm.mmc.sum +
    I(i.fm.mmc.sum^2) +
    ij.dist + I(i.fm.mmc.sum^2):ij.dist + 
    i.pow.n2 +  I(i.fm.mmc.sum^2):i.pow.n2 +
    ( 1 | i ) + ( 1 | t ), 
  # verbose = 9, 
  family=binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",
                       optCtrl=list(maxfun=2e5)),
  data=df.targ
)
summary(m4)


mlist <- list(m0=m0,m1=m1,m2=m2,m3=m3,m4=m4)
screenreg(mlist, digits = 3, ci.force = T, ci.force.level = .95)
screenreg(mlist, digits=3)
htmlreg(mlist, file = "sms_acq_glmm_results_ibm.html", ci.force = T, ci.force.level = .95, digits = 3, star.symbol = '')

df.targ2 <- cbind(predict(m4),(1/(1+exp(-predict(m4)))),df.targ)
write.csv(df.targ2, file="acqlogit_covs_pred_df_ibm.csv",row.names = F)

##--------------end sms-----------------------


## NESTED -- NO
glm.targ1 <- glmer(
  y ~ ij.same.region + ij.same.employee.range + ij.diff.deg  +
    log(1 + i.deg) +  i.fm.mmc.sum +
    I(i.fm.mmc.sum^2) +
    ij.dist + I(i.fm.mmc.sum^2):ij.dist + 
    i.pow.n2 +  I(i.fm.mmc.sum^2):i.pow.n2 +
    (1  | t/i ), 
  verbose = 9, 
  family=binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",
                       optCtrl=list(maxfun=1e3)),
  data=df.targ
)
summary(glm.targ1)

## CROSSED -- YES
glm.targ2 <- glmer(
  y ~ ij.same.region + ij.same.employee.range + ij.diff.deg  +
    log(1 + i.deg) +  i.fm.mmc.sum +
    I(i.fm.mmc.sum^2) +
    ij.dist + I(i.fm.mmc.sum^2):ij.dist + 
    i.pow.n2 +  I(i.fm.mmc.sum^2):i.pow.n2 +
    ( 1 | i ) + ( 1 | t ), 
  verbose = 9, 
  family=binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",
                       optCtrl=list(maxfun=1e4)),
  data=df.targ
)
summary(glm.targ2)


glm.targ2 <- glmer(
  y ~ ij.same.region + ij.same.employee.range + ij.diff.deg  +
    log(1 + i.deg) +  i.fm.mmc.sum +
    I(i.fm.mmc.sum^2) +
    ij.dist + I(i.fm.mmc.sum^2):ij.dist + 
    i.pow.n2 +  I(i.fm.mmc.sum^2):i.pow.n2 +
    (1  | i/t ), 
  verbose = 9, 
  family=binomial(link = "logit"),
  control=glmerControl(optimizer="bobyqa",
                       optCtrl=list(maxfun=2e5)),
  data=df.targ
)
summary(glm.targ2)

##----------------------------------------
## Mixed LOGISTIC REGRESSION
##---------------------------------------
cols <- c('i.deg','i.fm.mmc.sum','ij.dist','i.pow.n2')
df.sub.std <- df.sub
df.sub.std[,cols] <- scale(df.sub.std[,cols], center = T, scale = T)


lm1 <- lmer(
  y ~ ij.same.region + ij.same.employee.range + ij.diff.deg +
    i.deg  + 
    i.fm.mmc.sum + I(i.fm.mmc.sum^2) + 
    ij.dist + I(i.fm.mmc.sum^2):ij.dist + 
    i.pow.n2 +  I(i.fm.mmc.sum^2):i.pow.n2 + 
    (1 | j) + (1 | i), 
  verbose = 9, family=binomial,
  data=df.sub.std
)
summary(lm1)









##--------------------------------------------------------------
## Examples
##--------------------------------------------------------------


##################################################
### MCLOGIT MIXED CONDITIONAL LOGIT
data(Transport)

mc1 <- mclogit(
  cbind(resp,suburb) ~ distance+cost,
  data=Transport
)
summary(mc1)

data(electors)

mc2 <- mclogit(
  cbind(Freq,interaction(time,class)) ~ econ.left/class+welfare/class+auth/class,
  random=~1|party.time,
  data=within(electors,party.time<-interaction(party,time)))
summary(mc2)



######################################################
## MCLOGIT  

data("Fishing", package = "mlogit")
Fish <- mlogit.data(Fishing, varying = c(2:9), shape = "wide", choice = "mode")

## a pure "conditional" model

summary(mlogit(mode ~ price + catch, data = Fish))

## a pure "multinomial model"

summary(mlogit(mode ~ 0 | income, data = Fish))

## which can also be estimated using multinom (package nnet)

library("nnet")
summary(multinom(mode ~ income, data = Fishing))

## a "mixed" model

m <- mlogit(mode ~ price+ catch | income, data = Fish)
summary(m)

## same model with charter as the reference level

m <- mlogit(mode ~ price+ catch | income, data = Fish, reflevel = "charter")

## same model with a subset of alternatives : charter, pier, beach

m <- mlogit(mode ~ price+ catch | income, data = Fish,
            alt.subset = c("charter", "pier", "beach"))

## model on unbalanced data i.e. for some observations, some
## alternatives are missing

# a data.frame in wide format with two missing prices
Fishing2 <- Fishing
Fishing2[1, "price.pier"] <- Fishing2[3, "price.beach"] <- NA
mlogit(mode~price+catch|income, Fishing2, shape="wide", choice="mode", varying = 2:9)

# a data.frame in long format with three missing lines
data("TravelMode", package = "AER")
Tr2 <- TravelMode[-c(2, 7, 9),]
mlogit(choice~wait+gcost|income+size, Tr2, shape = "long",
       chid.var = "individual", alt.var="mode", choice = "choice")

## An heteroscedastic logit model

data("TravelMode", package = "AER")
hl <- mlogit(choice ~ wait + travel + vcost, TravelMode,
             shape = "long", chid.var = "individual", alt.var = "mode",
             method = "bfgs", heterosc = TRUE, tol = 10)

## A nested logit model

TravelMode$avincome <- with(TravelMode, income * (mode == "air"))
TravelMode$time <- with(TravelMode, travel + wait)/60
TravelMode$timeair <- with(TravelMode, time * I(mode == "air"))
TravelMode$income <- with(TravelMode, income / 10)

# Hensher and Greene (2002), table 1 p.8-9 model 5
TravelMode$incomeother <- with(TravelMode, ifelse(mode %in% c('air', 'car'), income, 0))
nl <- mlogit(choice~gcost+wait+incomeother, TravelMode,
             shape='long', alt.var='mode',
             nests=list(public=c('train', 'bus'), other=c('car','air')))

# same with a comon nest elasticity (model 1)
nl2 <- update(nl, un.nest.el = TRUE)

## a probit model
## Not run: 
pr <- mlogit(choice ~ wait + travel + vcost, TravelMode,
             shape = "long", chid.var = "individual", alt.var = "mode",
             probit = TRUE)

## End(Not run)


## a mixed logit model
rpl <- mlogit(mode ~ price+ catch | income, Fishing, varying = 2:9,
              shape = 'wide', rpar = c(price= 'n', catch = 'n'),
              correlation = TRUE, halton = NA,
              R = 10, tol = 10, print.level = 0)
summary(rpl)
rpar(rpl)
cor.mlogit(rpl)
cov.mlogit(rpl)
rpar(rpl, "catch")
summary(rpar(rpl, "catch"))

## End(Not run)

# # a ranked ordered model
# data("Game", package = "mlogit")
# g <- mlogit(ch~own|hours, Game, choice='ch', varying = 1:12,
#             ranked=TRUE, shape="wide", reflevel="PC")










