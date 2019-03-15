library(igraph)
library(intergraph)
library(btergm)
library(xergm)
library(parallel)
library(texreg)
library(stringr)
library(reshape2)
library(plyr)
library(dplyr)
library(lattice)
library(ggplot2)
library(intergraph)

## DIRECTORIES

work_dir <- "C:/Users/T430/Google Drive/PhD/Dissertation/competition networks/compnet2"
data_dir <- "C:/Users/T430/Google Drive/PhD/Dissertation/competition networks/compnet2/firm_nets_rnr"
results_dir <- "C:/Users/T430/Google Drive/PhD/Dissertation/competition networks/compnet2/amj_rnr_results"
img_dir  <- "C:/Users/T430/Google Drive/PhD/Dissertation/competition networks/envelopment/img"


## set woring dir
setwd(work_dir)

## load awareness functions
source(file.path(getwd(),'R','amj_awareness_functions.R')) 

## -----------Model Results Settings-----
name_i <- 'qualtrics'
firm_i <- name_i
d <- 3
R <- 2000
nPeriods <- 11
m_x <- 'm4'
##----------------------------------------

## FUNCTIONS FOR NAs REMOVED
.med <- function(x){return(median(x, na.rm=TRUE))}
.min <- function(x){return(min(x, na.rm=TRUE))}
.max <- function(x){return(max(x, na.rm=TRUE))}
.avg <- function(x){return(mean(x, na.rm=TRUE))}
.std <- function(x){return(sd(x, na.rm=TRUE))}
.qtl <- function(x, probs){return(quantile(x, probs=probs, na.rm=TRUE))}
.iqr <- function(x){return(IQR(x, na.rm=TRUE))}

##=========================================
##  MAIN RESULTS
##-----------------------------------------

d <- 3
R <- 2000
nPeriods <- 11
firms.todo <- c('qualtrics')

coho <- list(focal=list(), 
             observations=list(), 
             encounters=list(), 
             firms=list(), 
             names=list(), 
             periods=list(),
             vdf=list())

for (name_i in firms.todo) {
  for (m_x in c('m0','m1','m2','m3','m4')) {
    
    cat(sprintf('%s %s\n', name_i, m_x))
    firm_i <- name_i
    ## results 
    fits.file <- sprintf('%s/fit_%s_pd%s_R%s_%s.rds', results_dir, name_i, nPeriods, R, m_x)
    if (! file.exists(fits.file)) 
    {
      fits.file <- sprintf('%s/fit_%s_pd%s_R%s_%s.rds', results_dir, name_i, nPeriods-1, R, m_x)
    }
    fits <- readRDS(fits.file)
    if (class(fits) == "list" & class(fits[[name_i]]) == "list") {
      fits <- fits[[name_i]][[m_x]]
    }
    
    ##  network data
    nets.file <- file.path(data_dir,sprintf('%s_d%s.rds',firm_i,d))
    nets <- readRDS(nets.file)
    
    num.pd <- length(nets)
    net <- nets[[num.pd]]
    rm(nets)
    vdf <- igraph::as_data_frame(asIgraph(net), what = 'vertices')
    
    coho$focal[[m_x]] <- name_i
    coho$observations[[m_x]] <- fits@nobs
    coho$encounters[[m_x]] <- sum(fits@response)
    coho$names[[m_x]] <- net %v% 'vertex.names'
    coho$firms[m_x] <- length(coho$names[[m_x]])
    coho$periods[[m_x]] <- fits@time.steps
    coho$vdf[[m_x]] <- vdf[,c('company_name','vertex.names','ipo_status','category_list','category_group_list')]
    
    saveRDS(coho, file="awareness_cohorts_main_results_qualtrics.rds")
    
    rm(fits);rm(vdf);gc()
    gc()
  
  }
}


## PRINT COHORT SUMMARY
coho.df <- data.frame(
  observations=sapply(coho$observations, function(focal)focal),
  encounters=sapply(coho$encounters, function(focal)focal),
  firms=sapply(coho$firms, function(focal)focal)
)
print(coho.df)
print(colSums(coho.df))
print(summary(coho.df$firms))





##=========================================
##  REPLICATIONS
##-----------------------------------------

d <- 3
R <- 2000
nPeriods <- 11
m_x <- 'm4'
firms.todo <- c('qualtrics','abroad101','checkmarket','clarabridge','cloudcherry',
                'confirmit','customergauge','cx-index','empathica',
                'feedback-lite','first-mile-geo','getfeedback',
                'inqwise','leaderamp', 'medallia','myfeelback',
                'promoter-io','satmetrix',
                'snap-surveys-ltd','super-simple-survey','survata',
                'surveyrock','typeform','userate','verint','voice-polls')

coho <- list(focal=list(), 
             observations=list(), 
             encounters=list(), 
             firms=list(), 
             names=list(), 
             periods=list(),
             vdf=list())

for (name_i in firms.todo) {
  cat(sprintf('%s\n', name_i))
  firm_i <- name_i
  ## results
  fits.file <- sprintf('%s/fit_%s_pd%s_R%s_%s.rds', results_dir, name_i, nPeriods, R, m_x)
  if (! file.exists(fits.file)) 
  {
    fits.file <- sprintf('%s/fit_%s_pd%s_R%s_%s.rds', results_dir, name_i, nPeriods-1, R, m_x)
  }
  fits <- readRDS(fits.file)
  if (class(fits) == "list" & class(fits[[name_i]]) == "list") {
    fits <- fits[[name_i]][[m_x]]
  }

  ##  network data
  nets.file <- file.path(data_dir,sprintf('%s_d%s.rds',firm_i,d))
  nets <- readRDS(nets.file)
  
  num.pd <- length(nets)
  net <- nets[[num.pd]]
  rm(nets)
  vdf <- igraph::as_data_frame(asIgraph(net), what = 'vertices')
  
  coho$focal[[name_i]] <- name_i
  coho$observations[[name_i]] <- fits@nobs
  coho$encounters[[name_i]] <- sum(fits@response)
  coho$names[[name_i]] <- net %v% 'vertex.names'
  coho$firms[[name_i]] <- length(coho$names[[name_i]])
  coho$periods[[name_i]] <- fits@time.steps
  coho$vdf[[name_i]] <- vdf[,c('company_name','vertex.names','ipo_status','category_list','category_group_list')]

  saveRDS(coho, file="awareness_cohorts_26_focal_firm_replications.rds")
  
  rm(fits);rm(vdf);gc()
  gc()
  
}


## PRINT COHORT SUMMARY
coho.df <- data.frame(
  observations=sapply(coho$observations, function(focal)focal),
  encounters=sapply(coho$encounters, function(focal)focal),
  firms=sapply(coho$firms, function(focal)focal)
)
print(coho.df)
print(colSums(coho.df))
print(summary(coho.df$firms))

all.cohorts <- c()
for (focal in coho$focal) {
  all.cohorts <- c(all.cohorts, coho$names[[focal]])
}
all.cohorts <- unique(all.cohorts)

print(sprintf('unique firms over cohorts: %s', length(all.cohorts)))


##----------------------
## SIC CODES COMPUTATION
##----------------------

##------------------------------------------
##  CB Categories for SIC private companies
##------------------------------------------
##  network data
nets.file <- file.path(data_dir,sprintf('%s_d%s.rds',firm_i,d))
nets <- readRDS(nets.file)
g <- asIgraph(nets$`2017`)
rm(nets)

g.full <- read.graph("g_full.graphml", format = "graphml")

pgl <- list()



## PRIVATE ----------------
for (firm in names(coho$vdf)) {
  coho$vdf[[firm]]
}
g.cl.pri <- V(g)$category_list[V(g)$ipo_status=='0']
g.cgl.pri <- V(g)$category_group_list[V(g)$ipo_status=='0']

g2.cl.pri <- V(g.full)$category_list[V(g.full)$status_update!='ipo']
g2.cgl.pri <- V(g.full)$category_group_list[V(g.full)$status_update!='ipo']

g.cl <- unique(c(unlist(unname(sapply(g.cl.pri, function(x) str_split(x,"[|]")[[1]])))))
g.cgl <- unique(c(unlist(unname(sapply(g.cgl.pri, function(x) str_split(x,"[|]")[[1]])))))
g.cl.len <- length(g.cl)
g.cgl.len <- length(g.cgl)

g2.cl <- unique(c(unlist(unname(sapply(g2.cl.pri, function(x) str_split(x,"[|]")[[1]])))))
g2.cgl <- unique(c(unlist(unname(sapply(g2.cgl.pri, function(x) str_split(x,"[|]")[[1]])))))
g2.cl.len <- length(g2.cl)
g2.cgl.len <- length(g2.cgl)

sprintf('PRIVATE: %s / %s categories (%.3f)', g.cl.len, g2.cl.len, g.cl.len/g2.cl.len)
sprintf('PRIVATE: %s / %s category groups (%.3f)', g.cgl.len, g2.cgl.len, g.cgl.len/g2.cgl.len)

#[1] "PRIVATE: 211 / 636 categories (0.332)"
#[1] "PRIVATE: 41 / 46 category groups (0.891)"

pgl$pri <- g.cgl
pgl$pri.full <- g2.cgl

## ALL - PRIVATE + PUBLIC ------------------
g.cl.pub <- V(g)$category_list
g.cgl.pub <- V(g)$category_group_list

g2.cl.pub <- V(g.full)$category_list
g2.cgl.pub <- V(g.full)$category_group_list

g.cl <- unique(c(unlist(unname(sapply(g.cl.pub, function(x) str_split(x,"[|]")[[1]])))))
g.cgl <- unique(c(unlist(unname(sapply(g.cgl.pub, function(x) str_split(x,"[|]")[[1]])))))
g.cl.len <- length(g.cl)
g.cgl.len <- length(g.cgl)

g2.cl <- unique(c(unlist(unname(sapply(g2.cl.pub, function(x) str_split(x,"[|]")[[1]])))))
g2.cgl <- unique(c(unlist(unname(sapply(g2.cgl.pub, function(x) str_split(x,"[|]")[[1]])))))
g2.cl.len <- length(g2.cl)
g2.cgl.len <- length(g2.cgl)

sprintf('ALL PRIVATE + PUBLIC: %s / %s categories (%.3f)', g.cl.len, g2.cl.len, g.cl.len / g2.cl.len)
sprintf('ALL PRIVATE + PUBLIC: %s / %s category groups (%.3f)', g.cgl.len, g2.cgl.len, g.cgl.len / g2.cgl.len)

#[1] "PUBLIC: 63 / 331 categories (0.190)"
#[1] "PUBLIC: 22 / 45 category groups (0.489)"

pgl$pub <- g.cgl
pgl$pub.full <- g2.cgl

## make comparison dataframe
.al <- unique(c(pgl$pri.full, pgl$pub.full))
pdf <- data.frame(group=.al, 
                  group_in_pub=ifelse(.al %in% pgl$pub,1,0), 
                  group_in_pri=ifelse(.al %in% pgl$pri,1,0))


vdf <- igraph::as_data_frame(g, what = 'vertices')

sics <- vdf$company_sic[vdf$ipo_status=='1']
sics <- unlist(str_split(sics, pattern = '[|]'))
scnt <- plyr::count(sics)
scnt

idx <- vdf$ipo_status=='1' & grepl('NA', vdf$company_sic, ignore.case = T, perl = T)
jdx <- c('vertex.names','category_list','category_group_list','company_sic')
vdf[idx, jdx]


View(vdf[vdf$ipo_status=='1', jdx])

### MANUAL CHECK OF PUBLIC COMPANIES SIC CODES:
## qualtrics_d3_public_firm_sic_codes_manual_fix.csv
qmf <- read.csv('qualtrics_d3_public_firm_sic_codes_manual_fix.csv', header = T )
psic <- qmf$SIC_fix
psicc <- plyr::count(psic)
psicc
## PUBLIC  SIC CODES
# x freq
# 1  3579    1
# 2  4899    1
# 3  7310    1
# 4  7370   13
# 5  7371    1
# 6  7372   11
# 7  7373    2
# 8  7389    1
# 9  8700    1
# 10 8742    1

## 10 SIC codes covered by 33 public firms in Qualtrics 574 firm cohort cover

## PRIVATE FIRMS CRUNCHBASE DATA ------------------
# "PRIVATE: 211 / 636 categories (0.332)"
# "PRIVATE: 41 / 46 category groups (0.891)"

## PUBLIC FIRMS CRUNCHBASE DATA ------------------
# "PUBLIC: 63 / 331 categories (0.190)"
# "PUBLIC: 22 / 45 category groups (0.489)"

## CRUNCHBASE CATEGORY GROUPS
## 10 SIC by public firms  --> 22/45 (48.9%) crunchbase category groups
## X  SIC by private firms --> 41/46  (89.1%) crunchbase category groups
## 10 / (22/45) = X / (41/46)
## X = 10 * (41/46) / (22/45)
##   = 18.2 SIC codes estimated by CrunchBase CATEGORY GROUPS
###
## CRUNCHBASE CATEGORIES
## 10 SIC by public firms  --> 63/331 (19.0%%) crunchbase categories
## X  SIC by private firms --> 211/636  (33.2%) crunchbase categories
## 10 / (63/331) = X / (211/636)
## X = 10 * (211/636) / (63/331)
##   = 17.4 SIC codes estimated by CrunchBase CATEGORIES





## CRUNCHBASE CATEGORY GROUPS
## 10 SIC by public firms  --> 22/45 (48.9%) crunchbase category groups
## X  SIC by ALL     firms --> 41/46  (89.1%) crunchbase category groups
## 10 / (22/45) = X / (41/46)
## X = 10 * (41/46) / (22/45)
##   = 18.2 SIC codes estimated by CrunchBase CATEGORY GROUPS
###
## CRUNCHBASE CATEGORIES
## 10 SIC by public firms  --> 63/331 (19.0%%) crunchbase categories
## X  SIC by private firms --> 211/636  (33.2%) crunchbase categories
## 10 / (63/331) = X / (211/636)
## X = 10 * (211/636) / (63/331)
##   = 17.4 SIC codes estimated by CrunchBase CATEGORIES











##=========================================
##  DIVERSIFICATION CLUSTERING ALGORITHM
##-----------------------------------------

d <- 3
R <- 2000
nPeriods <- 11
m_x <- 'm4'
firms.todo <- c('qualtrics')
algos <- c('edgebetween','fastgreedy','infomap','labelprop','walktrap')

coho <- list(focal=list(), 
             observations=list(), 
             encounters=list(), 
             firms=list(), 
             names=list(), 
             periods=list(),
             vdf=list())

for (name_i in firms.todo) {
  for (algo in algos) {
    cat(sprintf('%s %s \n', name_i, algo))
    firm_i <- name_i
    ## results
    fits.file <- sprintf('%s/fit_%s_pd%s_R%s_%s_%s.rds', results_dir, name_i, nPeriods, R, m_x, algo)
    if (! file.exists(fits.file)) 
    {
      fits.file <- sprintf('%s/fit_%s_pd%s_R%s_%s.rds', results_dir, name_i, nPeriods-1, R, m_x)
    }
    fits <- readRDS(fits.file)
    if (class(fits) == "list" & class(fits[[name_i]]) == "list") {
      fits <- fits[[name_i]][[sprintf('%s_%s',m_x,algo)]]
    }
    
    ##  network data
    nets.file <- file.path(data_dir,sprintf('%s_d%s.rds',firm_i,d))
    nets <- readRDS(nets.file)
    
    num.pd <- length(nets)
    net <- nets[[num.pd]]
    rm(nets)
    vdf <- igraph::as_data_frame(asIgraph(net), what = 'vertices')
    
    coho$focal[[algo]] <- name_i
    coho$observations[[algo]] <- fits@nobs
    coho$encounters[[algo]] <- sum(fits@response)
    coho$names[[algo]] <- net %v% 'vertex.names'
    coho$firms[[algo]] <- length(coho$names[[algo]])
    coho$periods[[algo]] <- fits@time.steps
    coho$vdf[[algo]] <- vdf[,c('company_name','vertex.names','ipo_status','category_list','category_group_list')]
    
    saveRDS(coho, file="awareness_cohorts_qualtrics_diversification_algo.rds")
    
    rm(fits);rm(vdf);gc()
    gc()
  }
}


## PRINT COHORT SUMMARY
coho.df <- data.frame(
  observations=sapply(coho$observations, function(focal)focal),
  encounters=sapply(coho$encounters, function(focal)focal),
  firms=sapply(coho$firms, function(focal)focal)
)
print(coho.df)
print(colSums(coho.df))
print(summary(coho.df$firms))





##=========================================
##  Power Centrality Beta Decay - H2 Competitive Asymmetry 
##-----------------------------------------

d <- 3
R <- 2000
nPeriods <- 11
m_x <- 'm4'
firms.todo <- c('qualtrics')
betas <- list(beta1=-0.1, beta2=-0.2, beta3=-0.3, beta5=-0.5)

coho <- list(focal=list(), 
             observations=list(), 
             encounters=list(), 
             firms=list(), 
             names=list(), 
             periods=list(),
             vdf=list())

for (name_i in firms.todo) {
  for (j in 1:length(betas)) {
    betastr <- names(betas)[j]
    beta <- betas[j]
    #
    cat(sprintf('%s %s \n', name_i, beta))
    firm_i <- name_i
    ## results
    fits.file <- sprintf('%s/fit_%s_pd%s_R%s_%s_%s.rds', results_dir, name_i, nPeriods, R, m_x, betastr)
    if (! file.exists(fits.file)) 
    {
      fits.file <- sprintf('%s/fit_%s_pd%s_R%s_%s.rds', results_dir, name_i, nPeriods-1, R, m_x)
    }
    fits <- readRDS(fits.file)
    if (class(fits) == "list" & class(fits[[name_i]]) == "list") {
      fits <- fits[[name_i]][[sprintf('%s_%s',m_x,betastr)]]
    }
    
    ##  network data
    nets.file <- file.path(data_dir,sprintf('%s_d%s.rds',firm_i,d))
    nets <- readRDS(nets.file)
    
    num.pd <- length(nets)
    net <- nets[[num.pd]]
    rm(nets)
    vdf <- igraph::as_data_frame(asIgraph(net), what = 'vertices')
    
    coho$focal[[betastr]] <- name_i
    coho$observations[[betastr]] <- fits@nobs
    coho$encounters[[betastr]] <- sum(fits@response)
    coho$names[[betastr]] <- net %v% 'vertex.names'
    coho$firms[[betastr]] <- length(coho$names[[betastr]])
    coho$periods[[betastr]] <- fits@time.steps
    coho$vdf[[betastr]] <- vdf[,c('company_name','vertex.names','ipo_status','category_list','category_group_list')]
    
    saveRDS(coho, file="awareness_cohorts_qualtrics_power_centrality_decay_beta.rds")
    
    rm(fits);rm(vdf);gc()
    gc()
  }
}


## PRINT COHORT SUMMARY
coho.df <- data.frame(
  observations=sapply(coho$observations, function(focal)focal),
  encounters=sapply(coho$encounters, function(focal)focal),
  firms=sapply(coho$firms, function(focal)focal)
)
print(coho.df)
print(colSums(coho.df))
print(summary(coho.df$firms))









##=========================================
##  SCOPE OF AWARENESS CHECK - higher cycles (6,7) 
##-----------------------------------------

d <- 3
R <- 2000
nPeriods <- 11
m_x <- 'm4'
firms.todo <- c('qualtrics')


kcycle <- '7cycle'
fits.file <- sprintf('%s/fit_%s_pd%s_R%s_%s_%s.rds', results_dir, name_i, nPeriods, R, m_x, kcycle)
if (! file.exists(fits.file)) 
{
  fits.file <- sprintf('%s/fit_%s_pd%s_R%s_%s.rds', results_dir, name_i, nPeriods-1, R, m_x)
}
fits <- readRDS(fits.file)
if (class(fits) == "list" & class(fits[[name_i]]) == "list") {
  fits <- fits[[name_i]][[sprintf('%s_%s',m_x,kcycle)]]
}

##  network data
nets.file <- file.path(data_dir,sprintf('%s_d%s.rds',firm_i,d))
nets <- readRDS(nets.file)

num.pd <- length(nets)
net <- nets[[num.pd]]
rm(nets)
vdf <- igraph::as_data_frame(asIgraph(net), what = 'vertices')

coho$focal[[betastr]] <- name_i
coho$observations[[betastr]] <- fits@nobs
coho$encounters[[betastr]] <- sum(fits@response)
coho$firms[[betastr]] <- length(coho$names[[betastr]])
coho$periods[[betastr]] <- fits@time.steps
coho$vdf[[betastr]] <- vdf[,c('company_name','vertex.names','ipo_status','category_list','category_group_list')]


##=========================================
##  SCOPE OF AWARENESS CHECK - d=2
##-----------------------------------------


d <- 2
R <- 2000
nPeriods <- 11
m_x <- 'm4'
firms.todo <- c('qualtrics')


fits.file <- sprintf('%s/fit_%s_pd%s_R%s_%s_d%s.rds', results_dir, name_i, nPeriods, R, m_x, d)
if (! file.exists(fits.file)) 
{
  fits.file <- sprintf('%s/fit_%s_pd%s_R%s_%s.rds', results_dir, name_i, nPeriods-1, R, m_x)
}
fits <- readRDS(fits.file)
if (class(fits) == "list" & class(fits[[name_i]]) == "list") {
  fits <- fits[[name_i]][[sprintf('%s',m_x)]]
}

##  network data
nets.file <- file.path(data_dir,sprintf('%s_d%s.rds',firm_i,d))
nets <- readRDS(nets.file)

num.pd <- length(nets)
net <- nets[[num.pd]]
rm(nets)
vdf <- igraph::as_data_frame(asIgraph(net), what = 'vertices')

name_i
fits@nobs
sum(fits@response)
length(net %v% 'vertex.names')
fits@time.steps
coho$vdf[[betastr]] <- vdf[,c('company_name','vertex.names','ipo_status','category_list','category_group_list')]








##=========================================
##  Correlations
##-----------------------------------------
library(psych)

d <- 3
R <- 2000
nPeriods <- 11
m_x <- 'm4'
firms.todo <- c('qualtrics')


fits.file <- sprintf('%s/fit_%s_pd%s_R%s_%s.rds', results_dir, name_i, nPeriods, R, m_x)
if (! file.exists(fits.file)) 
{
  fits.file <- sprintf('%s/fit_%s_pd%s_R%s_%s.rds', results_dir, name_i, nPeriods-1, R, m_x)
}
fits <- readRDS(fits.file)
if (class(fits) == "list" & class(fits[[name_i]]) == "list") {
  fits <- fits[[name_i]][[m_x]]
}

reorder <- c(11, 13, 14, 15, 16, 6, 12, 7, 4, 5, 8, 2, 3, 9, 10)
eff <- fits@effects[, reorder]
n <- fits@nvertices[1,1]
eff.cor <- psych::corr.test(eff, adjust = 'bonferroni')
eff.desc <- psych::describe(eff)

write.csv(eff.cor$r, file = "awareness_qualtrics_m4_results_correlations.csv")
write.csv(eff.cor$p, file = "awareness_qualtrics_m4_results_corr_pvals.csv")
write.csv(eff.desc, file = "awareness_qualtrics_m4_results_describe.csv")






## covars list
mmc <- lapply(nets, function(net) as.matrix(net %n% 'mmc'))
cpc <- lapply(nets, function(net) as.matrix(net %n% 'coop'))
cpp <- lapply(nets, function(net) as.matrix(net %n% 'coop_past'))
cpa <- lapply(nets, function(net) as.matrix(net %n% 'coop') + as.matrix(net %n% 'coop_past') )




