library(igraph)
library(intergraph)
library(btergm)
library(parallel)
library(texreg)


## DIRECTORIES
data_dir <- '/home/sdowning/data/firm_nets_rnr2/firmctrl'
results_dir <- '/home/sdowning/compnet/results/amj_rnr2_firmctrl'


## -----------Model Results Settings-----
name_i <- 'qualtrics'
firm_i <- name_i
d <- 3
R <- 2000
nPeriod <- 11
m_x <- 'm4'
##----------------------------------------






##--------------------
## Table A1
## FOCAL FIRM REPLICATIONS
##--------------------
firms.list <- list(
  c(
    'checkmarket',
    'clarabridge',
    'cloudcherry',
    'confirmit', 
    'customergauge', 
    'cx-index'
  ),
    c(
    'empathica', 
    'feedback-lite', 
    'first-mile-geo', 
    'getfeedback',
    'inqwise', 
    'leaderamp'
  ),
    c(
    'medallia', 
    'myfeelback',    
    'promoter-io',   
    'satmetrix',   
    'snap-surveys-ltd', 
    'super-simple-survey'
  ),
    c(
    'survata',   
    'surveyrock',    
    'typeform',    
    'userate',   
    'verint',    
    'voice-polls' 
  )
)

for (i in 1:length(firms.list))
{
  group <- firms.list[[i]]
  dat <- list()
  for (firm_i in group)
  {
    ## FITTED MODEL RESULTS
    fit_file <- file.path(results_dir, sprintf('fit_%s_pd%s_R%s_%s.rds',firm_i,nPeriod,R,m_x))
    if (!file.exists(fit_file))
      next
    fits <- readRDS(fit_file)
    dat[[firm_i]] <- fits[[firm_i]][[m_x]]
  }

  file.noext <- sprintf('%s/table_A1reps_pd%s_R%s_%s_group%s', results_dir, nPeriod, R, m_x, i)
  htmlreg(dat, file=sprintf('%s.html',file.noext), digits = 2, single.row=TRUE)
  texreg(dat, file=sprintf('%s.txt',file.noext), digits = 2, single.row=TRUE)
}




##--------------------
## Table A2
## CLUSTERING in H1. Diversification
##--------------------
firm_i <- 'qualtrics'
dat <- list()
cls <- c('edgebetween','fastgreedy','infomap','labelprop','walktrap')

for (cl in cls)
{
  fits <- readRDS(file.path(results_dir, sprintf('fit_%s_pd%s_R%s_%s_cl_%s.rds',firm_i,nPeriod,R,m_x,cl)))
  dat[[cl]] <- fits[[firm_i]][[m_x]]
}

## OUTPUT
file.noext <- sprintf('%s/table_A2clust_pd%s_R%s_%s', results_dir, nPeriod, R, m_x)
htmlreg(dat, file=sprintf('%s.html',file.noext), digits = 2, single.row=TRUE)
texreg(dat, file=sprintf('%s.txt',file.noext), digits = 2, single.row=TRUE)


##--------------------
## Table A3
## DECAY in H2. Asymmetric Pressure
##--------------------
firm_i <- 'qualtrics'
dat <- list()
## 1
beta <- 'beta1'
fits <- readRDS(file.path(results_dir, sprintf('fit_%s_pd%s_R%s_%s_%s.rds',firm_i,nPeriod,R,m_x,beta)))
dat[[beta]] <- fits[[firm_i]][[m_x]]
## 2
beta <- 'beta2'
fits <- readRDS(file.path(results_dir, sprintf('fit_%s_pd%s_R%s_%s_%s.rds',firm_i,nPeriod,R,m_x,beta)))
dat[[beta]] <- fits[[firm_i]][[m_x]]
## 3
beta <- 'beta3'
fits <- readRDS(file.path(results_dir, sprintf('fit_%s_pd%s_R%s_%s_%s.rds',firm_i,nPeriod,R,m_x,beta)))
dat[[beta]] <- fits[[firm_i]][[m_x]]
## 4 MAIN RESULTS (Beta 4)
fits <- readRDS(file.path(results_dir, sprintf('fit_%s_pd%s_R%s_%s.rds',firm_i,nPeriod,R,m_x)))
dat[['beta4']] <- fits[[firm_i]][[m_x]]
## 5
beta <- 'beta5'
fits <- readRDS(file.path(results_dir, sprintf('fit_%s_pd%s_R%s_%s_%s.rds',firm_i,nPeriod,R,m_x,beta)))
dat[[beta]] <- fits[[firm_i]][[m_x]]
## OUTPUT
file.noext <- sprintf('%s/table_A3decay_pd%s_R%s_%s', results_dir, nPeriod, R, m_x)
htmlreg(dat, file=sprintf('%s.html',file.noext), digits = 2, single.row=TRUE)
texreg(dat, file=sprintf('%s.txt',file.noext), digits = 2, single.row=TRUE)


##--------------------
## Table A4
## CYCLES & SEPARATION THRESHOLD in H3. Separation
##--------------------
firm_i <- 'qualtrics'
dat <- list()

sep <- 'd2'
fits <- readRDS(file.path(results_dir, sprintf('fit_%s_pd%s_R%s_%s_%s.rds',firm_i,nPeriod,R,m_x,sep)))
dat[[sep]] <- fits[[firm_i]][[m_x]]

sep <- '7cycle'
fits <- readRDS(file.path(results_dir, sprintf('fit_%s_pd%s_R%s_%s_%s.rds',firm_i,nPeriod,R,m_x,sep)))
dat[[sep]] <- fits[[firm_i]][[m_x]]

## OUTPUT
file.noext <- sprintf('%s/table_A4sep_pd%s_R%s_%s', results_dir, nPeriod, R, m_x)
htmlreg(dat, file=sprintf('%s.html',file.noext), digits = 2, single.row=TRUE)
texreg(dat, file=sprintf('%s.txt',file.noext), digits = 2, single.row=TRUE)



