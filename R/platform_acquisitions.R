##########################################################################################
#
# COMPETITION NETWORKS AND ACQUISITIONS ACTIVITY ANALYSIS
#
# @author   Stephen Downing <sdowning.bm02g@nctu.edu.tw>
# @date     May 2015
#
# @depends  comp_net_functions.R, cb_data_prep.R
#
##########################################################################################
setwd("C:/Users/sdowning/Google Drive/PhD/Dissertation/competition networks/compnet")
.libPaths('C:/Users/sdowning/Documents/R/win-library/3.2')
#
library(plyr)
library(dplyr)
library(magrittr)
library(texreg)
library(coefplot2)
# library(devtools)
# library(rcrunchbase)
library(reshape2)
library(ggplot2)
# library(xlsx)
library(igraph)
# library(sna)
# library(network)
library(networkDynamic)
library(stringr)
library(MASS)
library(memisc)
# library(pscl)
# library(AER)
library(psych)
library(glmmADMB)
data_dir <- "C:/Users/sdowning/Google Drive/PhD/Dissertation/crunchbase"
#
source(file.path(getwd(),'R','comp_net_functions.R'))
source(file.path(getwd(),'R','cb_data_prep.R'))
#

#load('acquisitions_data_analysis.RData')
# save.image('acquisitions_data_analysis.RData')

#--------------------------------- ANALYSIS -----------------------------------------------------

##--------------------------------------------------------
## PLOT ACQUISITIONS BY YEAR
##--------------------------------------------------------
tmp <- plyr::count(acq$acquired_year[acq$acquired_year<2016]) %>% sort('x',decreasing=T)
years <- data.frame(year=seq(min(tmp$x),max(tmp$x)))
acq.per.pd <- merge(years,tmp,by.x='year',by.y='x',all=T)
names(acq.per.pd) <- c('year','acq_count')
acq.per.pd$acq_count[is.na(acq.per.pd$acq_count)] <- 0

# ## SAVE PLOT
# png('acquisitions_per_year.png',res=200,units='in',height=4,width=7)
# par(mfrow=c(1,2))
# plot(acq.per.pd$year,acq.per.pd$acq_count, type='o', col='darkblue',pch=19,
#      main="Acquisition Count per Year",xlab='year',ylab='Acquisitions')
# abline(v=c(2001,2008.5),lty=2)
# plot(acq.per.pd$year,log(acq.per.pd$acq_count), type='o', col='darkblue',pch=19,
#      main="Ln Acquisition Count per Year",xlab='year',ylab='Ln Acquisitions')
# abline(v=c(2001,2008.5),lty=2)
# dev.off()

par(mar=c(4,4,3,1))

## ACQ per YEAR
plot(acq.per.pd$year, acq.per.pd$acq_count, type='o', col='darkblue',pch=19,
     main="Acquisition Count per Year",
     xlab='year',ylab='Ln Acquisitions')

##  ACQUISITIONS BY YEAR on LOG SCALE
par(mfrow=c(1,1))
plot(acq.per.pd$year, acq.per.pd$acq_count, log='y', type='o', col='darkblue',pch=19,
     main="Acquisition Count per Year",
     xlab='year',ylab='Acquisitions (Ln scale)')

## LOG ACQUISITIONS BY YEAR
par(mfrow=c(1,1))
sub <- subset(acq.per.pd, year>=1986)
plot(sub$year, log(sub$acq_count), type='o', col='darkblue',pch=19,
     main="Ln Acquisitions per Year",
     xlab='year',ylab='Ln Acquisitions')
#abline(v=c(2001,2008.5),lty=2)
f1 <- lm( log(sub$acq_count) ~ sub$year);  abline(f1, xpd=F)


#----------------------------------------------------------
#  Illustration of dynamic comp net plot
#----------------------------------------------------------
png('example_dynamic_comp_net_with_acquisitions.png',res=200,units='in',height=6.5,width=6.5)
  getExampleDynamicCompNet(output = T)
dev.off()



########################################################################
#   Multimarket contact and competition edges multiplex graph
#
# 1. MAKE PERIOD GRAPH
#
#  1.a. compute time-variates (age, funding, number of markets, etc.) as vertex df for period t
#  1.b. remove acquired/closed before period end (founded after period end) companies (check other closed on dates to remove companies, eg, wikipedia, etc.)
#  1.c. make graph from competitor edgelist (only edges with both vertices not removed in 1.b) with vertex df (1.a) for period t
#
#
# 2. ADD MULTIMARKET CONTACT Edges if: 
#
#  2.a. edge (i,j) still in `comp`  (wasn't removed in 1.c:  neither company close/acquired/founded after period end)
#  2.b. MMC edge (i,j) created before end of period t
#
########################################################################

## BRANCHES
brcnt <- plyr::count(br,'company_name_unique') %>% sort('freq', decreasing=T)
mkcnt <- plyr::count(br,'market2') %>% sort('freq',decreasing=T)

par(mar=c(7,3.5,3,1))
tmp<-brcnt[1:40,];barplot(height=tmp$freq,names.arg = tmp$company_name_unique,las=2,log='y',main='Company Branch Counts')
tmp<-mkcnt[1:40,];barplot(height=tmp$freq,names.arg = tmp$market2,las=2,log='y',main='Market Counts')

sprintf('Avg branches per co: %.2f',nrow(br)/length(unique(br$company_name_unique)))

# ## MAKE FIRM-MARKET COMPETITION EDGELIST
# elm <- data.frame(i=NA,j=NA,market=NA,i_created_at=NA,i_updated_at=NA,j_created_at=NA,j_updated_at=NA,created_at=NA)
# row <- 1
# for (m in 1:length(unique(br$market2))) {
#   mar.m <- br$market2[m]
#   br.sub <- br[which(br$market2==mar.m),]
#   for (i in 1:length(unique(br.sub$company_name_unique))) {
#     for (j in 1:i) {
#       co.i <- br.sub$company_name_unique[i]
#       co.j <- br.sub$company_name_unique[j]
#       if (i != j & br.sub$company_name_unique %in% c(co.i,co.j) ) {
#         i_created_at <- br.sub[which(br.sub$company_name_unique==co.i), 'created_at']
#         i_updated_at <- br.sub[which(br.sub$company_name_unique==co.i), 'updated_at']
#         j_created_at <- br.sub[which(br.sub$company_name_unique==co.j), 'created_at']
#         j_updated_at <- br.sub[which(br.sub$company_name_unique==co.j), 'updated_at']
#         created_at <- max(i_created_at,j_created_at)
#         elm[row, ] <- c(co.i,co.j,mar.m,i_created_at,i_updated_at,j_created_at,j_updated_at, created_at)
#         #cat(sprintf('%s %s %s\n',co.i,co.j,mar.m))
#         row = row+1
#       }
#     }
#   }
#   cat(sprintf('finished market %d.%s (%.2f%s)\n',m,mar.m,(m/length(unique(br$market2)))*100,"%"))
# }; elm$created_at_str <- timestamp2date(as.numeric(elm$created_at))
# head(elm, 30)
# write.csv(x = elm, file = 'market_edgelist_spells.csv',sep = ',',row.names = F, col.names = T, na = 'NA')
# save.image(file='img_with_firm_market_edgelist.RData')

## INCIDENCE --> ADJACENCY transform
## A <- I %*% t(I)

gb <- igraph::graph.data.frame(br[,c('company_name_unique','market2')],directed = F)
igraph::make_bipartite_graph(types=c('firm','market'),edges = br[,c('company_name_unique','market2')],directed = F)

##-------------------------------------------------------
## BIPARTITE NETWORK
##-------------------------------------------------------
mode1 <- 'company_name_unique'
mode2 <- 'market2'
market <- 'market2'

getBipartiteGraph <- function(df,mode1,mode2,market) 
{
  created_at <- 'created_at'
  gb <- graph.empty()
  n1 <- length(unique(df[,mode1]))
  n2 <- length(unique(df[,mode2]))
  gb <- add.vertices(gb, nv=n1, attr=list(name=unique(df[,mode1])), type=rep(TRUE, n1))
  gb <- add.vertices(gb, nv=n2, attr=list(name=unique(df[,mode2])), type=rep(FALSE,n2))
  # elv <- c()  #edge list vector
  # for (row in 1:nrow(df)){
  #   elv <- c(elv, df[row,mode1],df[row,mode2])
  # }
  elv <- as.vector(rbind(df[,mode1],df[,mode2]))
  gb <- add.edges(gb, elv, attr=list(created_at=df[,created_at],market=df[,market]))
  E(gb)$created_at <- timestamp2date(as.numeric(E(gb)$created_at))
  return(gb)
}

gb <- getBipartiteGraph(br,'company_name_unique','market2','market2')

gb.inc <- igraph::get.incidence(gb, names=TRUE,sparse = TRUE)
inc <- igraph::graph.incidence(gb.inc)

gb.p <- igraph::bipartite.projection(gb,multiplicity = T,remove.type = T)

elg <- get.edgelist(g)
elaff  <- get.edgelist(gb.p$proj2)


#--------------------- CHOOSE TIME FRAME -------------------

g.time.df <- data.frame(yr=NA,firms=NA,firms_w_branch=NA)
for (i in 1995:2015) {
  gsub <- induced.subgraph(g.lcc, vids=V(g.lcc)[which(V(g.lcc)$founded_year <= i)])
  gsubd <- delete.vertices(gsub, v=V(gsub)[which(degree(gsub)==0)])
  gsubd.names <- V(gsubd)$name[which(V(gsubd)$name %in% br$company_name_unique)]
  tmp <- data.frame(yr=i,firms=vcount(gsubd),firms_w_branch=length(gsubd.names))
  g.time.df <- rbind(g.time.df,tmp)
}; g.time.df <- na.omit(g.time.df)
print(g.time.df)
par(mar=c(4.1,4.1,3,1))
matplot(x=g.time.df$yr,y=as.matrix(g.time.df[,2:3]),type='o',pch=19)
write.csv(g.time.df,'compnet_company_subset_year_firm_count_with_branches.csv')


##--------------------------------------------------------
## 
##   MAIN REGRESSION VARIABLES PREPARATION LOOP
## 
## BUILD REGRESSION PANEL DATAFRAME BY LOOPING THROUGH PERIODS
## AND COMPUTING OUTCOME VARIABLE (ACQUISITION COUNT) 
## AND NETWORK PREDICTOR VARIABLES
##--------------------------------------------------------
yrpd <- 2
startYr <- 2005
endYr <- 2015
periods <- seq(startYr,endYr,yrpd)
company.name <- 'company_name_unique'
acqExperienceFields <- c('acq_exp', 'acq_count')
verbose <- TRUE
df.in <- co.acq
g <- makeGraph(comp = comp, vertdf = co)
g.lcc <- getLcc(g)
print(g); print(g.lcc)

## t starts at 3 because:  
##    periods[t-2]~periods[t-1]==experience @ periods[t] = PANEL_t
##    t-2 >= 1 ===> t >= 3
acq.l <- list(); lcc.l <- list()
# Start with BASE vertex attributes 
# Non-varying attributes
# name, HQ region, country, categories

for (t in 1:length(periods)) {
  # 1. MAKE PERIOD GRAPH
  
  # 1.a. compute time-variates (age, funding, number of markets, etc.) as vertex df for period t
    # age (diff year_t - year_founded)
  
  
    # funding (sum)
  
  
    # market count (concat string?)
  
  
    # competitor count  (concat string?)
  
  
    # OUTCOME:  Acquisition count
  
  
  #  1.b. remove acquired/closed before period end (founded after period end) companies (check other closed on dates to remove companies, eg, wikipedia, etc.)

  
  #  1.c. make graph from competitor edgelist (only edges with both vertices not removed in 1.b) with vertex df (1.a) for period t

  

  # 2. ADD MULTIMARKET CONTACT Edges to graph if: 
    #  2.a. Competition edge (i,j) firms i & j still both in graph  (wasn't removed in 1.b/1.c:  neither company close/acquired/founded after period end)
 
  
    #  2.b. MMC edge (i,j) created_at before end of period t
  
  
}









for(t in 3:length(periods)) {
    ##--------------
    ## GET `acq_exp`  :  ACQUSITION EXPERIENCE <- SUM OF PAST ACQUISITIONS   (to be lagged?)
    ##     `acq_count`:  DEPENDENT VARIABLE
    ##--------------
    df.acq <- df.in
    df.acq$period <- as.factor(periods[t])
    for(field in acqExperienceFields) {
        df.acq <- getAcqCountsByPeriod( count.df=df.acq
            , count.name= company.name
            , new.count.field=field
            , start=periods[t-2]
            , end=periods[t-1]
            , to.merge.df=df.acq
        )  
    }
    ##---------------
    ## FIX ACQ_EXP COMPUTATION: previous period(s)
    ##---------------
    
    
    ##---------------
    ## GET FUNDING TOTAL up to & including this period
    ##---------------
    
    
    ##---------------
    ## GET AGE up to & including this period(year)
    ##---------------
 
  
    ##---------------
    ## GET BRANCH LOCATIONS for those opened up to & including this period
    ##---------------   
    
    
    ##---------------
    ## 
    ##---------------
    
    ## GET PERIOD SUBSET GRAPH (remove firms closed before `end` or founded after `end`)
    g.sub <- makePdSubgraph(g=g, end=periods[t]) 
    ##---------------
    ## CREATE PERIOD GRAPH
    ##---------------
    ## CONTRACT AQUIRED VERTICES
    acq.el <- data.frame( 
          acquirer=acq$company_name_unique[which(acq$acquired_at < periods[t])]
        , target=acq$acquired_name_unique[which(acq$acquired_at < periods[t])]
        , stringsAsFactors = FALSE
    )
    g.sub.acq <- getAcquisitionContractedGraph( 
          g.sub
        , acquirer = acq.el$acquirer
        , target = acq.el$target
    ) 
    ## GET LARGEST COMPONENT
    lcc <- getLcc(g.sub.acq) 
    ## GLOBAL NETWORK VERTEX & EGO NETWORK PROPERTIES
    lcc.l[[t-2]] <- getNetworkProperties(lcc)
    ## GET DATAFRAME OF NETWORK ATTRIBUTES
    df.net <- getNetworkPropertiesDataframe(lcc.l[[t-2]])
    ##----------------
    ## MERGE NETWORK VARIABLES INTO PERIOD REGRESSION DATAFRAME
    ##----------------
    df.net.cols <- names(df.net)[which( !(names(df.net) %in% c('age','acquired_at')) )]
    df.acq.cols <- names(df.acq)[which( !(names(df.acq) %in% c('permalink','company_name','homepage_url','funding_total_usd')) )]
    acq.l[[t-2]] <- merge(df.acq[,df.acq.cols], df.net[,df.net.cols], by=company.name, all=T)

    if(verbose)
        cat(sprintf('\ncompleted period: %s',periods[t]))
}



# NAME LISTS
names(acq.l) <- periods[3:length(periods)] %>% sort(decreasing=F)  # name each period by ending year (remove first start year)
names(lcc.l)<- periods[3:length(periods)] %>% sort(decreasing=F)

# EXAMINE LISTS
par(mfrow=c(1,1),mar=c(4.1,4.1,3,1))
df.g <- data.frame(v=sapply(lcc.l,vcount),e=sapply(lcc.l,ecount))
matplot(x=as.numeric(rownames(df.g)),y=log(df.g), type='o')
summary(acq.l)


##--------------------------------------------------------
## LIST OF DFs to PANEL DATA DF
##--------------------------------------------------------
df.panel <- list2paneldf(acq.l)

# OUTPUT PANEL DATA TABLE
file.name <- file.path(getwd(),'..','acquisitions',sprintf('cb_compnet_panel_dataframe_%syrpd.csv',yrpd))
write.table(x = df.panel, file = file.name, sep = ',', row.names = F, col.names = T)
sprintf('completed yrpd: %s',yrpd)


#--------------------------------------------------------
#  RESULTS INSPECTION  (sanity check~~)
#--------------------------------------------------------
show.cols <- c('company_name_unique','period','acq_count','acq_exp','funding_total_usd.x','funding_total_usd','age','ego_size','constraint','ego_density')
show.cols <- show.cols[which(show.cols %in% names(df.panel))]
tmp <- df.panel[order(df.panel$acq_count,decreasing=T),show.cols]
View(head(tmp,50))

#--------------------------------------------------------
#  REMOVE NA
#--------------------------------------------------------
df.pna <- na.omit(df.panel)
df.pna <- droplevels(df.pna)

##--------------------------------------------------------
## ACQUISITION COUNTS FULL PANEL OVER PERIODS
##--------------------------------------------------------
# tmp <- data.frame(
#   value=df.panel$acq_count,
#   name=df.panel$company_name_unique,
#   variable=df.panel$period
# )
cnt <- plyr::count(df = acq,vars=c('company_name_unique','acquired_year'))
cnt.w <- dcast(cnt,freq,value.var = 'freq',fun.aggregate = sum)
df.panel.wide <- dcast(tmp, name ~ variable, sum, margins=c('name','variable'),fill = 0)




##--------------------------------------------------------
## GLMM ON NA OMITTED DATAFRAME
##------------------------------------------------------------

reg.vars <- c('acq_count','acq_exp','ego_size','ego_density','period','company_name_unique','age','funding_total_usd.x','constraint')
df.pna.reg <- na.omit(df.pna[,which(names(df.pna)%in%reg.vars)])
#
samp <- sample(seq_len(nrow(df.pna.reg)),size = 1000,replace = F)
# df.pna.reg.samp <- df.pna.reg[samp,]
df.pna.reg.samp <- df.pna.reg
#
dim(df.pna.reg.samp)
cnt <- plyr::count(df.pna.reg.samp$acq_count)
barplot(height=cnt$freq,names.arg = cnt$x)
pairsMod(df.pna.reg.samp, yName='acq_count')
#
formula <- acq_count ~ age + (1|company_name_unique)
formula <- acq_count ~ age + constraint + (1|company_name_unique)
formula <- acq_count ~ age + ego_size + ego_density + constraint + (1|company_name_unique)
formula <- acq_count ~ age + ego_size + ego_density + constraint + log(1+funding_total_usd.x) + (1|company_name_unique)
fit4.1 <- glmmADMB::glmmadmb(formula=formula, data=df.pna.reg.samp, 
                           family="nbinom1", #family="nbinom", 
                           link="log", corStruct="diag", zeroInflation=TRUE, 
                           #start=list(fixed=0,pz=0.05,log_alpha=.5,RE_sd=0.25,RE_cor=0.0001,u=0),
                           admb.opts=admbControl(maxfn=50000),
                           mcmc=FALSE, mcmc.opts=mcmcControl(mcmc = 10000, mcmc2=0, mcnoscale = FALSE, mcgrope = FALSE, mcmult = 1),
                           save.dir=file.path(getwd(),'glmmadmb'), verbose=FALSE, #extra.args="",
                           bin_loc=NULL,debug=TRUE, extra.args="-ndi 50000")

screenreg(list(m1=fit1, m2=fit2, m3=fit3, m4=fit4, m41=fit4.1))


re <- list2paneldf(ranef(fit1)) %>% as.data.frame()
re$name <- rownames(re)
re$intercept <- re$`(Intercept)`
re <- sort(re, decreasing=T)

x[x$company_name_unique==levels(x$company_name_unique)[4],]

##--------------------------------------------------------
## GLMM
##--------------------------------------------------------

reg.vars <- c('acq_count','ego_size','ego_density','period','company_name_unique','age','funding_total_usd','constraint')
df.panel.reg <- na.omit(df.panel[,which(names(df.panel)%in%reg.vars)])
#
samp <- sample(seq_len(nrow(df.panel.reg)),size = 1000,replace = F)
df.panel.reg.samp <- df.panel.reg[samp,]
#
dim(df.panel.reg.samp)
cnt <- plyr::count(df.panel.reg.samp$acq_count)
barplot(height=cnt$freq,names.arg = cnt$x)
pairsMod(df.panel.reg.samp, yName='acq_count')
#
formula <- acq_count ~ age + ego_size + ego_density + constraint + funding_total_usd + (1|period)
fit5 <- glmmADMB::glmmadmb(formula=formula, data=df.panel.reg.samp, 
                   family="nbinom", link="log", corStruct="full", zeroInflation=TRUE, 
                   #start=list(fixed=0,pz=0.05,log_alpha=.5,RE_sd=0.25,RE_cor=0.0001,u=0),
                   admb.opts=admbControl(maxfn=30000),
                   mcmc=FALSE, mcmc.opts=mcmcControl(mcmc = 10000, mcmc2=0, mcnoscale = FALSE, mcgrope = FALSE, mcmult = 1),
                   save.dir=file.path(getwd(),'glmmadmb'), verbose=FALSE, #extra.args="",
                   bin_loc=NULL,debug=TRUE, extra.args="-ndi 30000")

screenreg(list(m2=fit1.z,m3=fit3,m4=fit4,m5=fit5))

# ##--------------------------------------------------------
# ## VISUALIZE NETWORK PROPERTIES RELATIONSHIPS
# ##--------------------------------------------------------
# df.net <- data.frame(auth=V(lcc)$authority,
#                      bet=V(lcc)$between,
#                      clo=V(lcc)$closeness,
#                      cons=V(lcc)$constraint,
#                      ecc=V(lcc)$eccentricity,
#                      eig=V(lcc)$eigen,
#                      size=log(V(lcc)$egosize))
# set.seed(1111)
# pairs(df.net[sample(nrow(df.net)%>%seq_len(),1000), ]%>%na.omit(),
#       lower.panel = panel.smooth,diag.panel = panel.hist,upper.panel=panel.cor)



