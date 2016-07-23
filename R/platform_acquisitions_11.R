##########################################################################################
#
# COMPETITION NETWORKS AND ACQUISITIONS ACTIVITY
#
##########################################################################################
setwd("C:/Users/sdowning/Google Drive/PhD/Dissertation/competition networks/acquisitions")
load('aom_analysis.RData')
# save.image('aom_analysis.RData')

library(plyr)
library(dplyr)
library(magrittr)
library(texreg)
library(coefplot2)
# library(devtools)
# library(rcrunchbase)
library(reshape2)
library(ggplot2)
library(xlsx)
library(igraph)
# library(sna)
library(network)
library(stringr)
library(MASS)
library(memisc)
library(pscl)
library(AER)
library(psych)
library(glmmADMB)
data_dir <- "C:/Users/sdowning/Google Drive/PhD/Dissertation/crunchbase"

#--------------------------------- FUNCTIONS ------------------------------------------------------
par(mfrow=c(2,2),mar=c(.5,.5,.5,.5))
w <- graph.data.frame(d=data.frame(
  s=c(1,1,1,2,3,3,4,4,5),
  t=c(2,3,5,4,2,4,6,5,6)
), directed = F)
E(w)$weight <- 1
plotNet(w)
# 6 --A--> 1: 
acquirer <- 6
target <- 1
mapping <- seq(1,6)
mapping[mapping==target] <- acquirer
w2 <- igraph::contract(w,mapping = mapping, vertex.attr.comb=function(x)vertCombNames(x,acquirer))
plotNet(w2)
w3 <- simplify(w2, remove.multiple = T, remove.loops = T, edge.attr.comb=list(weight='sum',name='ignore'))
plotNet(w3)

###
# UPDATE GRAPH FROM ACQUISITIONS: 
#     ACQUIRER ABSORBS TIES TO TARGET'S COMPETITORS
##
getAcquisitionContractedGraph(g,acquirer,target)
{
  mapping <- replace(V(g), target, acquirer)

  g <- igraph::contract(g,mapping = mapping, vertex.attr.comb=function(x)vertCombNames(x,acquirer))
  g <- simplify(g, remove.multiple=T,remove.loops=T, edge.attr.comb=list(weight='sum',name='ignore'))
  return(g)
}

### 
# KEEP NAME OF AQUIRER WHEN CONTRACTING VERTICES FOR ACQUISITION `vertex.attr.comb`
##
vertCombNames <- function(x, acquirer, concat=FALSE)
{
  if(concat) {
    out <- ifelse(length(x)==1,x,sprintf('%s (%s)',x[which(x==acquirer)],paste(x[which(x!=acquirer)],collapse=','))) 
  } else {
    out <- ifelse(length(x)==1,x,x[which(x==acquirer)])
  }
  return(out)
}

###
#  WRAPPER FOR igraph PLOTTING
##
plotNet <- function(g,...)
{
  igraph::plot.igraph( g
    , layout=layout.circle
    , vertex.color='steelblue'
    , vertex.label.color='black'
    , vertex.size=log(degree(g))*15
    , edge.label=E(g)$weight
    , edge.width=E(g)$weight*2
  )
}

###
# MAP A VECTOR X PROPORTIONALLY TO NEW RANGE (Y1, Y2)
##
map <- function(x,y1,y2)
{
  dist <- diff(range(x))
  ymin <- min(y1,y2)
  ymax <- max(y1,y2)
  p <- (x - min(x))/dist
  ydist <- ymax - ymin
  y <- ymin + p*ydist
  return(y)
}
###
# GET ZERO, NON-ZERO PROPORTIONS
##
zprop <- function(x,digits=3)
{
  z <- ifelse(x>0,1,0)
  s <- sum(z)
  l <- length(z)
  df <- data.frame( p1=s/l, p0=1-(s/l) )
  return(round(df,digits))
}
###
# PAIRS CORR PLOT HELPER FUNCTIONS
##
panel.cor <- function(x, y, digits=2, cex.cor)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  #txt <- format(c(r, 0.123456789), digits=digits)[1]
  test <- cor.test(x,y)
  Signif <- ifelse(round(test$p.value,3)<0.001,"p<0.001",paste("p=",round(test$p.value,3)))
  text(0.5, 0.25, sprintf('r = %.2f',r), cex=1.4)
  text(.5, .75, Signif, cex=1.4)
}
panel.smooth<-function (x, y, col = "steelblue", bg = NA, pch = 18,cex = 0.8, 
                        col.smooth = "red", span = 2/3, iter = 3)
{
  points(x, y, pch = pch, col = col, bg = bg, cex = cex)
  ok <- is.finite(x) & is.finite(y)
  if (any(ok))
    lines(stats::lowess(x[ok], y[ok], f = span, iter = iter),
          col = col.smooth)
}
panel.hist <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col="steelblue", ...)
}
###
# ENHANCED pairs() FUNCTION
# SAMPLE, LOG TRANSFORM, DROP FACTOR VARS, OMIT NA, PLOT SMOOTH,HIST,CORR
##
pairsMod <- function(x,yName='acq_count',sampSize=2000,output=FALSE,naOmit=FALSE,dropNames=c('company_name_unique','period'),seed=1111)
{
  yIndex <- which(names(x)==yName)
  x <- x[,which(!(names(x)%in%dropNames))]
  x[,yName] <- log(x[,yName]+1e-6)
  names(x)[which(names(x)==yName)] <- sprintf('ln_%s',yName)
  if(naOmit)
    x <- na.omit(x)
  set.seed(seed)
  samp <- sample(seq_len(nrow(df.panel.reg)),sampSize)
  if(length(nrow(x))==0)
    return('zero non-NA data to plot')
  pairs(x[samp,],lower.panel=panel.smooth,
        diag.panel=panel.hist,upper.panel=panel.cor)
  if(output) 
    return(x)
}
###
# MONEY STRING (WITH UNITs ABBREVIATIONS) TO INTEGER
##
cleanMoney <- function(values)
{
  x <- gsub('[$]','',values)
  cleaned   <- ifelse(grepl('B',x), as.numeric(gsub('B','',x))*1e9,
                      ifelse(grepl('M',x),as.numeric(gsub('M','',x))*1e6,
                             ifelse(grepl('k',x),as.numeric(gsub('k','',x))*1e3,
                                    as.numeric(x) ) ) )
  return(cleaned)
}
###
# UNIX EPOCH TO DATE (posix.ct)
##
timestamp2date <- function(stamp, asDate=FALSE)
{
  posix <- as.POSIXct(stamp, origin="1970-01-01")
  if(asDate) return(as.date(posix))
  return(posix)
}
###
# CONCATENATE LIST OF DFs into PANEL DATA DF
##
list2paneldf <- function(li)
{
  return(do.call('rbind', li))
}
###
# ADD ACQUISITION COUNTS PER PERIOD (from `count.df`)  TO COMPANY DATAFRAME `to.merge.df`
##
getAcqCountsByPeriod <- function(count.df, start, end, to.merge.df, 
                                 count.name='company_name_unique', new.count.field='acq_count',
                                 pdName='acquired_year')
{
  if(!(count.name%in%names(count.df)))
    return(NA)
  ## PERIOD COUNTS
  df.sub <- acq[which( acq[,pdName]>=start & acq[,pdName]<end ), ]
  df.sub.count <- count(df.sub[,count.name])
  names(df.sub.count) <- c(count.name,new.count.field)
  # MERGE
  to.merge.df <- merge(to.merge.df,df.sub.count,by=count.name,all.x=T)
  fillIndices <- which( is.na(to.merge.df[,new.count.field]) | to.merge.df[,new.count.field]=='' )
  to.merge.df[fillIndices, new.count.field] <- 0
  return(to.merge.df)
}
###
# CREATE GRAPH FROM COMPETITIVE RELATIONS IN `comp` DATAFRAME
##
makeGraph <- function(comp,vertdf,name='company_name_unique', 
                      compName='competitor_name_unique', relationPdName='relation_created_at',
                      datesVec=c('founded_at','founded_month','founded_quarter','founded_year','acquired_at','closed_at') )
{
  el <- data.frame(source=comp[,name], target=comp[,compName],
                   relation_created_at=comp[,relationPdName], stringsAsFactors = F)
  verts <- data.frame(company_name_unique=unique(c(el$source,el$target)), stringsAsFactors = F)
  verts <- merge(x=verts,y=vertdf[,c(name,datesVec[datesVec%in%names(vertdf)])],
                 by=name,all.x=T,all.y=F)  
  g <- igraph::graph.data.frame(d = el, directed = F, vertices = verts)
  return(g)
}
###
# CREATE SUBGRAPH WITH VERTICES EXISTING 
#       & NOT REMOVED (closed,acquired) 
#       BEFORE `end` DATE
#       FROM  GRAPH BUILT WITH makeGraph()
##
makePdSubgraph <- function(g,end,pdAttr='founded_at',acqPdAttr='acquired_at',closedPdAttr='closed_at')
{
  vertexAttrs <- names(igraph::vertex.attributes(g))
  keepVids <- c()
  removeVids <- c()
  # g.sub <- igraph::delete_edges(graph=g,edges = E(g)[which(E(g)$relation_created_at>end )])
  ##  KEEP VERTICES (EDGES) IF   FOUNDED **BEFORE** `end`
  if(pdAttr %in% vertexAttrs){
    tmp <- igraph::get.vertex.attribute(g,pdAttr)
    pdVids <- V(g)[which( !is.na(tmp) & tmp < end)]
    keepVids <- c(keepVids, pdVids )
  }
  ##  REMOVE VERTICES (EDGES) IF   ACQUIRED BEFORE `end` 
  if(acqPdAttr %in% vertexAttrs) {
    tmp <- igraph::get.vertex.attribute(g,acqPdAttr)
    acqPdVids <- V(g)[which( !is.na(tmp) & tmp < end)]
    removeVids <- c(removeVids, acqPdVids )    
  }
  ##  REMOVE VERTICES (EDGES) IF   CLOSED BEFORE `end`
  if(closedPdAttr %in% vertexAttrs) {
    tmp <- igraph::get.vertex.attribute(g,closedPdAttr) 
    closedPdVids <- V(g)[which( !is.na(tmp) & tmp < end)]
    removeVids <- c(removeVids, closedPdVids )    
  }
  ## INDUCE SUBGRAPH
  uKeepVids <- unique(keepVids)
  uRemoveVids <- unique(removeVids)
  vids <- uKeepVids[which( !(uKeepVids %in% uRemoveVids) )]
  g.sub <- igraph::induced.subgraph(graph=g,vids=vids)
  return(g.sub)
}
###
# GET LARGEST CONNECTED COMPONENT FROM LIST OF DECOMPOSED SUBGRPAHS
##
getLcc <- function(comps)
{
  return(comps[[which.max(sapply(comps,vcount))]])
}
###
# GET GLOBAL NETWORK VERTEX PROPERTIES 
##
getNetworkProperties <- function(lcc)
{
  # V(lcc)$alpha <- igraph::alpha.centrality(lcc)
  V(lcc)$authority <- igraph::authority.score(lcc)$vector
  V(lcc)$between <- igraph::betweenness(lcc,directed=F,normalized = F)
  V(lcc)$lnbetween <- log(V(lcc)$between)
  # V(lcc)$bonpow <- igraph::bonpow(lcc)
  V(lcc)$closeness <- igraph::closeness(lcc)
  V(lcc)$constraint <- igraph::constraint(lcc)
  V(lcc)$eccentricity <- igraph::eccentricity(lcc)
  V(lcc)$eigen <- igraph::eigen_centrality(lcc)$vector
  # V(lcc)$edgecomm <- igraph:edge.betweenness.community(lcc)$membership
  V(lcc)$labelcomm <- igraph::label.propagation.community(lcc)$membership
  V(lcc)$walkcomm <- igraph::walktrap.community(lcc)$membership
  ## EGO NET PROPERTIES
  l.ego <- igraph::make_ego_graph(lcc,order = 1)
  V(lcc)$ego_size <- laply(l.ego,vcount)
  V(lcc)$ego_density <- laply(l.ego,function(x) ecount(x)/(vcount(x)*(vcount(x)-1)) )
  V(lcc)$ego_clustering <- laply(l.ego,function(x)igraph::transitivity(x,type='globalundirected'))
  #
  sprintf('diameter = %d; avg.path = %.3f',diameter(lcc),average.path.length(lcc))
  return(lcc)
}
###
# GET DATAFRAME OF NETWORK VERTEX ATTRIBUTES FROM IGRAPH OBJECT `lcc` (largest component)
##
getNetworkPropertiesDataframe <- function(lcc, name='company_name_unique',what='vertices', 
                                          date.fields.exclude=c('founded_at','founded_month',
                                                                'founded_quarter','founded_year')) 
{
  df.net <- igraph::get.data.frame(x=lcc,what=what)
  df.net[,name] <- row.names(df.net)
  df.net <- df.net[ ,which(!(names(df.net)%in%date.fields.exclude))]
  return(df.net)
}
#--------------------------------- ANALYSIS -----------------------------------------------------

##--------------------------------------------------------
## LOAD DATA; CLEAN DATA
##---------------------------------------------------------
file.companies <- file.path(data_dir,'cb_export_with_competitors_20160106_companies.csv')
co <- read.table(file.companies, sep=",",header=T, quote='"', stringsAsFactors = F, fill=T)
co$funding_total_usd <- as.numeric(gsub('[-]','0',gsub('[, ]','',co$funding_total_usd)))

file.acquisitions <- file.path(data_dir,'cb_export_with_competitors_20160106_acquisitions.csv')
acq <- read.table(file.acquisitions, sep=",",header=T, quote='"', stringsAsFactors = F, fill=T)
names(acq)[which(names(acq)=='name')] <- 'company_name_unique'

file.competitors <- file.path(data_dir,'cb_export_with_competitors_20160106_competitors.csv')
comp <- read.table(file.competitors, sep=",",header=T, quote='"', stringsAsFactors = F, fill=T)
comp$relation_updated_at_date <- timestamp2date(as.numeric(comp$relation_updated_at))

##-----------------------------------------------------------
## CONVERT FACTORS
##------------------------------------------------------------
co$status <- as.factor(co$status)
co$country_code <- as.factor(co$country_code)
co$state_code <- as.factor(co$state_code)
co$region <- as.factor(co$region)
co$company_name_unique <-  as.factor(co$company_name_unique)

##-----------------------------------------------------------
## COMPUTE REGRESSION VARIABLES
##------------------------------------------------------------
co$age <- 2016 - co$founded_year


# ##--------------------------------------------------------
# ## SUBSET COMPANIES (regression df) THAT HAVE AT LEAST ONE COMPETITIVE TIE
# ## WILL NEED TO SELECT ONLY LARGEST CONNECTED COMPONENT
# ##--------------------------------------------------------
# names.u <- unique(c(comp$company_name_unique,comp$competitor_name_unique))
# co.c <- co[which(co$company_name_unique%in%names.u), ]


# ##-------------------------------------------------------
# ##  ADD ACQUIRED DATES TO REMOVE COMPANIES FROM COMPETITION NETWORK
##-------------------------------------------------------
tmp <- acq[,c('acquired_name_unique','acquired_at','company_name_unique')]
names(tmp) <- c('company_name_unique','acquired_at','acquirer_company_name_unique')
co.acq.dup <- merge(co,tmp, by='company_name_unique', all.x=T,all.y=F)
co.acq <- co.acq.dup[!duplicated(co.acq.dup$company_name_unique),]

## check REMOVED DUPLICATES
cnt <- count(co.acq$company_name_unique) %>% sort('freq',decreasing=T)
dups <- cnt$x[cnt$freq>1]
length(dups) == 0

##------------------------------------
##
## NEED CLOSED_ON DATES HERE...
##
##------------------------------------
##-------------------------------------------------------
##  ADD ClOSED DATES TO REMOVE COMPANIES FROM COMPETITION NETWORK
##-------------------------------------------------------
co$closed_at  <- '' ###


##--------------------------------------------------------
## PLOT ACQUISITIONS BY YEAR
##--------------------------------------------------------
tmp <- count(acq$acquired_year[acq$acquired_year<2016]) %>% sort('x',decreasing=T)
years <- data.frame(year=seq(min(tmp$x),max(tmp$x)))
acq.per.pd <- merge(years,tmp,by.x='year',by.y='x',all=T)
names(acq.per.pd) <- c('year','acq_count')
acq.per.pd$acq_count[is.na(acq.per.pd$acq_count)] <- 0
# ## SAVE PLOT
# png('acquisitions_per_year.png',res=200,units='in',height=4,width=7)
#   par(mfrow=c(1,2))
#   plot(acq.per.pd$year,acq.per.pd$acq_count, type='o', col='darkblue',pch=19,
#        main="Acquisition Count per Year",xlab='year',ylab='Acquisitions')
#   abline(v=c(2001,2008.5),lty=2)
#   plot(acq.per.pd$year,log(acq.per.pd$acq_count), type='o', col='darkblue',pch=19,
#        main="Ln Acquisition Count per Year",xlab='year',ylab='Ln Acquisitions')
#   abline(v=c(2001,2008.5),lty=2)
# dev.off()






########################################################################
#
#
# TO DO:
#
#  1. REMOVE COMPANIES (closed)
#  1. ADD ACQUIRED CO's COMPETITORS INTO ACQUIRER COMPANY's EGONETWORK
#
#
########################################################################

##--------------------------------------------------------
## 
##          MAIN REGRESSION DATA LOOP
## 
## BUILD REGRESSION PANEL DATAFRAME BY LOOPING THROUGH PERIODS
## AND COMPUTING OUTCOME VARIABLE (ACQUISITION COUNT) 
## AND NETWORK PREDICTOR VARIABLES
##--------------------------------------------------------
yrpd <- 2

periods <- c(0,seq(2005,2015,yrpd))
company.name <- 'company_name_unique'
acq.l <- list()
lcc.l <- list()
df.in <- co.acq
g <- makeGraph(comp=comp,vertdf=df.in)
## t starts at 3 because:  
##    periods[t-2]~periods[t-1]==experience @ periods[t] = PANEL_t
##    t-2 >= 1 ===> t >= 3
for(t in 3:length(periods)) {
  ##--------------
  ## GET `acq_exp` :  ACQUSITION EXPERIENCE <- SUM OF PAST ACQUISITIONS   (to be lagged?)
  ##--------------
  df.acq <- getAcqCountsByPeriod(count.df=acq, count.name=company.name, new.count.field='acq_exp', 
                                 start=periods[t-2], end=periods[t-1], to.merge.df = df.in)  
  ##--------------
  ## GET `acq_count` DEPENDENT VARIABLE
  ##--------------
  df.acq <- getAcqCountsByPeriod(count.df=acq, count.name=company.name, new.count.field='acq_count', 
                                 start=periods[t-1], end=periods[t], to.merge.df = df.acq)  
  df.acq$period <- as.factor(periods[t])
  ##--------------
  ## CREATE PERIOD GRAPH
  ##-------------
  ## GET LARGEST COMPONENET
  lcc <- makePdSubgraph(g=g, end=periods[t]) %>%
    igraph::decompose.graph() %>%
    getLcc() 
  ## GLOBAL NETWORK VERTEX & EGO NETWORK PROPERTIES
  lcc.l[[t-1]] <- getNetworkProperties(lcc)
  ## GET DATAFRAME OF NETWORK ATTRIBUTES
  df.net <- getNetworkPropertiesDataframe(lcc.l[[t-1]])
  ##---------------
  ## MERGE NETWORK VARIABLES INTO PERIOD REGRESSION DATAFRAME
  ##---------------
  acq.l[[t-1]] <- merge(df.acq,df.net,by='company_name_unique',all=T)
  ## 
  cat(sprintf('\ncompleted period: %s',periods[t]))
}
names(acq.l) <- periods[-1] %>% sort(decreasing=F)  # name each period by ending year (remove first start year)
names(lcc.l)<- periods[-1] %>% sort(decreasing=F)


df.g <- data.frame(v=sapply(lcc.l,vcount),e=sapply(lcc.l,ecount))
matplot(x=as.numeric(rownames(df.g)),y=log(df.g), type='o')
summary(acq.l)


##--------------------------------------------------------
## LIST OF DFs to PANEL DATA DF
##--------------------------------------------------------
df.panel <- list2paneldf(acq.l)

write.table(x = df.panel, file = sprintf('panel_dataframe_%syrpd.csv',yrpd), sep = ',', row.names = F, col.names = T)
sprintf('completed yrpd: %s',yrpd)




##--------------------------------------------------------
## ACQUISITION COUNTS FULL PANEL OVER PERIODS
##--------------------------------------------------------
# tmp <- data.frame(
#   value=df.panel$acq_count,
#   name=df.panel$company_name_unique,
#   variable=df.panel$period
# )
cnt <- count(df = acq,vars=c('company_name_unique','acquired_year'))
cnt.w <- dcast(cnt,freq,value.var = 'freq',fun.aggregate = sum)
df.panel.wide <- dcast(tmp, name ~ variable, sum, margins=c('name','variable'),fill = 0)









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
cnt <- count(df.panel.reg.samp$acq_count)
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










# #--------------------------------------------------------
# # Load data sources
# #---------------------------------------------------------
# co <- read.table("C:/Users/sdowning/Google Drive/PhD/Dissertation/competition networks/acquisitions/cb_companies.csv", sep=",",header=T,quote='"',stringsAsFactors=F,fill=T)
# 
# main <- read.table(file.path(data_dir,"companies_main.csv"), sep=",",header=T, quote='"', stringsAsFactors = F, fill=T)
# main$founded_year <- as.numeric(main$founded_year)
# # main$name <- as.factor(main$name)
# 
# acq <- read.table(file.path(data_dir,"companies_acquisition.csv"), sep=",",header=T, quote='"', stringsAsFactors = F, fill=T)
# # acq$name <- as.factor(acq$name)
# 
# acqcount <- count(acq, vars = c('name'))
# acqcount <- acqcount[order(acqcount[,2],decreasing = T),]
# 
# comp <- read.table(file.path(data_dir,"companies_competitions.csv"), sep=",",header=T, quote='"', stringsAsFactors = F, fill=T)
# # comp$name <- as.factor(comp$name)
# 
# newco <- read.table('crunchbase_export_20160105_companies.csv',sep=",",header=T,stringsAsFactors = F, quote='"',fill=T)
# 
# 
# df <- read.table("platforms_vs_others.csv",sep=",",header=T,fill=T,quote='"', stringsAsFactors = F, na.strings = "")
# df$platform_type <- gsub("[?]","unknown",df$platform_type)
# df$platform_type <- as.factor(df$platform_type)
# 
# # sampUnknown <- sample(df[which(df$platform_type=="unknown"),],
# #                       100,replace = F)
# # df <- df[which(df$platform_type != "unknown"),]
# # df <- rbind(df,sampUnknown)
# # df <- droplevels(df)
# # levels(df$platform_type)
# # dim(df)
# # head(df)
# 
# # df <- merge(df, main[,c('name','founded_year')],by='name',all=T)
# df <- merge(df, na.omit(main),by='name',all=T)
# df$count_acquired[ is.na(df$count_acquired) ] <- 0
# df.full <- df
# 
# df <- df.full[ (!is.na(df.full$founded_year) & !is.na(df.full$name)), ]
# 
# 




#----------------------------------------
#
# Merge multirecordd dataframe
#
#----------------------------------------
# acquired compies multirecord dataframe
dfacq <- merge(df,main[,c('name','founded_year')],by='name',all.x = T)
# dfm <- merge(dfm,co[,c('name','founded_year')],by='name',all.x=T)
dfacq <- merge(dfacq,acq[,c('acquired_company','name')],by='name',all.x=T)

#competitor companies multirecord dataframe
dfcomp <- merge(df[,c('name','platform_type')], comp[,c('name','competitions')],by='name',all.x=T)
countcomp <- count(dfcomp,vars = "name")
names(countcomp)[2] <- 'comp_count'

# "dfacqu
# [1] "name"             "count"
# [3] "platform_type"    "founded_year"
# [5] "acquired_company"

# dfcomp
# [1] "name"          "platform_type"
# [3] "competitions"

#------------------------------------------------------
# identify key observations for app2platform cateogry
# by comanpy's number of acquired competitors
#------------------------------------------------------

# # number of acquired companie who were in same market
# acqcount$acquired_same_market <- NA
# for (i in 1:nrow(acqcount)) {
#     co_i <- as.character(acqcount$name[i])
#     mark_i <- co[which(co$name == co_i),'market']
#     mark_i <- gsub(" ","",mark_i)
#     acq_i <- dfacq[which(dfacq$name == co_i),c('acquired_company')]
#     # for each company j  acquired by i, get the name of the acquired company's market
#     acq_i_mark_j <- c()
#     for (j in 1:length(acq_i)) {
#         s <- co[which(co$name == acq_i[j]), 'market']
#         # some names may match multiple so return all matched markets
#         # add them to vector of acquired markets
#         for (k in 1:length(s)) {
#             acq_i_mark_j[length(acq_i_mark_j)+1] <- ifelse(length(gsub(" ","",s[k]))>0,
#                                                            gsub(" ","",s[k]),
#                                                            NA)
#         }
#     }
#     #acq_markets <- co[which(co$name %in% acq_i),'market']
#     acqcount$acquired_same_market[i] <- sum(mark_i %in% acq_i_mark_j)
#     if (i%%1000==0) cat(paste0('row ',i,'\n'))
# }
# acqcount <- acqcount[order(acqcount[,3],decreasing = T),]
# head(acqcount)
# 
# # dfall$acq_comp_percent <- dfall$acquired_competitors / dfall$comp_count

#-------------------------------------------------------
#
# Prepare regression dataframe
#
#-------------------------------------------------------

dfall <- merge(df,countcomp,by='name',all=T)
co.merge.cols <- c('name','market','founded_at', 'funding_total_usd',
                   'country_code','status', 'state_code','region','city',
                   'funding_rounds','category_list','founded_at')
dfall <- merge(dfall, co[ , co.merge.cols], by='name', all.x=T)
dfall$age <- 2015 - as.numeric(sapply(seq_len(nrow(dfall)), function(x)strsplit(dfall$founded_at[x],'-')[[1]][1]))

#------------------------------------------------------------
# #FIX TOTAL MONEY TO NUMERIC
# # OMIT NOT USD 
dfall.full <- dfall
dfall <- dfall.full[grep('[$]',dfall.full$total_money),]
dfall <- dfall[ !grepl('C',dfall$total_money),]

dfall$total_money <- gsub('[$]','',dfall$total_money)
dfall$usd   <- ifelse(grepl('B',dfall$total_money), as.numeric(gsub('B','',dfall$total_money))*1e9,
                      ifelse(grepl('M',dfall$total_money),as.numeric(gsub('M','',dfall$total_money))*1e6,
                             ifelse(grepl('k',dfall$total_money),as.numeric(gsub('k','',dfall$total_money))*1e3,
                                    as.numeric(dfall$total_money)
                             )
                      )
)

dfall.full <- dfall
dfall <- dfall.full[which(dfall.full$usd>0),]

#----------------------------------------------------------
# 
# # add number of competitors
# # company names list
# names <- unique(df$name)
# 
# 
# # # # number of acquired companies who were competitors of the acquirer
# dfall$acquired_competitors <- NA
# for (i in 1:nrow(dfall)) {
#     co_i <- dfall$name[i]
#     acq_i <- dfacq[which(dfacq$name == co_i),c('acquired_company')]
#     comp_i <- dfcomp[which(dfcomp$name==co_i),c('competitions')]
#     acq_comp_i <- sum(acq_i %in% comp_i)   # count the number of TRUES
#     dfall$acquired_competitors[i] <- acq_comp_i
#     if (i%%1000==0) cat(paste0('row ',i,'\n'))
# }
# # dfall$acq_comp_percent <- dfall$acquired_competitors / dfall$comp_count
# 
# 
# # diversity of acquired companies
# dfall$acquired_markets <- 0
# dfall$acquired_categories <- 0
# dfall$compsamemarket <- 0
# dfall$compdiffmarket <- 0
# dfall$comps_markets <- 0
# 
# for (i in 1:nrow(dfall)) {
#     co_i <- dfall$name[i]
#     acq_i <- dfacq[which(dfacq$name == co_i),c('acquired_company')]
#     acq_markets <- co[which(co$name %in% acq_i),'market']
#     dfall$acquired_markets[i] <- length(unique(acq_markets))
#     #
#     comps_i <- comp[which(comp$name == co_i),c('competitions')]
#     comps_markets <- co[which(co$name %in% comps_i),'market']
#     dfall$comps_markets[i] <- length(comps_markets)
#     m <- co[which(co$name == co_i),'market']
#     dfall$compsamemarket[i] <- length(comps_markets[which(comps_markets == m)])
#     dfall$compdiffmarket[i] <- length(comps_markets[which(comps_markets != m)])
#     #   print(dfall$comps_markets[i])
#     #   print(dfall$compsamemarket[i])
#     #   print(dfall$compdiffmarket[i])
#     
#     # categories list combined
#     acq_categories <- co[which(co$name %in% acq_i),'category_list']
#     if (length(acq_categories) > 0 ) {
#         holder <- c()
#         for (j in 1:length(acq_categories)) {
#             # split list at bar with escape characters []; take first item from list
#             cats_j <- strsplit(acq_categories[j],split = "[|]")[[1]]
#             # drop the empy space entries in vector of categories
#             holder <- c(holder,cats_j[which(cats_j != "")])
#             
#         } #end j loop for all categories within one company i
#         dfall$acquired_categories[i] <- length(unique(holder))
#     } #end if
#     
#     # NEW competitor markets
#     if (i%%1000==0) cat(paste0('row ',i,'\n'))
# } 
# 
# # dfall$market_diversity <- dfall$acquired_markets / dfall$count
# # dfall$category_diversity <- dfall$acquired_categories / dfall$count
# 
# 
# # how many acquired companies are in same market as acquiring co
# dfall$acquired_same_market <- NA
# for (i in 1:nrow(dfall)) {
#     #   co_i <- dfall$name[i]
#     #   mark_i <- co[which(co$name == co_i),'market']
#     #   #mark_i <- gsub(" ","",mark_i)
#     #   acq_i <- dfacq[which(dfacq$name == co_i),c('acquired_company')]
#     #   acq_markets <- co[which(co$name %in% acq_i),'market']
#     #   dfall$acquired_same_market[i] <- sum(mark_i %in% acq_markets)
#     
#     co_i <- as.character(dfall$name[i])
#     mark_i <- co[which(co$name == co_i),'market']
#     mark_i <- gsub(" ","",mark_i)
#     acq_i <- dfacq[which(dfacq$name == co_i),c('acquired_company')]
#     # for each company j  acquired by i, get the name of the acquired company's market
#     acq_i_mark_j <- c()
#     for (j in 1:length(acq_i)) {
#         s <- co[which(co$name == acq_i[j]), 'market']
#         # some names may match multiple so return all matched markets
#         # add them to vector of acquired markets
#         for (k in 1:length(s)) {
#             acq_i_mark_j[length(acq_i_mark_j)+1] <- ifelse(length(gsub(" ","",s[k]))>0,
#                                                            gsub(" ","",s[k]),
#                                                            NA)
#         }
#     }
#     #acq_markets <- co[which(co$name %in% acq_i),'market']
#     dfall$acquired_same_market[i] <- sum(mark_i %in% acq_i_mark_j)
#     if (i%%1000==0) cat(paste0('row ',i,'\n'))
# }
# View(head(dfall,20))
# 
# # write regression data to file
# write.table(dfall,"regression_all3k_df.csv",sep=",",col.names = T,row.names = F)

#-------------------------------------------------
#
#  Build network of competition
#
#--------------------------------------------------
compnames <- unique(c(as.character(comp$name),
                      as.character(comp$competitions)))
# vertices <- merge(x=main[which(main$name %in% compnames),
#                          c('name','founded_year','number_of_employees',
#                            'total_money')],
#                   y=co[which(co$name %in% compnames),
#                        c('name','market','founded_at',
#                          'funding_total_usd','country_code','status')],
#                   by='name', all=T)
vertices <- merge(x=dfall[which(dfall$name %in% compnames), ],
                  y=co[which(co$name %in% compnames),
                       c('name','market','founded_at',
                         'funding_total_usd','country_code','status', 
                         'state_code','region','city','funding_rounds',
                         'category_list')],
                  by='name', all=T)
vertices <- vertices[which(!duplicated(vertices$name)), ]
discrep <- compnames[!(compnames %in% as.character(vertices$name)) ]
discrepdf <- data.frame(name=discrep)
discrepdf[,c(2:(ncol(vertices)))] <- NA
names(discrepdf) <- names(vertices)
vertices <- rbind(vertices,discrepdf)

# build graph object and add vertex attributes manually
g.full <- graph.data.frame(d=comp[,c('name','competitions')],directed = FALSE,vertices=vertices)

## GET LARGEST CONNECTED COMPONENT
g.decomp <- decompose.graph(g.full, mode = 'weak', min.vertices = 2)
g1 <- g.decomp[[ which.max(sapply(g.decomp,vcount)) ]]

# iteratively add attributes if more edges than attributes info
# for (i in 1:vcount(g1)) {
#   if (V(g1)$name[i] %in% vertices$name) {
#     for (j in 2:ncol(vertices)) {
#       g1 <- igraph::set.vertex.attribute(graph=g1,
#                                          name=names(vertices)[j],
#                                          index=i,
#                                          value=vertices[i,j])
#     }
#   }
#   if(i%%1000==0){cat(paste("\nadded vertex",i))}
# }




V(g1)$constraint <- igraph::constraint(g1)
V(g1)$betweenness <- igraph::betweenness(g1,normalized = T)
# V(g1)$eigen <- igraph::eigen_centrality(g1)$value
V(g1)$degree <- igraph::degree(g1)


# df.top <- dfall[,c('name','density','overlap','count')]
# df.top <- df.top[order(df.top[,'density'],decreasing =F),]
# head(df.top,20)


# CENTRAL IMPORTANCE
degdf <- data.frame(name=V(g1)$name, 
                    degree=V(g1)$degree, 
                    between=V(g1)$betweenness,
                    constraint=V(g1)$constraint)
degdf <- merge(degdf, acqcount[,c('name','freq')], by='name',all.x=T)
degdf$freq[is.na(degdf$freq)] <- 0
degdf <- degdf[order(degdf[,'freq'],decreasing = T),]
names(degdf)[which(names(degdf)=='freq')] <- 'count'
head(degdf, 20)
degdf <- degdf[order(degdf[,2],decreasing = T),]
head(degdf, 20)
degdf <- degdf[order(degdf[,'between'],decreasing = T), ]
head(degdf, 20)

degdf$central_importance <- (degdf$between / degdf$degree) / sum(degdf$between / degdf$degree)
degdf <- degdf[order(degdf[,'central_importance'],decreasing = T),]
head(degdf, 20)
degdf <- degdf[order(degdf[,'constraint'],decreasing = F),]
head(degdf, 20)
transitivity()
# PLOT IMPORTANT COMPANIES ---------------------------------------

degdf$nl_between <- -log(degdf$between)

png('constraint_betweenness_comapre_var.png',height=6.5, width=6.5, units = 'in', res=400)
par(mfrow=c(2,1), xpd=FALSE, mar=c(4,4.2,1,1))

fit1a <- glm.nb(count ~ poly( nl_between,1), init.theta=.05, data =degdf[which(degdf$nl_between < Inf),]) # degdf[which(degdf$count>0),])
fit2a <- glm.nb(count~ poly( nl_between,2), init.theta=.05, data= degdf[which(degdf$nl_between < Inf), ]) #degdf[which(degdf$count>0),])

degdf <- degdf[order(degdf$nl_between),]
plot(degdf$nl_between, log(degdf$count), 
     cex= degdf$degree*.005,   #degdf$nl_between*50 , 
     pch=21, col='black', bg='steelblue',
     ylab='Ln Acquisitions (A)', 
     xlab='Negative Ln Normalized Betweenness Centrality (B)')
lines(x=degdf$nl_between[which(degdf$nl_between < Inf)], 
      y=fit1a$fitted.values, col='black', lty=1)#abline(fit1, lty=1)
lines(x=degdf$nl_between[which(degdf$nl_between < Inf)], 
      y=fit2a$fitted.values, col='red', lty=2)#abline(fit2, lty=2, col='red')
co.sub <- degdf[which( degdf$degree > quantile(degdf$degree, .999)), ]
text(co.sub$nl_between, log(co.sub$count), 
     labels=co.sub$name, 
     cex= co.sub$degree*.0025, #16*co.sub$nl_between,
     pos=1) 
theta1 <- round(fit1a$theta, 3)
theta2 <- round(fit2a$theta, 3)
mu <- mean(degdf$count)
var1 <- round( mu + ((mean(mu)^2)/theta1),3)
var2 <- round( mu + ((mean(mu)^2)/theta2),3)
legend('topright', cex=.8,
       #            legend=c(expression("E[A | B ~ NegBin("~theta==theta1~")]"),
       #                     expression("E[A | B+B"^"2"*"~ NegBin("~theta==theta2~")]")),
       legend=c(paste("E[A | B, var(A)~",var1,"]"),
                paste("E[A | B+B^2, var(A)~",var2,"]")),
       lty=1:2,col=c('black','red'))
# dev.off()

fit1b <- glm.nb(count ~ poly(constraint, 1), init.theta = .05, data = degdf) #degdf[which(degdf$count>0),])
fit2b <- glm.nb(count ~ poly(constraint, 2), init.theta = 2, data = degdf) #degdf[which(degdf$count>0),])

degdf <- degdf[order(degdf$constraint),]
# png('constraint_degree_compare.png',height=6, width=8, units = 'in', res=400)
plot(degdf$constraint, log(degdf$count),
     xlim=c(0,.55),
     cex=  degdf$degree*.005,   #4e-2/degdf$constraint, 
     pch=21, col='black', bg='steelblue',
     ylab='Ln Acquisitions (A)', 
     xlab='Network Constraint (C)')
lines(x=degdf$constraint, y=fit1b$fitted.values, col='black', lty=1)#abline(fit1, lty=1)
lines(x=degdf$constraint, y=fit2b$fitted.values, col='red', lty=2)#abline(fit2, lty=2, col='red')
co.sub <- degdf[which( degdf$degree > quantile(degdf$degree, .999)), ]
text( co.sub$constraint, log(co.sub$count),
      labels=co.sub$name, 
      cex= log(co.sub$degree)*.1, #1.5e-2/co.sub$constraint,
      pos=1)
theta1 <- round(fit1b$theta, 3)
theta2 <- round(fit2b$theta, 3)
mu <- mean(degdf$count)
var1 <- round( mu + ((mean(mu)^2)/theta1),3)
var2 <- round( mu + ((mean(mu)^2)/theta2),3)
legend('topright', cex=.8,
       #            legend=c(expression("E[A | B ~ NegBin("~theta==theta1~")]"),
       #                     expression("E[A | B+B"^"2"*"~ NegBin("~theta==theta2~")]")),
       legend=c(paste("E[A | C, var(A)~",var1,"]"),
                paste("E[A | C+C^2, var(A)~",var2,"]")),
       lty=1:2,col=c('black','red'))   
#     degdf <- degdf[order(degdf$constraint),]
#     plot(log(degdf$constraint), log(degdf$count),
#          cex=  log(degdf$degree)*.2,   #4e-2/degdf$constraint, 
#          pch=21, col='black', bg='steelblue',
#          ylab='Ln Acquisitions', 
#          xlab='Ln Network Constraint')
#     lines(x=log(degdf$constraint), y=fit1b$fitted.values, col='black', lty=1)#abline(fit1, lty=1)
#     lines(x=log(degdf$constraint), y=fit2b$fitted.values, col='red', lty=2)#abline(fit2, lty=2, col='red')
#     co.sub <- degdf[which( degdf$degree > quantile(degdf$degree, .999)), ]
#     text( log(co.sub$constraint), log(co.sub$count),
#          labels=co.sub$name, 
#          cex= log(co.sub$degree)*.2, #1.5e-2/co.sub$constraint,
#          pos=3)

dev.off()



# color by market
V(g1)$market_factor <- as.factor(V(g1)$market)
marvec <- unique(V(g1)$market)
colors <- rainbow(length(marvec),alpha = 0.4)
for (i in 1:length(marvec)) {
  V(g1)[which(V(g1)$market == marvec[i])]$color <- colors[i]
}





# save graphml
# igraph::write.graph(g1,"competition_graph.graphml",format="graphml")
# g1 <- read.graph("competition_graph.graphml", format='graphml')
# g1 <- induced.subgraph(g1, vids = V(g1)[which( !(V(g1)$market=="NA") )])

g1.sub.plot <- induced.subgraph(g1, vids = V(g1)[which(V(g1)$degree >= 10)])
png("full_net_graph_fr.png",height = 10, width=10, units='in', res=400)
plot.igraph(g1.sub.plot, layout=layout.fruchterman.reingold,
            vertex.size=1+(0.3* log(V(g1.sub.plot)$degree)),
            vertex.label.cex=.1,
            vertex.color=V(g1.sub.plot)$color,
            vertex.shape='circle',
            vertex.label=ifelse(V(g1.sub.plot)$constraint<quantile(V(g1.sub.plot)$constraint,.999),V(g1.sub.plot)$name,''),
            vertex.label.cex=1.5e-2/V(g1.sub.plot)$constraint,
            #vertex.label.color='white',
            edge.arrow.size=0.05,
            edge.width=0.5,
            #edge.label=E(sub)$weight,
            #edge.label.cex=E(sub)$edgelabelcex,
            #edge.label.color='black',
            edge.color=gray.colors(1,start = 0, alpha=0.5)
)
dev.off()



#
##------------------------------------------------------------------------
## plotting ego networks -------------------------------------------------

ecomacq <- acq[grep(symbol,acq$name),'acquired_company']
competitionNetworkComparison <- function()
{
  png("competition_networks_comparison_2.png",height=8,width=12,units='in',res=400)
  par(mfrow=c(2,3))
  set.seed(5)
  par(mar=c(.5,.5,2,1.5))
  labelColor <- rgb(.1,.1,.1,.95)
  
  symbol <- '^eBay$'
  id <- grep(symbol,V(g1)$name, perl = T)
  order <- 1
  ego.sub <- make_ego_graph(g1,order,id,mode='all',mindist = 0)[[1]]
  plot.igraph(ego.sub, 
              vertex.size=log(igraph::degree(ego.sub)+1)*4,
              vertex.label.cex=log(igraph::degree(ego.sub)+1)*.4, 
              vertex.label.color=labelColor,
              layout=layout.fruchterman.reingold,
              main=paste0(V(g1)$name[id]," (Order: ",1,")"))
  sub.id <- grep(symbol,V(ego.sub)$name, perl = T)
  c <- round(igraph::constraint(ego.sub, nodes=sub.id),3)
  s <- vcount(ego.sub)
  b <- round(-log(igraph::betweenness(ego.sub, sub.id, normalized=T)) , 3)
  a <- round( sum(V(ego.sub)$name %in% acq[grep(symbol,acq$name),'acquired_company']
  ) / degdf[grep(symbol,degdf$name),'count'], 3 )
  legend('topright', bty='n', legend=paste0('S: ', s,'\nC: ',c,'\nB: ',b,'\nA: ',a)   
  )
  order <- 2
  ego.sub <- make_ego_graph(g1,order,id,mode='all',mindist = 0)[[1]]
  plot.igraph(ego.sub, 
              vertex.size=log(igraph::degree(ego.sub)+1)*2.5,
              vertex.label.cex=log(igraph::degree(ego.sub)+1)*.2, 
              vertex.label.color=labelColor,
              layout=layout.fruchterman.reingold,
              main=paste0(V(g1)$name[id]," (Order: ",2,")"))
  sub.id <- grep(symbol,V(ego.sub)$name, perl = T)
  c <- round(igraph::constraint(ego.sub, nodes=sub.id),3)
  s <- vcount(ego.sub)
  b <- round(-log(igraph::betweenness(ego.sub, sub.id, normalized=T)) , 3)
  a <- round( sum(V(ego.sub)$name %in% acq[grep(symbol,acq$name),'acquired_company']
  ) / degdf[grep(symbol,degdf$name),'count'], 3 )
  legend('topright', bty='n', legend=paste0('S: ', s,'\nC: ',c,'\nB: ',b,'\nA: ',a)   
  )
  order <- 3
  ego.sub <- make_ego_graph(g1,order,id,mode='all',mindist = 0)[[1]]
  plot.igraph(ego.sub, 
              vertex.size=log(igraph::degree(ego.sub)+1)*1.5,
              vertex.label.cex=log(igraph::degree(ego.sub)+1)*.05, 
              vertex.label.color=labelColor,
              layout=layout.fruchterman.reingold,
              main=paste0(V(g1)$name[id]," (Order: ",3,")"))
  sub.id <- grep(symbol,V(ego.sub)$name, perl = T)
  c <- round(igraph::constraint(ego.sub, nodes=sub.id),3)
  s <- vcount(ego.sub)
  b <- round(-log(igraph::betweenness(ego.sub, sub.id, normalized=T)) , 3)
  a <- round( sum(V(ego.sub)$name %in% acq[grep(symbol,acq$name),'acquired_company']
  ) / degdf[grep(symbol,degdf$name),'count'], 3 )
  legend('topright', bty='n', legend=paste0('S: ', s,'\nC: ',c,'\nB: ',b,'\nA: ',a)   
  )
  ###############################################################
  
  symbol <- '^Groupon$'
  id <- grep(symbol,V(g1)$name, perl = T)
  order <- 1
  ego.sub <- make_ego_graph(g1,order,id,mode='all',mindist = 0)[[1]]
  plot.igraph(ego.sub,
              vertex.size=log(igraph::degree(ego.sub)+1)*4,
              vertex.label.cex=log(igraph::degree(ego.sub)+1)*.4, 
              vertex.label.color=labelColor,
              layout=layout.fruchterman.reingold,
              main=paste0(V(g1)$name[id]," (Order: ",1,")"))
  sub.id <- grep(symbol,V(ego.sub)$name, perl = T)
  c <- round(igraph::constraint(ego.sub, nodes=sub.id),3)
  s <- vcount(ego.sub)
  b <- round(-log(igraph::betweenness(ego.sub, sub.id, normalized=T)) , 3)
  a <- round( sum(V(ego.sub)$name %in% acq[grep(symbol,acq$name),'acquired_company']
  ) / degdf[grep(symbol,degdf$name),'count'], 3 )
  legend('topright', bty='n', legend=paste0('S: ', s,'\nC: ',c,'\nB: ',b,'\nA: ',a)   
  )
  order <- 2
  ego.sub <- make_ego_graph(g1,order,id,mode='all',mindist = 0)[[1]]
  plot.igraph(ego.sub, 
              vertex.size=log(igraph::degree(ego.sub)+1)*2.5,
              vertex.label.cex=log(igraph::degree(ego.sub)+1)*.2, 
              vertex.label.color=labelColor,
              layout=layout.fruchterman.reingold,
              main=paste0(V(g1)$name[id]," (Order: ",2,")"))
  sub.id <- grep(symbol,V(ego.sub)$name, perl = T)
  c <- round(igraph::constraint(ego.sub, nodes=sub.id),3)
  s <- vcount(ego.sub)
  b <- round(-log(igraph::betweenness(ego.sub, sub.id, normalized=T)) , 3)
  a <- round( sum(V(ego.sub)$name %in% acq[grep(symbol,acq$name),'acquired_company']
  ) / degdf[grep(symbol,degdf$name),'count'], 3 )
  legend('topright', bty='n', legend=paste0('S: ', s,'\nC: ',c,'\nB: ',b,'\nA: ',a)   
  )
  order <- 3
  ego.sub <- make_ego_graph(g1,order,id,mode='all',mindist = 0)[[1]]
  plot.igraph(ego.sub, 
              vertex.size=log(igraph::degree(ego.sub)+1)*1.5,
              vertex.label.cex=log(igraph::degree(ego.sub)+1)*.02, 
              vertex.label.color=labelColor,
              layout=layout.fruchterman.reingold,
              main=paste0(V(g1)$name[id]," (Order: ",3,")"))
  sub.id <- grep(symbol,V(ego.sub)$name, perl = T)
  c <- round(igraph::constraint(ego.sub, nodes=sub.id),3)
  s <- vcount(ego.sub)
  b <- round(-log(igraph::betweenness(ego.sub, sub.id, normalized=T)) , 3)
  a <- round( sum(V(ego.sub)$name %in% acq[grep(symbol,acq$name),'acquired_company']
  ) / degdf[grep(symbol,degdf$name),'count'], 3 )
  legend('topright', bty='n', legend=paste0('S: ', s,'\nC: ',c,'\nB: ',b,'\nA: ',a)   
  )
  dev.off()
}
competitionNetworkComparison()



######################################
######################################
# Acquisitions by Ego Network order
# #####################################
abeno <-  data.frame(name=as.character(dfall$name),
                     count=dfall$count, count_fac=NA,
                     o1=NA,o2=NA,o3=NA,o4=NA,o5=NA,
                     o6=NA,o7=NA,o8=NA,o9=NA,o10=NA,
                     o11=NA,o12=NA,o13=NA,o14=NA,o15=NA) 
df.sub <- abeno[which( (abeno$count > 0) & (abeno$count < Inf)), ]
df.sub <- df.sub[order(df.sub$count, decreasing=T),]
df.sub$count_fac <- ifelse(df.sub$count > 4,'>= 5',ifelse(df.sub$count > 1,'2 - 4',' 1 '))
head(df.sub)
dim(df.sub)


for (o in 1:15) {
  for (i in 1:nrow(df.sub)) {  #nrow(df.sub)
    symbol <- paste0('^',df.sub$name[i],'$')
    id.in.full <- grep(symbol,V(g1)$name,perl=T)
    ego.sub <- igraph::make_ego_graph(g1, order = o, nodes = id.in.full, mode='all')
    if (length(ego.sub)>0) {
      sub.count.acq <- sum(V(ego.sub[[1]])$name %in% 
                             acq[grep(symbol,acq$name),'acquired_company'])
      df.sub[i,3+o] <- sub.count.acq  
    } else {
      df.sub[i,3+o] <- NA
    }
  } 
  cat(paste('\ncompleted co. :',i,', order:',o))
}

abeno <- df.sub
abeno[,4:ncol(abeno)] <- abeno[,4:ncol(abeno)] / abeno$count

matplot(t(abeno[,4:ncol(abeno)]),type='l')

keepOrders <- 8
abeno.sub <- na.omit(abeno[,1:(3 + keepOrders)])
abeno_melt <- reshape2::melt(data=abeno.sub[,c(1,3:ncol(abeno.sub))], 
                             id=c('name','count_fac')
)

## Optimal distance ########################################
t0 <- 5
ax <- na.omit(abeno[which(abeno$count>=1 & abeno$count < t0),4:ncol(abeno)])
axm <- colMeans(ax)
axmd1 <- c(axm[1],diff(axm))
plot(axmd)

ax <- na.omit(abeno[which(abeno$count>=t0 & abeno$count < Inf),4:ncol(abeno)])
axm <- colMeans(ax)
axmd2 <- c(axm[1],diff(axm))
plot(axmd)

# ax <- na.omit(abeno[which(abeno$count>=t0 & abeno$count < Inf),4:ncol(abeno)])
# axm <- colMeans(ax)
# axmd3 <- c(axm[1],diff(axm))
# plot(axmd)

matplot(cbind(axmd1,axmd2),type='b',pch=c(15,16),
        xlab="Ego Network Order", ylab="Proportion of Acquisistions")
legend('topright',legend=c('1-4','>=5'),
       lty=1:2,pch=c(15,16),col=c('black','red'))


x <- seq(0,1,length.out = length(axmd))
plot(dgamma(x,4,rate=20))
##########################


abeno_melt$variable <- gsub("o","",abeno_melt$variable)
p1 <- lattice::dotplot(value ~ variable  | count_fac, groups=name, 
                       data=abeno_melt, type='b', ylim=c(0,1.05),
                       index.cond=list(c(3,2,1)), layout=c(1,3), pch=16,
                       ylab=list(label='Proportion of Acquisitions',cex=1.1),
                       xlab=list(label='Ego Competition Network Order',cex=1.1),
                       alternating=2,
                       par.strip.text=list(cex=.8),
                       scales=list(cex=.8))
p2 <- lattice::bwplot(value ~ variable |count_fac , 
                      data=abeno_melt[which(abeno_melt$count_fac!=" 1 "),],  ylim=c(-.05,1.05),
                      index.cond=list(c(2,1)),layout=c(1,2),
                      ylab=list(label='Proportion of Acquisitions',cex=1.1),
                      xlab=list(label='Ego Competition Network Order',cex=1.1),
                      scales=list(cex=1),
                      col='red')
p3 <- lattice::dotplot(value ~ variable | name , 
                       data=abeno_melt[which(abeno_melt$count_fac==">= 5"),],  
                       ylim=c(-.05,1.05),
                       ylab=list(label='Proportion of Acquisitions',cex=1.1),
                       xlab=list(label='Ego Competition Network Order ( >=5 Acquisitions)',cex=1.1),
                       # main=list(label=' >= 5',cex=.8),
                       par.strip.text=list(cex=.6),
                       scales=list(cex=.6)                      
)
png('acquisitions_by_egonet_order_and_count_combined_factor_2.png',height=5,width = 12,units='in',res=200)
grid.arrange(p3,p2,ncol=2)
dev.off()

# lattice::dotplot(value ~ variable |name, 
#                 data=abeno_melt,  ylim=c(-.05,1.05),
#                 ylab='Proportion of Acquisitions',
#                 xlab='Ego Competition Network Order')

################################


#################################
#################################
#--------------------------------
#--------------------------------


# MAKE COMPETITION NETWORKS
complist <- makeCompNet(g = g1, vertvec = as.character(dfall$name))

# complist <- list()
# for (i in 1:nrow(dfall)){
#     
#     complist[[i]] <- make_ego_graph(g1,1,id,mode='all',mindist = 0)[[1]]
# }
# )
# complist2 <- sapply('#waywire', function(name){
#     id <- grep(paste0('^',name,'$'),V(g1)$name, perl = T)
#     return(make_ego_graph(g1,1,id,mode='all',mindist = 0)[[1]])
# })



# MAKE EGO NETWORK DENSITY
make_ego_graph(g1,order,id,mode='all',mindist = 0)[[1]]

symbol <- '^Etsy$'
id <- grep(symbol,V(g1)$name, perl = T)
order <- 1
ego.sub <- make_ego_graph(g1,order,id,mode='all',mindist = 0)[[1]]


dfall$full_net_nlog_betweenness <- NA




#--------------------------------------------------------
#
#
#
#
#    CHECKPOINT
#
#
#
#-------------------------------------------------------

# # convert to network to compute brokerage score
# library(network)
# library(sna)
# netg <- as.network(igraph::get.adjacency(g1,type='both'))
# g2 <- as.igraph(netg)


# density
# clustering
# nl_between

#--------------------------------------------------------
# add graph variables to the data frame

# dfall$clustering <- NA
# dfall$density <- NA
# # dfall$reach <- NA
# for (i in 1:length(complist)) {
#   comp <- complist[[i]]
#   if (vcount(comp) < 2){
#     dfall$clustering[i] <- ecount(comp)/(vcount(comp)*(vcount(comp)-1)) #1e-6
#     dfall$density[i] <- igraph::transitivity(comp, type = 'global') # 1e-6
#     # dfall$reach[i] <- 0
#   } else {
#     dfall$density[i] <- ecount(comp)/(vcount(comp)*(vcount(comp)-1))
#     # dfall$reach[i] <- distWeightReach(comp, mode='in')
#     clust_i <- igraph::transitivity(comp, type = 'global')
#     if (is.nan(clust_i) | is.na(clust_i)){
#       dfall$clustering[i] <- 0
#     } else {
#       dfall$clustering[i] <- clust_i
#     }
#   }
# #   vert_i <- V(g1)[which(V(g1)$name==as.character(dfall$name[i]))]
# #   b_i <- igraph::betweenness(g1, v = vert_i)
# #   if(length(b_i) >0) {
# #     dfall$full_net_betweenness[i] <- b_i
# #   } else {
# #     dfall$full_net_betweenness[i] <- 0
# #   }
# #   dfall$full_net_betweenness[i] <- V(g1)[which(V(g1)$name==as.character(dfall$name[i]))]
# # 
# #   con_i <- igraph::constraint(g1, nodes = vert_i)
# #   if(length(con_i) >0) {
# #       dfall$full_net_constraint[i] <- b_i
# #   } else {
# #       dfall$full_net_constraint[i] <- 0
# #   }
#   if (i<=2) cat(paste0('row ',i,'\n'))
#   if (i%%1000==0) cat(paste0('row ',i,'\n'))
# } 

#------------------------------------------------------
# GET EGO NET CLUSTERING & DENSITY
# #

dfall$clustering <- NA
dfall$density <- NA
dfall$hierarchy <- NA
for (i in 1:nrow(dfall)) {  #nrow(df.sub)
  symbol <- paste0('^',dfall$name[i],'$')
  id.in.full <- grep(symbol,V(g1)$name,perl=T)
  ego.sub.list <- igraph::make_ego_graph(g1, order = 1, nodes = id.in.full, mode='all')
  if (length(ego.sub.list)>0) {
    ego.sub <- ego.sub.list[[1]]
    ex.id <- grep(symbol,V(ego.sub)$name,perl=T) 
    #
    dfall$density[i] <- ecount(ego.sub) / ( vcount(ego.sub)*(vcount(ego.sub)-1) )
    dfall$clustering[i] <- igraph::transitivity(ego.sub, type='global')
    mu <- mean(V(ego.sub)$constraint[-ex.id])
    n <- igraph::degree(ego.sub, v = ex.id) 
    sum_r <- sum( (V(ego.sub)$constraint[-ex.id]/mu)*log(V(ego.sub)$constraint[-ex.id]/mu) )
    dfall$hierarchy[i] <- ifelse(n*log(n)==0, NA, sum_r/(n*log(n)) )
    
    #         sub.count.acq <- sum(V(ego.sub[[1]])$name %in%  
    #                                  acq[grep(symbol,acq$name),'acquired_company'])
  } else {
    dfall$density[i] <- NA
    dfall$clustering[i] <- NA
    dfall$hierarchy[i] <- NA
  }
  if(i%%500==0) cat(paste0('\ncompleted:',i,' (',round((i/nrow(dfall))*100,1),'%)'))
} 




## ADD GLOBAL POSITION VARIABLES TO REGRESSION DF
dfall$full_net_betweenness <- sapply(dfall$name, function(name){
  x <- V(g1)$betweenness[which(V(g1)$name == name)]
  return(ifelse(length(x)>0,x,NA))
})

dfall$full_net_constraint <- sapply(dfall$name, function(name){
  x <- V(g1)$constraint[which(V(g1)$name == name)]
  return(ifelse(length(x)>0,x,NA))
})

# dfall$market <- sapply(dfall$name, function(name){
#     x <- V(g1)$market[which(V(g1)$name == name)]
#     return(ifelse(length(x)>0,x,NA))
# })
# 

dfall$status <- sapply(dfall$name, function(name){
  x <- V(g1)$status[which(V(g1)$name == name)]
  return(ifelse(length(x)>0,x,NA))
});  dfall$status <- as.factor(dfall$status)

dfall$density <- sapply(dfall$name, function(name){
  neighborVerts <- neighbors(g1, v = V(g1)[which(V(g1)$name == name)], mode='all' )
  subg <- induced.subgraph(g1, vids=neighborVerts)
  # print(name)
  return( ecount(subg)/(vcount(subg)*(vcount(subg)-1)) )
})



#OMIT UNNECESSARY VARS
omit.vars <- c('homepage_url','blog_url','blog_feed_url',
               'twitter_username','email_address','phone_number',
               'serialid')
dfall <- dfall[ , !(names(dfall) %in% omit.vars) ]

names(dfall)[which(names(dfall)=="count_acquired")] <- "count"
# names(dfall)[which(names(dfall)=="acquired_count")] <- "count"
write.table(dfall, "dfall_all3k_2.csv", sep=",", row.names=F, na = 'NA')

dfall <- read.table("dfall_all3k_2.csv", header=T, sep=",", na.strings=c('NA',''), quote='"', fill=T)



# plot pairs
# cls <- sapply(dfall, class)
# numer_names <- names( sapply(dfall,class)[ which(cls %in% c('integer','numeric','float') ) ]  )
# # colored pairs by platform type
# pairs( dfall[ ,numer_names], col=dfall$platform_type)
# # pairs with correlations, histograms, and  loess lines
# pairs( dfall[ ,numer_names], na.action=na.omit, lower.panel = panel.smooth,
#        upper.panel = panel.cor, diag.panel = panel.hist)
#
# pairsdata <- na.omit(dfall[,c('count','comp_count',
#                  'clustering','density', 'full_net_betweenness')])
# ggpairs(data = dfall, columns = c(2,5,9,12,13,14) ,  title = "Correlations", upper = list(), lower = list(continuous='points'), diag = list(continuous='density'), colour='platform_type', legends = T)


# namesvec <- c()
# for (i in 1:nrow(dfall)) {
#   namesvec[i] <- as.character(dfall$name[i]) %in% V(g1)$name
# }
# sum(namesvec) / length(namesvec)

#----------------------------------------------
# plotting competition networks

# n <- length(dfall$name)
# png("comp_net_plot_kk.png",height=12,width=12,units='in',res=250)
# par(mfrow=c( ceiling(sqrt(n)) , ceiling(sqrt(n)) ))
# par(mar=c(.2,.2,1,.2))
# for ( i in 1:length(complist)) {
#   comp <- complist[[i]]
#   comp <- igraph::simplify(comp, edge.attr.comb = list(weight='sum'))
#   if(all(degree(comp)<1)){
#     V(comp)$size <- 1
#   } else {
#     V(comp)$size <- 5 + degree(comp)*(35/max(degree(comp)))
#   }
# #   V(comp)$label <- NA
# #   V(comp)[which(V(comp)$name ==
# #                   as.character(dfall$name)[i])]$label <-
# #     as.character(dfall$name)[i]
#   V(comp)$label <- V(comp)$name
#   V(comp)$label_size <- .1 + degree(comp) * (.6 / max(degree(comp)))
#
#   V(comp)$color <- 'blue'
#   V(comp)[which(V(comp)$name == as.character(dfall$name)[i])]$color <- 'red'
#
#   main_i <- as.character(dfall$name)[i]
#   clustering_i <- round(transitivity(comp,type = 'global'), 2)
#   density_i <- round(ecount(comp)/(vcount(comp)*(vcount(comp)-1)), 2)
#
#   set.seed(1)
#   plot.igraph(comp, layout=layout.kamada.kawai,
#               vertex.size=V(comp)$size,
#               vertex.label=V(comp)$label,
#               vertex.label.color='black',
#               vertex.label.cex=V(comp)$label_size,
#               vertex.label.font=7,
#               main=main_i)
#   legend(x = 'topright', legend=paste('C=',clustering_i,'\nD=',density_i))
# }
# dev.off()



############NEW COUNT DATA########################
# a2 <- read.table("crunchbase_export_20160105_acquisitions.csv",sep=",",
#                  quote='"',header=T,fill=T, stringsAsFactors = F)
# a2$acquired_at_date <- mdy(a2$acquired_at)
# 
# # a2c <- count(a2$acquirer_name[which(a2$acquired_at > '2015-03-30')])
# a2c <- count(a2$acquirer_name[which(a2$acquired_year > 2014)])
# 
# a2c <- a2c[order(a2c$freq, decreasing=T),]
# a2c$x <- as.character(a2c$x)
# 
# a2c$x <- as.character(a2c$x)
# a2c$x <- gsub('[?]','',a2c$x)
# a2c$x <- gsub('[|]','',a2c$x)
# a2c$x <- gsub('[+]','',a2c$x)
# 
# # dfall$count2015 <- 0
# # for(i in 1:nrow(a2c)){
# #     dfall$count2015[which(dfall$name == a2c$x[i])] <- a2c$freq[i]
# #     # dfall$count2015[grep(a2c$x[i],dfall$name,perl=T,ignore.case=T)] <- a2c$freq[i]
# # #     if(a2c$freq[i] > 50) {
# # #         cat(paste('stopped: ',i))
# # #         break
# # #     }
# #     if(i%%10==0) cat(paste('\n',i))
# # }
# # 
# # n2 <- unique(a2$acquirer_name)
# # n1 <- unique(dfall$name)
# 
# 
# names(a2c) <- c('name','count2015')
# dfall2 <- merge(dfall[,-which(names(dfall)=="count2015")], a2c, by = 'name', all.x = T)
# dfall2$count2015[is.na(dfall2$count2015)] <- 0
# 
# 
# dfall <- dfall2

##################################################



#------------------------------------------------
#
#  Regression
#
#-------------------------------------------------



x <-dfall[ which(dfall$full_net_nlog_betweenness<Inf),c('name','comp_count','count','age','number_of_employees',
                                                        'density','overlap','clustering',
                                                        'full_net_constraint','full_net_betweenness', 'full_net_nlog_betweenness')]
x <- cbind(x, usd = log(dfall$usd+.001), ln_comp_count=log(dfall$comp_count+.001))


dfall$full_net_log_betweenness <- log(dfall$full_net_betweenness)
dfall$ln_usd <- log(dfall$usd)
dfall$m100_usd <- dfall$usd / 1e8
dfall$m_usd <- dfall$usd / 1e6
dfall$t_usd <- dfall$usd / 1e3
dfall$t_employess <- dfall$number_of_employees / 1e3
dfall$t10_employees <- dfall$number_of_employees / 1e4
dfall$overlap <- dfall$clustering / dfall$comp_count

# dfall$full_net_constraint <- dfall$full_net_constraint * 100

#################################
reg.data.frame <- dfall[ which( (dfall$full_net_log_betweenness > -Inf) & 
                                  (dfall$number_of_employees < 1e6) ),  ]
reg.data.frame$full_net_constraint <- reg.data.frame$full_net_constraint * 100
#################################

dim(reg.data.frame)

# numeric.pred <- dfall[,which(sapply(dfall,class) %in% c('inter','numeric'))]
# 
# cor(numeric.pred)
# 
# dim(na.omit(numeric.pred))
# # psych::cor.ci(na.omit(numeric.pred) )
# psych::corr.test(numeric.pred)

## SAVE VARIABLES
out <- dfall[,c('name','count', 'age',  'usd',  "number_of_employees", 
                'status', 'state_code' , 'comp_count', 'density', 'overlap' , 
                'full_net_constraint' , 'full_net_log_betweenness' )]
out <- out[which(out$full_net_log_betweenness > -Inf & 
                   out$full_net_log_betweenness < Inf),  ]
out <- out[which(out$number_of_employees < 999999),]
out <- na.omit(out)
write.table(out, "competition_network_regression_dataframe3.csv",sep=",",
            col.names = T,row.names = F,na = "")

############################################
############################################
############################################
############################################
########## REGRESSION MODELS  ##############
############################################
############################################
############################################
############################################

# dfall$bet_bin <- ifelse(dfall$full_net_betweenness==0, 0, 1)

f0 <- count ~ age + m100_usd  + t10_employees + status + state_code
f1a <- count ~ age + m100_usd  + t10_employees + status + state_code +  comp_count
f1b <- count ~ age + m100_usd  + t10_employees + status + state_code +  density
f1c <- count ~ age + m100_usd  + t10_employees + status + state_code +  overlap
f2a <- count ~ age + m100_usd  + t10_employees + status + state_code +  full_net_constraint
f2b <- count ~ age + m100_usd  + t10_employees + status + state_code + full_net_log_betweenness
f2all <- count ~ age + m100_usd  + t10_employees + status + state_code +  comp_count + density + overlap +  full_net_log_betweenness + full_net_constraint
f3a <- count ~ age + m100_usd  + t10_employees + status + state_code +  full_net_constraint:comp_count
f3a <- count ~ age + m100_usd  + t10_employees + status + state_code +  full_net_constraint:density
f3a <- count ~ age + m100_usd  + t10_employees + status + state_code +  full_net_constraint:overlap
f4a <- count ~ age + m100_usd  + t10_employees + status + state_code + full_net_log_betweenness:comp_count
f4b <- count ~ age + m100_usd  + t10_employees + status + state_code + full_net_log_betweenness:density
f4c <- count ~ age + m100_usd  + t10_employees + status + state_code + full_net_log_betweenness:overlap
f5 <- count ~ age + m100_usd  + t10_employees + status + state_code + comp_count + density + overlap +  full_net_constraint + full_net_log_betweenness + 
  full_net_constraint:comp_count + full_net_constraint:density + full_net_constraint:overlap + 
  full_net_log_betweenness:comp_count + full_net_log_betweenness:density + full_net_log_betweenness:overlap 

# f52n <- count2015 ~ age + m_usd  + number_of_employees +
#     comp_count + density + overlap + 
#     full_net_constraint + full_net_log_betweenness + 
#     full_net_constraint:comp_count + full_net_constraint:density + full_net_constraint:overlap + 
#     full_net_log_betweenness:comp_count + full_net_log_betweenness:density + full_net_log_betweenness:overlap 
# 
# f52c <- count2015 ~ age + m_usd  + number_of_employees + status + state_code + 
#     comp_count + density + overlap  + 
#     full_net_constraint + full_net_log_betweenness + 
#     full_net_constraint:comp_count + full_net_constraint:density + full_net_constraint:overlap + 
#     full_net_log_betweenness:comp_count + full_net_log_betweenness:density + full_net_log_betweenness:overlap 


# m5.zinb <- zeroinfl(f52,EM=T,counts=c(1,1,1,1,1,1,1,1),zero=-1,
#                     data=reg.data.frame, dist='negbin',link='log')

# summary( zipmodel <- glmmadmb(f52n,data=reg.data.frame,
#                      family="nbinom2",zeroInflation=T,
#                      admb.opts=admbControl(maxfn = 5000),
#                      debug=T)
# )
# 
# m52.nb <- zeroinfl(f52n, data = reg.data.frame)



# f5 <- count ~ age + m_usd  + number_of_employees + status + state_code + 
#     comp_count + density + overlap + 
#     full_net_constraint + full_net_log_betweenness + 
#     full_net_constraint:comp_count + full_net_constraint:density + full_net_constraint:overlap + 
#     full_net_log_betweenness:comp_count + full_net_log_betweenness:density + full_net_log_betweenness:overlap 

# f5b <- count ~ age + m_usd  + number_of_employees + status + state_code + 
#     comp_count + overlap + 
#     full_net_constraint + full_net_log_betweenness + 
#     full_net_log_betweenness:full_net_constraint + 
#     full_net_constraint:comp_count + full_net_constraint:overlap + 
#     full_net_log_betweenness:comp_count + full_net_log_betweenness:overlap 

# f5i <- count ~ age + m_usd  + number_of_employees + status + state_code + 
#     overlap + 
#     full_net_constraint + full_net_log_betweenness + 
#     full_net_constraint:overlap + 
#     full_net_log_betweenness:overlap 
# 
# 
# 
# 
# 
# f5n <- count ~ age + m_usd  + number_of_employees + 
#     comp_count + density + overlap + 
#     full_net_constraint + full_net_log_betweenness + 
#     full_net_constraint:comp_count + full_net_constraint:density + full_net_constraint:overlap + 
#     full_net_log_betweenness:comp_count + full_net_log_betweenness:density + full_net_log_betweenness:overlap 


f6 <- count ~ age + m100_usd  + t10_employees + status + state_code + 
  comp_count + density + overlap + 
  full_net_constraint + full_net_log_betweenness + 
  full_net_constraint:comp_count + full_net_constraint:density + full_net_constraint:overlap + 
  full_net_log_betweenness:comp_count + full_net_log_betweenness:density + full_net_log_betweenness:overlap +
  I(full_net_constraint^2) + I(full_net_log_betweenness^2)

# f7 <- count ~ age + m_usd  + number_of_employees + status + state_code + 
#     comp_count + density + overlap + 
#     full_net_constraint + full_net_log_betweenness + 
#     full_net_constraint:comp_count + full_net_constraint:density + full_net_constraint:overlap + 
#     full_net_log_betweenness:comp_count + full_net_log_betweenness:density + full_net_log_betweenness:overlap +
#     I(full_net_constraint^2) + I(full_net_log_betweenness^2) + 
#     I(full_net_constraint^2):comp_count + I(full_net_constraint^2):density + I(full_net_constraint^2):overlap + 
#     I(full_net_log_betweenness^2):comp_count + I(full_net_log_betweenness^2):density + I(full_net_log_betweenness^2):overlap 


m0.nb <- glm.nb(f0,  data = reg.data.frame, init.theta = .1, control=glm.control(maxit = 1e4))
m1a.nb <- glm.nb(f1a, data = reg.data.frame, init.theta = .1, control=glm.control(maxit = 1e4))
m1b.nb <- glm.nb(f1b, data = reg.data.frame,init.theta = .1, control=glm.control(maxit = 1e4))
m1c.nb <- glm.nb(f1c, data = reg.data.frame, init.theta = .1,control=glm.control(maxit = 1e4))
m2a.nb <- glm.nb(f2a,  data = reg.data.frame, init.theta = .1, control=glm.control(maxit = 1e4))
m2b.nb <- glm.nb(f2b,  data = reg.data.frame, init.theta = .1,control=glm.control(maxit = 1e4))
m2all.nb <- glm.nb(f2all, data = reg.data.frame, init.theta = .5, control=glm.control(maxit = 1e4))

m5.p <- glm(f5, data = reg.data.frame, family = poisson, control=glm.control(maxit = 1e4))
m5.nb <- glm.nb(f5, data = reg.data.frame, control=glm.control(maxit = 5000))
# m5.nb.n <- glm.nb(f5n, data = reg.data.frame, init.theta = .5,  control=glm.control(maxit = 5000))
# m5.nb.b <- glm.nb(f5b, data = reg.data.frame, init.theta = .5,  control=glm.control(maxit = 5000))
# m5.nb.i <- glm.nb(f5i, data = reg.data.frame, init.theta = .5,  control=glm.control(maxit = 5000))

# m52.nb <- glm.nb(f52, data = reg.data.frame, init.theta=.5, control=glm.control(maxit = 5000))


# m5.p <- glm(f6, data = reg.data.frame, family = poisson, control=glm.control(maxit = 1e4))
m6.nb <- glm.nb(f6, data = reg.data.frame, control=glm.control(maxit = 5000))

# # 
# summary(m2all.nb)
# 
# 
# reg.df100 <- reg.data.frame
# reg.df100$full_net_constraint <- reg.df100$full_net_constraint * 100
# m5.nb.100 <- glm.nb(f5, data = reg.df100, control=glm.control(maxit = 5000))
# m6.nb.100 <- glm.nb(f6, data = reg.df100, control=glm.control(maxit = 5000))



mtable(m0.nb, m1a.nb, m1b.nb, m1c.nb, m2a.nb, m2b.nb, m2all.nb, m5.nb, m6.nb, digits=2)



m7.nb <- glm.nb(f7, data = reg.data.frame, control=glm.control(maxit = 5000))

reg.data.frame.used <- reg.data.frame[names(m5.nb$fitted.values), ]
reg.data.frame.used <- reg.data.frame.used[,c('count','age','m_usd','t_employess',
                                              'status','state_code','comp_count',
                                              'density','overlap','full_net_constraint',
                                              'full_net_log_betweenness')]

reg.data.frame.desc <- reg.data.frame.used[,which( !(names(reg.data.frame.used) %in% c('state_code','status')))]

summary(reg.data.frame.desc)

write.table(psych::describe(reg.data.frame.desc), 'REG_describe.csv',sep = ',',row.names = T,col.names = T)

cortest <- psych::corr.test(reg.data.frame.desc)
write.table(cor(reg.data.frame.desc), 'REG_correlation.csv',sep = ',',row.names = T,col.names = T)


dispersiontest(m5.p)
dispersiontest(m6.p)



par(mfrow=c(3,3))
for (i in 1:ncol(reg.data.frame.desc)) {
  hist(reg.data.frame.desc[,i],breaks=50,main=names(reg.data.frame.desc)[i])
}





## INTERACTIONS
X <- m5.nb$model
X$full_net_constraint <- m5.nb$model$full_net_constraint * 100
levels.labels <- c('Low','Mid','High')
X$size_b <- factor(ifelse(X$comp_count > quantile(X$comp_count,2/3), 'High',
                          ifelse(X$comp_count >quantile(X$comp_count,1/3), 'Mid','Low')),
                   levels = levels.labels)
X$density_b <- factor(ifelse(X$density > quantile(X$density,2/3), 'High',
                             ifelse(X$density >quantile(X$density,1/3), 'Mid','Low')),
                      levels=levels.labels)

X$between_b <- factor(ifelse(X$full_net_log_betweenness > quantile(X$full_net_log_betweenness,2/3), 'High',
                             ifelse(X$full_net_log_betweenness >quantile(X$full_net_log_betweenness,1/3), 'Mid','Low')),
                      levels=levels.labels)
X$constraint_b <- factor(ifelse(X$full_net_constraint > quantile(X$full_net_constraint,2/3), 'High',
                                ifelse(X$full_net_constraint >quantile(X$full_net_constraint,1/3), 'Mid','Low')),
                         levels=levels.labels)


X$between_f <- NA; X$constraint_f <- NA

Q <- 2
for (q in 1:Q){
  X$between_f[which(X$full_net_log_betweenness > quantile(X$full_net_log_betweenness, (q-1)/Q))] <- q
  X$constraint_f[which(X$full_net_constraint > quantile(X$full_net_constraint, (q-1)/Q))] <- q
}


png("sig_interactions_2.png",height=4, width=11, units='in', res=200)
par(mfrow=c(1,3))
cols <- c('black','steelblue','darkred')
interaction.plot(X$constraint_f, X$size_b, log(X$count + 1e-3), type="b", 
                 pch=c(1,2,15),cex=1.6,lwd=1.8,col=cols, cex.lab=1.6,cex.main=1.7,
                 trace.label = "Size", xlab="Constraint",ylab="Ln( Acquisitions + 0.001 )",
                 main="(a)                                                           ",
)
interaction.plot(X$constraint_f, X$density_b, log(X$count + 1e-3), type="b", 
                 pch=c(1,2,15),cex=1.6,lwd=1.8, col=cols, cex.lab=1.6,cex.main=1.7,
                 trace.label = "Density", xlab="Constraint",ylab="Ln( Acquisitions + 0.001 )",
                 main="(b)                                                            "
)
interaction.plot(X$between_f, X$size_b, log(X$count + 1e-3), type="b", 
                 pch=c(1,2,15),cex=1.6, lwd=1.8,col=cols,cex.lab=1.6,cex.main=1.7,
                 trace.label = "Size", xlab="Ln Betweenness",ylab="Ln( Acquisitions + 0.001 )",
                 main="(c)                                                             "
)
dev.off()






## INTERACTIOIN WITH ERROR BARS
X_melt <- reshape::melt.data.frame(data=X,
                                   measure.var="count",
                                   id = c("size_b","density_b",
                                          "between_b","constraint_b"))
X_ply <- ddply(.data = X, 
               .variables = .(count, size_b, density_b, 
                              between_b, constraint_b),
               summarise, 
               "Mean Size"=mean(X$comp_count),
               "SE Size"=sd(X$comp_count)/sqrt(nrow(X)-1)
)
ggplot(data=X, aes(x=mean(constraint_b), mean(y=log(count + 1e-3)), 
                   colour=size_b, group=size_b))+
  geom_line() + 
  geom_point() 

# PREDICTION
mu.0 <- colMeans(reg.data.frame.used[,which(sapply(reg.data.frame,class) %in% c('interger','numeric'))],na.rm = T)


preds <- data.frame(preds=c(0,mu.0, 
                            mean(reg.data.frame$comp_count)*mean(reg.data.frame$full_net_constraint),
                            mean(reg.data.frame$density)*mean(reg.data.frame$full_net_constraint),
                            mean(reg.data.frame$overlap)*mean(reg.data.frame$full_net_constraint),
                            mean(reg.data.frame$comp_count)*mean(reg.data.frame$full_net_log_betweenness),
                            mean(reg.data.frame$density)*mean(reg.data.frame$full_net_log_betweenness),
                            mean(reg.data.frame$overlap)*mean(reg.data.frame$full_net_log_betweenness)
)
)
row.names(preds) <- names(m5.nb.n$coefficient)
p.0 <- predict(m5.nb.n, preds)


p1 <- glm.nb(f5 <- count ~ age + m_usd  + t_employess + status + state_code + 
               comp_count + density + overlap + 
               full_net_constraint + full_net_log_betweenness + 
               full_net_constraint:comp_count + full_net_constraint:density + full_net_constraint:overlap + 
               full_net_log_betweenness:comp_count + full_net_log_betweenness:density + full_net_log_betweenness:overlap,
             
)



# f6 <- count ~ age + usd  + number_of_employees + state_code + 
#     comp_count + density + overlap + 
#     full_net_constraint + full_net_betweenness + 
#     full_net_constraint:comp_count + full_net_constraint:density + full_net_constraint:overlap + 
#     full_net_betweenness:comp_count + full_net_betweenness:density + full_net_betweenness:overlap  + 
#     I(full_net_constraint^2) + I(full_net_betweenness^2)
# 
# # f6.d <- count ~ age + ln_usd  + number_of_employees + state_code +  status + 
# #     comp_count  + density + overlap + 
# #     full_net_constraint + full_net_betweenness + 
# #     full_net_constraint:comp_count +  + full_net_constraint:overlap + 
# #     full_net_betweenness:comp_count + full_net_betweenness:density + 
# #     full_net_betweenness:overlap + 
# #     I(full_net_constraint^2) + I(full_net_betweenness^2)
# 
# f6.l <- count ~ age + ln_usd  + number_of_employees + state_code +  status + 
#     ln_comp_count  + density + overlap + 
#     full_net_constraint + ln_full_net_betweenness + 
#     full_net_constraint:comp_count +  + full_net_constraint:overlap + 
#     ln_full_net_betweenness:comp_count + ln_full_net_betweenness:density + 
#     ln_full_net_betweenness:overlap + 
#     I(full_net_constraint^2) + I(ln_full_net_betweenness^2)
# 
# f7 <- count ~ age + usd  + number_of_employees + state_code +  status + 
#     comp_count  + density + overlap + 
#     full_net_constraint  + 
#     full_net_constraint:comp_count + full_net_constraint:density + full_net_constraint:overlap + 
#     I(full_net_constraint^2)
# 
# 
# m2all.p <- glm(f2all, family = poisson, data = dfall, control=glm.control(maxit = 1e4))
# m2all.nb <- glm.nb(f2all, data = dfall, control=glm.control(maxit = 1e4))
# 
# m5.p <- glm(f5, family = poisson, data = dfall, control=glm.control(maxit = 1e4))
# m5.nb <- glm.nb(f5, data = dfall, control=glm.control(maxit = 1e4))
# 
# m5.d.p <- glm(f5.d, family = poisson, data = dfall, control=glm.control(maxit = 1e4))
# m5.d.nb <- glm.nb(f5.d, data = dfall, control=glm.control(maxit = 5000))
# 
# 
# m6.p <- glm(f6, family = poisson, data = dfall, control=glm.control(maxit = 50000))
# m6.nb <- glm.nb(f6, data = dfall, control=glm.control(maxit = 50000))
# 
# m6.d3.p <- glm(f6.d, family = poisson, data = dfall, control=glm.control(maxit = 1e4))
# m6.d3.nb <- glm.nb(f6.d, data = dfall, control=glm.control(maxit = 5000))
# 
# m6.l.p <- glm(f6.l, family = poisson, data = dfall, control=glm.control(maxit = 1e4))
# m6.l.nb <- glm.nb(f6.l, data = dfall, control=glm.control(maxit = 5000))
# 
# 
# m7.p <- glm(f7, family = poisson, data = dfall, control=glm.control(maxit = 5000))
# m7.nb <- glm.nb(f7, data = dfall, control=glm.control(maxit = 100))
# 
# 
# mtable(m7.p, m7.nb)


df.std <- dfall
num.vars <- c('age','usd','number_of_employees','comp_count',
              'density','overlap',
              'full_net_betweenness','full_net_constraint')
df.std[,num.vars] <- scale(df.std[,num.vars], center = T,scale = T)
m6.m.nb <- glmer.nb(count ~ age + ln_usd  + number_of_employees + 
                      comp_count + overlap + 
                      full_net_constraint + full_net_betweenness + 
                      full_net_constraint:comp_count + full_net_constraint:overlap + 
                      full_net_betweenness:comp_count + full_net_betweenness:overlap  + 
                      I(full_net_constraint^2) + I(full_net_betweenness^2)  +
                      (1 | state_code), 
                    data = dfall)


m5.zip <- pscl::zeroinfl(count ~ age + ln_usd  + number_of_employees + state_code + 
                           comp_count + density + overlap + 
                           full_net_constraint + full_net_betweenness + 
                           full_net_constraint:comp_count + full_net_constraint:density + full_net_constraint:overlap + 
                           full_net_betweenness:comp_count + full_net_betweenness:density + full_net_betweenness:overlap 
                         | 1 , 
                         dist='negbin', link='log',data = dfall)
summary(m5.zip)
m5.zinb


fit2 <- glm(f.2nb, family=poisson, data=dfall)
summary(fit2)

fit2nb <-  glm.nb(f.2nb, data = dfall, glm.control(maxit = 1e4))
summary(fit2nb)

fit1zip <- zeroinfl(count ~  usd + number_of_employees +  status + state_code + 
                      comp_count + full_net_betweenness + full_net_constraint +
                      I(full_net_betweenness^2) + I(full_net_constraint^2)
                    | age_bin,
                    dist='poisson', link='log',
                    data = dfall)
summary(fit1zip)
fit1zinb <- zeroinfl(count ~ age + usd + number_of_employees  + 
                       comp_count + density + overlap + 
                       full_net_betweenness + full_net_constraint + 
                       full_net_betweenness:comp_count + full_net_betweenness:density + 
                       full_net_betweenness:overlap + 
                       full_net_constraint:comp_count + full_net_constraint:density + 
                       full_net_constraint:overlap   | age_bin ,
                     dist='negbin', link='log',
                     control = zeroinfl.control(maxit = 60000),
                     data = dfall)
summary(fit1zinb)

zero
# add platform dummy and interaction effect
fit2 <-  glm(count ~ comp_count + age + full_net_betweenness,
             family = poisson(link='log'), data = dfall)
summary(fit2)

fit3 <-  glm(count ~ comp_count + age + full_net_betweenness +
               density + clustering ,
             family = poisson(link='log'), data = dfall)
summary(fit3)

fit4 <-  glm(count ~ comp_count + age + full_net_betweenness +
               density + clustering + I(clustering*density) ,
             family = poisson(link='log'), data = dfall)
summary(fit4)


fit5 <-  glm(count ~ comp_count + age + full_net_betweenness +
               density + clustering + I(clustering*density) +
               full_net_betweenness:comp_count +
               full_net_betweenness:density + 
               full_net_betweenness:clustering +
               full_net_betweenness:I(clustering*density) ,
             family = poisson(link='log'), data = dfall)
summary(fit5)

fit6a <-  glm(count ~ comp_count + age + full_net_betweenness +
                density + clustering + I(clustering*density) +
                I(full_net_betweenness^2) +
                full_net_betweenness:comp_count +
                full_net_betweenness:density + 
                full_net_betweenness:clustering +
                full_net_betweenness:I(clustering*density) ,
              family = poisson(link='log'), data = dfall)
summary(fit6a)

fit6b <-  glm(count ~ comp_count + age + full_net_betweenness +
                density + clustering + I(clustering*density) +
                I(density^2) + I(clustering^2) + I((clustering*density)^2) + 
                full_net_betweenness:comp_count +
                full_net_betweenness:density + 
                full_net_betweenness:clustering +
                full_net_betweenness:I(clustering*density) ,
              family = poisson(link='log'), data = dfall)
summary(fit6b)

fit6ab <-  glm(count ~ comp_count + age + full_net_betweenness +
                 density + clustering + I(clustering*density) +
                 I(full_net_betweenness^2) +
                 I(density^2) + I(clustering^2) + I((clustering*density)^2) + 
                 full_net_betweenness:comp_count +
                 full_net_betweenness:density + 
                 full_net_betweenness:clustering +
                 full_net_betweenness:I(clustering*density) ,
               family = poisson(link='log'), data = dfall)
summary(fit6ab)

mtable(fit3,fit4,fit5,fit6ab)

fit7 <- glm(count ~ comp_count + age + full_net_betweenness +
              density + I(clustering/comp_count) +
              full_net_betweenness:comp_count +
              full_net_betweenness:density + 
              full_net_betweenness:I(clustering/comp_count) , 
            family = poisson(link='log'),  data = dfall)

fit8 <- glm( I(count-1) ~ comp_count + age + full_net_betweenness +
               density + I(clustering/comp_count) + I(clustering*density) +
               I(full_net_betweenness^2) +
               I(density^2) + I((clustering/comp_count)^2) + I(((clustering/comp_count)*density)^2) + 
               full_net_betweenness:comp_count +
               full_net_betweenness:density + 
               full_net_betweenness:I(clustering/comp_count) +
               full_net_betweenness:I((clustering/comp_count)*density) ,
             data = dfall, family=poisson(link = 'log'))
fit8nb <- glm.nb( I(count-1) ~ comp_count + age + full_net_betweenness +
                    density + I(clustering/comp_count) + I(clustering*density) +
                    I(full_net_betweenness^2) +
                    I(density^2) + I((clustering/comp_count)^2) + I(((clustering/comp_count)*density)^2) + 
                    full_net_betweenness:comp_count +
                    full_net_betweenness:density + 
                    full_net_betweenness:I(clustering/comp_count) +
                    full_net_betweenness:I((clustering/comp_count)*density) ,
                  data = dfall)

#-------------------------------------------------------------------
dfall$betweenfactor <- factor(ifelse(dfall$full_net_betweenness>median(dfall$full_net_betweenness),
                                     'H','L'), levels=c('L','H'))

dfall$compfactor <- factor(ifelse(dfall$comp_count>median(dfall$comp_count),
                                  'H','L'), levels=c('L','H'))

png("between_egonetsize_interaction.png",height=6,width=6,units='in',res = 300)
interaction.plot(dfall$compfactor, dfall$betweenfactor, log(dfall$count),
                 type='b', pch=c(15,17), trace.label="Global\nNetwork\nBetweenness",
                 legend=T ,
                 main="Strategic Acquisitions Interactions\nBetweenness x Ego Network Size",
                 xlab="Ego Network Size", ylab="Acquisitions")
#legend('topleft',legend=c('Global Betweenness','L','H'),lty=c(NA,2,1),pch=c(NA,15,17))
dev.off()

#plot(log(count)~compfactor,col='light blue', data=dfall)


x <- matrix(rep(NA,4),nrow=2)
lev <- c('L','H')
rownames(x) <- lev;  colnames(x) <- lev
for (i in 1:2) { #compfactor rows
  for (j in 1:2) { #betweenfactor cols
    x[i,j] <- median(dfall$count[which(dfall$compfactor==lev[i] &
                                         dfall$betweenfactor==lev[j])])
    print(paste0("compfactor ",lev[i],", betweenfactor ",lev[j]," ",x[i,j]))
  }
}
x


#--------------------------------------------------------------------
fit52all <-  glm(count ~ comp_count + age + full_net_betweenness +
                   density + clustering + I(clustering*density) +
                   I(comp_count^2) + I(full_net_betweenness^2) +
                   I(density^2) + I(clustering^2) + I(I(clustering*density)^2) +
                   comp_count:full_net_betweenness + density:full_net_betweenness +
                   clustering:full_net_betweenness + 
                   I(clustering*density):full_net_betweenness +
                   age:full_net_betweenness  ,
                 family = poisson(link='log'), data = dfall)
summary(fit52all)


fit6 <-  glm(count ~ comp_count + 
               age +  density + clustering +
               I(clustering*density) +
               full_net_betweenness ,
             family = poisson(link='log'), data = dfall)
summary(fit6)

fit62 <-  glm(count ~ comp_count + I(comp_count^2) +
                age +  density + clustering +
                I(clustering*density) +
                I(density^2) +  I(clustering^2)+
                I((clustering*density)^2) +
                full_net_betweenness ,
              family = poisson(link='log'), data = dfall)
summary(fit62)




fit7 <-  glm(count ~ comp_count + I(comp_count^2) + I(comp_count^3)+
               I(comp_count^4) + I(comp_count^5) +I(comp_count^6) + I(comp_count^7) + I(comp_count^8) +I(comp_count^9) +
               age +  density + clustering +
               I(clustering*density) +
               I(density^2) +
               I(clustering^2)+
               I((clustering*density)^2) +
               full_net_betweenness +
               platform_type + platform_type:comp_count +
               platform_type:age +
               platform_type:density +
               platform_type:clustering +
               platform_type:I(clustering*density),
             family = poisson(link='log'), data = dfall)
summary(fit7)

# data <- na.omit(dfall[which(dfall$founded_year>=1995),])
dfall$era <- as.factor(ifelse(dfall$founded_year<1995,0,
                              ifelse(dfall$founded_year<2005,1,2))
)
fit8 <-  glm(count ~ comp_count  + era + comp_count:era +
               I(comp_count^2) +
               I(comp_count^3) +
               age +  density + clustering +
               I(clustering*density) +
               I(density^2) +
               I(clustering^2)+
               I((clustering*density)^2) +
               full_net_betweenness +
               platform_type + platform_type:comp_count +
               platform_type:age +
               platform_type:density +
               platform_type:clustering +
               platform_type:I(clustering*density),
             family = poisson(link='log'), data = dfall)
summary(fit8)


fit9 <-  glm(count ~ compsamemarket + compdiffmarket  +
               I(compsamemarket^2) +
               I(compdiffmarket^2) +
               age +  density + clustering +
               I(clustering*density) +
               I(density^2) +
               I(clustering^2)+
               I((clustering*density)^2) +
               full_net_betweenness +
               platform_type + platform_type:comp_count +
               platform_type:age +
               platform_type:density +
               platform_type:clustering +
               platform_type:I(clustering*density),
             family = poisson(link='log'), data = dfall)
summary(fit9)


fit10 <-  glm(count ~ comp_count +
                age +  density + clustering +
                I(clustering*density) +
                I(density^2) +
                I(clustering^2)+
                I((clustering*density)^2) +
                full_net_betweenness +
                platform_type + platform_type:comp_count +
                platform_type:age +
                platform_type:density +
                platform_type:clustering +
                platform_type:I(clustering*density),
              family = poisson(link='log'), data = dfall)
summary(fit10)

fit11 <-  glm(count ~ compsamemarket + compdiffmarket +
                age +  density + clustering +
                I(clustering*density) +
                I(density^2) +
                I(clustering^2)+
                I((clustering*density)^2) +
                full_net_betweenness +
                compsamemarket:full_net_betweenness +
                compdiffmarket:full_net_betweenness +
                compsamemarket:density +
                compdiffmarket:density+
                compsamemarket:clustering+
                compdiffmarket:clustering+
                compsamemarket:I(clustering*density) +
                compdiffmarket:I(clustering*density) +
                platform_type + platform_type:comp_count +
                platform_type:age +
                platform_type:density +
                platform_type:clustering +
                platform_type:I(clustering*density),
              family = poisson(link='log'), data = dfall)
summary(fit11)


fit12 <-  glm(count ~ compsamemarket + compdiffmarket +
                age +  density + clustering +
                I(clustering*density) +
                I(density^2) +
                I(clustering^2)+
                I((clustering*density)^2) +
                full_net_betweenness +
                compsamemarket:full_net_betweenness +
                compdiffmarket:full_net_betweenness +
                compsamemarket:density +
                compdiffmarket:density+
                compsamemarket:clustering+
                compdiffmarket:clustering+
                compsamemarket:I(clustering*density) +
                compdiffmarket:I(clustering*density),
              family = poisson(link='log'), data = dfall)
summary(fit12)


# cmopare regression outputs
mtable(fit0,fit1,fit2,fit3,fit3cd,fit4, getSummary = T,summary.stats = T)

## MANOVA FOR outcome vectors
mv0 <- manova( cbind(count, acquired_categories, acquired_competitors) ~ comp_count ,
               data = dfall, na.action=na.omit)

mv1 <- manova( cbind(count, acquired_categories, acquired_competitors) ~ comp_count + density + clustering + dens_clus ,  data = dfall, na.action=na.omit)

# mv2 <- manova( cbind(count, acquired_categories, acquired_competitors) ~ comp_count + comp_count:density + comp_count:clustering + comp_count:dens_clus ,  data = dfall, na.action=na.omit)
#


# Chi-sq deviance test of the significance of platform type
anova(fit0,fit1,test='Chisq')
anova(fit0,fit2,test='Chisq')
anova(fit0,fit3,test='Chisq')
anova(fit0,fit4,test='Chisq')



# network outcomes



# acquired percent of competitors by platform
# not significant with small sample
summary(aov(acquired_markets ~ platform_type, data=dfall))
fit3 <-  glm(count ~ comp_count + platform_type +  acquired_markets +
               acquired_markets:platform_type,
             family = poisson(link='log'), data = dfall)
summary(fit3)

# resampling:
index <- sample(x=1:nrow(dfall), size = nrow(dfall), replace = TRUE)
fitr <-  glm(count ~ comp_count + platform_type + full_net_betweenness
             +  comp_count:platform_type,
             family = poisson(link='log'), data = dfall[index,])
summary(fitr)

# # logistic regression for acquired competitors
# fitlogistic <-  glm(acquired_competitors ~ comp_count + platform_type + comp_count:platform_type,
#              family = binomial(link='logit'), data = dfall)
# summary(fitlogistic)
#
#
# # regression for diversity of acquisitions
# # add betweenness
# fitacq <-  glm(acquired_markets ~ comp_count + platform_type + comp_count:platform_type,
#              family = poisson(link='log'), data = dfall)
# summary(fitacq)


#--------------------------------------------------
#
#  Plotting count to competitors outcome
#
#---------------------------------------------------
ggplot(aes(x=comp_count,colour=platform_type), data=dfall) +
  geom_density(aes(fill=platform_type), alpha=0.4) +
  ggtitle("Competition Distribution by Platform Type")
ggplot(aes(x=count,colour=platform_type), data=dfall) +
  geom_density(aes(fill=platform_type), alpha=0.4) +
  ggtitle("Acquisition Distribution by Platform Type")
ggplot(aes(x=acquired_competitors,colour=platform_type), data=dfall) +
  geom_density(aes(fill=platform_type), alpha=0.4) +
  ggtitle("Acquisition Competition by Platform Type")
ggplot(aes(x=acquired_categories,colour=platform_type), data=dfall) +
  geom_density(aes(fill=platform_type), alpha=0.4)

# plot scatterplots of regression relationships
ggplot(aes(x=comp_count,y=(count),colour=platform_type),data=dfall) +
  geom_point(aes(colour=platform_type),size=4) +
  geom_smooth(aes(colour=platform_type),lwd=1.05,
              alpha=0.2,method=lm, data=dfall) +
  xlab("Competitors") + ylab("Acquisitions") +   theme_bw() +
  ggtitle("Platform Acquisitions by Competitor Count
          for Platform Types")
ggsave("platform_acquisitions.png",height=6,width=8,dpi=200,units='in')

ggplot(aes(x=comp_count,y=acquired_markets,colour=platform_type),data=dfall) +
  geom_point(aes(colour=platform_type),size=4) +
  geom_smooth(aes(colour=platform_type),lwd=1.05,
              alpha=0.2,method=lm, data=dfall) +
  xlab("Competitors") + ylab("Diversity") +   theme_bw() +
  ggtitle("Diversity Acquisition by Competitor Count
          for Platform Types")
ggsave("platform_diveristy_acquisitions.png",height=6,width=8,dpi=200,units='in')

ggplot(aes(x=comp_count,y=acquired_competitors,colour=platform_type),data=dfall) +
  geom_point(aes(colour=platform_type),size=4) +
  geom_smooth(aes(colour=platform_type),lwd=1.05,
              alpha=0.2,method='glm', family='binomial',
              data=dfall) +
  xlab("Competitors") + ylab("Competitors Acquired") +   theme_bw() +
  ggtitle("Competitor Acquisition by Competitor Count
          for Platform Types")
ggsave("platform_competitor_acquisitions.png",height=6,width=8,dpi=200,units='in')

ggplot(aes(x=full_net_betweenness,y=count,colour=platform_type),data=dfall) +
  geom_point(aes(colour=platform_type),size=4) +
  geom_smooth(aes(colour=platform_type),alpha=0.1,method=lm) +
  xlab("Betweenness") + ylab("Competitors Acquired") +   theme_bw() +
  ggtitle("Competitor Acquisition by Betweenness Centrality
          for Platform Types")
ggsave("platform_betweenness_acquisitions.png",height=6,width=8,dpi=200,units='in')

#plot sigmoid
fitlogistic <-  glm(acquired_competitors ~ comp_count + platform_type + comp_count:platform_type,
                    family = binomial(link='logit'), data = dfall)
X <- data.frame(y=dfall$acquired_competitors,
                platform_type=dfall$platform_type,
                x1=dfall$comp_count)

ggplot(aes(x=comp_count,y=acquired_competitors,colour=platform_type),data=dfall) +
  geom_point(aes(colour=platform_type),size=4) +
  geom_line(aes(x=x1,y=y_hat,colour=platform_type), data=X) +
  xlab("Competitors") + ylab("Competitors Acquired") +   theme_bw() +
  ggtitle("Competitor Acquisition by Competitor Count
          for Platform Types")
ggsave("platform_competitor_acquisitions.png",height=6,width=8,dpi=200,units='in')


