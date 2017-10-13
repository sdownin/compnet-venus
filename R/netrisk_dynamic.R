#---------------------------------------------------------------------
setwd("C:/Users/T430/Google Drive/PhD/Dissertation/competition networks/compnet")
# .libPaths('C:/Users/T430/Documents/R/win-library/3.2')
library(parallel)
library(statnet, quietly = T)
library(network, quietly = T)
library(xergm, quietly = T)  ## includes rem, tnam, GERGM
library(texreg, quietly = T)
library(igraph, quietly = T)
library(plyr, quietly = T)
library(dplyr, quietly = T)
library(stringr, quietly = T)
library(ndtv, quietly = T)
library(visNetwork, quietly = T)
library(scatterplot3d, quietly = T)
library(lattice, quietly = T)
library(latticeExtra, quietly = T)
library(directlabels, quietly = T)
library(lubridate)
library(ggplot2, quietly = T)
library(reshape2)
library(plyr)
library(ggplot2)
data_dir <- "C:/Users/T430/Google Drive/PhD/Dissertation/crunchbase/"
img_dir  <- "C:/Users/T430/Google Drive/PhD/Dissertation/competition networks/envelopment/img"
# if( !('net' %in% ls()) )
#   load('netrisk_dynamic_2.RData')
###
# save.image('netrisk_dynamic_2.RData')
###
source(file.path(getwd(),'R','comp_net_functions.R'))
source(file.path(getwd(),'R','netrisk_functions.R'))
source(file.path(getwd(),'R','cb_data_prep.R'))
par.default <- par()
lattice::trellis.par.set(strip.background=list(col="lightgrey"))
###########################################################################
## Firm type global variables
# multi.prod <- c("cisco","google","microsoft","ibm","yahoo","oracle","hewlett-packard","intel",
#                 "aol","apple","facebook","amazon","adobe-systems","nokia","dell","sap","motorola",
#                 "groupon","autodesk","salesforce","iac","quest-software","zayo-group","sas",
#                 "qualcomm","blackberry","berkshire-hathaway-corp","carlyle-group","intuit",
#                 "symantec","broadcom","kkr","medtronic","vmware","sony","lg","motorola-mobility",
#                 "motorola-solutions")
# 
# single.prod <- c('netflix','medallia','dropbox','surveymonkey')  #
###########################################################################

#---------------------------------------------------------------------
#   1.0 ORGANIZATIONS
##   1.1 ORGANIZATION PARENT
##      **RELATION**
##   1.2 BRANCHES

# net <- net1
# end <- 2017
# 
# system.time(
#   mmc <- getMultiMarketContact(br, net%v%'vertex.names', end)
# )
# mmc

##   2 ACQUISITIONS
##      **RELATION**
##      **AGGREGATE**

##   3. PRODUCTS
##      **AGGREGATE**

##   4 Category group -- ALL OK

##   5 COMPETITORS
##      **RELATION**

##   6 CUSTOMERS
##      **RELATION**

##   7.1 EVENTS
##   7.2 EVENTS RELATIONSHIPS
##      **RELATIONS**

##   8.1 FUNDS
##   8.2 FUNDING ROUNDS
##      **RELATION**
##      **AGGREGATE**
##   8.3 IPOs

##   9.1 INVESTORS
##   9.2 INVESTMENTS
##      **RELATION**
##   9.3 INVESTMENT PARTNERS
##      **RELATION**

##   10 JOBS

##   11.1 PEOPLE
##   11.2 PEOPLE  DESCRIPTIONS

#####################################################################################
## MAKE FULL COMP NET OF ALL RELATIONS IN DB 
#####################################################################################
g.full <- makeGraph(comp = co_comp, vertdf = co)
## cut out confirmed dates >= 2016
g.full <- igraph::induced.subgraph(g.full, vids=V(g.full)[which(V(g.full)$founded_year <= 2016
                                                                | is.na(V(g.full)$founded_year)
                                                                | V(g.full)$founded_year=='' ) ] )
g.full <- igraph::delete.edges(g.full, E(g.full)[which(E(g.full)$relation_created_at >= '2017-01-01')])
## SIMPLIFY
g.full <- igraph::simplify(g.full, remove.loops=T,remove.multiple=T,
                           edge.attr.comb = list(weight='sum',
                                                 relation_began_on='min',
                                                 relation_ended_on='min'))
#igraph::write.graph(graph = g.full, file="g_full.graphml", format = 'graphml')

g.full <- read.graph('g_full.graphml', format='graphml')

####################################################################
####################################################################
#-----------------------------------------------------------------
#
#                 Create Dynamic igraph form TERGM
#                   by ONLY REMOVING EDGES
#                   KEEP ALL N nodes
#                   compute PREDICTORS
#             
#                 MAIN LOOP
#
#infomap
#optimal
#walktrap
#spinglass
#leading.eigenvector
#multilevel
#fastgreedy
#label.propagation
#edge.betweenness

#----------------------------------------------------------------
##-------------------FIND MARKET of suitable size --------------
#----------------------------------------------------------------
firms <- V(g.full)$name
deg <- igraph::degree(g.full)
firms.sub <- firms[which(deg > 8 & deg < 13)]
#View(data.frame(name=firms.sub)) 
##
name_i <- 'medallia'   ## check companies
k <- 2
(nbs <- neighbors(g.full, v = V(g.full)[which(V(g.full)$name==name_i)]))
g.ego <- make_ego_graph(g.full, order=k, 
                        nodes = V(g.full)[which(V(g.full)$name==name_i)] )[[1]]
cat(vcount(g.ego))
##
egodeg <- igraph::degree(g.ego, normalized = F)^.4
plot(g.ego, layout=layout.kamada.kawai,
     vertex.size=egodeg*1.0,vertex.label.cex=egodeg*.2)
##------------------ possible focal firm &  markets -------------------
# c('visa','mastercard')    ## PCI; square,mint,paypal too many edges 
# c('zipcar','lyft')   ## ride share / p2p transport;  uber too many edges
# c('fitbit')   ## fitness IOT
# c('agoda')     ## online booking & reservations
# c('zenefits')  ## HR & insurance SW & Services
# c('basecamp)   ## PM & productivity software
##--------------------------------------------------------------
View(head(co[grep('biotec',
                  co$short_description,
                  ignore.case = T,perl=T) & 
               co$company_name_unique %in% firms,],100))

#-----------------------------------------------------------------
# ## EFM / CEM 
# firms.todo <-  c('medallia','clarabridge','qualtrics','satmetrix','confirmit',
#                  'empathica','allegiance','hybris','customergauge',
#                  'mindshare-technologies','markettools')


#--------------------------------------------------------------------
#    Envelopment Risk Firms (without too many connections)
#--------------------------------------------------------------------
firms <- V(g.full)$name
deg <- igraph::degree(g.full)
min <- 1
max <- Inf
firms.sub <- firms[which(deg > min & deg < max)]

rindex <- which(co$status %in% c('closed','acquired') 
                #& co$company_name_unique %in% firms.sub
                #& (co$acquired_year >= 2012 | co$closed_year >= 2012)
                & co$closed_year >= 2010
                )

cindex <- c('company_name_unique', 'company_name', 'acquired_on','closed_on','homepage_url','short_description','category_list') 
sub <- co[rindex, cindex]
cnt <- plyr::count(co_comp[co_comp$company_name_unique %in% sub$company_name_unique
                           | co_comp$competitor_name_unique %in% sub$company_name_unique, 'company_name_unique'])
names(cnt) <- c('company_name_unique','competitor_count')
sub <- merge(sub, cnt, by.x='company_name_unique',all.x=T)
sub <- sub[order(sub$competitor_count, decreasing=T), ]
sub <- sub[ !is.na(sub$competitor_count) & sub$competitor_count > 0, ]
dim(sub)
View(sub)

filename <- sprintf('envrisk_CLOSED_companies_full_names_r%s.csv', nrow(sub))
write.table(sub, file = filename, sep = ',', row.names = F, col.names = T, na = " ")


##---------------------------------------------------------------------
#       News stories
#_---------------------------------------------------------------------
filename <- 'C:/Users/sdowning/Google Drive/PhD/Dissertation/competition networks/envelopment/cb_cem/cb_cem_news_3.csv'
news <- read.table(filename, header = T, sep = ',', na.strings = c('NA','','na'), fill = T, stringsAsFactors = F)
head(news$url, 30)

# pattern1 <- "^http(?s)[:\\/.]+"
# pattern2 <- "\\.com.+"
# text <- "https://gigaom.com/2014/05/27/acquia-scores-50m-in-series-f-funding/"

getDomain <- function(text) {
  pattern1 <- "http(?s)(w{0,3})[:\\/.]+"
  pattern2 <- "\\.com.+"
  x <- gsub(pattern1, '', text, ignore.case=T, perl=T)
  out <- gsub(pattern2, '', x, ignore.case=T, perl=T)
  return(out)
}


domains <- unname(sapply(news$url[1:30], getDomain))

cnt <- plyr::count(domains)
head(cnt)

##--------------------------------------------------------------
##--------------------------------------------------------------
##--------- CREATE FIRM NETWORK PERIOD LISTS  ------------------
##--------------------------------------------------------------
##--------------------------------------------------------------

## creat list if not exists
if( !('firm.nets' %in% ls()) ) firm.nets <- list()

## set market group of firms
net_group <- 'test'
if( !(net_group %in% names(firm.nets)) ) firm.nets[[net_group]] <- list()

## set firms to create networks
firms.todo <- c('visa','mastercard','fitbit','ridejoy')  # c('fitbit','runtastic','zipcar','ridejoy','visa','mastercard')

## run main network period creation loop
for (i in 1:length(firms.todo)) {
  ## -- settings --
  k <- 2
  yrpd <- 1
  startYr <- 2007
  endYr <- 2017
  ## --------------
  name_i <- firms.todo[i]
  cat(sprintf('\n---------%s----------\n',name_i))
  periods <- seq(startYr,endYr,yrpd)
  company.name <- 'company_name_unique'
  verbose <- TRUE
  #
  #g.base <- igraph::make_ego_graph(g.full,order=k,nodes=V(g.full)[V(g.full)$name=='surveymonkey'])[[1]]
  g.base <- g.full
  g.k.sub <- igraph::make_ego_graph(graph = g.base, nodes = V(g.full)[V(g.full)$name==name_i], order = k, mode = 'all')[[1]]
  net.k.sub <- getNetFromIgraph(g.k.sub)
  net <- net.k.sub
  net %n% 'ego' <- name_i
  #----------------Network List-------------------
  nl <- list()
  for (t in 2:length(periods)) {
    cat(sprintf('\nmaking period %s-%s:\n', periods[t-1],periods[t]))
    tmp.net <- makePdNetwork(net.k.sub, 
                             start=periods[t-1], end=periods[t])
    nl[[t]] <- setCovariates(tmp.net, periods[t-1], periods[t],
                             netRiskCommunityAlgo='multilevel.community',
                             downweight.env.risk=FALSE,
                             acq=co_acq,br=co_br,rou=co_rou,ipo=co_ipo)
  }
  nl.bak <- nl
  nl <- nl[which(sapply(nl, length)>0)]
  names(nl) <- periods[2:length(periods)]
  ## ---------- add LAGS ----------------
  for (t in 2:length(nl)) { 
    nl[[t]] %v% 'net_risk_lag' <- nl[[t-1]] %v% 'net_risk'
    nl[[t]] %n% 'DV_lag' <- nl[[t-1]][,]
    # nl[[t]] <- network::set.network.attribute(nl[[t]], 'dist_lag', (nl[[t-1]] %n% 'dist') )
    # g.tmp <- getIgraphFromNet(nl[[t]])
    # if (vcount(g.tmp)>0 & ecount(g.tmp)>0) {
    #   nl[[t]] %v% 'constraint' <- igraph::constraint(g.tmp)
    # }
  }
  ##--------------- GET TERGM NETS LIST -----------
  ## only nets with edges > 0
  nets.all <- nl[2:length(nl)]
  nets <- nets.all[ which(sapply(nets.all, getNetEcount) > 0) ]
  #-------------------------------------------------
  
  ## SAVE variable in image
  # firm.nl <- list()
  firm.nets[[net_group]][[name_i]] <- nets
  
  ## CAREFUL TO OVERWRITE 
  file.name <- sprintf('netrisk_dynamic_firm_nets_1yr_v3_%s.RData',net_group)
  save.image(file.name)
    
}


# load('netrisk_dynamic_firm_nets.RData')


## # ADD SIMILARITY NETWORK PROPERTY TO FIRM.NETS
# for (i in seq_along(firm.nets)) {
#   firm.list <- firm.nets[[i]]
#   for (j in seq_along(firm.list)) {
#     net <- firm.list[[j]]
#     g.net <- getIgraphFromNet(net)
#     sim <- igraph::similarity(g.net,vids = V(g.net), 
#                               mode = "all", method = "invlogweighted" )
#     sim[is.nan(sim) | is.na(sim)] <- 0
#     firm.nets[[i]][[j]] %n% 'similarity' <- sim
#   }
# }

# # ADD CONSTRAINT NODE PROPERTY
# for (t in 1:length(nets)) {
#   g.tmp <- getIgraphFromNet(nets[[t]])
#   if (vcount(g.tmp)>0 & ecount(g.tmp)>0) {
#     cons <-  igraph::constraint(g.tmp)
#     cons[is.nan(cons) | is.na(cons)] <- 0 ### ???
#     nets[[t]] %v% 'constraint' <- cons
#   }
# }

#------------------------------------------------------
#              Predictors Diagnostics
#------------------------------------------------------
## Plot density
n <- ceiling(sqrt(length(firm.nets)))
m <- ifelse(n*(n-1) >= length(firm.nets), n-1, n)
par(mfrow=c(m,n), mar=c(2.5,2.5,2,1))
for (firm_i in names(firm.nets)) {
  nets <- firm.nets[[firm_i]]
  plot(as.numeric(names(nets))-1,   
       sapply(nets,function(net) {
         sum(net[lower.tri(net)])/(nrow(net[,])*nrow(net[,]-1)/2) 
       }), 
       ylab='density', xlab='year', type='b', main=firm_i)
}

## Net Risk
par(mfrow=c(3,3), mar=c(2.5,2.5,2,1))
for (firm_i in names(firm.nets)) {
  nets <- firm.nets[[firm_i]]
  sapply(seq_along(nets), function(j) {
    hist(nets[[j]] %v% 'net_risk', breaks=25, main=sprintf('%s %s',firm_i,names(nets)[j]))
  })
}

#------------------------------------------------------
############################################################################
#--------------------- BTERGM ------------------------------
############################################################################
nets.sub <- nets
mmc <- lapply(nets.sub,function(net) as.matrix(net %n% 'mmc'))


# nets <- list()
# pds <- 2007:2016
# for (i in 2:length(pds)) {
#   nets[[i]] <- network(network.extract(nd, onset=pds[i-1], terminus=pds[i])[,], directed = F, hyper = F, multiple = F, loops = F, bipartite = F)
# }; names(nets) <- pds; nets <- nets[-1]

fb0 <- btergm(nets.sub ~ edges + gwesp(0, fixed=T) + nodecov('age') + 
                nodefactor('state_code') + nodematch('state_code', diff=F) +  
                 nodecov('net_risk'),
              R = 5, parallel = "multicore", ncpus = detectCores())

fb1 <- btergm(nets.sub ~ edges + gwesp(0, fixed=T) + nodecov('age') + 
                nodematch('state_code', diff=F) +  nodefactor('state_code') + 
                nodecov('net_risk') + edgecov(mmc),
              R = 5, parallel = "multicore", ncpus = detectCores())

fb3 <- btergm(nets.sub ~ edges + gwesp(0, fixed=T)  + cycle(3:4) + 
                nodefactor('state_code') + nodematch('state_code', diff=F) +
                nodecov('age') +   edgecov(mmc)  +
                nodecov('net_risk') + nodecov('net_risk_lag') +
                nodematch('npm',diff=F)
               ,
              R = 20, parallel = "multicore", ncpus = detectCores())


fb4 <- btergm(nets.sub ~ edges + gwesp(0, fixed=T)  + cycle(3:4) + 
                nodefactor('state_code') + nodematch('state_code', diff=F) +
                nodecov('age') +   edgecov(mmc)  +
                nodecov('net_risk') + nodecov('net_risk_lag') +
                nodematch('npm',diff=F) + 
                nodematch('ipo_status', diff=TRUE) 
              ,
              R = 5, parallel = "multicore", ncpus = detectCores())


nets.sub <- nets
mmc <- lapply(nets.sub,function(net) as.matrix(net %n% 'mmc'))
fb5clar <- btergm(nets.sub ~ edges + gwesp(0, fixed=T)  + cycle(3:5) + 
                nodefactor('state_code') + nodematch('state_code', diff=F) +
                nodecov('age') +   edgecov(mmc)  +
                nodecov('net_risk') + nodecov('net_risk_lag') +
                nodematch('npm',diff=F) + 
                nodematch('ipo_status', diff=TRUE) 
              ,
              R = 30, parallel = "multicore", ncpus = detectCores())
fb6clar <- btergm(nets.sub ~ edges + gwesp(0, fixed=T)  + 
                    cycle(3:5) + 
                    nodematch('state_code', diff=F) +
                    nodecov('age') +   
                    edgecov(mmc)  +
                    nodecov('net_risk') + 
                    nodecov('net_risk_lag') +
                    nodematch('npm',diff=F) + 
                    nodematch('ipo_status', diff=TRUE) +
                    nodecov('constraint') + 
                    absdiff('constraint')
                 ,
                 R = 5, parallel = "multicore", ncpus = detectCores())

(l <- list(fb0, fb5clar, fb6clar))
texreg::screenreg(l, single.row = T)
write.regtable(filterModels(l), filename = "clarabridge_3k_2y_btergm_constraint", digits=3)

#-------------- ADD NETWORK STATS NOT YET COMPUTED ----------------------
# load('netrisk_dynamic_firm_nets.RData')
# firm.nets.bak <- firm.nets
# for (i in 1:length(firm.nets)) {
#   nets <- firm.nets[[i]]
#   for (t in 1:length(nets)) {
#     g.tmp <- getIgraphFromNet(nets[[t]])
#     if (vcount(g.tmp)>0 & ecount(g.tmp)>0) {
#       cons <-  igraph::constraint(g.tmp)
#       cons[is.nan(cons) | is.na(cons)] <- 0 ### ???
#       nets[[t]] %v% 'constraint' <- cons
#       betw <- igraph::betweenness(g.tmp)
#       nets[[t]] %v% 'betweenness' <- betw
#       nets[[t]] %v% 'betweenness_log' <- log(betw + .001) 
#     }
#   }
#   firm.nets[[i]] <- nets
# }
# save.image('netrisk_dynamic_firm_nets_constr_betw.RData')

#------------------------------------------------------------------
#------------- CEM Industry btergm firm MODEL FIT LIST --------------------------
#-----------------------------------------------------------------
firms.todo <- names(firm.nets)
#####
if ( !('l.fit.b' %in% ls()) ) l.fit.b <- list()
nPeriods <- min(sapply(firm.nets,function(net)length(net))) 
resamp <- 1000
yrpd <- 1
if ('tmp.npds' %in% ls()) rm(tmp.npds)
for (i in 1:length(firms.todo)) {
  firm_i <- firms.todo[i]; cat(sprintf('---------%s---------\n',firm_i))
  nets <- firm.nets[[firm_i]]
  nets.sub <- nets[ (length(nets)-nPeriods+1):length(nets) ]
  if ( !('tmp.npds' %in% ls()) )   tmp.npds <- length(nets.sub)
  mmc <- lapply(nets.sub, function(net) as.matrix(net %n% 'mmc'))
  sim <- lapply(nets.sub, function(net) as.matrix(net %n% 'similarity'))
  fit <- btergm(nets.sub ~ edges + gwesp(0, fixed=T)  + cycle(3:6) + 
                 nodefactor('state_code') + nodematch('state_code', diff=F) +
                 nodecov('age') +   edgecov(mmc)  +
                 nodecov('net_risk') + nodecov('net_risk_lag') +
                 nodematch('npm',diff=F) + 
                 nodematch('ipo_status', diff=TRUE)  +
                 nodecov('constraint') + absdiff('constraint') +
                 edgecov(sim) #+
                # nodecov('betweenness') + absdiff('betweenness')
               , R = resamp, parallel = "multicore", ncpus = detectCores())
  l.fit.b[[firm_i]] <- fit
  file.name <- sprintf('fit_list_btergm_%syr_%spd_%sR_.RData', yrpd, tmp.npds, resamp)
  save.image(file.name) # save.image('fit_list_btergm_med_clar_qual_1yr_.RData')
}


#------------------------------------------------------------------
#------------- MISCELLANEOUS markets btergm firm MODEL FIT LIST ---
#-----------------------------------------------------------------
net_group <- 'misc'
nets.group <- firm.nets[[net_group]]
# firms.todo <- names(firm.nets[[net_group]])
firms.todo <- c("medallia","clarabridge", "satmetrix")
#####
if ( !('l.fit.b' %in% ls()) ) l.fit.b <- list()
if ( !(net_group %in% names(l.fit.b)) ) l.fit.b[[net_group]] <- list()
nPeriods <- min(sapply(nets.group,function(net)length(net))) 
resamp <- 100
yrpd <- 1
if ('tmp.npds' %in% ls()) rm(tmp.npds)
for (i in 1:length(firms.todo)) {
  firm_i <- firms.todo[i]; cat(sprintf('---------%s---------\n',firm_i))
  nets <- nets.group[[firm_i]]
  nets.sub <- nets[ (length(nets)-nPeriods+1):length(nets) ]
  if ( !('tmp.npds' %in% ls()) )   tmp.npds <- length(nets.sub)
  mmc <- lapply(nets.sub, function(net) as.matrix(net %n% 'mmc'))
  sim <- lapply(nets.sub, function(net) as.matrix(net %n% 'similarity'))
  fit <- btergm(nets.sub ~ edges + gwesp(0, fixed=T)  + cycle(3:6) + 
                  nodefactor('state_code') + nodematch('state_code', diff=F) +
                  nodecov('age') +   edgecov(mmc)  +
                  nodecov('net_risk') + nodecov('net_risk_lag') +
                  nodematch('npm',diff=F) + 
                  nodematch('ipo_status', diff=TRUE)  +
                  nodecov('constraint') + absdiff('constraint') + 
                  edgecov(sim)  #+
                # nodecov('betweenness') + absdiff('betweenness')
                , R = resamp, parallel = "multicore", ncpus = detectCores())
  l.fit.b[[net_group]][[firm_i]] <- fit
  file.name <- sprintf('fit_list_btergm_%syr_%spd_%sR_%s-grp.RData', yrpd, tmp.npds, resamp,net_group)
  save.image(file.name) # save.image('fit_list_btergm_med_clar_qual_1yr_.RData')
}


##_---------------------------------------------------------
load('fit_list_btergm_med_clar_qual_.RData')
co.str <- names(firm.nets)

l <- list(satmetrix=l.fit.b$satmetrix,
          empathica=l.fit.b$empathica,confirmit=l.fit.b$confirmit,
          clarabridge=l.fit.b$clarabridge,medallia=l.fit.b$medallia,
          qualtrics=l.fit.b$qualtrics, allegiance=l.fit.b$allegiance)
# l <- list(clarabridge=l.fit.b$clarabridge,
#           satmetrix=l.fit.b$satmetrix,
#           empathica=l.fit.b$empathica)
screenreg(l, single.row = T)
write.regtable(l, filename='fit_list_btergm_1yr_6pd_')

write.regtable(l.fit.b$misc, filename='fit_list_btergm_1yr_5pd_misc_efm', ci.force.level=0.01)
#----------------------------------------------


nr <- c(.38,.47,.29,.5,.4,.51,.45,.46,.32)

#-------------------------------------------------------
#  PLOT COEFS DISTRIBUTION
#-------------------------------------------------------
l.fit.b.sub <- l.fit.b$misc[ which(names(l.fit.b$misc) != 'medallia') ]
if ('df.coefs' %in% ls()) rm(df.coefs) 
for (i in seq_along(l.fit.b.sub)) {
  firm_i <- names(l.fit.b.sub)[i]
  coefs <- unlist(l.fit.b.sub[[firm_i]]@coef)
  coefs <- coefs[!grepl('state_code[.]',names(coefs),ignore.case=T,perl=T)]
  tmp <- data.frame(name=coefs)
  names(tmp)[1] <- firm_i
  if ( !('df.coefs' %in% ls()) ) 
    df.coefs <- data.frame(coefs=names(coefs))
  df.coefs <- cbind(df.coefs, tmp)  
}

## Melt
df.coefs.m <- melt(df.coefs, id.vars = 'coefs')

## Add market category
# df.coefs.m$market <- sprintf('EFM (n=%s)',length(unique(df.coefs.m$variable)))
df.coefs.m$market <- NA
markets <- list(FIoT=c('fitbit','runtastic'),
                P2PT=c('zipcar','ridejoy'),
                PCI=c('visa','mastercard'),
                EFM=c('clarabridge','satmetrix'))
for (i in seq_along(markets)) {
  df.coefs.m[which(df.coefs.m$variable %in% markets[[i]]), 'market'] <- names(markets)[i]
}

# bwplot(value ~ coefs, data=df.coefs.m)

##RENAME
sw <- function(x){
  switch(x,
    'absdiff.constraint' = '2. Abs. Diff. Constraint',
    'nodematch.ipo_status.1' = '1. Size Homophily (IPO)',
    'nodematch.ipo_status.0' = '1. Size Homophily (Private)',
    'nodecov.net_risk' = '0. Net Risk',
    'cycle3' = '3. cycle3',
    'cycle4' = '3. cycle4',
    'cycle5' = '3. cycle5',
    'cycle6' = '3. cycle6'
  )
}

## HYPOTHESES
hyp <- c('absdiff.constraint', 'nodematch.ipo_status.1',
         'nodematch.ipo_status.0','nodecov.net_risk','cycle3','cycle4',
         'cycle5','cycle6')
df.sub.hyp <- subset(df.coefs.m, subset= (coefs %in% hyp) )
df.sub.hyp$coefs <- sapply(df.sub.hyp$coefs, function(x) sw(as.character(x)))
## CONTROLS
df.sub.con <- subset(df.coefs.m, subset= !(coefs %in% hyp) )
## BW PLOT of COEFFICIENTS
ggplot(data=df.sub.hyp, aes(x=coefs, y=value, fill=market)) + ## fill=industry
  geom_boxplot() +
  scale_fill_brewer(type = 'qual') + 
  geom_hline(yintercept = 0) + coord_flip() +
  ylab('Point Estimate') + xlab('Variable') + 
  # scale_y_log10() +
  theme_bw() + theme(legend.position="top")
ggsave('btergm_coef_bwplot_compare_misc_HYP.png',width = 8, height = 6, units = 'in', dpi = 250)

ggplot(data=df.sub.con, aes(x=coefs, y=value, fill=market)) + ## fill=industry
  geom_boxplot() +
  scale_fill_brewer(type = 'qual') + 
  geom_hline(yintercept = 0) + coord_flip() +
  ylab('Point Estimate') + xlab('Variable') + 
  # scale_y_log10() +
  theme_bw() + theme(legend.position="top")
ggsave('btergm_coef_bwplot_compare_misc_CONTR.png',width = 8, height = 6, units = 'in', dpi = 250)


#---------------------------------------------------------
# --------- BTERGM HYPOTHESES MODEL  COMPARE -------------
#---------------------------------------------------------
nPeriods <- 5
resamp <- 100
net_group <- 'misc'
firm_i <- 'clarabridge'
if ( !('l.hyp' %in% ls()) ) l.hyp <- list()
if ( !(net_group %in% names(l.hyp)) ) l.hyp[[net_group]] <- list()
if ( !(firm_i %in% names(l.hyp[[net_group]])) ) l.hyp[[net_group]][[firm_i]] <- list()
nets.sub <- firm.nets[[net_group]][[firm_i]]
nets.sub <- nets.sub[(length(nets.sub)-nPeriods+1):(length(nets.sub))]
mmc <- lapply(nets.sub, function(net) as.matrix(net %n% 'mmc'))
sim <- lapply(nets.sub, function(net) as.matrix(net %n% 'similarity'))

l.hyp[[net_group]][[firm_i]]$fbc <- btergm(
                nets.sub ~ edges + gwesp(0, fixed=T) + 
                nodefactor('state_code') + nodematch('state_code', diff=F) +
                nodecov('age') +   edgecov(mmc)  +
                nodematch('npm',diff=F) + 
                edgecov(sim)  #+
              , R = resamp, parallel = "multicore", ncpus = detectCores())

l.hyp[[net_group]][[firm_i]]$fb0 <- btergm(
                nets.sub ~ edges + gwesp(0, fixed=T) + 
                nodefactor('state_code') + nodematch('state_code', diff=F) +
                nodecov('age') +   edgecov(mmc)  +
                nodematch('npm',diff=F) + 
                edgecov(sim)  +
                nodecov('net_risk') + nodecov('net_risk_lag') 
              , R = resamp, parallel = "multicore", ncpus = detectCores())

l.hyp[[net_group]][[firm_i]]$fb1 <- btergm(
                nets.sub ~ edges + gwesp(0, fixed=T) + 
                nodefactor('state_code') + nodematch('state_code', diff=F) +
                nodecov('age') +   edgecov(mmc)  +
                nodematch('npm',diff=F) + 
                edgecov(sim)  +
                nodematch('ipo_status', diff=TRUE)
              , R = resamp, parallel = "multicore", ncpus = detectCores())

l.hyp[[net_group]][[firm_i]]$fb2 <- btergm(
                nets.sub ~ edges + gwesp(0, fixed=T) + 
                nodefactor('state_code') + nodematch('state_code', diff=F) +
                nodecov('age') +   edgecov(mmc)  +
                nodematch('npm',diff=F) + 
                edgecov(sim)  +
                nodecov('constraint') + absdiff('constraint') 
              , R = resamp, parallel = "multicore", ncpus = detectCores())

l.hyp[[net_group]][[firm_i]]$fb3 <- btergm(
                nets.sub ~ edges + gwesp(0, fixed=T)   +
                nodefactor('state_code') + nodematch('state_code', diff=F) +
                nodecov('age') +   edgecov(mmc)  +
                nodematch('npm',diff=F) + 
                edgecov(sim)  +
                cycle(3:6)
              , R = resamp, parallel = "multicore", ncpus = detectCores())

l.hyp[[net_group]][[firm_i]]$fb4 <- btergm(
                nets.sub ~ edges + gwesp(0, fixed=T) + 
                nodefactor('state_code') + nodematch('state_code', diff=F) +
                nodecov('age') +   edgecov(mmc)  +
                nodematch('npm',diff=F) + 
                edgecov(sim)  +
                nodecov('net_risk') + nodecov('net_risk_lag') +
                nodematch('ipo_status', diff=TRUE)  +
                nodecov('constraint') + absdiff('constraint') + 
                cycle(3:6)
              , R = resamp, parallel = "multicore", ncpus = detectCores())

save.image(sprintf('btergm_fit_HYP_%s_%s_%sR.RData',net_group,firm_i,resamp))

write.regtable(l.hyp$misc$clarabridge, html = T, filename = 'btergm_HYP_model_compare_clarabridge')

#---------------------------------------------------------
#             REMOVE BOOTSAMP OUTLIERS
#---------------------------------------------------------

l.fix = list()
l.fix$misc = list()
l.fix$misc$clarabridge = list()
for (model in c('f0', 'f1', 'f2', 'f3', 'f4')) {
  fit <- l.hyp$misc$clarabridge[[model]]
  ol.list <- apply(fit@bootsamp, 2, function(x) which(x > median(x, na.rm = T) + 1.5*IQR(x, na.rm = T) 
                                                      | x < median(x, na.rm = T) - 1.5*IQR(x, na.rm = T)) )
  ol <- unique(unlist(ol.list))
  keep <- seq_len(nrow(fit@bootsamp))[ !( seq_len(nrow(fit@bootsamp)) %in% ol) ]
  fit@bootsamp <- fit@bootsamp[keep, ]
  l.fix$misc$clarabridge[[model]] <- fit
}

l.fix$misc$clarabridge$f3 = f3

#----------------------------------------------------------
#                MCMLE TERGM
##--------------------------------------------------------
# load('netrisk_dynamic_netflix_2y.RData')

net_group <- 'misc'
nets.group <- firm.nets[[net_group]]
firms.todo <- names(firm.nets[[net_group]])
#####
if ( !('l.fit.m' %in% ls()) ) l.fit.m <- list()
if ( !(net_group %in% names(l.fit.m)) ) l.fit.m[[net_group]] <- list()
nPeriods <- min(sapply(nets.group,function(net)length(net))) 
resamp <- 100
yrpd <- 1
if ('tmp.npds' %in% ls()) rm(tmp.npds)
for (i in 1:length(firms.todo)) {
  firm_i <- firms.todo[i]; cat(sprintf('---------%s---------\n',firm_i))
  nets <- nets.group[[firm_i]]
  nets.sub <- nets[ (length(nets)-nPeriods+1):length(nets) ]
  if ( !('tmp.npds' %in% ls()) )   tmp.npds <- length(nets.sub)
  mmc <- lapply(nets.sub, function(net) as.matrix(net %n% 'mmc'))
  sim <- lapply(nets.sub, function(net) as.matrix(net %n% 'similarity'))
  fit <- mtergm(nets.sub ~ edges + gwesp(0, fixed=T)  + cycle(3:6) + 
                nodefactor('state_code') + nodematch('state_code', diff=F) +
                nodecov('age') +   edgecov(mmc)  +
                nodecov('net_risk') + nodecov('net_risk_lag') +
                nodematch('npm',diff=F) + 
                nodematch('ipo_status', diff=TRUE)  +
                nodecov('constraint') + absdiff('constraint') + 
                edgecov(sim)  #+
              # nodecov('betweenness') + absdiff('betweenness')
              ,  parallel = "multicore", ncpus = detectCores())
  l.fit.m[[net_group]][[firm_i]] <- fit
  file.name <- sprintf('fit_list_mtergm_%syr_%spd_%s-grp.RData', yrpd, tmp.npds,net_group)
  save.image(file.name) # save.image('fit_list_btergm_med_clar_qual_1yr_.RData')
}

write.regtable(list(mt6), filename='fit_mtergm_clar')



##-----------------------------------------------------------
#            TEST MTERGM MCMLE
#----------------------------------------------------------
load('netrisk_dynamic_firm_nets_1yr_v3_misc.RData')

nets.sub <- firm.nets$test$clarabridge
mmc <- lapply(nets.sub, function(net) as.matrix(net %n% 'mmc'))
sim <- lapply(nets.sub, function(net) as.matrix(net %n% 'similarity'))

fm.c5 <- mtergm(  nets.sub ~ edges + gwesp(0, fixed=T) + 
             # nodefactor('state_code') +
             # nodematch('state_code', diff=F) +
             nodecov('age') +   # edgecov(mmc)  + # edgecov(ldv) +
             nodematch('npm',diff=F) + 
             edgecov(sim)  +
             #nodematch('ipo_status', diff=TRUE)  +
             nodecov('net_risk') +
             nodecov('constraint') + absdiff('constraint') + 
             cycle(3) + cycle(4) + cycle(5) 
       ,  parallel = "multicore", ncpus = detectCores())


#--------------------------------------------------------------------
#------------------- 1000 Resample to check normality ---------------
#--------------------------------------------------------------------
load('netrisk_dynamic_firm_nets_1yr_v2_misc.RData')
## save.image('netrisk_dynamic_firm_nets_1yr_v2_misc.RData')
n <- 3#length(firm.nets$misc$clarabridge)
nets.sub <- firm.nets$misc$clarabridge[(n-6+1):n]
mmc <- lapply(nets.sub, function(net) as.matrix(net %n% 'mmc'))
sim <- lapply(nets.sub, function(net) as.matrix(net %n% 'similarity'))
ldv <- lapply(nets.sub, function(net) as.matrix(net %n% 'DV_lag'))
fbc6.100 <- btergm(nets.sub ~ edges + gwesp(0, fixed=T) + 
                      nodefactor('state_code') + nodematch('state_code', diff=F) +
                      nodecov('age') +   edgecov(mmc)  +
                      nodematch('npm',diff=F) + 
                      edgecov(sim)  + edgecov(ldv) +
                      nodecov('net_risk') + nodecov('net_risk_lag') +
                      nodematch('ipo_status', diff=TRUE)  +
                      nodecov('constraint') + absdiff('constraint') + 
                    cycle(3) + cycle(4) + cycle(5) + 
                    cycle(6)
                    , R=30, parallel = "multicore", ncpus = detectCores())  #parallel = "multicore", ncpus = detectCores()
btergm.se(fbc6.100, print=T)
(gofc6.100 <- btergm::gof(fbc6.100, nsim=30))
btergm::plot(gofc6.100)
###
fbc5.100 <- btergm(nets.sub ~ edges + gwesp(0, fixed=T) + 
                 nodefactor('state_code') + nodematch('state_code', diff=F) +
                 nodecov('age') +   edgecov(mmc)  +
                 nodematch('npm',diff=F) + 
                 edgecov(sim)  +
                 nodecov('net_risk') + nodecov('net_risk_lag') +
                 nodematch('ipo_status', diff=TRUE)  +
                 nodecov('constraint') + absdiff('constraint') + 
                 cycle(3) + cycle(4) + cycle(5)
               , R=100, parallel = "multicore", ncpus = detectCores())  #parallel = "multicore", ncpus = detectCores()
btergm.se(fbc5.100, print=T)
(gofc5.100 <- btergm::gof(fbc5.100, nsim=30))
btergm::plot(gofc5.100)
###
fbc4.100 <- btergm(nets.sub ~ edges + gwesp(0, fixed=T) + 
                     nodefactor('state_code') + nodematch('state_code', diff=F) +
                     nodecov('age') +   edgecov(mmc)  +
                     nodematch('npm',diff=F) + 
                     edgecov(sim)  +
                     nodecov('net_risk') + nodecov('net_risk_lag') +
                     nodematch('ipo_status', diff=TRUE)  +
                     nodecov('constraint') + absdiff('constraint') + 
                     cycle(3) + cycle(4)
                   , R=100, parallel = "multicore", ncpus = detectCores())  #parallel = "multicore", ncpus = detectCores()
btergm.se(fbc4.100, print=T)
(gofc4.100 <- btergm::gof(fbc4.100, nsim=30))
btergm::plot(gofc4.100)
###
fbc4r.100 <- btergm(nets.sub ~ edges + 
                     nodematch('state_code', diff=F) +
                     nodecov('age') +   edgecov(mmc)  +
                     nodematch('npm',diff=F) + 
                     nodecov('net_risk') +
                     nodematch('ipo_status', diff=TRUE)  +
                     nodecov('constraint') + absdiff('constraint') + 
                     cycle(3) + cycle(4)
                   , R=100, parallel = "multicore", ncpus = detectCores())  #parallel = "multicore", ncpus = detectCores()
btergm.se(fbc4r.100, print=T)
(gofc4r.100 <- btergm::gof(fbc4r.100, nsim=30))
btergm::plot(gofc4r.100)
###
fbc4nl.50 <- btergm(nets.sub ~ edges + gwesp(0, fixed=T) + 
                       nodefactor('state_code') + nodematch('state_code', diff=F) +
                       nodecov('age') +   edgecov(mmc)  +
                       nodematch('npm',diff=F) + 
                       edgecov(sim)  +
                       nodecov('net_risk') + 
                       nodematch('ipo_status', diff=TRUE)  +
                       nodecov('constraint') + absdiff('constraint') + 
                       cycle(3) + cycle(4)
                    , R=10, parallel = "multicore", ncpus = detectCores())  #parallel = "multicore", ncpus = detectCores()
btergm.se(fbc4nl.50, print=T)
(gofc4nl.50 <- btergm::gof(fbc4nl.50, nsim=30))
btergm::plot(gofc4nl.50)
###
fbc3.100 <- btergm(nets.sub ~ edges + gwesp(0, fixed=T) + 
                     nodefactor('state_code') + nodematch('state_code', diff=F) +
                     nodecov('age') +   edgecov(mmc)  +
                     nodematch('npm',diff=F) + 
                     edgecov(sim)  +
                     nodecov('net_risk') + nodecov('net_risk_lag') +
                     nodematch('ipo_status', diff=TRUE)  +
                     nodecov('constraint') + absdiff('constraint') + 
                     cycle(3) 
                   , R=100, parallel = "multicore", ncpus = detectCores())  #parallel = "multicore", ncpus = detectCores()
btergm.se(fbc3.100, print=T)
(gofc3.100 <- btergm::gof(fbc3.100, nsim=30))
btergm::plot(gofc3.100)
###
fbcno <- btergm(nets.sub ~ edges + gwesp(0, fixed=T) + 
                     nodefactor('state_code') + nodematch('state_code', diff=F) +
                     nodecov('age') +   edgecov(mmc)  +
                     nodematch('npm',diff=F) + 
                     edgecov(sim)  +
                     nodecov('net_risk') + nodecov('net_risk_lag') +
                     nodematch('ipo_status', diff=TRUE)  +
                     nodecov('constraint') + absdiff('constraint') 
                   , R=100, parallel = "multicore", ncpus = detectCores())  #parallel = "multicore", ncpus = detectCores()
btergm.se(fbcno, print=T)
(gofno <- btergm::gof(fbcno, nsim=30))
btergm::plot(gofno)
#--------------------------------------------------------------------

mt1 <- mtergm(nets ~ edges + gwesp(0, fixed=T) + 
                nodecov('age') + nodematch('state_code', diff=F) +  
                nodecov('net_risk'),
              parallel = "multicore", ncpus = detectCores())
# fb1 <- btergm(nets ~ edges + gwesp(0, fixed=F) + triangle + cycle(4:6) ,
#               R = 500, parallel = "multicore", ncpus = detectCores())
# fb2 <- btergm(nets ~ edges + gwesp(0, fixed=F) + triangle + cycle(4:6) + 
#                 nodecov('net_risk') , 
#               R = 500, parallel = "multicore", ncpus = detectCores())
mt3 <- mtergm(nets ~ edges + gwesp(0, fixed=T) + triangle + cycle(4:7) + 
                nodecov('age') + nodematch('state_code', diff=F) +  
                nodecov('net_risk') +  edgecov(mmc),
              parallel = "multicore", ncpus = detectCores())

mt4 <- mtergm(nets ~ edges + gwesp(0, fixed=T) + triangle + cycle(4:7) + 
                nodecov('age') + nodematch('state_code', diff=F) +  
                nodecov('net_risk') + nodecov('net_risk_lag') + edgecov(mmc),
              parallel = "multicore", ncpus = detectCores())
mt5<- mtergm(nets ~ edges + gwesp(0, fixed=T) + triangle + cycle(4:7) + 
                nodecov('age') + nodematch('state_code', diff=F) +  
                nodecov('net_risk') + nodecov('net_risk_lag') + 
                edgecov(mmc) + 
                nodematch('npm', diff=TRUE),
              parallel = "multicore", ncpus = detectCores())

mt6<- mtergm(nets ~ edges + gwesp(0, fixed=T) + triangle + cycle(4:7) + 
               nodecov('age') + nodematch('state_code', diff=F) +  
               nodecov('net_risk') + nodecov('net_risk_lag') + 
               edgecov(mmc) + nodematch('npm', diff=TRUE) +
               nodematch('ipo_status', diff=TRUE),
             parallel = "multicore", ncpus = detectCores())


(l <- list(mt1, mt3, mt4, mt5, mt6))
texreg::screenreg(l, single.row = T)
write.regtable(filterModels(l), filename = "netflix_k3_2yr_m", digits=3)

#_-------------------------------------------------------------
#                       Compare STERGM, BTERG, MTERGM
#--------------------------------------------------------------
bt1 <- btergm(nets ~ edges + gwesp(0, fixed=T) + nodecov('age') + nodematch('state_code', diff=F) +  nodecov('net_risk'), 
              R = 200, parallel = "multicore", ncpus = detectCores())
mt1 <- mtergm(nets ~ edges + gwesp(0, fixed=T) + nodecov('age') + nodematch('state_code', diff=F) + nodecov('net_risk'), 
              parallel = "multicore", ncpus = detectCores())

c.s <- control.stergm(seed = 1111, parallel = 4, parallel.version.check = T)
nd <- networkDynamic::networkDynamic(network.list=nets.sub)
st1 <- stergm(nets.sub,
             formation= ~ edges + gwesp(0, fixed=T) + nodecov('age') + nodematch('state_code', diff=F) +  nodecov('net_risk') ,
             dissolution= ~ edges + gwesp(0, fixed=T) +  nodecov('net_risk'),
             estimate="CMLE", control = c.s, times = 1:length(nets.sub))

l <- list(boot1=bt1,mle1=mt1)
texreg::screenreg(list(mt1), single.row = T, ci.force = F )
write.regtable(list(mt1), filename = "netflix_k3_2yr_m", digits=3)

# texreg::screenreg(list(bt=bt1,mt=mt1,st=st1), single.row = T, ci.force = F)


#-------------------------------------------------------------------#
#                        Curved Exponential Family
#_-------------------------------------------------------------------
ceb2 <- btergm(nets ~ edges 
              + gwesp(1, fixed=F) 
              + gwdsp(1, fixed=F) 
              + gwdegree(1, fixed=F) 
              + cycle(4:6), 
              R = 500, parallel = "multicore", ncpus = detectCores())
cem2 <- mtergm(nets ~ edges 
               + gwesp(1, fixed=F) 
               + gwdsp(1, fixed=F) 
               + gwdegree(1, fixed=F) 
               + cycle(4:6), 
               R = 500, parallel = "multicore", ncpus = detectCores())

screenreg(list(b=ce2,m=cem2))

############################################################################
#--------------------- STERGM ------------------------------
############################################################################
## example 
# library(ergm) 
# library(statnet) 
# library(tergm) 
# data(samplk) 
# samp <- list() 
# samp[[1]] <- samplk1 
# samp[[2]] <- samplk2 
# samp[[3]] <- samplk3 
# samp[[1]] %v% "charm" <- runif(18) 
# samp[[2]] %v% "charm" <- runif(18) 
# samp[[3]] %v% "charm" <- runif(18) 
# samp[[1]] %n% "closeness" <- matrix(runif(18*18),ncol=18,nrow=18) 
# samp[[2]] %n% "closeness" <- matrix(runif(18*18),ncol=18,nrow=18) 
# samp[[3]] %n% "closeness" <- matrix(runif(18*18),ncol=18,nrow=18) 
# fit1 <- ergm(samp[[1]] ~ edges +nodecov("charm") +edgecov("closeness") ) 
# summary(fit1)
# samp.fit <- stergm(samp, 
#                    formation = ~edges +nodecov("charm") +edgecov("closeness"), 
#                    dissolution = ~edges +mutual, 
#                    estimate = "CMLE", times = 1:3) 
# summary(samp.fit)
##----------------------------------------------------------
c.s <- control.stergm(SA.plot.progress=TRUE, seed = 1111, parallel = 4, parallel.version.check = T)

##
nPeriods <- 3
nets <- firm.nets$clarabridge
nets.sub <- nets[ (length(nets)-nPeriods+1):length(nets) ]
.pd <- as.numeric(names(nets.sub)[2]) - as.numeric(names(nets.sub)[1])
.start <- as.numeric(names(nets.sub)[1]) - .pd
.end <- as.numeric(names(nets.sub)[length(nets.sub)])
## Covariate Matrices
mmc <- lapply(nets.sub, function(net) as.matrix(net %n% 'mmc'))

nd <- networkDynamic::networkDynamic(network.list = nets.sub,
                                     start = 1, 
                                     end =  length(nets.sub))

# nodematch('market2') + nodemix('status')
# baseline check
st6 <- stergm(nw = nd, 
             formation= ~ edges + gwesp(0, fixed=T)  + cycle(3:6) + 
               nodefactor('state_code') + nodematch('state_code', diff=F) +
               nodecov('age') +   edgecov('mmc')  +
               nodecov('net_risk') + nodecov('net_risk_lag') +
               nodematch('npm',diff=F) + 
               nodematch('ipo_status', diff=TRUE)  +
               nodecov('constraint') + absdiff('constraint') ,
             dissolution= ~ edges,
             estimate="CMLE", control = c.s, times = 1:length(nets.sub))
save.image('netrisk_dynamic_fit_stergm_2yr_3pd_.RData')
summary(st6)
write.regtable(list(st6), filename='fit_mtergm_clar')


network::get.edge.attribute(nd,'de_alio_entry.active')
network::get.edge.attribute(nd,'de_alio_entry.active')
pred[[1]]
f1 <- stergm(nd, 
              formation= ~ edges ,
              dissolution= ~ edges,
              estimate="CMLE", control = c.s, times = periods[-1])
write.summary(f1); plot(gof(f1))

f2 <- stergm(nd,
             formation= ~ edges + gwesp(0, fixed=T) + kstar(3:6),
             dissolution= ~ edges + gwesp(0, fixed=T) + kstar(3:6),
             estimate="CMLE",control = c.s, times = 2008:2016)
write.summary(f2); plot(gof(f2))

f3 <- stergm(nd,
             formation= ~ edges + gwesp(0, fixed=T) + cycle(4:6),
             dissolution= ~ edges + kstar(3:5) + triangle,
             estimate="CMLE",control = c.s, times = 2008:2016)
write.summary(f3); plot(gof(f3))

f4 <- stergm(nd,
             formation = ~ edges + gwesp(0, fixed=T) + kstar(3:6)  + cycle(4:6),
             dissolution = ~ edges + gwesp(0, fixed=T) + kstar(3:6) + cycle(4:6),
             estimate="CMLE",control = c.s, times = 2008:2016)
write.summary(f4); plot(gof(f4))

## CUREVED EXPONENTIAL FAMILY fixed=FALSE  breaks the stergm 
## [error msg: "if(any(bad.stat))" evals to TRUE]
f5 <- stergm(nd,
             formation = ~ edges + gwesp(0, fixed=T) + gwdegree(0, fixed=T) + kstar(3:6) + cycle(4:5),
             dissolution = ~ edges + gwesp(0, fixed=T) + gwdegree(0, fixed=T) + kstar(3:6) + cycle(4:5), 
             estimate="CMLE",control = c.s, times = 2008:2016)
write.summary(f5); plot(gof(f5))

nets.u <- unname(nets)
f6 <- stergm(nw = nets.u, 
            formation= ~ edges + triangle + cycle(4:5) + # gwesp(0, fixed=F) #  + 
              nodecov('env_risk') #+ 
              #nodematch('ipo_status') + 
              #edgecov('mmc') 
            ,
            dissolution= ~ edges ,
            estimate="CMLE", control = c.s, times = 1:length(nets.u))

# ## LOCAL TRIANGLE -- where COMMUNITY GIVEN BY NPM (COMMUNITY) PARTITIONING
# formation = ~ localtriangle(comMat)
g <- getIgraphFromNet(net)
mlcom <- igraph::multilevel.community(g)
dmat <- as.matrix( dist(mlcom$membership, method = 'manhattan', diag = T, upper = T) )
comMat <- dmat
comMat[dmat == 0] <- 1  ## same community -->  =  0 distance
comMat[dmat >  0] <- 0  ## diff community -->  >= 1 distance
f6 <- stergm(nd,
            formation = ~ edges 
             + gwesp(0, fixed=T) 
             # + gwdegree(0, fixed=T)
             + kstar(3:6) 
             # + cycle(4:5)
             + localtriangle(comMat)
            ,
            dissolution = ~ edges 
             + gwesp(0, fixed=T) 
             # + gwdegree(0, fixed=T) 
             + kstar(3:6) 
             # + cycle(4:5)
             + localtriangle(comMat)
            ,
            estimate="CMLE",control = c.s, times=2008:2016)
write.summary(f6);plot(gof(f6))


#-----------------------------------------------------------------------
texreg::screenreg(list(btergm=fb1, stergm=f2), stars=c(.001,.01,.05,.1), single.row = F)


## ------------------- NETWORKDYNAMIC PLOTS -----------------------------
data("short.stergm.sim")
ss <- short.stergm.sim
ndtv::timeline(ss)
ndtv::timePrism(ss, at=seq(1,10,by=3))
ndtv::filmstrip(ss, frames=9, mfrow=c(3,3))
ndtv::render.d3movie(ss, filename = )



#-------------------------------------------------------------------------
#                    TEST NET
#--------------------------------------------------------------------------

nets.sub = firm.nets$test$clarabridge

for(i in 2:length(nets.sub)) {
  nets.sub[[i]] %n% 'DV_lag' <- as.matrix(nets.sub[[i-1]][,])
}; nets.sub <- nets.sub[2:length(nets.sub)]

nPeriods <- 3
net_group <- 'test'
firm_i <- 'clarabridge'

if ( !('l.hyp' %in% ls()) ) l.hyp <- list()
if ( !(net_group %in% names(l.hyp)) ) l.hyp[[net_group]] <- list()
if ( !(firm_i %in% names(l.hyp[[net_group]])) ) l.hyp[[net_group]][[firm_i]] <- list()

mmc <- lapply(nets.sub, function(net) as.matrix(net %n% 'mmc'))
sim <- lapply(nets.sub, function(net) as.matrix(net %n% 'similarity'))
ldv <- lapply(nets.sub, function(net) as.matrix(net %n% 'DV_lag'))


l.hyp[[net_group]][[firm_i]]$f4 <- btergm(
  nets.sub ~ edges + gwesp(0, fixed=T) + 
    #nodefactor('state_code') + 
    nodematch('state_code', diff=F) +
    nodecov('age') +   edgecov(mmc)  + edgecov(ldv) +
    nodematch('npm',diff=F) + 
    edgecov(sim)  +
    nodematch('ipo_status', diff=TRUE)  +
    nodecov('net_risk') +
    nodecov('constraint') + absdiff('constraint') + 
    cycle(3) + cycle(4) #+ cycle(5) #+ cycle(6)
  , R=30, parallel = "multicore", ncpus = detectCores())

fit <- l.hyp$test$clarabridge$f4
interpret(fit, type='tie', i=1,j=2)

system.time(
  ep <- edgeprob(fit)
)

#-------------------------------------------------------------------------
#                    
#--------------------------------------------------------------------------

# library("statnet")
# set.seed(5)
# 
# networks <- list()
# for(i in 1:10){            # create 10 random networks with 10 actors
#   mat <- matrix(rbinom(100, 1, .25), nrow = 10, ncol = 10)
#   diag(mat) <- 0           # loops are excluded
#   nw <- network(mat)       # create network object
#   networks[[i]] <- nw      # add network to the list
# }
# 
# covariates <- list()
# for (i in 1:10) {          # create 10 matrices as covariate
#   mat <- matrix(rnorm(100), nrow = 10, ncol = 10)
#   covariates[[i]] <- mat   # add matrix to the list
# }
# 
# fit <- btergm(networks ~ edges + istar(2) +
#                 edgecov(covariates), R = 100)
# 
# summary(fit)               # show estimation results
# 
# # The same example using MCMC MLE:
# 
# fit2 <- mtergm(networks ~ edges + istar(2) + 
#                  edgecov(covariates))
# 
# summary(fit2)
# 
# # For an example with real data, see help("knecht").
# 
# 
# # Examples for parallel processing:
# 
# # Some preliminaries: 
# # - "Forking" means running the code on multiple cores in the same 
# #   computer. It's fast but consumes a lot of memory because all 
# #   objects are copied for each node. It's also restricted to 
# #   cores within a physical computer, i.e. no distribution over a 
# #   network or cluster. Forking does not work on Windows systems.
# # - "MPI" is a protocol for distributing computations over many 
# #   cores, often across multiple physical computers/nodes. MPI 
# #   is fast and can distribute the work across hundreds of nodes 
# #   (but remember that R can handle a maximum of 128 connections, 
# #   which includes file access and parallel connections). However, 
# #   it requires that the Rmpi package is installed and that an MPI 
# #   server is running (e.g., OpenMPI).
# # - "PSOCK" is a TCP-based protocol. It can also distribute the 
# #   work to many cores across nodes (like MPI). The advantage of 
# #   PSOCK is that it can as well make use of multiple nodes within 
# #   the same node or desktop computer (as with forking) but without 
# #   consuming too much additional memory. However, the drawback is 
# #   that it is not as fast as MPI or forking.
# # The following code provides examples for these three scenarios.
# 
# # btergm works with clusters via the parallel package. That is, the 
# # user can create a cluster object (of type "PSOCK", "MPI", or 
# # "FORK") and supply it to the 'cl' argument of the 'btergm' 
# # function. If no cluster object is provided, btergm will try to 
# # create a temporary PSOCK cluster (if parallel = "snow") or it 
# # will use forking (if parallel = "multicore").
# 
# # To use a PSOCK cluster without providing an explicit cluster 
# # object:
# require("parallel")
# fit <- btergm(networks ~ edges + istar(2) + edgecov(covariates), 
#               R = 100, parallel = "snow", ncpus = 25)
# 
# # Equivalently, a PSOCK cluster can be provided as follows:
# require("parallel")
# cores <- 25
# cl <- makeCluster(cores, type = "PSOCK")
# fit <- btergm(networks ~ edges + istar(2) + edgecov(covariates), 
#               R = 100, parallel = "snow", ncpus = cores, cl = cl)
# stopCluster(cl)
# 
# # Forking (without supplying a cluster object) can be used as 
# # follows.
# require("parallel")
# cores <- 25
# fit <- btergm(networks ~ edges + istar(2) + edgecov(covariates), 
#               R = 100, parallel = "multicore", ncpus = cores)
# stopCluster(cl)
# 
# # Forking (by providing a cluster object) works as follows:
# require("parallel")
# cores <- 25
# cl <- makeCluster(cores, type = "FORK")
# fit <- btergm(networks ~ edges + istar(2) + edgecov(covariates), 
#               R = 100, parallel = "snow", ncpus = cores, cl = cl)
# stopCluster(cl)
# 
# # To use MPI, a cluster object MUST be created beforehand. In 
# # this example, a MOAB HPC server is used. It stores the number of 
# # available cores as a system option:
# require("parallel")
# cores <- as.numeric(Sys.getenv("MOAB_PROCCOUNT"))
# cl <- makeCluster(cores, type = "MPI")
# fit <- btergm(networks ~ edges + istar(2) + edgecov(covariates), 
#               R = 100, parallel = "snow", ncpus = cores, cl = cl)
# stopCluster(cl)
# 
# # In the following example, the Rmpi package is used to create a 
# # cluster. This may not work on all systems; consult your local 
# # support staff or the help files on your HPC server to find out how 
# # to create a cluster object on your system.
# 
# # snow/Rmpi start-up
# if (!is.loaded("mpi_initialize")) {
#   library("Rmpi")
# }
# library(snow);
# 
# mpirank <- mpi.comm.rank (0)
# if (mpirank == 0) {
#   invisible(makeMPIcluster())
# } else {
#   sink (file="/dev/null")
#   invisible(slaveLoop (makeMPImaster()))
#   mpi.finalize()
#   q()
# }
# # End snow/Rmpi start-up
# 
# cl <- getMPIcluster()
# 
# fit <- btergm(networks ~ edges + istar(2) + edgecov(covariates), 
#               R = 100, parallel = "snow", ncpus = 25, cl = cl)

