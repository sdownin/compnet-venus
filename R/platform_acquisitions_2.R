setwd("C:\\Users\\sdowning\\Google Drive\\PhD\\Dissertation\\1. platform acquisition")
library(plyr)
library(devtools)
library(rcrunchbase)
library(reshape2)
library(ggplot2)
library(igraph)
library(stringr)
library(MASS)
library(memisc)
data_dir <- "C:\\Users\\sdowning\\Google Drive\\PhD\\Dissertation\\crunchbase\\crunchbase_CSV - old"

panel.cor <- function(x, y, digits=2, cex.cor)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  test <- cor.test(x,y)
  Signif <- ifelse(round(test$p.value,3)<0.001,"p<0.001",paste("p=",round(test$p.value,3)))
  text(0.5, 0.25, paste("r=",txt))
  text(.5, .75, Signif)
}
panel.smooth<-function (x, y, col = "blue", bg = NA, pch = 18,
                        cex = 0.8, col.smooth = "red", span = 2/3,
                        iter = 3, ...)
{
  points(x, y, pch = pch, col = col, bg = bg, cex = cex)
  ok <- is.finite(x) & is.finite(y)
  if (any(ok))
    lines(stats::lowess(x[ok], y[ok], f = span, iter = iter),
          col = col.smooth)
}
panel.hist <- function(x)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col="cyan", ...)
}

# DATA
comps2 <- read.table(file.path(getwd(),"enigma_crunchbase_competitions_20160106.csv"), sep=",", header=T, quote = '"',fill=T, na.strings = c('','NA'), stringsAsFactors = F)
    names(comps2) <- c('company_name_1','company_name_2','id')
co2 <- read.table(file.path(getwd(),"crunchbase_export_20160105_companies.csv"), sep=",", header=T, quote = '"',fill=T, na.strings = c('','NA'), stringsAsFactors = F)
    co2 <- cbind(name=as.character(co2$name), co2[,which( !(names(co2) %in% c('name')) )])
aq2 <- read.table(file.path(getwd(),"crunchbase_export_20160105_acquisitions.csv"), sep=",", header=T, quote = '"',fill=T, na.strings = c('','NA'), stringsAsFactors = F)

# SUBSET TO COMMON COMPANY NAMES
commonCo <- intersect(co2$name, unique(c(comps2[,1],comps2[,2])) )
co2.c <- co2[  which(co2$name %in% commonCo),  ]
comps2.c <- comps2[  which(comps2[,1] %in% commonCo | comps2[,2] %in% commonCo),  ]

# ucomps <- unique(c(comps2$company_name_1, comps2$company_name_2))
# uco <- unique(co2$name)
# uaq <- unique(c(aq2$company_name, aq2$acquirer_name))
# uall <- unique(c(uco,uaq))
# length(uall[which(uall %in% ucomps)])
# length(ucomps[which(ucomps %in% uall)])


#---------------------------------------------------------
# REGRESSION DATAFRAME
#---------------------------------------------------------


#---------------------------------------------------------
# GRAPH   # remove duplicated names
#---------------------------------------------------------
n <- count(co2.c$name)
n$x <- as.character(n$x)
n <- n[order(n[,2],decreasing = T),]
dupCo <- n[which(n$freq>1),'x']
co2.c.u <- co2.c[which( !(co2.c$name%in%dupCo)),]
edgesNotInVertList <- unique(c(comps2.c[,1],comps2.c[,2]))[which(!unique(c(comps2.c[,1],comps2.c[,2]))%in%co2.c.u$name)]
co2.c.u.m <- merge(x=co2.c.u, 
                   y=data.frame( name=edgesNotInVertList ), 
                   by='name', all = T)

g <- igraph::graph.data.frame(d = comps2.c, directed = F, vertices = co2.c.u.m )











#--------------------------------------------------------
# Load data sources
#---------------------------------------------------------
co <- read.table("C:\\Users\\sdowning\\Google Drive\\PhD\\Dissertation\\1. multi-level platform competition\\cb_companies.csv", sep=",",header=T,quote='"',stringsAsFactors=F,fill=T)

main <- read.table(paste0(data_dir,"\\companies_main.csv"), sep=",",header=T, quote='"', stringsAsFactors = F, fill=T)
main$founded_year <- as.numeric(main$founded_year)
# main$name <- as.factor(main$name)

acq <- read.table(paste0(data_dir,"\\companies_acquisition.csv"), sep=",",header=T, quote='"', stringsAsFactors = F, fill=T)
# acq$name <- as.factor(acq$name)

acqcount <- count(acq, vars = c('name'))
acqcount <- acqcount[order(acqcount[,2],decreasing = T),]

comp <- read.table(paste0(data_dir,"\\companies_competitions.csv"), sep=",",header=T, quote='"', stringsAsFactors = F, fill=T)
# comp$name <- as.factor(comp$name)

# df <- read.table("platform_types_augmented.csv",sep=",",header=T, fill=T, quote='"')
# df$platform_type <- gsub("product_to_platform","product_2_platform",df$platform_type)

# # subset obs with one of two platform types
# plat_types_list <- c('2_sided_platform','product_2_platform')
# df <- df[which(df$platform_type %in% plat_types_list), ]
# df$platform_type <- as.factor(df$platform_type)
# levels(df$platform_type) <- c("2sided","app2plat")


df <- read.table("platforms_vs_others.csv",sep=",",header=T,fill=T,quote='"', stringsAsFactors = F, na.strings = "")
df$platform_type <- gsub("[?]","unknown",df$platform_type)
df$platform_type <- as.factor(df$platform_type)

# sampUnknown <- sample(df[which(df$platform_type=="unknown"),],
#                       100,replace = F)
# df <- df[which(df$platform_type != "unknown"),]
# df <- rbind(df,sampUnknown)
# df <- droplevels(df)
# levels(df$platform_type)
dim(df)
head(df)

df <- merge(df, main[,c('name','founded_year')],by='name',all.x=T)

# df$name <- gsub("[[]","_",df$name)
# df$name <- gsub("[]]","_",df$name)
# df$name <- gsub("[@]","_",df$name)
# df$name <- gsub("[\\]","_",df$name)
# df$name <- gsub("[/]","_",df$name)
# df$name <- gsub("[!]","_",df$name)
# df$name <- gsub("[~]","_",df$name)
# df$name <- gsub("[`]","_",df$name)
# df$name <- gsub("[,]","_",df$name)
# df$name <- gsub("[.]","_",df$name)
# df$name <- gsub("[$]","_",df$name)
# df$name <- gsub("[%]","_",df$name)
# df$name <- gsub("[&]","_",df$name)
# df$name <- gsub("[*]","_",df$name)
# df$name <- gsub("[-]","_",df$name)
# df$name <- gsub("[|]","_",df$name)

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

# number of acquired companie who were in same market
acqcount$acquired_same_market <- NA
for (i in 1:nrow(acqcount)) {
  co_i <- as.character(acqcount$name[i])
  mark_i <- co[which(co$name == co_i),'market']
  mark_i <- gsub(" ","",mark_i)
  acq_i <- dfacq[which(dfacq$name == co_i),c('acquired_company')]
  # for each company j  acquired by i, get the name of the acquired company's market
  acq_i_mark_j <- c()
  for (j in 1:length(acq_i)) {
    s <- co[which(co$name == acq_i[j]), 'market']
    # some names may match multiple so return all matched markets
    # add them to vector of acquired markets
    for (k in 1:length(s)) {
      acq_i_mark_j[length(acq_i_mark_j)+1] <- ifelse(length(gsub(" ","",s[k]))>0,
                                                     gsub(" ","",s[k]),
                                                     NA)
    }
  }
  #acq_markets <- co[which(co$name %in% acq_i),'market']
  acqcount$acquired_same_market[i] <- sum(mark_i %in% acq_i_mark_j)
}
acqcount <- acqcount[order(acqcount[,3],decreasing = T),]
head(acqcount)

# dfall$acq_comp_percent <- dfall$acquired_competitors / dfall$comp_count

#-------------------------------------------------------
#
# Prepare regression dataframe
#
#-------------------------------------------------------

dfall <- merge(df,countcomp,by='name',all=T)
dfall$age <- 2015 - dfall$founded_year

# add number of competitors
# company names list
names <- unique(df$name)


# number of acquired companies who were competitors of the acquirer
dfall$acquired_competitors <- NA
for (i in 1:nrow(dfall)) {
  co_i <- dfall$name[i]
  acq_i <- dfacq[which(dfacq$name == co_i),c('acquired_company')]
  comp_i <- dfcomp[which(dfcomp$name==co_i),c('competitions')]
  acq_comp_i <- sum(acq_i %in% comp_i)   # count the number of TRUES
  dfall$acquired_competitors[i] <- acq_comp_i
}
dfall$acq_comp_percent <- dfall$acquired_competitors / dfall$comp_count


# diversity of acquired companies
dfall$acquired_markets <- 0
dfall$acquired_categories <- 0
dfall$compsamemarket <- 0
dfall$compdiffmarket <- 0
dfall$comps_markets <- 0

for (i in 1:nrow(dfall)) {
  co_i <- dfall$name[i]
  acq_i <- dfacq[which(dfacq$name == co_i),c('acquired_company')]
  acq_markets <- co[which(co$name %in% acq_i),'market']
  dfall$acquired_markets[i] <- length(unique(acq_markets))
  #
  comps_i <- comp[which(comp$name == co_i),c('competitions')]
  comps_markets <- co[which(co$name %in% comps_i),'market']
  dfall$comps_markets[i] <- length(comps_markets)
  m <- co[which(co$name == co_i),'market']
  dfall$compsamemarket[i] <- length(comps_markets[which(comps_markets == m)])
  dfall$compdiffmarket[i] <- length(comps_markets[which(comps_markets != m)])
#   print(dfall$comps_markets[i])
#   print(dfall$compsamemarket[i])
#   print(dfall$compdiffmarket[i])

  # categories list combined
  acq_categories <- co[which(co$name %in% acq_i),'category_list']
  if (length(acq_categories) > 0 ) {
    holder <- c()
    for (j in 1:length(acq_categories)) {
      # split list at bar with escape characters []; take first item from list
      cats_j <- strsplit(acq_categories[j],split = "[|]")[[1]]
      # drop the empy space entries in vector of categories
      holder <- c(holder,cats_j[which(cats_j != "")])

    } #end j loop for all categories within one company i
    dfall$acquired_categories[i] <- length(unique(holder))
  } #end if

  # NEW competitor markets


} # end all companies i...N
# dfall$market_diversity <- dfall$acquired_markets / dfall$count
# dfall$category_diversity <- dfall$acquired_categories / dfall$count


# how many acquired companies are in same market as acquiring co
dfall$acquired_same_market <- NA
for (i in 1:nrow(dfall)) {
#   co_i <- dfall$name[i]
#   mark_i <- co[which(co$name == co_i),'market']
#   #mark_i <- gsub(" ","",mark_i)
#   acq_i <- dfacq[which(dfacq$name == co_i),c('acquired_company')]
#   acq_markets <- co[which(co$name %in% acq_i),'market']
#   dfall$acquired_same_market[i] <- sum(mark_i %in% acq_markets)

  co_i <- as.character(dfall$name[i])
  mark_i <- co[which(co$name == co_i),'market']
  mark_i <- gsub(" ","",mark_i)
  acq_i <- dfacq[which(dfacq$name == co_i),c('acquired_company')]
  # for each company j  acquired by i, get the name of the acquired company's market
  acq_i_mark_j <- c()
  for (j in 1:length(acq_i)) {
    s <- co[which(co$name == acq_i[j]), 'market']
    # some names may match multiple so return all matched markets
    # add them to vector of acquired markets
    for (k in 1:length(s)) {
      acq_i_mark_j[length(acq_i_mark_j)+1] <- ifelse(length(gsub(" ","",s[k]))>0,
                                                     gsub(" ","",s[k]),
                                                     NA)
    }
  }
  #acq_markets <- co[which(co$name %in% acq_i),'market']
  dfall$acquired_same_market[i] <- sum(mark_i %in% acq_i_mark_j)
}
View(dfall)

# write regression data to file
write.table(dfall,"poisson_regression_all4000_df.csv",sep=",",col.names = T,row.names = F)

#-------------------------------------------------
#
#  Build network of competition
#
#--------------------------------------------------
compnames <- unique(c(as.character(comp$name),
                      as.character(comp$competitions)))
vertices <- merge(x=main[which(main$name %in% compnames),
                         c('name','founded_year','number_of_employees',
                           'total_money')],
                  y=co[which(co$name %in% compnames),
                       c('name','market','founded_at',
                         'funding_total_usd','country_code','status')],
                  by='name', all=T)
vertices <- vertices[which(!duplicated(vertices$name)),]
discrep <- compnames[!(compnames %in% as.character(vertices$name)) ]
discrepdf <- data.frame(name=discrep)
discrepdf[,c(2:(ncol(vertices)))] <- NA
names(discrepdf) <- names(vertices)
vertices <- rbind(vertices,discrepdf)

# build graph object and add vertex attributes manually
g1 <- graph.data.frame(d=comp[,c('name','competitions')],directed = FALSE,vertices=vertices)

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


V(g1)$betweenness <- betweenness(g1)
V(g1)$degree <- degree(g1)
degdf <- data.frame(name=V(g1)$name, degree=V(g1)$degree)
degdf <- degdf[order(degdf[,2],decreasing = T),]
head(degdf)

#find largest connected component
comps <- decompose.graph(g1, mode='weak', max.comps =NA ,min.vertices = 2)
lcc <- comps[[which.max(sapply(comps,vcount))]]

# color by market
marvec <- unique(V(g1)$market)
colors <- rainbow(length(marvec),alpha = 0.4)
for (i in 1:length(marvec)) {
  V(g1)[which(V(g1)$market == marvec[i])]$color <- colors[i]
}


# save graphml
# igraph::write.graph(g1,"competition_graph.graphml",format="graphml")
# g1 <- read.graph("competition_graph.graphml", format='graphml')
g1 <- induced.subgraph(g1, vids = V(g1)[which( !(V(g1)$market=="NA") )])


# png("comp_graph_kk.png",height = 20, width=20, units='in', res=200)
# plot.igraph(g1, layout=layout.kamada.kawai,
#             vertex.size=1+(0.3* log(V(g1)$degree)),
#             vertex.label.cex=.1,
#             vertex.color=V(g1)$color,
#             vertex.shape='circle',
#             vertex.label="",
#             vertex.label.cex=V(g1)$degree * 0.05,
#             #vertex.label.color='white',
#             edge.arrow.size=0.05,
#             edge.width=0.5,
#             #edge.label=E(sub)$weight,
#             #edge.label.cex=E(sub)$edgelabelcex,
#             #edge.label.color='black',
#             edge.color=gray.colors(1,start = 0, alpha=0.5)
# )
# dev.off()

#--------------------------------
makeCompNet <- function(g, vertvec)
{
  complist <- list()
  for ( i in 1:length(vertvec)) {
    i_vert <- V(g)[which(V(g)$name==vertvec[i])]
    i_neigh <- neighbors(graph = g, v=i_vert, mode='all')
    # vertices who are not neighbors of i
    #keep vertex i in its own net
    delverts <- V(g)[which( !(V(g) %in% c(i_neigh,i_vert)) )]
    keepverts <- V(g)[which( V(g) %in% c(i_neigh,i_vert)   )]
    # delete all vertices that are not neighbors of i; save as i's comp net
    # complist[[i]] <- delete.vertices(graph = g, v = delverts)
    # keep vertices in subgraph that are in neighbors list
    complist[[i]] <- induced.subgraph(g, vids=keepverts)
    if(i%%ceiling(length(vertvec)/5)==0){cat(paste("\ncompleted vertex", i))}
  }
  return(complist)
}
#--------------------------------


# find competition networks
complist <- makeCompNet(g = g1, vertvec = as.character(dfall$name))


# define DISTANCE WEIGHTED REACH to be computed as predictor
#################################
distWeightReach <- function(g,
                            mode='in',
                            weights=NA
) {
  D <- c()
  #for each vertex
  for (k in 1:vcount(g)) {
    #find vertex subcomponent
    vec <- subcomponent(graph = g,v = V(g)[k],mode = mode)
    vec <- vec[order(vec)]

    d <- numeric(length(vec)-1)
    #for each other vertex l in subcomponent of vertex k
    for (l in 1:(length(vec))) {
      # excluding when k=l (which would make -Inf length)
      if (k!=vec[l]) {
        # vertex path of geodesic from k to l
        dp <-  unlist(get.shortest.paths(graph = g, from = k, to = vec[l],
                                         mode=mode, weights=weights,
                                         output="vpath")$vpath) #directed graph
        # inverse of length of geodesic from k to l
        #  -1 to subtract the origin vertex
        d[l] <- 1 / ( length(dp)-1 )
      }
    }
    D[k] <- sum(d)
  } #end vertex loop

  R <- sum(D) / vcount(g)
  return(R)
}#end function
#################################



#--------------------------------------------------------
# add graph variables to the data frame
#
dfall$clustering <- NA
dfall$density <- NA
dfall$full_net_betweenness <- NA
# dfall$reach <- NA
for (i in 1:nrow(dfall)) {

  comp <- complist[[i]]

  if (vcount(comp) < 2){
    dfall$clustering[i] <- 0
    dfall$density[i] <- 0
    # dfall$reach[i] <- 0
  } else {
    dfall$density[i] <- ecount(comp)/(vcount(comp)*(vcount(comp)-1))
    # dfall$reach[i] <- distWeightReach(comp, mode='in')
    clust_i <- transitivity(comp, type = 'global')
    if (is.nan(clust_i) | is.na(clust_i)){
      dfall$clustering[i] <- 0
    } else {
      dfall$clustering[i] <- clust_i
    }
  }

  vert_i <- V(g1)[which(V(g1)$name==as.character(dfall$name[i]))]
  b_i <- betweenness(g1, v = vert_i)
  if(length(b_i) >0) {
    dfall$full_net_betweenness[i] <- b_i
  } else {
    dfall$full_net_betweenness[i] <- 0
  }

}   # end add graph variables loop

names(dfall)[which(names(dfall)=="count_acquired")] <- "count"
names(dfall)[which(names(dfall)=="acquired_count")] <- "count"
write.table(dfall, "dfall_all4000.csv", sep=",", row.names=F)

dfall <- read.table("dfall_all4000.csv", header=T, sep=",", quote='"')



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





#------------------------------------------------
#
# Poisson Regression
#
#-------------------------------------------------

#plot acquisitions by competition:
data <- na.omit(dfall[which(dfall$founded_year>=1970),])
ggplot(aes(x=comp_count,y=count,colour=platform_type),data=data) +
  geom_point(aes(colour=platform_type),data=data) +
  geom_smooth(aes(colour=platform_type),method=lm,data=data)

names(dfall)[which(names(dfall)=="count_acquired")] <- "count"
names(dfall)[which(names(dfall)=="acquired_count")] <- "count"

fit <- glm(count ~ platform_type, family=poisson(link='log'), data = dfall)
summary(fit)

# baseline model with only competition
fit0 <- glm(count ~ comp_count + age ,family=poisson(link='log'), data = dfall)
summary(fit0)

# nbfit0 <- glm.nb(count ~ comp_count , data = dfall)
# summary(nbfit0)

# model with platform type
fit1 <-  glm(count ~ comp_count + age + full_net_betweenness,
             family = poisson(link='log'), data = dfall)
summary(fit1)

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


