setwd("C:\\Users\\sdowning\\Google Drive\\PhD\\Dissertation\\1. platform acquisition")
load('aom_analysis.RData')
setwd("C:\\Users\\sdowning\\Google Drive\\PhD\\Dissertation\\competition networks")
library(plyr)
# library(devtools)
# library(rcrunchbase)
library(reshape2)
library(ggplot2)
library(igraph)
library(network)
library(stringr)
library(MASS)
library(memisc)
library(pscl)
library(AER)
library(psych)
library(lubridate)
# library(sna)
# library(ergm)
# library(tergm)
# library(Bergm)
data_dir <- "C:\\Users\\sdowning\\Google Drive\\PhD\\Dissertation\\crunchbase\\crunchbase_CSV - old"



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

ms <- read.table(file.path(data_dir,'companies_milestones.csv'),sep=",",header=T,quote='"',stringsAsFactors = F,fill = T)
ms$mdy_string <- paste(ms$stoned_month,ms$stoned_day,ms$stoned_year,sep="/")
ms$date <- lubridate::mdy(ms$mdy_string)

rel <- read.table(file.path(data_dir,'companies_relationships.csv'),sep=",",header=T,quote='"',stringsAsFactors = F,fill = T)


#(?=^lg)((?!Landis|Laboratoire|C Wireless).)*$
x <- reg.data.frame[grep('(?=Facebook|Twitter)',
                        reg.data.frame$category_list,
                         ignore.case = T,perl = T), ]
x <- x[order(x$acquired_markets, decreasing=T), ]
fb <- x; View(fb)


x <- reg.data.frame[grep('(?=buy|commerce)',
                         reg.data.frame$category_list,
                         ignore.case = T,perl = T), ]
x <- x[order(x$acquired_markets, decreasing=T), ]
buy <- x; View(buy)


x <- reg.data.frame[grep('(?=ibm)',
                         reg.data.frame$name,
                         ignore.case = T,perl = T), ]
x <- x[order(x$acquired_markets, decreasing=T), ]
ibm <- x; View(ibm)



# xdf <- ms
# x <- xdf[grep('(?=medallia)',
#               xdf$description,
#               ignore.case = T,perl = T), ]
# x <- x[order(x$acquired_markets, decreasing=T), ]
# cem <- x; View(cem)


xdf <- ms
x <- xdf[grep('(?=box)',
              xdf$description,
              ignore.case = T,perl = T), ]
x <- x[order(x$acquired_markets, decreasing=T), ]
box <- x; View(box)


#active milestone
xdf <- ms
x <- xdf[grep('(?=launch|introduce|new|product)',
              xdf$description,
              ignore.case = T,perl = T), ]
x <- x[order(x$acquired_markets, decreasing=T), ]
launch <- x; View(launch)

launch.cnt <- count(launch$milestones)
launch.cnt <- launch.cnt[order(launch.cnt$freq, decreasing=T),]
names(launch.cnt)[which(names(launch.cnt)=='x')] <- 'name'
launch.cnt.m <- merge(x=launch.cnt, y= co, by='name', all.x = T, all.y=F)
launch.cnt.m <- launch.cnt.m[order(launch.cnt.m$freq, decreasing=T), ]
View(launch.cnt.m)






View(co[grep('(?=medallia$|satmetrix|clarabridge|customergauge|qualtrics|ibm|allegiance|empathica|markettools|mindshare|responsetek|avaya|sdl$|SAS$|avius|zendesk|clicktale|adobe|kana|gemius|maxymiser|userzoom|ux360|usabilitytools|amazon|oracle|netcracker|userreplay|qmatic|KPMG|econsultancy)',
             co$name,
             ignore.case = T,perl = T), ])


cem.re <- '(?=medallia$|satmetrix|clarabridge|customergauge|qualtrics|ibm|allegiance|empathica|markettools|mindshare|responsetek|avaya|sdl enterprise|^SAS|avius|zendesk|clicktale|adobe|maxymiser|userzoom|usabilitytools|oracle|netcracker|userreplay|KPMG|^SAP$)' 
cem.co <- names( V(g1)[grep(cem.re, V(g1)$name, ignore.case=T, perl=T)] )

ms.co <- ms[which(ms$milestones %in% cem.co ), ]
ms.co2 <- ms[grep(cem.re, ms$description, ignore.case = T,perl = T), ]

vids <- V(g1)[ grep(cem.re, V(g1)$name, ignore.case=T, perl=T) ]


ord <- 2:6
g.s <- lapply(ord,  function(x){
    return( igraph::make_ego_graph(g1,order = x,nodes = vids, mode = 'all')[[1]] )
})



n_cem_co <- sapply(g.s, function(x){ 
  return(length(grep(cem.re, V(x)$name, ignore.case=T, perl=T) )) 
})

##   MAKE SUMMARY DATAFRAME
sdf <- data.frame(order=ord, v=sapply(g.s, vcount),
                  e=sapply(g.s, ecount),n_cem_co=n_cem_co,
                  avgpath=sapply(g.s, igraph::average.path.length),
                  clustering=sapply(g.s, igraph::transitivity)
                  )
sdf$avgdegree <- sdf$e / sdf$v
sdf$density <- sdf$e / (sdf$v * (sdf$v-1))

write.table(sdf,file = "cem_combined_ego_networks.csv",sep = ",",row.names = F, col.names = T, na = "")





s <- 3
g1.sub.plot <- induced.subgraph(g1, vids = V(g1)[which(V(g1)$degree >= 10)])

png("cem_combined_ego_net_3.png",height = 8, width=8, units='in', res=200)
par(mar=c(.1,.1,.1,.1))
g.plot <- g.s[[3]]
plot.igraph(g.plot, layout=layout.fruchterman.reingold,
            vertex.size=log(igraph::degree(g.plot,mode = 'total')) * 1.3,
            vertex.color=V(g.plot)$color,
            vertex.shape='circle',
            vertex.label=ifelse(igraph::degree(g.plot,mode = 'total')>quantile(igraph::degree(g.plot,mode = 'total'),.99),V(g.plot)$name,''),
            vertex.label.cex=log(igraph::degree(g.plot,mode = 'total')) * .2,
            #vertex.label.color='white',
            edge.arrow.size=0.05,
            edge.width=0.5,
            #edge.label=E(g.plot)$weight,
            #edge.label.cex=E(g.plot)$edgelabelcex,
            #edge.label.color='black',
            edge.color='darkgray'
)
dev.off()




## YEARLY COMPETITION NETWORK EVOLUTION----------------------------------
file <- "C:\\Users\\sdowning\\Google Drive\\PhD\\Dissertation\\competition networks\\CEM_companies.csv"
co <- read.csv(file, header=T, encoding = 'UTF-8', fill=T, stringsAsFactors = F)

file <- "C:\\Users\\sdowning\\Google Drive\\PhD\\Dissertation\\competition networks\\cb_cem\\cb_cem_competitors_3.csv"
cn <- read.csv(file, header = T, encoding = 'UTF-8', fill = T, stringsAsFactors = F)
cn$created_at <- as.POSIXct(cn$created_at, origin = '1970-01-01', tz='EST')
cn$updated_at <- as.POSIXct(cn$updated_at, origin = '1970-01-01', tz='EST')
cn$founded_on <- lubridate::ymd(cn$founded_on)

cn$founded_on[is.na(cn$founded_on)] <- cn$created_at[which(is.na(cn$founded_on))]

el <- cn[,c('company','name')]

otherVerts <- data.frame(company=cn$name[which( !(cn$name %in% cn$company) )])
otherVerts$company <- as.character(otherVerts$company)
combinedVdf <- merge(cn, otherVerts, by='company', all=T)
uniqueVerts <- data.frame(company=unique(c(cn$company,cn$name)))
uniqueVerts$company <- as.character(uniqueVerts$company)



vl <- merge(uniqueVerts, combinedVdf, by='company', all = F)



g <- graph.data.frame(el, directed = F, vertices = uniqueVerts)










## NEWS TOPIC MODEL----------------------------------------------
file <- "C:\\Users\\sdowning\\Google Drive\\PhD\\Dissertation\\competition networks\\cb_cem\\cb_cem_news_3.csv"
nw <- read.csv(file, header=T, encoding = 'UTF-8', fill = T, stringsAsFactors = F)










































########## NETWORK PACKAGE DATA STRUCTURE
gx <- g.s[[1]]
V(gx)$numer_of_employees <- as.numeric(V(gx)$number_of_employees)
vertattrs <- c('name','usd','number_of_employees',
              'status', 'founded_at')
vertattrlist <- lapply(vertattrs, function(x) {
    return(igraph::get.vertex.attribute(gx,x))
})
for (i in 2:3) {
    vertattrlist[[i]][is.na(vertattrlist[[i]])] <- 0
}

vertattrlist[[which(vertattrs=='number_of_employees')]] <- as.numeric(vertattrlist[[which(vertattrs=='number_of_employees')]])

n1 <- network(x = igraph::get.edgelist(gx),
              vertex.attr = vertattrlist,
              vertex.attrnames = vertattrs,
              directed = F,hyper = F,loops = F,
              multiple = F, bipartite = F)


# n1 <- network.initialize(vcount(gx))
# for (v in vertattrs) {
#     n1 %v% v <- igraph::get.vertex.attribute(gx, v )
# }
# edgeattrs <- c('')
# for (e in edgeattrs) {
#     n1 %e% e <- igraph::get.edge.attribute(gx, e)
# }

er1 <- ergm(n1 ~ edges + absdiff("usd") + absdiff("number_of_employees") +
                triangle,
            response=NULL,
            reference=~Bernoulli,
            constraints=~.,
            offset.coef=NULL,
            target.stats=NULL,
            eval.loglik=T,
            # estimate=c("MLE", "MPLE", "CD"),
            control=control.ergm(MCMC.samplesize=1e4),
            verbose=T)


ergm(molecule ~ edges + kstar(2:3) + triangle
     + nodematch("atomic type",diff=TRUE)
     + triangle + absdiff("atomic type"))










