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

dups <- function(vec, n=10)
{
    c <- count(vec)
    c <- c[order(c$freq, decreasing=T),]
    return(head(c, n))
}


path <- "C:\\Users\\sdowning\\Google Drive\\PhD\\Dissertation\\competition networks\\crunchbase_export_20160106_companies.csv"
co <- read.csv(path, header=T, fill = T, stringsAsFactors = F,encoding = 'UTF-8')


path <- "C:\\Users\\sdowning\\Google Drive\\PhD\\Dissertation\\competition networks\\cb_cem\\cb_cem_competitors_ALL_nodups.csv"
comp <- read.csv(path, header=T, fill = T, stringsAsFactors = F,encoding = 'UTF-8')
comp <- comp[,-1]
names(comp)[which(names(comp) == 'company')] <- 'competitor'


## ------------------- EDGELIST -----------------------------------------
comp[ grep('^apple$',x=comp$name,ignore.case = F,perl = T), 'name'] <- 'Apple'
comp[ grep('^apple$',x=comp$competitor,ignore.case = F,perl = T), 'competitor'] <- 'Apple'

comp$updated_at <- as.numeric(comp$updated_at)
comp$updated_at_timestamp <- comp$updated_at
comp$updated_at <- lubridate::ymd_hms( format(as.POSIXct(comp$updated_at, origin = '1970-01-01', tz='EST')) )

comp$created_at <- mdy(comp$created_at)

el <- data.frame(name=gsub('[\\]','',str_to_lower(comp$name)), 
                 competitor=gsub('[\\]','',str_to_lower(comp$competitor)),
                 created_at=comp$created_at,
                 updated_at=comp$updated_at,
                 stringsAsFactors = F)
el <- el[order(el$name),]
el$name <- str_to_lower(el$name)


##--------------------- VERTICES LIST ---------------------------------------

# # vert <- unique(comp[,which( !(names(comp)%in%c('competitor','also_known_as')) )])

# vert <- unique( data.frame(name=comp$name, closed_on=comp$closed_on, created_at=comp$created_at,
#                    founded_on=comp$founded_on, is_closed=comp$is_closed,
#                    num_employees_min=comp$num_employees_min, num_employees_max=comp$num_employees_max,
#                    stock_exchange=comp$stock_exchange, stock_symbol=comp$stock_symbol,total_funding_usd=comp$total_funding_usd)
#                  )
# vert <- vert[ -which(vert$name=="Branch" & vert$founded_on==""),]
# dropNames <- as.character( dups(vert$name,20)[which(dups(vert$name, 20)$freq > 1), 'x'] )
# vert <- vert[ -which( (vert$name%in%dropNames) & vert$total_funding_usd<1),]
# dropNames <- as.character( dups(vert$name,20)[which(dups(vert$name, 20)$freq > 1), 'x'] )
# vert <- vert[ which( !(vert$name%in%dropNames)), ]

# u <- as.character( unique(c(el$source, el$target)) )
# missingVerts <- data.frame(name=u[which( !(vert$name%in%u))])
# 
# vert.c <- merge(vert, missingVerts, by = 'name', all=T)

comp$founded_on <- mdy(comp$founded_on)

yrdf <- data.frame(name=comp$name, founded_on=comp$founded_on,
                   total_funding_usd=comp$total_funding_usd)

vert <- data.frame(name=unique(c(comp$name,comp$competitor)))
vert$name <- as.character(vert$name)

## MERGE IN DEFAULT YEARS FROM CO df



# vert$yr <- sapply(vert$name[1:2], function(x){
#     df.sub <- yrdf[which(yrdf$name==x), ]
#     if (nrow(df.sub) < 1) next 
#     if( !is.na(df.sub$founded_on) & df.sub$founded_on != ''  ) {
#         yr <- year( mdy( df.sub$founded_on ) );
#     } else if (!is.na(df.sub$created_at) & df.sub$created_at != '') {
#         yr <- year( mdy( df.sub$created_at ) );
#     } else if (!is.na(df.sub$updated_at) & df.sub$updated_at != '') {
#         yr <- year( mdy( df.sub$updated_at ) );
#     } else {
#         yr <- NA
#     }
#     return( yr );
# })
# df.sub


# vert$yr1 <- NA
# vert$yr2 <- NA
# vert$total_funding_usd <- NA
# for(i in 1:nrow(vert)) {
#     founded_on <- created_at <- updated_at <- updated_at_timestamp <- NA
#     df.sub <- yrdf[which(yrdf$name==vert$name[i]), ]
#     if (nrow(df.sub) < 1) {
#         # PERL REGEX CASE INSENSITIVE
#         df.sub <- co[grep(vert$name[i], co$name, ignore.case = T, perl = T), ]
#         if nrow(df.sub) < 1) next
#     }
#     founded_on <- min( year( mdy( df.sub$founded_on ) ) );
#     created_at <- min( year( mdy( df.sub$created_at ) ) );
#     updated_at_timestamp <- min( as.numeric(df.sub$updated_at ));
#     updated_at <- year(as.POSIXct(updated_at_timestamp, origin = '1970-01-01', tz='EST') )
#     
#     vert$yr1[i] <- min(founded_on, created_at)
#     if (is.na(vert$yr1[i])) vert$yr2[i] <- updated_at
#     vert$total_funding_usd[i] <- max(df.sub$total_funding_usd)
#     if(i%%2000==0) cat(paste0('\ncompleted ',i,' (',round((i/nrow(vert))*100,1),'%)'))
# }
# 
# 
# 
# 
# vert$yr1 <- NA
# vert$yr2 <- NA
# vert$total_funding_usd <- NA
# for(i in 1:nrow(vert)) {
#     founded_on <- created_at <- updated_at <- updated_at_timestamp <- NA
#     df.sub <- yrdf[which(yrdf$name==vert$name[i]), ]
#     if (nrow(df.sub) < 1) {
#         # PERL REGEX CASE INSENSITIVE
#         df.sub <- co[which(co$name==vert$name[i]), ]
#         if nrow(df.sub) < 1) next
#     }
#     founded_on <- min( year( mdy( df.sub$founded_on ) ) );
#     created_at <- min( year( mdy( df.sub$created_at ) ) );
#     updated_at_timestamp <- min( as.numeric(df.sub$updated_at ));
#     updated_at <- year(as.POSIXct(updated_at_timestamp, origin = '1970-01-01', tz='EST') )
#     
#     vert$yr1[i] <- min(founded_on, created_at)
#     if (is.na(vert$yr1[i])) vert$yr2[i] <- updated_at
#     vert$total_funding_usd[i] <- max(df.sub$total_funding_usd)
#     if(i%%2000==0) cat(paste0('\ncompleted ',i,' (',round((i/nrow(vert))*100,1),'%)'))
# }
# 
# 
# vert_2 <- vert



## -_--------------------- GRAPH DATA FRAME ------------------------------
## 
g <- graph.data.frame(d = el, directed = F)

temp <- sapply(seq_len(vcount(g)), function(x){
    name <- str_to_lower(names(V(g))[x])
    name <- gsub('[\\]','',name)
    # pattern <- paste0('^',name,'$')
    if (name %in% c('google','microsoft','sony','paypal','amazon')) cat(paste0('\n',name))
    founded_on <- comp$founded_on[grep(name,comp$name,ignore.case = T,perl = T)]
    if (length(founded_on) < 1) {
        founded_on <- co$founded_at[grep(name,co$name,ignore.case = T,perl = T)]
    }
    return(founded_on)
});  

temp2 <- sapply(temp, function(x) return(unique(x)[1]))
V(g)$founded_yr <- year(lubridate::ymd_hms( format(as.POSIXct(as.numeric(temp2), origin = '1970-01-01', tz='EST')) ) )



# V(g)$yr <- sapply(seq_len(vcount(g)), function(x){
#     name <- names(V(g))[x]
#     sub <- vert[which(vert$name==name), ]
#     if (length(sub$founded_on)>0) {
#         if (!is.na(sub$founded_on)) {
#             val <- year( mdy(sub$founded_on) )
#             if(length(val)>1) val <- val[1]
#         } else if (!is.na(sub$created_at)) {
#             val <- year( mdy(sub$created_at) )
#             if(length(val)>1) val <- val[1]
#         } else {
#             val <- NA
#         }
#     } else {
#         val <- NA
#     }
#     return(val)
# })
# V(g)$yr <- as.numeric(V(g)$yr)




## ------------------- DYNAMIC NETWORK --------------------------------------
names(V(g))[grep('amazon',names(V(g)),ignore.case = T,perl = T)]
##

df1 <- df2 <- df3 <- df4 <- df5 <- data.frame()
gl <- list()
y <- 2015:2001
CO <- 'amazon'

mapping <- seq_len(vcount(g))

# mainIndex <- names(V(g))[grep(paste0('^',CO,'$'),names(V(g)),ignore.case = T,perl = T)]
# likeNames <- names(V(g))[grep(CO,names(V(g)),ignore.case = T,perl = T)]
# mapping[which(names(V(g)) %in% likeNames  )] <- mainIndex
#
compNames <- c('google','apple','netflix','etsy','facebook')
for (co2 in compNames) {
    mainIndex <- names(V(g))[grep(paste0('^',co2,'$'),names(V(g)),ignore.case = T,perl = T)]
    if(length(mainIndex) <1) mainIndex <-  names(V(g))[grep(co2,names(V(g)),ignore.case = T,perl = T)][1]
    likeNames <- names(V(g))[grep(co2,names(V(g)),ignore.case = T,perl = T)]
    mapping[which(names(V(g)) %in% likeNames  )] <- mainIndex    
}
mainIndex <- names(V(g))[grep(paste0('^','google','$'),names(V(g)),ignore.case = T,perl = T)]
likeNames <- names(V(g))[grep('youtube',names(V(g)),ignore.case = T,perl = T)]
mapping[which(names(V(g)) %in% likeNames  )] <- mainIndex    

gc <- delete.vertices(g, v=V(g)[which(names(V(g))=='4info')])
gc <- contract(g, mapping=mapping , vertex.attr.comb='first')

for (i in 1:length(y)) {
    cat(y[i],'\n')
    V(gc)$name[which(V(gc)$name == 'youtube')] <- 'google'
    gx <- induced.subgraph(gc, vids = V(gc)[which(V(gc)$founded_yr<= y[i] | is.na(V(gc)) )] )
    gx <- igraph::delete.edges(graph = gx, edges = E(gx)[which(year(as.POSIXct(E(gx)$created_at, origin='1970-01-01')) > y[i] )])
    
    v <- grep(paste0('^',CO,'$'),x = V(gx)$name,ignore.case = T,perl = T)
    gs <- igraph::make_ego_graph(gx, order=3, nodes = v)[[1]]
    
#     first <- names(V(igraph::make_ego_graph(gx, order=1, nodes = v)[[1]]))
#     second <- names(V(gs))[which( !(names(V(gs))%in% first ) )]
#     V(gs)$color <- ifelse(V(gs)$name%in%first,rgb(.8,.1,.1,.7),rgb(.1,.1,.1,.3))

    
    V(gs)$color <- ifelse(V(gs)$name%in%c(CO),rgb(.8,.1,.1,.7),rgb(.1,.1,.1,.3))
    V(gs)$color[which(names(V(gs))%in%compNames)] <- rgb(.1,.1,.8,.7)
    V(gs)$labelColor <- ifelse(V(gs)$name%in%c(compNames, CO),'black',rgb(.05,.05,.05,.8))
    V(gs)$scale <- ifelse(V(gs)$name%in% c(compNames, CO),1.3,1)

    a <- unname(igraph::shortest.paths(gx,V(gx)[which(str_to_lower(V(gx)$name)=='netflix')], 
                                       V(gx)[which(str_to_lower(V(gx)$name)==CO)]) )
    b <- unname(igraph::shortest.paths(gx,V(gx)[which(str_to_lower(V(gx)$name)=='google')], 
                                       V(gx)[which(str_to_lower(V(gx)$name)==CO)]) )
    c <- unname(igraph::shortest.paths(gx,V(gx)[which(str_to_lower(V(gx)$name)=='facebook')], 
                                       V(gx)[which(str_to_lower(V(gx)$name)==CO)]) )
    d <- unname(igraph::shortest.paths(gx,V(gx)[which(str_to_lower(V(gx)$name)=='etsy')], 
                                       V(gx)[which(str_to_lower(V(gx)$name)==CO)]) )
    e <- unname(igraph::shortest.paths(gx,V(gx)[which(str_to_lower(V(gx)$name)=='apple')], 
                                       V(gx)[which(str_to_lower(V(gx)$name)==CO)]) )
    if(nrow(a)>0) {
        temp <-  data.frame( yr=y[i], A=a)
        df1 <- rbind(df1, temp)   }
    if(nrow(b)>0) {
        temp <-  data.frame( yr=y[i], B=b)
        df2 <- rbind(df2, temp)  }
    if(nrow(c)>0) {
        temp <-  data.frame( yr=y[i], C=c)
        df3 <- rbind(df3, temp) }
    if(nrow(d)>0) {
        temp <-  data.frame( yr=y[i], D=d)
        df4 <- rbind(df4, temp) }
    if(nrow(e)>0) {
        temp <-  data.frame( yr=y[i], E=e)
        df5 <- rbind(df5, temp) }
    gl[[i]] <- gs
};  names(gl) <- as.character(y)

df1;df2;df3;df4; df5

dfyear <- data.frame(yr=y)
dfall <- merge(dfyear,df1,by='yr',all=T)
dfall <- merge(dfall,df2,by='yr',all=T)
dfall <- merge(dfall,df3,by='yr',all=T)
dfall <- merge(dfall,df4,by='yr',all=T)
dfall <- merge(dfall,df5,by='yr',all=T)
dfall <- dfall[order(dfall$yr, decreasing=T),]


png('compnet_sms_3graphs_071115_7.png',height=3,width=8, units='in',res=250)
    alpha = .3
    par(mar=c(.5,.5,.5,.5))
    par(mfrow=c(1,3))
    set.seed(1111)
    g.plot <- gl$`2007`
    plot.igraph(g.plot,
                layout=layout.fruchterman.reingold, 
                vertex.label.cex= V(g.plot)$scale *  alpha*degree(g.plot)^.2, 
                vertex.label.color = V(g.plot)$labelColor,
                vertex.label = ifelse(degree(g.plot)>1,V(g.plot)$name,''),
                vertex.size= 3*log2(degree(g.plot)),
                vertex.color=V(g.plot)$color)
    legend('topright',legend='2007',bty='n')
    #
    g.plot <- gl$`2011`
    plot.igraph(g.plot,
                layout=layout.fruchterman.reingold, 
                vertex.label.cex= V(g.plot)$scale *  alpha*degree(g.plot)^.2, 
                vertex.label.color = V(g.plot)$labelColor,
                vertex.label = ifelse(degree(g.plot)>1,V(g.plot)$name,''),
                vertex.size= 3*log2(degree(g.plot)),
                vertex.color=V(g.plot)$color)
    legend('topright',legend='2011',bty='n')
    #
    g.plot <- gl$`2015`
    plot.igraph(g.plot,
                layout=layout.fruchterman.reingold, 
                vertex.label.cex= V(g.plot)$scale *  alpha*degree(g.plot)^.2, 
                vertex.label.color = V(g.plot)$labelColor,
                vertex.label = ifelse(degree(g.plot)>1,V(g.plot)$name,''),
                vertex.size= 3*log2(degree(g.plot)),
                vertex.color=V(g.plot)$color)
    legend('topright',legend='2015',bty='n')
dev.off()







