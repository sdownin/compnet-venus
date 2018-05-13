
setwd('C:\\Users\\T430\\Google Drive\\PhD\\Dissertation\\competition networks\\compnet2')
library(stringdist)
library(igraph)
library(stringr)
library(plyr)
library(dplyr)
library(reshape2)
library(intergraph)
library(RMySQL)
library(lubridate)

source(file.path(getwd(),'R','netrisk_functions.R'))
source(file.path(getwd(),'R','comp_net_functions.R'))
source(file.path(getwd(),'R','cb_data_prep.R'))



g.full <- read.graph('g_full.graphml', format = 'graphml')

nets <- readRDS("firm_nets_cem\\qualtrics_d3.rds")

gs <- lapply(nets, function(net)asIgraph(net))
g1 <- gs[[length(gs)]]

df.v <- gdf(g1)
df.v <- df.v[order(df.v$cent_deg, decreasing = T), ]
df.v <- merge(df.v, co[,c('company_name','company_name_unique','employee_count','funding_total_usd','funding_rounds')], 
              by.x='vertex.names', by.y='company_name_unique', all.x=T, all.y=F)
df.v2 <- df.v[, c('vertex.names','company_name','cent_deg','ipo_status','employee_count','funding_total_usd','funding_rounds') ]
df.v2$name2 <- sapply(1:nrow(df.v2), function(i){
  return(ifelse( is.na(df.v2$company_name[i]), df.v2$vertex.names[i], df.v2$company_name[i]  ))
})
View(df.v2)
write.csv(df.v2, file = 'awareness_qualtrics_net_firms_details.csv', row.names = F)

##--------------------------------------------------------------------------

namedf <- expand.grid(V(g1)$vertex.names, V(g1)$vertex.names, stringsAsFactors = F)
names(namedf) <- c('i','j')

df <- expand.grid(as.integer(V(g1)), as.integer(V(g1)))
df$ij <- apply(df,1,function(x)paste(x,collapse = "-"))

df <- cbind(df, namedf)

df$d12 <- t(t(c(igraph::distances(gs$`2012`))))
df$d13 <- t(t(c(igraph::distances(gs$`2013`))))
df$d14 <- t(t(c(igraph::distances(gs$`2014`))))
df$d15 <- t(t(c(igraph::distances(gs$`2015`))))
df$d16 <- t(t(c(igraph::distances(gs$`2016`))))
df$d17 <- t(t(c(igraph::distances(gs$`2017`))))

lo <- 2
hi <- 5
rows <- which(
  df$d17 <= 2 
  & ( 
    (df$d16 >= hi & df$d16 < Inf) 
    | (df$d15 >= hi & df$d15 < Inf) 
    | (df$d14 >= hi & df$d14 < Inf)
    | (df$d13 >= hi & df$d13 < Inf)
  )
)
df2 <- df[rows, ]

##ONLY CLOSED DISTANCE FIRMS THAT WERE ACQUIRED
rows2 <- which(df2$i %in% co_acq$acquiree_name_unique 
               | df2$j %in% co_acq$acquiree_name_unique)
df3 <- df2[rows2, ]

## ONLY ACQUISITIONS THAT ARE ALSO IN CLOSED DISTANCE FIRM EDGES
idx.acq <- which(
  acq$acquired_on >= '2012-01-01' 
  & acq$acquired_on <= '2016-10-30'
  & (
    acq$acquiree_name_unique %in% df3$i
    | acq$acquiree_name_unique %in% df3$j
    | acq$acquirer_name_unique %in% df3$i
    | acq$acquirer_name_unique %in% df3$j
    )
)
acq <- co_acq[idx.acq, c('acquirer_name_unique','acquiree_name_unique','acquired_on')]



df4 <- merge(df3, acq, by.x='i',by.y='acquiree_name_unique',all.x=T,all.y=F)
df4 <- merge(df4, acq, by.x='j',by.y='acquiree_name_unique',all.x=T,all.y=F)
df4 <- merge(df4, acq, by.x='i',by.y='acquirer_name_unique',all.x=T,all.y=F)
df4 <- merge(df4, acq, by.x='j',by.y='acquirer_name_unique',all.x=T,all.y=F)


dim(df3)
dim(acq)

View(df3)
View(acq)

write.csv(acq, file = 'awareness_distance_change_acquisition_5-2.csv',row.names = F)
write.csv(df3, file = 'awareness_distance_change_acquisition_5-2_d3.csv',row.names = F)


ldf <- lapply(gs, igraph::distances)

ldiff <- list()

for (i in 2:length(ldf)) {
  tmp1 <- ldf[[i]]
  tmp0 <- ldf[[i-1]]
  ldiff[[names(ldf)[i]]] <- tmp1 - tmp0
}







