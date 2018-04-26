setwd('C:\\Users\\T430\\Google Drive\\PhD\\Dissertation\\competition networks\\compnet2')
library(stringdist)
library(igraph)
library(stringr)
library(dplyr)
library(plyr)
library(reshape2)
library(intergraph)
library(RMySQL)
library(lubridate)

source(file.path(getwd(),'R','cb_data_prep.R'))



readCsv <- function(file.name, na.strings=c('')) {
  return(read.csv(file.name, 
                  stringsAsFactors = F, 
                  na.strings = na.strings))
}

headCsv <- function(file.name, na.strings=c('')) {
  return(read.csv(file.name, 
                  stringsAsFactors = F, 
                  nrows=5,
                  na.strings = na.strings))
}

getDbCon <- function(dbname="usabiz", user="root", password="", port=3306) {
  return(dbConnect(RMySQL::MySQL(), dbname = dbname, user=user, password=password, port=port))
}

##
# Return a dataframe for the query string
##
fetch <- function(query) {
  con <- getDbCon()
  rs <- dbSendQuery(con, query)
  data <- dbFetch(rs, n= -1)
  dbDisconnect(con)
  return(data)
}

sic <- read.csv('SIC_codes_description.csv', stringsAsFactors = F, na.strings = c('','NA'))
sic$description <- str_to_lower(sic$description)
siclen <- str_length(sic$description)

g.full <- read.graph('g_full.graphml', format = 'graphml')

nets <- readRDS("firm_nets_cem\\qualtrics_d3.rds")
g <- intergraph::asIgraph(nets$`2017`)
va <- vertex.attributes(g)


##=======================================================
# Fuzzy Matching Category to SIC Code Description
##-------------------------------------------------------

cat.split <- strsplit(va$category_group_list, split = "[|]")
# na.idx <- sapply(cat.split, function(x) length(x)==1 && x=='NA')
# cat.split.rmna <- cat.split[!na.idx]

# lsic <- lapply(seq_along(cat.split), FUN = function(i){
#   x <- cat.split.rmna[[i]]
#   if (i %% 100 == 0) 
#     cat(sprintf("i = %s\n",i))
#   if (length(x)==1 && x=='NA')
#     return(data.frame(sic_code=NA, d=NA, code_len=NA, x=paste(x,collapse = "|"),xi=NA,sic_description=NA, stringsAsFactors = F))
#   mdf <- data.frame()
#   for (xi in x) {
#     edit <- stringdist(xi, sic$description, method = 'osa') ## edit distance
#     maxlen <- sapply(siclen, function(j)max(j,str_length(xi))) ## max string len
#     df <- data.frame(sic_code=sic$code, d=edit/maxlen, code_len=sic$code_len, x=paste(x,collapse = "|"),xi=xi,sic_description=sic$description, stringsAsFactors = F)
#     df <- df[df$code_len==max(df$code_len), ]
#     mdf <- rbind(mdf, df[which.min(df$d), ])
#   }
#   return(mdf[which.min(mdf$d),])
# })

lsic <- list()
for(i in seq_along(cat.split)) {
  x <- cat.split.rmna[[i]]
  if (i %% 100 == 0)  {
    cat(sprintf("i = %s\n",i))    
    saveRDS(list(lsic=lsic), file = "sic_codes_list_parsed.rds")
  }
  if (length(x)==1 && x=='NA')
    return(data.frame(sic_code=NA, d=NA, code_len=NA, x=paste(x,collapse = "|"),xi=NA,sic_description=NA, stringsAsFactors = F))
  mdf <- data.frame()
  for (xi in x) {
    edit <- stringdist(xi, sic$description, method = 'osa') ## edit distance
    maxlen <- sapply(siclen, function(j)max(j,str_length(xi))) ## max string len
    df <- data.frame(sic_code=sic$code, d=edit/maxlen, code_len=sic$code_len, x=paste(x,collapse = "|"),xi=xi,sic_description=sic$description, stringsAsFactors = F)
    df <- df[df$code_len==max(df$code_len), ]
    mdf <- rbind(mdf, df[which.min(df$d), ])
  }
  lsic[[i]] <- mdf[which.min(mdf$d),]
}


dfsic <- plyr::ldply(lsic)

V(g)$category_group_matched <- dfsic$xi
V(g)$sic_d <- dfsic$d
V(g)$sic_code <- dfsic$sic_code
V(g)$sic_description <- dfsic$sic_description

saveRDS(list(lsic=lsic,g=g), file = "sic_codes_g_full_parsed.rds")

length(unique(dfsic$sic_code))





##======================================================
#  PUBLIC FIRMS IN FIRM NETS
##------------------------------------------------------
nets.files <- dir("firm_nets_cem",pattern = "_d3\\.rds")
pub.firms <- c()
pri.firms <- c()

for (i in 1:length(nets.files)) {
  cat(sprintf("finding public firms from %s\n",nets.files[i]))
  nets <- readRDS(file.path('firm_nets_cem',nets.files[i]))
  g <- intergraph::asIgraph(nets[[length(nets)]])
  vdf <- igraph::as_data_frame(g, what='vertices')
  pub.firms <- unique(c(pub.firms,vdf[vdf$ipo_status==1,'vertex.names']))
  pri.firms <- unique(c(pri.firms,vdf[vdf$ipo_status==0,'vertex.names']))
}

skipCols <- c('first_funding_on','last_funding_on')

co.pub <- co[co$company_name_unique %in% pub.firms, ]
co.pub <- co.pub[, !(names(co.pub) %in% skipCols) ]
write.csv(co.pub, file = "awareness_cem_comp_nets_PUBLIC_firms.csv",row.names = T)

co.pri <- co[co$company_name_unique %in% pri.firms, ]
co.pri <- co.pri[, !(names(co.pri) %in% skipCols) ]
write.csv(co.pri, file = "awareness_cem_comp_nets_PRIVATE_firms.csv",row.names = T)


  
  
  
##======================================================
#  Name matching to USA Biz DB
##------------------------------------------------------

lpri <- list()

name.query <- "SELECT * from company where company_name like '%s%s%s';"

for (i in 1:nrow(co.pri)) {
  firm0 <- co.pri$company_name_unique[i]
  firm <- stringr::str_replace_all(firm0, "-"," ")
  cat(sprintf("fetching %s (%s)\n",firm,firm0))
  df <- fetch(sprintf(name.query,'%',firm,'%s'))
  if (nrow(df)>0) {
    lpri[[firm]] <- df;
  } else  {
    lpri[[firm]] <- NA;
  }
  if (i %% 20 == 0) saveRDS(lpri, file = "list_private_firms_usa_biz_db.rds")
}
saveRDS(lpri, file = "list_private_firms_usa_biz_db_ALL.rds") 

tmp <- lpri
sum(sapply(tmp,function(x)return(class(x)!='logical'))) / length(tmp)


