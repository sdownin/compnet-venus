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

sic2 <- read.csv('SIC_codes_EDGAR.csv', stringsAsFactors = F, na.strings = c('','NA')) ## SIC_codes_description.csv
sic2$description <- str_to_lower(sic2$title)

sic <- read.csv('SIC_codes_description.csv', stringsAsFactors = F, na.strings = c('','NA')) ## SIC_codes_description.csv
sic$code5 <- str_pad(sic$code,5,'right','0')
sic$code4 <- str_sub(sic$code5,2)
sic$description <- str_to_lower(sic$description)
siclen <- str_length(sic$description)

g.full <- read.graph('g_full.graphml', format = 'graphml')

nets <- readRDS("firm_nets_cem\\qualtrics_d3.rds")
g <- intergraph::asIgraph(nets$`2017`)
va <- vertex.attributes(g)

vdf <- data.frame(name=va$vertex.names,city=va$city,state=va$state_code,region=va$region)
View(vdf)

write.csv(vdf, file = "__firm_regions.csv",row.names = F)

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
  x <- cat.split[[i]]
  if (i %% 100 == 0)  {
    cat(sprintf("i = %s\n",i))    
    saveRDS(list(lsic=lsic), file = "sic_codes_list_parsed_lcs.rds")
  }
  if (length(x)==1 && x=='NA')
    lsic[[i]] <- data.frame(sic_code=NA, d=NA, code_len=NA, x=paste(x,collapse = "|"),xi=NA,sic_description=NA, stringsAsFactors = F)
  mdf <- data.frame()
  for (xi in x) {
    edit <- stringdist(xi, sic$description, method = 'lcs') ## edit distance
    maxlen <- sapply(siclen, function(j)max(j,str_length(xi))) ## max string len
    df <- data.frame(sic_code=sic$code5, d=edit/maxlen, code_len=sic$code_len, x=paste(x,collapse = "|"),xi=xi,sic_description=sic$description, stringsAsFactors = F)
    df <- df[df$code_len==max(df$code_len), ]
    mdf <- rbind(mdf, df[which.min(df$d), ])
  }
  lsic[[i]] <- mdf[which.min(mdf$d),]
}
saveRDS(list(lsic=lsic), file = "sic_codes_list_parsed_lcs.rds")


dfsic <- plyr::ldply(lsic)
dfsic$sic4  <- str_sub(dfsic$sic_code, 2)
dfsic$company_name_unique <- V(g)$vertex.names
# dfsic$company_name <- V(g)$company_name
# dfsic$name2 <- sapply(1:vcount(g), function(i){
#   return(ifelse( is.na(V(g)$company_name[i]), V(g)$vertex.names[i], V(g)$company_name[i]  ))
# })

V(g)$category_group_matched <- dfsic$xi
V(g)$sic_d <- dfsic$d
V(g)$sic_code <- dfsic$sic_code
V(g)$sic_description <- dfsic$sic_description

saveRDS(list(lsic=lsic,g=g,dfsic=dfsic), file = "sic_codes_g_full_parsed_lcs.rds")

write.csv(dfsic, file = "awareness_583_SIC_codes_lcs.csv", row.names = F)


length(unique(dfsic$sic_code))


## READ IN AFTER SAVING
l.tmp <- readRDS('sic_codes_g_full_parsed_lcs.rds')
lsic <- l.tmp$lsic
g <- l.tmp$g
dfsic <- l.tmp$dfsic

# va <- gdf(g)
# 
# View(va[,c('category_group_matched','sic_d','sic_code','sic_description','vertex.names','company_name_unique')])


unique(dfsic$sic_code)
unique(dfsic$sic4)
View(dfsic)

  
  
# ##======================================================
# #  Name matching to USA Biz DB
# ##------------------------------------------------------
# 
# lpri <- list()
# 
# name.query <- "SELECT * from company where company_name like '%s%s%s';"
# 
# for (i in 1:nrow(co.pri)) {
#   firm0 <- co.pri$company_name_unique[i]
#   firm <- stringr::str_replace_all(firm0, "-"," ")
#   cat(sprintf("fetching %s (%s)\n",firm,firm0))
#   df <- fetch(sprintf(name.query,'%',firm,'%s'))
#   if (nrow(df)>0) {
#     lpri[[firm]] <- df;
#   } else  {
#     lpri[[firm]] <- NA;
#   }
#   if (i %% 20 == 0) saveRDS(lpri, file = "list_private_firms_usa_biz_db.rds")
# }
# saveRDS(lpri, file = "list_private_firms_usa_biz_db_ALL.rds") 
# 
# tmp <- lpri
# sum(sapply(tmp,function(x)return(class(x)!='logical'))) / length(tmp)




gsic <- read.csv("SIC_NAICS_cem_codes.csv")
View(gsic[!is.na(gsic$sic_code),c('Company','sic_code','sic_description')])

gsic <- merge(gsic, co[,c('company_name_unique','category_group_list')])


# ##
# 3572
# 5045
# 5734
# 7311
# 7331
# 7370
# 7371
# 7372
# 7373
## 7374
## 7375
## 7376
## 7377
# 7378
# 7379
# 7389
# 8713
# 8732
# 8748
# ##








