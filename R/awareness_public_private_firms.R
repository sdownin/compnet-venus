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


  
