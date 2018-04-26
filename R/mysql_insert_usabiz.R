##
#
# Bulk Import USA Biz DB
#
##

setwd('C:\\Users\\T430\\Google Drive\\PhD\\Dissertation\\usa_biz_db')

# library(RODBC)
library(RMySQL)
library(reshape2)
library(plyr)
library(lubridate)
library(stringr)

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
  con <- dbConnect(RMySQL::MySQL(), dbname = "usabiz", user="root", password="", port=3306)
  rs <- dbSendQuery(con, query)
  data <- dbFetch(rs, n= -1)
  dbDisconnect(con)
  return(data)
}



##
# INSERT RECORDS
#
# Add Index:
# CREATE INDEX idx_company_company_name ON company (company_name(50)) USING BTREE;
##

files <- c(
  'Agriculture, Forestry, Fishing - 266,845.csv',
  'Construction Part 1 - 483,601.csv',
  'Construction Part 2 - 171,120.csv',
  'Finance, Insurance, Real Estate Part 1 - 889,709.csv',
  'Finance, Insurance, Real Estate Part 2 - 236,236.csv',
  'Manufacturing - 440,825.csv',
  'Mining - 40,319.csv',
  'Public Administration - 328,887.csv',
  'Retail Trade Part 1 - 977,763.csv',
  'Retail Trade Part 2 - 981,187.csv',
  'Retail Trade Part 3 - 538,935.csv',
  'Services Part 1 - 1,027,634.csv',
  'Services Part 2 - 1,036,148.csv',
  'Services Part 3 - 1,028,169.csv',
  'Services Part 4 - 878,760.csv',
  'Transportation & Public Utilities - 458,406.csv',
  'Wholesale Trade - 610,816.csv'
)
added <- c(
  'Agriculture, Forestry, Fishing - 266,845.csv',
  'Construction Part 1 - 483,601.csv',
  'Construction Part 2 - 171,120.csv',
  'Finance, Insurance, Real Estate Part 1 - 889,709.csv',
  'Finance, Insurance, Real Estate Part 2 - 236,236.csv',
  'Manufacturing - 440,825.csv',
  'Mining - 40,319.csv',
  'Public Administration - 328,887.csv',
  'Retail Trade Part 1 - 977,763.csv',
  'Retail Trade Part 2 - 981,187.csv',
  'Retail Trade Part 3 - 538,935.csv',
  'Services Part 1 - 1,027,634.csv',
  'Services Part 2 - 1,036,148.csv',
  'Services Part 3 - 1,028,169.csv',
  'Services Part 4 - 878,760.csv',
  'Transportation & Public Utilities - 458,406.csv',
  'Wholesale Trade - 610,816.csv'
)

con <- getDbCon()
for (j in 1:length(files)) 
{
  cat(sprintf("\nstarting df %s\n",files[j]))
  if (files[j] %in% added)
  {
    cat("skipping already added\n")
    next
  }
  df <- readCsv(files[j])
  for(i in 1:length(names(df))) 
  {
    names(df)[i] <- str_replace_all(stringr::str_to_lower(names(df)[i]),"[.]","_")
  }
  dbWriteTable(con, name = 'company', value = df, append=T)
  added <- c(added, files[j])
}
dbDisconnect(con)









