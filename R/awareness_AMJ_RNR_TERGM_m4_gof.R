library(igraph)
library(intergraph)
library(btergm)
library(xergm)
library(parallel)
library(texreg)
library(stringr)
library(reshape2)
library(plyr)
library(dplyr)
library(lattice)
library(ggplot2)

## DIRECTORIES

work_dir <- "C:/Users/T430/Google Drive/PhD/Dissertation/competition networks/compnet2"
data_dir <- "C:/Users/T430/Google Drive/PhD/Dissertation/competition networks/compnet2/firm_nets_rnr"
results_dir <- "C:/Users/T430/Google Drive/PhD/Dissertation/competition networks/compnet2/amj_rnr_results"
img_dir  <- "C:/Users/T430/Google Drive/PhD/Dissertation/competition networks/envelopment/img"


## set woring dir
setwd(work_dir)

## load awareness functions
source(file.path(getwd(),'R','amj_awareness_functions.R')) 


## FUNCTIONS FOR NAs REMOVED
.med <- function(x){return(median(x, na.rm=TRUE))}
.min <- function(x){return(min(x, na.rm=TRUE))}
.max <- function(x){return(max(x, na.rm=TRUE))}
.avg <- function(x){return(mean(x, na.rm=TRUE))}
.std <- function(x){return(sd(x, na.rm=TRUE))}
.qtl <- function(x, probs){return(quantile(x, probs=probs, na.rm=TRUE))}
.iqr <- function(x){return(IQR(x, na.rm=TRUE))}

## -----------Model Results Settings-----
name_i <- 'qualtrics'
firm_i <- name_i
d <- 3
R <- 2000
nPeriods <- 11
m_x <- 'm4'
nsim <- 100
##----------------------------------------

gofs <- c('degree','esp','dsp','geodesic','rocpr')


##============================
##
## PLOT Limited Values -- relevant for focal firm competition network
##
##----------------------------


##======================
## DEGREE
##----------------------
gofi <- 'degree'
gofi.name <- 'Degree'


gof.file <- sprintf('%s/gof_%s_pd%s_R%s_%s_nsim%s_%s.rds', 
                    results_dir, name_i, nPeriods, R, m_x, nsim, gofi)

deg.gf <- readRDS(gof.file)
print(deg.gf)

## table
gof.tbl.file <- sprintf('%s/gof_table_%s_pd%s_R%s_%s_nsim%s_%s.csv', 
                        results_dir, name_i, nPeriods, R, m_x, nsim, gofi)
write.csv(deg.gf[[gofi.name]]$stats, file=gof.tbl.file)

## plot
gof.tbl.file <- sprintf('%s/gof_table_limited_%s_pd%s_R%s_%s_nsim%s_%s.png', 
                        results_dir, name_i, nPeriods, R, m_x, nsim, gofi)
png(gof.tbl.file, width = 9, height = 4.5, units = 'in', res = 250)
plot(deg.gf[[gofi.name]])
dev.off()


##======================
## DSP
##----------------------
gofi <- 'dsp'
gofi.name <- 'Dyad-wise shared partners'

gof.file <- sprintf('%s/gof_%s_pd%s_R%s_%s_nsim%s_%s.rds', 
                    results_dir, name_i, nPeriods, R, m_x, nsim, gofi)

deg.gf <- readRDS(gof.file)
print(deg.gf)

## table
gof.tbl.file <- sprintf('%s/gof_table_%s_pd%s_R%s_%s_nsim%s_%s.csv', 
                        results_dir, name_i, nPeriods, R, m_x, nsim, gofi)
write.csv(deg.gf[[gofi.name]]$stats, file=gof.tbl.file)

## plot
gof.tbl.file <- sprintf('%s/gof_table_limited_%s_pd%s_R%s_%s_nsim%s_%s.png', 
                        results_dir, name_i, nPeriods, R, m_x, nsim, gofi)
png(gof.tbl.file, width = 6, height = 4.5, units = 'in', res = 250)
plot(deg.gf[[gofi.name]])
dev.off()



##======================
## ESP
##----------------------
gofi <- 'geodesic'
gofi.name <- 'Geodesic distances'

gof.file <- sprintf('%s/gof_%s_pd%s_R%s_%s_nsim%s_%s.rds', 
                    results_dir, name_i, nPeriods, R, m_x, nsim, gofi)

deg.gf <- readRDS(gof.file)
print(deg.gf)

## keep which values
idx.keep <- c(1:11, nrow(deg.gf[[gofi.name]]$stats))

## subset
deg.gf[[gofi.name]]$stats <- deg.gf[[gofi.name]]$stats[idx.keep, ]
deg.gf[[gofi.name]]$raw <- deg.gf[[gofi.name]]$raw[idx.keep, ]


## plot
gof.tbl.file <- sprintf('%s/gof_table_limited_%s_pd%s_R%s_%s_nsim%s_%s.png', 
                        results_dir, name_i, nPeriods, R, m_x, nsim, gofi)
png(gof.tbl.file, width = 9, height = 4.5, units = 'in', res = 250)
plot(deg.gf[[gofi.name]])
dev.off()































##============================
##
## ORIGINALS -- ALL VALUES
##
##------------------------------

##======================
## DEGREE
##----------------------
gofi <- 'degree'
gofi.name <- 'Degree'

gof.file <- sprintf('%s/gof_%s_pd%s_R%s_%s_nsim%s_%s.rds', 
                    results_dir, name_i, nPeriods, R, m_x, nsim, gofi)

deg.gf <- readRDS(gof.file)
print(deg.gf)

## table
gof.tbl.file <- sprintf('%s/gof_table_%s_pd%s_R%s_%s_nsim%s_%s.csv', 
                        results_dir, name_i, nPeriods, R, m_x, nsim, gofi)
write.csv(deg.gf[[gofi.name]]$stats, file=gof.tbl.file)

## plot
gof.tbl.file <- sprintf('%s/gof_table_%s_pd%s_R%s_%s_nsim%s_%s.png', 
                        results_dir, name_i, nPeriods, R, m_x, nsim, gofi)
png(gof.tbl.file, width = 9, height = 4.5, units = 'in', res = 250)
plot(deg.gf[[gofi.name]])
dev.off()


##======================
## DSP
##----------------------
gofi <- 'dsp'
gofi.name <- 'Dyad-wise shared partners'

gof.file <- sprintf('%s/gof_%s_pd%s_R%s_%s_nsim%s_%s.rds', 
                    results_dir, name_i, nPeriods, R, m_x, nsim, gofi)

deg.gf <- readRDS(gof.file)
print(deg.gf)

## table
gof.tbl.file <- sprintf('%s/gof_table_%s_pd%s_R%s_%s_nsim%s_%s.csv', 
                        results_dir, name_i, nPeriods, R, m_x, nsim, gofi)
write.csv(deg.gf[[gofi.name]]$stats, file=gof.tbl.file)

## plot
gof.tbl.file <- sprintf('%s/gof_table_%s_pd%s_R%s_%s_nsim%s_%s.png', 
                        results_dir, name_i, nPeriods, R, m_x, nsim, gofi)
png(gof.tbl.file, width = 6, height = 4.5, units = 'in', res = 250)
plot(deg.gf[[gofi.name]])
dev.off()


##======================
## ESP
##----------------------
gofi <- 'esp'
gofi.name <- 'Edge-wise shared partners'

gof.file <- sprintf('%s/gof_%s_pd%s_R%s_%s_nsim%s_%s.rds', 
                    results_dir, name_i, nPeriods, R, m_x, nsim, gofi)

deg.gf <- readRDS(gof.file)
print(deg.gf)

## table
gof.tbl.file <- sprintf('%s/gof_table_%s_pd%s_R%s_%s_nsim%s_%s.csv', 
                        results_dir, name_i, nPeriods, R, m_x, nsim, gofi)
write.csv(deg.gf[[gofi.name]]$stats, file=gof.tbl.file)

## plot
gof.tbl.file <- sprintf('%s/gof_table_%s_pd%s_R%s_%s_nsim%s_%s.png', 
                        results_dir, name_i, nPeriods, R, m_x, nsim, gofi)
png(gof.tbl.file, width = 3, height = 4.5, units = 'in', res = 250)
plot(deg.gf[[gofi.name]])
dev.off()


##======================
## ESP
##----------------------
gofi <- 'geodesic'
gofi.name <- 'Geodesic distances'

gof.file <- sprintf('%s/gof_%s_pd%s_R%s_%s_nsim%s_%s.rds', 
                    results_dir, name_i, nPeriods, R, m_x, nsim, gofi)

deg.gf <- readRDS(gof.file)
print(deg.gf)

## table
gof.tbl.file <- sprintf('%s/gof_table_%s_pd%s_R%s_%s_nsim%s_%s.csv', 
                        results_dir, name_i, nPeriods, R, m_x, nsim, gofi)
write.csv(deg.gf[[gofi.name]]$stats, file=gof.tbl.file)

## plot
gof.tbl.file <- sprintf('%s/gof_table_%s_pd%s_R%s_%s_nsim%s_%s.png', 
                        results_dir, name_i, nPeriods, R, m_x, nsim, gofi)
png(gof.tbl.file, width = 9, height = 4.5, units = 'in', res = 250)
plot(deg.gf[[gofi.name]])
dev.off()











