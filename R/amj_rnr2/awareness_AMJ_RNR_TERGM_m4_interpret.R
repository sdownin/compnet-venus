library(igraph)
library(intergraph)
library(btergm)
library(parallel)


## DIRECTORIES
data_dir <- '/home/sdowning/data/firm_nets_rnr2'
results_dir <- '/home/sdowning/compnet/results/amj_rnr2'


## -----------Model Results Settings-----
name_i <- 'qualtrics'
firm_i <- name_i
d <- 3
R <- 2000
nPeriod <- 11
m_x <- 'm4'
##----------------------------------------


## FITTED MODEL RESULTS
fit_file <- file.path(results_dir, sprintf('fit_%s_pd%s_R%s_%s.rds',firm_i,nPeriod,R,m_x))
fits <- readRDS(fit_file)
fit <- fits[[firm_i]][[m_x]]

## NETWORKS LIST
data_file <- file.path(data_dir,sprintf('%s_d%s.rds',firm_i,d))
nets <- readRDS(data_file)

## covariates list
mmc <- lapply(nets, function(net) as.matrix(net %n% 'mmc'))
cpc <- lapply(nets, function(net) as.matrix(net %n% 'coop'))
cpp <- lapply(nets, function(net) as.matrix(net %n% 'coop_past'))
cpa <- lapply(nets, function(net) as.matrix(net %n% 'coop') + as.matrix(net %n% 'coop_past') )
cossim <- lapply(nets, function(net) as.matrix(net %n% 'cat_cos_sim'))
centjoin <- lapply(nets, function(net) as.matrix(net %n% 'joint_cent_pow_n0_4'))
centratio <- lapply(nets, function(net) as.matrix(net %n% 'cent_ratio_pow_n0_4'))
shcomp <- lapply(nets, function(net) as.matrix(net %n% 'shared_competitor'))
shinv <- lapply(nets, function(net) as.matrix(net %n% 'shared_investor_nd'))

## create igraph lists
gs <- lapply(nets, asIgraph)

##===============================================
## interpreation data frame i-->j (for all t)
##-----------------------------------------------
g <- asIgraph(nets[[length(nets)]])
vcnt <- vcount(g)
time.steps <- fit@time.steps
firm.names <-  V(g)$vertex.names
v.focal <- as.integer( V(g)[V(g)$vertex.names==name_i] )

## competitor interpretation loop
interp.df.file <- file.path(results_dir, sprintf('interpret_%s_pd%s_R%s_%s.csv',firm_i,nPeriod,R,m_x))

## RUN MAIN DYAD PREDICTION LOOP
idf <- data.frame()
for (j in 1:vcount(g)) 
{
  cat(sprintf('%s:  %s\n',j,V(g)$vertex.names[j]))
  j.name <- V(g)$vertex.names[j]
  if (j == v.focal) {
    probs <- rep(0, time.steps)
  } else {
    probs <- btergm::interpret(fit, type='tie', i=v.focal, j=j)
  }
  for (t in 1:length(probs)) {
    d <- igraph::distances(gs[[t+1]], v = v.focal, to = j)[1,1]
    tmp <- data.frame(i=v.focal, j=j, i.name=name_i, j.name=j.name, t=t, d=d, p=probs[t])
    idf <- rbind(idf, tmp)
  }
  if (j %% 100 == 0)  write.csv(x = idf, file = interp.df.file)
}

## OUTPUT CSV
write.csv(x = idf, file = interp.df.file)


cat('\nfinished successfully.\n')
