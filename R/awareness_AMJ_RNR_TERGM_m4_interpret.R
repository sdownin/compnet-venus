library(igraph)
library(intergraph)
library(btergm)
library(xergm)
library(parallel)
library(texreg)
library(stringr)

## DIRECTORIES

work_dir <- "C:/Users/T430/Google Drive/PhD/Dissertation/competition networks/compnet2"
data_dir <- "C:/Users/T430/Google Drive/PhD/Dissertation/competition networks/compnet2/firm_nets_rnr"
results_dir <- "C:/Users/T430/Google Drive/PhD/Dissertation/competition networks/compnet2/amj_rnr_results"
img_dir  <- "C:/Users/T430/Google Drive/PhD/Dissertation/competition networks/envelopment/img"


## set woring dir
setwd(work_dir)

## -----------Model Results Settings-----
name_i <- 'qualtrics'
d <- 3
R <- 500
nPeriods <- 11
m_x <- 'm4'
##----------------------------------------

## results
fits.file <- sprintf('%s/fit_winlocal_%s_pd%s_d%s_R%s_%s.rds', 
                     results_dir, name_i, nPeriods, d, R, m_x)
fits <- readRDS(fits.file)

##  network data
nets.file <- file.path(data_dir,sprintf('%s_d%s.rds',firm_i,d))
nets <- readRDS(nets.file)


effects <- names(fits@effects)
par(mfrow=c(3,3))
for (effect in effects) {
  qqnorm(fits@boot$t[,effect], main=effect)
}
par(mfrow=c(3,3))
for (effect in effects) {
  x <- fits@boot$t[,effect]
  dr <- diff(range(x))
  xl1 <- min(0-(.1*dr), min(x))
  xl2 <- max(.1*dr, max(x))
  ci <- quantile(x, c(.025,.975))
  hist(x, main=effect, breaks=13, col=rgb(.2,.2,.2,.2), xlim=c(xl1,xl2))
  abline(v=ci, col='red',lwd=2)
  abline(v=0, col='black',lwd=1, lty=2)
  segments(x0=ci[1],x1=ci[2],y0=0,y1=0,col='red',lwd=2)
}

# library()
# par(mfrow=c(1,1))
# ci95 <- c(.025,.5,.975)
# coef.mat <- t(apply(fits@boot$t,2,function(x)quantile(x,ci95)))
# matplot(x=df.coef, y=1:nrow(coef.mat))


##===============================================
## interpreation data frame i-->j (for all t)
##-----------------------------------------------
g <- asIgraph(nets[[length(nets)]])
vcnt <- vcount(g)
time.steps <- fits@time.steps
firm.names <-  V(g)$vertex.names
v.focal <- as.integer( V(g)[V(g)$vertex.names==name_i] )
idf <- data.frame(i=v.focal,j=1:vcnt,i.name=name_i,j.name = firm.names)
## make data frame of predicted probabilities
for (t in 1:time.steps)
  idf[,paste0('t',t)]<- NA

## time columns
tcolnames <- names(idf)[grep(names(idf), pattern = 't\\d{1,3}', perl = T)]

## competitor interpretation loop
interp.df.file <- sprintf('%s/interpret_winlocal_%s_pd%s_d%s_R%s_%s.csv', 
                     results_dir, name_i, nPeriods, d, R, m_x)
for (j in 1:vcount(g)) {
  cat(sprintf('%s: competitor %s\n',j,V(g)$vertex.names[j]))
  if (j == v.focal) {
    idf[j,tcolnames] <- rep(0, time.steps)
  } else {
    idf[j,tcolnames] <- btergm::interpret(fits, type='tie', i=v.focal, j=j)
  }
  if (j %% 20 == 0)  write.csv(x = idf, file = interp.df.file)
}
write.csv(x = idf, file = interp.df.file)



## GOF
gf <- gof(fits, target=nets.all[length(nets.all)],
          nsim=50, 
          statistics=c(dsp, esp, deg, ideg, 
                       geodesic, rocpr, walktrap.modularity))
par(mfrow=c(1,5))
plot(gf)


## Compare to GLM
X <- cbind(fits@response,fits@effects)
fixnames <- unname(sapply(names(fits@effects),function(x)str_replace_all(x,"[\\[i\\]]","")))
names(X) <- c('y',fixnames)

formula1 <- as.formula(sprintf('y ~ %s',paste(fixnames,collapse=" + ")))
glfit <- glm(formula1 ,
             data=X,
             family=binomial(link = "logit"))

screenreg(list(glm=glfit,tergm=fits), ci.force = T, digits=3)


fitm <- mtergm(get(m_x))
plot(mcmc.diagnostics(fitm@ergm))

screenreg(list(glm=glfit,btergm=fits,mtergm=fitm), digits=3)



m0 <-   nets ~ edges + gwesp(0, fixed = T) +
  nodematch("ipo_status", diff = F) +
  nodematch("state_code", diff = F) +
  nodecov("age") + absdiff("age") +
  # edgecov(mmc) + 
  # edgecov(cpp) + #edgecov(cpc) +
  nodecov("njobs_multilevel") + #absdiff("njobs_multilevel") +
  nodecov("cent_pow_n0_2") + absdiff("cent_pow_n0_2") +
  cycle(3)  #+ #cycle(4) + cycle(5) + 




##
# DEFINE MODEL and MODEL NAME TO COMPUTE
##
m_x <- 'm0'
##
# SET RESAMPLES
##
R <- 30


## RUN TERGM
fits0 <- btergm(get(m_x), R=R, parallel = "multicore", ncpus = detectCores())

saveRDS(fits0, 'test_nets_qualtrics_d2.rds')

screenreg(fits0)

gf0 <- gof(fits0, nsim=30, statistics=c(dsp, esp, deg, ideg, geodesic, rocpr, 
                                      walktrap.modularity))
plot(gf0)
