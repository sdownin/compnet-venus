setwd("C:/Users/sdowning/Google Drive/PhD/Dissertation/competition networks/compnet")
.libPaths('C:/Users/sdowning/Documents/R/win-library/3.2')
library(ergm)
library(statnet)
library(tergm)
library(network)
library(igraph)
library(plyr)
library(dplyr)
library(stringr)
library(ndtv)
library(visNetwork)
library(scatterplot3d)
data_dir <- "C:/Users/sdowning/Google Drive/PhD/Dissertation/crunchbase"
img_dir  <- "C:/Users/sdowning/Google Drive/PhD/Dissertation/competition networks/envelopment"
#
source(file.path(getwd(),'R','comp_net_functions.R'))
source(file.path(getwd(),'R','netrisk_functions.R'))
source(file.path(getwd(),'R','cb_data_prep.R'))


saveVideo( ndtv::render.animation(ss) )

# ##------------------- PLOTS----------------------------------------------
# # Plot comp net size cumulative distribution
# png('comp_net_3by1_size_k_order_ego_net.png', width = 5, height=7, units = 'in', res = 200)
# par(mfrow=c(3,1),mar=c(4.5,4.5,1.5,1))
#   matplot(net.size, pch=16:17, type='o',xlab = expression(k^'th'~Order~Ego~Net),ylab=expression('Comp Net Size'))
#   legend(x='topleft',legend=names(net.size),pch=16:17,col=1:2,lty=1:2)
#   #
#   matplot(net.size.pct, pch=16:17, type='o',xlab = expression(k^'th'~Order~Ego~Net),
#           ylab=expression('Comp Net Proportion of Total'),  ylim=c(0,1))
#   legend(x='topleft',legend=names(net.size),pch=16:17,col=1:2,lty=1:2)
#   #
#   matplot(net.size.diff, pch=16:17, type='o',xlab = expression(k^'th'~Order~Ego~Net),
#           ylab=expression('Change in Comp Net Size by Order (k)'))
#   legend(x='topright',legend=names(net.size),pch=16:17,col=1:2,lty=1:2)
# dev.off()
# 
# # Plot comp net k=1, k=2
# png('comp_net_3by1_ego_net_plot_k1_k2.png', width = 14, height=8, units = 'in', res = 200)
# par(mfrow=c(1,2),mar=c(.1,.1,.1,.1))
#   plotCompNet(l[[1]],multi.prod)
#   legend(x='topright',legend='k = 1',bty='n')
#   #
#   plotCompNet(l[[2]],multi.prod, label.log.base = 20)
#   legend(x='topright',legend='k = 2',bty='n')
# dev.off()
# ## -------------------END PLOTS----------------------------------------


##  SELECT n Multi-Product Firms  (serial acquirers)
top.acquirers <- c("cisco","google","microsoft","ibm","yahoo","oracle","hewlett-packard","intel","aol","apple",
                   "emc","facebook","amazon","ebay","twitter","adobe-systems","nokia","dell","electronicarts",
                   "riverside","groupon","autodesk","salesforce","thoma-bravo","iac","quest-software","zayo-group",
                   "zynga","qualcomm","blackberry","ca","berkshire-hathaway-corp","carlyle-group","intuit",
                   "symantec","broadcom","kkr","medtronic","dropbox","vmware")
# multi.prod <- c("cisco","google","microsoft","ibm","yahoo","oracle","facebook","amazon","adobe-systems","nokia",
#                "dell","groupon","salesforce","blackberry")

## MAKE FULL COMP NET OF ALL RELATIONS IN DB 
g.full <- makeGraph(comp = comp, vertdf = co)
# fill in missing founded_year values from founded_at year
V(g.full)$founded_year <- ifelse( (!is.na(V(g.full)$founded_year)|V(g.full)$founded_year==''), V(g.full)$founded_year, as.numeric(substr(V(g.full)$founded_at,1,4)))


##################################################################################
#                        MAIN NETWORK COMPUTATION
##################################################################################
## starting values and params
yrpd <- 2
startYr <- 1999
endYr <- 2015
periods <- seq(startYr,endYr,yrpd)
company.name <- 'company_name_unique'
verbose <- TRUE
df.in <- co.acq

##--------------------- MAKE NETRISK SUBGRAPH ALGORITHM -------------------------
## ## 1.  Choose Multi-Product firms
multi.prod <- c("google","microsoft","ibm","oracle","amazon")
## ## 2. choose kth order for ego-net
k <- 2
## ## 3. get ego net from multi-product firms chosen above
mpen <- getMultiProdEgoNet(g.full, multi.prod, k=k)
## ## 4. get 2015 ego net igraph object
g <- igraph::induced.subgraph(g.full, vids=mpen$vids)

##--------------------- MAIN PERIOD SUBGRAPH LOOP -----------------------------  
gl <- list()
for(t in 2:length(periods)) {
  cat(sprintf('\nmaking period %s-%s:\n', periods[t-1],periods[t]))
  start <- periods[t-1]
  end <- periods[t]
  gl[[t-1]] <- setEdgeActiveState(g=g,start=start,end=end) 
}; names(gl) <- periods[-1]
## END MAIN LOOP 

# ## get Net Size Change (with plot)
# net.size <- getNetSizeChange(gl)

# ## plot comp net subgraphs
# plotCompNet(gl[[1]],multi.prod)

################################################################################
#                       // end main network computation
################################################################################


# ##------------------- PLOTTING PERIOD SUGRAPHS ---------------------
# png("comp_net_4_pd_subgraph_change_k2.png",width=10, height=12, units='in', res=200)
# par(mfrow=c(2,2),mar=c(.2,.2,.2,.2))
#   for (t in 1:length(gl)) {
#     gx <- gl[[t]]
#     yr <- names(gl)[t]
#     plotCompNet(gx, multi.prod, vertex.log.base = 20, label.log.base = 20)
#     legend(x='topright',legend=sprintf('Yr = %s',yr),bty='n')
#   }
# dev.off()
# ##-------------------------------------------------------------

  
  
  
# Make Network Dynamic
net.list <- lapply(X = gl, FUN = function(x)getNetFromIgraph(x))
nd <- networkDynamic(network.list = net.list, onsets = periods[1:(length(periods)-1)], termini = periods[-1])

# save.image(file = 'netrisk_sms_image.RData')

formation <- ~ edges
dissolution <- ~ edges
sm1 <- stergm(nd,
              formation=formation, 
              dissolution=dissolution, 
              estimate="CMLE", 
              times=4:6)
  

  
  



# Estimte ERGM models
f1 <- net ~ edges + nodecov("degree") + gwdegree(.5)
m1 <- ergm(formula = f1)
summary(m1)

f2 <- net ~ edges + nodecov("degree") + triangle()
m2 <- ergm(formula = f2)
summary(m2)

f3 <- net ~ edges + nodecov("degree") + dsp(1:3)
m3 <- ergm(formula = f3)
summary(m3)

  


## ------------------- NETWORKDYNAMIC PLOTS -----------------------------
data("short.stergm.sim")
ss <- short.stergm.sim
ndtv::timeline(ss)
ndtv::timePrism(ss, at=seq(1,10,by=3))
ndtv::filmstrip(ss, frames=9, mfrow=c(3,3))
ndtv::render.d3movie(ss, filename = )
  
  