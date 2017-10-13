Competition Network Analysis
=======

Computations for Downing, Kang, & Markman, 2017.

Acknowledgement:
This research was supported in part by MOST-105-2420-H-009-012-DR and  MOST-106-2922-I-009-127.

Getting Started
======

in directory `R` download the R script `netrisk_awareness_AOM.R`. 

Paste the binary data file `netrisk_dynamic_firm_nets_1yr_v3_misc.RData` is in the same directory that you save the above script. You can run the script in its entirety simply to get the results, but an explanation of each part is provided below in case you want to change the analysis.

Set working dir:
```R
setwd("<full path>")
```

load libraries:
```R
library(parallel)
library(btergm)
library(texreg)
library(plyr)
```

load binary data file in R:
```R
load('netrisk_dynamic_firm_nets_1yr_v3_misc.RData');
```

set params and save network covariate lists:
```R
R <- 1000               ## number of bootstrap resamples
nPeriods <- 6           ## 6 out of 7, skips first period for DV lag
net_group <- 'misc'     ## network group name
firm_i <- 'clarabridge' ## focal firm name

# subset of networks used to fit model
nets.sub <- firm.nets[[net_group]][[firm_i]]
nets.sub <- nets.sub[(length(nets.sub)-nPeriods+1):(length(nets.sub))]

## Edge covariates list of matrices [[NxN],[NxN],...]
mmc <- lapply(nets.sub, function(net) as.matrix(net %n% 'mmc'))
sim <- lapply(nets.sub, function(net) as.matrix(net %n% 'similarity'))
ldv <- lapply(nets.sub, function(net) as.matrix(net %n% 'DV_lag'))
```

cache models as follows:
```R
m0 <- nets.sub ~ edges + gwesp(0, fixed=T) + 
  nodematch('ipo_status', diff=TRUE) +
  nodematch('state_code', diff=F) +
  nodematch('npm',diff=F) + 
  nodecov('age') +   
  edgecov(mmc)  + edgecov(ldv) +  edgecov(sim)
```

fit TERGMs via bootstrapped MPLE as follows:
```R
f0 <- btergm(m0, R=R, parallel = "multicore", ncpus = detectCores())
```

cache model fits as a list:
```R
fits <- list(f0=f0,f1=f1,f2=f2,f3=f3,f4=f4)
```

echo models comparison table to screen or save to formatted HTML file with:
```R
screenreg(fits)
htmlreg(fits, file="fits.html")
```

save binary data file of model fits:
```R
save(fits, file="fits.RData")
```
