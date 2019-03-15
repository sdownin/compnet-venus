cat('\n\n');timestamp();cat('\n')
library(btergm)
library(parallel)
library(texreg)

data.dir <- '/home/sdowning/data'
data.file <- 'run_pmle_hyp_OUT.RData'
out.file <- 'run_pmle_hyp_OUT.RData'
out.txt <- 'run_pmle_hyp_OUT.txt'
load(sprintf('%s/%s',data.dir,data.file))



cat('running goodness of fit on model f4.\n')

g45.100  = btergm::gof(l.hyp[[net_group]][[firm_i]]$f4.5, nsim=100 , statistics = c(dsp, esp, deg, geodesic) )
g46.100  = btergm::gof(l.hyp[[net_group]][[firm_i]]$f4.6, nsim=100 , statistics = c(dsp, esp, deg, geodesic) )

#-----------------------------------------------------------------------------

cat('saving image\n')
save.image(sprintf('%s/%s',data.dir,out.file))

cat('completed successfully.\n\n')

