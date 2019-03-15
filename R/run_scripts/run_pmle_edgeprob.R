cat('\n\n');timestamp();cat('\n')
library(btergm)
library(parallel)
library(texreg)

data.dir <- '/home/sdowning/data'
data.file <- 'run_pmle_hyp1000_OUT.RData'
out.file <- 'run_pmle_hyp1010ep2_OUT.RData'
out.txt <- 'run_pmle_hyp1000ep2_OUT.txt'
load(sprintf('%s/%s',data.dir,data.file))



cat('running edge prob interpretation on model f4.\n')

eprob <- btergm::edgeprob(l.hyp$misc$clarabridge$f4, parallel='multicore', ncpus=detectCores() )


#-----------------------------------------------------------------------------

cat('saving image\n')
save.image(sprintf('%s/%s',data.dir,out.file))

cat('completed successfully.\n\n')

