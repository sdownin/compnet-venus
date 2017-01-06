cat('\n\n');timestamp();cat('\n')
library(btergm)
library(parallel)
library(snow)
library(texreg)

load('/home/sdowning/data/run_mcmle_test.RData')

nets.sub <- firm.nets$test$clarabridge

mmc <- lapply(nets.sub, function(net) as.matrix(net %n% 'mmc'))
sim <- lapply(nets.sub, function(net) as.matrix(net %n% 'similarity'))
cat('summary(sim)\n')
print(summary(sim))


ncpus <- 8
(cl <- snow::makeCluster(ncpus))

fm1 <- mtergm(nets.sub ~ edges + gwesp(0, fixed=T)  +
                nodecov('age') +   edgecov(sim)  +
                absdiff('constraint') +
                cycle(3)  
              , parallel = "snow", ncpus = ncpus, cl=cl)

cat('completed fit.')
save.image('/home/sdowning/data/run_mcmle_test_OUT.RData')

tryCatch( screenreg(fm1), error=function(e) e, finally=print('finishing screenreg'))
tryCatch( screenreg(fm1, file="/home/sdowning/data/run_mcmle_test_OUT.txt"), error=function(e) e, finally=print('finishing screenreg to file'))

cat('saving image\n')
save.image('/home/sdowning/data/run_mcmle_test_OUT.RData')

snow::stopCluster(cl)

cat('completed successfully.\n\n')

