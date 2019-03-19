#!/bin/bash
#$ -cwd
#$ -S /bin/bash
#$ -l h_vmem=16G
#$ -pe smp 1

PATH=$PATH:$SGE_O_PATH
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH

cd $SGE_O_WORKDIR
/home/sdowning/R-3.3.2/bin/Rscript /home/sdowning/compnet/R/amj_rnr2/awareness_AMJ_RNR_TERGM_m4-gof-degree.R >> /home/sdowning/compnet/results/amj_rnr2/awareness_AMJ_RNR_TERGM_m4-gof-degree_out.txt


