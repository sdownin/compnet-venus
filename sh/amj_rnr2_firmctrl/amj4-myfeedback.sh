#!/bin/bash
#$ -cwd
#$ -S /bin/bash
#$ -l h_vmem=6G
#$ -pe smp 4

export PATH=$PATH:$SGE_O_PATH
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH

cd $SGE_O_WORKDIR
/home/sdowning/R-3.3.2/bin/Rscript /home/sdowning/compnet/R/amj_rnr2_firmctrl/awareness_AMJ_RNR_TERGM_m4-myfeedback.R >> /home/sdowning/compnet/results/amj_rnr/awareness_AMJ_RNR_TERGM_m4-myfeedback_out.txt


