#!/bin/bash
#$ -cwd
#$ -S /bin/bash
#$ -l h_vmem=36G

export PATH=$PATH:$SGE_O_PATH
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH

cd $SGE_O_WORKDIR
/home/sdowning/R-3.3.2/bin/Rscript /home/sdowning/compnet/R/awareness_AMJ_TERGM_m4_timecov.R >> /home/sdowning/compnet/results/amj_rnr2/awareness_AMJ_TERGM_m4_timecov_out.txt


