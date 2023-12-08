#!/bin/bash

#SBATCH --qos=priority
#SBATCH --job-name=mrdownscale
#SBATCH --output=logs/slurm-%j.log
#SBATCH --mail-type=END
#SBATCH --cpus-per-task=16
#SBATCH --mem-per-cpu=0
#SBATCH --partition=priority

module load nco/5.1.0
Rscript startRun.R
