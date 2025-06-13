#!/bin/bash
#SBATCH --job-name=taxize
#SBATCH --partition=day
#SBATCH --cpus-per-task=12
#SBATCH --mem=100G
#SBATCH --time=23:00:00
#SBATCH --mail-type=END,FAIL
#SBATCH --mail-user=stanley.tan@yale.edu

module load R/4.4.1-foss-2022b

Rscript taxize.R


