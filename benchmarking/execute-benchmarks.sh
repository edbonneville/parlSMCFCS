#!/bin/bash
#SBATCH -J parlsmcfcs-benchmarks
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=3
#SBATCH --time=10:00:00 
#SBATCH --mem=5G 

# Purge
module purge

# Load module
module load statistical/R/4.0.2

# Run imps
Rscript ./benchmarking/benchmarks.R
