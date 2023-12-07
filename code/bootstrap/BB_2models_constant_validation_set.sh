#!/bin/bash

# Example of running R script with a job array

#SBATCH --array=1-2220                     # how many tasks in the array
#SBATCH --output=outputs/o.%a.out   # file to collect standard output
#SBATCH --error=errors/err.%a.log    # file to collect standard output

#SBATCH --cpus-per-task=1       # number of cores
#SBATCH --nodes=1               # number of nodes
#SBATCH --mem=15GB               # memory per __node__

module load conda_R

R CMD BATCH --no-save BB_2models_constant_validation_set.R log/e.$SLURM_ARRAY_TASK_ID
exit 0