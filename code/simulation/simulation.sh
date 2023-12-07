#!/bin/bash

# Example of running R script with a job array

#SBATCH --array=1-400                     # how many tasks in the array
#SBATCH --output=outputs/o.%a.out   # file to collect standard output
#SBATCH --error=errors/err.%a.log    # file to collect standard output

#SBATCH --cpus-per-task=1       # number of cores
#SBATCH --nodes=1               # number of nodes
#SBATCH --mem=7GB               # memory per __node__

module load conda_R

R CMD BATCH --no-save simulation.R log/e.$SLURM_ARRAY_TASK_ID
exit 0