#!/bin/bash
#SBATCH --account=def-benthos
#SBATCH --time=2:00:00
#SBATCH --nodes=6
#SBATCH --cpus-per-task=1
#SBATCH --mem-per-cpu=4G
#SBATCH --array=1-171
#SBATCH --mail-user=kcazelle@uoguelph.ca
#SBATCH --mail-type=FAIL

# I think this is the defaut behavior anyway
module load StdEnv/2020 r/4.2.1
Rscript ./launch.R $SLURM_ARRAY_TASK_ID