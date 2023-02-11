#!/bin/bash
#SBATCH --account=def-benthos
#SBATCH --mem=20GB
#SBATCH --time=30:00:00
#SBATCH --array=1-205
#SBATCH --mail-user=david.beauchesne@hotmail.com
#SBATCH --mail-type=ALL

# I think this is the defaut behavior anyway
module load StdEnv/2020 r/4.2.1
Rscript ./launch.R $SLURM_ARRAY_TASK_ID