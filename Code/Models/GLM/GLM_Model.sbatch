#!/bin/bash
#SBATCH --mail-user mejiaa@smu.edu          #Edit Email Address
#SBATCH --mail-type=all
#SBATCH -J GLM_Model                         # Job name
#SBATCH -o GLM_Model_%A-%a.out               # Output file name
#SBATCH -p standard-mem-s                   # Partition (queue)
#SBATCH --exclusive                         # Exclusivity 
#SBATCH --mem=250G                          # Total memory required per node

module purge                                # Unload all modules
module load r/4.0.2                         # Load R, change version as needed

R --vanilla < GLM.R               # Edit R script name as needed
