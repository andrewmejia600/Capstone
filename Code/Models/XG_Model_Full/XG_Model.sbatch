#!/bin/bash
#SBATCH --mail-user mejiaa@smu.edu
#SBATCH --mail-type=all
#SBATCH -J XG_Model_Full                         # Job name
#SBATCH -o XG_Model_FULL_%A-%a.out               # Output file name
#SBATCH -p standard-mem-s                   # Partition (queue)
#SBATCH --exclusive                         # Exclusivity 
#SBATCH --mem=250G                          # Total memory required per node

module purge                                # Unload all modules
module load r/4.0.2                         # Load R, change version as needed

R --vanilla < XG_MII_Model.R               # Edit R script name as needed