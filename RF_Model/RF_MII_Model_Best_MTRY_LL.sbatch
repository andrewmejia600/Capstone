#!/bin/bash                                 
#SBATCH --mail-user llazarescou@smu.edu              #Edit email address 
#SBATCH --mail-type=all
#SBATCH -J RF_Model_Grid_Search                 # Job name
#SBATCH -o RF_Model_MTRY_GRID_SEARCH%A-%a.out   # Output file name
#SBATCH -p standard-mem-s                       # Partition (queue)
#SBATCH --exclusive                             # Exclusivity 
#SBATCH --mem=250G                              # Total memory required per node

module purge                                    # Unload all modules
module load r/4.0.2                             # Load R, change version as needed

R --vanilla < RF_MII_Model_Best_MTRY.R               # Edit R script name as needed