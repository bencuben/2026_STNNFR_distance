#!/bin/bash
#SBATCH --job-name=serial_job_paper2 	 # Job name
#SBATCH --partition=sixhour        	 # Partition Name (Required) (sixhour, biostat)
#SBATCH --mail-type=FAIL       	         # Mail events (NONE, BEGIN, END, FAIL, ALL)
#SBATCH --mail-user=bcanourrego@kumc.edu # Where to send mail
#SBATCH --ntasks=1			 # Run on a single CPU
#SBATCH --cpus-per-task=4                # Number of CPU cores per task
#SBATCH --mem=4gb			 # Job memory request
#SBATCH --time=4:00:00		 	 # Time limit days-hrs:min:sec
#SBATCH --output=logs/paper2_%j.log         # Standard output and error log

pwd; hostname; date

module load R/4.5

echo "Running R script"

Rscript /home/bcanourrego/work/paper2/Pilot_simulation_1.R "$1"

date