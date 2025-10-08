#!/bin/bash
#SBATCH --get-user-env
#SBATCH --nodes=1
#SBATCH --time=10:00:00
#SBATCH --partition=amilan,amilan128c,csu
#SBATCH --qos=normal
#SBATCH --ntasks=8
#SBATCH --mem=29800
#SBATCH --array=2-10
#SBATCH --mail-type=end
#SBATCH --mail-user=rafa.ricomillan@colostate.edu
#SBATCH --job-name=pcangsd_admix_PCUL_K2-10_def
#SBATCH --output=/scratch/alpine/c838048135@colostate.edu/pelobates_plasticity/loco-pipe-pcul/pcangsd_admix/slurm_logs/pcangsd_admix_PCUL_K2-10_def.%j.out
#SBATCH --error=/scratch/alpine/c838048135@colostate.edu/pelobates_plasticity/loco-pipe-pcul/pcangsd_admix/slurm_logs/pcangsd_admix_PCUL_K2-10_def.%j.err

# Set directories and load conda environment
DIR=/scratch/alpine/c838048135\@colostate.edu/pelobates_plasticity/loco-pipe-pcul
eval "$(conda shell.bash hook)"
conda activate pcangsd_lcpipe
eig=$((SLURM_ARRAY_TASK_ID - 1))

# Run pcangsd --admix
for i in {1..5}; do
  \time pcangsd --beagle $DIR/angsd/snp_calling_global/combined.subsetted.beagle.gz --threads 8 --out $DIR/pcangsd_admix/PCUL_pcangsd_admix_eig${eig}K${SLURM_ARRAY_TASK_ID}${i}_alpha0 -e $eig --iter 2000 --admix --admix_alpha 0 --admix_seed $RANDOM
done

\time pcangsd --beagle $DIR/angsd/snp_calling_global/combined.subsetted.beagle.gz --threads 8 --out $DIR/pcangsd_admix/PCUL_pcangsd_admix_eig${eig}_K${SLURM_ARRAY_TASK_ID}_alpha10000 -e $eig --iter 2000 --admix --admix_alpha 10000