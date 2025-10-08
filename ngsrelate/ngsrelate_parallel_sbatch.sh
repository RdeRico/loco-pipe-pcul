#!/bin/bash
#SBATCH --get-user-env
#SBATCH --nodes=1
#SBATCH --time=10:00:00
#SBATCH --partition=amilan,amilan128c
#SBATCH --qos=normal
#SBATCH --ntasks=8
#SBATCH --mem=29800
#SBATCH --array=1-14  # Adjust based on your number of chromosomes
#SBATCH --mail-type=end
#SBATCH --mail-user=rafa.ricomillan@colostate.edu
#SBATCH --job-name=PCUL_ngsrelate_parallel
#SBATCH --output=/scratch/alpine/c838048135@colostate.edu/pelobates_plasticity/ngsrelate/slurm_logs/PCUL_ngsrelate_parallel.%A_%a.out
#SBATCH --error=/scratch/alpine/c838048135@colostate.edu/pelobates_plasticity/ngsrelate/slurm_logs/PCUL_ngsrelate_parallel.%A_%a.err

# Set up directory and activate conda environment
DIR=/scratch/alpine/c838048135@colostate.edu/pelobates_plasticity/ngsrelate
BAMS=$DIR/bamlist.txt
CHR_FILE=$DIR/chr_table.PCUL_onlychromosomes_.txt

eval "$(conda shell.bash hook)"
conda activate angsd_lcpipe

# Create chromosome list
CHROMS=($(cat ${CHR_FILE}))

# Get chromosome for this array task
CHR=${CHROMS[$((SLURM_ARRAY_TASK_ID - 1))]}

# Generate allele frequencies and genotype likelihoods with ANGSD for specific chromosome
angsd -b $BAMS -P 8 -gl 2 -domajorminor 1 -snp_pval 1e-6 -domaf 1 -minmaf 0.05 -doGlf 3 -r ${CHR}: -out "$DIR/angsdput_chr${CHR}" &> "$DIR/angsd_chr${CHR}.log"

echo "Finished processing chromosome ${CHR}"