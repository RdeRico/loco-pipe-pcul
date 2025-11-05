#!/bin/bash
#SBATCH --get-user-env
#SBATCH --nodes=1
#SBATCH --time=10:00:00
#SBATCH --partition=amilan
#SBATCH --qos=normal
#SBATCH --ntasks=32
#SBATCH --mem=60000
#SBATCH --mail-type=end
#SBATCH --mail-user=rafa.ricomillan@colostate.edu
#SBATCH --job-name=PCUL_ngsrelate
#SBATCH --output=/scratch/alpine/c838048135@colostate.edu/pelobates_plasticity/ngsrelate/PCUL_ngsrelate.%j.out
#SBATCH --error=/scratch/alpine/c838048135@colostate.edu/pelobates_plasticity/ngsrelate/PCUL_ngsrelate.%j.err

# Set up directory and activate conda environment
DIR=/scratch/alpine/c838048135@colostate.edu/pelobates_plasticity/ngsrelate
BAMS=$DIR/bamlist.txt
INDS=$DIR/inds_list.txt

source /projects/c838048135@colostate.edu/miniconda3/etc/profile.d/conda.sh
conda activate angsd_lcpipe

zcat "$DIR/angsdput_chrOW240912.1.mafs.gz" | cut -f5 | sed 1d > "$DIR/freq_OW240912.1"
zcat "$DIR/angsdput_chrOW240913.1.mafs.gz" | cut -f5 | sed 1d > "$DIR/freq_OW240913.1"

# Get number of individuals from bamlist
N_IND=$(wc -l < $BAMS)

# Run NgsRelate
echo "Running NgsRelate..."
./ngsrelate -g "$DIR/angsdput_chrOW240912.1.glf.gz" -p 32 -n $N_IND -z $INDS -f "$DIR/freq_OW240912.1" -O "$DIR/relatedness_chr1" &> "$DIR/ngsrelate_chr1.log"
./ngsrelate -g "$DIR/angsdput_chrOW240913.1.glf.gz" -p 32 -n $N_IND -z $INDS -f "$DIR/freq_OW240913.1" -O "$DIR/relatedness_chr2" &> "$DIR/ngsrelate_chr2.log"

echo "Analysis complete!"