#!/bin/bash
#
#all commands that start with SBATCH contain commands that are just used by SLURM for scheduling
#################
#set a job name
#SBATCH --job-name=loco-pipe
#################
#a file for job output, you can check job progress
#SBATCH --output=loco-pipe.%j.out
#################
# a file for errors from the job
#SBATCH --error=loco-pipe.%j.err
#################
#time you think you need; default is one hour
#in minutes in this case
#SBATCH -t 24:00:00
#################

#################
#number of nodes
#SBATCH --nodes=1
#SBATCH --ntasks-per-node 1
#################
#SBATCH --mem=3G
#################
#get emailed about job BEGIN, END, and FAIL
#SBATCH --mail-type=END
#################
#who to send email to; please change to your email
#SBATCH  --mail-user=rafael.rico@ebd.csic.es
#################
#now run normal batch commands
##################
#echo commands to stdout

set -x 

# 1. Limpieza total
module purge

# 2. Carga la arquitectura correcta (Rocky Linux) y el Conda del servidor
module load cesga/system
module load miniconda3

# 3. Inicializa Conda de forma limpia (sustituye a source ~/.bashrc)
eval "$(conda shell.bash hook)"

# 4. Limpia variables de R para evitar conflictos
unset R_LIBS R_LIBS_USER R_LIBS_SITE
export R_LIBS_USER=""

conda activate loco-pipe
DIR=/home/csic/eye/rrm/lustre/pelobates_plasticity/loco-pipe-pcul
SOFTWARE_DIR=/home/csic/eye/rrm/store/soft
snakemake \
  --use-conda \
  --conda-frontend mamba \
  --directory $DIR \
  --rerun-triggers mtime \
  --scheduler greedy \
  --printshellcmds \
  --latency-wait 300 \
  --snakefile $SOFTWARE_DIR/loco-pipe/workflow/pipelines/loco-pipe.smk \
  --profile $SOFTWARE_DIR/loco-pipe/workflow/profiles/slurm \
  --jobs 20 \
  --default-resources mem_mb=None disk_mb=None