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
#SBATCH -t 48:00:00
#################
#quality of service; think of it as job priority
#SBATCH -p amilan128c,amilan
#SBATCH --qos=long
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
#SBATCH  --mail-user=rafa.ricomillan@colostate.edu
#################
#now run normal batch commands
##################
#echo commands to stdout
set -x
source ~/.bashrc
conda activate loco-pipe
DIR=/scratch/alpine/c838048135@colostate.edu/pelobates_plasticity/loco-pipe-pcul
SOFTWARE_DIR=/projects/c838048135@colostate.edu/soft
snakemake \
  --use-conda \
  --conda-frontend mamba \
  --conda-prefix /projects/c838048135@colostate.edu/miniconda3/envs/ \
  --directory $DIR \
  --rerun-triggers mtime \
  --scheduler greedy \
  --printshellcmds \
  --latency-wait 300 \
  --snakefile $SOFTWARE_DIR/loco-pipe/workflow/pipelines/loco-pipe.smk \
  --profile $SOFTWARE_DIR/loco-pipe/workflow/profiles/slurm \
  --default-resources mem_mb=None disk_mb=None