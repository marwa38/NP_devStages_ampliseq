#! /bin/bash
#SBATCH --partition=uoa-compute
#SBATCH --ntasks=24
#SBATCH --time=05:00:00
#SBATCH --mem-per-cpu=8g
#SBATCH --mail-type=ALL
#SBATCH --mail-user=m.tawfik.19@abdn.ac.uk

printf " \n"
echo "Pedal to the metal!"
printf " \n"

#loading the necessary TrimGalore software module
echo "loading required modules"
printf "\n"

module load FastQC-0.11.8
module load TrimGalore-0.6.4
module load parallel-20200222
parallel --citation

# Define an array with all the sample names
sample_names=(
    "M-T1-Rep1"
    "M-T5-Rep2"
    "M-T1-Rep2"
    "M-T5-Rep3"
    "M-T1-Rep4"
    "M-T5-Rep5"
    "M-T5-Rep6"
    "M-T9-Rep1"
    "M-T9-Rep2"
    "M-T9-Rep3"
    "M-T9-Rep4"
    "M-T9-Rep6"
    "M-T1-Rep6"
    "M-T5-Rep1"
    "M-T5-Rep4"
    "M-T9-Rep5"
    "M-T1-Rep5"
    "M-T1-Rep3"
    "V-T8-Rep1"
    "V-T3-Rep1"
    "V-T8-Rep2"
    "V-T3-Rep2"
    "V-T8-Rep3"
    "V-T3-Rep3"
    "V-T8-Rep4"
    "V-T3-Rep4"
    "V-T8-Rep5"
    "V-T3-Rep5"
    "V-T8-Rep6"
    "V-T3-Rep6"
    "V-T12-Rep2"
    "V-T12-Rep3"
    "V-T12-Rep4"
    "V-T12-Rep5"
    "V-T12-Rep6"
    "V-T12-Rep1"
    "MM-T5-Rep3"
    "MM-T9-Rep1"
    "MM-T9-Rep2"
    "MM-T5-Rep2"
    "MM-T5-Rep1"
    "MM-T9-Rep4"
    "MM-T9-Rep5"
    "MM-T1-Rep1"
    "MM-T1-Rep5"
    "MM-T9-Rep3"
    "MM-T5-Rep6"
    "MM-T1-Rep3"
    "MM-T1-Rep4"
    "MM-T1-Rep6"
    "MM-T9-Rep6"
    "MM-T1-Rep2"
    "MM-T5-Rep4"
    "MM-T5-Rep5"
    "VM-T8-Rep1"
    "VM-T8-Rep2"
    "VM-T8-Rep3"
    "VM-T8-Rep4"
    "VM-T8-Rep5"
    "VM-T8-Rep6"
    "VM-T12-Rep5"
    "VM-T3-Rep6"
    "VM-T3-Rep5"
    "VM-T12-Rep1"
    "VM-T12-Rep3"
    "VM-T12-Rep4"
    "VM-T3-Rep1"
    "VM-T12-Rep6"
    "VM-T3-Rep3"
    "VM-T3-Rep2"
    "VM-T12-Rep2"
    "VM-T3-Rep4"
    "MMV-T5-Rep6"
    "MMV-T9-Rep1"
    "MMV-T9-Rep2"
    "MMV-T9-Rep4"
    "MMV-T9-Rep5"
    "MMV-T5-Rep5"
    "MMV-T1-Rep5"
    "MMV-T5-Rep2"
    "MMV-T5-Rep3"
    "MMV-T9-Rep6"
    "MMV-T5-Rep4"
    "MMV-T1-Rep4"
    "MMV-T9-Rep3"
    "MMV-T1-Rep2"
    "MMV-T1-Rep1"
    "MMV-T5-Rep1"
    "MMV-T1-Rep6"
    "MMV-T1-Rep3"
    "VMV-T8-Rep2"
    "VMV-T3-Rep4"
    "VMV-T8-Rep4"
    "VMV-T8-Rep6"
    "VMV-T12-Rep1"
    "VMV-T12-Rep2"
    "VMV-T3-Rep3"
    "VMV-T8-Rep1"
    "VMV-T3-Rep5"
    "VMV-T3-Rep2"
    "VMV-T12-Rep5"
    "VMV-T8-Rep5"
    "VMV-T12-Rep4"
    "VMV-T12-Rep6"
    "VMV-T3-Rep6"
    "VMV-T3-Rep1"
    "VMV-T8-Rep3"
    "VMV-T12-Rep3"
    "feed-M-Rep1"
    "feed-M-Rep3"
    "feed-M-Rep2"
    "feed-V-Rep1"
    "feed-V-Rep2"
    "feed-V-Rep3"
    "water-M-T5-Rep2"
    "water-M-T9-Rep3"
    "water-M-T1-Rep1"
    "water-V-T3-Rep1"
    "water-V-T8-Rep2"
    "water-V-T12-Rep3"
    "water-MM-T5-Rep2"
    "water-MM-T1-Rep1"
    "water-MM-T9-Rep3"
    "water-VM-T8-Rep2"
    "water-VM-T3-Rep1"
    "water-VM-T12-Rep3"
    "water-MMV-T1-Rep1"
    "water-MMV-T9-Rep2"
    "water-VMV-T3+12-Rep4"
    "water-VMV-T3-Rep1"
    "water-VMV-T12-Rep3"
    "water-VMV-T8-Rep2"
    "negative-1"
    "negative-2"
    "negative-3"
    "positive-2"
    "positive-1"
)

#run trim Galore across all zipped ziles
echo "running trim Galore over all fastq files"
printf "\n"

# Iterate over each sample name
for sample in "${sample_names[@]}"; do
    echo "Running TrimGalore for $sample"
    printf "\n"

echo "running TrimGalore over read pair"
printf "\n"

    trim_galore -q 30 --phred33 --paired --gzip --fastqc --fastqc_args "--nogroup" -o "./trimGalored" --length 20 --stringency 1 -e 0 --clip_R1 17 --clip_R2 21 "${sample}_R1.fq.gz" "${sample}_R2.fq.gz"
done

echo "FINITO!!"
printf "\n"
