# Diorhaba Population Structure
Data and analysis of Diorhabda population structure using RADseq and a de novo reference genome.

Manuscript in prep!

Goals are to
* Quantify prevalence and levels of hybridization among the four introduced species and source populations.
* Examine the consequences of population bottlenecks and admixture on genome-wide diversity across broad, landscape-wide expansion fronts and the hybrid zone.

Most work was performed on the IBEST cluster, usually crick or watson.

## What I did

1. Assembled de novo Diorhabda carinulata reference genome, combining shotgun scaffolds assembled in Spades with in-silico 'linked' 10X reads.
  * raw reads for shotgun
  * raw reads for 10X
  * spades assembly
  * LINK/Arcs pipeline
  * BUSCO and QUAST assessment

2. Generated SNPs across lots of individuals and populations (raw2vcftask.sh)
  * raw reads
  * clone_filtered
  * process_radtags
  * aligned to reference
  * gstacks
  * populations

3. Model-based ancestry assignment in Structure
  * downloaded vcf to local computer
  * converted vcf to structure format with PGDspider
  * uploaded this back to cluster
  * ran K= 1-10 for 10 reps each
  * Re-ran this for carinulata and elongata to examine population substructure

4. Examined population genetics in the context of hybridization
  * Popgen stats
  * Isolation by distance
  * RangeExpansion analysis on carinulata

## To-do
  * upload scripts for genome assembly
  * upload raw data for genome assembly
  * upload raw data for population genetics
  * associate each of the steps above with scripts and directories
