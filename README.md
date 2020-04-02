# Diorhaba Population Structure
Data and analysis of Diorhabda population structure using RADseq and a de novo reference genome.

Draft manuscript here
https://docs.google.com/document/d/1wOD6VSrJXd9g6Ph7XQQMsSpeIC3T84tSsuULdKta_jA/edit

Goals are to
* Quantify prevalence and levels of hybridization among the four introduced species and source populations.
* Examine the consequences of population bottlenecks and admixture on genome-wide diversity across broad, landscape-wide expansion fronts and the hybrid zone.

Most work was performed on the IBEST cluster, usually crick or watson.

Important metadata and tabular results here
https://docs.google.com/spreadsheets/d/1s_31zzCnG2Orny9uoOTxPE4iI0ZpVWUXuOm5Ja10hTE/edit?usp=sharing

#### What I did

1. Assembled de novo Diorhabda carinulata reference genome, combining shotgun scaffolds assembled in Spades with in-silico 'linked' 10X reads.
  * raw reads for shotgun
  * raw reads for 10X
  * spades assembly
  * LINK/Arcs process
  * BUSCO and QUAST assessment
2. Generated SNPs across lots of individuals and populations (raw2vcftask.sh)
  * raw reads
  * clone_filtered
  * process_radtags
  * aligned to reference (1)
  * gstacks
  * populations
3. Model-based ancestry assignment in Structure
   * downloaded vcf to local computer
   * converted vcf to structure format with PGDspider
   * uploaded this back to cluster
   * ran K= 1-10 for 10 reps each

#### Visualizations
  * Coverage
  * Missing Data
  * Population genetic stats
    * pi
    * exp_het
    * FIS
    * private alleles

#### Next Steps
