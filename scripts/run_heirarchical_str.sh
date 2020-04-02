#!/bin/sh

study=$3
projdir=/mnt/lfs2/stah3621/diorhabda/structure/${study}
outdir=${projdir}/Results/

mkdir -p $outdir

module load structure

mainparams=$projdir/mainparams.infer_each_alpha
extraparams=$projdir/extraparams
in=$projdir/${study}.structure

### get numinds
numlines=$(wc -l $in | awk '{print $1}') #-1 and divide by two
N=$(( (numlines - 1) / 2 ))

### get  num loci
L=$(head -n 1 $in | awk '{print NF}') 

touch $extraparams # I never put anything special in the extraparams, but it's required by structure. This creates an empty file. 

K=$1 #MAXPOPS
eachK=$2 # REP NUMBER
                structure -m $mainparams -e $extraparams \
			  -D $RANDOM -K $K -i $in -L $L -N $N \
                          -o $outdir/outfile_${study}_k${K}_rep${eachK}
