#!/bin/bash
#SBATCH --mail-user=amandastahlke@gmail.com
#SBATCH --mail-type=ALL

study=$1
echo $study
projdir=/mnt/lfs2/stah3621/diorhabda/structure/$study
mkdir -p $projdir/slurmout

for k in `seq 1 10`
        do for eachK in `seq 1 10`
                do echo "K = $k rep = $eachK"
               	sbatch -o $projdir/slurmout/K${k}rep${eachK}.out \
		/mnt/lfs2/stah3621/diorhabda/structure/run_heirarchical_str.sh \
		$k $eachK $study
                done
                done
