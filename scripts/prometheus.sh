#!/bin/bash -l
## Job name
#SBATCH -J foraminifera
## Number of nodes to allocate
#SBATCH -N 1
## Number of tasks per node (by default number of cores per node to allocate)
#SBATCH --ntasks-per-node=24
#SBATCH --mem-per-cpu=4GB
#SBATCH --time=00:10:00
## Partition
#SBATCH -p plgrid-short
#SBATCH -A foraminifera

cd ${SLURM_SUBMIT_DIR}

module add plgrid/tools/java8/1.8.0_60

SUPERVISOR_HOSTNAME=`/bin/hostname`

echo $SUPERVISOR_HOSTNAME

HOSTNAMES=()
for HOST in `scontrol show hostnames`;
do
    if [ "$HOST" != "$SUPERVISOR_HOSTNAME" ]; then
        HOSTNAMES+=(${HOST})
    fi
done

mkdir -p ${SLURM_SUBMIT_DIR}/results/${SLURM_JOB_ID}
##mkdir -p ${SCRATCH}/results/${SLURM_JOB_ID}

${JAVA_HOME}/bin/java \
    -Xmx65536m -Xms512m -XX:+UseG1GC \
    -Dclustering.ip=${SUPERVISOR_HOSTNAME} \
    -Dclustering.supervisor.ip=${SUPERVISOR_HOSTNAME} \
    -Dformin.config.isSupervisor=true \
    -Dformin.config.iterationsNumber=1000 \
    -Dformin.config.gridSize=100 \
    -Dformin.config.workersRoot=6 \
    -Dakka.cluster.min-nr-of-members=1 \
    -jar formin.jar

mv forminData.txt ${SLURM_SUBMIT_DIR}/results/${SLURM_JOB_ID}/data.txt

sleep 5
exit 0
