#!/bin/bash -l
## Job name
#SBATCH -J foraminifera
## Number of nodes to allocate
#SBATCH -N 2
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
ITERATIONS_NUMBER=1000
GRID_SIZE=100
WORKERS_ROOT=9

HOSTSS=`scontrol show hostnames`
HOSTNAMES=`echo $HOSTSS | sed "s/\b$SUPERVISOR_HOSTNAME\b//g"`
DIRECTORY_NAME=${#HOSTNAMES[@]}_${WORKERS_ROOT}_${SLURM_JOB_ID}

mkdir -p ${SLURM_SUBMIT_DIR}/results/${DIRECTORY_NAME}

for WORKER_HOST in $HOSTNAMES
    do
        srun -w${WORKER_HOST} -c${SLURM_NTASKS_PER_NODE} -N1 -n1 \
            ${JAVA_HOME}/bin/java \
            -Xmx65536m -Xms512m -XX:+UseG1GC \
            -Dclustering.ip=${WORKER_HOST} \
            -Dclustering.supervisor.ip=${SUPERVISOR_HOSTNAME} \
            -Dlog.name=${SLURM_SUBMIT_DIR}/results/${DIRECTORY_NAME}/${WORKER_HOST} \
            -Dformin.config.iterationsNumber=${ITERATIONS_NUMBER} \
            -Dformin.config.gridSize=${GRID_SIZE} \
            -Dformin.config.workersRoot=${WORKERS_ROOT} \
            -Dakka.cluster.min-nr-of-members=${#HOSTNAMES[@]} \
            -jar formin.jar &
    done

sleep 15

${JAVA_HOME}/bin/java \
    -Xmx65536m -Xms512m -XX:+UseG1GC \
    -Dclustering.ip=${SUPERVISOR_HOSTNAME} \
    -Dclustering.supervisor.ip=${SUPERVISOR_HOSTNAME} \
    -Dlog.name=${SLURM_SUBMIT_DIR}/results/${DIRECTORY_NAME}/${SUPERVISOR_HOSTNAME} \
    -Dformin.config.isSupervisor=true \
    -Dformin.config.iterationsNumber=${ITERATIONS_NUMBER} \
    -Dformin.config.gridSize=${GRID_SIZE} \
    -Dformin.config.workersRoot=${WORKERS_ROOT} \
    -Dakka.cluster.min-nr-of-members=${#HOSTNAMES[@]} \
    -jar formin.jar

sleep 5
exit 0
