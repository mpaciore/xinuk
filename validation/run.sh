#!/bin/bash -l
#SBATCH --job-name=xinuk
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=24
#SBATCH --mem-per-cpu=4GB
#SBATCH --time=48:00:00
#SBATCH --partition=plgrid
##SBATCH --account=<grand_id>


#===============================================================================
# global config
#===============================================================================
cd ${SLURM_SUBMIT_DIR}

module add plgrid/tools/java8/1.8.0_60

SUPERVISOR_HOSTNAME=`/bin/hostname`
HOSTSS=`scontrol show hostnames`
NHOSTS=`echo ${HOSTSS} | wc -w`
echo NHOSTS ${NHOSTS}
HOSTNAMES=`echo ${HOSTSS} | sed "s/\b$SUPERVISOR_HOSTNAME\b//g"`

WORKERS_ROOT_MAX=4
SAMPLES=30
JAR_DIR=${PLG_USER_STORAGE}/xinuk/jar
LOG_DIR=${PLG_USER_STORAGE}/xinuk/log/latest
mkdir -p ${LOG_DIR}



#===============================================================================
# functions
#===============================================================================

run_simulation() {
    for ((WORKERS_ROOT=1; WORKERS_ROOT<=${WORKERS_ROOT_MAX}; WORKERS_ROOT++)); do
        echo `date -u +"%H:%M:%S"` workers root: ${WORKERS_ROOT}
        GRID_SIZE=$((${GRID_SIZE_TOTAL} / ${WORKERS_ROOT} + 2))
        for ((SAMPLE_N=1; SAMPLE_N<=${SAMPLES}; SAMPLE_N++)); do
            echo `date -u +"%H:%M:%S"` sample: ${SAMPLE_N}

            for WORKER_HOST in ${HOSTNAMES}; do
                srun -w${WORKER_HOST} -c${SLURM_NTASKS_PER_NODE} -N1 -n1 \
                    ${JAVA_HOME}/bin/java \
                    -Xmx65536m -Xms512m -XX:+UseG1GC \
                    -Dclustering.ip=${WORKER_HOST} \
                    -Dclustering.supervisor.ip=${SUPERVISOR_HOSTNAME} \
                    -Dlog.name=${LOG_FULL_DIR}/${WORKERS_ROOT}_${SAMPLE_N}_${WORKER_HOST} \
                    -D${PREFIX}.config.guiType=none \
                    -D${PREFIX}.config.iterationsNumber=${ITERATIONS_NUMBER} \
                    -D${PREFIX}.config.gridSize=${GRID_SIZE} \
                    -D${PREFIX}.config.workersRoot=${WORKERS_ROOT} \
                    -Dakka.cluster.min-nr-of-members=${NHOSTS} \
                    ${PARAMS} \
                    -jar ${JAR_DIR}/${PREFIX}.jar &
            done

            ${JAVA_HOME}/bin/java \
                -Xmx65536m -Xms512m -XX:+UseG1GC \
                -Dclustering.ip=${SUPERVISOR_HOSTNAME} \
                -Dclustering.supervisor.ip=${SUPERVISOR_HOSTNAME} \
                -Dlog.name=${LOG_FULL_DIR}/${WORKERS_ROOT}_${SAMPLE_N}_${SUPERVISOR_HOSTNAME} \
                -D${PREFIX}.config.guiType=none \
                -D${PREFIX}.config.isSupervisor=true \
                -D${PREFIX}.config.iterationsNumber=${ITERATIONS_NUMBER} \
                -D${PREFIX}.config.gridSize=${GRID_SIZE} \
                -D${PREFIX}.config.workersRoot=${WORKERS_ROOT} \
                -Dakka.cluster.min-nr-of-members=${NHOSTS} \
                ${PARAMS} \
                -jar ${JAR_DIR}/${PREFIX}.jar
        done
    done
}

run_variants() {
    echo `date -u +"%H:%M:%S"` prefix: ${PREFIX}

    # default
    echo `date -u +"%H:%M:%S"` series: default
    LOG_FULL_DIR=${LOG_DIR}/${PREFIX}/default
    mkdir -p ${LOG_FULL_DIR}
    PARAMS=${PARAMS_DEFAULT}
    run_simulation

    # variant 1
    echo `date -u +"%H:%M:%S"` series: variant 1
    LOG_FULL_DIR=${LOG_DIR}/${PREFIX}/variant1
    mkdir -p ${LOG_FULL_DIR}
    PARAMS=${PARAMS_VARIANT_1}
    run_simulation

    # variant 2
    echo `date -u +"%H:%M:%S"` series: variant 2
    LOG_FULL_DIR=${LOG_DIR}/${PREFIX}/variant2
    mkdir -p ${LOG_FULL_DIR}
    PARAMS=${PARAMS_VARIANT_2}
    run_simulation

    # variant 3
    echo `date -u +"%H:%M:%S"` series: variant 3
    LOG_FULL_DIR=${LOG_DIR}/${PREFIX}/variant3
    mkdir -p ${LOG_FULL_DIR}
    PARAMS=${PARAMS_VARIANT_3}
    run_simulation

    echo `date -u +"%H:%M:%S"` prefix: ${PREFIX} done
}


#===============================================================================
# formin
#===============================================================================
if [[ "$@" =~ ^(.+ )?formin( .+)?$ ]]; then
    PREFIX=formin
    ITERATIONS_NUMBER=1000
    GRID_SIZE_TOTAL=120
    PARAMS_DEFAULT="-D${PREFIX}.config.foraminiferaReproductionCost=0.5"
    PARAMS_VARIANT_1="-D${PREFIX}.config.foraminiferaReproductionCost=0.5 -D${PREFIX}.config.algaeEnergeticCapacity=0.45"  # base 0.6, foraminifera need more food
    PARAMS_VARIANT_2="-D${PREFIX}.config.foraminiferaReproductionCost=0.5 -D${PREFIX}.config.algaeReproductionFrequency=5"  # from 2, 2.5 times slower (it is actually delay)
    PARAMS_VARIANT_3="-D${PREFIX}.config.foraminiferaReproductionCost=0.5 -D${PREFIX}.config.foraminiferaLifeActivityCost=0.5"  # from 0.1, foraminifera need much more food
    run_variants
fi

#===============================================================================
# fortwist
#===============================================================================
if [[ "$@" =~ ^(.+ )?fortwist( .+)?$ ]]; then
    PREFIX=fortwist
    ITERATIONS_NUMBER=1000
    GRID_SIZE_TOTAL=120
    PARAMS_DEFAULT=
    PARAMS_VARIANT_1="-D${PREFIX}.config.algaeEnergeticCapacity=0.05"  # base 0.1, foraminifera need more food
    PARAMS_VARIANT_2="-D${PREFIX}.config.foraminiferaInitialSignal=-5"  # base -1, more incentive to spread out
    PARAMS_VARIANT_3="-D${PREFIX}.config.foraminiferaLifeActivityCost=0.1"  # base 0.2, foraminifera need less food
    run_variants
fi


#===============================================================================
# torch
#===============================================================================
if [[ "$@" =~ ^(.+ )?torch( .+)?$ ]]; then
    PREFIX=torch
    ITERATIONS_NUMBER=200
    GRID_SIZE_TOTAL=180
    PARAMS_DEFAULT="-D${PREFIX}.config.fireSpeadingFrequency=1 -D${PREFIX}.config.fireSpawnChance=0.01"
    PARAMS_VARIANT_1="-D${PREFIX}.config.fireSpeadingFrequency=1 -D${PREFIX}.config.fireSpawnChance=0.01 -D${PREFIX}.config.fireInitialSignal=-0.5"  # base -0.001, more "scary" fire
    PARAMS_VARIANT_2="-D${PREFIX}.config.fireSpeadingFrequency=1 -D${PREFIX}.config.fireSpawnChance=0.05"  # higher fire spawn chance (initial)
    PARAMS_VARIANT_3="-D${PREFIX}.config.fireSpeadingFrequency=1 -D${PREFIX}.config.fireSpawnChance=0.01 -D${PREFIX}.config.humanInitialSignal=-0.01"  # base -0.1, less incentive to spread out
    run_variants
fi

sleep 20

echo `date -u +"%H:%M:%S"` all done

exit 0
