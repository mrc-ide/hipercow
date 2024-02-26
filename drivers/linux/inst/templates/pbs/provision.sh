#!/bin/bash

#PBS -lwalltime=00:30:00
#PBS -lselect=1:ncpus=1:mem=4gb
cd ${PBS_O_WORKDIR}

echo "generated on host: {{hostname}}"
echo "generated on date: {{date}}"
echo "hipercow version: {{hipercow_version}}"
echo "running on: $(hostname)"
echo "working directory: $(pwd)"

module load tools/prod
module load {{r_module}}

export RENV_AUTOLOADER_ENABLED=FALSE

echo "this is a provisioning task"

Rscript "hipercow/provision/{{id}}/conan.R" > "hipercow/provision/{{id}}/log" 2>&1
