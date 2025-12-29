#!/bin/bash

#PBS -l walltime={{walltime}}
#PBS -l select=1:ncpus={{cores}}:mem={{memory}}gb
#PBS -N {{task_id}}

### This technically makes this driver dependent on easybuild or
### this module system, and other ways would be possible.  However, we
### don't know what those look like and this could easily be
### substituted in.  The tools/prod is likely very Imperial-ICT
### specific too and we might want to make that configurable.
module load tools/prod
module load {{r_module}}

### If the task was submitted from the root directory of hipercow
### (which we can guarantee if we don't do so already) this will
### change directory to the hipercow root from now, which is good for
### us.
cd ${PBS_O_WORKDIR}

export R_LIBS_USER={{hipercow_library}}
export HIPERCOW_NO_DRIVERS=1
export RENV_AUTOLOADER_ENABLED=FALSE
### According to NASA, who are we to argue...
export HIPERCOW_CORES=$NCPUS

echo "generated on host: {{hostname}}"
echo "generated on date: {{date}}"
echo "hipercow version: {{hipercow_version}}"
echo "running on: $(hostname)"

echo "working directory: $(pwd)"

echo "this is a single task"

Rscript -e "hipercow::task_eval('{{task_id}}', verbose = TRUE)" > "hipercow/tasks/{{task_id}}/log" 2>&1
