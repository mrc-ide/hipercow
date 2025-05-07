#!/bin/bash
# automatically generated
echo generated on host: {{hostname}}
echo generated on date: {{date}}
echo hipercow version: {{hipercow_version}}
echo hipercow.dide version: {{hipercow_dide_version}}
echo running on: $(hostname -f)

export PATH=/opt/apps/lmod/lmod/libexec:$PATH
source /opt/apps/lmod/lmod/init/bash
export LMOD_CMD=/opt/apps/lmod/lmod/libexec/lmod
module use /modules-share/modules/all

module load R/{{r_version}}
module load hiredis

cd {{hipercow_root_path}}
echo working directory: $(pwd)

export R_LIBS_USER={{hipercow_library}}
export R_ENVIRON_USER={{renviron_path}}
export HIPERCOW_NO_DRIVERS=1
export RENV_AUTOLOADER_ENABLED=FALSE
export HIPERCOW_CORES=$CCP_NUMCPUS
export REDIS_URL={{redis_url}}

echo this is a single task

Rscript -e "hipercow::task_eval('{{task_id}}', verbose = TRUE)" > "hipercow/tasks/{{task_id_1}}/{{task_id_2}}/log" 2>&1

ErrorCodeTask=$?

if [ -f hipercow/tasks/{{task_id_1}}/{{task_id_2}}/status-success ]; then
  TaskStatus=0
else
  TaskStatus=1
fi

echo ERRORLEVEL was $ErrorCodeTask

echo Cleaning up

if [ $ErrorCodeTask -ne 0 ]; then
  echo Task failed catastrophically
  exit $ErrorCodeTask
fi

if [ $TaskStatus -eq 0 ]; then
  echo Task completed successfully!
  echo Quitting
else
  echo Task did not complete successfully
  exit 1
fi
