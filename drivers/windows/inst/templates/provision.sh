#!/bin/bash
# automatically generated
echo generated on host: {{hostname}}
echo generated on date: {{date}}
echo hipercow version: {{hipercow_version}}
echo running on: $(hostname -f)

export PATH=/opt/apps/lmod/lmod/libexec:$PATH
source /opt/apps/lmod/lmod/init/bash
export LMOD_CMD=/opt/apps/lmod/lmod/libexec/lmod
module use /modules-share/modules/all

module load R/{{r_version}}
module load hiredis
module load libsodium

echo working directory: $(pwd)

export RENV_AUTOLOADER_ENABLED=FALSE

echo this is a provisioning task

@REM The quoting here is necessary for paths with spaces.
Rscript "hipercow/provision/{{id}}/conan.R" > "hipercow/provision/{{id}}/log" 2>&1

ErrorCode=$?


if [ $ErrorCode -ne 0 ]; then
  echo Error running task
  exit $ErrorCode
fi

echo Quitting
