@echo off
REM automatically generated
ECHO generated on host: {{hostname}}
ECHO generated on date: {{date}}
ECHO hipercow version: {{hipercow_version}}
ECHO running on: %COMPUTERNAME%

net use I: \\wpia-hn\hipercow
call setr64_{{r_version}}.bat

{{network_shares_create}}

{{hipercow_root_drive}}
cd {{hipercow_root_path}}
ECHO working directory: %CD%

set R_LIBS_USER={{hipercow_library}}
set HIPERCOW_NO_DRIVERS=1
set RENV_AUTOLOADER_ENABLED=FALSE
set HIPERCOW_CORES=%CCP_NUMCPUS%
set REDIS_URL=10.0.2.254

ECHO this is a single task

@REM The quoting here is necessary for paths with spaces.
ECHO on
Rscript -e "hipercow::task_eval('{{task_id}}', verbose = TRUE)" > "hipercow\tasks\{{task_id_1}}\{{task_id_2}}\log" 2>&1

@ECHO off
%SystemDrive%
set ErrorCode=%ERRORLEVEL%

{{network_shares_delete}}

net use I: /delete /y

set ERRORLEVEL=%ErrorCode%

if %ERRORLEVEL% neq 0 (
  ECHO Error running task
  EXIT /b %ERRORLEVEL%
)

@ECHO Quitting
