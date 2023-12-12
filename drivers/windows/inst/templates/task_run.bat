@echo off
REM automatically generated
ECHO generated on host: {{hostname}}
ECHO generated on date: {{date}}
ECHO hermod version: {{hermod_version}}
ECHO running on: %COMPUTERNAME%

net use I: \\wpia-hn\hipercow
call setr64_{{r_version}}.bat

{{network_shares_create}}

{{hermod_root_drive}}
cd {{hermod_root_path}}
ECHO working directory: %CD%

set R_LIBS_USER={{hermod_library}}

ECHO this is a single task

@REM The quoting here is necessary for paths with spaces.
ECHO on
Rscript -e "hermod::hermod_task_eval('{{hermod_task_id}}')" > "hermod\tasks\{{hermod_task_id}}\log" 2>&1

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
