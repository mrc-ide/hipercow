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

set RENV_AUTOLOADER_ENABLED=FALSE

ECHO this is a provisioning task

@REM The quoting here is necessary for paths with spaces.
ECHO on
Rscript "hipercow\provision\{{id}}\conan.R" > "hipercow\provision\{{id}}\log" 2>&1

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
