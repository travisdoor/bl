@echo off

set CONFIG_FILE=..\etc\bl.conf

echo Running blc setup...

set /A _STATUS=0
set PDIR="%cd%"
set WDIR=%~dp0
set WDIR=%WDIR:~0,-1%
echo Working directory: %WDIR%

cd /D %WDIR%

echo - Looking for bl APIs
set LIB_DIR=..\lib\bl\api
if exist %LIB_DIR% (
   call :LibDirFound
) else (
   call :LibDirNotFound
)

echo - Looking for Visual Studio Installation
set VSWHERE="%ProgramFiles(x86)%\Microsoft Visual Studio\Installer"

if exist %VSWHERE% (
   call :VSFound
) else (
   call :VSNotFound
)

cd %VSWHERE%
for /f "delims=" %%a in ('vswhere.exe -property installationPath') do @set _VS_DIR=%%a 
cd %WDIR%
rem check _VS_DIR
set _VS_DIR=%_VS_DIR:~0,-1%

set VC_VARS_ALL=%_VS_DIR%\VC\Auxiliary\Build\vcvarsall.bat

set VC_VARS_ALL=%VC_VARS_ALL:\=\\%
set LIB_DIR=%LIB_DIR:\=\\%

if not exist "..\etc" mkdir ..\etc
(
  echo./*
  echo. * blc config file
  echo. */
  echo.
  echo.LIB_DIR "%LIB_DIR%"
  echo.LINKER_EXEC "link.exe"
  echo.VC_VARS_ALL "%VC_VARS_ALL%"
  echo.LINKER_OPT "/ENTRY:__os_start /SUBSYSTEM:CONSOLE /NOLOGO /INCREMENTAL:NO /MACHINE:x64 kernel32.lib user32.lib gdi32.lib shell32.lib ucrt.lib legacy_stdio_definitions.lib Msvcrt.lib"
  echo.LINKER_LIB_PATH ""
) > %CONFIG_FILE%

cd /D %PDIR%

if %_STATUS%==0 (
    echo Configuration finished without errors and written to %CONFIG_FILE% file.
    exit /B
) else (
    echo Configuration finished with errors.
    exit /B 1
)

:: ========== FUNCTIONS ==========
exit /B

:NormalizePath
  set RETVAL=%~dpfn1
  exit /B

:LibDirFound
  call :NormalizePath %LIB_DIR%
  set LIB_DIR=%RETVAL%
  echo.  FOUND - %LIB_DIR%
  exit /B

:LibDirNotFound
  echo.  error: Cannot find bl APIs. You can try to set correct path manually in bl.conf file.
  set /A _STATUS=1
  exit /B

:VSFound
  echo.  FOUND - %VSWHERE%
  exit /B

:VSNotFound
  echo.  error: Cannot find Visual Studio Installation. 
  set /A _STATUS=1
  exit /B
