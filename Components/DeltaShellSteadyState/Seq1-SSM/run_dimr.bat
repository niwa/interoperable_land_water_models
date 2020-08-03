@echo off
rem Set the system path to include directory holdng DIMR exe and libraries
set SYSPATH=%PATH%
set PATH=c:\Models\Programs\dimr;%PATH%
set PATH=C:\Models\lib;%PATH%

rem run DIMR with arguments from the command line of this batch file
dimr %*

rem Restore the system path 
set PATH=%SYSPATH%

pause

