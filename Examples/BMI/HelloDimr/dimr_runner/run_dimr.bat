@echo off
rem Set the system path to include directory holdng DIMR exe and libraries
rem set PATH=c:\Users\evansta\DeltaShell\SVN_repos\delft3d\src\engines_gpl\dimr\bin\x64\Debug;%PATH%
set PATH=..\bin;%PATH%

rem Add the path to the ArrayBuilder DLL and dependencies
set PATH=..\Debug;%PATH%

rem run DIMR with arguments from the command line of this batch file
dimr %*

set /p scratch="Press enter to finish..."
