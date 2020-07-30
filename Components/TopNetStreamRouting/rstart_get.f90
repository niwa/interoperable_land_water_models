!function RSTART_GET(LAKEFLAG,NRCH,NLAK,DTIME,LEXIST,FEXIST,MAXQPAR,RESTART_FILE)
function RSTART_GET(RESTART_FILE,DTIME,LEXIST,FEXIST)
! ----------------------------------------------------------------------------------------
! Creator:
!   Martyn Clark, 2006
!
! ----------------------------------------------------------------------------------------
! Purpose:
!   READ from the model re-start file
!    (1) model states
!    (2) flow routed in the basins (time-delay histogram)
!    (3) individual streamflow particles in each reach
!    (4) lake states
!
! ----------------------------------------------------------------------------------------
! I/O:
!
!   INPUT:
!      NENS: Number of ensemble members
!      NRCH: Number of basins
!      NLAK: Number of lakes
!     DTIME: Time difference between restart file and model reference time
!    LEXIST: Logical flag = .TRUE. if re-start file exists
!    FEXIST: if true, only check if data-file exists
!
! ----------------------------------------------------------------------------------------
! Future revisions:
!
!   (none planned)
!
! ----------------------------------------------------------------------------------------
USE nrtype                                               ! variable types (DP, I4B, etc.)
!USE ioinfo                                               ! I/O structures (filenames, etc.)
!USE simul_info                                           ! simulation attributes
!USE model_time                                           ! model time structure
!USE basin_flux                                           ! basin fluxes
!USE basinstate                                           ! basin state variables
USE reachvars                                           ! reach state variables (particles)
USE lakevars                                           ! lake states (level, area, volume)
!USE basinparam,ONLY: DPARAM                              ! sub-basin parameters
!USE assimparam,ONLY: assim_ran                           ! random number fields
!USE user_state
!USE userparam
!USE cutils,ONLY: get_rstart_fname                        ! interface to cutils c-library
USE netcdf                                               ! netCDF subroutines
IMPLICIT NONE
INTEGER(I4B)                           :: RSTART_GET
! Input variables
!INTEGER(I4B), INTENT(IN)            :: LAKEFLAG            ! if lake will be simulated
!INTEGER(I4B), INTENT(IN)               :: NENS           ! Number of ensemble members
!INTEGER(I4B), INTENT(IN)               :: NRCH           ! Number of basins
!INTEGER(I4B), INTENT(IN)               :: NLAK           ! Number of lakes
REAL(DP), INTENT(IN)                   :: DTIME          ! Time difference between restart file and model reference time
LOGICAL(LGT), INTENT(OUT)              :: LEXIST         ! .TRUE. if re-start file exists
LOGICAL(LGT), INTENT(IN), OPTIONAL     :: FEXIST         ! if .TRUE., only check for file existance
!REAL(DP), INTENT(IN)                   :: MAXQPAR
CHARACTER(LEN=LEN_PATH)           :: RESTART_FILE   ! name of re-start file
! Global NetCDF variables
INTEGER(I4B)                           :: IERR           ! NetCDF error code
INTEGER(I4B)                           :: NCID           ! file ID
INTEGER(I4B)                           :: IVARID         ! NetCDF variable ID
INTEGER(I4B)                           :: IDIMID         ! NetCDF dimension ID
!CHARACTER(LEN=LEN_FILE_PATH)           :: RPATH          ! path for re-start file
!CHARACTER(LEN=LEN_FILE_PATH)           :: RPATH_SRC      ! source path for re-start file
!CHARACTER(LEN=LEN_FILE_NAME)           :: PATTERN        ! pattern of re-start file
! Temporary vectors
INTEGER(I4B)                           :: IENS,IRCH,ILAK,IUSER ! (looping variables)
INTEGER(I4B), DIMENSION(NRCH)          :: NTDH           ! number time steps in timdel histogram
INTEGER(I4B), DIMENSION(1)             :: NTDH_1D        ! number time steps in timdel histogram
INTEGER(I4B)                           :: NPAR           ! number of flow particles
INTEGER(I4B), DIMENSION(1)             :: TMPINT         ! number of flow particles
INTEGER(I4B), DIMENSION(2)             :: IPOS           ! (iens,irch) index
INTEGER(I4B), DIMENSION(3)             :: ISTART         ! start position for data write
INTEGER(I4B), DIMENSION(3)             :: ICOUNT         ! number of values written
INTEGER(I4B), DIMENSION(2)             :: JSTART         ! start position for data write
INTEGER(I4B), DIMENSION(2)             :: JCOUNT         ! number of values written
REAL(SP),DIMENSION(1)                  :: TMPDAT         ! temporary data point
REAL(SP),DIMENSION(:,:,:),ALLOCATABLE  :: TMPMOD         ! temporary data array (model output)
REAL(DP),DIMENSION(:,:,:),ALLOCATABLE  :: TMPTIM         ! temporary data array (time)
LOGICAL(LGT),DIMENSION(:),ALLOCATABLE  :: TMPFLG         ! vector of routing flags
INTEGER(I1B),DIMENSION(:,:,:),ALLOCATABLE :: TMPBYT      ! convert to array of bytes
!INTEGER(I4B),DIMENSION(NRCH)           :: NBAND          ! number of elevation bands for sub-basins
INTEGER(I4B)                           :: MAXH           ! maximum number of steps in time delay histogram
INTEGER(I4B)                           :: IMAXH          ! intermediate number of steps in time delay histogram
INTEGER(I4B)                           :: MTDH           ! number of steps in current delay histogram
INTEGER(I4B)                           :: MAXP           ! maximum number of flow particles
INTEGER(I4B)                           :: MXELEBNDS      ! maximum number of elevation bands
!INTEGER(I4B)                           :: IY,IM,ID,IH,MM ! year, month, day, hour, minute
!INTEGER(I4B)                           :: JY,JM,JD,JH    ! year, month, day, hour, minute
!REAL(DP)                               :: SS             ! seconds
!CHARACTER(LEN=10)                      :: RSTCHAR        ! yyyymmddhh
!CHARACTER(LEN=10)                      :: STRDATE        ! yyyymmddhh - creation time for rstart file
!CHARACTER(LEN=10)                      :: ITIME2CHAR     ! FUNCTION to convert IY,IM,ID,IH to string
!CHARACTER(LEN=10)                      :: GET_RSTART_CTIME ! FUNCTION, get date string from filename
LOGICAL(LGT)                           :: FEXOPT         ! if .TRUE., only check for file existance
!INTEGER(I4B)                           :: NENSF          ! number of ensemble members in restart file
INTEGER(I4B)                           :: MAXPF          ! maximum number of particles in reaches
INTEGER(I4B)                           :: MXELEBNDSF     ! maximum number of elevation bands in reaches
LOGICAL(LGT)                           :: TMP_PATH       ! true if using temporary paths
!CHARACTER(LEN=1024)                    :: SYSCALL        ! system call string
!INTEGER(I4B)                           :: RSLT           ! result code from a system call
!INTEGER(I4B)                           :: SYSTEM         ! use the system function call
!INTEGER(I4B)                            :: NARGS       ! number of arguments from calling topnet to check if using a temporary folder
! this is necessary to set the calling conventions with the intel compiler on windows
!!DEC$ ATTRIBUTES DEFAULT               :: SYSTEM
! ----------------------------------------------------------------------------------------
RSTART_GET = 0
!IF (IO_INFO%DEBUG_FILE) WRITE(99,*) 'in rstart_get...'
! set the optional variable
IF (PRESENT(FEXIST)) THEN
  FEXOPT = FEXIST
ELSE
  FEXOPT = .FALSE.
END IF
! define the time string in the restart file (use the dtime argument)
!CALL UPDATETIME(DTIME,IY,IM,ID,IH,MM,SS)  ! convert DTIME to IY,IM,ID,IH,MM,SS
! convert IH to the nearest hour
!IH = NINT(REAL(IH,KIND(SS))+REAL(MM,KIND(SS))/60.0D0+SS/3600.0D0)
! get the character string of start of the first time step
!RSTCHAR = ITIME2CHAR(IY,IM,ID,IH)

! are we using non-default path? if so NARGS>1
!NARGS = COMMAND_ARGUMENT_COUNT()
! if so -- indicate that file need to be moved from the temporary ../running/topnet directory
! define path and filename the time step
!RPATH = TRIM(IO_INFO%TEMP_RUNNING_PATH)
!IF (NARGS.GT.1) RPATH_SRC = TRIM(RUNNING_DIR_PATH)
!IF (IO_INFO%OPERSYS(1:7).EQ.'windows') THEN
!  CALL OS_SLASH(RPATH)
!  CALL OS_SLASH(RPATH_SRC)
!END IF
!IF (IO_INFO%OUT_SUFFIX.EQ.'xx') THEN
! IF (IO_INFO%ECO_CONV) THEN 
! PATTERN = 'restart_'//TRIM(RSTCHAR)//'*_utc_'//TRIM(IO_INFO%MODEL_NAME)//'_'// &
!                  TRIM(IO_INFO%BASIN_ID)//'_'//TRIM(IO_INFO%AGG_LEVEL)//'.nc'
! ELSE
! PATTERN = 'restart_'//TRIM(RSTCHAR)//'*_utc_topnet_'// &
!                  TRIM(IO_INFO%BASIN_ID)//'_'//TRIM(IO_INFO%AGG_LEVEL)//'.nc'
! ENDIF
!ELSE
! IF (IO_INFO%ECO_CONV) THEN
! PATTERN = 'restart_'//TRIM(RSTCHAR)//'*_utc_'//TRIM(IO_INFO%MODEL_NAME)//'_'// &
!                  TRIM(IO_INFO%BASIN_ID)//'_'//TRIM(IO_INFO%AGG_LEVEL)//'-'//IO_INFO%OUT_SUFFIX//'.nc'
! ELSE
! PATTERN = 'restart_'//TRIM(RSTCHAR)//'*_utc_topnet_'// &
!                  TRIM(IO_INFO%BASIN_ID)//'_'//TRIM(IO_INFO%AGG_LEVEL)//'-'//IO_INFO%OUT_SUFFIX//'.nc'
! ENDIF
!ENDIF
! pad char(0) to make valid c-strings (get_rstart_fname.c is a c routine)
!CALL GET_RSTART_FNAME(TRIM(RPATH)//CHAR(0),TRIM(PATTERN)//CHAR(0),RESTART_FILE)
! check that the file exists
INQUIRE(FILE=TRIM(RESTART_FILE),EXIST=LEXIST)

!Temporary fix (P6 xlf90 AIX) issue: inquire returns LEXIST=true when ichar(restart_file)=18 whereas gnu on turbine returns ichar(restart_file)=10 and LEXIST=false
! TO DO: investigate why get_rstart_fname.c would return different values for restart_file depending on OS and/or compiler [Celine]
! if LEN_TRIM(restart_file)<=1 then the file does not exist and LEXIST=FALSE
IF (LEN_TRIM(RESTART_FILE).LE.1) LEXIST=.FALSE.

! move restart file from source path if using non-default path
IF (.NOT.LEXIST.OR.FEXOPT) then
    rstart_get = -1
    RETURN  ! re-start file does not exist (or fexopt=ture)
end if
! get creation time of restart file (second date in restart file name)
!STRDATE = GET_RSTART_CTIME(TRIM(RESTART_FILE))
!READ (STRDATE,'(I4,I2,I2,I2)') JY,JM,JD,JH
!IF (JY.NE.IY.OR.JM.NE.IM.OR.JD.NE.ID.OR.JH.NE.IH) THEN
!  PRINT *,'restart file (get): ',TRIM(RPATH)//TRIM(RESTART_FILE)
!  PRINT '(A,I4,A1,I2.2,A1,I2.2,1X,I2.2,A3)',' NOTE: using restart file with different creation time: ', &
!    JY,'-',JM,'-',JD,JH,':00'
!END IF
! open file
IERR = NF90_OPEN(TRIM(RESTART_FILE),NF90_NOWRITE,NCID); !CALL HANDLE_ERR90(IERR)
if(IERR.NE.NF90_NOERR)then
    rstart_get  = -1; return
end if
! get number of elevation bands per basin
!NBAND(:) = DPARAM(:)%PRBELEV%NBANDS
! get the number of ensembles in restart file and check if it matches the number
! of ensembles in the simulation
!IERR = NF90_INQ_DIMID(NCID,'nens',IDIMID); CALL HANDLE_ERR90(IERR)
!IERR = NF90_INQUIRE_DIMENSION(NCID,IDIMID,LEN=NENSF); CALL HANDLE_ERR90(IERR)
!IF (NENSF.NE.NENS) &
!  CALL EXIT_TOPNET(1,'wrong number of ensembles in restart file [rstart_get.f90]')
! allocate temporary data array
! get the maximum number of steps in the time delay histogram
!IF (SIMDAT%TDH_FLAG.GE.1) THEN  ! only if we're computing basin routing
!  JSTART = (/1,1/)
!  JCOUNT = (/1,NRCH/)
!  IERR = NF90_INQ_VARID(NCID,'numhist',IVARID); CALL HANDLE_ERR90(IERR)
!  IERR = NF90_GET_VAR(NCID,IVARID,NTDH,JSTART,JCOUNT); CALL HANDLE_ERR90(IERR)
!ELSE
!  NTDH(:) = 0
!END IF
! the model structure may not match restart file (depends on the value of the
! overland flow velocity when restart file was created and current value),
! therefore it is necessary to get the maximum value from the restart file
! (instead of the model structure)
!MAXH = MAXVAL(NTDH)
! get the maximum number of particles in reaches
MAXP = MAXQPAR
! check if model structure matches restart file
IERR = NF90_INQ_DIMID(NCID,'maxq',IDIMID); !CALL HANDLE_ERR90(IERR)
if(IERR.NE.NF90_NOERR)then
    rstart_get  = -1; return
end if
IERR = NF90_INQUIRE_DIMENSION(NCID,IDIMID,LEN=MAXPF); !CALL HANDLE_ERR90(IERR)
if(IERR.NE.NF90_NOERR)then
    rstart_get  = -1; return
end if
IF (MAXPF.NE.MAXP) then
  !CALL EXIT_TOPNET(1,'wrong maximum number of particles per reach in restart file [rstart_get.f90]')
  rstart_get = -1; return
end if
! get maximum number of elevation bands
! check if maximum number of elevation bands matches the value from the restart file
! allocate temporary arrays
ALLOCATE(TMPMOD(1,MAX(MXELEBNDS,MAXH,MAXP),1),TMPTIM(1,MAX(MXELEBNDS,MAXH,MAXP),1), &
         TMPFLG(MAXP),TMPBYT(1,MAXP,1),STAT=IERR)
IF (IERR.NE.0) then
!  CALL EXIT_TOPNET(1,' Problem allocating array [rstart_put.f90]')
  rstart_get = -1; return
end if
 ! loop over ensemble and reaches
! DO IENS=1,NENS
  IENS = 1
  DO IRCH=1,NRCH
   ! define position for VAR1 read
   IPOS = (/IENS,IRCH/)
   ! define position for VARA read
   !ISTART = (/IENS,          1,IRCH/)    ! starting position of array
   !ICOUNT = (/1   ,NBAND(IRCH),1   /)    ! number of array elements
   ! -------------------------------------------------------------------------------------
   ! (1) READ MODEL STATES
   ! -------------------------------------------------------------------------------------
!   IF(SIMDAT%SNOWFLAG.GT.0) THEN
    ! fractional snow covered area
    ! time since fresh snowfall
    ! snow water equivalent
    ! average snow temperature
    ! total snow accumulation
    ! total snow melt
!   ENDIF
   ! canopy storage
   ! soil moisture
   ! depth to the water table
   ! -------------------------------------------------------------------------------------
   ! (2) READ RUNOFF PLACED IN FUTURE TIME STEPS VIA THE TIME-DELAY HISTOGRAM
   ! -------------------------------------------------------------------------------------
   ! -------------------------------------------------------------------------------------
   ! (3) READ STREAMFLOW PARTICLES
   ! -------------------------------------------------------------------------------------
   ! read the number of streamflow particles
   IERR = NF90_INQ_VARID(NCID,'numqpar',IVARID); !CALL HANDLE_ERR90(IERR)
   if(IERR.NE.NF90_NOERR)then
    rstart_get  = -1; return
   end if
   IERR = NF90_GET_VAR(NCID,IVARID,TMPINT,IPOS); !CALL HANDLE_ERR90(IERR)
    if(IERR.NE.NF90_NOERR)then
        rstart_get  = -1; return
    end if   
   NPAR = TMPINT(1)
   IF (NPAR.GE.1) THEN
    ! allocate space for the routing structure
    ! deallocate the routing structure first
    IF (ASSOCIATED(KROUTE(IRCH)%KWAVE)) THEN
      DEALLOCATE(KROUTE(IRCH)%KWAVE,STAT=IERR)
    IF (IERR.NE.0) then
      !CALL EXIT_TOPNET(1,'problem deallocating array [rstart_get.f90]')
      rstart_get  = -1; return
    end if
    END IF
    ALLOCATE(KROUTE(IRCH)%KWAVE(0:NPAR-1),STAT=IERR)
    IF (IERR.NE.0) then
!      CALL EXIT_TOPNET(1,'problem allocating array [rstart_get.f90]')
        rstart_get  = -1; return
    end if
    ! define indices for VARA write (read all particles)
    ISTART = (/IENS,  1, IRCH/)              ! starting position of array
    ICOUNT = (/   1,NPAR,   1/)              ! number of array elements (all basins, 1 timestep)
    ! read the flow particles
    IERR = NF90_INQ_VARID(NCID,'q_ratep',IVARID); !CALL HANDLE_ERR90(IERR)
    if(IERR.NE.NF90_NOERR)then
        rstart_get  = -1; return
    end if
    IERR = NF90_GET_VAR(NCID,IVARID,TMPMOD,ISTART,ICOUNT); !CALL HANDLE_ERR90(IERR)
    if(IERR.NE.NF90_NOERR)then
        rstart_get  = -1; return
    end if    
    KROUTE(IRCH)%KWAVE(0:NPAR-1)%QF = REAL(TMPMOD(1,1:NPAR,1),KIND(DP))
    ! read the modified flow particles (if needed)
!    IF (NUSER.GT.0.AND.SIMDAT%UCFFLAG.GE.1) THEN
!      IERR = NF90_INQ_VARID(NCID,'qm_ratep',IVARID); CALL HANDLE_ERR90(IERR)
!      IERR = NF90_GET_VAR(NCID,IVARID,TMPMOD,ISTART,ICOUNT); CALL HANDLE_ERR90(IERR)
!      KROUTE(IENS,IRCH)%KWAVE(0:NPAR-1)%QM = REAL(TMPMOD(1,1:NPAR,1),KIND(DP))
!    END IF
    ! read the entry time -- NOTE: use of DTIME to convert to seconds since model
    ! reference time
    IERR = NF90_INQ_VARID(NCID,'tentryp',IVARID); !CALL HANDLE_ERR90(IERR)
    if(IERR.NE.NF90_NOERR)then
        rstart_get  = -1; return
    end if    
    IERR = NF90_GET_VAR(NCID,IVARID,TMPTIM,ISTART,ICOUNT); !CALL HANDLE_ERR90(IERR)
    if(IERR.NE.NF90_NOERR)then
        rstart_get  = -1; return
    end if    
    KROUTE(IRCH)%KWAVE(0:NPAR-1)%TI = TMPTIM(1,1:NPAR,1) + DTIME
    ! read the exit time -- NOTE: use of DTIME to convert to seconds since model
    ! reference time
    IERR = NF90_INQ_VARID(NCID,'t_exitp',IVARID); !CALL HANDLE_ERR90(IERR)
    if(IERR.NE.NF90_NOERR)then
        rstart_get  = -1; return
    end if    
    IERR = NF90_GET_VAR(NCID,IVARID,TMPTIM,ISTART,ICOUNT); !CALL HANDLE_ERR90(IERR)
    if(IERR.NE.NF90_NOERR)then
        rstart_get  = -1; return
    end if    
    KROUTE(IRCH)%KWAVE(0:NPAR-1)%TR = TMPTIM(1,1:NPAR,1) + DTIME
    ! read the routing flag
    IERR = NF90_INQ_VARID(NCID,'q_flagp',IVARID); !CALL HANDLE_ERR90(IERR)
    if(IERR.NE.NF90_NOERR)then
        rstart_get  = -1; return
    end if    
    IERR = NF90_GET_VAR(NCID,IVARID,TMPBYT,ISTART,ICOUNT); !CALL HANDLE_ERR90(IERR)
    if(IERR.NE.NF90_NOERR)then
        rstart_get  = -1; return
    end if    
    TMPFLG(1:NPAR) = .FALSE.; WHERE(TMPBYT(1,1:NPAR,1).EQ.1) TMPFLG(1:NPAR)=.TRUE. ! convert to characters
    KROUTE(IRCH)%KWAVE(0:NPAR-1)%RF = TMPFLG(1:NPAR) ! get logical flags
   ENDIF   ! (only read if there are particles)
   ! --------------------------------------------------------------------------------------
   ! (4) WRITE RANDOM FIELDS
   ! --------------------------------------------------------------------------------------
  ! --------------------------------------------------------------------------------------
  ! (5) READ USER STATES
  ! --------------------------------------------------------------------------------------
  ! --------------------------------------------------------------------------------------
  ! (6) READ LAKE STATES
  ! --------------------------------------------------------------------------------------
  IF (NLAK.GT.0.AND.LAKEFLAG.GE.1) THEN
   DO ILAK=1,NLAK
    ! define position for VAR1 read
    IPOS = (/IENS,ILAK/)
    ! read lake level
    IERR = NF90_INQ_VARID(NCID,'lakelvl',IVARID); !CALL HANDLE_ERR90(IERR)
    if(IERR.NE.NF90_NOERR)then
        rstart_get  = -1; return
    end if        
    IERR = NF90_GET_VAR(NCID,IVARID,TMPDAT,IPOS); !CALL HANDLE_ERR90(IERR)
    if(IERR.NE.NF90_NOERR)then
        rstart_get  = -1; return
    end if        
    LSTATE(ILAK)%LAKELVL = REAL(TMPDAT(1),KIND(DP))
    ! read lake area
    IERR = NF90_INQ_VARID(NCID,'lakeare',IVARID); !CALL HANDLE_ERR90(IERR)
    if(IERR.NE.NF90_NOERR)then
        rstart_get  = -1; return
    end if        
    IERR = NF90_GET_VAR(NCID,IVARID,TMPDAT,IPOS); !CALL HANDLE_ERR90(IERR)
    if(IERR.NE.NF90_NOERR)then
        rstart_get  = -1; return
    end if        
    LSTATE(ILAK)%LAKEARE = REAL(TMPDAT(1),KIND(DP))
    ! read lake volume
    IERR = NF90_INQ_VARID(NCID,'lakevol',IVARID); !CALL HANDLE_ERR90(IERR)
    if(IERR.NE.NF90_NOERR)then
        rstart_get  = -1; return
    end if    
    IERR = NF90_GET_VAR(NCID,IVARID,TMPDAT,IPOS);! CALL HANDLE_ERR90(IERR)
    if(IERR.NE.NF90_NOERR)then
        rstart_get  = -1; return
    end if    
    LSTATE(ILAK)%LAKEVOL = REAL(TMPDAT(1),KIND(DP))
   END DO  ! loop through lakes
  ENDIF   ! (if lakes exist)
  ! --------------------------------------------------------------------------------------
 END DO  ! iens
! close the NetCDF file
IERR = NF90_CLOSE(NCID); !CALL HANDLE_ERR90(IERR)
if(IERR.NE.NF90_NOERR)then
    rstart_get  = -1; return
end if    
DEALLOCATE(TMPMOD,TMPTIM,TMPFLG,TMPBYT,STAT=IERR)
IF (IERR.NE.0) then
    rstart_get  = -1; 
!  CALL EXIT_TOPNET(1,' Problem deallocating array [rstart_put.f90]')
end if
! ----------------------------------------------------------------------------------------
!IF (IO_INFO%DEBUG_FILE) FLUSH(99)
! ----------------------------------------------------------------------------------------
END function RSTART_GET
