! $Id: namelist_array_mod.f, v1.0

!******************************************************************************
!  Module NAMELIST_ARRAY_MOD contains all the variables that read from 
!  namelist.ini by namelist_mod.f90 
!******************************************************************************
   MODULE NAMELIST_ARRAY_MOD
   
   ! Make everything PBLIC ...
   PUBLIC   

   CHARACTER(LEN=10), PARAMETER :: codeVersion = '1.0.0'
   
   ! CONTROL MENU
   CHARACTER(LEN=255)  :: DIR_RUN
   CHARACTER(LEN=255)  :: DIR_DATA
   CHARACTER(LEN=255)  :: DIR_OUTPUT
   
   ! RADIATION MENU
   LOGICAL             :: LFREQUENCY
   INTEGER             :: NSPECTRA_LST
   REAL                :: SPECTRA_LST(99)
   REAL                :: SPECTRA_STEP
   INTEGER             :: NRH_LST 
   REAL                :: RH_LST(99)
   
   ! AEROSOL
   LOGICAL			   :: LEXTERNAL_MIX
   LOGICAL			   :: LINTERNAL_MIX
   LOGICAL             :: LSULFATE_SHELL
   LOGICAL             :: LOC_SHELL
   
!    REAL                :: MASS_RATIO(2)
   REAL                :: BC2SULF_MASS(99)
   INTEGER             :: NBC2SULF_MASS
   REAL                :: BC2OC_MASS(99)
   INTEGER             :: NBC2OC_MASS   
   REAL                :: DUST2ALL_MASS(99)
   INTEGER             :: NDUST2ALL_MASS
   REAL                :: BCRHO
   REAL                :: OCRHO
   REAL                :: DUSTRHO
   REAL                :: RMRI(2,4)
   INTEGER             :: NSULF
   INTEGER             :: SULFID_LST(99)
   REAL                :: SULF_MASSRATIO_LST(99)
   INTEGER             :: NSULFMASSRATIO
   REAL                :: DISTPAR(4,4)
   
   
   ! DIAGNOSTIC MENU
   LOGICAL			   :: LDIAG
   CHARACTER(LEN=255)  :: DIAG_PREFIX
   LOGICAL             :: LPRT = .TRUE.
   
   END MODULE NAMELIST_ARRAY_MOD
   
   
   
   