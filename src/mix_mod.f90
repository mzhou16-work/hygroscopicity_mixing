! $Id: mix_mod.f, v1.0
   MODULE MIX_MOD
   
!******************************************************************************
!  Module MIX_MOD calculate aerosol optical properties in different mixing status
!
!  Module Routines
!  ============================================================================
!  (1 ) GET_OPTICAL
!  (2 ) MIXING
!  (3 ) MIX_INTERNAL
!  (4 ) MIX_INTERNAL
!  (5 ) SIZE2NUM
!  (5 ) GET_MIX_DENSITY
!  (5 ) GEN_POLYNOMIAL

!******************************************************************************    
   
   IMPLICIT NONE
   
   !=================================================================
   ! MODULE PRIVATE DECLARATIONS -- keep certain internal variables 
   ! and routines from being seen outside "mix_mod.f"
   !=================================================================

   ! Make everything PRIVATE ...
   PRIVATE
   
   
   ! ... except these routines
   PUBLIC  :: GET_OPTICAL
   PUBLIC  :: MIX_INTERNAL
   PUBLIC  :: MIX_EXTERNAL
   PUBLIC  :: MIXING
   PUBLIC  :: GET_MIX_DENSITY
   PUBLIC  :: GEN_POLYNOMIAL
 
   
   ! ... and these variables
   PUBLIC  :: BCID
   PUBLIC  :: OCID
   PUBLIC  :: DUSTID
   PUBLIC  :: RHID
   PUBLIC  :: LAMDAID
   
   PUBLIC  :: NANG
   PUBLIC  :: NLGNDR
   PUBLIC  :: OPT_SULF
   PUBLIC  :: OPT_BC
   PUBLIC  :: OPT_OC
   PUBLIC  :: OPT_DUST
   PUBLIC  :: OPT_MIX
   PUBLIC  :: VBC
   PUBLIC  :: SULFBCRRATIO
   PUBLIC  :: SULFBCGF
   PUBLIC  :: SULFBCMGF
   PUBLIC  :: SULFBCRHO
 
   PUBLIC  :: CSMIXINGCEXT
   PUBLIC  :: CSMIXINGSSA
   PUBLIC  :: CSMIXINGREFF
   PUBLIC  :: CSMIXINGPHASE
   PUBLIC  :: CSMIXINGLGNDR
   PUBLIC  :: CSTOTALNUM
   
   PUBLIC  :: MIXINGCEXT
   PUBLIC  :: MIXINGSSA
   PUBLIC  :: MIXINGREFF
   PUBLIC  :: MIXINGPHASE
   PUBLIC  :: MIXINGLGNDR
   PUBLIC  :: TOTALNUM 
            
   ! ... module variable  
   INTEGER, PARAMETER :: NANG   = 1025
   INTEGER, PARAMETER :: NLGNDR = 32
   REAL   , PARAMETER :: PI = 3.14159265357
   
   REAL              :: CSMIXINGCEXT
   REAL              :: CSMIXINGSSA
   REAL              :: CSMIXINGREFF
   REAL              :: CSMIXINGPHASE(NANG)
   REAL              :: CSMIXINGLGNDR(0:NLGNDR)
   REAL              :: CSTOTALNUM 

   REAL              :: MIXINGCEXT
   REAL              :: MIXINGSSA
   REAL              :: MIXINGREFF
   REAL              :: MIXINGPHASE(NANG)
   REAL              :: MIXINGLGNDR(0:NLGNDR)
   REAL              :: TOTALNUM 
 
   TYPE OPTICAL_GRID
      REAL    ::   REFF    
      REAL    ::   VEFF
      REAL    ::   AREA
      REAL    ::   VOLUME
      REAL    ::   SSA
      REAL    ::   ASY    
      REAL    ::   CEXT
      REAL    ::   CSCA
      REAL    ::   MEXTEFFCY
      REAL    ::   EXTEFFCY
      REAL    ::   HBSCAT
      REAL    ::   EXTTOBCARATIO 
      REAL    ::   PHASE(NANG)
      REAL    ::   ANGLE(NANG)
      REAL    ::   LGNDR(0:NLGNDR)
   ENDTYPE OPTICAL_GRID
   
   TYPE (OPTICAL_GRID)   :: OPT_SULF
   TYPE (OPTICAL_GRID)   :: OPT_BC
   TYPE (OPTICAL_GRID)   :: OPT_OC
   TYPE (OPTICAL_GRID)   :: OPT_DUST
   TYPE (OPTICAL_GRID)   :: OPT_MIX
   
   INTEGER               :: BCID
   INTEGER               :: OCID
   INTEGER               :: DUSTID
   INTEGER               :: RHID
   INTEGER               :: LAMDAID
      
   REAL           :: NUMSULF
   REAL           :: MOM2SULF
   REAL           :: MOM3SULF
   REAL           :: NUMBC
   REAL           :: MOM2BC
   REAL           :: MOM3BC
   REAL           :: NUMOC
   REAL           :: MOM2OC
   REAL           :: MOM3OC
   REAL           :: NUMDUST
   REAL           :: MOM2DUST
   REAL           :: MOM3DUST
   REAL, ALLOCATABLE    :: QMU(:)
   REAL, ALLOCATABLE    :: QWT(:) 
   REAL, ALLOCATABLE    :: PLGNDR(:, :)
   REAL, ALLOCATABLE    :: PK(:)
   
   REAL, ALLOCATABLE    :: VBC(:)
   REAL, ALLOCATABLE    :: VOC(:)
   REAL, ALLOCATABLE    :: OCBCRRATIO(:)
   REAL, ALLOCATABLE    :: OCBCGF(:)
   REAL, ALLOCATABLE    :: OCBCRHO(:)

   REAL, ALLOCATABLE    :: SULFBCRRATIO(:, :)
   REAL, ALLOCATABLE    :: SULFBCGF(:, :)
   REAL, ALLOCATABLE    :: SULFBCMGF(:, :)
   REAL, ALLOCATABLE    :: SULFBCRHO(:, :)
    
   !=================================================================
   ! MODULE ROUTINES -- follow below the "CONTAINS" statement 
   !=================================================================
   CONTAINS
   
   SUBROUTINE GET_OPTICAL
    
   !=================================================================
   ! GET_OPTICAL Calculate optical properties of BC, sulfate and dust
   !=================================================================

   !
   ! References to F90 modules      
   USE NAMELIST_ARRAY_MOD, ONLY : DISTPAR      ! size distribution of BC and dust
   USE NAMELIST_ARRAY_MOD, ONLY : RMRI         ! Refractive indices of BC and dust
   USE NAMELIST_ARRAY_MOD, ONLY : BCRHO        ! density of BC
   USE NAMELIST_ARRAY_MOD, ONLY : OCRHO        ! density of OC
   USE NAMELIST_ARRAY_MOD, ONLY : DUSTRHO      ! density of dust
   USE SPECTRA_MOD,        ONLY : NSPECTRA     ! number of wavelength
   USE SPECTRA_MOD,        ONLY : LAMDAS       ! wavelength
   USE SULFATE_MOD,        ONLY : COMPOSITENR  ! Refractive indices of sulfate
   USE SULFATE_MOD,        ONLY : AVGGROWTH    ! average growth factor
   USE SULFATE_MOD,        ONLY : AVGWETSULFRHO! average wet growth factor for sulfate density

   !       INTEGER           :: IDLAMDA
   !       INTEGER           :: IDRH
   REAL              :: CURRENTLAMDA

   CURRENTLAMDA = LAMDAS(LAMDAID) / 1000.0
   
   ! Calculate Sulfate Mie
   WRITE (6, 100), '  ' 
   WRITE (6, 100), '  - GET_OPTICAL: Calculate optical properties of sulfate...'
   CALL SULF_STDMIE( DISTPAR(1,1)*AVGGROWTH(RHID), DISTPAR(2,1),   & 
                     DISTPAR(3,1), DISTPAR(4,1),                   &
                     COMPOSITENR(LAMDAID, RHID), 0.0,              & 
                     AVGWETSULFRHO(RHID), CURRENTLAMDA,            &
                     OPT_SULF%REFF,       OPT_SULF%VEFF,           &
                     OPT_SULF%AREA,       OPT_SULF%VOLUME,         &
                     OPT_SULF%SSA,        OPT_SULF%ASY,            &
                     OPT_SULF%CEXT,       OPT_SULF%CSCA,           & 
                     OPT_SULF%MEXTEFFCY,  OPT_SULF%EXTEFFCY,       &
                     OPT_SULF%HBSCAT,     OPT_SULF%EXTTOBCARATIO,  &
                     OPT_SULF%PHASE,      OPT_SULF%ANGLE,          &
                     OPT_SULF%LGNDR)
   WRITE (6, 110), '  Reff     : ', OPT_SULF%REFF      , ', Veff          : ', OPT_SULF%VEFF
   WRITE (6, 110), '  Area     : ', OPT_SULF%AREA      , ', Volm          : ', OPT_SULF%VOLUME
   WRITE (6, 110), '  SSA      : ', OPT_SULF%SSA       , ', ASY           : ', OPT_SULF%ASY
   WRITE (6, 110), '  Cext     : ', OPT_SULF%CEXT      , ', Csca          : ', OPT_SULF%CSCA
   WRITE (6, 110), '  MEXTEFFCY: ', OPT_SULF%MEXTEFFCY , ', EXTEFFCY      : ', OPT_SULF%EXTEFFCY
   WRITE (6, 110), '  HBSCAT   : ', OPT_SULF%HBSCAT    , ', EXTTOBCARATIO : ', OPT_SULF%EXTTOBCARATIO

   ! Calculate BC Mie
   WRITE (6, 100), '  ' 
   WRITE (6, 100), '  - GET_OPTICAL: Calculate optical properties of BC...'
   CALL SULF_STDMIE( DISTPAR(1,2), DISTPAR(2,2),                  & 
				     DISTPAR(3,2), DISTPAR(4,2),                  &
				     RMRI(1, 2),  RMRI(2, 2),                     &
				     BCRHO,  CURRENTLAMDA,                        & 
				     OPT_BC%REFF,       OPT_BC%VEFF,              &
				     OPT_BC%AREA,       OPT_BC%VOLUME,            &
				     OPT_BC%SSA,        OPT_BC%ASY,               &
				     OPT_BC%CEXT,       OPT_BC%CSCA,              & 
				     OPT_BC%MEXTEFFCY,  OPT_BC%EXTEFFCY,          &
				     OPT_BC%HBScat,     OPT_BC%EXTToBCARatio,     &
				     OPT_BC%PHASE,      OPT_BC%ANGLE,             &
				     OPT_BC%LGNDR)
   WRITE (6, 110), '  Reff: ', OPT_BC%REFF , ', Veff: ', OPT_BC%VEFF
   WRITE (6, 110), '  Area: ', OPT_BC%AREA , ', Volm: ', OPT_BC%VOLUME
   WRITE (6, 110), '  SSA : ', OPT_BC%SSA  , ', ASY : ', OPT_BC%ASY
   WRITE (6, 110), '  Cext: ', OPT_BC%CEXT,  ', Csca: ', OPT_BC%CSCA
   WRITE (6, 110), '  MEXTEFFCY: ', OPT_BC%MEXTEFFCY , ', EXTEFFCY : ', OPT_BC%EXTEFFCY
   WRITE (6, 110), '  HBSCAT: ', OPT_BC%HBSCAT , ', EXTTOBCARATIO : ', OPT_BC%EXTTOBCARATIO
   
   ! Calculate OC Mie
   WRITE (6, 100), '  ' 
   WRITE (6, 100), '  - GET_OPTICAL: Calculate optical properties of OC...'
   CALL SULF_STDMIE( DISTPAR(1,3), DISTPAR(2,3),                  & 
				     DISTPAR(3,3), DISTPAR(4,3),                  &
				     RMRI(1, 3),  RMRI(2, 3),                     &
				     OCRHO,  CURRENTLAMDA,                        & 
				     OPT_OC%REFF,       OPT_OC%VEFF,              &
				     OPT_OC%AREA,       OPT_OC%VOLUME,            &
				     OPT_OC%SSA,        OPT_OC%ASY,               &
				     OPT_OC%CEXT,       OPT_OC%CSCA,              & 
				     OPT_OC%MEXTEFFCY,  OPT_OC%EXTEFFCY,          &
				     OPT_OC%HBScat,     OPT_OC%EXTToBCARatio,     &
				     OPT_OC%PHASE,      OPT_OC%ANGLE,             &
				     OPT_OC%LGNDR)
   WRITE (6, 110), '  Reff: ', OPT_OC%REFF , ', Veff: ', OPT_OC%VEFF
   WRITE (6, 110), '  Area: ', OPT_OC%AREA , ', Volm: ', OPT_OC%VOLUME
   WRITE (6, 110), '  SSA : ', OPT_OC%SSA  , ', ASY : ', OPT_OC%ASY
   WRITE (6, 110), '  Cext: ', OPT_OC%CEXT,  ', Csca: ', OPT_OC%CSCA
   WRITE (6, 110), '  MEXTEFFCY: ', OPT_OC%MEXTEFFCY , ', EXTEFFCY : ', OPT_OC%EXTEFFCY
   WRITE (6, 110), '  HBSCAT: ', OPT_OC%HBSCAT , ', EXTTOBCARATIO : ', OPT_OC%EXTTOBCARATIO
					 
   ! Calculate DUST Mie
   WRITE (6, 100), '  ' 
   WRITE (6, 100), '  - GET_OPTICAL: Calculate optical properties of dust...'
   CALL SULF_STDMIE( DISTPAR(1,4), DISTPAR(2,4),                   & 
				     DISTPAR(3,4), DISTPAR(4,4),                   &
				     RMRI(1, 4),  RMRI(2, 4),                      &
				     DUSTRHO,  CURRENTLAMDA,                       & 
				     OPT_DUST%REFF,       OPT_DUST%VEFF,           &
				     OPT_DUST%AREA,       OPT_DUST%VOLUME,         &
				     OPT_DUST%SSA,        OPT_DUST%ASY,            &
				     OPT_DUST%CEXT,       OPT_DUST%CSCA,           & 
				     OPT_DUST%MEXTEFFCY,  OPT_DUST%EXTEFFCY,       &
				     OPT_DUST%HBScat,     OPT_DUST%EXTToBCARatio,  &
				     OPT_DUST%PHASE,      OPT_DUST%ANGLE,          &
				     OPT_DUST%LGNDR) 
   WRITE (6, 110), '  Reff: ', OPT_DUST%REFF , ', Veff: ', OPT_DUST%VEFF
   WRITE (6, 110), '  Area: ', OPT_DUST%AREA , ', Volm: ', OPT_DUST%VOLUME
   WRITE (6, 110), '  SSA : ', OPT_DUST%SSA  , ', ASY : ', OPT_DUST%ASY
   WRITE (6, 110), '  Cext: ', OPT_DUST%CEXT,  ', Csca: ', OPT_DUST%CSCA
   WRITE (6, 110), '  MEXTEFFCY: ', OPT_DUST%MEXTEFFCY , ', EXTEFFCY : ', OPT_DUST%EXTEFFCY
   WRITE (6, 110), '  HBSCAT: ', OPT_DUST%HBSCAT , ', EXTTOBCARATIO : ', OPT_DUST%EXTTOBCARATIO

   100 FORMAT (A, A)
   110 FORMAT (3X, A, F12.3, A, F12.3)
         
   END SUBROUTINE GET_OPTICAL
   
!------------------------------------------------------------------------------   
   
   SUBROUTINE MIXING
   !=================================================================
   ! MIXING determine the calling of external and internal mixing state
   ! calculation based on the input 
   ! 
   !=================================================================   
   ! References to F90 modules      
   USE NAMELIST_ARRAY_MOD, ONLY : DISTPAR ! size distribution of BC and dust
   USE NAMELIST_ARRAY_MOD, ONLY : NBC2SULF_MASS
   USE NAMELIST_ARRAY_MOD, ONLY : NBC2OC_MASS
   USE NAMELIST_ARRAY_MOD, ONLY : BCRHO
   USE NAMELIST_ARRAY_MOD, ONLY : OCRHO
   USE NAMELIST_ARRAY_MOD, ONLY : DUSTRHO
   USE NAMELIST_ARRAY_MOD, ONLY : BC2OC_MASS
   USE NAMELIST_ARRAY_MOD, ONLY : BC2SULF_MASS
   USE NAMELIST_ARRAY_MOD, ONLY : DUST2ALL_MASS
   USE NAMELIST_ARRAY_MOD, ONLY : SULF_MASSRATIO_LST
   USE NAMELIST_ARRAY_MOD, ONLY : NSULF
   USE NAMELIST_ARRAY_MOD, ONLY : LSULFATE_SHELL
   USE NAMELIST_ARRAY_MOD, ONLY : LOC_SHELL   
   USE NAMELIST_ARRAY_MOD, ONLY : LEXTERNAL_MIX
   USE NAMELIST_ARRAY_MOD, ONLY : LINTERNAL_MIX
   USE SULFATE_MOD       , ONLY : DRYRHO
   USE SULFATE_MOD       , ONLY : AVGDRYSULFRHO
   USE SULFATE_MOD       , ONLY : TOTALDRYSULFMASS
   
   REAL           :: DUSTMASSRATIO
   REAL           :: TOTALDRYMASS
   INTEGER        :: I
   
   !!! If sulfate is shell
   IF (LSULFATE_SHELL) THEN
   
   ! I think we may simplify the mass of sulfate to 1
   ! here we get the dust mass
   DUSTMASSRATIO = DUST2ALL_MASS(DUSTID)/(1-DUST2ALL_MASS(DUSTID))* &
                   ( BC2SULF_MASS(BCID) + SUM(SULF_MASSRATIO_LST(1: NSULF)) ) 
   
   ! Total dry mass (BC + dust + sulfate)
   TOTALDRYMASS = BC2SULF_MASS(BCID) + DUSTMASSRATIO + SUM(SULF_MASSRATIO_LST(1: NSULF))
   
   ! sulfate
   CALL SIZE2NUM(DISTPAR(1, 1), DISTPAR(2, 1), AVGDRYSULFRHO, TOTALDRYSULFMASS, 'Sulfate', NUMSULF, MOM2SULF, MOM3SULF)
   
   ! BC
   CALL SIZE2NUM(DISTPAR(1, 2), DISTPAR(2, 2), BCRHO, BC2SULF_MASS(BCID), 'BC', NUMBC, MOM2BC, MOM3BC)
   
   ! Dust
   CALL SIZE2NUM(DISTPAR(1, 4), DISTPAR(2, 4), DUSTRHO, DUSTMASSRATIO, 'Dust', NUMDUST, MOM2DUST, MOM3DUST)

   IF (LINTERNAL_MIX) THEN
      CALL MIX_INTERNAL
   ENDIF

   IF (LEXTERNAL_MIX) THEN
      CALL MIX_EXTERNAL
   ENDIF   
      
   !!! If OC is shell
   ELSEIF (LOC_SHELL) THEN
   
   ! I think we may simplify the mass of OC to 1
   ! here we get the dust mass
   DUSTMASSRATIO = DUST2ALL_MASS(DUSTID)/(1-DUST2ALL_MASS(DUSTID))* &
                   ( BC2OC_MASS(BCID) + 1 ) 
   
   ! Total dry mass (BC + dust + OC)
   TOTALDRYMASS = BC2OC_MASS(BCID) + DUSTMASSRATIO + 1
   
   ! OC
   CALL SIZE2NUM(DISTPAR(1, 3), DISTPAR(2, 3), OCRHO, 1., 'OC', NUMOC, MOM2OC, MOM3OC)
   
   ! BC
   CALL SIZE2NUM(DISTPAR(1, 2), DISTPAR(2, 2), BCRHO, BC2OC_MASS(BCID), 'BC', NUMBC, MOM2BC, MOM3BC)
   
   ! Dust
   CALL SIZE2NUM(DISTPAR(1, 4), DISTPAR(2, 4), DUSTRHO, DUSTMASSRATIO, 'Dust', NUMDUST, MOM2DUST, MOM3DUST)

   IF (LINTERNAL_MIX) THEN
      CALL MIX_INTERNAL
   ENDIF

   IF (LEXTERNAL_MIX) THEN
      CALL MIX_EXTERNAL
   ENDIF   
   
   ENDIF   
   
   END SUBROUTINE MIXING
!------------------------------------------------------------------------------   
   
   SUBROUTINE MIX_INTERNAL
   !=================================================================
   ! MIX_INTERNAL calculate the optical property for the internal mixing
   ! state
   !=================================================================      
   USE NAMELIST_ARRAY_MOD, ONLY : DISTPAR
   USE NAMELIST_ARRAY_MOD, ONLY : RMRI
   USE NAMELIST_ARRAY_MOD, ONLY : BCRHO
   USE NAMELIST_ARRAY_MOD, ONLY : OCRHO
   USE NAMELIST_ARRAY_MOD, ONLY : DUSTRHO
   USE NAMELIST_ARRAY_MOD, ONLY : LSULFATE_SHELL
   USE NAMELIST_ARRAY_MOD, ONLY : LOC_SHELL   
   USE SPECTRA_MOD,        ONLY : NSPECTRA
   USE SPECTRA_MOD,        ONLY : LAMDAS
   USE SULFATE_MOD,        ONLY : COMPOSITENR
   USE SULFATE_MOD,        ONLY : AVGGROWTH
   USE SULFATE_MOD,        ONLY : AVGWETSULFRHO

       
   REAL              :: CURRENTLAMDA
   REAL              :: TMPSULFBCRRATIO
   REAL              :: TMPOCBCRRATIO

   REAL              :: ceflgn
   INTEGER           :: I
   
   CURRENTLAMDA = LAMDAS(LAMDAID) / 1000.0
   
   ! if sulfate is shell
   IF (LSULFATE_SHELL) THEN
   
   TMPSULFBCRRATIO = SULFBCRRATIO(BCID, RHID)
   IF ( TMPSULFBCRRATIO .lt. 0.0001 ) THEN
      TMPSULFBCRRATIO = 0.0001
   ENDIF
   
   WRITE (6, 100), '  ' 
   WRITE (6, 100), '  - MIX_INTERNAL: Calculate optical properties of internal mixing (BC/Sulfate)...'
   CALL CORE_SHELL(CURRENTLAMDA, NANG, NLGNDR,                         &
                   DISTPAR(1,1)* SULFBCGF(BCID, RHID), DISTPAR(2,1),   & 
                   COMPOSITENR(LAMDAID, RHID), 0.0,                    &
                   RMRI(1, 2),  RMRI(2, 2),                            &
                   TMPSULFBCRRATIO,  SULFBCRHO(BCID, RHID),            &
                   OPT_MIX%REFF,       OPT_MIX%VEFF,                   &
                   OPT_MIX%AREA,       OPT_MIX%VOLUME,                 &
                   OPT_MIX%SSA,        OPT_MIX%ASY,                    &
                   OPT_MIX%CEXT,       OPT_MIX%CSCA,                   & 
                   OPT_MIX%MEXTEFFCY,  OPT_MIX%EXTEFFCY,               &
                   OPT_MIX%HBSCAT,     OPT_MIX%EXTTOBCARATIO,          &
                   OPT_MIX%PHASE,      OPT_MIX%ANGLE,                  &
                   OPT_MIX%LGNDR)                   
   WRITE (6, 110), '  CURRENTLAMDA     : ', CURRENTLAMDA      , ', NANG          : ', NANG
   WRITE (6, 110), '  Rg     : ', DISTPAR(1,1)* SULFBCGF(BCID, RHID)      , ', Sg         : ', DISTPAR(2,1)
   WRITE (6, 110), '  COMPOSITENR     : ', COMPOSITENR      , ', Sg         : ', DISTPAR(2,1)
   WRITE (6, 110), '  RMRI(1, 2)     : ', RMRI(1, 2)      , ', RMRI(1, 2)          : ', RMRI(2, 2)
   WRITE (6, 110), '  TMPSULFBCRRATIO     : ', TMPSULFBCRRATIO      , ', SULFBCRHO         : ', SULFBCRHO(BCID, RHID)
   print *, '--------'
   WRITE (6, 110), '  Sulf Reff: ', DISTPAR(1,1)* SULFBCGF(BCID, RHID), ', Sulf Veff     : ',  DISTPAR(2,1)
   WRITE (6, 110), '  Reff     : ', OPT_MIX%REFF      , ', Veff          : ', OPT_MIX%VEFF
   WRITE (6, 110), '  Area     : ', OPT_MIX%AREA      , ', Volm          : ', OPT_MIX%VOLUME
   WRITE (6, 110), '  SSA      : ', OPT_MIX%SSA       , ', ASY           : ', OPT_MIX%ASY
   WRITE (6, 110), '  Cext     : ', OPT_MIX%CEXT      , ', Csca          : ', OPT_MIX%CSCA
   WRITE (6, 110), '  MEXTEFFCY: ', OPT_MIX%MEXTEFFCY , ', EXTEFFCY      : ', OPT_MIX%EXTEFFCY
   WRITE (6, 110), '  HBSCAT   : ', OPT_MIX%HBSCAT    , ', EXTTOBCARATIO : ', OPT_MIX%EXTTOBCARATIO


   100 FORMAT (A, A)
   110 FORMAT (3X, A, F12.5, A, F12.5)

   ! external mixing with dust
   CSTOTALNUM = NUMSULF + NUMDUST
   CSMIXINGCEXT = (OPT_MIX%CEXT * NUMSULF + OPT_DUST%CEXT * NUMDUST) / &
                  CSTOTALNUM
   
   CSMIXINGSSA = (OPT_DUST%CEXT * NUMDUST * OPT_DUST%SSA + &
                  OPT_MIX%CEXT * NUMSULF * OPT_MIX%SSA)  / &
                 (CSMIXINGCEXT * CSTOTALNUM)
   
   CSMIXINGPHASE(1:NANG) = ( OPT_MIX%CSCA  * NUMSULF * OPT_MIX%PHASE(1:NANG)    +    & 
                             OPT_DUST%CSCA * NUMDUST * OPT_DUST%PHASE(1:NANG) ) /    & 
                           ( OPT_DUST%CSCA * NUMDUST + OPT_MIX%CSCA * NUMSULF)

   CSMIXINGREFF = (NUMDUST * MOM3DUST + NUMSULF * MOM3SULF * SULFBCGF(BCID, RHID)**3) / &
                  (NUMDUST * MOM2DUST + NUMSULF * MOM2SULF * SULFBCGF(BCID, RHID)**2) 

   DO I = 0, NLGNDR 
      CSMIXINGLGNDR(I) = ceflgn( NANG, CSMIXINGPHASE, QWT, I, PLGNDR(1,I) )
   ENDDO 

   WRITE (6, 100), '  - MIX_INTERNAL: Then external mixing with dust...'
   WRITE (6, 110), '  Reff     : ', CSMIXINGREFF
   WRITE (6, 110), '  Cext     : ', CSMIXINGCEXT
   WRITE (6, 110), '  SSA      : ', CSMIXINGSSA
   WRITE (6, 110), '  OPT_MIX%CSCA      : ', OPT_MIX%CSCA
   WRITE (6, 110), '  NUMSULF      : ', NUMSULF
   WRITE (6, 110), '  OPT_DUST%CSCA      : ', NUMDUST
   WRITE (6, 110), '  NUMDUST      : ', NUMDUST
   WRITE (6, 110), '  CSMIXINGPHASE 1      : ', CSMIXINGPHASE(1)
   WRITE (6, 110), '  CSMIXINGPHASE 100    : ', CSMIXINGPHASE(100)
   WRITE (6, 110), '  CSMIXINGPHASE 1000   : ', CSMIXINGPHASE(1000)

   ! if OC is shell
   ELSEIF (LOC_SHELL) THEN
   
   TMPOCBCRRATIO = OCBCRRATIO(BCID)
   IF ( TMPOCBCRRATIO .lt. 0.0001 ) THEN
      TMPOCBCRRATIO = 0.0001
   ENDIF
   
   WRITE (6, 100), '  ' 
   WRITE (6, 100), '  - MIX_INTERNAL: Calculate optical properties of internal mixing (BC/OC)...'
   CALL CORE_SHELL(CURRENTLAMDA, NANG, NLGNDR,                         &
                   DISTPAR(1,3)* OCBCGF(BCID), DISTPAR(2,3),           & 
                   RMRI(1, 3),  RMRI(2, 3),                            &
                   RMRI(1, 2),  RMRI(2, 2),                            &
                   TMPOCBCRRATIO,  OCBCRHO(BCID),                      &
                   OPT_MIX%REFF,       OPT_MIX%VEFF,                   &
                   OPT_MIX%AREA,       OPT_MIX%VOLUME,                 &
                   OPT_MIX%SSA,        OPT_MIX%ASY,                    &
                   OPT_MIX%CEXT,       OPT_MIX%CSCA,                   & 
                   OPT_MIX%MEXTEFFCY,  OPT_MIX%EXTEFFCY,               &
                   OPT_MIX%HBSCAT,     OPT_MIX%EXTTOBCARATIO,          &
                   OPT_MIX%PHASE,      OPT_MIX%ANGLE,                  &
                   OPT_MIX%LGNDR)                   
   WRITE (6, 110), '  CURRENTLAMDA     : ', CURRENTLAMDA      , ', NANG          : ', NANG
   WRITE (6, 110), '  Rg     : ', DISTPAR(1,3)* OCBCGF(BCID)      , ', Sg         : ', DISTPAR(2,3)
   WRITE (6, 110), '  RMRI(1, 2)     : ', RMRI(1, 2)      , ', RMRI(2, 2)          : ', RMRI(2, 2)
   WRITE (6, 110), '  RMRI(1, 3)     : ', RMRI(1, 3)      , ', RMRI(2, 3)          : ', RMRI(2, 3)
   WRITE (6, 110), '  TMPOCBCRRATIO     : ', TMPOCBCRRATIO      , ', OCBCRHO         : ', OCBCRHO(BCID)
   print *, '--------'
   WRITE (6, 110), '  Reff     : ', OPT_MIX%REFF      , ', Veff          : ', OPT_MIX%VEFF
   WRITE (6, 110), '  Area     : ', OPT_MIX%AREA      , ', Volm          : ', OPT_MIX%VOLUME
   WRITE (6, 110), '  SSA      : ', OPT_MIX%SSA       , ', ASY           : ', OPT_MIX%ASY
   WRITE (6, 110), '  Cext     : ', OPT_MIX%CEXT      , ', Csca          : ', OPT_MIX%CSCA
   WRITE (6, 110), '  MEXTEFFCY: ', OPT_MIX%MEXTEFFCY , ', EXTEFFCY      : ', OPT_MIX%EXTEFFCY
   WRITE (6, 110), '  HBSCAT   : ', OPT_MIX%HBSCAT    , ', EXTTOBCARATIO : ', OPT_MIX%EXTTOBCARATIO

   ! external mixing with dust
   CSTOTALNUM = NUMOC + NUMDUST
   CSMIXINGCEXT = (OPT_MIX%CEXT * NUMOC + OPT_DUST%CEXT * NUMDUST) / &
                  CSTOTALNUM
   
   CSMIXINGSSA = (OPT_DUST%CEXT * NUMDUST * OPT_DUST%SSA + &
                  OPT_MIX%CEXT * NUMOC * OPT_MIX%SSA)  / &
                 (CSMIXINGCEXT * CSTOTALNUM)
   
   CSMIXINGPHASE(1:NANG) = ( OPT_MIX%CSCA  * NUMOC * OPT_MIX%PHASE(1:NANG)    +      & 
                             OPT_DUST%CSCA * NUMDUST * OPT_DUST%PHASE(1:NANG) ) /    & 
                           ( OPT_DUST%CSCA * NUMDUST + OPT_MIX%CSCA * NUMOC)

   CSMIXINGREFF = (NUMDUST * MOM3DUST + NUMOC * MOM3OC * OCBCGF(BCID)**3) / &
                  (NUMDUST * MOM2DUST + NUMOC * MOM2OC * OCBCGF(BCID)**2) 

   DO I = 0, NLGNDR 
      CSMIXINGLGNDR(I) = ceflgn( NANG, CSMIXINGPHASE, QWT, I, PLGNDR(1,I) )
   ENDDO 

   WRITE (6, 100), '  - MIX_INTERNAL: Then external mixing with dust...'
   WRITE (6, 110), '  Reff     : ', CSMIXINGREFF
   WRITE (6, 110), '  Cext     : ', CSMIXINGCEXT
   WRITE (6, 110), '  SSA      : ', CSMIXINGSSA
   WRITE (6, 110), '  OPT_MIX%CSCA      : ', OPT_MIX%CSCA
   WRITE (6, 110), '  NUMOC      : ', NUMOC
   WRITE (6, 110), '  OPT_DUST%CSCA      : ', NUMDUST
   WRITE (6, 110), '  NUMDUST      : ', NUMDUST
   WRITE (6, 110), '  CSMIXINGPHASE 1      : ', CSMIXINGPHASE(1)
   WRITE (6, 110), '  CSMIXINGPHASE 100    : ', CSMIXINGPHASE(100)
   WRITE (6, 110), '  CSMIXINGPHASE 1000   : ', CSMIXINGPHASE(1000)
   
   ENDIF
   
   END SUBROUTINE MIX_INTERNAL
!------------------------------------------------------------------------------   
   
   SUBROUTINE MIX_EXTERNAL
   !=================================================================
   ! MIX_INTERNAL calculate the optical property for the external mixing
   ! state
   !=================================================================   
   USE SULFATE_MOD,        ONLY : AVGGROWTH
   USE NAMELIST_ARRAY_MOD, ONLY : LSULFATE_SHELL
   USE NAMELIST_ARRAY_MOD, ONLY : LOC_SHELL
      
   REAL              :: ceflgn
   INTEGER           :: I
   
   ! if we consider sulfate and not OC
   IF (LSULFATE_SHELL) THEN
     
   TOTALNUM = NUMBC + NUMDUST + NUMSULF
   
   MIXINGCEXT = ( OPT_BC%CEXT * NUMBC + OPT_DUST%CEXT * NUMDUST + &
                  OPT_SULF%CEXT * NUMSULF ) / TOTALNUM

   MIXINGSSA = ( OPT_BC%CEXT   * NUMBC   * OPT_BC%SSA     + &
                 OPT_DUST%CEXT * NUMDUST * OPT_DUST%SSA   + &
                 OPT_SULF%CEXT * NUMSULF * OPT_SULF%SSA ) / & 
               (MIXINGCEXT * TOTALNUM)
   
   MIXINGPHASE(1:nang) = (OPT_BC%CSCA   * NUMBC   * OPT_BC%PHASE(1:NANG)     + &
                          OPT_DUST%CSCA * NUMDUST * OPT_DUST%PHASE(1:NANG)   + &
                          OPT_SULF%CSCA * NUMSULF * OPT_SULF%PHASE(1:NANG))  / &
                         (OPT_BC%CSCA * NUMBC + OPT_DUST%CSCA * NUMDUST + OPT_SULF%CSCA * NUMSULF) 

   MIXINGREFF = ( NUMBC * MOM3BC + NUMDUST * MOM3DUST + NUMSULF * MOM3SULF * AVGGROWTH(RHID)**3) / &
                ( NUMBC * MOM2BC + NUMDUST * MOM2DUST + NUMSULF * MOM2SULF * AVGGROWTH(RHID)**2) 

   ! if we consider OC and not sulfate   
   ELSEIF (LOC_SHELL) THEN
     
   TOTALNUM = NUMBC + NUMDUST + NUMOC
   
   MIXINGCEXT = ( OPT_BC%CEXT * NUMBC + OPT_DUST%CEXT * NUMDUST + &
                  OPT_OC%CEXT * NUMOC ) / TOTALNUM

   MIXINGSSA = ( OPT_BC%CEXT   * NUMBC   * OPT_BC%SSA     + &
                 OPT_DUST%CEXT * NUMDUST * OPT_DUST%SSA   + &
                 OPT_OC%CEXT * NUMOC * OPT_OC%SSA ) / & 
               (MIXINGCEXT * TOTALNUM)
   
   MIXINGPHASE(1:nang) = (OPT_BC%CSCA   * NUMBC   * OPT_BC%PHASE(1:NANG)     + &
                          OPT_DUST%CSCA * NUMDUST * OPT_DUST%PHASE(1:NANG)   + &
                          OPT_OC%CSCA * NUMOC * OPT_OC%PHASE(1:NANG))  / &
                         (OPT_BC%CSCA * NUMBC + OPT_DUST%CSCA * NUMDUST + OPT_OC%CSCA * NUMOC) 

   MIXINGREFF = ( NUMBC * MOM3BC + NUMDUST * MOM3DUST + NUMOC * MOM3OC ) / &
                ( NUMBC * MOM2BC + NUMDUST * MOM2DUST + NUMOC * MOM2OC ) 
   
   ENDIF    

                
   DO I = 0, NLGNDR 
      MIXINGLGNDR(I) = ceflgn( NANG, MIXINGPHASE, QWT, I, PLGNDR(1,I) )
   ENDDO 
   
   WRITE (6, 100), '  - MIX_EXTERNAL: External mix all aerosol...'
   
   WRITE (6, 110), '  Reff     : ', MIXINGREFF
   WRITE (6, 110), '  Cext     : ', MIXINGCEXT
   WRITE (6, 110), '  SSA      : ', MIXINGSSA
   100 FORMAT (A, A)
   110 FORMAT (3X, A, F12.3, A, F12.3)   
   END SUBROUTINE MIX_EXTERNAL 
   
!------------------------------------------------------------------------------
   
   SUBROUTINE SIZE2NUM(RG, SIGMA, RHO, MASS, NAME, NUM, MOM2, MOM3)

   !=================================================================
   ! SIZE2NUM convert the size distribution to number, area and 
   ! volume concentration 
   !=================================================================  
   
      IMPLICIT NONE
   
      REAL                   :: RG
      REAL                   :: SIGMA
      REAL                   :: RHO
      REAL                   :: MASS
      REAL                   :: VOL
      REAL                   :: MOM2
      REAL                   :: MOM3
      REAL                   :: NUM
      CHARACTER(*)           :: NAME
      
      VOL = 4*pi/3. * RG **3 * exp (4.5* log(SIGMA)**2)
      MOM2 = RG**2 * exp (2.0* log(SIGMA)**2) 
      MOM3 = RG**3 * exp (4.5* log(SIGMA)**2) 
      NUM = MASS / ( RHO * VOL )
      
      WRITE (6, 100), '  - SIZE2NUM: Calculate particle number of ', &
                        TRIM(NAME)
      WRITE (6, 110), '  Rg     :'    , RG
      WRITE (6, 110), '  Sigma  :'    , SIGMA
      WRITE (6, 110), '  Density:'    , RHO
      WRITE (6, 110), '  Mass   :'    , MASS
      WRITE (6, 110), '  Volume :'    , VOL
      WRITE (6, 110), '  Number :'    , NUM
      100 FORMAT (A, A)
      110 FORMAT (2X, A, F16.6)
   
   END SUBROUTINE SIZE2NUM
!------------------------------------------------------------------------------

   SUBROUTINE GET_MIX_DENSITY

   USE NAMELIST_ARRAY_MOD, ONLY : LSULFATE_SHELL
   USE NAMELIST_ARRAY_MOD, ONLY : LOC_SHELL
   USE NAMELIST_ARRAY_MOD, ONLY : BC2SULF_MASS
   USE NAMELIST_ARRAY_MOD, ONLY : NBC2SULF_MASS
   USE NAMELIST_ARRAY_MOD, ONLY : BC2OC_MASS
   USE NAMELIST_ARRAY_MOD, ONLY : NBC2OC_MASS   
   USE NAMELIST_ARRAY_MOD, ONLY : BCRHO
   USE NAMELIST_ARRAY_MOD, ONLY : OCRHO
   USE NAMELIST_ARRAY_MOD, ONLY : NRH_LST
   USE NAMELIST_ARRAY_MOD, ONLY : RH_LST
   USE SULFATE_MOD,        ONLY : DRYV_SULF
   USE SULFATE_MOD,        ONLY : WETV_SULF
   USE SULFATE_MOD,        ONLY : TOTALWETSULFMASS
   USE SULFATE_MOD,        ONLY : TOTALDRYSULFMASS

   INTEGER            :: I, J

   ! if sulfate is shell
   IF (LSULFATE_SHELL) THEN

   ALLOCATE( VBC( NBC2SULF_MASS ) )
   ALLOCATE( SULFBCRRATIO( NBC2SULF_MASS, NRH_LST) )
   ALLOCATE( SULFBCGF( NBC2SULF_MASS, NRH_LST ) )
   ALLOCATE( SULFBCMGF( NBC2SULF_MASS, NRH_LST ) )
   ALLOCATE( SULFBCRHO( NBC2SULF_MASS, NRH_LST ) )
      
   DO I = 1, NBC2SULF_MASS
   
      VBC(I) =  BC2SULF_MASS(I) / BCRHO 
      
      DO J = 1, NRH_LST
         SULFBCRRATIO(I, J) = (VBC(I)/(WETV_SULF(J)+VBC(I)))**(1./3.) 
         SULFBCGF(I, J) = ( (VBC(I) + WETV_SULF(J))/ (DRYV_SULF) )**(1./3.)   
         SULFBCRHO(I, J) = (BC2SULF_MASS(I)+ TOTALWETSULFMASS(J) )/(VBC(I)+ WETV_SULF(J))
         SULFBCMGF(I, J) = (BC2SULF_MASS(I)+ TOTALWETSULFMASS(J) )/(TOTALDRYSULFMASS+BC2SULF_MASS(I))
         
      END DO
      
   END DO
   
   WRITE (6, 100), '  - GET_MIX_DENSITY: Calculate sulfate & carbon mixing in different RH...'
   WRITE (6, 170) '  RH :', RH_LST(1:2), ' ...', RH_LST(NRH_LST-1:NRH_LST)
   WRITE (6, 170) '  BC to sulfate mass ratio :', BC2SULF_MASS(1:2), ' ...', BC2SULF_MASS(NBC2SULF_MASS-1:NBC2SULF_MASS)
   
   WRITE (6, 170), '  Mixing growth factor:' , SulfBCGF(1, 1:2), ' ...', SulfBCGF(1, NRH_LST-1:NRH_LST)
   WRITE (6, 180) ' ...'
   WRITE (6, 170), '  Mixing growth factor:' , SulfBCGF(NBC2SULF_MASS, 1:2), ' ...', SulfBCGF(NBC2SULF_MASS, NRH_LST-1:NRH_LST)
   WRITE (6, 170), ''
   WRITE (6, 170), '  Mixing density:' , SULFBCRHO(1, 1:2), ' ...', SULFBCRHO(1, NRH_LST-1:NRH_LST)
   WRITE (6, 180) ' ...'
   WRITE (6, 170), '  Mixing density:' , SULFBCRHO(NBC2SULF_MASS, 1:2), ' ...', SULFBCRHO(NBC2SULF_MASS, NRH_LST-1:NRH_LST)
   WRITE (6, 170), ''
   WRITE (6, 170), '  Mixing mass growth factor:' , SULFBCMGF(1, 1:2), ' ...', SULFBCMGF(1, NRH_LST-1:NRH_LST)
   WRITE (6, 180) ' ...'
   WRITE (6, 170), '  Mixing mass growth factor:' , SULFBCMGF(NBC2SULF_MASS, 1:2), ' ...', SULFBCMGF(NBC2SULF_MASS, NRH_LST-1:NRH_LST)
   
   ! if OC is shell
   ELSEIF (LOC_SHELL) THEN
   
   ALLOCATE( VBC( NBC2OC_MASS ) )
   ALLOCATE( VOC( NBC2OC_MASS ) )
   ALLOCATE( OCBCRRATIO( NBC2OC_MASS ) )
   ALLOCATE( OCBCGF( NBC2OC_MASS ) )
   ALLOCATE( OCBCRHO( NBC2OC_MASS ) )

   DO I = 1, NBC2OC_MASS
   	  
      VBC(I) =  BC2OC_MASS(I) / BCRHO 
      VOC(I) =  1 / OCRHO
      
      OCBCRRATIO(I) = (VBC(I)/(VOC(I)+VBC(I)))**(1./3.) 
      OCBCGF(I) = ( (VBC(I) + VOC(I))/ VOC(I) )**(1./3.) 
      OCBCRHO(I) = (BC2OC_MASS(I)+ 1 )/(VBC(I)+ VOC(I))
      
   END DO   

   WRITE (6, 100), '  - GET_MIX_DENSITY: Calculate OC & BC mixing ...'
   WRITE (6, 170) '  BC to OC mass ratio :', BC2OC_MASS(1:2), ' ...', BC2OC_MASS(NBC2OC_MASS-1:NBC2OC_MASS)
   
   WRITE (6, 170), '  Mixing growth factor:' , OCBCGF(1:2), ' ...', OCBCGF(NBC2OC_MASS-1:NBC2OC_MASS)
   WRITE (6, 180) ' ...'
   WRITE (6, 170), '  Mixing density:' , OCBCRHO(1:2), ' ...', OCBCRHO(NBC2OC_MASS-1:NBC2OC_MASS)
   WRITE (6, 180) ' ...'
   
   ENDIF   
   
   100 FORMAT (A, A)
   110 FORMAT (2X, A, F16.6)
   170 FORMAT (2X, A, 2F7.3, A, 2F7.3 )
   180 FORMAT (18X, A)
    
   END SUBROUTINE GET_MIX_DENSITY 

!------------------------------------------------------------------------------
 
   SUBROUTINE GEN_POLYNOMIAL
   
   INTEGER        :: I, K
   
   ALLOCATE( PK(0:NLGNDR) )
   ALLOCATE( QMU(NANG) )
   ALLOCATE( QWT(NANG) )
   ALLOCATE( PLGNDR(NANG, 0:NLGNDR) )
   
   DO I = 1, NANG
      CALL LEGNDR( NLGNDR, QMU(I), PK )
      DO K = 0, NLGNDR 
         PLGNDR(I, K) = PK(K)
      ENDDO
   ENDDO
   WRITE (6, 100), '  - GEN_PLOYNOMIAL: Finish generating polynomial...'
   
   100 FORMAT (A, A)
   END SUBROUTINE GEN_POLYNOMIAL
!------------------------------------------------------------------------------
   
   END MODULE MIX_MOD
   
   
   
   
   
   