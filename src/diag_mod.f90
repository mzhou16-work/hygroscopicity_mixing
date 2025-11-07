! $id: diag_mod.f90
MODULE DIAG_MOD
!***********************************************************************
! Module DIAG_MOD write all data of interests into .nc file
!
!  Module Routines
!======================================================================
!
!  (1.0) CREATE_DIAG   : 
!
!  (2.0) DEFINE_DIAG   : main function to define .nc variables
!  (2.1) DEFINE_DIAG01 : define .nc variables for parameters that are 
!        not related to wavelength and RH
!  (2.2) DEFINE_DIAG02 : define .nc variables that are not related to RH 
!        but wavelength
!  (2.3) DEFINE_DIAG03 : define .nc variables for parameters that are 
!        not related to wavelength but RH
!  (2.4) DEFINE_DIAG03 : define .nc variables for parameters that are 
!        not related to wavelength and RH

!  (3.0) WRITE_DIAG    :
!  (3.1) WRITE_DIAG01  : write .nc variables for parameters that are 
!        not related to wavelength and RH
!  (3.2) WRITE_DIAG02  : write .nc variables for bc & dust optical
!        parameters, not related to RH but wavelength
!  (3.3) WRITE_DIAG03  : define .nc variables for parameters that are 
!        related to  wavelength and rh, bc & dust optical properties 
!        are also created here, however, need a identification to control
!        the code only write these parameters for the first time
!
!  (4  ) CLOSEUP_DIAG
!
!***********************************************************************

   IMPLICIT NONE
   !
   ! Make everthing PRIVATE,
   PRIVATE
   !
   ! except
   PUBLIC  :: CREATE_DIAG
   PUBLIC  :: DEFINE_DIAG
   PUBLIC  :: WRITE_DIAG
   PUBLIC  :: CLOSEUP_DIAG

  !
  ! Module variables
  INTEGER, PARAMETER   :: ncKind = 4
  INTEGER                     :: DIM_SPECTRA
  INTEGER                     :: DIM_RH
  INTEGER                     :: DIM_SULF
  INTEGER                     :: DIM_BC2SULF
  INTEGER                     :: DIM_DUST2ALL
  INTEGER                     :: DIM_ANG
  INTEGER                     :: DIM_LGNDR
  INTEGER                     :: DIM_AERO
  INTEGER                     :: DIM_DISPARA
  INTEGER                     :: DIM_SULFMASS
  INTEGER                     :: Dim_Ch6,  Dim_Ch10, Dim_Ch45, Dim_Ch30
  INTEGER                     :: Dim_N2,     Dim_N3,      Dim_N5  
   
  INTEGER                     :: vID
  CHARACTER(LEN=40)           :: LName
  CHARACTER(LEN=20)           :: Units
  INTEGER                     :: Dims0D(0), Dims1D(1), Dims2D(2), Dims3D(3)
  INTEGER                     :: Dims4D(4), Dims5D(5), Dims6D(6)
  ! All subroutines follow CONTAINS statement:
  CONTAINS

!------------------------------------------------------------------------------
   !
   SUBROUTINE CREATE_DIAG ( UInc )
   !
   ! USES:
   USE NETCDF_MOD
   USE NAMELIST_ARRAY_MOD, ONLY : codeVersion
   USE NAMELIST_ARRAY_MOD, ONLY : DIR_OUTPUT
   USE NAMELIST_ARRAY_MOD, ONLY : DIAG_PREFIX 
   USE NAMELIST_ARRAY_MOD, ONLY : LEXTERNAL_MIX, LINTERNAL_MIX
   USE ERROR_MOD,          ONLY : ERROR_STOP
   USE TIME_MOD,           ONLY : SYSTEM_TIMESTAMP

   !
   IMPLICIT NONE
   !
   ! Arguments:
   INTEGER, INTENT(OUT)             :: UInc
   
   ! Local Variables
   LOGICAL, PARAMETER          :: DO_netCDF4 = .False.
   CHARACTER(LEN=255)          :: ncFile
   CHARACTER(LEN=10 )          :: ncFormat  
   CHARACTER(LEN=16)           :: Stamp

   !
   ! OUTPUT_DIAG begins here ...
   !
   UInc = 1 
   ! Define names of the output netCDF file and info file
   ncFile   = TRIM(Dir_Output) //TRIM(Diag_Prefix) // '.nc'

   ! Create the netCDF file
   CALL netCDF_Create( UInc, ncFile, Write_NC4=DO_netCDF4 )
 
   ! Prepare some information for global attributes below

   ! netCDf format
   IF ( DO_netCDF4 ) THEN
      ncFormat = 'netCDF4'
   ELSE
      ncFormat = 'netCDF3'
   ENDIF 

   ! System time
   Stamp = SYSTEM_TIMESTAMP()

   ! -999 for Global attributes
   vID = -999
   CALL netCDF_Def_Attr( UInc, vID, 'H & M Diagnostic File'   , 'Title'   )
   CALL netCDF_Def_Attr( UInc, vID, 'Created on '//TRIM(Stamp), 'History' )
   CALL netCDF_Def_Attr( UInc, vID, TRIM(ncFormat)            , 'Format'  )
   CALL netCDF_Def_Attr( UInc, vID, 'Hygroscopicity & Mixing' , 'Model'   )
   CALL netCDF_Def_Attr( UInc, vID, TRIM(codeVersion)         , 'Version' )
 
   ! Model settings 
   IF ( LEXTERNAL_MIX ) THEN 
      CALL netCDF_Def_Attr( UInc, vID, 'On',  'External' )
   ELSE
      CALL netCDF_Def_Attr( UInc, vID, 'Off',  'External' )
   ENDIF

   IF ( LINTERNAL_MIX ) THEN 
      CALL netCDF_Def_Attr( UInc, vID, 'On',  'Internal' )
   ELSE
      CALL netCDF_Def_Attr( UInc, vID, 'Off',  'Internal' )
   ENDIF

   ! Initialize as non-success
   CALL netCDF_Def_Attr( UInc, vID, 'No', 'Success' )

   !
   RETURN
   !
   END SUBROUTINE CREATE_DIAG

!------------------------------------------------------------------------------

   SUBROUTINE DEFINE_DIAG( UInc)
   
   ! USES:
   USE NETCDF_MOD

   !
   IMPLICIT NONE
   !
   INTEGER             :: UInc
   
   CALL DEFINE_DIM( UInc )
   CALL DEFINE_DIAG01( UInc )
   CALL DEFINE_DIAG02( UInc )
   CALL DEFINE_DIAG03( UInc )
   CALL DEFINE_DIAG04( UInc )
   CALL DEFINE_DIAG05( UInc )
   CALL netCDF_End_Def( UInc )
   
   END SUBROUTINE DEFINE_DIAG
   
!------------------------------------------------------------------------------
   !
   SUBROUTINE DEFINE_DIM(UInc)
   
   USE NETCDF_MOD
   USE SPECTRA_MOD,          ONLY : NSPECTRA
   USE NAMELIST_ARRAY_MOD,   ONLY : NRH_LST
   USE NAMELIST_ARRAY_MOD,   ONLY : NSULF
   USE NAMELIST_ARRAY_MOD,   ONLY : NBC2SULF_MASS
   USE NAMELIST_ARRAY_MOD,   ONLY : NDUST2ALL_MASS
   USE NAMELIST_ARRAY_MOD,   ONLY : NSULFMASSRATIO
   USE MIX_MOD,              ONLY : NANG
   USE MIX_MOD,              ONLY : NLGNDR
   
   ! local variable
   INTEGER, INTENT(IN)         :: UInc   
   ! DEFINE_DIAG begins here ...
   !  
   ! Some initialization
   
   Dims0D(0:0) = (/ 0 /)
   
   ! Define dimensions
   CALL netCDF_Def_Dim( UInc, 'Spectra', NSPECTRA      , DIM_SPECTRA )
   CALL netCDF_Def_Dim( UInc, 'RH'     , NRH_LST       , DIM_RH      )
   CALL netCDF_Def_Dim( UInc, 'Sulfate', NSULF         , DIM_SULF    ) 
   CALL netCDF_Def_Dim( UInc, 'BcMRatio',NBC2SULF_MASS , DIM_BC2SULF )
   CALL netCDF_Def_Dim( UInc, 'DustMRatio', NDUST2ALL_MASS, DIM_DUST2ALL)   
   CALL netCDF_Def_Dim( UInc, 'Angle'  , NANG          , DIM_ANG     )  
   CALL netCDF_Def_Dim( UInc, 'Poly'   , NLGNDR        , DIM_LGNDR   )
   CALL netCDF_Def_Dim( UInc, 'AeroSpec', 3            , DIM_AERO    )
   CALL netCDF_Def_Dim( UInc, 'DisPara', 4             , DIM_DISPARA )
   CALL netCDF_Def_Dim( UInc, 'SulfMass',NSULFMASSRATIO, DIM_SULFMASS)
   
   ! Some un-specific dimensions
   CALL netCDF_Def_Dim( UInc, 'Ch6' , 6 , Dim_Ch6  )
   CALL netCDF_Def_Dim( UInc, 'Ch10', 10, Dim_Ch10 ) 
   CALL netCDF_Def_Dim( UInc, 'Ch45', 45, Dim_Ch45 ) 
   CALL netCDF_Def_Dim( UInc, 'Ch30', 30, Dim_Ch30 ) 
   CALL netCDF_Def_Dim( UInc, 'Num2', 2 , Dim_N2   )
   CALL netCDF_Def_Dim( UInc, 'Num3', 3 , Dim_N3   )
   CALL netCDF_Def_Dim( UInc, 'Num5', 5 , Dim_N5   )
   
   END SUBROUTINE DEFINE_DIM
         
!------------------------------------------------------------------------------
   !
   SUBROUTINE DEFINE_DIAG01( UInc )
   
   USE NETCDF_MOD
   USE SPECTRA_MOD,          ONLY : NSPECTRA
   USE NAMELIST_ARRAY_MOD,   ONLY : NRH_LST
   USE NAMELIST_ARRAY_MOD,   ONLY : NSULF
   USE NAMELIST_ARRAY_MOD,   ONLY : NBC2SULF_MASS
   USE NAMELIST_ARRAY_MOD,   ONLY : NDUST2ALL_MASS
   USE NAMELIST_ARRAY_MOD,   ONLY : NSULFMASSRATIO
   USE MIX_MOD,              ONLY : NANG
   USE MIX_MOD,              ONLY : NLGNDR

   !
   IMPLICIT NONE
   !
   include 'netcdf.inc'
   ! 
   ! Arguments
   INTEGER, INTENT(IN)         :: UInc   


   ! LAMDAS
   vID    = 100
   LName  = 'Spectral wavength'
   Units  = 'nm'
   Dims1D = (/ Dim_Spectra /)

   CALL netCDF_Def_Var( UInc, 'LAMDAS', NF_FLOAT, 1, Dims1D, vID )
   CALL netCDF_Def_Attr( UInc, vID, TRIM(Lname), 'longname' )
   CALL netCDF_Def_Attr( UInc, vID, TRIM(Units), 'units'    )   
   
   ! Wavenum: Spectral frequency
   vID    = vID + 1
   LName  = 'Spectral frequency'
   Units  = 'cm^-1'
   Dims1D = (/ Dim_Spectra /)
   CALL netCDF_Def_Var( UInc, 'Wavenum', NF_FLOAT, 1, Dims1D, vID )
   CALL netCDF_Def_Attr( UInc, vID, TRIM(Lname), 'longname' )
   CALL netCDF_Def_Attr( UInc, vID, TRIM(Units), 'units'    )
 
   ! Relative Humidity
   vID    = vID + 1
   LName  = 'Relative Humidity'
   Units  = 'none'
   Dims1D = (/ DIM_RH /)
   CALL netCDF_Def_Var( UInc, 'RH', NF_FLOAT, 1, Dims1D, vID )
   CALL netCDF_Def_Attr( UInc, vID, TRIM(Lname), 'longname' )
   CALL netCDF_Def_Attr( UInc, vID, TRIM(Units), 'units'    ) 

   ! sulfate mass ratio
   vID    = vID + 1
   LName  = 'Sulfate mass ratio'
   Units  = 'none'
   Dims1D = (/ DIM_SULFMASS /)
   CALL netCDF_Def_Var( UInc, 'SulfMassRatio', NF_FLOAT, 1, Dims1D, vID )
   CALL netCDF_Def_Attr( UInc, vID, TRIM(Lname), 'longname' )
   CALL netCDF_Def_Attr( UInc, vID, TRIM(Units), 'units'    )

   ! BC mass ratio
   vID    = vID + 1
   LName  = 'BC to sulfate mass ratio'
   Units  = 'none'
   Dims1D = (/ DIM_BC2SULF /)
   CALL netCDF_Def_Var( UInc, 'BcMassRatio', NF_FLOAT, 1, Dims1D, vID )
   CALL netCDF_Def_Attr( UInc, vID, TRIM(Lname), 'longname' )
   CALL netCDF_Def_Attr( UInc, vID, TRIM(Units), 'units'    )     
   
   ! Dust mass ratio
   vID    = vID + 1
   LName  = 'Dust to all mass ratio'
   Units  = 'none'
   Dims1D = (/ DIM_DUST2ALL /)
   CALL netCDF_Def_Var( UInc, 'DustMassRatio', NF_FLOAT, 1, Dims1D, vID )
   CALL netCDF_Def_Attr( UInc, vID, TRIM(Lname), 'longname' )
   CALL netCDF_Def_Attr( UInc, vID, TRIM(Units), 'units'    )

   ! size distribution
   vID    = vID + 1
   LName  = 'Species size distribution'
   Units  = 'none'
   Dims2D = (/DIM_DISPARA, DIM_AERO/)
   CALL netCDF_Def_Var( UInc, 'DisPara', NF_FLOAT, 2, Dims2D, vID )
   CALL netCDF_Def_Attr( UInc, vID, TRIM(Lname), 'longname' )
   CALL netCDF_Def_Attr( UInc, vID, TRIM(Units), 'units'    )
   
   ! Related to sulfate
   ! Sulfate species names
   vID    = vID + 1
   LName  = 'Sulfate species names'
   Units  = 'none'
   Dims2D = (/ Dim_Ch45, DIM_SULF /)
   CALL netCDF_Def_Var( UInc, 'SulfNames', NF_CHAR, 2, Dims2D, vID )
   CALL netCDF_Def_Attr( UInc, vID, TRIM(Lname), 'longname' )
   CALL netCDF_Def_Attr( UInc, vID, TRIM(Units), 'units'    )   
   
   ! sulfate dry volume
   vID    = vID + 1
   LName  = 'Sulfate dry volume'
   Units  = 'none'
   CALL netCDF_Def_Var( UInc, 'SulfDryV', NF_FLOAT, 0, Dims0D, vID )
   CALL netCDF_Def_Attr( UInc, vID, TRIM(Lname), 'longname' )
   CALL netCDF_Def_Attr( UInc, vID, TRIM(Units), 'units'    )

   ! sulfate dry mass
   vID    = vID + 1
   LName  = 'Sulfate dry mass'
   Units  = 'none'
   CALL netCDF_Def_Var( UInc, 'SulfDryMass', NF_FLOAT, 0, Dims0D, vID )
   CALL netCDF_Def_Attr( UInc, vID, TRIM(Lname), 'longname' )
   CALL netCDF_Def_Attr( UInc, vID, TRIM(Units), 'units'    )
   
   ! sulfate dry density
   vID    = vID + 1
   LName  = 'Sulfate dry density'
   Units  = 'none'
   CALL netCDF_Def_Var( UInc, 'SulfDryRho', NF_FLOAT, 0, Dims0D, vID )
   CALL netCDF_Def_Attr( UInc, vID, TRIM(Lname), 'longname' )
   CALL netCDF_Def_Attr( UInc, vID, TRIM(Units), 'units'    )

   ! sulfate wet volume
   vID    = vID + 1
   LName  = 'Sulfate wet volume'
   Units  = 'none'
   Dims1D = (/DIM_RH/)
   CALL netCDF_Def_Var( UInc, 'SulfWetV', NF_FLOAT, 1, Dims1D, vID )
   CALL netCDF_Def_Attr( UInc, vID, TRIM(Lname), 'longname' )
   CALL netCDF_Def_Attr( UInc, vID, TRIM(Units), 'units'    )

   ! sulfate wet mass
   vID    = vID + 1
   LName  = 'Sulfate wet mass'
   Units  = 'none'
   Dims1D = (/DIM_RH/)
   CALL netCDF_Def_Var( UInc, 'SulfWetMass', NF_FLOAT, 1, Dims1D, vID )
   CALL netCDF_Def_Attr( UInc, vID, TRIM(Lname), 'longname' )
   CALL netCDF_Def_Attr( UInc, vID, TRIM(Units), 'units'    )

   ! sulfate wet density
   vID    = vID + 1
   LName  = 'Sulfate wet density'
   Units  = 'none'
   Dims1D = (/DIM_RH/)
   CALL netCDF_Def_Var( UInc, 'SulfWetRho', NF_FLOAT, 1, Dims1D, vID )
   CALL netCDF_Def_Attr( UInc, vID, TRIM(Lname), 'longname' )
   CALL netCDF_Def_Attr( UInc, vID, TRIM(Units), 'units'    )
   
   ! Sulfate growth factor   
   vID    = vID + 1
   LName  = 'Sulfate growth factor'
   Units  = 'none'
   Dims1D = (/DIM_RH/)
   CALL netCDF_Def_Var( UInc, 'SulfAvgGF', NF_FLOAT, 1, Dims1D, vID )
   CALL netCDF_Def_Attr( UInc, vID, TRIM(Lname), 'longname' )
   CALL netCDF_Def_Attr( UInc, vID, TRIM(Units), 'units'    ) 
   
   ! Sulfate mass growth factor
   vID    = vID + 1
   LName  = 'Sulfate mass growth factor'
   Units  = 'none'
   Dims1D = (/DIM_RH/)
   CALL netCDF_Def_Var( UInc, 'SulfAvgMGF', NF_FLOAT, 1, Dims1D, vID )
   CALL netCDF_Def_Attr( UInc, vID, TRIM(Lname), 'longname' )
   CALL netCDF_Def_Attr( UInc, vID, TRIM(Units), 'units'    )

   ! Related to BC...
   ! BC density
   vID = vID + 1
   LName = 'BC density'
   Units = 'g/cm^3'
   CALL netCDF_Def_Var( UInc, 'BcRho', NF_FLOAT, 0, Dims0D, vID )
   CALL netCDF_Def_Attr( UInc, vID, TRIM(Lname), 'longname' )
   CALL netCDF_Def_Attr( UInc, vID, TRIM(Units), 'units'    )
   
   ! MR: BC Real-part refractive index
   vID = vID + 1
   LName = 'BC Real-part refractive index'
   Units = 'none'
   CALL netCDF_Def_Var( UInc, 'BcMR', NF_FLOAT, 0, Dims0D, vID )
   CALL netCDF_Def_Attr( UInc, vID, TRIM(Lname), 'longname' )
   CALL netCDF_Def_Attr( UInc, vID, TRIM(Units), 'units'    )
   
   ! MI: BC Imag-part refractive index
   vID = vID + 1
   LName = 'BC Imaginary-part refractive index'
   Units = 'none'
   CALL netCDF_Def_Var( UInc, 'BcMI', NF_FLOAT, 0, Dims0D, vID )
   CALL netCDF_Def_Attr( UInc, vID, TRIM(Lname), 'longname' )
   CALL netCDF_Def_Attr( UInc, vID, TRIM(Units), 'units'    )
 
   ! BC Effective radius
   vID    = vID + 1
   LName  = 'BC Effective radius'
   Units  = 'um'
   CALL netCDF_Def_Var( UInc, 'BcREFF', NF_FLOAT, 0, Dims0D, vID )
   CALL netCDF_Def_Attr( UInc, vID, TRIM(Lname), 'longname' )
   CALL netCDF_Def_Attr( UInc, vID, TRIM(Units), 'units'    )

   ! BC Effective variance
   vID    = vID + 1
   LName  = 'BC Effective variance'
   Units  = 'um'
   CALL netCDF_Def_Var( UInc, 'BcVEFF', NF_FLOAT, 0, Dims0D, vID )
   CALL netCDF_Def_Attr( UInc, vID, TRIM(Lname), 'longname' )
   CALL netCDF_Def_Attr( UInc, vID, TRIM(Units), 'units'    )
   
   ! BC Surface area
   vID    = vID + 1
   LName  = 'BC Surface area'
   Units  = 'um^2'
   CALL netCDF_Def_Var( UInc, 'BcAREA', NF_FLOAT, 0, Dims0D, vID )
   CALL netCDF_Def_Attr( UInc, vID, TRIM(Lname), 'longname' )
   CALL netCDF_Def_Attr( UInc, vID, TRIM(Units), 'units'    )
   
   ! BC Volume
   vID    = vID + 1
   LName  = 'BC Volume'
   Units  = 'um^3'
   CALL netCDF_Def_Var( UInc, 'BcVOLUME', NF_FLOAT, 0, Dims0D, vID )
   CALL netCDF_Def_Attr( UInc, vID, TRIM(Lname), 'longname' )
   CALL netCDF_Def_Attr( UInc, vID, TRIM(Units), 'units'    )  
     
   ! Related to dust...   
   ! MR: Dust real-part refractive index
   vID = vID + 1
   LName = 'Dust density'
   Units = 'g/cm^3'
   CALL netCDF_Def_Var( UInc, 'DustRho', NF_FLOAT, 0, Dims0D, vID )
   CALL netCDF_Def_Attr( UInc, vID, TRIM(Lname), 'longname' )
   CALL netCDF_Def_Attr( UInc, vID, TRIM(Units), 'units'    )

   ! MI: Dust real-part refractive index
   vID = vID + 1
   LName = 'Dust Real-part refractive index'
   Units = 'none'
   CALL netCDF_Def_Var( UInc, 'DustMR', NF_FLOAT, 0, Dims0D, vID )
   CALL netCDF_Def_Attr( UInc, vID, TRIM(Lname), 'longname' )
   CALL netCDF_Def_Attr( UInc, vID, TRIM(Units), 'units'    )

   ! MI: Dust imag-part refractive index
   vID = vID + 1
   LName = 'Dust Imaginary-part refractive index'
   Units = 'none'
   CALL netCDF_Def_Var( UInc, 'DustMI', NF_FLOAT, 0, Dims0D, vID )
   CALL netCDF_Def_Attr( UInc, vID, TRIM(Lname), 'longname' )
   CALL netCDF_Def_Attr( UInc, vID, TRIM(Units), 'units'    )

   ! Dust Effective radius
   vID    = vID + 1
   LName  = 'Dust Effective radius'
   Units  = 'um'
   CALL netCDF_Def_Var( UInc, 'DustREFF', NF_FLOAT, 0, Dims0D, vID )
   CALL netCDF_Def_Attr( UInc, vID, TRIM(Lname), 'longname' )
   CALL netCDF_Def_Attr( UInc, vID, TRIM(Units), 'units'    )

   ! Effective variance - dust
   vID    = vID + 1
   LName  = 'Dust Effective variance'
   Units  = 'um'
   CALL netCDF_Def_Var( UInc, 'DustVEFF', NF_FLOAT, 0, Dims0D, vID )
   CALL netCDF_Def_Attr( UInc, vID, TRIM(Lname), 'longname' )
   CALL netCDF_Def_Attr( UInc, vID, TRIM(Units), 'units'    )
   
   ! Surface area - dust
   vID    = vID + 1
   LName  = 'Dust Surface area'
   Units  = 'um^2'
   CALL netCDF_Def_Var( UInc, 'DustAREA', NF_FLOAT, 0, Dims0D, vID )
   CALL netCDF_Def_Attr( UInc, vID, TRIM(Lname), 'longname' )
   CALL netCDF_Def_Attr( UInc, vID, TRIM(Units), 'units'    )
   
   ! Volume - dust
   vID    = vID + 1
   LName  = 'Dust Volume - dust'
   Units  = 'um^3'
   CALL netCDF_Def_Var( UInc, 'DustVOLUME', NF_FLOAT, 0, Dims0D, vID )
   CALL netCDF_Def_Attr( UInc, vID, TRIM(Lname), 'longname' )
   CALL netCDF_Def_Attr( UInc, vID, TRIM(Units), 'units'    )  

   ! related to mixing water...
   vID = vID + 1
   LName = 'Water refractive index'
   Units = 'none'
   Dims1D = (/DIM_SPECTRA/)
   CALL netCDF_Def_Var( UInc, 'WaterMr', NF_FLOAT, 1, Dims1D, vID )
   CALL netCDF_Def_Attr( UInc, vID, TRIM(Lname), 'longname' )
   CALL netCDF_Def_Attr( UInc, vID, TRIM(Units), 'units'    ) 
   
   ! Sulfate composite  Real-part refractive index
   vID = vID + 1
   LName = 'Sulfate composite  Real-part refractive index'
   Units = 'none'
   Dims2D = (/DIM_SPECTRA, DIM_RH/)
   CALL netCDF_Def_Var( UInc, 'SulfCompMR', NF_FLOAT, 2, Dims2D, vID )
   CALL netCDF_Def_Attr( UInc, vID, TRIM(Lname), 'longname' )
   CALL netCDF_Def_Attr( UInc, vID, TRIM(Units), 'units'    ) 

   ! Sulfate species Real-part refractive index
   vID = vID + 1
   LName = 'Sulfate species Real-part refractive index'
   Units = 'none'
   Dims2D = (/DIM_SPECTRA, DIM_SULF/)
   CALL netCDF_Def_Var( UInc, 'SulfSpecMR', NF_FLOAT, 2, Dims2D, vID )
   CALL netCDF_Def_Attr( UInc, vID, TRIM(Lname), 'longname' )
   CALL netCDF_Def_Attr( UInc, vID, TRIM(Units), 'units'    ) 
   
   ! scattering angle...
   vID    = vID + 1
   LName  = 'Scattering angle'
   Units  = 'none'
   Dims1D = (/ DIM_ANG /)
   CALL netCDF_Def_Var( UInc, 'SANGLE', NF_FLOAT, 1, Dims1D, vID )
   CALL netCDF_Def_Attr( UInc, vID, TRIM(Lname), 'longname' )
   CALL netCDF_Def_Attr( UInc, vID, TRIM(Units), 'units'    ) 

   ! scattering angle...
   vID    = vID + 1
   LName  = 'BC total volume'
   Units  = 'none'
   Dims1D = (/ DIM_BC2SULF /)
   CALL netCDF_Def_Var( UInc, 'BcTotalV', NF_FLOAT, 1, Dims1D, vID )
   CALL netCDF_Def_Attr( UInc, vID, TRIM(Lname), 'longname' )
   CALL netCDF_Def_Attr( UInc, vID, TRIM(Units), 'units'    )

   ! Sulfate to BC radius ratio
   vID    = vID + 1
   LName  = 'Sulf to BC radius ratio'
   Units  = 'none'
   Dims2D = (/ DIM_BC2SULF, DIM_RH /)
   CALL netCDF_Def_Var( UInc, 'SulfBcRRATIO', NF_FLOAT, 2, Dims2D, vID )
   CALL netCDF_Def_Attr( UInc, vID, TRIM(Lname), 'longname' )
   CALL netCDF_Def_Attr( UInc, vID, TRIM(Units), 'units'    )

   ! Sulfate-BC growth factor
   vID    = vID + 1
   LName  = 'Sulf-BC growth factor'
   Units  = 'none'
   Dims2D = (/ DIM_BC2SULF, DIM_RH /)
   CALL netCDF_Def_Var( UInc, 'SulfBcGF', NF_FLOAT, 2, Dims2D, vID )
   CALL netCDF_Def_Attr( UInc, vID, TRIM(Lname), 'longname' )
   CALL netCDF_Def_Attr( UInc, vID, TRIM(Units), 'units'    )

   ! Sulfate-BC mass growth factor
   vID    = vID + 1
   LName  = 'Sulf-BC mass growth factor'
   Units  = 'none'
   Dims2D = (/ DIM_BC2SULF, DIM_RH /)
   CALL netCDF_Def_Var( UInc, 'SulfBcMGF', NF_FLOAT, 2, Dims2D, vID )
   CALL netCDF_Def_Attr( UInc, vID, TRIM(Lname), 'longname' )
   CALL netCDF_Def_Attr( UInc, vID, TRIM(Units), 'units'    )

   ! Sulfate-BC mass growth factor
   vID    = vID + 1
   LName  = 'Sulf-BC density'
   Units  = 'g/cm^3'
   Dims2D = (/ DIM_BC2SULF, DIM_RH /)
   CALL netCDF_Def_Var( UInc, 'SulfBcRHO', NF_FLOAT, 2, Dims2D, vID )
   CALL netCDF_Def_Attr( UInc, vID, TRIM(Lname), 'longname' )
   CALL netCDF_Def_Attr( UInc, vID, TRIM(Units), 'units'    )
 
   END SUBROUTINE DEFINE_DIAG01  
!------------------------------------------------------------------------------
   
   SUBROUTINE DEFINE_DIAG02 (UInc)

   ! USES: 
   USE NETCDF_MOD
   USE SPECTRA_MOD,          ONLY : NSPECTRA
   USE NAMELIST_ARRAY_MOD,   ONLY : NRH_LST
   USE NAMELIST_ARRAY_MOD,   ONLY : NSULF
   USE NAMELIST_ARRAY_MOD,   ONLY : NBC2SULF_MASS
   USE NAMELIST_ARRAY_MOD,   ONLY : NDUST2ALL_MASS
   USE NAMELIST_ARRAY_MOD,   ONLY : NSULFMASSRATIO
   USE MIX_MOD,              ONLY : NANG
   USE MIX_MOD,              ONLY : NLGNDR
   
!    USE MIX_MOD,        ONLY : OPT_SULF
!    USE MIX_MOD,        ONLY : OPT_BC
!    USE MIX_MOD,        ONLY : OPT_DUST
!    USE MIX_MOD,        ONLY : OPT_MIX
   !
   IMPLICIT NONE
   !
   include 'netcdf.inc'
   !
   INTEGER             :: UInc
   
   !--------------------------------------------------------
   ! parameters related to black carbon
   ! Single scattering albedo - black carbon
   !--------------------------------------------------------
   vID    = vID + 1
   LName  = 'BC Single scattering albedo'
   Units  = 'none'
   Dims1D = (/ DIM_SPECTRA /)
   CALL netCDF_Def_Var( UInc, 'BcSSA', NF_FLOAT, 1, Dims1D, vID )
   CALL netCDF_Def_Attr( UInc, vID, TRIM(Lname), 'longname' )
   CALL netCDF_Def_Attr( UInc, vID, TRIM(Units), 'units'    )
   
   ! Asymmetric factor - black carbon
   vID    = vID + 1
   LName  = 'BC Asymmetric factor'
   Units  = 'none'
   Dims1D = (/ DIM_SPECTRA /)
   CALL netCDF_Def_Var( UInc, 'BcASY', NF_FLOAT, 1, Dims1D, vID )
   CALL netCDF_Def_Attr( UInc, vID, TRIM(Lname), 'longname' )
   CALL netCDF_Def_Attr( UInc, vID, TRIM(Units), 'units'    )

   ! Extinction coefficient - black carbon
   vID    = vID + 1
   LName  = 'BC Extinction cross section'
   Units  = 'none'
   Dims1D = (/ DIM_SPECTRA /)
   CALL netCDF_Def_Var( UInc, 'BcCEXT', NF_FLOAT, 1, Dims1D, vID )
   CALL netCDF_Def_Attr( UInc, vID, TRIM(Lname), 'longname' )
   CALL netCDF_Def_Attr( UInc, vID, TRIM(Units), 'units'    )

   ! Scattering cross sections - black carbon
   vID    = vID + 1
   LName  = 'BC Scattering cross section'
   Units  = 'none'
   Dims1D = (/ DIM_SPECTRA /)
   CALL netCDF_Def_Var( UInc, 'BcCSCA', NF_FLOAT, 1, Dims1D, vID )
   CALL netCDF_Def_Attr( UInc, vID, TRIM(Lname), 'longname' )
   CALL netCDF_Def_Attr( UInc, vID, TRIM(Units), 'units'    )  

   ! Extinction coefficient - black carbon
   vID    = vID + 1
   LName  = 'BC Extinction coefficient'
   Units  = 'none'
   Dims1D = (/ DIM_SPECTRA /)
   CALL netCDF_Def_Var( UInc, 'BcEXTEFFCY', NF_FLOAT, 1, Dims1D, vID )
   CALL netCDF_Def_Attr( UInc, vID, TRIM(Lname), 'longname' )
   CALL netCDF_Def_Attr( UInc, vID, TRIM(Units), 'units'    )   
      
   ! Mass extinction coefficient - black carbon
   vID    = vID + 1
   LName  = 'BC Mass extinction coefficient'
   Units  = 'none'
   Dims1D = (/ DIM_SPECTRA /)
   CALL netCDF_Def_Var( UInc, 'BcMEXTEFFCY', NF_FLOAT, 1, Dims1D, vID )
   CALL netCDF_Def_Attr( UInc, vID, TRIM(Lname), 'longname' )
   CALL netCDF_Def_Attr( UInc, vID, TRIM(Units), 'units'    )   

   ! Hemispheric backscattering - black carbon
   vID    = vID + 1
   LName  = 'BC Hemispheric backscattering'
   Units  = 'none'
   Dims1D = (/ DIM_SPECTRA /)
   CALL netCDF_Def_Var( UInc, 'BcHBSCAT', NF_FLOAT, 1, Dims1D, vID )
   CALL netCDF_Def_Attr( UInc, vID, TRIM(Lname), 'longname' )
   CALL netCDF_Def_Attr( UInc, vID, TRIM(Units), 'units'    )  

   ! Extinction to back scattering ratio - black carbon
   vID    = vID + 1
   LName  = 'BC Extinction to back scattering ratio'
   Units  = 'none'
   Dims1D = (/ DIM_SPECTRA /)
   CALL netCDF_Def_Var( UInc, 'BcEXTTOBCARATIO', NF_FLOAT, 1, Dims1D, vID )
   CALL netCDF_Def_Attr( UInc, vID, TRIM(Lname), 'longname' )
   CALL netCDF_Def_Attr( UInc, vID, TRIM(Units), 'units'    )

   ! Phase function - black carbon
   vID    = vID + 1
   LName  = 'BC Phase function'
   Units  = 'none'
   Dims2D = (/ DIM_SPECTRA, DIM_ANG /)
   CALL netCDF_Def_Var( UInc, 'BcPHASE', NF_FLOAT, 2, Dims2D, vID )
   CALL netCDF_Def_Attr( UInc, vID, TRIM(Lname), 'longname' )
   CALL netCDF_Def_Attr( UInc, vID, TRIM(Units), 'units'    )   

   !--------------------------------------------------------
   ! Parameters related to dust...
   !--------------------------------------------------------
   ! Dust Single scattering albedo
   vID    = vID + 1
   LName  = 'Dust Single scattering albedo'
   Units  = 'none'
   Dims1D = (/ DIM_SPECTRA /)
   CALL netCDF_Def_Var( UInc, 'DustSSA', NF_FLOAT, 1, Dims1D, vID )
   CALL netCDF_Def_Attr( UInc, vID, TRIM(Lname), 'longname' )
   CALL netCDF_Def_Attr( UInc, vID, TRIM(Units), 'units'    )
   
   ! Dust Asymmetric factor
   vID    = vID + 1
   LName  = 'Dust Asymmetric factor'
   Units  = 'none'
   Dims1D = (/ DIM_SPECTRA /)
   CALL netCDF_Def_Var( UInc, 'DustASY', NF_FLOAT, 1, Dims1D, vID )
   CALL netCDF_Def_Attr( UInc, vID, TRIM(Lname), 'longname' )
   CALL netCDF_Def_Attr( UInc, vID, TRIM(Units), 'units'    )

   ! Dust Extinction coefficient
   vID    = vID + 1
   LName  = 'Dust Extinction cross section'
   Units  = 'none'
   Dims1D = (/ DIM_SPECTRA /)
   CALL netCDF_Def_Var( UInc, 'DustCEXT', NF_FLOAT, 1, Dims1D, vID )
   CALL netCDF_Def_Attr( UInc, vID, TRIM(Lname), 'longname' )
   CALL netCDF_Def_Attr( UInc, vID, TRIM(Units), 'units'    )

   ! Dust Scattering cross sections
   vID    = vID + 1
   LName  = 'Dust Scattering cross section'
   Units  = 'none'
   Dims1D = (/ DIM_SPECTRA /)
   CALL netCDF_Def_Var( UInc, 'DustCSCA', NF_FLOAT, 1, Dims1D, vID )
   CALL netCDF_Def_Attr( UInc, vID, TRIM(Lname), 'longname' )
   CALL netCDF_Def_Attr( UInc, vID, TRIM(Units), 'units'    )  

   ! Dust Extinction coefficient
   vID    = vID + 1
   LName  = 'Dust Extinction coefficient'
   Units  = 'none'
   Dims1D = (/ DIM_SPECTRA /)
   CALL netCDF_Def_Var( UInc, 'DustEXTEFFCY', NF_FLOAT, 1, Dims1D, vID )
   CALL netCDF_Def_Attr( UInc, vID, TRIM(Lname), 'longname' )
   CALL netCDF_Def_Attr( UInc, vID, TRIM(Units), 'units'    )   
      
   ! Dust Mass extinction coefficient
   vID    = vID + 1
   LName  = 'Dust Mass extinction coefficient'
   Units  = 'none'
   Dims1D = (/ DIM_SPECTRA /)
   CALL netCDF_Def_Var( UInc, 'DustMEXTEFFCY', NF_FLOAT, 1, Dims1D, vID )
   CALL netCDF_Def_Attr( UInc, vID, TRIM(Lname), 'longname' )
   CALL netCDF_Def_Attr( UInc, vID, TRIM(Units), 'units'    )   

   ! Dust Hemispheric backscattering
   vID    = vID + 1
   LName  = 'Dust Hemispheric backscattering'
   Units  = 'none'
   Dims1D = (/ DIM_SPECTRA /)
   CALL netCDF_Def_Var( UInc, 'DustHBSCAT', NF_FLOAT, 1, Dims1D, vID )
   CALL netCDF_Def_Attr( UInc, vID, TRIM(Lname), 'longname' )
   CALL netCDF_Def_Attr( UInc, vID, TRIM(Units), 'units'    )  

   ! Dust Extinction to back scattering ratio
   vID    = vID + 1
   LName  = 'Dust Extinction to back scattering ratio'
   Units  = 'none'
   Dims1D = (/ DIM_SPECTRA /)
   CALL netCDF_Def_Var( UInc, 'DustEXTTOBCARATIO', NF_FLOAT, 1, Dims1D, vID )
   CALL netCDF_Def_Attr( UInc, vID, TRIM(Lname), 'longname' )
   CALL netCDF_Def_Attr( UInc, vID, TRIM(Units), 'units'    )

   ! Phase function - dust
   vID    = vID + 1
   LName  = 'Dust Phase function'
   Units  = 'none'
   Dims2D = (/ DIM_SPECTRA, DIM_ANG /)
   CALL netCDF_Def_Var( UInc, 'DustPHASE', NF_FLOAT, 2, Dims2D, vID )
   CALL netCDF_Def_Attr( UInc, vID, TRIM(Lname), 'longname' )
   CALL netCDF_Def_Attr( UInc, vID, TRIM(Units), 'units'    ) 
   
   END SUBROUTINE DEFINE_DIAG02
   !------------------------------------------------------------------------------

 SUBROUTINE DEFINE_DIAG03(UInc)
   
   ! USES:
   USE NETCDF_MOD
   USE SPECTRA_MOD,          ONLY : NSPECTRA
   USE NAMELIST_ARRAY_MOD,   ONLY : NRH_LST
   USE NAMELIST_ARRAY_MOD,   ONLY : NSULF
   USE NAMELIST_ARRAY_MOD,   ONLY : NBC2SULF_MASS
   USE NAMELIST_ARRAY_MOD,   ONLY : NDUST2ALL_MASS
   USE NAMELIST_ARRAY_MOD,   ONLY : NSULFMASSRATIO
   USE MIX_MOD,              ONLY : NANG
   USE MIX_MOD,              ONLY : NLGNDR
   
   !
   IMPLICIT NONE
   !
   include 'netcdf.inc'
   
   INTEGER             :: UInc
 
  
   !--------------------------------------------------------
   ! parameters related to sulfate composite
   !--------------------------------------------------------
   ! Effective radius - sulfate composite
   vID    = vID + 1
   LName  = 'Sulfate composite Effective radius'
   Units  = 'um'
   Dims1D = (/ DIM_RH /)
   CALL netCDF_Def_Var( UInc, 'SulfCompREFF', NF_FLOAT, 1, Dims1D, vID )
   CALL netCDF_Def_Attr( UInc, vID, TRIM(Lname), 'longname' )
   CALL netCDF_Def_Attr( UInc, vID, TRIM(Units), 'units'    )

   ! Effective variance - sulfate composite
   vID    = vID + 1
   LName  = 'Sulfate composite Effective variance'
   Units  = 'um'
   Dims1D = (/ DIM_RH /)
   CALL netCDF_Def_Var( UInc, 'SulfCompVEFF', NF_FLOAT, 1, Dims1D, vID )
   CALL netCDF_Def_Attr( UInc, vID, TRIM(Lname), 'longname' )
   CALL netCDF_Def_Attr( UInc, vID, TRIM(Units), 'units'    )
   
   ! Surface area - sulfate composite
   vID    = vID + 1
   LName  = 'Sulfate composite Surface area'
   Units  = 'um^2'
   Dims1D = (/ DIM_RH /)
   CALL netCDF_Def_Var( UInc, 'SulfCompAREA', NF_FLOAT, 1, Dims1D, vID )
   CALL netCDF_Def_Attr( UInc, vID, TRIM(Lname), 'longname' )
   CALL netCDF_Def_Attr( UInc, vID, TRIM(Units), 'units'    )
   
   ! Volume - sulfate composite
   vID    = vID + 1
   LName  = 'Sulfate composite Volume'
   Units  = 'um^3'
   Dims1D = (/ DIM_RH /)
   CALL netCDF_Def_Var( UInc, 'SulfCompVOLUME', NF_FLOAT, 1, Dims1D, vID )
   CALL netCDF_Def_Attr( UInc, vID, TRIM(Lname), 'longname' )
   CALL netCDF_Def_Attr( UInc, vID, TRIM(Units), 'units'    )


   END SUBROUTINE DEFINE_DIAG03

!------------------------------------------------------------------------------
   SUBROUTINE DEFINE_DIAG04(UInc)
   
   ! USES:
   USE NETCDF_MOD
   USE SPECTRA_MOD,          ONLY : NSPECTRA
   USE NAMELIST_ARRAY_MOD,   ONLY : NRH_LST
   USE NAMELIST_ARRAY_MOD,   ONLY : NSULF
   USE NAMELIST_ARRAY_MOD,   ONLY : NBC2SULF_MASS
   USE NAMELIST_ARRAY_MOD,   ONLY : NDUST2ALL_MASS
   USE NAMELIST_ARRAY_MOD,   ONLY : NSULFMASSRATIO
   USE MIX_MOD,              ONLY : NANG
   USE MIX_MOD,              ONLY : NLGNDR
   
   !
   IMPLICIT NONE
   !
   include 'netcdf.inc'
   !
   INTEGER             :: UInc

   !--------------------------------------------------------
   ! Optical parameters related to sulfate composites
   !--------------------------------------------------------  
   ! Single scattering albedo - sulfate composites
   vID    = vID + 1
   LName  = 'Sulfate composite Single scattering albedo'
   Units  = 'none'
   Dims2D = (/ DIM_SPECTRA, DIM_RH /)
   CALL netCDF_Def_Var( UInc, 'SulfCompSSA', NF_FLOAT, 2, Dims2D, vID )
   CALL netCDF_Def_Attr( UInc, vID, TRIM(Lname), 'longname' )
   CALL netCDF_Def_Attr( UInc, vID, TRIM(Units), 'units'    )
   
   ! Asymmetric factor - sulfate composites
   vID    = vID + 1
   LName  = 'Sulfate composite Asymmetric factor'
   Units  = 'none'
   Dims2D = (/ DIM_SPECTRA, DIM_RH /)
   CALL netCDF_Def_Var( UInc, 'SulfCompASY', NF_FLOAT, 2, Dims2D, vID )
   CALL netCDF_Def_Attr( UInc, vID, TRIM(Lname), 'longname' )
   CALL netCDF_Def_Attr( UInc, vID, TRIM(Units), 'units'    )

   ! Extinction coefficient - sulfate composites
   vID    = vID + 1
   LName  = 'Sulfate composite Extinction cross section'
   Units  = 'none'
   Dims2D = (/ DIM_SPECTRA, DIM_RH /)
   CALL netCDF_Def_Var( UInc, 'SulfCompCEXT', NF_FLOAT, 2, Dims2D, vID )
   CALL netCDF_Def_Attr( UInc, vID, TRIM(Lname), 'longname' )
   CALL netCDF_Def_Attr( UInc, vID, TRIM(Units), 'units'    )

   ! Scattering cross sections - sulfate composites
   vID    = vID + 1
   LName  = 'Sulfate composite Scattering cross section'
   Units  = 'none'
   Dims2D = (/ DIM_SPECTRA, DIM_RH /)
   CALL netCDF_Def_Var( UInc, 'SulfCompCSCA', NF_FLOAT, 2, Dims2D, vID )
   CALL netCDF_Def_Attr( UInc, vID, TRIM(Lname), 'longname' )
   CALL netCDF_Def_Attr( UInc, vID, TRIM(Units), 'units'    )  

   ! Extinction coefficient - sulfate composites
   vID    = vID + 1
   LName  = 'Sulfate composite Extinction coefficient'
   Units  = 'none'
   Dims2D = (/ DIM_SPECTRA, DIM_RH /)
   CALL netCDF_Def_Var( UInc, 'SulfCompEXTEFFCY', NF_FLOAT, 2, Dims2D, vID )
   CALL netCDF_Def_Attr( UInc, vID, TRIM(Lname), 'longname' )
   CALL netCDF_Def_Attr( UInc, vID, TRIM(Units), 'units'    )   
      
   ! Mass extinction coefficient - sulfate composites
   vID    = vID + 1
   LName  = 'Sulfate composite Mass extinction coefficient'
   Units  = 'none'
   Dims2D = (/ DIM_SPECTRA, DIM_RH /)
   CALL netCDF_Def_Var( UInc, 'SulfCompMEXTEFFCY', NF_FLOAT, 2, Dims2D, vID )
   CALL netCDF_Def_Attr( UInc, vID, TRIM(Lname), 'longname' )
   CALL netCDF_Def_Attr( UInc, vID, TRIM(Units), 'units'    )   

   ! Hemispheric backscattering - sulfate composites
   vID    = vID + 1
   LName  = 'Sulfate composite Hemispheric backscattering'
   Units  = 'none'
   Dims2D = (/ DIM_SPECTRA, DIM_RH /)
   CALL netCDF_Def_Var( UInc, 'SulfCompHBSCAT', NF_FLOAT, 2, Dims2D, vID )
   CALL netCDF_Def_Attr( UInc, vID, TRIM(Lname), 'longname' )
   CALL netCDF_Def_Attr( UInc, vID, TRIM(Units), 'units'    )  

   ! Extinction to back scattering ratio - sulfate composites
   vID    = vID + 1
   LName  = 'Sulfate composite Extinction to back scattering ratio'
   Units  = 'none'
   Dims2D = (/ DIM_SPECTRA, DIM_RH /)
   CALL netCDF_Def_Var( UInc, 'SulfCompEXTTOBCARATIO', NF_FLOAT, 2, Dims2D, vID )
   CALL netCDF_Def_Attr( UInc, vID, TRIM(Lname), 'longname' )
   CALL netCDF_Def_Attr( UInc, vID, TRIM(Units), 'units'    )

   ! Phase function - sulfate composites
   vID    = vID + 1
   LName  = 'Sulfate composite Phase function'
   Units  = 'none'
   Dims3D = (/ DIM_SPECTRA, DIM_RH, DIM_ANG/)
   CALL netCDF_Def_Var( UInc, 'SulfCompPHASE', NF_FLOAT, 3, Dims3D, vID )
   CALL netCDF_Def_Attr( UInc, vID, TRIM(Lname), 'longname' )
   CALL netCDF_Def_Attr( UInc, vID, TRIM(Units), 'units'    )        
   
   END SUBROUTINE DEFINE_DIAG04
   
!------------------------------------------------------------------------------
   SUBROUTINE DEFINE_DIAG05(UInc)
   
   ! USES:
   USE NETCDF_MOD
   USE NAMELIST_ARRAY_MOD, ONLY : LEXTERNAL_MIX
   USE NAMELIST_ARRAY_MOD, ONLY : LINTERNAL_MIX
   !
   IMPLICIT NONE
   !
   include 'netcdf.inc'
   !
   INTEGER             :: UInc

   IF (LEXTERNAL_MIX) THEN
      ! External mixing Extinction cross section
      vID    = vID + 1
      LName  = 'External mixing Extinction cross section'
      Units  = 'none'
      Dims4D = (/ DIM_SPECTRA, DIM_RH, DIM_DUST2ALL, DIM_BC2SULF /)
      CALL netCDF_Def_Var( UInc, 'ExMixCEXT', NF_FLOAT, 4, Dims4D, vID )
      CALL netCDF_Def_Attr( UInc, vID, TRIM(Lname), 'longname' )
      CALL netCDF_Def_Attr( UInc, vID, TRIM(Units), 'units'    )   

      ! External mixing Single scattering albedo
      vID    = vID + 1
      LName  = 'External mixing Single scattering albedo'
      Units  = 'none'
      Dims4D = (/ DIM_SPECTRA, DIM_RH, DIM_DUST2ALL, DIM_BC2SULF /)
      CALL netCDF_Def_Var( UInc, 'ExMixSSA', NF_FLOAT, 4, Dims4D, vID )
      CALL netCDF_Def_Attr( UInc, vID, TRIM(Lname), 'longname' )
      CALL netCDF_Def_Attr( UInc, vID, TRIM(Units), 'units'    )
      
      ! External mixing Effective radius
      vID    = vID + 1
      LName  = 'External mixing Effective radius'
      Units  = 'um'
      Dims4D = (/ DIM_SPECTRA, DIM_RH, DIM_DUST2ALL, DIM_BC2SULF /)
      CALL netCDF_Def_Var( UInc, 'ExMixREFF', NF_FLOAT, 4, Dims4D, vID )
      CALL netCDF_Def_Attr( UInc, vID, TRIM(Lname), 'longname' )
      CALL netCDF_Def_Attr( UInc, vID, TRIM(Units), 'units'    )
      
   
      ! External mixing Total Number
      vID    = vID + 1
      LName  = 'External mixing Total Number'
      Units  = 'none'
      Dims4D = (/ DIM_SPECTRA, DIM_RH, DIM_DUST2ALL, DIM_BC2SULF /)
      CALL netCDF_Def_Var( UInc, 'ExMixNUM', NF_FLOAT, 4, Dims4D, vID )
      CALL netCDF_Def_Attr( UInc, vID, TRIM(Lname), 'longname' )
      CALL netCDF_Def_Attr( UInc, vID, TRIM(Units), 'units'    )
      
      ! External mixing Phase Function
      vID    = vID + 1
      LName  = 'External mixing Phase Function'
      Units  = 'none'
      Dims5D = (/ DIM_SPECTRA, DIM_RH, DIM_DUST2ALL, DIM_BC2SULF, DIM_ANG /)
      CALL netCDF_Def_Var( UInc, 'ExMixPHASE', NF_FLOAT, 5, Dims5D, vID )
      CALL netCDF_Def_Attr( UInc, vID, TRIM(Lname), 'longname' )
      CALL netCDF_Def_Attr( UInc, vID, TRIM(Units), 'units'    )
         
   END IF 
 

   IF (LINTERNAL_MIX) THEN
      ! Internal mixing Extinction cross section
      vID    = vID + 1
      LName  = 'Internal mixing Extinction cross section'
      Units  = 'none'
      Dims4D = (/ DIM_SPECTRA, DIM_RH, DIM_DUST2ALL, DIM_BC2SULF /)
      CALL netCDF_Def_Var( UInc, 'InMixCEXT', NF_FLOAT, 4, Dims4D, vID )
      CALL netCDF_Def_Attr( UInc, vID, TRIM(Lname), 'longname' )
      CALL netCDF_Def_Attr( UInc, vID, TRIM(Units), 'units'    )   

      ! Internal mixing Single scattering albedo
      vID    = vID + 1
      LName  = 'Internal mixing Single scattering albedo'
      Units  = 'none'
      Dims4D = (/ DIM_SPECTRA, DIM_RH, DIM_DUST2ALL, DIM_BC2SULF /)
      CALL netCDF_Def_Var( UInc, 'InMixSSA', NF_FLOAT, 4, Dims4D, vID )
      CALL netCDF_Def_Attr( UInc, vID, TRIM(Lname), 'longname' )
      CALL netCDF_Def_Attr( UInc, vID, TRIM(Units), 'units'    )
      
      ! Internal mixing Effective radius
      vID    = vID + 1
      LName  = 'Internal mixing Effective radius'
      Units  = 'um'
      Dims4D = (/ DIM_SPECTRA, DIM_RH, DIM_DUST2ALL, DIM_BC2SULF /)
      CALL netCDF_Def_Var( UInc, 'InMixREFF', NF_FLOAT, 4, Dims4D, vID )
      CALL netCDF_Def_Attr( UInc, vID, TRIM(Lname), 'longname' )
      CALL netCDF_Def_Attr( UInc, vID, TRIM(Units), 'units'    )
      
   
      ! Internal mixing Total Number
      vID    = vID + 1
      LName  = 'Internal mixing Total Number'
      Units  = 'none'
      Dims4D = (/ DIM_SPECTRA, DIM_RH, DIM_DUST2ALL, DIM_BC2SULF /)
      CALL netCDF_Def_Var( UInc, 'InMixNUM', NF_FLOAT, 4, Dims4D, vID )
      CALL netCDF_Def_Attr( UInc, vID, TRIM(Lname), 'longname' )
      CALL netCDF_Def_Attr( UInc, vID, TRIM(Units), 'units'    )
      
      ! Internal mixing Phase Function
      vID    = vID + 1
      LName  = 'Internal mixing Phase Function'
      Units  = 'none'
      Dims5D = (/ DIM_SPECTRA, DIM_RH, DIM_DUST2ALL, DIM_BC2SULF, DIM_ANG /)
      CALL netCDF_Def_Var( UInc, 'InMixPHASE', NF_FLOAT, 5, Dims5D, vID )
      CALL netCDF_Def_Attr( UInc, vID, TRIM(Lname), 'longname' )
      CALL netCDF_Def_Attr( UInc, vID, TRIM(Units), 'units'    )
   END IF     


   END SUBROUTINE DEFINE_DIAG05
!------------------------------------------------------------------------------

   SUBROUTINE WRITE_DIAG( UInc )
   !
   ! routine to write all data of interests into .nc file
   ! 
   !
   ! use
   USE MIX_MOD,            ONLY : BCID
   USE MIX_MOD,            ONLY : DUSTID
   USE MIX_MOD,            ONLY : RHID
   USE MIX_MOD,            ONLY : LAMDAID
   
   IMPLICIT NONE
   !
   ! Arguments:
   INTEGER, INTENT(IN)        :: UInc
   
   IF ( ( RHID == 1 ) .AND. ( LAMDAID == 1 ).AND. ( DUSTID == 1 ) .AND. ( BCID == 1 ) ) THEN
      CALL WRITE_DIAG01(UInc)
   END IF

   IF  ((RHID == 1) .AND. ( DUSTID == 1 ) .AND. ( BCID == 1 )) THEN
      CALL WRITE_DIAG02(UInc)
   END IF
    
   IF (( LAMDAID == 1 ) .AND. ( DUSTID == 1 ) .AND. ( BCID == 1 )) THEN
      CALL WRITE_DIAG03(UInc)
   END IF
   
     
   IF ( ( DUSTID == 1 ) .AND. ( BCID == 1 ) )  THEN
      CALL WRITE_DIAG04(UInc)
   END IF
   
   CALL WRITE_DIAG05(UInc)
   

   END SUBROUTINE WRITE_DIAG
!------------------------------------------------------------------------------
   SUBROUTINE WRITE_DIAG01( UInc )
   
   ! model input and parameters that are not related to wavelength & BC and dust
   ! mass ratio...
    
   USE NETCDF_MOD

   USE NAMELIST_ARRAY_MOD, ONLY : NRH_LST, RH_LST
   USE NAMELIST_ARRAY_MOD, ONLY : NDUST2ALL_MASS, DUST2ALL_MASS   
   USE NAMELIST_ARRAY_MOD, ONLY : NBC2SULF_MASS,  BC2SULF_MASS
   USE NAMELIST_ARRAY_MOD, ONLY : BCRHO,          DUSTRHO    
   USE NAMELIST_ARRAY_MOD, ONLY : DISTPAR
   USE NAMELIST_ARRAY_MOD, ONLY : RMRI
   
   USE SPECTRA_MOD,        ONLY : NSPECTRA
   USE SPECTRA_MOD,        ONLY : LAMDAS, WAVENUMS  
   
   ! sulfate species...
   USE NAMELIST_ARRAY_MOD, ONLY : NSULF
   USE SULFATE_MOD,        ONLY : SNAME
   USE NAMELIST_ARRAY_MOD, ONLY : NSULFMASSRATIO, SULF_MASSRATIO_LST

   ! Water
   USE SULFATE_MOD,        ONLY : WATERNR
      
   ! Dry Sulfate
   USE SULFATE_MOD,        ONLY : DRYV_SULF
   USE SULFATE_MOD,        ONLY : AVGDRYSULFRHO
   USE SULFATE_MOD,        ONLY : TOTALDRYSULFMASS
   USE SULFATE_MOD,        ONLY : COMPOSITENR
   USE SULFATE_MOD,        ONLY : NR
   
   USE SULFATE_MOD,        ONLY : WETV_SULF 
   USE SULFATE_MOD,        ONLY : AVGWETSULFRHO
   USE SULFATE_MOD,        ONLY : TOTALWETSULFMASS
   USE SULFATE_MOD,        ONLY : AVGGROWTH
   USE SULFATE_MOD,        ONLY : AVGMGROWTH

 	
   USE MIX_MOD,			   ONLY : OPT_BC
   USE MIX_MOD,			   ONLY : OPT_DUST
   USE MIX_MOD, 		   ONLY : NANG
   USE MIX_MOD,			   ONLY : VBC
   USE MIX_MOD,			   ONLY : SULFBCRRATIO
   USE MIX_MOD,			   ONLY : SULFBCGF
   USE MIX_MOD,			   ONLY : SULFBCMGF
   USE MIX_MOD,			   ONLY : SULFBCRHO
  
   IMPLICIT NONE
   !
   ! Arguments:
   INTEGER, INTENT(IN)        :: UInc
   
   !
   ! Local variables
   INTEGER            :: st1D, st2D(2)
   INTEGER            :: ct1D, ct2D(2)
   INTEGER            :: StrLen
   
   ! lamda
   CALL netCDF_Write_Var( UInc, REAL(LAMDAS,ncKind),   'LAMDAS',  1,  NSPECTRA )
   CALL netCDF_Write_Var( UInc, REAL(WAVENUMS,ncKind), 'Wavenum', 1,  NSPECTRA )   
   
   ! Relative Humidity
   CALL netCDF_Write_Var( UInc, REAL(RH_LST,ncKind), 'RH', 1, NRH_LST )

   !Sulfate mass ratio
   CALL netCDF_Write_Var( UInc, REAL(SULF_MASSRATIO_LST(1:NSULFMASSRATIO),ncKind), 'SulfMassRatio',1, NSULFMASSRATIO)

   ! BC to sulfate mass ratio
   CALL netCDF_Write_Var( UInc, REAL(BC2SULF_MASS,ncKind), 'BcMassRatio', 1, NBC2SULF_MASS)
   
   ! Dust to all mass ratio
   CALL netCDF_Write_Var( UInc, REAL(DUST2ALL_MASS,ncKind), 'DustMassRatio', 1, NDUST2ALL_MASS)  
   
   ! Species size distribution
   st2D(:) = 1
   ct2D = (/ 4, 3 /)
   CALL netCDF_Write_Var( UInc, REAL(DISTPAR(1:4,1:3), ncKind),  'DisPara', st2D, ct2D)
   
   ! Sulfate species names
   StrLen = 45
   st2D(1:2) = 1
   ct2D(1)   = StrLen
   ct2D(2)   = NSULF   
   CALL netCDF_Write_Var( UInc, SNAME(1:NSULF), 'SulfNames', st2D, ct2D) 
   
   ! Dry Sulfate...
   CALL netCDF_Write_Var( UInc, REAL(DRYV_SULF,ncKind), 'SulfDryV')
   CALL netCDF_Write_Var( UInc, REAL(TOTALDRYSULFMASS,ncKind), 'SulfDryMass')   
   CALL netCDF_Write_Var( UInc, REAL(AVGDRYSULFRHO,ncKind), 'SulfDryRho')
   CALL netCDF_Write_Var( UInc, REAL(WETV_SULF,ncKind), 'SulfWetV', 1, NRH_LST )   
   CALL netCDF_Write_Var( UInc, REAL(TOTALWETSULFMASS,ncKind), 'SulfWetMass', 1, NRH_LST )  
   CALL netCDF_Write_Var( UInc, REAL(AVGWETSULFRHO,ncKind), 'SulfWetRho', 1, NRH_LST )  
   CALL netCDF_Write_Var( UInc, REAL(AVGGROWTH,ncKind), 'SulfAvgGF', 1, NRH_LST )  
   CALL netCDF_Write_Var( UInc, REAL(AVGMGROWTH,ncKind), 'SulfAvgMGF', 1, NRH_LST )

   ! BC...
   CALL netCDF_Write_Var( UInc, REAL(RMRI(1,2),ncKind),     'BcMR')
   CALL netCDF_Write_Var( UInc, REAL(RMRI(2,2),ncKind),     'BcMI')
   CALL netCDF_Write_Var( UInc, REAL(BCRHO,ncKind),         'BcRho')
   CALL netCDF_Write_Var( UInc, REAL(OPT_BC%REFF,ncKind),   'BcREFF')
   CALL netCDF_Write_Var( UInc, REAL(OPT_BC%VEFF,ncKind),   'BcVEFF')
   CALL netCDF_Write_Var( UInc, REAL(OPT_BC%AREA,ncKind),   'BcAREA')
   CALL netCDF_Write_Var( UInc, REAL(OPT_BC%VOLUME,ncKind), 'BcVOLUME')
   
   ! Dust...
   CALL netCDF_Write_Var( UInc, REAL(RMRI(1,3),ncKind), 'DustMR')
   CALL netCDF_Write_Var( UInc, REAL(RMRI(1,3),ncKind), 'DustMI')
   CALL netCDF_Write_Var( UInc, REAL(DUSTRHO,ncKind), 'DustRho')
   CALL netCDF_Write_Var( UInc, REAL(OPT_DUST%REFF,ncKind),   'DustREFF')
   CALL netCDF_Write_Var( UInc, REAL(OPT_DUST%VEFF,ncKind),   'DustVEFF')
   CALL netCDF_Write_Var( UInc, REAL(OPT_DUST%AREA,ncKind),   'DustAREA')
   CALL netCDF_Write_Var( UInc, REAL(OPT_DUST%VOLUME,ncKind), 'DustVOLUME')   
 
   ! Water refractive index real part
   CALL netCDF_Write_Var( UInc, REAL(WATERNR,ncKind), 'WaterMr', 1, NSPECTRA)     
   
   ! We aslo prepare the retractive index of the sulfate at the beginning
   ! of the calculation...    
   st2D(:) = 1
   ct2D = (/ NSPECTRA, NRH_LST /)
   CALL netCDF_Write_Var( UInc, REAL(COMPOSITENR, ncKind),  'SulfCompMR', st2D, ct2D)   

   st2D(:) = 1
   ct2D = (/ NSPECTRA, NSULF /)
   CALL netCDF_Write_Var( UInc, REAL(NR, ncKind),  'SulfSpecMR', st2D, ct2D)

   ! Dust Phase function
   CALL netCDF_Write_Var( UInc, OPT_DUST%ANGLE, 'SANGLE', 1, NANG)
 
   ! BC total volume
   CALL netCDF_Write_Var( UInc, VBC, 'BcTotalV', 1, NBC2SULF_MASS)   

   ! Sulf to BC radius ratio
   st2D(:) = 1
   ct2D = (/ NBC2SULF_MASS, NRH_LST /)
   CALL netCDF_Write_Var( UInc, SULFBCRRATIO, 'SulfBcRRATIO', st2D, ct2D)  
   
   ! Sulf-BC growth factor
   st2D(:) = 1
   ct2D = (/ NBC2SULF_MASS, NRH_LST /)
   CALL netCDF_Write_Var( UInc, REAL(SULFBCGF, ncKind),  'SulfBcGF', st2D, ct2D)

   ! Sulf-BC mass growth factor
   st2D(:) = 1
   ct2D = (/ NBC2SULF_MASS, NRH_LST /)
   CALL netCDF_Write_Var( UInc, REAL(SULFBCMGF, ncKind),  'SulfBcMGF', st2D, ct2D)

   ! Sulf-BC density
   st2D(:) = 1
   ct2D = (/ NBC2SULF_MASS, NRH_LST /)
   CALL netCDF_Write_Var( UInc, REAL(SULFBCRHO, ncKind),  'SulfBcRHO', st2D, ct2D)
   
   RETURN
   !
   END SUBROUTINE WRITE_DIAG01
    
!------------------------------------------------------------------------------
   SUBROUTINE WRITE_DIAG02( UInc )
   
   ! mixing parameters that are not related to wavelength
   USE NETCDF_MOD
   
   USE SPECTRA_MOD,        ONLY : NSPECTRA
   USE SPECTRA_MOD,        ONLY : LAMDAS, WAVENUMS  
   USE MIX_MOD,			   ONLY : OPT_BC
   USE MIX_MOD,			   ONLY : OPT_DUST
   USE MIX_MOD, 		   ONLY : NANG
   USE MIX_MOD,            ONLY : LAMDAID
   
   IMPLICIT NONE
   !
   ! Arguments:
   INTEGER, INTENT(IN)        :: UInc
   
   ! Local variables
   INTEGER						:: st1D, st2D(2)
   INTEGER						:: ct1D, ct2D(2) 
   REAL*4, ALLOCATABLE			:: tmpArr1D(:)
   REAL*4, ALLOCATABLE			:: tmpArr2D(:, :)
   
   st1D = LAMDAID
   ct1D = 1
   
   ! BC Single scattering albedo
   ALLOCATE( tmpArr1D(1) )
   tmpArr1D(1) = OPT_BC%SSA
   CALL netCDF_Write_Var( UInc, tmpArr1D, 'BcSSA', st1D, ct1D)
   DEALLOCATE( tmpArr1D )
   
   ! BC Asymmetric factor
   ALLOCATE( tmpArr1D(1) )
   tmpArr1D(1) = OPT_BC%ASY
   CALL netCDF_Write_Var( UInc, tmpArr1D, 'BcASY', st1D, ct1D)
   DEALLOCATE( tmpArr1D )
   
   ! BC Extinction cross section
   ALLOCATE( tmpArr1D(1) )
   tmpArr1D(1) = OPT_BC%CEXT
   CALL netCDF_Write_Var( UInc, tmpArr1D, 'BcCEXT', st1D, ct1D)
   DEALLOCATE( tmpArr1D )
   
   ! BC Scattering cross section
   ALLOCATE( tmpArr1D(1) )
   tmpArr1D(1) = OPT_BC%CSCA
   CALL netCDF_Write_Var( UInc, tmpArr1D, 'BcCSCA', st1D, ct1D)
   DEALLOCATE( tmpArr1D )   

   ! BC Extinction coefficient
   ALLOCATE( tmpArr1D(1) )
   tmpArr1D(1) = OPT_BC%EXTEFFCY
   CALL netCDF_Write_Var( UInc, tmpArr1D, 'BcEXTEFFCY', st1D, ct1D)
   DEALLOCATE( tmpArr1D )   

   ! BC Mass extinction coefficient
   ALLOCATE( tmpArr1D(1) )
   tmpArr1D(1) = OPT_BC%MEXTEFFCY
   CALL netCDF_Write_Var( UInc, tmpArr1D, 'BcMEXTEFFCY', st1D, ct1D)
   DEALLOCATE( tmpArr1D ) 

   ! BC Hemispheric backscattering
   ALLOCATE( tmpArr1D(1) )
   tmpArr1D(1) = OPT_BC%HBSCAT
   CALL netCDF_Write_Var( UInc, tmpArr1D, 'BcHBSCAT', st1D, ct1D)
   DEALLOCATE( tmpArr1D ) 

   ! BC Extinction to back scattering ratio
   ALLOCATE( tmpArr1D(1) )
   tmpArr1D(1) = OPT_BC%EXTTOBCARATIO
   CALL netCDF_Write_Var( UInc, tmpArr1D, 'BcEXTTOBCARATIO', st1D, ct1D)
   DEALLOCATE( tmpArr1D ) 
   
   ! BC Phase function
   ALLOCATE( tmpArr2D(1, NANG) )
   tmpArr2D(1, 1: NANG) = OPT_BC%PHASE(1: NANG)
   st2D = (/LAMDAID, 1/)
   ct2D = (/1, NANG/)
   CALL netCDF_Write_Var( UInc, tmpArr2D, 'BcPHASE', st2D, ct2D)
   DEALLOCATE( tmpArr2D ) 

   ! Dust...
   ! Dust Single scattering albedo
   ALLOCATE( tmpArr1D(1) )
   tmpArr1D(1) = OPT_DUST%SSA
   CALL netCDF_Write_Var( UInc, tmpArr1D, 'DustSSA', st1D, ct1D)
   DEALLOCATE( tmpArr1D )
   
   ! Dust Asymmetric factor
   ALLOCATE( tmpArr1D(1) )
   tmpArr1D(1) = OPT_DUST%ASY
   CALL netCDF_Write_Var( UInc, tmpArr1D, 'DustASY', st1D, ct1D)
   DEALLOCATE( tmpArr1D )
   
   ! Dust Extinction cross section
   ALLOCATE( tmpArr1D(1) )
   tmpArr1D(1) = OPT_DUST%CEXT
   CALL netCDF_Write_Var( UInc, tmpArr1D, 'DustCEXT', st1D, ct1D)
   DEALLOCATE( tmpArr1D )
   
   ! Dust Scattering cross section
   ALLOCATE( tmpArr1D(1) )
   tmpArr1D(1) = OPT_DUST%CSCA
   CALL netCDF_Write_Var( UInc, tmpArr1D, 'DustCSCA', st1D, ct1D)
   DEALLOCATE( tmpArr1D )   

   ! Dust Extinction coefficient
   ALLOCATE( tmpArr1D(1) )
   tmpArr1D(1) = OPT_DUST%EXTEFFCY
   CALL netCDF_Write_Var( UInc, tmpArr1D, 'DustEXTEFFCY', st1D, ct1D)
   DEALLOCATE( tmpArr1D )   

   ! Dust Mass extinction coefficient
   ALLOCATE( tmpArr1D(1) )
   tmpArr1D(1) = OPT_DUST%MEXTEFFCY
   CALL netCDF_Write_Var( UInc, tmpArr1D, 'DustMEXTEFFCY', st1D, ct1D)
   DEALLOCATE( tmpArr1D ) 

   ! Dust Hemispheric backscattering
   ALLOCATE( tmpArr1D(1) )
   tmpArr1D(1) = OPT_DUST%HBSCAT
   CALL netCDF_Write_Var( UInc, tmpArr1D, 'DustHBSCAT', st1D, ct1D)
   DEALLOCATE( tmpArr1D ) 

   ! Dust Extinction to back scattering ratio
   ALLOCATE( tmpArr1D(1) )
   tmpArr1D(1) = OPT_DUST%EXTTOBCARATIO
   CALL netCDF_Write_Var( UInc, tmpArr1D, 'DustEXTTOBCARATIO', st1D, ct1D)
   DEALLOCATE( tmpArr1D ) 
     
   ! Dust Phase function
   ALLOCATE( tmpArr2D(1, NANG) )
   tmpArr2D(1, 1: NANG) = OPT_DUST%PHASE(1: NANG)
   st2D = (/LAMDAID, 1/)
   ct2D = (/1, NANG/)
   CALL netCDF_Write_Var( UInc, tmpArr2D, 'DustPHASE', st2D, ct2D)
   DEALLOCATE( tmpArr2D )
   
   
   RETURN
   !
   END SUBROUTINE WRITE_DIAG02

!------------------------------------------------------------------------------
   SUBROUTINE WRITE_DIAG03( UInc )
   
   USE NETCDF_MOD
   USE MIX_MOD,			   ONLY : OPT_SULF
   USE MIX_MOD,            ONLY : RHID
   
   IMPLICIT NONE
   !
   ! Arguments:
   INTEGER, INTENT(IN)          :: UInc
   
   ! Local variables
   INTEGER						:: st1D
   INTEGER						:: ct1D
   REAL*4, ALLOCATABLE			:: tmpArr1D(:)
   
   st1D = RHID
   ct1D = 1
 
   ! Sulfate Sulfate composite Effective radius
   ALLOCATE( tmpArr1D(1) )
   tmpArr1D(1) = OPT_SULF%REFF
   CALL netCDF_Write_Var( UInc, tmpArr1D, 'SulfCompREFF', st1D, ct1D)
   DEALLOCATE( tmpArr1D )    

   ! Sulfate composite Effective variance
   ALLOCATE( tmpArr1D(1) )
   tmpArr1D(1) = OPT_SULF%VEFF
   CALL netCDF_Write_Var( UInc, tmpArr1D, 'SulfCompVEFF', st1D, ct1D)
   DEALLOCATE( tmpArr1D )   

   ! Sulfate composite Surface area
   ALLOCATE( tmpArr1D(1) )
   tmpArr1D(1) = OPT_SULF%AREA
   CALL netCDF_Write_Var( UInc, tmpArr1D, 'SulfCompAREA', st1D, ct1D)
   DEALLOCATE( tmpArr1D )   

   ! Sulfate composite  Volume
   ALLOCATE( tmpArr1D(1) )
   tmpArr1D(1) = OPT_SULF%VOLUME
   CALL netCDF_Write_Var( UInc, tmpArr1D, 'SulfCompVOLUME', st1D, ct1D)
   DEALLOCATE( tmpArr1D )   
  
   RETURN
   !
   END SUBROUTINE WRITE_DIAG03

!------------------------------------------------------------------------------
   SUBROUTINE WRITE_DIAG04( UInc )
   
   USE NETCDF_MOD
   USE MIX_MOD,			   ONLY : OPT_SULF
   USE MIX_MOD,            ONLY : RHID
   USE MIX_MOD,            ONLY : LAMDAID
   USE MIX_MOD, 		   ONLY : NANG   
   
   IMPLICIT NONE
   !
   ! Arguments:
   INTEGER, INTENT(IN)          :: UInc
   
   ! Local variables
   INTEGER						:: st2D(2), st3D(3)
   INTEGER						:: ct2D(2), ct3D(3)  
   REAL*4, ALLOCATABLE			:: tmpArr2D(:, :), tmpArr3D(:, :, :)
  
   st2D = (/LAMDAID, RHID/)
   ct2D = (/1, 1/)  
   
   ! Sulfate composite Single scattering albedo
   ALLOCATE( tmpArr2D(1, 1) )
   tmpArr2D(1, 1) = OPT_SULF%SSA
   CALL netCDF_Write_Var( UInc, tmpArr2D, 'SulfCompSSA', st2D, ct2D)
   DEALLOCATE( tmpArr2D ) 

   ! Sulfate composite Asymmetric factor
   ALLOCATE( tmpArr2D(1, 1) )
   tmpArr2D(1, 1) = OPT_SULF%ASY
   CALL netCDF_Write_Var( UInc, tmpArr2D, 'SulfCompASY', st2D, ct2D)
   DEALLOCATE( tmpArr2D ) 
   
   ! Sulfate composite Extinction cross section
   ALLOCATE( tmpArr2D(1, 1) )
   tmpArr2D(1, 1) = OPT_SULF%CEXT
   CALL netCDF_Write_Var( UInc, tmpArr2D, 'SulfCompCEXT', st2D, ct2D)
   DEALLOCATE( tmpArr2D ) 
   
   ! Sulfate composite Scattering cross section
   ALLOCATE( tmpArr2D(1, 1) )
   tmpArr2D(1, 1) = OPT_SULF%CSCA
   CALL netCDF_Write_Var( UInc, tmpArr2D, 'SulfCompCSCA', st2D, ct2D)
   DEALLOCATE( tmpArr2D )    
   
   ! Sulfate composite Extinction coefficient
   ALLOCATE( tmpArr2D(1, 1) )
   tmpArr2D(1, 1) = OPT_SULF%EXTEFFCY
   CALL netCDF_Write_Var( UInc, tmpArr2D, 'SulfCompEXTEFFCY', st2D, ct2D)
   DEALLOCATE( tmpArr2D ) 
   
   ! Sulfate composite Mass extinction coefficient
   ALLOCATE( tmpArr2D(1, 1) )
   tmpArr2D(1, 1) = OPT_SULF%MEXTEFFCY
   CALL netCDF_Write_Var( UInc, tmpArr2D, 'SulfCompMEXTEFFCY', st2D, ct2D)
   DEALLOCATE( tmpArr2D )    
   
   ! Sulfate composite Hemispheric backscattering
   ALLOCATE( tmpArr2D(1, 1) )
   tmpArr2D(1, 1) = OPT_SULF%HBSCAT
   CALL netCDF_Write_Var( UInc, tmpArr2D, 'SulfCompHBSCAT', st2D, ct2D)
   DEALLOCATE( tmpArr2D )      

   ! Sulfate composite Extinction to back scattering ratio
   ALLOCATE( tmpArr2D(1, 1) )
   tmpArr2D(1, 1) = OPT_SULF%SSA
   CALL netCDF_Write_Var( UInc, tmpArr2D, 'SulfCompEXTTOBCARATIO', st2D, ct2D)
   DEALLOCATE( tmpArr2D ) 

   ! Sulfate composite Extinction to back scattering ratio
   ALLOCATE( tmpArr2D(1, 1) )
   tmpArr2D(1, 1) = OPT_SULF%SSA
   CALL netCDF_Write_Var( UInc, tmpArr2D, 'SulfCompEXTTOBCARATIO', st2D, ct2D)
   DEALLOCATE( tmpArr2D ) 

   ! Sulfate composite Phase function
   ALLOCATE( tmpArr3D(1, 1, NANG) )
   tmpArr3D(1, 1, 1:NANG) = OPT_SULF%PHASE(1:NANG)
   st3D = (/LAMDAID, RHID, 1/)
   ct3D = (/1, 1, NANG/) 
   CALL netCDF_Write_Var( UInc, tmpArr3D, 'SulfCompPHASE', st3D, ct3D)
   DEALLOCATE( tmpArr3D ) 

   RETURN
   !
   END SUBROUTINE WRITE_DIAG04

!------------------------------------------------------------------------------
   SUBROUTINE WRITE_DIAG05( UInc )
   
   ! mixing parameters that are not related to wavelength
   USE NETCDF_MOD
   
   USE SPECTRA_MOD,        ONLY : NSPECTRA
   USE NAMELIST_ARRAY_MOD, ONLY : NRH_LST
   USE NAMELIST_ARRAY_MOD, ONLY : NBC2SULF_MASS
   USE NAMELIST_ARRAY_MOD, ONLY : NDUST2ALL_MASS
   USE MIX_MOD, 		   ONLY : NANG
   
   USE NAMELIST_ARRAY_MOD, ONLY : LEXTERNAL_MIX
   USE NAMELIST_ARRAY_MOD, ONLY : LINTERNAL_MIX

   
   USE MIX_MOD,			   ONLY : OPT_BC
   USE MIX_MOD,			   ONLY : OPT_MIX
   
   USE MIX_MOD,            ONLY : LAMDAID
   USE MIX_MOD,            ONLY : RHID
   USE MIX_MOD,            ONLY : BCID
   USE MIX_MOD,            ONLY : DUSTID
   USE MIX_MOD,            ONLY : MIXINGCEXT
   USE MIX_MOD,            ONLY : MIXINGSSA
   USE MIX_MOD,            ONLY : MIXINGREFF
   USE MIX_MOD,            ONLY : MIXINGPHASE
   USE MIX_MOD,            ONLY : TOTALNUM
   
   USE MIX_MOD,            ONLY : CSMIXINGCEXT
   USE MIX_MOD,            ONLY : CSMIXINGSSA
   USE MIX_MOD,            ONLY : CSMIXINGREFF
   USE MIX_MOD,            ONLY : CSMIXINGPHASE
   USE MIX_MOD,            ONLY : CSTOTALNUM
   
   IMPLICIT NONE
   !
   ! Arguments:
   INTEGER, INTENT(IN)          :: UInc
   INTEGER						:: st4D(4), st5D(5)
   INTEGER						:: ct4D(4), ct5D(5)  
   REAL*4, ALLOCATABLE			:: tmpArr4D(:, :, :, :)
   REAL*4, ALLOCATABLE			:: tmpArr5D(:, :, :, :, :)
 
 
   IF (LEXTERNAL_MIX) THEN
      ! External mixing Extinction cross section
      PRINT *, MIXINGCEXT
      ALLOCATE( tmpArr4D(1, 1, 1, 1) )
      tmpArr4D(1, 1, 1, 1) = MIXINGCEXT
      st4D = (/LAMDAID, RHID, DUSTID, BCID/)
      ct4D = (/1, 1, 1, 1/) 
      CALL netCDF_Write_Var( UInc, tmpArr4D, 'ExMixCEXT', st4D, ct4D)
      DEALLOCATE( tmpArr4D )     

      ! External mixing Single scattering albedo
      ALLOCATE( tmpArr4D(1, 1, 1, 1) )
      tmpArr4D(1, 1, 1, 1) = MIXINGSSA
      st4D = (/LAMDAID, RHID, DUSTID, BCID/)
      ct4D = (/1, 1, 1, 1/) 
      CALL netCDF_Write_Var( UInc, tmpArr4D, 'ExMixSSA', st4D, ct4D)
      DEALLOCATE( tmpArr4D ) 
      
      ! External mixing Effective radius
      ALLOCATE( tmpArr4D(1, 1, 1, 1) )
      tmpArr4D(1, 1, 1, 1) = MIXINGREFF
      st4D = (/LAMDAID, RHID, DUSTID, BCID/)
      ct4D = (/1, 1, 1, 1/) 
      CALL netCDF_Write_Var( UInc, tmpArr4D, 'ExMixREFF', st4D, ct4D)
      DEALLOCATE( tmpArr4D ) 
      
      ! External mixing Total Number
      ALLOCATE( tmpArr4D(1, 1, 1, 1) )
      tmpArr4D(1, 1, 1, 1) = TOTALNUM
      st4D = (/LAMDAID, RHID, DUSTID, BCID/)
      ct4D = (/1, 1, 1, 1/) 
      CALL netCDF_Write_Var( UInc, tmpArr4D, 'ExMixNUM', st4D, ct4D)
      DEALLOCATE( tmpArr4D )       

       ! External mixing Total Number
      ALLOCATE( tmpArr5D(1, 1, 1, 1, NANG) )
      tmpArr5D(1, 1, 1, 1, 1:NANG) = MIXINGPHASE(1:NANG)
      st5D = (/LAMDAID, RHID, DUSTID, BCID, 1/)
      ct5D = (/1, 1, 1, 1, NANG/) 
      CALL netCDF_Write_Var( UInc, tmpArr5D, 'ExMixPHASE', st5D, ct5D)
      DEALLOCATE( tmpArr5D )   
   
   END IF


   IF (LINTERNAL_MIX) THEN
      ! Internal mixing Extinction cross section
      ALLOCATE( tmpArr4D(1, 1, 1, 1) )
      tmpArr4D(1, 1, 1, 1) = CSMIXINGCEXT
      st4D = (/LAMDAID, RHID, DUSTID, BCID/)
      ct4D = (/1, 1, 1, 1/) 
      CALL netCDF_Write_Var( UInc, tmpArr4D, 'InMixCEXT', st4D, ct4D)
      DEALLOCATE( tmpArr4D )     

      ! Internal mixing Single scattering albedo
      ALLOCATE( tmpArr4D(1, 1, 1, 1) )
      tmpArr4D(1, 1, 1, 1) = CSMIXINGSSA
      st4D = (/LAMDAID, RHID, DUSTID, BCID/)
      ct4D = (/1, 1, 1, 1/) 
      CALL netCDF_Write_Var( UInc, tmpArr4D, 'InMixSSA', st4D, ct4D)
      DEALLOCATE( tmpArr4D ) 
      
      ! Internal mixing Effective radius
      ALLOCATE( tmpArr4D(1, 1, 1, 1) )
      tmpArr4D(1, 1, 1, 1) = CSMIXINGREFF
      st4D = (/LAMDAID, RHID, DUSTID, BCID/)
      ct4D = (/1, 1, 1, 1/) 
      CALL netCDF_Write_Var( UInc, tmpArr4D, 'InMixREFF', st4D, ct4D)
      DEALLOCATE( tmpArr4D ) 
      
      ! Internal mixing Total Number
      ALLOCATE( tmpArr4D(1, 1, 1, 1) )
      tmpArr4D(1, 1, 1, 1) = CSTOTALNUM
      st4D = (/LAMDAID, RHID, DUSTID, BCID/)
      ct4D = (/1, 1, 1, 1/) 
      CALL netCDF_Write_Var( UInc, tmpArr4D, 'InMixNUM', st4D, ct4D)
      DEALLOCATE( tmpArr4D )       

       ! Internal mixing Total Number
      ALLOCATE( tmpArr5D(1, 1, 1, 1, NANG) )
      tmpArr5D(1, 1, 1, 1,1:NANG) = CSMIXINGPHASE(1:NANG)
      st5D = (/LAMDAID, RHID, DUSTID, BCID, 1/)
      ct5D = (/1, 1, 1, 1, NANG/) 
      CALL netCDF_Write_Var( UInc, tmpArr5D, 'InMixPHASE', st5D, ct5D)
      DEALLOCATE( tmpArr5D )   
   
   END IF
   

   RETURN
   !
   END SUBROUTINE WRITE_DIAG05
!------------------------------------------------------------------------------
!
   SUBROUTINE CLOSEUP_DIAG( UInc )
   !
   ! USES:
   USE NETCDF_MOD
   !
   IMPLICIT NONE
   !
   ! Arguments:
   INTEGER, INTENT(IN)         :: UInc
   !
   ! Local variables
   INTEGER                     :: vID

   ! Go to define mode
   CALL netCDF_Begin_Def ( UInc )
  
   ! Write one last attribute to indicate a sucessful write
   vID = -999
   CALL netCDF_Def_Attr( UInc, vID, 'Yes', 'Success' )

   ! Close the netCDF file and the diaginfo file
   CALL netCDF_Close( UInc )

   END SUBROUTINE CLOSEUP_DIAG
!------------------------------------------------------------------------------
   !
   SUBROUTINE CREATE_DIAGINFO ( IUinfo, infoFile, ncFile )
   !
   ! USES:
   USE ERROR_MOD,          ONLY : ERROR_STOP
   USE TIME_MOD,           ONLY : SYSTEM_TIMESTAMP 
   !
   IMPLICIT NONE  
   !
   ! Arguments
   INTEGER, INTENT(IN)         :: IUinfo
   CHARACTER(LEN=*), INTENT(IN):: infoFile
   CHARACTER(LEN=*), INTENT(IN):: ncFile
   !
   ! Local Variables
   CHARACTER(LEN=128)          :: Msg
   CHARACTER(LEN=16)           :: Stamp
   INTEGER                     :: ios
   !
   ! CREATE_DIAGINFO begins here ...
   !
   ! Create a text file
   OPEN( UNIT=IUinfo, FILE=infoFile, STATUS='replace', ACTION='write', IOSTAT=ios )
      print*, 'ios = ', ios
   IF ( ios /= 0 ) THEN 
      Msg = 'Cannot open the file: ' // infoFile
      CALL ERROR_STOP( Msg, 'diag_mod.f90' )
   ENDIF

   ! Write file header
    Stamp = SYSTEM_TIMESTAMP()
    WRITE(IUinfo,'(A)') '#' // REPEAT( '=', 78 )
    WRITE(IUinfo,  100) Stamp
    WRITE(IUinfo,  105) 
    WRITE(IUinfo,  110) TRIM( ncFile )
    WRITE(IUinfo,  115)
    WRITE(IUinfo,'(A)') '#' // REPEAT( '=', 78 )

    100 FORMAT( 1X, 'This file is created by Hygroscopic and Mxing at ', A )
    105 FORMAT( 1X, 'It contains info for variables in the output netCDF file:' )
    110 FORMAT( 3X, '"', A, '"', / )
    115 FORMAT( 1X, ' Name:     Units:     Longname:' ) 


    END SUBROUTINE CREATE_DIAGINFO 
!------------------------------------------------------------------------------
 
END MODULE DIAG_MOD
 