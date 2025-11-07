! $Id: namelist_mod.f, v1.0
   MODULE NAMELIST_MOD
!******************************************************************************
!  Module NANMELIST_MOD reads the namelist input file at the start run and
!  passes the information to several other modules
!
!
!  Module Routines
!  ============================================================================
!  (1 ) READ_NAMELIST        : Main subroutine to read the file namelist.ini
!
!******************************************************************************   
   
   IMPLICIT NONE
   
   !=================================================================
   ! MODULE PRIVATE DECLARATIONS -- keep certain internal variables 
   ! and routines from being seen outside "namelist_mod.f"
   !=================================================================

   ! Make everything PRIVATE ...
   PRIVATE

   ! ... except these routines
   PUBLIC :: READ_NAMELIST
   PUBLIC :: READ_ONE_LINE
   PUBLIC :: SPLIT_ONE_LINE
   
   
   ! ... and these variables
   PUBLIC :: IU_NAME
   PUBLIC :: MAXDIM

   !=================================================================
   ! MODULE VARIABLES 
   !=================================================================
   LOGICAL            :: VERBOSE  = .FALSE.
   INTEGER, PARAMETER :: FIRSTCOL = 26
   INTEGER, PARAMETER :: MAXDIM   = 255
   !CHARACTER(LEN=255) :: FILENAME = 'namelist.ini'
   INTEGER, PARAMETER :: IU_NAME  = 11

   !=================================================================
   ! MODULE ROUTINES -- follow below the "CONTAINS" statement 
   !=================================================================
   CONTAINS   
   
!------------------------------------------------------------------------------

   SUBROUTINE READ_NAMELIST
   !
   !******************************************************************************
   !  Subroutine READ_NAMELIST is the driver program for reading the NAMELIST
   !  input file "namelist.ini" from disk.
   !******************************************************************************
   ! References to F90 modules
   USE CHARPAK_MOD, ONLY : STRREPL
   USE ERROR_MOD,   ONLY : ERROR_STOP


   ! Local variables
   CHARACTER(LEN=255) :: FILENAME = 'namelist.ini'
   CHARACTER(LEN=255) :: TOPTITLE
   LOGICAL            :: EOF
   INTEGER            :: IOS
   CHARACTER(LEN=1)   :: TAB   = ACHAR(9)
   CHARACTER(LEN=1)   :: SPACE = ' '
   CHARACTER(LEN=255) :: LINE
   CHARACTER(LEN=255) :: MESSAGE
   
   !=================================================================
   ! READ_INPUT_FILE begins here!
   !=================================================================
   WRITE( 6, '(a  )' ) REPEAT( '=', 79 )
   WRITE( 6, 100   ) TRIM( FILENAME )
   100  FORMAT( ' - READ_NAMELIST: Reading ', a )
 
   
   ! Open file
   OPEN( IU_NAME, FILE=TRIM( FILENAME ), STATUS='OLD', IOSTAT=IOS )
   IF ( IOS /= 0 ) THEN
      MESSAGE = 'Fail to open file: '//TRIM( FILENAME )
      CALL ERROR_STOP( MESSAGE, 'namelist_mod.f' )
   ENDIF

   ! Read TOPTITLE for binary punch file
   TOPTITLE = READ_ONE_LINE( EOF  )
   IF ( EOF ) RETURN

   ! Loop until EOF
   DO

      ! Read a line from the file, exit if EOF
      LINE = READ_ONE_LINE( EOF )
      IF ( EOF ) EXIT

      ! Replace tab characters in LINE (if any) w/ spaces
      CALL STRREPL( LINE, TAB, SPACE )

      IF ( INDEX( LINE, 'CONTROL MENU'   ) > 0 ) THEN
         CALL READ_CONTROL_MENU
      ELSE IF ( INDEX( LINE, 'RADIATION MENU'   ) > 0 ) THEN
         CALL READ_RADIATION_MENU
      ELSE IF ( INDEX( LINE, 'AEROSOL MENU'   ) > 0 ) THEN
          CALL READ_AEROSOL_MENU
      ELSE IF ( INDEX( LINE, 'DIAGNOSTIC MENU'   ) > 0 ) THEN    
          CALL READ_DIAGNOSTIC_MENU
	  ENDIF
	  
   ENDDO
   
   ! Close input file
   CLOSE( IU_NAME )

   WRITE(6,110)
   WRITE(6,'(A)') REPEAT( '=', 79 )   
   110  FORMAT(" - READ_NAMELIST: complete reading the namelist ")
   
   END SUBROUTINE READ_NAMELIST

   
   
   
!------------------------------------------------------------------------------

   FUNCTION READ_ONE_LINE( EOF, LOCATION ) RESULT( LINE )
!
!******************************************************************************
!  Subroutine READ_ONE_LINE reads a line from the input file.  If the global 
!  variable VERBOSE is set, the line will be printed to stdout.  READ_ONE_LINE
!  can trap an unexpected EOF if LOCATION is passed.  Otherwise, it will pass
!  a logical flag back to the calling routine, where the error trapping will
!  be done. (bmy, 7/20/04)
! 
!  Arguments as Output:
!  ===========================================================================
!  (1 ) EOF      (CHARACTER) : Logical flag denoting EOF condition
!  (2 ) LOCATION (CHARACTER) : Name of calling routine; traps premature EOF
!
!  Function value:
!  ===========================================================================
!  (1 ) LINE     (CHARACTER) : A line of text as read from the file
!
!  NOTES:
!******************************************************************************
!      
   ! References to F90 modules
   USE ERROR_MOD, ONLY : ERROR_STOP

   ! Arguments
   LOGICAL,          INTENT(OUT)          :: EOF
   CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: LOCATION

   ! Local variables
   INTEGER                                :: IOS
   CHARACTER(LEN=255)                     :: LINE, MSG

   !=================================================================
   ! READ_ONE_LINE begins here!
   !=================================================================

   ! Initialize
   EOF = .FALSE.

   ! Read a line from the file
   READ( IU_NAME, '(a)', IOSTAT=IOS ) LINE

   ! IO Status < 0: EOF condition
   IF ( IOS < 0 ) THEN
      EOF = .TRUE.

      ! Trap unexpected EOF -- stop w/ error msg if LOCATION is passed
      ! Otherwise, return EOF to the calling program
      IF ( PRESENT( LOCATION ) ) THEN
         MSG = 'READ_ONE_LINE: error at: ' // TRIM( LOCATION )
         WRITE( 6, '(a)' ) MSG
         WRITE( 6, '(a)' ) 'Unexpected end of file encountered!'
         WRITE( 6, '(a)' ) 'STOP in READ_ONE_LINE (namelist_mod.f)'
         WRITE( 6, '(a)' ) REPEAT( '=', 79 )
         STOP
      ELSE
         RETURN
      ENDIF
   ENDIF

   ! IO Status > 0: true I/O error condition
   IF ( IOS > 0 ) CALL ERROR_STOP( 'Error when reading', &                                
                                   'read_one_line:1' )

   ! Print the line (if necessary)
   IF ( VERBOSE ) WRITE( 6, '(a)' ) TRIM( LINE )

   ! Return to calling program
   END FUNCTION READ_ONE_LINE

!------------------------------------------------------------------------------

   SUBROUTINE SPLIT_ONE_LINE( SUBSTRS, N_SUBSTRS, N_EXP, LOCATION )
!
!******************************************************************************
!  Subroutine SPLIT_ONE_LINE reads a line from the input file (via routine 
!  READ_ONE_LINE), and separates it into substrings. (bmy, 7/20/04)
!
!  SPLIT_ONE_LINE also checks to see if the number of substrings found is 
!  equal to the number of substrings that we expected to find.  However, if
!  you don't know a-priori how many substrings to expect a-priori, 
!  you can skip the error check.
! 
!  Arguments as Input:
!  ===========================================================================
!  (3 ) N_EXP     (INTEGER  ) : Number of substrings we expect to find
!                               (N_EXP < 0 will skip the error check!)
!  (4 ) LOCATION  (CHARACTER) : Name of routine that called SPLIT_ONE_LINE
!
!  Arguments as Output:
!  ===========================================================================
!  (1 ) SUBSTRS   (CHARACTER) : Array of substrings (separated by " ")
!  (2 ) N_SUBSTRS (INTEGER  ) : Number of substrings actually found
!
!  NOTES:
!******************************************************************************
!
   ! References to F90 modules
   USE CHARPAK_MOD, ONLY: STRSPLIT

   ! Arguments
   CHARACTER(LEN=255), INTENT(OUT) :: SUBSTRS(MAXDIM)
   INTEGER,            INTENT(OUT) :: N_SUBSTRS
   INTEGER,            INTENT(IN)  :: N_EXP
   CHARACTER(LEN=*),   INTENT(IN)  :: LOCATION

   ! Local varaibles
   LOGICAL                         :: EOF
   CHARACTER(LEN=255)              :: LINE, MSG

   !=================================================================
   ! SPLIT_ONE_LINE begins here!
   !=================================================================  

   ! Create error msg
   MSG = 'SPLIT_ONE_LINE: error at ' // TRIM( LOCATION )

   !=================================================================
   ! Read a line from disk
   !=================================================================
   LINE = READ_ONE_LINE( EOF )

   ! STOP on End-of-File w/ error msg
   IF ( EOF ) THEN
      WRITE( 6, '(a)' ) TRIM( MSG )
      WRITE( 6, '(a)' ) 'End of file encountered!'
      WRITE( 6, '(a)' ) 'STOP in SPLIT_ONE_LINE (namelist_mod.f)!'
      WRITE( 6, '(a)' ) REPEAT( '=', 79 )
      STOP
   ENDIF

   !=================================================================
   ! Split the lines between spaces -- start at column FIRSTCOL
   !=================================================================
   CALL STRSPLIT( LINE(FIRSTCOL:), ' ', SUBSTRS, N_SUBSTRS )

   ! Sometimes we don't know how many substrings to expect,
   ! if N_EXP is greater than MAXDIM, then skip the error check
   IF ( N_EXP < 0 ) RETURN

   ! Stop if we found the wrong 
   IF ( N_EXP /= N_SUBSTRS ) THEN
      WRITE( 6, '(a)' ) TRIM( MSG )
      WRITE( 6, 100   ) N_EXP, N_SUBSTRS
      WRITE( 6, '(a)' ) 'STOP in SPLIT_ONE_LINE (namelist_mod.f)!'
      WRITE( 6, '(a)' ) REPEAT( '=', 79 )
      STOP
100   FORMAT( 'Expected ',i2, ' substrs but found ',i3 )
   ENDIF

   ! Return to calling program
   END SUBROUTINE SPLIT_ONE_LINE

!------------------------------------------------------------------------------

   SUBROUTINE READ_CONTROL_MENU
   
   ! References to F90 modules
   USE NAMELIST_ARRAY_MOD, ONLY : DIR_RUN
   USE NAMELIST_ARRAY_MOD, ONLY : DIR_DATA,     DIR_OUTPUT   
   USE ERROR_MOD,          ONLY : ERROR_STOP
   
   ! Local variables
   INTEGER            :: N
   CHARACTER(LEN=255) :: SUBSTRS(MAXDIM) 
   CHARACTER(LEN=255) :: ERRMSG   
   !=================================================================
   ! READ_CONTROL_MENU begins here!
   !=================================================================
   
   ! Run dir
   CALL SPLIT_ONE_LINE( SUBSTRS, N, 1, 'read_control_menu:0' )
   READ( SUBSTRS(1:N), '(a)' ) DIR_RUN

   ! Data dir
   ! Run dir
   CALL SPLIT_ONE_LINE( SUBSTRS, N, 1, 'read_control_menu:1' )
   READ( SUBSTRS(1:N), '(a)' ) DIR_DATA

   ! Output dir
   CALL SPLIT_ONE_LINE( SUBSTRS, N, 1, 'read_control_menu:2' )
   READ( SUBSTRS(1:N), '(a)' ) DIR_OUTPUT

   ! Separator line
   CALL SPLIT_ONE_LINE( SUBSTRS, N, 1, 'read_control_menu:3' )
   
   WRITE( 6, 150) '' 
   WRITE( 6, 150) 'CONTROL MENU:' 
   WRITE( 6, 150) REPEAT( '-', 48 )
   WRITE( 6, 120 ) 'Run    directory        :', TRIM(DIR_RUN    )
   WRITE( 6, 120 ) 'Data   directory        :', TRIM(DIR_DATA   )
   WRITE( 6, 120 ) 'Output directory        :', TRIM(DIR_OUTPUT )   
   120  FORMAT(5X, A, A )
   150  FORMAT(5X, A ) 

   CALL CHECK_DIRECTORY( DIR_RUN    )
   CALL CHECK_DIRECTORY( DIR_DATA   )
   CALL CHECK_DIRECTORY( DIR_OUTPUT )
   
   
      
   END SUBROUTINE READ_CONTROL_MENU

!------------------------------------------------------------------------------

   SUBROUTINE READ_RADIATION_MENU
   
   ! References to F90 modules
   USE NAMELIST_ARRAY_MOD, ONLY : DIR_RUN
   USE NAMELIST_ARRAY_MOD, ONLY : LFREQUENCY
   USE NAMELIST_ARRAY_MOD, ONLY : NSPECTRA_LST
   USE NAMELIST_ARRAY_MOD, ONLY : SPECTRA_LST
   USE NAMELIST_ARRAY_MOD, ONLY : SPECTRA_STEP
   USE NAMELIST_ARRAY_MOD, ONLY : NRH_LST 
   USE NAMELIST_ARRAY_MOD, ONLY : RH_LST
   USE ERROR_MOD,          ONLY : ERROR_STOP
   
   ! Local variables
   INTEGER            :: N
   CHARACTER(LEN=255) :: SUBSTRS(MAXDIM) 
   CHARACTER(LEN=255) :: ERRMSG   
   !=================================================================
   ! READ_RADIATION_MENU begins here!
   !=================================================================
   
   ! LFREQUENCY
   CALL SPLIT_ONE_LINE( SUBSTRS, N, 1, 'read_radiation_menu:0')
   READ( SUBSTRS(1:N), * ) LFREQUENCY

   ! Start and end Wavelengths
   CALL SPLIT_ONE_LINE( SUBSTRS, N, -1, 'read_radiation_menu:1' )
   ! The wavelengths input here is quite flexible:
   !   N=1, Mono-spectrum
   !   N=2, Multi-spectra with regular intervals
   !   N>2, Direct inputs of Multi-spectra
   ! For detail see the below commented lines 
   NSPECTRA_LST = MIN( N, 99 )
   READ( SUBSTRS(1:NSPECTRA_LST), * ) SPECTRA_LST(1:NSPECTRA_LST)

   ! Spectral step
   CALL SPLIT_ONE_LINE( SUBSTRS, N, 1, 'read_radiation_menu:1a' )
   READ( SUBSTRS(1), * ) SPECTRA_STEP 
   
   ! RH
   CALL SPLIT_ONE_LINE( SUBSTRS, N, -1, 'read_radiation_menu:2' )
   NRH_LST = MIN( N, 99 )
   READ( SUBSTRS(1:NRH_LST), * ) RH_LST(1:NRH_LST)

   ! Separator line
   CALL SPLIT_ONE_LINE( SUBSTRS, N, 1, 'read_radiation_menu:3' )
   
   WRITE( 6, 150) '' 
   WRITE( 6, 150) 'RADIATION MENU:' 
   WRITE( 6, 150) REPEAT( '-', 48 )
   WRITE( 6, 130 ) 'Wavelength as input?    :', LFREQUENCY
   WRITE( 6, 100 ) 'Spectra set (nm or 1/cm):', SPECTRA_LST(1),  &
                   SPECTRA_LST(NSPECTRA_LST)
   WRITE( 6, 100 ) '  - interval(nm or 1/cm):', SPECTRA_STEP
   WRITE( 6, 100 ) 'Relative Humidy         :', RH_LST(1), &
                   RH_LST(NRH_LST)                
   100  FORMAT(5X, A, 2F8.2 )     
   110  FORMAT(5X, A, I5    )
   120  FORMAT(5X, A, A     )
   130  FORMAT(5X, A, L5    )
   140  FORMAT(5X, A, F9.3  )   
   150  FORMAT(5X, A )
   END SUBROUTINE READ_RADIATION_MENU

!------------------------------------------------------------------------------
   SUBROUTINE READ_AEROSOL_MENU
   
   ! References to F90 modules
   USE NAMELIST_ARRAY_MOD, ONLY : LEXTERNAL_MIX
   USE NAMELIST_ARRAY_MOD, ONLY : LINTERNAL_MIX   
   USE NAMELIST_ARRAY_MOD, ONLY : LOC_SHELL
   USE NAMELIST_ARRAY_MOD, ONLY : LSULFATE_SHELL
!    USE NAMELIST_ARRAY_MOD, ONLY : MASS_RATIO
   USE NAMELIST_ARRAY_MOD, ONLY : BC2SULF_MASS
   USE NAMELIST_ARRAY_MOD, ONLY : NBC2SULF_MASS
   USE NAMELIST_ARRAY_MOD, ONLY : BC2OC_MASS
   USE NAMELIST_ARRAY_MOD, ONLY : NBC2OC_MASS
   USE NAMELIST_ARRAY_MOD, ONLY : DUST2ALL_MASS
   USE NAMELIST_ARRAY_MOD, ONLY : NDUST2ALL_MASS
   USE NAMELIST_ARRAY_MOD, ONLY : RMRI
   USE NAMELIST_ARRAY_MOD, ONLY : NSULF
   USE NAMELIST_ARRAY_MOD, ONLY : SULFID_LST
   USE NAMELIST_ARRAY_MOD, ONLY : NSULFMASSRATIO
   USE NAMELIST_ARRAY_MOD, ONLY : SULF_MASSRATIO_LST
   USE NAMELIST_ARRAY_MOD, ONLY : DISTPAR
   USE NAMELIST_ARRAY_MOD, ONLY : BCRHO
   USE NAMELIST_ARRAY_MOD, ONLY : OCRHO
   USE NAMELIST_ARRAY_MOD, ONLY : DUSTRHO
   !USE NAMELIST_ARRAY_MOD, ONLY : N_MASS_RATIO



   ! Local variables
   INTEGER            :: N, I, IMODE
   CHARACTER(LEN=255) :: SUBSTRS(MAXDIM)
   CHARACTER(LEN=255) :: ERRMSG
   REAL*8             :: TEMP2(2)
   ! INTEGER            :: N_MASS_RATIO
   
   !=================================================================
   ! READ_AEROSOL_MENU begins here!
   !=================================================================
   
   ! LEXTERNAL_MIX
    CALL SPLIT_ONE_LINE( SUBSTRS, N, 1, 'read_aerosol_menu:0' )
    READ( SUBSTRS(1:N), * ) LEXTERNAL_MIX
    
   ! LINTERNAL_MIX
   CALL SPLIT_ONE_LINE( SUBSTRS, N, 1, 'read_aerosol_menu:1' )
   READ( SUBSTRS(1:N), * ) LINTERNAL_MIX
   
   ! LSULFATE_SHELL
    CALL SPLIT_ONE_LINE( SUBSTRS, N, 1, 'read_aerosol_menu:2' )
    READ( SUBSTRS(1:N), * ) LSULFATE_SHELL

   ! LOC_SHELL
   CALL SPLIT_ONE_LINE( SUBSTRS, N, 1, 'read_aerosol_menu:3' )
   READ( SUBSTRS(1:N), * ) LOC_SHELL
   
   ! BC to sulfate mass ratio
   CALL SPLIT_ONE_LINE( SUBSTRS, N, -1, 'read_aerosol_menu:4' )
   NBC2SULF_MASS = MIN( N, 99 )
   READ( SUBSTRS(1:NBC2SULF_MASS), * ) BC2SULF_MASS(1:NBC2SULF_MASS) 
   
   ! BC to OC mass ratio
   CALL SPLIT_ONE_LINE( SUBSTRS, N, -1, 'read_aerosol_menu:5' )
   NBC2OC_MASS = MIN( N, 99 )
   READ( SUBSTRS(1:NBC2OC_MASS), * ) BC2OC_MASS(1:NBC2OC_MASS) 
   
   ! dust to all mass ratio
   CALL SPLIT_ONE_LINE( SUBSTRS, N, -1, 'read_aerosol_menu:6' )
   NDUST2ALL_MASS = MIN( N, 99 )
   READ( SUBSTRS(1:NDUST2ALL_MASS), * ) DUST2ALL_MASS(1:NDUST2ALL_MASS)

   ! Sulfate # 1
   IMODE = 1   
   ! Separator line
   CALL SPLIT_ONE_LINE( SUBSTRS, N, -1, 'read_aerosol_menu:s1' )   
   
   ! ID
   CALL SPLIT_ONE_LINE( SUBSTRS, N, -1, 'read_aerosol_menu:s2' )
   NSULF = MIN( N, 99 )
   READ( SUBSTRS(1:NSULF), * ) SULFID_LST(1:NSULF)

   CALL SPLIT_ONE_LINE( SUBSTRS, N, -1, 'read_aerosol_menu:s3' )
   NSULFMASSRATIO = MIN( N, 99 )
   READ( SUBSTRS(1:NSULFMASSRATIO), * ) SULF_MASSRATIO_LST(1:NSULF)

   ! Size range
   CALL SPLIT_ONE_LINE( SUBSTRS, N, 2, 'read_aerosol_menu:s4' )
   READ( SUBSTRS(1), * ) DISTPAR(3, IMODE)
   READ( SUBSTRS(2), * ) DISTPAR(4, IMODE)
   
   ! Separator line
   CALL SPLIT_ONE_LINE( SUBSTRS, N, -1, 'read_aerosol_menu:s5' )
   
   ! Rg Sigma_g
   CALL SPLIT_ONE_LINE( SUBSTRS, N, 2, 'read_aerosol_menu:s6' )
   READ( SUBSTRS(1), * ) DISTPAR(1, IMODE)
   READ( SUBSTRS(2), * ) DISTPAR(2, IMODE)

   ! BC # 2
   IMODE = 2   
   ! Separator line
   CALL SPLIT_ONE_LINE( SUBSTRS, N, -1, 'read_aerosol_menu:b1' )   
   
   ! density
   CALL SPLIT_ONE_LINE( SUBSTRS, N, 1, 'read_aerosol_menu:b2' )
   READ( SUBSTRS(1), * ) BCRHO
      
   ! RMRI
   CALL SPLIT_ONE_LINE( SUBSTRS, N, 2, 'read_aerosol_menu:b3' )
   READ( SUBSTRS(1), * ) RMRI(1, IMODE)
   READ( SUBSTRS(2), * ) RMRI(2, IMODE)

   ! Size range
   CALL SPLIT_ONE_LINE( SUBSTRS, N, 2, 'read_aerosol_menu:b4' )
   READ( SUBSTRS(1), * ) DISTPAR(3, IMODE)
   READ( SUBSTRS(2), * ) DISTPAR(4, IMODE)
   
   ! Separator line
   CALL SPLIT_ONE_LINE( SUBSTRS, N, -1, 'read_aerosol_menu:b5' )
   
   ! Rg Sigma_g
   CALL SPLIT_ONE_LINE( SUBSTRS, N, 2, 'read_aerosol_menu:b6' )
   READ( SUBSTRS(1), * ) DISTPAR(1, IMODE)
   READ( SUBSTRS(2), * ) DISTPAR(2, IMODE)
   
   ! OC # 3
   IMODE = 3  
   ! Separator line
   CALL SPLIT_ONE_LINE( SUBSTRS, N, -1, 'read_aerosol_menu:o1' )   
   
   ! density
   CALL SPLIT_ONE_LINE( SUBSTRS, N, 1, 'read_aerosol_menu:o2' )
   READ( SUBSTRS(1), * ) OCRHO
      
   ! RMRI
   CALL SPLIT_ONE_LINE( SUBSTRS, N, 2, 'read_aerosol_menu:o3' )
   READ( SUBSTRS(1), * ) RMRI(1, IMODE)
   READ( SUBSTRS(2), * ) RMRI(2, IMODE)

   ! Size range
   CALL SPLIT_ONE_LINE( SUBSTRS, N, 2, 'read_aerosol_menu:o4' )
   READ( SUBSTRS(1), * ) DISTPAR(3, IMODE)
   READ( SUBSTRS(2), * ) DISTPAR(4, IMODE)
   
   ! Separator line
   CALL SPLIT_ONE_LINE( SUBSTRS, N, -1, 'read_aerosol_menu:o5' )
   
   ! Rg Sigma_g
   CALL SPLIT_ONE_LINE( SUBSTRS, N, 2, 'read_aerosol_menu:o6' )
   READ( SUBSTRS(1), * ) DISTPAR(1, IMODE)
   READ( SUBSTRS(2), * ) DISTPAR(2, IMODE)

   ! Dust # 4
   IMODE = 4  
   ! Separator line
   CALL SPLIT_ONE_LINE( SUBSTRS, N, -1, 'read_aerosol_menu:d1' )  
    
   ! density
   CALL SPLIT_ONE_LINE( SUBSTRS, N, 1, 'read_aerosol_menu:d2' )
   READ( SUBSTRS(1), * ) DUSTRHO
   
   ! RMRI
   CALL SPLIT_ONE_LINE( SUBSTRS, N, 2, 'read_aerosol_menu:d3' )
   READ( SUBSTRS(1), * ) RMRI(1, IMODE)
   READ( SUBSTRS(2), * ) RMRI(2, IMODE)

   ! Size range
   CALL SPLIT_ONE_LINE( SUBSTRS, N, 2, 'read_aerosol_menu:d4' )
   READ( SUBSTRS(1), * ) DISTPAR(3, IMODE)
   READ( SUBSTRS(2), * ) DISTPAR(4, IMODE)
   
   ! Separator line
   CALL SPLIT_ONE_LINE( SUBSTRS, N, -1, 'read_aerosol_menu:d5' )  

   ! Rg Sigma_g
   CALL SPLIT_ONE_LINE( SUBSTRS, N, 2, 'read_aerosol_menu:d6' )
   READ( SUBSTRS(1), * ) DISTPAR(1, IMODE)
   READ( SUBSTRS(2), * ) DISTPAR(2, IMODE)

   ! Separator line
   CALL SPLIT_ONE_LINE( SUBSTRS, N, 1, 'read_aerosol_menu:d7' )
   
   WRITE( 6, 150 ) '' 
   WRITE( 6, 150 ) 'AEROSOL MENU:' 
   WRITE( 6, 150 ) REPEAT( '-', 48 )
   WRITE( 6, 130 ) 'External Mix?           :', LEXTERNAL_MIX
   WRITE( 6, 130 ) 'Internal Mix?           :', LINTERNAL_MIX
   WRITE( 6, 130 ) 'Sulfate Shell?          :', LSULFATE_SHELL
   WRITE( 6, 130 ) 'OC Shell?               :', LOC_SHELL
   WRITE( 6, 100 ) 'BC/Sulfate Mass Ratio   :', BC2SULF_MASS(1), &
                                                BC2SULF_MASS(NBC2SULF_MASS) 
   WRITE( 6, 100 ) 'BC/OC Mass Ratio        :', BC2OC_MASS(1), &
                                                BC2OC_MASS(NBC2OC_MASS) 
   WRITE( 6, 100 ) 'Dust/All Mass Ratio     :', DUST2ALL_MASS(1),&
                                                DUST2ALL_MASS(NDUST2ALL_MASS)
   WRITE( 6, 150 ) 'Sulf #1 Properties      :'
   WRITE( 6, 110 ) '  - sulfate ID          :', SULFID_LST(1), SULFID_LST(NSULF)
   WRITE( 6, 100 ) '  - species mass ratio  :', SULF_MASSRATIO_LST(1), & 
                                                SULF_MASSRATIO_LST(NSULFMASSRATIO)   
   WRITE( 6, 100 ) '  - size range [um]     :', DISTPAR(3, 1), DISTPAR(4, 1)
   WRITE( 6, 150 ) '  - size distribution   : PAR(1)    PAR(2)'
   WRITE( 6, 100 ) '             ==> Entries:', DISTPAR(1, 1), DISTPAR(2, 1)
   WRITE( 6, 150 ) 'BC #2 Properties        :'
   WRITE( 6, 100 ) '  - density             :', BCRHO
   WRITE( 6, 100 ) '  - refractive index    :', RMRI(1, 2), RMRI(2, 2)
   WRITE( 6, 100 ) '  - size range [um]     :', DISTPAR(3, 2), DISTPAR(4, 2)
   WRITE( 6, 150 ) '  - size distribution   : PAR(1)    PAR(2)'
   WRITE( 6, 100 ) '             ==> Entries:', DISTPAR(1, 2), DISTPAR(2, 2) 
   WRITE( 6, 150 ) 'OC #3 Properties        :'
   WRITE( 6, 100 ) '  - density             :', OCRHO
   WRITE( 6, 100 ) '  - refractive index    :', RMRI(1, 3), RMRI(2, 3)
   WRITE( 6, 100 ) '  - size range [um]     :', DISTPAR(3, 3), DISTPAR(4, 3)
   WRITE( 6, 150 ) '  - size distribution   : PAR(1)    PAR(2)'
   WRITE( 6, 100 ) '             ==> Entries:', DISTPAR(1, 3), DISTPAR(2, 3)    
   WRITE( 6, 150 ) 'Dust #4 Properties      :'
   WRITE( 6, 100 ) '  - density             :', DUSTRHO
   WRITE( 6, 100 ) '  - refractive index    :', RMRI(1, 4), RMRI(2, 4)
   WRITE( 6, 100 ) '  - size range [um]     :', DISTPAR(3, 4), DISTPAR(4, 4)
   WRITE( 6, 150 ) '  - size distribution   : PAR(1)    PAR(2)'
   WRITE( 6, 100 ) '             ==> Entries:', DISTPAR(1, 4), DISTPAR(2, 4)   
   100  FORMAT(5X, A, 2F8.3 )     
   110  FORMAT(5X, A, 2I5   )
   120  FORMAT(5X, A, A     )
   130  FORMAT(5X, A, L5    )
   140  FORMAT(5X, A, F9.3  )
   150  FORMAT(5x, A )
        
   END SUBROUTINE READ_AEROSOL_MENU
   
!------------------------------------------------------------------------------

   SUBROUTINE READ_DIAGNOSTIC_MENU

   ! References to F90 modules
   USE NAMELIST_ARRAY_MOD, ONLY : DIAG_PREFIX, LDIAG

   ! Local variables
   INTEGER            :: N
   CHARACTER(LEN=255) :: SUBSTRS(MAXDIM)

   !=================================================================
   ! READ_DIAGNOSTIC_MENU begins here!
   !=================================================================

   ! LDIAG
   CALL SPLIT_ONE_LINE( SUBSTRS, N, 1, 'read_diagnostic_menu:1' )
   READ( SUBSTRS(1:N), * ) LDIAG

   ! DIAG_FILENAME
   CALL SPLIT_ONE_LINE( SUBSTRS, N, 1, 'read_diagnostic_menu:2' )
   READ( SUBSTRS(1:N), '(A)' ) DIAG_PREFIX
   
   CALL SPLIT_ONE_LINE( SUBSTRS, N, 1, 'read_diagnostic_menu:3' )
   
   ! Screen print
   WRITE( 6, 150) ''
   WRITE( 6, 150) 'DIAGNOSTIC MENU'
   WRITE( 6, 150) REPEAT( '-', 48 )
   WRITE(6,110) 'Turn on DIAGNOSTIC?     : ', LDIAG
   WRITE(6,100) 'Output NC file prefix   : ', TRIM(DIAG_PREFIX)
	
   100 FORMAT(5X, A, A  ) 
   110 FORMAT(5X, A, L5 )
   150 FORMAT(5x, A )
   END SUBROUTINE READ_DIAGNOSTIC_MENU
      
!------------------------------------------------------------------------------
   SUBROUTINE CHECK_DIRECTORY( DIR )

   ! References to F90 modules 
   USE ERROR_MOD,     ONLY : ERROR_STOP

   ! !INPUT PARAMETERS: 
   !
   CHARACTER(LEN=*), INTENT(INOUT) :: DIR

   INTEGER                         :: C
   CHARACTER(LEN=255)              :: MSG
   LOGICAL                         :: IT_EXISTS

   !=================================================================
   ! CHECK_DIRECTORY begins here!
   !=================================================================

   ! Locate the last non-white-space character of NEWDIR
   C = LEN_TRIM( DIR )

   ! Add the trailing directory separator if it is not present
   IF ( DIR(C:C) /= '/' ) THEN
      DIR(C+1:C+1) = '/'
   ENDIF

   !=================================================================
   ! Test if the directory actually exists
   !=================================================================

   ! Test whether directory exists w/ F90 INQUIRE function
   INQUIRE( FILE=TRIM( DIR ), EXIST=IT_EXISTS )

   ! IFORT v9 compiler requires use of the DIRECTORY keyword to 
   ! INQUIRE for checking existence of directories.
   IF ( .not. IT_EXISTS ) THEN
      INQUIRE( DIRECTORY=TRIM( DIR ), EXIST=IT_EXISTS )
   ENDIF
   ! If the directory does not exist then stop w/ an error message
   IF ( .not. IT_EXISTS ) THEN
      MSG = 'Invalid directory: ' // TRIM( DIR )
      CALL ERROR_STOP( MSG, 'CHECK_DIRECTORY ("input_mod.f")' )
   ENDIF

   END SUBROUTINE CHECK_DIRECTORY




!------------------------------------------------------------------------------

   ! End of module
   END MODULE NAMELIST_MOD
   
   
   
   
   
   
   
   