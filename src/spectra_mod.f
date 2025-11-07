! $Id: spectra_mod.f90, v1.0 2012/07/17 12:42:41 CST xxu
      MODULE SPECTRA_MOD
!
!******************************************************************************
!  Module SPECTRA_MOD contains routines to address the spectra
!  parameters, i.e. wavelength, wavenumber, etc.
!
!
!  Module Variables:
!  ============================================================================
!  (1 ) NLAMDA      (INTEGER): # of spectra 
!  (2 ) LAMDAS      (REAL*8 ): Wavelengths [nm]
!  (3 ) WAVENUMS    (REAL*8 ): Wave numbers [cm^-1]
!
!  Module Routines:
!  ============================================================================ 
!  (1 ) GET_SPECTRA          : 
!  (2 ) INIT_SPECTRA         :
!  (3 ) CLEANUP_SPECTRA      : 
!  
!******************************************************************************
!
      IMPLICIT NONE

      !=================================================================
      ! MODULE PRIVATE DECLARATIONS -- keep certain internal variables 
      ! and routines from being seen outside "spectra_mod.f"
      !=================================================================

      ! Make everything PRIVATE ...
      PRIVATE

      ! ... except these routines
      PUBLIC                :: GET_SPECTRA
      PUBLIC                :: CLEANUP_SPECTRA
      PUBLIC                :: SPLINE
      PUBLIC                :: SPLINT

      ! ... and these variables
      PUBLIC                :: LREAD_SPECTRA 
      !PUBLIC                :: NLAMDA
      PUBLIC                :: NSPECTRA
      PUBLIC                :: LAMDAS
      PUBLIC                :: WAVENUMS
      PUBLIC                :: NREAL0
      PUBLIC                :: NIMAG0
      PUBLIC                :: SURFACE_SPECTRA

      ! Module variables
      LOGICAL               :: LREAD_SPECTRA 
      !INTEGER               :: NLAMDA
      INTEGER               :: NSPECTRA
      REAL*8, ALLOCATABLE   :: LAMDAS(:)
      REAL*8, ALLOCATABLE   :: WAVENUMS(:) 
      REAL*8, ALLOCATABLE   :: NREAL0(:,:)
      REAL*8, ALLOCATABLE   :: NIMAG0(:,:)
      REAL*8, ALLOCATABLE   :: SURFACE_SPECTRA(:,:)

      !=================================================================
      ! MODULE ROUTINES -- follow below the "CONTAINS" statement 
      !=================================================================
      CONTAINS

!-------------------------------------------------------------------------------

      SUBROUTINE GET_SPECTRA
!
!******************************************************************************
!  Subroutine GET_SPECTRA calculates the spectral wavelength and
!  wavenumbers based on the inputed starting and end wavelength as well
!  as the spectral resoultion.
!  (xxu, 7/17/12)
!
!  NOTES:
!  (1 ) Now the spectral step can be wavenumber if SPECTRA_STEP is
!  negative. (xxu, 7/17/12)
!******************************************************************************
!
      ! Refereces to F90 modules
      USE NAMELIST_ARRAY_MOD, ONLY : LFREQUENCY
      USE NAMELIST_ARRAY_MOD, ONLY : SPECTRA_LST,  NSPECTRA_LST
      USE NAMELIST_ARRAY_MOD, ONLY : SPECTRA_STEP
      USE NAMELIST_ARRAY_MOD, ONLY : LPRT
      USE ERROR_MOD,          ONLY : ERROR_STOP

      ! Local parameters (~0.01 nm or 0.01 cm^-1)
      REAL*8, PARAMETER     :: EPS = 1.0D-10
      REAL*8, PARAMETER     :: MIN_SPECTRA_STEP = 1.0D-4
      REAL*8, PARAMETER     :: MAXLAM = 4.0D4
      REAL*8, PARAMETER     :: MINLAM = 2.0D2
      
      ! Local variables
      INTEGER               :: I
      INTEGER               :: IDCASE
      CHARACTER(LEN=255)    :: ERRMSG
      REAL*8                :: SPECTRAb
      REAL*8                :: SPECTRAe
      REAL*8                :: SPECTRA_RANGE

      !================================================================
      ! GET_SPECTRA begins here!
      !================================================================
      

      WRITE(6, 100)
 100  FORMAT( /, 1X, '- GET_SPECTRA: calculate spectral parameters' )

      ! The beging and end of spectra
      SPECTRAb = SPECTRA_LST(1)
      SPECTRAe = SPECTRA_LST(NSPECTRA_LST)

      ! Determine if needs to read spectrum data
      IF ( NSPECTRA_LST == 1 .AND. SPECTRAb <= EPS ) THEN
         LREAD_SPECTRA = .TRUE.
      ELSE
         LREAD_SPECTRA = .FALSE.
      ENDIF

      ! Get the index for each case
      IF ( LREAD_SPECTRA ) THEN 
         IDCASE = -1 
      ELSE
         IF ( ABS(SPECTRA_STEP) <= EPS .OR. 
     &        NSPECTRA_LST == 1 .OR.  NSPECTRA_LST > 2    ) IDCASE = 1
         IF ( SPECTRA_STEP >  EPS .AND. NSPECTRA_LST == 2 ) IDCASE = 2
         IF ( SPECTRA_STEP < -EPS .AND. NSPECTRA_LST == 2 ) IDCASE = 3 
      ENDIF

      ! Case statement for spectrum scenarios
      SELECT CASE ( IDCASE )  

      !================================================================
      ! Reads spectrum
      !================================================================
      CASE( -1 )
         
         CALL READ_SPECTRA  

         ! Determine the wavelength and frequency
         IF ( LFREQUENCY ) THEN

            WAVENUMS = LAMDAS
            LAMDAS   = 1.D7 / WAVENUMS

         ELSE

            WAVENUMS = 1.0D7 / LAMDAS

         ENDIF

      !================================================================
      ! Monospectral or Direct inputs of multi-spectra
      !================================================================
      CASE ( 1 ) 

         ! Single wavelength
         NSPECTRA = NSPECTRA_LST

         ! Allocate arrays
         CALL INIT_SPECTRA

         ! Asign the wavenumber and wavelength
         IF ( LFREQUENCY ) THEN 

            ! If frequency is used
            WAVENUMS(1:NSPECTRA) = SPECTRA_LST(1:NSPECTRA)
            LAMDAS(1:NSPECTRA) = 1.0D7 / WAVENUMS(1:NSPECTRA)

         ELSE 

            ! If wavelength is used
            LAMDAS(1:NSPECTRA) = SPECTRA_LST(1:NSPECTRA)
            WAVENUMS(1:NSPECTRA) = 1.0D7 / LAMDAS(1:NSPECTRA)

         ENDIF

      !================================================================
      ! Equal wavelength interval for wavelength as input,
      ! or equal frequency interval for frequency as input
      !================================================================
      CASE ( 2 )

         ! Check the minimum spectral interval
         IF ( SPECTRA_STEP < MIN_SPECTRA_STEP ) THEN
            ERRMSG = "SPECTRA_STEP exceeds minimum 1E-4"
            CALL ERROR_STOP( ERRMSG, 'spectra_mod.f' )
         ENDIF

         ! Number of spectra
         NSPECTRA = FLOOR( ( SPECTRAe - SPECTRAb ) / SPECTRA_STEP )
         NSPECTRA = NSPECTRA + 1         

         ! Allocate arrays 
         CALL INIT_SPECTRA

         ! Calculate wavenumber and wavelength
         IF ( LFREQUENCY ) THEN

            ! If frequency as input
            DO I = 1, NSPECTRA
               WAVENUMS(I) = SPECTRAb + SPECTRA_STEP * REAL(I-1,8)
               LAMDAS(I)   = 1.0D7 / WAVENUMS(I)
            ENDDO

         ELSE

            ! If wavelength as input
            DO I = 1, NSPECTRA
               LAMDAS(I)   = SPECTRAb + SPECTRA_STEP * REAL(I-1,8)
               WAVENUMS(I) = 1.0D7 / LAMDAS(I)
            ENDDO


         ENDIF

      !================================================================
      ! Equal wavelength interval for frequency as input,
      ! or equal frequency interval for wavelength as input
      !================================================================
      CASE( 3 ) 

         ! Check the minimum spectral interval
         IF ( ABS(SPECTRA_STEP) < MIN_SPECTRA_STEP ) THEN
            ERRMSG = "SPECTRA_STEP exceeds minimum 1E-4"
            CALL ERROR_STOP( ERRMSG, 'spectra_mod.f' )
         ENDIF

         ! Number of spectra
         NSPECTRA = FLOOR( ( 1d7/SPECTRAb - 1d7/SPECTRAe ) 
     &                   / ABS(SPECTRA_STEP) )
         NSPECTRA = NSPECTRA + 1

         ! Allocate arrays
         CALL INIT_SPECTRA

         ! Calculate wavenumber and wavelength
         IF ( LFREQUENCY ) THEN
 
            ! If frequency as input
            DO I = 1, NSPECTRA
               LAMDAS(I)   = 1.0D7 / SPECTRAb + SPECTRA_STEP*REAL(I-1,8)
               WAVENUMS(I) = 1.0D7 / LAMDAS(I)
            ENDDO

         ELSE

            ! If Wavelength as input
            DO I = 1, NSPECTRA
               WAVENUMS(I) = 1.0D7 / SPECTRAb + SPECTRA_STEP*REAL(I-1,8)
               LAMDAS(I)   = 1.0D7 / WAVENUMS(I)
            ENDDO

         ENDIF

      ! End of Case statement
      END SELECT

      ! Screen printing
      IF ( LPRT ) THEN
        
         WRITE(6, 110 )

         IF ( NSPECTRA > 20 ) THEN

            DO I = 1, 10
               WRITE(6, 120 ) I, LAMDAS(I), WAVENUMS(I)
            ENDDO
            WRITE( 6, 130) '  ...   ...   ...', '  ...   ...   ...'
            DO I = NSPECTRA-10, NSPECTRA
               WRITE(6, 120 ) I, LAMDAS(I), WAVENUMS(I)
            ENDDO

         ELSE

            DO I = 1, NSPECTRA
               WRITE(6, 120 ) I, LAMDAS(I), WAVENUMS(I)
            ENDDO

         ENDIF
 
      ENDIF

      !================================================================
      ! Check on spectral range
      !================================================================

      ! Give a warning if the wavelength is out of range
      IF ( MINVAL(LAMDAS) < MINLAM .OR. MAXVAL(LAMDAS) > MAXLAM  ) THEN
         PRINT*, MINVAL(LAMDAS), MAXVAL(LAMDAS)
         ERRMSG = "Wavelenth out of range [200, 20000nm]"
         CALL ERROR_STOP( ERRMSG, 'namelist_mod.f' )
      ENDIF

      ! Ending lamda must be larger or equal to the start one
      !IF ( LAMDAb > LAMDAe ) THEN
      !   PRINT*, LAMDAb, LAMDAe
      !   ERRMSG = "Starting lamda is larger than ending one"
      !   CALL ERROR_STOP( ERRMSG, 'namelist_mod.f' )
      !ENDIF       

      WRITE(6,'(1X,A)')'- GET_SPECTRA: done spectral settings'
      WRITE(6,'(A)') REPEAT( '=', 79 )

      ! Formats
 110  FORMAT( 2X, " List of spectra ", /, 
     &        2X, " index#, lamda[nm],  wavenumber[cm^-1]")
 120  FORMAT( I6, 2F12.4 )
 130  FORMAT( 2X, A, /, 2X, A )

      ! Return the calling routine
      END SUBROUTINE GET_SPECTRA

!-------------------------------------------------------------------------------

      SUBROUTINE READ_SPECTRA

      ! References to F90 modules
      USE ERROR_MOD,          ONLY : ERROR_STOP
      USE NAMELIST_ARRAY_MOD, ONLY : LPRT

      ! Local variables
      INTEGER, PARAMETER :: IU_SPECTRA = 36
      INTEGER            :: IU, IOS 
      CHARACTER*255      :: FILENAME = 'spectra.dat'
      CHARACTER*255      :: MESSAGE
      INTEGER            :: I, J
      REAL*4             :: ONELINE(8) 

      !=================================================================
      ! READ_SPECTRA begins here!
      !=================================================================
      WRITE(6,100) 
 100  FORMAT(2X,'-READ_SPECTRA: read data from spectra.dat')

      ! Open file
      IU = IU_SPECTRA 
      OPEN(UNIT=IU, FILE=TRIM(FILENAME), STATUS='OLD', IOSTAT=IOS )
      IF ( IOS /= 0 ) THEN
         MESSAGE = 'Fail to open file: '//TRIM( FILENAME )
         CALL ERROR_STOP( MESSAGE, 'spectra_mod.f' )
      ENDIF

      ! Read comments lines, 10 lines in total
      DO J = 1, 10
         READ(UNIT=IU,FMT=*)
      ENDDO

      ! Read number of spectrum 
      READ(UNIT=IU,FMT=*) NSPECTRA

      ! allocate module arrays
      CALL INIT_SPECTRA
      IF ( LPRT ) WRITE(6,110) NSPECTRA

      ! Now read the spectra and refractive indices
      DO I = 1, NSPECTRA

         READ(UNIT=IU,FMT=*) ONELINE(1:8)

         ! Copy read values to variables
         LAMDAS(I)   = ONELINE(1)
         NREAL0(I,1) = ONELINE(2)
         NIMAG0(I,1) = ONELINE(3)
         NREAL0(I,2) = ONELINE(4)
         NIMAG0(I,2) = ONELINE(5)
         SURFACE_SPECTRA(I,1:3) = ONELINE(6:8)

      ENDDO

      ! Close file
      CLOSE( UNIT=IU )

      ! Screen print
      IF ( LPRT ) THEN 

         WRITE(6,120) "Data read from spectra.dat:", 
     &                "Spectra, mreal, mimag, mreal, mimag, surfacex3"
          
         IF ( NSPECTRA > 20 ) THEN 

            DO I = 1, 10
               WRITE(6,130) LAMDAS(I),
     &                       NREAL0(I,1), NIMAG0(I,1),
     &                       NREAL0(I,2), NIMAG0(I,2),
     &                       SURFACE_SPECTRA(I,1:3)
            ENDDO
            WRITE( 6, 120) '  ...   ...   ...', '  ...   ...   ...'
            DO I = NSPECTRA-10, NSPECTRA
               WRITE(6,130) LAMDAS(I),
     &                       NREAL0(I,1), NIMAG0(I,1),
     &                       NREAL0(I,2), NIMAG0(I,2),
     &                       SURFACE_SPECTRA(I,1:3)
            ENDDO

         ELSE

            DO I = 1, NSPECTRA
               WRITE(6,130) LAMDAS(I),
     &                       NREAL0(I,1), NIMAG0(I,1),
     &                       NREAL0(I,2), NIMAG0(I,2),
     &                       SURFACE_SPECTRA(I,1:3)
            ENDDO

         ENDIF

      ENDIF

      ! Formats
 110  FORMAT( 2X, 'NSPECTRA = ', I4 )
 120  FORMAT( 2X, A, /, 2X, A )
 130  FORMAT( F11.4, 2(F8.3, E11.3), 3F8.3 )

      END SUBROUTINE READ_SPECTRA

!-------------------------------------------------------------------------------

      SUBROUTINE INIT_SPECTRA
!
!******************************************************************************
!  Subroutine INIT_SPECTRA allocate module variables
!  (xxu, 7/17/11)
!******************************************************************************
!
      ! Refereced to F90 modules
      USE ERROR_MOD,          ONLY : ALLOC_ERR
      USE NAMELIST_ARRAY_MOD, ONLY : RMRI
      ! Local variables
      INTEGER            :: AS
      LOGICAL, SAVE      :: IS_INIT = .FALSE.

      ! Return if we have already initialized
      IF ( IS_INIT ) RETURN 

      !=================================================================
      ! INIT_SPECTRA begins here!
      !=================================================================

      ! LAMDAS
      ALLOCATE( LAMDAS(NSPECTRA), STAT=AS )
      IF (AS/=0) CALL ALLOC_ERR( 'LAMDAS' )
      LAMDAS = 0d0

      ! WAVENUMS
      ALLOCATE( WAVENUMS(NSPECTRA), STAT=AS )
      IF (AS/=0) CALL ALLOC_ERR( 'WAVENUMS' )
      WAVENUMS = 0d0

      ! NREAL0
      ALLOCATE( NREAL0(NSPECTRA,2), STAT=AS )
      IF (AS/=0) CALL ALLOC_ERR( 'NREAL0' )
      !NREAL0 = 0d0
      NREAL0(:,1) = RMRI(1,1)
      NREAL0(:,2) = RMRI(1,2)

      ! NIMAG0
      ALLOCATE( NIMAG0(NSPECTRA,2), STAT=AS )
      IF (AS/=0) CALL ALLOC_ERR( 'NIMAG0' )
      !NIMAG0 = 0d0
      NIMAG0(:,1) = RMRI(2,1)
      NIMAG0(:,2) = RMRI(2,2)

      ! SURFACE_SPECTRA
      ALLOCATE( SURFACE_SPECTRA(NSPECTRA,3), STAT=AS )
      IF (AS/=0) CALL ALLOC_ERR( 'SURFACE_SPECTRA' )
      SURFACE_SPECTRA = 0d0

      ! Reset IS_INIT so we do not allocate arrays again
      IS_INIT = .TRUE.

      WRITE(6,100)
 100  FORMAT(" - INIT_SPECTRA: initialize spectra arrays" )

      ! Return to the calling routine
      RETURN

      END SUBROUTINE INIT_SPECTRA

!-------------------------------------------------------------------------------

      SUBROUTINE CLEANUP_SPECTRA

      !=================================================================
      ! CLEANUP_SPCETRA begins here!
      !=================================================================

      ! Deallocates all module arrays
      IF ( ALLOCATED( LAMDAS          ) ) DEALLOCATE( LAMDAS          )
      IF ( ALLOCATED( WAVENUMS        ) ) DEALLOCATE( WAVENUMS        )
      IF ( ALLOCATED( NREAL0          ) ) DEALLOCATE( NREAL0          ) 
      IF ( ALLOCATED( NIMAG0          ) ) DEALLOCATE( NIMAG0          )
      IF ( ALLOCATED( SURFACE_SPECTRA ) ) DEALLOCATE( SURFACE_SPECTRA )

      END SUBROUTINE CLEANUP_SPECTRA

!------------------------------------------------------------------------------

      SUBROUTINE SPLINE( X, Y, N, YP1, YPN, Y2 )
!
!******************************************************************************
! Routine SPLINE calculates the cubic spline given arrays X(1:N) and
! Y(1:N) 
!  containing a tabulated function, i.e., Y(i) = f( X(i) ), with X(1) <
!  X(2) < ... < X(N), and given YP1 and YPN for the first derivative of the
!  interplotating function at points 1 and N, respectively. this routine
!  returns an array Y2(1:N) of length N which contains the second
!  derivatives of the interpolating function at the tabulated points
!  X(i).
!  If YP1 and/or YPN are equal to 1E+30 or larger, the routine is
!  signaled to set the corresponding boundary condition for a natural
!  spline, with zero second derivative on that boundary.
!
!  Code is obtained from Dr. Xing Liu and updated to this model. 
!  (xxu, 7/16/2012)
!
! Reference:
!   Numerical Recipes in Fortran 77: The Art of Scientific Computing,
!   Cambridge
!    University Press. page 107-110.  "The goal of cubic spline
!    interpolation 
!    is to get an interpolation formula that is smooth in the first
!    derivative,
!    and continuous in the second derivative, both within an interval
!    and at
!    its boundaries."
!   
!******************************************************************************
!
      ! Arguments
      INTEGER, INTENT(IN)   :: N
      REAL*8,  INTENT(IN)   :: X(N), Y(N), YP1, YPN
      REAL*8,  INTENT(OUT)  :: Y2(N)

      ! Local variables
      INTEGER               :: I, K
      REAL*8                :: P, QN, SIG, UN, U(N)

      !================================================================
      ! SPLINE begins here!
      !================================================================

      ! The lower boundary condition is set either to be "natural"
      IF (YP1 > .99d30) THEN

         Y2(1) = 0d0
         U(1)  = 0d0

      ! or else to have a specified first derivative.
      ELSE

         Y2(1) = -0.5d0
         U(1)  = 3d0 / ( X(2) - X(1) )
     &         * ( ( Y(2) - Y(1) ) / ( X(2) - X(1 ) ) - YP1 )

      ENDIF

      ! This is the decomposition loop of the tridiagonal algorithm. Y2
      ! and U are used for temporary storage of the decomposed factors.
      DO I = 2, N-1

         SIG   = ( X(I) - X(I-1) ) / ( X(I+1) - X(I-1) )
         P     = SIG * Y2(I-1) + 2D0
         Y2(I) = ( SIG - 1D0 ) / P
         U(I)  = 1d0 / P
     &         * ( 6.0D0 * ( ( Y(I+1) - Y(I) ) / ( X(I+1) - X(I) )
     &                     - ( Y(I) - Y(I-1) ) / ( X(I) - X(I-1) ) )
     &                   / ( X(I+1) - X(I-1) )
     &                   - SIG * U(I-1) )

      ENDDO

      ! The upper boundary condition is set either to be “natural”
      IF (YPN > .99D30) THEN

         QN = 0d0
         UN = 0d0

      ! or else to have a specified first derivative.
      ELSE

         QN = 0.5d0
         UN = 3d0 / ( X(N) - X(N-1) )
     &      * ( YPN - ( Y(N) - Y(N-1) ) / ( X(N) - X(N-1) ) )

      ENDIF

      Y2(N) = ( UN - QN * U(N-1) ) / ( QN * Y2(N-1) + 1d0 )

      ! This is the backsubstitution loop of the tridiago- nal
      ! algorithm.
      DO K = N-1, 1, -1

        Y2(K) = Y2(K) * Y2(K+1) + U(K)

      ENDDO

      ! Return to the calling routine
      END SUBROUTINE SPLINE

!------------------------------------------------------------------------------

      SUBROUTINE SPLINT( XA, YA, Y2A, N, X, Y)
!
!******************************************************************************
!
!  Code is obtained from Dr. Xing Liu and updated to this model. 
!  (xxu, 7/16/2012)
!
! Reference:
!   Numerical Recipes in Fortran 77: The Art of Scientific Computing,
!   Cambridge
!    University Press. page 107-110.  "The goal of cubic spline
!    interpolation 
!    is to get an interpolation formula that is smooth in the first
!    derivative,
!    and continuous in the second derivative, both within an interval
!    and at
!    its boundaries."
!
!******************************************************************************
!
      ! Arguments
      INTEGER, INTENT(IN)   :: N
      REAL*8,  INTENT(IN)   :: X, XA(N), YA(N), Y2A(N)
      REAL*8,  INTENT(OUT)  :: Y

      ! Local variables
      INTEGER               :: K, KHI, KLO
      REAL*8                :: A, B, H

      !================================================================
      ! SPLINT begins here!
      !================================================================

      KLO = 1
      KHI = N

 1    IF ( KHI-KLO > 1 ) THEN

         K = ( KHI + KLO ) / 2

         IF( XA(K) > X ) THEN
            KHI = K
         ELSE
            KLO = K
         ENDIF

      GOTO 1
      ENDIF

      ! KLO and KHI now bracket at the value of X
      H = XA(KHI) - XA(KLO)

      ! The xa’s must be distinct. 
      IF ( H == 0d0 ) PAUSE

      ! Cubic spline polynomial is now evaluated. 
      A = ( XA(KHI) - X ) / H
      B = ( X - XA(KLO) ) / H
      Y = A * YA(KLO) + B * YA(KHI)
     &  + ( (A**3-A) * Y2A(KLO) + (B**3-B) * Y2A(KHI) ) * (H**2) /6d0

      ! Return to the calling routine
      END SUBROUTINE SPLINT

!-------------------------------------------------------------------------------

      ! End of module 
      END MODULE SPECTRA_MOD
