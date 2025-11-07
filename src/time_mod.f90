! $id: time_mod.f, v1.1 2012/07/23 09:02:01 xxu
! $id: time_mod.f90, v1.2 2015/11/16 22:30:01 xxu
MODULE TIME_MOD
!
!******************************************************************************
! Module TIME\_MOD contains ART_RTM date and time variables
!  and timesteps, and routines for accessing them.
!
!  Module Variables:
!  ============================================================================
!
!  Module Routines
!  ============================================================================
!  ( 1) SYSTEM_TIMESTAMP()   :
!  ( 2) SYSTEM_DATE_TIME     :
!  ( 3) TIMESTAMP_STRING()   :
!  ( 4) YMD_EXTRACT          : 
!
!******************************************************************************
!
  IMPLICIT NONE

!=================================================================
! MODULE PRIVATE DECLARATIONS -- keep certain internal variables 
! and routines from being seen outside "gas_mod.f"
!=================================================================

! Make everything PRIVATE ...
  PRIVATE

! ... except these routines
  PUBLIC :: SYSTEM_TIMESTAMP
  PUBLIC :: DAY_NUMBER
  PUBLIC :: ITS_A_LEAPYEAR 
  PUBLIC :: YMD_EXTRACT

! ... and these variables

! Module variables
  !INTEGER               :: YEAR, MONTH, DAY
  !INTEGER               :: HOUR, MINUTE

!=================================================================
! MODULE ROUTINES -- follow below the "CONTAINS" statement 
!=================================================================
CONTAINS

!------------------------------------------------------------------------------ 

FUNCTION SYSTEM_TIMESTAMP() RESULT( STAMP )
!
! !RETURN VALUE:
!
  CHARACTER(LEN=16) :: STAMP
! 
! !LOCAL VARIABLES:
!
  INTEGER           :: SYS_NYMD, SYS_NHMS

  !=================================================================
  ! SYSTEM_TIMESTAMP begins here!
  !=================================================================

  ! Get system date and time
  CALL SYSTEM_DATE_TIME( SYS_NYMD, SYS_NHMS )

  ! Create a string w/ system date & time
  STAMP = TIMESTAMP_STRING( SYS_NYMD, SYS_NHMS )

END FUNCTION SYSTEM_TIMESTAMP

!------------------------------------------------------------------------------

SUBROUTINE SYSTEM_DATE_TIME( SYS_NYMD, SYS_NHMS )
!
!******************************************************************************
! Subroutine SYSTEM\_DATE\_TIME returns the actual local date 
!  and time (as opposed to the model date and time).
! This routine is modified frim time_mod.f of GEOS-Chem by R. Yantosca.
!  (xxu, 7/23/12)
!******************************************************************************
!
! !OUTPUT PARAMETERS:
!
  INTEGER, INTENT(OUT) :: SYS_NYMD   ! System date in YYYY/MM/DD format
  INTEGER, INTENT(OUT) :: SYS_NHMS   ! System time in YYYY/MM/DD format
!
! !REMARKS:
!  Uses the F90 intrinsic function DATE_AND_TIME.
!
! !LOCAL VARIABLES:
!
! Arguments

! Local variables
  INTEGER              :: V(8)
  CHARACTER(LEN=8)     :: D
  CHARACTER(LEN=10)    :: T

  !=================================================================
  ! SYSTEM_DATE_TIME begins here!
  !=================================================================

  ! Initialize
  D = 'ccyymmdd'
  T = 'hhmmss.sss'

  ! Call the F90 intrinsic routine DATE_AND_TIME
  ! Return values are (/YYYY, MM, DD, GMT_MIN, HH, MM, SS, MSEC/)
  CALL DATE_AND_TIME( DATE=D, TIME=T, VALUES=V )

  ! Save to YYYYMMDD and HHMMSS format
  SYS_NYMD = ( V(1) * 10000 ) + ( V(2) * 100 ) + V(3)
  SYS_NHMS = ( V(5) * 10000 ) + ( V(6) * 100 ) + V(7)

END SUBROUTINE SYSTEM_DATE_TIME

!------------------------------------------------------------------------------

FUNCTION TIMESTAMP_STRING( YYYYMMDD, HHMMSS ) RESULT( TIME_STR )
!
!******************************************************************************
! Function TIMESTAMP\_STRING returns a formatted string 
!  "YYYY/MM/DD hh:mm" for the a date and time specified by YYYYMMDD and
!  hhmmss. If YYYYMMDD and hhmmss are omitted, then TIMESTAMP\_STRING will
!  create a formatted string for the current date and time.
! This routine is modified frim time_mod.f of GEOS-Chem by R. Yantosca.
!  (xxu, 7/23/12)
!******************************************************************************
!
! !INPUT PARAMETERS: 
!
  INTEGER, INTENT(IN) :: YYYYMMDD   ! YYYY/MM/DD date
  INTEGER, INTENT(IN) :: HHMMSS     ! hh:mm:ss time
!
! !RETURN VALUE:
!
  CHARACTER(LEN=16)             :: TIME_STR
!
! !LOCAL VARIABLES:
!
  INTEGER :: THISYEAR, THISMONTH,  THISDAY
  INTEGER :: THISHOUR, THISMINUTE, THISSECOND

  ! Extract date info from YYYYMMDD
  CALL YMD_EXTRACT( YYYYMMDD, THISYEAR, THISMONTH, THISDAY )

  ! Extra time info from HHMMSS
  CALL YMD_EXTRACT( HHMMSS, THISHOUR, THISMINUTE, THISSECOND )

  ! For other platforms, we can just use a FORTRAN internal write
  WRITE( TIME_STR, 100 ) THISYEAR, THISMONTH, THISDAY, THISHOUR, THISMINUTE

 ! Format statement
 100  FORMAT( i4.4, '/', i2.2, '/', i2.2, ' ', i2.2, ':', i2.2 )

END FUNCTION TIMESTAMP_STRING

!------------------------------------------------------------------------------

SUBROUTINE YMD_EXTRACT( NYMD, Y, M, D )
!
!******************************************************************************
! Subroutine YMD\_EXTRACT extracts the year, month, and date 
!  from an integer variable in YYYYMMDD format.  It can also extract the 
!  hours, minutes, and seconds from a variable in HHMMSS format.
! This routine is modified frim time_mod.f of GEOS-Chem by R. Yantosca.
!  (xxu, 7/23/12)
!*****************************************************************************
!
! !INPUT PARAMETERS: 
!
  INTEGER, INTENT(IN)  :: NYMD      ! YYYY/MM/DD format date
!
! !OUTPUT PARAMETERS:
!
  INTEGER, INTENT(OUT) :: Y, M, D   ! Separated YYYY, MM, DD values
! 
! !REVISION HISTORY: 
!  21 Nov 2001 - R. Yantosca - Initial Version
!  15 Jan 2010 - R. Yantosca - Added ProTeX headers
! !LOCAL VARIABLES:
!
  REAL*8 :: REM

  ! Extract YYYY from YYYYMMDD 
  Y = INT( DBLE( NYMD ) / 1d4 )

  ! Extract MM from YYYYMMDD
  REM = DBLE( NYMD ) - ( DBLE( Y ) * 1d4 )
  M   = INT( REM / 1d2 )

  ! Extract DD from YYYYMMDD
  REM = REM - ( DBLE( M ) * 1d2 )
  D   = INT( REM )

! Return to calling program
END SUBROUTINE YMD_EXTRACT

!------------------------------------------------------------------------------

FUNCTION ITS_A_LEAPYEAR ( Year ) RESULT( is_leapyear )

! Function ITS_A_LEAPYEAR test if the given year a leap year

! Arguments
  INTEGER, INTENT(IN) :: Year

! Functions value
  LOGICAL             :: is_leapyear

  !=================================================================
  ! A leap year is:
  ! (1) evenly divisible by 4 (if not a century year)
  ! (2) evenly divisible by 4, 100, and 400 (if a century year)
  !=================================================================
  is_leapyear = .False.

  IF ( MOD( Year, 4 ) == 0 ) THEN
     IF ( MOD( Year, 100 ) == 0 ) THEN
        IF ( MOD( Year, 400 ) == 0 ) THEN
           is_leapyear = .True.
        ENDIF
     ELSE
        is_leapyear = .True.
     ENDIF
  ENDIF

  RETURN

END FUNCTION ITS_A_LEAPYEAR

!------------------------------------------------------------------------------

FUNCTION DAY_NUMBER( Day, Month, Year ) RESULT( j )
!
!******************************************************************************
! Subroutine DAY_NUMBER calculates the Day of the year given the Day/Month/Year
! (xxu, 10/13/2015)
!******************************************************************************
!
! Arguments
  INTEGER, INTENT(IN)   :: Day, Month, Year

! Function value
  INTEGER               :: j

  !======================================================================
  ! DAY_NUMBER begins here
  !======================================================================

  IF ( Month <=2 ) THEN
     j = 31 * ( Month - 1 ) + Day
     RETURN
  ENDIF

  IF ( Month > 8 ) THEN
     j = 31 * ( Month - 1 ) - ( (month-2)/2 ) - 2 + Day
  ELSE
     j = 31 * ( Month - 1 ) - ( (month-1)/2 ) - 2 + Day
  ENDIF

  ! Checking leap year
  IF ( ITS_A_LEAPYEAR ( Year ) ) j = j + 1

END FUNCTION DAY_NUMBER

!------------------------------------------------------------------------------


! End of module
END MODULE TIME_MOD
