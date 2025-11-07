! #id: ERROR_MOD.f v 1.1
      MODULE ERROR_MOD
!
!****************************************************************************
!  Module ERROR_MOD contains error checking routines.
!  
!  Module Routines:
!  =========================================================================
!  (1 ) ALLOC_ERR   : allocate error message print
!  (2 ) ERROR_STOP  : print error message and stop
! 
!  NOTES:
!  (1 ) This code is modified from 'error_mod.f' by xxu.
!
!****************************************************************************
      PRIVATE

      PUBLIC  :: ALLOC_ERR
      PUBLIC  :: ERROR_STOP

!  This module routines starts here ...

      CONTAINS

!  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

      SUBROUTINE ALLOC_ERR( ARRAYNAME, AS )

!  Subroutine ALLOC_ERR prints an error message if there is not enough
!  memory to allocate a particular allocatable array.

!  arguments

      CHARACTER(LEN=*),  INTENT(IN) :: ARRAYNAME
      INTEGER, OPTIONAL, INTENT(IN) :: AS

!  local variables

      CHARACTER(LEN=255) :: IFORT_ERRMSG, MSG
      CHARACTER(LEN=255) :: ERRMSG

! Define error message

      ERRMSG = 'Allocation error in array: ' // TRIM( ARRAYNAME )

      IF ( PRESENT( AS ) ) THEN

         ERRMSG = TRIM( ERRMSG ) // ' :: ' // TRIM( MSG )

      ENDIF

      ! Print error message, deallocate memory, and stop the run
      CALL ERROR_STOP( ERRMSG, 'alloc_err.f' )

      END SUBROUTINE ALLOC_ERR

!  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

      SUBROUTINE ERROR_STOP( MESSAGE, LOCATION )
!
!******************************************************************************
!  Subroutine ERROR_STOP is a wrapper for GEOS_CHEM_STOP.  It prints an error
!  message then calls GEOS_CHEM_STOP to free memory and quit. (bmy, 10/15/02)
!
!  Arguments as Input:
!  ============================================================================
!  (1 ) MESSAGE  (CHARACTER) : Error message to be printed
!  (2 ) LOCATION (CHARACTER) : Location where ERROR_STOP is called from
!
!  NOTES:
!******************************************************************************
!

      ! Arguments
      CHARACTER(LEN=*), INTENT(IN) :: MESSAGE, LOCATION

      !=================================================================
      ! ERROR_MSG begins here!
      !=================================================================

      ! Write msg
      WRITE( 6, '(a)' ) REPEAT( '=', 79 )
      WRITE( 6, '(a)' ) 'PROGRAMING ERROR: ' // TRIM( MESSAGE )
      WRITE( 6, '(a)' ) 'STOP at '           // TRIM( LOCATION )
      WRITE( 6, '(a)' ) REPEAT( '=', 79 )

      ! Deallocate memory and stop the run
      WRITE( 6, '(/a)' ) 'Deallocate memory and stop the run'
      CALL CLEANUP
      CALL EXIT( 99999 ) 

      ! Return to calling program
      END SUBROUTINE ERROR_STOP

!  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

      END MODULE ERROR_MOD
