!  $Id: CLEANUP.f, v1.1 
      SUBROUTINE CLEANUP
!
!******************************************************************************
!  Subroutine CLEANUP deallocates the memory assigned to dynamic allocatable 
!  arrays 
!******************************************************************************
!
      ! References to F90 modules
      !USE ATMPROF_MOD,        ONLY : CLEANUP_ATMOS
C     USE AIROPTIC_MOD,       ONLY : CLEANUP_AIROPTIC
C     USE SPECTRA_MOD,        ONLY : CLEANUP_SPECTRA
C     USE GAS_MOD,            ONLY : CLEANUP_GAS
C     USE RAYLEIGH_MOD,       ONLY : CLEANUP_RAYLEIGH
C     USE AEROSOL_MOD,        ONLY : CLEANUP_AEROSOL
C     USE IOP_MOD,            ONLY : CLEANUP_IOP
C     USE VLIDORT_ARRAY_MOD,  ONLY : CLEANUP_VLIDORT_ARRAY
C     USE LAMB_ALBEDO_MOD,    ONLY : CLEANUP_LAMB_ALBEDO
C     USE BRDF_MOD,           ONLY : CLEANUP_VBRDF
C     USE SLEAVE_MOD,         ONLY : CLEANUP_SLEAVE

      IMPLICIT NONE

      !================================================================
      ! CLEANUP begins here!
      !================================================================ 

      !CALL CLEANUP_ATMOS
C     CALL CLEANUP_AIROPTIC
C     CALL CLEANUP_SPECTRA
C     CALL CLEANUP_LAMB_ALBEDO
C     CALL CLEANUP_GAS
C     CALL CLEANUP_RAYLEIGH
C     CALL CLEANUP_AEROSOL
C     CALL CLEANUP_IOP
C     CALL CLEANUP_VBRDF
C     CALL CLEANUP_VLIDORT_ARRAY
C     CALL CLEANUP_SLEAVE

      !================================================================
      ! Print to screen
      !================================================================
      WRITE( 6, '(/a )' ) REPEAT( '=', 79 )
      WRITE( 6, 100     )
100   FORMAT( '  - CLEANUP: deallocating all retrieval arrays now...' )

      ! Return to the calling routine 
      END SUBROUTINE CLEANUP

!------------------------------------------------------------------------------
      
      SUBROUTINE CLEANUP_EACH_SPECTRUM

      ! References to F90 modules
C     USE AEROSOL_MOD,        ONLY : CLEANUP_AEROSOL
C     USE AIROPTIC_MOD,       ONLY : CLEANUP_AIROPTIC 
C     USE IOP_MOD,            ONLY : CLEANUP_IOP
C     USE VLIDORT_ARRAY_MOD,  ONLY : CLEANUP_VLIDORT_ARRAY
C     USE BRDF_MOD,           ONLY : CLEANUP_VBRDF
C     USE SLEAVE_MOD,         ONLY : CLEANUP_SLEAVE
C
      IMPLICIT NONE
C     
C     CALL CLEANUP_AIROPTIC
C     CALL CLEANUP_AEROSOL
C     CALL CLEANUP_IOP
C     CALL CLEANUP_VBRDF
C     CALL CLEANUP_VLIDORT_ARRAY
C     CALL CLEANUP_SLEAVE

      END SUBROUTINE CLEANUP_EACH_SPECTRUM
