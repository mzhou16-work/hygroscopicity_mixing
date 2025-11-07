! $Id: sulfate_mod.f90, v1.0
   MODULE SULFATE_MOD
!******************************************************************************
!  Module SULFATE calculate the growth factor and optical properties of SULFATE
!  aerosols
!
!  Module Routines
!  ============================================================================
!  (1 ) READ_HYGROPARAS        : Main subroutine to read the file namelist.ini
!  (2 ) GET_GROWTHFACTOR	   : Subroutine to calculate the growth factor
!  (3 ) GET_NRNI			   : Subroutine to calculate the refractive indice
!                                for sulfates
!
!******************************************************************************    
   
   
   IMPLICIT NONE
   
   !=================================================================
   ! MODULE PRIVATE DECLARATIONS -- keep certain internal variables 
   ! and routines from being seen outside "sulfate_mod.f"
   !=================================================================

   ! Make everything PRIVATE ...
   PRIVATE
   
   ! ... except these routines
   PUBLIC :: READ_HYGROPARAS
   PUBLIC :: GET_HYGROSCOPICITY
   PUBLIC :: GET_NRNI
   
   PUBLIC :: AVGGROWTH
   PUBLIC :: AVGMGROWTH
   PUBLIC :: AVGWETSULFRHO
   PUBLIC :: COMPOSITENR
   PUBLIC :: DRYRHO
   PUBLIC :: AVGDRYSULFRHO
   PUBLIC :: TOTALDRYSULFMASS
   PUBLIC :: TOTALWETSULFMASS
   PUBLIC :: DRYV_SULF
   PUBLIC :: WETV_SULF 
   PUBLIC :: WATERNR
   PUBLIC :: SNAME
   PUBLIC :: NR

   ! ... and these variables


   !=================================================================
   ! MODULE VARIABLES 
   !=================================================================
   
   LOGICAL              :: VERBOSE  = .FALSE.
   INTEGER, PARAMETER   :: FIRSTCOL = 26
   INTEGER, PARAMETER   :: IU_NAME  = 1
   INTEGER, PARAMETER   :: N        = 6   ! # of species in tang's data set
   INTEGER, PARAMETER   :: NC       = 5   ! # of coefficient for water activity and density
   INTEGER, PARAMETER   :: MaxN     = 100 !  maximum number for density
   INTEGER, PARAMETER   :: WRHO     = 1   !  water density
   
!    USE NAMELIST_ARRAY_MOD,   ONLY : NSULF
   
   ! module variable
   REAL                 :: TOTALMOL       ! sulfate total molar
   REAL                 :: DRYV_SULF      ! sulfate dry volume
   REAL                 :: AVGDRYSULFRHO 
   REAL                 :: TOTALDRYSULFMASS
   
   CHARACTER(len=50), ALLOCATABLE, DIMENSION(:) :: SNAME
   REAL, ALLOCATABLE    :: WETV_SULF(:)       ! sulfate wet volume
!    REAL, ALLOCATABLE    :: DRYV_SULF(:)       ! sulfate dry volume
   REAL, ALLOCATABLE    :: WACLIMIT(:)        ! water activity limit         
   REAL, ALLOCATABLE    :: DRYRHO(:)          ! density
   REAL, ALLOCATABLE    :: WACTCOEFF(:,:)     ! water activity coefficent
   REAL, ALLOCATABLE    :: DENSITYCOEFF(:,:)  ! density coefficient
   REAL, ALLOCATABLE    :: MWEIGHT(:)         ! molecular weight
   REAL, ALLOCATABLE    :: NR(:,:)            ! refractive index real part
   REAL, ALLOCATABLE    :: NI(:,:)            ! refractive index imaginary part 
   REAL, ALLOCATABLE    :: WATERNR(:)
   REAL, ALLOCATABLE    :: WaterNI(:)   
   REAL, ALLOCATABLE    :: CRH(:)             ! CRH point
   REAL, ALLOCATABLE    :: WACTIVITY(:,:)     ! water activity 
   REAL, ALLOCATABLE    :: DENSITY(:,:)       ! read density
   REAL, ALLOCATABLE    :: GF(:,:)            ! growth factor 
   REAL, ALLOCATABLE    :: INTPOLGF(:,:)      ! interpolated GF
   REAL, ALLOCATABLE    :: INTPOLMGF(:,:)     ! interpolated mass GF
   REAL, ALLOCATABLE    :: MOLARREF(:)        ! molar refraction
   REAL, ALLOCATABLE    :: COMPOSITENR(:,:)   ! refraction index of the sulfate Composite
   REAL, ALLOCATABLE    :: AVGGROWTH(:)       ! growth factor of the sulfate Composite
   REAL, ALLOCATABLE    :: AVGMGROWTH(:)      ! mass growth factor of the sulfate Composite
   REAL, ALLOCATABLE    :: TOTALWETSULFMASS(:)
   REAL, ALLOCATABLE    :: AVGWETSULFRHO(:)
   REAL, ALLOCATABLE    :: NRNISULFATE (:,:)  ! refractive index daaset            
   REAL, ALLOCATABLE    :: NRNISACID(:,:)     ! refractive index for acid 


   !=================================================================
   ! MODULE ROUTINES -- follow below the "CONTAINS" statement 
   !=================================================================
   CONTAINS
   
!------------------------------------------------------------------------------   
   SUBROUTINE READ_HYGROPARAS
   
   ! References to F90 modules
   USE ERROR_MOD,   ONLY : ERROR_STOP
   USE NAMELIST_ARRAY_MOD,   ONLY : DIR_DATA
   USE NAMELIST_ARRAY_MOD,   ONLY : NSULF
   USE NAMELIST_ARRAY_MOD,   ONLY : SULFID_LST
   USE NAMELIST_ARRAY_MOD,   ONLY : SULF_MASSRATIO_LST
   
   ! Local variables
   CHARACTER(LEN=255)   :: FILENAME = 'tang-w-d-sulfate-nitrate-only.dat'
  
   INTEGER              :: IOS
   INTEGER			    :: I, J 
   CHARACTER(LEN=300)   :: NOUSE
   CHARACTER(LEN=255)   :: MESSAGE
!    CHARACTER(LEN=10)    :: SNAME(99)
   CHARACTER(len=50), DIMENSION(N) :: RAW_SNAME
!    CHARACTER(len=50)    :: RAW_SNAME_TEMP
   REAL                 :: RAW_WACLIMIT(N)        ! water activity limit         
   REAL                 :: RAW_DRYRHO(N)          ! density
   REAL                 :: RAW_WACTCOEFF(N,NC)     ! water activity coefficent
   REAL                 :: RAW_DENSITYCOEFF(N,NC)  ! density coefficient
   REAL                 :: RAW_MWEIGHT(N)         ! molecular weight
   REAL                 :: RAW_NR(N)              ! refractive index
   REAL                 :: RAW_CRH(N)             ! CRH point

   
   ! allocate variable  
   ALLOCATE ( SNAME(NSULF) )
   ALLOCATE ( WACLIMIT(NSULF) )
   ALLOCATE ( DRYRHO(NSULF) ) 
   ALLOCATE ( WACTCOEFF(NSULF,NC) )
   ALLOCATE ( DENSITYCOEFF(NSULF,NC) )
   ALLOCATE ( MWEIGHT(NSULF) )
!    ALLOCATE ( NR(NSULF) )
   ALLOCATE ( CRH(NSULF) )
   
  ! start to read the database from Tang 
   OPEN( IU_NAME, FILE = TRIM( DIR_DATA ) // TRIM( FILENAME ),  &
         STATUS='OLD', IOSTAT=IOS )

   IF ( IOS /= 0 ) THEN
      MESSAGE = 'Fail to open file: '//TRIM( FILENAME )
      CALL ERROR_STOP( MESSAGE, TRIM( FILENAME) )
   ENDIF   
    
   READ(IU_NAME, *) NOUSE
   READ(IU_NAME, *) NOUSE
   READ(IU_NAME, *) NOUSE
   READ(IU_NAME, *) ( RAW_SNAME(I), I = 1, N )
   READ(IU_NAME, *) ( RAW_WACLIMIT(J), J = 1, N ) 
   READ(IU_NAME, *) ( RAW_DRYRHO(J), J = 1, N )
   READ(IU_NAME, *) ( ( RAW_WACTCOEFF(J, I), J = 1, N ), I = 1, NC ) 
   READ(IU_NAME, *) ( ( RAW_DENSITYCOEFF(J, I), J = 1, N ), I = 1, NC ) 
   READ(IU_NAME, *) ( RAW_MWEIGHT(J), j = 1, N) 
   READ(IU_NAME, *) ( RAW_NR(J), J = 1, N) 
   CLOSE( IU_NAME )
   
   ! print out database
!    WRITE (6, '(a)') '  - Reading Sulfate & Nitrate hygroscopic parameters'
!    WRITE (6, 100) 'species :',  RAW_SNAME
!    WRITE (6, 110) 'waclimit :', RAW_WACLIMIT
!    WRITE (6, 120) 'dry rho :',  RAW_DRYRHO
!    DO i = 1, NC
!       WRITE (6, 120), 'wactcoeff :', RAW_WACTCOEFF(:, i)
!    END DO
!    DO i = 1, NC
!       WRITE (6, 120), 'denscoeff :', RAW_DENSITYCOEFF(:, i)
!    END DO
!    WRITE (6, 110), 'Mol Weight :', RAW_MWEIGHT
!    WRITE (6, 110), 'Ref Index :', RAW_NR 
   
   ! extract parameters based on the input species...
   ! we dont use the refractive index here...
   DO I = 1, NSULF
      DO J = 1, N
         IF ( SULFID_LST(I) .EQ. J ) THEN
            SNAME(I) = TRIM(RAW_SNAME(J))         
            WACLIMIT(I) = RAW_WACLIMIT(J)
            DRYRHO(I) = RAW_DRYRHO(J)
            WACTCOEFF(I,:) = RAW_WACTCOEFF(J, :) 
            DENSITYCOEFF(I,:) = RAW_DENSITYCOEFF(J,:)
            MWEIGHT(I) = RAW_MWEIGHT(J)
!             NR(I) = RAW_NR(J)      
         ENDIF         
      ENDDO
   ENDDO
   
   
!    WRITE (6, 130) '  - Input species: ', SNAME
   WRITE (6, '(a)') '  - READ_HYGROPARAS: Extracting hygroscopic parameters...'
   WRITE (6, 100) 'species :',  SNAME
   WRITE (6, 110) 'waclimit :', WACLIMIT
   WRITE (6, 120) 'dry rho :',  DRYRHO
   DO i = 1, NC
      WRITE (6, 120), 'wactcoeff :', WACTCOEFF(:, i)
   END DO
   DO i = 1, NC
      WRITE (6, 120), 'denscoeff :', DENSITYCOEFF(:, i)
   END DO
   WRITE (6, 110), 'Mol Weight :', MWEIGHT
!    WRITE (6, 110), 'Ref Index :', NR
   WRITE(6,'(A)') REPEAT( '=', 79 )  
   100 FORMAT (4X, A12, 6(A15))
   110 FORMAT (4X, A12, 6(F11.3))
   120 FORMAT (4X, A12, 6(E11.3))
   130 FORMAT (A, 6(A15))
      
   END SUBROUTINE READ_HYGROPARAS
   
!------------------------------------------------------------------------------   
   SUBROUTINE GET_HYGROSCOPICITY
   
   ! References to F90 modules
   USE ERROR_MOD,            ONLY : ERROR_STOP
   USE NAMELIST_ARRAY_MOD,   ONLY : NSULF
   USE NAMELIST_ARRAY_MOD,   ONLY : SULF_MASSRATIO_LST
   USE NAMELIST_ARRAY_MOD,   ONLY : RH_LST
   USE NAMELIST_ARRAY_MOD,   ONLY : NRH_LST
   USE SPECTRA_MOD,          ONLY : NSPECTRA, LAMDAS
   
   ! Local variables
   INTEGER			           :: I, J, K
   REAL						   :: CURRENTNR(NSULF)
   REAL						   :: CURRENTLAMDA
   ! allocate variable
   ALLOCATE ( WACTIVITY(NSULF, MaxN) )
   ALLOCATE ( DENSITY(NSULF, MaxN) )
   ALLOCATE ( GF(NSULF, MaxN) )
   ALLOCATE ( INTPOLGF(NSULF, NRH_LST) )
   ALLOCATE ( INTPOLMGF(NSULF, NRH_LST) )
   ALLOCATE ( MOLARREF(NSULF) )
   ALLOCATE ( COMPOSITENR(NSPECTRA, NRH_LST) )
   ALLOCATE ( AVGGROWTH(NRH_LST) )
   ALLOCATE ( AVGMGROWTH(NRH_LST) ) 
   ALLOCATE ( TOTALWETSULFMASS(NRH_LST) )
   ALLOCATE ( AVGWETSULFRHO(NRH_LST) )
   ALLOCATE ( WETV_SULF(NRH_LST) )
!    ALLOCATE ( DRYV_SULF(NRH_LST) )
   
   
   WRITE (6, '(a)') ' - GET_HYGROSCOPICITY: calculate growth factor of the sulfate species in different RH'                   
   WRITE ( 6, 160 ) '   Input RH: ', RH_LST(1), RH_LST(NRH_LST)
   
   ! make the RH 100 case valid
   IF ( RH_LST(NRH_LST) .EQ. 100) THEN
      RH_LST(NRH_LST) = 99.5
   ENDIF
   
   ! convert RH to decimal...                   
   RH_LST(1:NRH_LST) = RH_LST(1:NRH_LST)/100.0
   
   
   WRITE (6, '(a)') ' - GET_HYGROSCOPICITY: Calculate water activity and density of each species'
   ! compute the growth factor at different RH
   LP_SPECIES: DO I = 1, NSULF     ! loop of sulfate species

      ! calculate water activity and density of different species
      LP_POLY: DO J = 1, INT(WACLIMIT(I)) ! loop of polynomial abscissae
      
         WACTIVITY(I,J) = 0.0
         DENSITY(I,J) = 0.0
         
         LP_COEFF: DO K = 1, NC ! loop of coefficient
         
            ! Eqn(6) - Tang, 1996
            WACTIVITY(I,J) = WACTIVITY(I,J) + WACTCOEFF(I, K)*(J*1.0)**(K-1)
            ! Eqn(5) - Tang, 1996
            DENSITY(I,J) = DENSITY(I,J) + DENSITYCOEFF(I, K)*(J*1.0)**(K-1)
            
         END DO LP_COEFF
         
         GF(I,J) = (100./J/DENSITY(I,J) * DRYRHO(I))**(1./3.)
           
      END DO LP_POLY
      
	  WRITE (6, 150) , REPEAT( ' ', 20 ),  TRIM(SNAME(I))
      WRITE (6, 140), 'wactivity: ', WACTIVITY(I, 1:3), '...', & 
                  WACTIVITY(I, WACLIMIT(I) - 1:WACLIMIT(I))
                  
      WRITE (6, 140), 'density: ', DENSITY(I, 1:3), '...', & 
                  DENSITY(I, WACLIMIT(I) - 1:WACLIMIT(I))
                  
      WRITE (6, 140), 'gf: ', GF(I, 1:3), '...', & 
                  GF(I, WACLIMIT(I) - 1:WACLIMIT(I))
      
      ! start to interpolate to specified RH
      DO 80 K = 1, NRH_LST
         INTPOLGF(I, K) = 1 
         do 70 J = 1, int(WACLIMIT(I))-1
            IF ( ( WACTIVITY(I,J) .GE. RH_LST(K)) .AND. &
                 ( WACTIVITY(I,J+1) .LT. RH_LST(K)) ) then
               
               INTPOLGF(I, K) = ( RH_LST(K) -  WACTIVITY(I,J+1) ) /   &
                                ( WACTIVITY(I,J) - WACTIVITY(I,J+1) ) * &
                                ( GF(I,J)- GF(I,J+1) ) + GF(I,J+1) 
            ENDIF
            ! it seems this parameter is useless
            INTPOLMGF(I,K) = (DRYRHO(I) + (INTPOLGF(I, k)**3-1))/DRYRHO(I) 
         70 continue
      80 continue
      WRITE (6, 140), 'IntpolGF: ', INTPOLGF(I, 1:3), '...', & 
                  INTPOLGF(I, NRH_LST - 1:NRH_LST)
      WRITE (6, 140), 'IntpolMGF: ', INTPOLMGF(I, 1:3), '...', & 
                  INTPOLMGF(I, NRH_LST - 1:NRH_LST)      
   END DO LP_SPECIES
   
   ! Here we also calculate the density of the dry sulfate composites...
   TOTALDRYSULFMASS = 0.0
   DRYV_SULF = 0.0 
   DO J = 1, NSULF
      TOTALDRYSULFMASS = TOTALDRYSULFMASS + SULF_MASSRATIO_LST(J)
      DRYV_SULF = SULF_MASSRATIO_LST(J)/DRYRHO(J) + DRYV_SULF  
   ENDDO
   AVGDRYSULFRHO = TOTALDRYSULFMASS/DRYV_SULF
   
   WRITE (6, '(/, a)') '  - GET_HYGROSCOPICITY: Calculate molar refraction of each species'
   WRITE (6, 100) '  - Total sulfate compostes volume : ', DRYV_SULF
   WRITE (6, 100) '  - Total sulfate compostes mass : ', TOTALDRYSULFMASS
   WRITE (6, 100) '  - Average dry sulfate composites density : ', AVGDRYSULFRHO
   
   
   
   ! Calculate Average Growth Factor for different mass ratio at different RH  
   WRITE (6, '(/, a)') '  - GET_HYGROSCOPICITY: Calculate molar refraction of each species'
   WRITE (6, 130) '  - Input species: ', SNAME   
   LOOP_SPECTRA: DO K = 1, NSPECTRA
   
      DO J = 1, NSULF
         CURRENTNR(J) = NR(K, J)
      ENDDO
      
!       print *, '******check_1******'
!       print *, CURRENTNR
      CURRENTLAMDA    = LAMDAS(K)
      WRITE (6, '(a)') ''
      WRITE (6, 100) '  - Current Wavelength:',  CURRENTLAMDA
      WRITE (6, 100) '  - Current Nr:',  CURRENTNR
      WRITE (6, 100) '  - Current Water Nr:',  WATERNR(K)
      ! Eqn(7)
      ! first Calculate the molar fraction
      DO J = 1, NSULF
         print *, '******check_-1******'
         print *, MWEIGHT(J), DRYRHO(J), CURRENTNR(J)
         MOLARREF(J) =  MWEIGHT(J)/DRYRHO(J)*(CURRENTNR(J)**2-1)/( CURRENTNR(J)**2+2 )
      ENDDO 
    
      
      WRITE (6, 100) '  - molar refraction :', MOLARREF

      ! now calcualte the composite NR at the different RH
      DO I = 1, NRH_LST
         WETV_SULF(I) = 0.0  
!          DRYV_SULF(I) = 0.0
         COMPOSITENR(K,I) = 0
         TOTALWETSULFMASS(I) = 0.0
         TOTALMOL = 0.0 
      
         ! integrate volume over different sulfate species...
         DO  J = 1, NSULF
            ! volume ratio and Molar Ref.      
!             DRYV_SULF(I) = SULF_MASSRATIO_LST(J)/DRYRHO(J) + DRYV_SULF(I)
            WETV_SULF(I) = SULF_MASSRATIO_LST(J)/DRYRHO(J) * INTPOLGF(J, I)**3 + WETV_SULF(I)
         
		    ! dry mass + water mass
            TOTALWETSULFMASS(I) = TOTALWETSULFMASS(I) + SULF_MASSRATIO_LST(J) + & 
                                  SULF_MASSRATIO_LST(J)/DRYRHO(J) * & 
                                  (INTPOLGF(J, I)**3-1) * WRHO  
            print *, '******check_0******'
            print *, COMPOSITENR(K,I), SULF_MASSRATIO_LST(J), MWEIGHT(J), MOLARREF(J)                  
            COMPOSITENR(K,I) =  COMPOSITENR(K,I) + SULF_MASSRATIO_LST(J)/ &
                                MWEIGHT(J) * MOLARREF(J)

            TOTALMOL = SULF_MASSRATIO_LST(J)/ MWEIGHT(J) + TOTALMOL     
         ENDDO
         print *, '******check_1******'
         print *, COMPOSITENR(K,I)
	     ! calculate the average growth factor of the sulfate composites
         AVGGROWTH(I)     = (WETV_SULF(I)/DRYV_SULF)**(1./3.)
         AVGMGROWTH(I)    = TOTALWETSULFMASS(I)/TOTALWETSULFMASS(1)

         AVGWETSULFRHO(I) = TOTALWETSULFMASS(I)/WETV_SULF(I)
         TOTALMOL = (WETV_SULF(I)-DRYV_SULF) * WRHO/18. + TOTALMOL           ! molar  of water
      	
      	 print *, '******check_1-2******'
      	 print *, COMPOSITENR(K,I)
         COMPOSITENR(K,I) = COMPOSITENR(K,I) + (WETV_SULF(I)-DRYV_SULF)*1.0/18.*18./1.0* & 
                           (WATERNR(K)**2-1)/(WATERNR(K)**2+2)        !molar refraction of water 
         print *, '******check_2******'
         print *, COMPOSITENR(K,I), WETV_SULF(I), DRYV_SULF, WATERNR(K)
         print *, (WETV_SULF(I)-DRYV_SULF)*1.0/18.*18./1.0
         print *, (WATERNR(K)**2-1)/(WATERNR(K)**2+2)
         
         COMPOSITENR(K,I) = COMPOSITENR(K,I)/(TOTALMOL*WETV_SULF(I)/TOTALMOL)
         COMPOSITENR(K,I) = SQRT( ( 1 + 2* COMPOSITENR(K,I) )/(1 -  COMPOSITENR(K,I)) )
         print *, '******check_3******'
         print *, COMPOSITENR(K,I)

      ENDDO
   
      IF ( NRH_LST .GE. 1 ) THEN
         WRITE (6, 170) '    RH :', RH_LST(1:2), ' ...', RH_LST(NRH_LST-1:NRH_LST)
         WRITE (6, 170) '    Total volume:', WETV_SULF(1:2), ' ...', WETV_SULF(NRH_LST-1:NRH_LST)
         WRITE (6, 170) '    Total mass:', TOTALWETSULFMASS(1:2), ' ...', TOTALWETSULFMASS(NRH_LST-1:NRH_LST)
         WRITE (6, 170) '    Average density:', AVGWETSULFRHO(1:2), ' ...', AVGWETSULFRHO(NRH_LST-1:NRH_LST)
         WRITE (6, 170) '    Average GF:', AVGGROWTH(1:2), ' ...', AVGGROWTH(NRH_LST-1:NRH_LST)
         WRITE (6, 170) '    Average MGF:', AVGMGROWTH(1:2), ' ...', AVGMGROWTH(NRH_LST-1:NRH_LST)
         WRITE (6, 170) '    Composite NR:', COMPOSITENR(K, 1:2), ' ...' ,COMPOSITENR(K, NRH_LST-1:NRH_LST)
      ENDIF
   END DO LOOP_SPECTRA
   WRITE (6, 170), ' ' 
   
   100 FORMAT (A, 6(F11.3))
   110 FORMAT (4X, A18, 6(F11.3))
   120 FORMAT (4X, A12, 6(E11.3))
   130 FORMAT (A, 6(A15))
   140 FORMAT (4X, A12, 3(F7.4), A, 2(F10.4))
   150 FORMAT (A, A)
   160 FORMAT (4X, A, 2F8.2 )   
   170 FORMAT (A, 2F7.3, A, 2F7.3 )
    
   END SUBROUTINE GET_HYGROSCOPICITY
!------------------------------------------------------------------------------
   
   SUBROUTINE GET_NRNI
   
 ! References to F90 modules
   USE ERROR_MOD,                   ONLY : ERROR_STOP
   USE NAMELIST_ARRAY_MOD,          ONLY : DIR_DATA
   USE NAMELIST_ARRAY_MOD,          ONLY : SULFID_LST
   USE NAMELIST_ARRAY_MOD,          ONLY : NSULF
!    USE NAMELIST_ARRAY_MOD,          ONLY : SPECTRA_LST
!    USE NAMELIST_ARRAY_MOD,          ONLY : NSPECTRA_LST
   USE SPECTRA_MOD,                 ONLY : NSPECTRA, LAMDAS

   ! Local variables
   CHARACTER(LEN=255)   :: FILENAME   = 'AS-AHS-LET-WATER-NR-NI_ALLSPEC.txt' 
   CHARACTER(LEN=255)   :: FILENAME_2 = 'SULF-NR-NI-ALLSPEC.txt'
   INTEGER              :: IOS
   INTEGER              :: I, J, K, IDX
   REAL                 :: CURRENTLAMDA
   REAL                 :: COEFF
   REAL                 :: RAW_NR(N)       ! refractive index
   REAL                 :: RAW_NI(N)       ! refractive index      
   INTEGER, PARAMETER   :: NP      = 13	   ! 
   INTEGER, PARAMETER   :: NL      = 115   !
   INTEGER, PARAMETER   :: ANP     = 13	   !
   INTEGER, PARAMETER   :: ANL     = 115   !
   INTEGER, PARAMETER   :: WLIDX   = 1     ! wavenumber index in nr&ni dataset
   INTEGER, PARAMETER   :: SAIDX   = 12    ! NR index of 95% sulfur acid    
   INTEGER, PARAMETER   :: ASIDX   = 4     ! Ammonia Sulfate NR IDX
   INTEGER, PARAMETER   :: AHSIDX  = 6	   ! NH4HSO4
   INTEGER, PARAMETER   :: LETIDX  = 8     ! (NH4)3H(SO4)2
   INTEGER, PARAMETER   :: NO3IDX  = 10	   ! NH4NO3, 
   INTEGER, PARAMETER   :: NACLIDX = 12    ! Nacl, seasalt
   INTEGER, PARAMETER   :: WTIDX   = 2 
   

   CHARACTER(LEN=300)   :: NOUSE
   CHARACTER(LEN=255)   :: MESSAGE
   ! allocate variable
   ALLOCATE ( NRNISULFATE(NP, NL) )
   ALLOCATE ( NRNISACID(ANP, ANL) )
   ALLOCATE ( NR(NSPECTRA, NSULF) )
   ALLOCATE ( NI(NSPECTRA, NSULF) )
   ALLOCATE ( WATERNR(NSPECTRA) )
   ALLOCATE ( WATERNI(NSPECTRA) )
   
   ! start to read refractiv index of all species
   WRITE (6, '(a)') ' - GET_NRNI: Reading refractive index of all species...'
   OPEN( IU_NAME, FILE = TRIM( DIR_DATA ) // TRIM( FILENAME ),  &
         STATUS='OLD', IOSTAT=IOS )

   IF ( IOS /= 0 ) THEN
      MESSAGE = 'Fail to open file: '//TRIM( FILENAME )
      CALL ERROR_STOP( MESSAGE, TRIM( FILENAME) )
   ENDIF

   READ(IU_NAME, *) NOUSE
   READ(IU_NAME, *) ((NRNISULFATE(i, j), i = 1, NP), j = 1, NL)  
   CLOSE(IU_NAME)
 
   WRITE (6, 170) NRNISULFATE(1:2,1), ' ...', NRNISULFATE(NP - 1:NP,1)
   WRITE (6, 180) ' ...'
   WRITE (6, 170) NRNISULFATE(1:2,NL-1), ' ...', NRNISULFATE(NP - 1:NP,NL-1)
   WRITE (6, 170) NRNISULFATE(1:2,NL), ' ...', NRNISULFATE(NP - 1:NP,NL)
   
   
   ! start to read the refractive index of acid in different RH
   WRITE (6, '(a)') ' - GET_NRNI: Reading refractive index of acid in different RH...'
   OPEN( IU_NAME, FILE = TRIM( DIR_DATA ) // TRIM( FILENAME ),  &
         STATUS='OLD', IOSTAT=IOS )

   IF ( IOS /= 0 ) THEN
      MESSAGE = 'Fail to open file: '//TRIM( FILENAME_2 )
      CALL ERROR_STOP( MESSAGE, TRIM( FILENAME_2) )
   ENDIF
   
   READ(IU_NAME, *) NOUSE
   READ(IU_NAME, *) (( NRNISACID(i,j), i = 1, ANP), j = 1, ANL)
   CLOSE(IU_NAME)

   WRITE (6, 170) NRNISACID(1:2,1), ' ...', NRNISACID(NP - 1:NP,1)
   WRITE (6, 180) ' ...'
   WRITE (6, 170) NRNISACID(1:2,NL-1), ' ...', NRNISACID(NP - 1:NP,NL-1)
   WRITE (6, 170) NRNISACID(1:2,NL), ' ...', NRNISACID(NP - 1:NP,NL)

   170 FORMAT (4X, 2F7.3, A, 2F7.3 )
   180 FORMAT (18X, A)
   
   WRITE (6, '(/, a)'), ' - GET_NRNI: Prepare Nr & Ni for each wavelength'
   
   ! find the species NR and water NR for different wavelength
   LP_SPECTRA: DO K = 1, NSPECTRA
      
      ! get the current lamda and convert to um
      CURRENTLAMDA = LAMDAS(K)/1000.0
      
      IDX = 2
      WLSEARCH : DO While (NRNISULFATE(WLIDX, IDX) < CURRENTLAMDA .and. IDX <= NL ) 
         IDX = IDX + 1 
      END DO WLSEARCH   

      IF (IDX > NL ) THEN 
        WRITE (6, '(a)'), ' - GET_NRNI: The specified wavelength is out of the data range'
        WRITE (6, '(a)'), ' - GET_NRNI: Lamda = ', CURRENTLAMDA
        ! note the infrared spectrum only goes up to 25um   
        IDX = NL
      ENDIF

      COEFF = (CURRENTLAMDA -  NRNISULFATE(1, IDX-1))/(NRNISULFATE(1, IDX)  - &
                                            NRNISULFATE(1, IDX-1))
  
      RAW_NR(1) = COEFF*(NRNISULFATE( ASIDX, IDX)-NRNISULFATE( ASIDX, IDX-1)) + &
                  NRNISULFATE( ASIDX, IDX-1)
      RAW_NI(1) = COEFF*(NRNISULFATE( ASIDX+1, IDX)-NRNISULFATE( ASIDX+1, IDX-1)) + &
                  NRNISULFATE( ASIDX+1, IDX-1)
   
      RAW_NR(2) = COEFF*(NRNISULFATE( AHSIDX, IDX)- NRNISULFATE( AHSIDX, IDX-1)) + &
                  NRNISULFATE( AHSIDX, IDX-1)
      RAW_NI(2) = Coeff*(NRNISULFATE( AHSIDX+1, IDX)-NRNISULFATE( AHSIDX+1, IDX-1)) + &
                  NRNISULFATE( AHSIDX+1, IDX-1)

      RAW_NR(3) = COEFF*(NRNISULFATE( LETIDX, IDX)-NRNISULFATE( LETIDX, IDX-1)) + &
                  NRNISULFATE( LETIDX, IDX-1)
      RAW_NI(3) = Coeff*(NRNISULFATE( LETIDX+1, IDX)-NRNISULFATE( LETIDX+1, IDX-1)) + &
                  NRNISULFATE( LETIDX+1, IDX-1)

      RAW_NR(4) = COEFF*(NRNISULFATE( NO3IDX, IDX)-NRNISULFATE( NO3IDX, IDX-1)) + &
                  NRNISULFATE( NO3IDX, IDX-1)           
      RAW_NI(4) = COEFF*(NRNISULFATE( NO3IDX+1, IDX)-NRNISULFATE( NO3IDX+1, IDX-1)) + &
                  NRNISULFATE( NO3IDX+1, IDX-1)

      RAW_NR(5) = COEFF*(NRNISULFATE( NACLIDX, IDX)-NRNISULFATE( NACLIDX, IDX-1)) + &
                  NRNISULFATE( NaCLIDX, IDX-1)
      RAW_NI(5) = COEFF*(NRNISULFATE( NACLIDX+1, IDX)-NRNISULFATE( NACLIDX+1, IDX-1)) + &
                  NRNISULFATE( NACLIDX+1, IDX-1)

      WATERNR(K) = COEFF*(NRNISULFATE( WTIDX, IDX)-NRNISULFATE( WTIDX, IDX-1)) + &
                   NRNISULFATE( WTIDX, IDX-1)
      WATERNI(K) = COEFF*(NRNISULFATE( WTIDX+1, IDX)-NRNISULFATE( WTIDX+1, IDX-1)) + &
                   NRNISULFATE( WTIDX+1, IDX-1)
   
      ! do the samething for Sulfur achid
      IDX = 1
      WLNRSEARCH : DO While (NRNISACID(WLIDX, IDX) < CURRENTLAMDA .and. IDX <= ANL ) 
         IDX = IDX + 1 
      END DO WLNRSEARCH
            
      IF (IDX > ANL ) THEN 
         WRITE (6, '(a)'), ' - GET_NRNI: the specified wavelength is out of the data in Sulfur acid'
         WRITE (6, '(a)'), ' - GET_NRNI: Lamda = ', CURRENTLAMDA
         ! note the infrared spectrum only goes up to 25um   
         IDX = ANL
      ENDIF

      ! interpolation to get NR and NI for five different salts
      COEFF = (CURRENTLAMDA -  NRNISACID(1, IDX-1))/(NRNISACID(1, IDX) &   
               - NRNISACID(1, IDX-1)) 
      RAW_NR(6) = COEFF*(NRNISACID( SAIDX, IDX)-NRNISACID( SAIDX, IDX-1)) &
                  +  NRNISACID( SAIDX, IDX-1)               
      RAW_NI(6) = COEFF*(NRNISACID( SAIDX+1, IDX)-NRNISACID( SAIDX+1, IDX-1))  &
                  +  NRNISACID( SAIDX+1, IDX-1)   

      ! extract parameters based on the input species...
      DO I = 1, NSULF
         DO J = 1, N
            IF ( SULFID_LST(I) .EQ. J ) THEN
               NR(K, I) = RAW_NR(J)  
               NI(K, I) = RAW_NI(J)      
            ENDIF         
         ENDDO
      ENDDO
      
      WRITE (6, 120), ''
      WRITE (6, 120), '  Wavelength :', CURRENTLAMDA, ' um'
      WRITE (6, 100), 'Species :',  SNAME
      WRITE (6, 110), 'Nr :', NR(K, :)
      WRITE (6, 110), 'Ni :', NI(K, :)
      WRITE (6, 130), '  Water Ni :', WATERNR(K)
      WRITE (6, 130), '  Water Ni :', WATERNI(K)  
   END DO LP_SPECTRA
   

   WRITE(6,'(A)') REPEAT( '=', 79 )  
   100 FORMAT (A12, 6(A15))
   110 FORMAT (A10, 6(F11.3))
   120 FORMAT (A12, F11.3, A)
   130 FORMAT (A, F11.3)
   
   END SUBROUTINE GET_NRNI

   END MODULE SULFATE_MOD
   
   
   
   
   
   
   
   
   