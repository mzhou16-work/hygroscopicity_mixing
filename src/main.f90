! $Id: main.f90, v1.0
   PROGRAM MAIN
   
   USE NAMELIST_ARRAY_MOD, ONLY : codeVersion

   USE NAMELIST_MOD,       ONLY : READ_NAMELIST
   USE SULFATE_MOD,        ONLY : GET_NRNI
   USE SULFATE_MOD,        ONLY : READ_HYGROPARAS
   USE SULFATE_MOD,        ONLY : GET_HYGROSCOPICITY
   USE SPECTRA_MOD,        ONLY : GET_SPECTRA
   USE MIX_MOD,            ONLY : GET_OPTICAL
   USE MIX_MOD,            ONLY : MIXING
   USE MIX_MOD,            ONLY : GET_MIX_DENSITY
   USE MIX_MOD, 	       ONLY : GEN_POLYNOMIAL
   
   USE DIAG_MOD,           ONLY : CREATE_DIAG
   USE DIAG_MOD,           ONLY : DEFINE_DIAG
   USE DIAG_MOD,           ONLY : WRITE_DIAG  
   USE DIAG_MOD,           ONLY : CLOSEUP_DIAG
   
   USE NAMELIST_ARRAY_MOD, ONLY : NRH_LST, RH_LST
   USE NAMELIST_ARRAY_MOD, ONLY : NBC2SULF_MASS, NDUST2ALL_MASS
   USE SPECTRA_MOD,        ONLY : NSPECTRA, LAMDAS      
   USE MIX_MOD,            ONLY : BCID
   USE MIX_MOD,            ONLY : DUSTID
   USE MIX_MOD,            ONLY : RHID
   USE MIX_MOD,            ONLY : LAMDAID
   
   USE NAMELIST_ARRAY_MOD, ONLY : DISTPAR

   INTEGER              :: I, J, K, L
   INTEGER              :: UInc    
   WRITE(6, 99) TRIM(codeVersion)
   

   
   CALL READ_NAMELIST
   
   CALL GET_SPECTRA
   
   CALL READ_HYGROPARAS
   
   CALL GET_NRNI
   
   CALL GET_HYGROSCOPICITY
   
   CALL GEN_POLYNOMIAL
   
   CALL GET_MIX_DENSITY
   
   CALL CREATE_DIAG( UInc )
   
   CALL DEFINE_DIAG( UInc )
   
   
   ! we have several loops, spectrum, rh, dust, bc 
   LP_SPECTRA: DO I = 1, NSPECTRA
   
      LAMDAID = I
      
      LP_RH: DO J = 1, NRH_LST
      
         
         RHID = J
         
         ! in different lamda ans RH we need to calculate the optical 
         ! properties of the BC
         CALL GET_OPTICAL
         
            LP_DUST: DO K = 1, NDUST2ALL_MASS
            
               DUSTID = K
               
               LP_BC: DO L = 1, NBC2SULF_MASS
            
                  BCID   = L
                  
                  CALL MIXING
                  CALL WRITE_DIAG(UInc)
 
               END DO LP_BC
            
            
            END DO LP_DUST
         
      END DO LP_RH
      
   END DO LP_SPECTRA
   
   CALL CLOSEUP_DIAG( UInc )
   ! Screen print to indicate the complete running
   WRITE(6,'(/,A)') REPEAT( '*', 79 )      
   WRITE(6, 300   ) 
   WRITE(6,'(A)'  ) REPEAT( '*', 79 )

   99   FORMAT(" - You are running Hygroscopicity and Mixing state Code version ", A )
   100  FORMAT(A,I6,1P5E11.3)
   199  FORMAT(/,"  MAIN: Spectral#:", I6, 3X, "Wavelength (nm):", F13.4)
   300  FORMAT(11x, "E N D   O F  H & M   S I M U L A T I O N")   
   END PROGRAM MAIN