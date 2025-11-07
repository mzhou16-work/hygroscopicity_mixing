!
! Routine to select the NR and NI for AS, AHS, WATER, Sulfur Acid
! and LET at a given wavelength 
!
! input:               
!         NRNISULFATE, NP, NL:  2D nr and ni data for sulfate particles
!         NRNISACID, ANP, ANL:  2D nr and ni data for sulfur acid
!         WL                 :  specified WL
!         N                  :  index for NR and NL.
! output:
!         NR, NI:      refractive index of AS, AHS, LET, and SA at WL          
!     WATERNR,WATERNI: 


subroutine select_nrni(WL, NRNISULFATE, NRNISACID, &
                       NR, NI, WATERNR, WATERNI)

! input & output
implicit none
include 'parameters.h'
real, dimension(NP, NL)::   NRNISULFATE
real, dimension(ANP, ANL):: NRNISACID
real, dimension(N):: NR, NI
real :: WL
real :: WaterNR, WaterNI

! other indexes
integer:: wlinx=1,       & ! wavenumber index in nr&ni dataset
          sainx=12,    & ! NR index of 95% sulfur acid    
          ASinx =  4,  & ! AS NR inx
          AHSinx = 6,  &
          LETinx = 8,  &
          NO3inx = 10,  &
          NaCLinx = 12, &
          WTinx = 2 
! temporal variables
integer:: i, j, inx
real :: Coeff



print* , ' INPUT'
print* , ' WL = ', WL
print* , 'NRNISULFATE = ', ((NRNISULFATE(i,j), i = 1,NP), j = 1,Nl)
print*,  'NRNISACID = ',   ((NRNISACID(i,j), i = 1,anp), j = 1,anl) 
print* , 'NP = ', NP, 'ANP = ', ANP
print*,  'NL = ', NL, 'ANL = ', ANL
print*,  'N = ', N


! do a loop
inx = 2
WLSEARCH : DO While (NRNISULFATE(wlinx, inx) < WL .and. inx <= NL ) 
              inx = inx + 1 
           END DO WLSEARCH
            
if (inx > NL ) then 
  print*, 'the specified wavelength is out of the data range'
  print*, 'WL = ', WL 
! note the infrared spectrum only goes up to 25um   
  inx = NL
endif

! interpolation to get NR and NI for five different salts
   
  Coeff = (WL -  NRNISULFATE(1, inx-1))/(NRNISULFATE(1, inx)  - &
                                         NRNISULFATE(1, inx-1))
  
  NR(1) = Coeff*(NRNISULFATE( ASinx, inx)-NRNISULFATE( ASinx, inx-1)) + &
           NRNISULFATE( ASinx, inx-1)
  NI(1) = Coeff*(NRNISULFATE( ASinx+1, inx)-NRNISULFATE( ASinx+1, inx-1)) + &
           NRNISULFATE( ASinx+1, inx-1)
   
  NR(2) = Coeff*(NRNISULFATE( AHSinx, inx)-NRNISULFATE( AHSinx, inx-1)) + &
           NRNISULFATE( AHSinx, inx-1)
  NI(2) = Coeff*(NRNISULFATE( AHSinx+1, inx)-NRNISULFATE( AHSinx+1, inx-1)) + &
           NRNISULFATE( AHSinx+1, inx-1)

  NR(3) = Coeff*(NRNISULFATE( LETinx, inx)-NRNISULFATE( LETinx, inx-1)) + &
           NRNISULFATE( LETinx, inx-1)
  NI(3) = Coeff*(NRNISULFATE( LETinx+1, inx)-NRNISULFATE( LETinx+1, inx-1)) + &
           NRNISULFATE( LETinx+1, inx-1)

  NR(4) = Coeff*(NRNISULFATE( NO3inx, inx)-NRNISULFATE( NO3inx, inx-1)) + &
           NRNISULFATE( NO3inx, inx-1)
  NI(4) = Coeff*(NRNISULFATE( NO3inx+1, inx)-NRNISULFATE( NO3inx+1, inx-1)) + &
           NRNISULFATE( NO3inx+1, inx-1)

  NR(5) = Coeff*(NRNISULFATE( NaCLinx, inx)-NRNISULFATE( NaCLinx, inx-1)) + &
           NRNISULFATE( NaCLinx, inx-1)
  NI(5) = Coeff*(NRNISULFATE( NaCLinx+1, inx)-NRNISULFATE( NaCLinx+1, inx-1)) + &
           NRNISULFATE( NaCLinx+1, inx-1)

  WATERNR = Coeff*(NRNISULFATE( WTinx, inx)-NRNISULFATE( WTinx, inx-1)) + &
            NRNISULFATE( WTinx, inx-1)
  WATERNI = Coeff*(NRNISULFATE( WTinx+1, inx)-NRNISULFATE( WTinx+1, inx-1)) + &
            NRNISULFATE( WTinx+1, inx-1)


! do the samething for Sulfur achid
 inx = 1
WLNRSEARCH : DO While (NRNISACID(wlinx, inx) < WL .and. inx <= ANL ) 
              inx = inx + 1 
           END DO WLNRSEARCH
            
if (inx > ANL ) then 
  print*, 'the specified wavelength is out of the data in Sulfur acid'
  print*, 'WL = ', WL 
! note the infrared spectrum only goes up to 25um   
  inx = ANL
endif


! interpolation to get NR and NI for five different salts
  Coeff = (WL -  NRNISACID(1, inx-1))/(NRNISACID(1, inx)  - &
                                         NRNISACID(1, inx-1))
  
  NR(6) = Coeff*(NRNISACID( SAinx, inx)-NRNISACID( SAinx, inx-1)) + &
           NRNISACID( SAinx, inx-1)
  NI(6) = Coeff*(NRNISACID( SAinx+1, inx)-NRNISACID( SAinx+1, inx-1)) + &
           NRNISACID( SAinx+1, inx-1)

print*, 'NR = ', NR, 'Water NR = ', WATERNR
print*, 'NI = ', NI, 'Water NI = ', WATERNI
print* , 'NP = ', NP, 'ANP = ', ANP
print*,  'NL = ', NL, 'ANL = ', ANL

END
