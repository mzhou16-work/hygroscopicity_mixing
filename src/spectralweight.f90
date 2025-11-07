    module spectralweight
    contains   
    real function plank( alamda, temp )
!
!      This function calculates radiance using plank formula when
!      wavelength and blackbody temperature are given.

!
!      input:
!               alamda  : wavelength in micron 
!               temp   : temperature in Kelvin.

!      output:  radiance  unit: W/m2*um*sterdian

!      const used in Plank function, hc_k

!       write(*,*) alamda, temp

       hc_k = 14388.443

       const = 1.1910468e+8

       x = hc_k / (alamda*temp)
       ex = exp(x)
      
       alamda_5 = alamda**5.0

       plank = const / (alamda_5 * (ex -1.0))
!       write(*,*) plank
        
       end function
   end module      
