! This code is modified from original Wang's multiple lognormal Mie code
! the code is changed to f90. Integrate with mixing code.   
!                                                 Wang , Nov 23, 2005.  
! CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!C to calculate the mutiple lognormal size
!C distribution.
!C step 1: read the paramters for each mode
!C step 2: generate the Guassian Quardrature and weights
!C	   for use by the Mie code
!C step 3: loop through each bins
!C step 4: integration and get the optical properties
!C Jun WAng, 07/28/01
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!   variable:
!  +  nang :  number of the scatering angles, also the nuber of
!              gaussian quadrature number.
!  +  Maxm :   Maximum mode of lognormal size distribution
!  +  wl   :  wavelength
!  +  nr,ni:  refractive index
!  +  ncon :  num concentration ( or how many numbers per volume )
!  +  ext  :  extinction efficiency.( coefficinet per volume)
!  +  sca  :  scattering efficiency.( coefficient per volume)
!  +  asy  :  asymetry factor
!  +  phase:  phase function ( has nang angles)
!  +  ssa  :  single scattering albedo
!  +  Qext :  extinction efficiency per particle
!  +  Qsca :  scattering efficiency per particle
!  +  the out volume extinction coefficient is in the unit of Mm
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
! INPUT:
!        rg, thegma  : two parameters for lognormal size distribution  
!       minr, maxr   : minimum and maximum size range for integration  
!        nr, ni      : real and imagineary part of refractive index 
!          rp        : density
!          wl        : wavelength


       subroutine sulf_stdmie( rg, thegma, rmin, rmax, nr, ni, rrp, wl,  &
                               Oreff, Oveff, Oarea, Ovolume, OSSA, OASY, OCEXT, OCSCA, &
                               OMEXTEFFCY, OEXTEFFCY, OHBSCA, OEXTToBSCA, &
                               OPHASE, OANGLE, OLGNDR)
 
       implicit none	
       integer, parameter::  nang=1025,   &    ! # of angles
                             Maxm = 1,   &    ! # of log mode
                             nlgndr = 20      ! # of Legendre polynomials  
       real, parameter::     pi=3.1215926

! initialize the input
       real rg, thegma, rmin, rmax, nr, ni, rrp, wl 

! output variable
       real Oreff, Oveff, Oarea, Ovolume, OSSA, OASY, OCEXT, OCSCA, OMEXTEFFCY, OEXTEFFCY 
       real OHBSCA, OEXTToBSCA            ! hemispheric backscattering, Ext to BCA ratio 
       real, dimension(nang):: OPHASE, OANGLE
       real, dimension(0:nlgndr):: OLGNDR 

! use function
       real ceflgn

! internal variables
! array for legendre polynomials
       real plgndr(nang, 0:nlgndr), pk(0:nlgndr), tmpplgndr(0:nlgndr)

! other internal variables 
       real aasy, eext, sssa 
       double precision ::  X          ,    &            ! size parameter 
                            area,           &            !
                            tmpr,           &            !
                            Qext,           &            !
                            tnr,            &            !
                            Qsca,           &            !
                           p11(nang),       &            !
                            asy ,           &            !
                            ext,            &            !
                            sca,            &            !
                          phase(nang),      &            !
                          asy_n,            &            !
                          asy_d,            &
                          totcal,           &
                          r,                &
                          dr,               &
                          rlogpass,         &
                         TmpCon(MaxM),      &
                         param(3*MaxM)                          
 
       real, dimension(nang) :: cgaus,     &            ! Gaussian points
                                 pdgs,      &            ! gaussian weight 
                                  rr,       &            !
                                  ww       
       real :: rrmode, rrmin, rrmax, GF, rnr, rni, rp          
       integer :: i, j, k,iii, jjj, kkk, n, np
       real totcon,fmom2, fmom3, fmom4, totw, reff, &
            veff, totals, totalv, volume, ssa , aqeff, &
            fmqeff, aarea, vvolume 
      print *, '  - Calling SULF_STD_MIE...'
! some intiliazation
       n = 1             ! total mode =1
       param(1)  = 1.0
       param(2) = rg
       param(3) = thegma
       rnr = nr
       rni = ni
       rp = rrp
     
      print 1000, 'input : rg   = ', param(2), ', vg   = ', param(3)
      print 1000, '        nr   = ', rnr,      ', ni   = ', rni  
      print 1000, '        rmin = ', rmin,     ', rmax = ', rmax
      print 1000, '        wl   = ', wl,       ', rp   = ', rp   
1000  FORMAT (5x, A, 1(F5.3), A, 1(F6.3))


! output for phase function
!        open(2,file="SO4-phase-New.dat")

! out put file
!        open ( 4, file = 'SO4-scattering-New.dat')

!        write(4,*) '  wl   cext(um2) csca(um2)    sca     asy'// &
!                    'reff(um)  veff  extefficiency masseff(g/m2)'// &
!                  ' totalv(um3)'
	

! calculate effective radius and effective variance

         totcon = 0.0
	 fmom2 = 0.
	 fmom3 = 0.
	 fmom4 = 0.
	 
         call angle_gauss(rmin, rmax, rr, ww, nang)
        print*, '  - SULF_STD_MIE: Calling gauss is over...'        

         do np = 1, nang
             do i = 0, n-1
	      call lognormal(param(i*3+1), param(i*3+2),  &
                              param(i*3+3),  rr(np)*1.0D0, TmpCon(i+1))
             enddo 
             totcon = 0.0 
             do i = 1, n 
               totcon = tmpcon(i) + totcon
             enddo 
            ww(np ) = totcon*ww(np)
	 enddo
  
	 TotW = 0.
         do np = 1, nang
           TotW = TotW + ww(np)
         enddo

         do np = 1, Nang
	    ww(np) = ww(np)/TotW
         enddo

         do np = 1, Nang
	   fmom2 = rr(np)*rr(np)*ww(np) + fmom2
           fmom3 = rr(np) * rr(np) * rR(np) *ww(np) + fmom3
           fmom4= rr(np)*rr(np)*rr(np)*rr(np)*ww(np) + fmom4
         enddo

           reff =  fmom3/fmom2
	   veff = fmom4*fmom2/fmom3/fmom3-1 

           Aarea = fmom2  
           Vvolume = fmom3 
         
	 print 1000,  'reff = ', reff, ' ,veff = ', veff
		  	 

! generate the gaussian points and weights
	call  angle_gauss(-1.,1.0, cgaus, pdgs,nang)
	
!	write(*,*) (cgaus(k), pdgs(k), k = 1, nang)
! 100    format(1x, (f10.4, f10.4))
! loop through each r, based on 6S, log(r/r+dr) <0.003 has the best 
! effect for the integration.
! initialize values
	r = rmin*1.0D+00
	rlogpass = 0.011d+00

         do k = 1, nang
         phase(k) = 0.D0
	 p11(k) = 0.D0
         enddo
	 
	 ext = 0.D+00
         sca = 0.D+0
         asy_n = 0.D+00
         asy_d = 0.D+00
         asy = 0
	 totcal = 0
	 totals = 0.
	 totalv = 0.

! loop each size bins
!        print*, 'rmin = ', r 	
	do 10 while ( r.ge.rmin .and. r.le.rmax )
	   dr = r*(10**rlogpass - 1.D+00)

! calcualte #for each mode, and the max value should be the #
! corresponding to the r. Note, several mode will be overlapped
! together in some radius. 
	
	   do j = 0, n-1

! note in here is the # concentration in terms of radius

             call lognormal(param(j*3+1), param(j*3+2), &
                param(j*3+3), r, TmpCon(j+1))
	   enddo
             tnr = 0.

            do j = 1, n
		tnr = tnr + TmpCon(j)   
            enddo

!	  write(*,*) r, tnr 
! calculate the surface area 

       	    area =  pi * r**2.D+00
       	    volume =  4./3. * pi * r**3.D+00

! # of particles with radius = r
	    tnr = tnr * dr
	    totcal = tnr + totcal
! Mie parameters
	    X = 2.d+00*pi*r/wl   

! call mie code
	    call EXSCPHASE( X, rnr, rni, Qext, Qsca, p11,cgaus,nang)

! calculate the extinction coefficient uisng extinctio efficiency	
	     ext = ext + Qext*tnr*area
	     sca = sca + Qsca*tnr*area
	     totals = totals + tnr*area
             totalv =  totalv + tnr*volume
 !            print*, 'ext = ', Qext, tnr, area 

! calculate the phase fuction
	     do k = 1, nang
	     phase(k) =  p11(k)*tnr*area + phase(k)
	     enddo
	
	     r = r + dr
  10      continue


!          write(*,*) " total particles", totcal

! considering how many particles we calculate. normalized to one particle
	
	   ext = ext/totcal
	   sca = sca/totcal
	   totals = totals / totcal
	   totalv = totalv / totcal
!	   write(*,*) ' totcal = ', totcal

! calculate the normalized phase function

         do k = 1, nang
         phase(k) = phase(k)/sca/totcal
         enddo

! calculate the asy factor
	
         do k = 1, nang

         asy_n = asy_n + cgaus(k)*phase(k)*pdgs(k)
	
         asy_d = asy_d + phase(k)*pdgs(k)
         enddo
	
         asy = asy_n/asy_d
         ssa = real(sca/ext)

!  convert ext, sca to volume (cross section/volume, 10^-12m**2/10^-6m**3)
	ext = ext
	sca = sca
	
! averged exticntion efficiency, aqeff, mass extinction efficiency mqeff
! note here the mass extion efficiency is NOT relative to the dry particles. 
! normally, it should be to dry particles. To get to dry particles, 
! we need the mass ratio of wet and dry particles, as well as dry particle extinction
! efficiency
! Since aqeff is normalized to  averaged particle area, it is an efficiency.
! Therefore, this efficiency * area / mass, where mass =rho * 4/3 pi r3 n(r) dr.
! therefore, we can use reff in here.   
! note totalv/totals = reff, in theory

        aqeff = ext/totals
	fmqeff = 3./4.*aqeff / (rp * reff ) 

! Altanatively, we can use, we have verified it already
!        fmqeff = ext/(rp * totalv) 


!        write(4,20)  wl, real(ext),real(sca),ssa, asy, reff, veff, &
!              aqeff, fmqeff, totalv
! 20    format (1x,  f5.2, g10.3, g10.3,  f8.3, f8.3, 5(g10.3))
        eext = real(ext)
        sssa = ssa
        aasy = asy

! Output the phase funciton for different angles
          do jjj = 1, nang   
              call legndr(nlgndr, cgaus(jjj), tmpplgndr) 
!              write(*,*) 'tmpplgndr = ', tmpplgndr
              do kkk = 0, nlgndr 
               plgndr(jjj, kkk) = tmpplgndr(kkk)
              enddo
          enddo
          
          do kkk = 0, nlgndr   
              pk(kkk) = ceflgn(nang, real(phase), pdgs, kkk, plgndr(1,kkk))
          enddo

!        write(2, 60) (pk(kkk), kkk = 0, nlgndr) 
!        write(2, 30) (acos(cgaus(k))*45./atan(1.0), &
!     		phase(k),  k=1,nang)
! 30    format( 1x,  (1x, f10.4, f10.4))
! 60    format (1x, 21(1x, f10.4))   
        
!        close(2)
!        close(4)


! return output 

          do kkk = 1, nang
            OANGLE(kkk) = real ( acos(cgaus(kkk))*45./atan(1.0) )
            OPHASE(kkk) = real ( phase(kkk) )
          enddo

          OLGNDR(0:nlgndr) = pk(0:nlgndr)

!
! pass internal variable to output variable
!
        OCEXT = real(ext)
        OCSCA = real(sca)
        OREFF = reff
        OVEFF = veff
        OEXTEFFCY = aqeff
        OMEXTEFFCY = fmqeff
        OSSA = SSA 
        OASY = real( ASY )
        OHBSCA  = 0.5 * (1. - 7/8.* real(ASY) )  ! Wiscombe & Granes, 1974, JAS 
        OEXTToBSCA = 4*pi/ (SSA * real (phase(1)))    ! Liu, applied opt, 2002
        Oarea = aarea
        Ovolume = vvolume
        
        print 1000, 'total s = ', totals/pi, ', total v = ', totalv/pi*0.75
        2000 FORMAT (A, 2(F6.3))
		
!        print*, 'OHemisphBSCA = ', OHBSCA, '  OEXTToBSCA=',  OEXTToBSCA
!          print*, 'OHBSCA = ', OHBSCA
         print*, '  - SULF_STD_MIE: Sulf_std is over' 
          return

        end

        subroutine lognormal(percentage, rg,thegma,r, nr)
        double precision rg, r, thegma, nr
	    double precision  percentage
        real, parameter:: pi = 3.1415926 
!         double precision percentage
        nr = percentage*1.0D+00/(sqrt(pi*2.)*dlog(thegma)*r)* &
            exp(-(dlog(r/rg)**2/(2*dlog(thegma)**2)))
!        nr = percentage/(sqrt(pi*2.)*dlog10(thegma)*r)* &
!     c       dexp(-(log10(r/rg)**2/(2*dlog10(thegma)**2)))
!        nr = nr / dlog(10.D+00)

        return
        end




