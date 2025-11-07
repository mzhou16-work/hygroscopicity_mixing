
        subroutine core_shell(PWL, PNANG, PNLGNDR, PR, PSTD, PSNR, PSNI, &
                              PCNR, PCNI, PCSRATIO, PRHO, &
                              Oreff, Oveff, Oarea, Ovolume, OSSA, OASY, OCEXT, OCSCA, &
                              OMEXTEFFCY, OEXTEFFCY, OHBSCA, OEXTToBSCA, &
                              OPHASE, OANGLE, OLGNDR)     

        implicit none
!
!	parameters for array dimension
!	mxnang	: maximum number of angles
!	mxnint	: maximum number of bins in the size distribution
!	mxnlgn	: maximum number of terms in the Legendre expansion of the
!		      phase funtion
!   maxband : maximum number of frequency intervals for mie calculation
!             maxband >= (lamda2-lamda1)/d_lamda
!	mu2cm	: conversion factor from micron to centimeter
!	const	: mu2cm**2

      integer, parameter:: mxnang = 1025, mxnint = 1025, mxnlgn = 1024, mxnband = 1000
      real,   parameter :: mu2cm = 1.0e-4, const = 1.0e-8, pi = 3.1415927


! input file
       integer :: PNANG  , &    ! phase function angles
                  PNLGNDR       ! number of lengendre polynomials 
 
       real :: PWL       , &    ! wavelength
               PR        , &    ! rg
               PSTD      , &    ! thegmag
               PSNR      , &    ! real part of shell
               PSNI      , &    ! imagnieary part of NI
               PCNR      , &    ! nr of core
               PCNI      , &    ! ni of core
            PCSRATIO     , &    ! radisu of core/shell
            PRHO                ! average density of the particle 
             

! output
       real :: Oreff     , &    ! effective radius
               Oveff     , &    ! effective varance
               OSSA      , &    ! single scattering albedo
               OASY      , &    ! asymetric factor
               Oarea     , &    !
               Ovolume,    &
               OCEXT, OCSCA,  &       ! extincion and scattering cross section
             OMEXTEFFCY, OEXTEFFCY, & ! extinction efficiency and mass extinction efficiency
             OHBSCA, OEXTToBSCA       ! hemispheiric back scattering fraction and extinctino to backscattering ratio

       real, dimension(PNANG) ::   OPHASE, OANGLE
       real, dimension(0: PNLGNDR)::  OLGNDR   ! phase function, angle, and legendre expansion coefficient                   

! other internal
       real :: aqeff, fmqeff
       real :: ceflgn

! internal leftover
       real :: WVNO    , &    ! wave number
               f1      , & 
               tot_num , &
               alamda  , &
               aerovol , &
               amm     , &
               totals 
                
       integer :: i, j, k            

                  
!
!	index of refration
!	lambda	: wavelength
!       lamda1 and lamda2 : starting and ending wavelengths
!       d_lamda : delta_wavelength, the wavelength interval(resolution)
!                 for spectral calculation
!	nr	: real part of the index of refraction
!	ni	: imaginary part of the index of refraction
!	nicut	: cutoff of the imaginary part
!	x	= 2*pi/lambda; x*radius = size parameter
!
      real	lambda, nr, ni, x, lamda1, lamda2, d_lamda

!
!	common block for subroutine miev0
!	xx	: size parameter
!	ior	: index of refraction
!	nicut	: cutoff of the imaginary part of the index of refraction
!	nang	: number of angle requested
!	qmu	: cosine of quadrature angles
!	qext	: extinction efficiency
!	qsca	: scattering efficiency
!	gfac	: asymetry factor
!
!	qabs	: absorption efficiency
!
      integer   nang, n_mode,mynang
      real      xx, nicut, qabs, qmu(mxnang)
      real      qext, qsca, gfac
      double complex s1(mxnang), s2(mxnang)
      double complex news1(mxnang,2),news2(mxnang,2)
      complex ior,iorcore

!^^^^^^^^^^^^^^^^^^^^^^^^^^zjl
      real qmubak(mxnang)
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
!
!	nlgn	: requested number of Legendre expansion terms
!	nmod	: resultant number of Legendre expansion terms
!	qwt	: quadrature weights
!	phase	: phase function value at quadrature angles
!	pk	: Legendre expnasion coefficient
!       b_phase : band phase function value at quadrature angles
!       b_pk    : band Legendre expnasion coefficient
!	plgndr	: Legender polynomial values at quadrature angles
!
      integer	nlgn, nmod
      real	qwt(mxnang), phase(mxnang), pk(0:mxnlgn)
      real      b_phase(mxnang), b_pk(0:mxnlgn)
      real	plgndr(mxnang,0:mxnlgn),rate

!
!	about the size distribution
!	logscl	: if .true. then log-scale is used
!	ntype	: type of size distrigution
!	nbin	: number of bins in the size distribution
!	param	: sizedistribution parameter
!	rg	: mean radius of log-normal distribution
!	sigma	: standard deviation for log-normal distribution
!	totcon	: total concentration
!	reff	: effective radius
!	veff	: effective variance
!
      logical	logscl
      data	logscl /.false./
      integer	ntype, nbin
      real	rg, sigma, totcon, reff, veff
      real	param(16)

!
!*****	Size distribution type strings
!
      character*20	typstr(8)
      data	typstr/ 'Modified Gamma', 'Bimodal Gamma', & 
     			'Single Log-Normal', 'Double Log-Normal', & 
     			'Triple Log-Normal', 'Power-Law', '', '' /
!
!	dist	: the size distrigution
!	radius	: quadrature radius
!	dwt	: quadrature weights for the size distribution
!
      real	dist(mxnint), radius(mxnint), dwt(mxnint)
      real	di, dwi, ri, risq,rcore

!
!	stdext	: standard extinction coefficient
!
      real	stdext

!
!*****	Bulk properties
!	bext	: volumn extinction coefficient
!	bsca	: volumn scattering coefficient
!	babs	: volumn absorption coefficient
!	asym	: asymetry factor
!       b_bext    : volumn extinction coefficient
!       b_bsca    : volumn scattering coefficient
!       b_babs    : volumn absorption coefficient
!       b_asym    : asymetry factor
!       tot_weight: total weight for the band average
!
      real	bext, bsca, babs, asym
      real      b_bext, b_bsca, b_babs, b_asym, tot_weight
      real	tmp1, tmp2, tmp
      real      rgv, rgm, frac_vol, fnr, fni, fsigma
      real      frac_num, fmom2, fmom3, fmom4
      real      mom2, mom3, mom4, tot_mom2, tot_mom3, tot_mom4
      real      con
!
!*****	Print control
!	When .true.
!	prnt(1)	: basic information about current run, including:
!		  * input information
!	prnt(2)	: detail size distribution information
!	prnt(3)	: bulk optical properties
!	prnt(4)	: output phase function Legendre expansion coefficients
!	prnt(5)	: output phase function moments
!	prnt(6)	: output quadrature angles and phase function values
!		  at these angles
!
      logical	prnt(8)

      DOUBLE PRECISION fncr,fnci
  
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


!ccccccccccccccccccccc
!		     c
!  PROGRAM BEGINS    c
!		     c
!ccccccccccccccccccccc

!**** initalize other parameters
      logscl= .True.
!      nang= 513

      nang = PNANG
 
!      nlgn=512
!      nbin=513
      
      nlgn=PNANG-1
      nbin=PNANG

      stdext = 10.0

! here set my nang used for function  DMiLay
        mynang=(nang+1)/2

!      write(*, *) 'lamda SSA  ASY  Cext  Csca Casb AEROVOL PLANK' 
      
! passing int parameter
      lamda1 = PWL
      rgm = PR
      fsigma = PSTD
      rate = PCSRATIO
      fncr = PCNR
      fnci = PCNI
      fnr = PSNR
      fni = PSNI
 
!      read(2,*) lamda1,rgm,fsigma,rate,fncr,fnci,fnr, fni

! prepare parameter for function  DMiLay
        WVNO=2*pi/lamda1

      nmod = 0
!
!*****	Calculate Lobato quadrature angles and weights
!
      call lobato( nang, -1.0, 1.0, qmu, qwt )
!
!*****	Calculate values of Legendre polynomials at quadrature angles
!
      do 10 i = 1, nang
         qmubak(i)=qmu(i)
        call legndr( nlgn, qmu(i), pk )

	do 11 k = 0, nlgn
	  plgndr(i,k) = pk(k)
   11   continue
   10 continue

!  ************* convert size distribution parameters ****************
  

!  ******* convert rgv to rgm using formula in Senfeld's Book: *********
!  ******* Air Pollution, Pg. 286, eq. 7.45 ****************************

!  ******* convert volume fraction to number density fraction **********
!  ******* using formula in Senfeld's Book: *********
!  ******* Air Pollution, Pg. 286, eq. 7.44 ****************************

       fsigma = alog(fsigma)
       f1 = fsigma*fsigma
!       rgm = rgv / exp(3.0*f1)
       frac_num = 1 / (rgm*rgm*rgm*exp(4.5*f1))
!       print*, 'rgm = ', rgm 
       tot_num = 0.0
       tot_num = tot_num + frac_num
       frac_num = frac_num / tot_num
       
      alamda = lamda1
      x = 2.0*pi/alamda
      phase(1:mxnang) = 0.0      

!
!*****	Loop through each size bin
!
      bext = 0.0
      bsca = 0.0
      babs = 0.0
      asym = 0.0

!   *********** for each lognormal distribution ************
!
       totcon = frac_num
!       print*, 'totcon = ', totcon
       rg = rgm
       sigma = fsigma

        call lgnrm1( logscl, rg, sigma, nbin, dist, radius, dwt, & 
                               totcon, mom2, mom3, mom4 )
      
      fmom2 = totcon * mom2
      fmom3 = totcon * mom3
      fmom4 = totcon * mom4 
      con = totcon

      aerovol=(4.*pi/3)*mom3/totcon

      ior = cmplx(fnr, -abs(fni))
      iorcore=cmplx(fncr,-abs(fnci))


      nicut = 1.0e-7
      amm=0.0e-7
      totals = 0.0 
 
      do 30 i = 1, nbin
	di = dist(i)
	ri = radius(i)
	dwi = dwt(i)
        amm=amm+di*dwi
        totals = totals + di*dwi*ri**2*pi     
      
	xx = x*ri
        rcore=ri*rate

        call DMiLay(rcore,ri,WVNO,ior,iorcore,qmubak, &
            mynang,qext,qsca,gfac,news1,news2,mxnang)

        do 88 j=1,mynang
          s1(nang+1-j)=news1(j,1)
          s1(j)=news1(j,2)
          s2(nang+1-j)=news2(j,1)
          s2(j)=news2(j,2)
  88    continue 


	risq = ri*ri
	qabs = qext - qsca
        bext = bext + risq*qext*di*dwi*pi
	bsca = bsca + risq*qsca*di*dwi*pi
        babs = babs + risq*qabs*di*dwi*pi
	asym = asym + gfac*di*dwi

! calculate phase function

	do 31 j = 1, nang
	  tmp1 = abs( s1(j) )
	  tmp2 = abs( s2(j) )
	  tmp = (tmp1*tmp1 + tmp2*tmp2)/2.0
	  phase(j) = phase(j) + tmp*di*dwi
   31   continue
   30 continue


! Size bin loop is over
      do 40 i = 1, nang
	phase(i) = phase(i)*4.0*pi/(x*x*bsca)
   40 continue
     
! legendre polynomials
      do 50 k = 0, nlgn
	tmp = ceflgn( nang, phase, qwt, k, plgndr(1,k) )
	b_pk(k) = tmp
   50 continue

! ************** Calculate Reff and Veff ***********************

       tot_mom2 = 0.0
       tot_mom3 = 0.0
       tot_mom4 = 0.0
       totcon = 0.0

       tot_mom2 = tot_mom2 + fmom2
       tot_mom3 = tot_mom3 + fmom3
       tot_mom4 = tot_mom4 + fmom4
       totcon = totcon + con

      reff = tot_mom3/tot_mom2
      veff = tot_mom4*tot_mom2/tot_mom3/tot_mom3 - 1.0
      

! extinction efficiency and mass extinction efficiency
   aqeff = bext/totals
   fmqeff = 3./4.*aqeff / (PRHO * reff )


!  correction
!      here to take care the case that a very very small std is inputed
      if(amm .gt. 1.001) then
            bext = bext/amm
            bsca = bsca/amm
            babs = babs/amm
            asym = asym/amm
      endif



! output parameter
       OCEXT = bext
       OCSCA =  bsca
       OSSA = bsca/bext
       OASY =  b_pk(1)/3
       OPHASE(1:NANG) = real(phase(1:NANG))
       OANGLE(1:NANG) = acos(qmu(1:NANG))*180./pi 
       OLGNDR(0:PNLGNDR) = b_pk(0:PNLGNDR) 
       OReff = reff
       OVeff = Veff
       Oarea = tot_mom2 
       Ovolume = tot_mom3 
       OMEXTEFFCY= fmqeff
       OEXTEFFCY=  aqeff
       OHBSCA = 0.5 * (1. - 7/8.* OASY )  ! Wiscombe & Granes, 1974, JAS 
       OEXTToBSCA = 4*pi/(OSSA * Ophase(1))   

      end



!********************************************************************
      FUNCTION INTERPRO(A,X,Y,NX)

!  GENERATED BY JIANGLONG ZHANG FOR INTERPOLATION
!  A  ARGUMENT
!  X  TABLE OF X'S
!  Y  TABLE OF Y'S
!  NX NUMBER OF ENTRY IN TABLE 'X'
!  X(I) MUST LESS THAN X(I+1)

       DOUBLE PRECISION INTERPRO,A,X,Y,CX,CY
       DIMENSION X(*),Y(*),CX(10),CY(10)

       DO 10 I=1,NX
       IF(A .LE.X(I)) GO TO 30
  10   CONTINUE
       I=NX

  30   I=I-1
       IF(I.LT.1) I=1
       IF((I+2) .GT. NX) I=NX-1
       
       DO 60 J=1,2
       CX(J)=X(I)-A
       CY(J)=Y(I)
  60   I=I+1
       
       INTERPRO=(CX(2)*CY(1)-CX(1)*CY(2))/(CX(2)-CX(1))
     
       RETURN 
       END
