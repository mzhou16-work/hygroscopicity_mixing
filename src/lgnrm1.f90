!************************************************************************/
!*  File Name:								*/
!*									*/
!*  Purpose:								*/
!*	To construct a single log-normal size distribution		*/
!*									*/
!*  Description:							*/
!*	The log-normal distribution take the following form:		*/
!*									*/
!*	n(r)dr = (1/(sqrt(2*pi)*sigma*r))				*/
!*			*exp(-(ln r - ln rg)^2/sigma^2)dr		*/
!*									*/
!*    where								*/
!*	ln rg = int(0,inf) ln r n(r)dr					*/
!*	sigma^2 = int(0,inf) (ln r - ln rg)^2 n(r)dr			*/
!*									*/
!* $Log$								*/
!*									*/
!*  Called Function/s:							*/
!*									*/
!*  External Devices:							*/
!*									*/
!************************************************************************/
!************************************************************************/
!  Note: This program is different from the program of lgnrm1.f in 
!  /x1/home/li/MIE which is from Kuo(original one). The difference is 
!  that in this program, input totcon represent the fraction of particle
!  number for some specific mode.   ----- Xiang Li   07/31/96


      subroutine lgnrm1( logscl, rg, sigma, nbin, poly, r, wt, &
     				totcon, mom2, mom3, mom4 )

!
!	nbin	: number bins in the size distribution
!	rg	: mean of the log-normal distribution as defined above
!	sigma	: standard deviation of the log-normal distribution
!	poly	: polydisperse distribution
!	r	: quadrature radii
!	wt	: quadrature weights
!	totcon:	total concentration
!	reff	: effective radius
!	veff	: effective variance
!
      logical	logscl
      integer	nbin
      real	rg, sigma, totcon, frac
      real	poly(1), r(1), wt(1)

!
!	const:	constant = sqrt( 2*pi )
!
      real	const

!
!	crite:	criterion used to define rmax and rmin
!	rmax:	maximum radius used for the distribution
!	rmin:	minimum radius used for the distribution
!
      real	crite, rmax, rmin
      data	crite	/1.0e-06/

!
!	con:	concentration a particular radius
!	mom2	: second moment of the distribution
!	mom3	: third moment of the distribution
!	mom4	: fourth moment of the distribution
!	ri	: current radius
!	wi	: current weight
!	i	: running index
!
      real	con, mom2, mom3, mom4
      integer	i


        frac = totcon
!
!  Define a frequently used constant
!
      const = 1.0/(sqrt( 2.0*3.1415927 )*sigma)

!
!  Find the maximum and minimum radii of the size distribution.
!	Since the computer doesn't understand analytical expressions,
!	the size distribution has to start and end at some numbers.
!	The maximum and minimum radii are found according to the following
!	procedure.  The radius is increased/decreased from rg by a factor
!	of 2 each time, until the concentration at that radius is less
!	than a pre-defined fraction of the concentration at rg.
!
      rmin = rg
    1 rmin = rmin/2.
      con = conlog( const, rg, sigma, rmin )
      if (con .gt. crite) go to 1

      rmax = rg
    2 rmax = rmax*2.
      con = conlog( const, rg, sigma, rmax )
      if (con .gt. crite) go to 2

!
!  Find Lobato quadrature points and weights
!
      if (nbin/2 .eq. 0) nbin = nbin - 1
      if (logscl) then
	call lobato( nbin, alog( rmin ), alog( rmax ), r, wt )
	do 10 i = 1, nbin
	  r(i) = exp( r(i) )
	  wt(i) = wt(i)*r(i)
   10   continue
      else
        call lobato( nbin, rmin, rmax, r, wt )
      endif

!
!  Construct the sizedistribution.
!	Once again, since analytical expression is not possible,
!	we have to make do with histograms.
!
      totcon = 0.0
      mom2 = 0.0
      mom3 = 0.0
      mom4 = 0.0
      do 20 i = 1, nbin
	ri = r(i)
	wi = wt(i)
        con = conlog( const, rg, sigma, ri )
	poly(i) = con*frac

	con = con*wi
	totcon = totcon + con
	con = con*ri*ri
	mom2 = mom2 + con
	con = con*ri
	mom3 = mom3 + con
	con = con*ri
	mom4 = mom4 + con
   20 continue

      reff = mom3/mom2
      veff = mom4*mom2/mom3/mom3 - 1.0

        totcon = totcon*frac

      return
      end

      real function	conlog( const, rg, sigma, r )
!
!	const:	constant
!	r:	radius
!	rg:	mean radius
!	sigma:	standard deviation of the log-normal distribution
!
      real	const, r, rg, sigma
      real	b

      b = alog( r ) - alog( rg )

      conlog = (const/r)*exp( -(b*b)/(2.0*sigma*sigma) )

      return
      end
