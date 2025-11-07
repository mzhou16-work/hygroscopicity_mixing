! c************************************************************************/
! c*									*/
! c* $Id$									*/
! c*									*/
! c*  Site:	IAS, SDSM&T						*/
! c*		501 E. St. Joseph St.					*/
! c*		Rapid City, SD 57701-3995				*/
! c*		U.S.A.							*/
! c*  Tel:	(605) 394-2291						*/
! c*  Fax:	(605) 394-6061						*/
! c*  Email:	kuo@chaos.ias.sdsmt.EDU					*/
! c*									*/
! c************************************************************************/
! c*  File Name:								*/
! c*									*/
! c*  Purpose:								*/
! c*	To construct a single log-normal size distribution		*/
! c*									*/
! c*  Description:							*/
! c*	The log-normal distribution take the following form:		*/
! c*									*/
! c*	n(r)dr = (1/(sqrt(2*pi)*sigma*r))				*/
! c*			*exp(-(ln r - ln rg)^2/sigma^2)dr		*/
! c*									*/
! c*    where								*/
! c*	ln rg = int(0,inf) ln r n(r)dr					*/
! c*	sigma^2 = int(0,inf) (ln r - ln rg)^2 n(r)dr			*/
! c*									*/
! c* $Log$								*/
! c*									*/
! c*  Called Function/s:							*/
! c*									*/
! c*  External Devices:							*/
! c*									*/
! c************************************************************************/
! c************************************************************************/

      subroutine lgnorm( logscl, n, param, nbin, poly, r, wt, &
     				totcon, reff, veff )

! c
! c    Inputs
! c	logscl	: if .true. log-scale is used
! c	n	: combination of n log-normal distributions
! c	param	: parameter of the distribution
! c	nbin	: number bins in the size distribution
! c
! c    Outputs
! c	poly	: polydisperse distribution
! c	r	: quadrature radii
! c	wt	: quadrature weights
! c	totcon	: total concentration
! c	reff	: effective radius
! c	veff	: effective variance
! c
      logical	logscl
      integer	nbin
      real	totcon
      real	param(1), poly(1), r(1), wt(1)

! c
! c	const:	constant = 1/(sqrt( 2*pi )*sigma)
! c
      real	const(6)

! c
! c	crite:	criterion used to define rmax and rmin
! c	rmax:	maximum radius used for the distribution
! c	rmin:	minimum radius used for the distribution
! c
      real	crite, rmax, rmin

      data	crite	/ 1.e-06/

! c
! c	con:	concentration a particular radius
! c	mom2	: second moment of the distribution
! c	mom3	: third moment of the distribution
! c	mom4	: fourth moment of the distribution
! c	ri	: current radius
! c	wi	: current weight
! c	i, j	: running indices
! c
      real	con, mom2, mom3, mom4
      integer	i, j

      real	tmp

! c
! c  Define  frequently used constants.
! c
      do 10 i = 0, n - 1
        const(i + 1) = param(i*3+1)/(sqrt( 2.0*3.1415927 )*param(i*3+3))
   10 continue

! c
! c  Find the maximum and minimum radii of the size distribution.
! c	Since the computer doesn't understand analytical expressions,
! c	the size distribution has to start and end at some numbers.
! c	The maximum and minimum radii are found according to the following
! c	procedure.  The radius is increased/decreased from rg by a factor
! c	of 2 each time, until the concentration at that radius is less
! c	than a pre-defined fraction of the concentration at rg.
! c
      rmin = param(2)
      do 20 i = 1, n - 1
	tmp = param(i*3+2)
	if (tmp .lt. rmin) rmin = tmp
   20 continue
    1 rmin = rmin/2.
      con = 0.0
      do 30 i = 0, n - 1
        con = con + conlog( const(i+1), param(i*3+2), &
                            param(i*3+3), rmin )
   30 continue
      if (con .gt. crite) go to 1

      rmax = param(2)
      do 40 i = 1, n - 1
	tmp = param(i*3+2)
	if (tmp .gt. rmax) rmax = tmp
   40 continue
    2 rmax = rmax*2.
      con = 0.0
      do 50 i = 0, n - 1
        con = con + conlog( const(i+1), &
							param(i*3+2), param(i*3+3), rmax )
   50 continue
      if (con .gt. crite) go to 2

! c
! c  Find Lobato quadrature points and weights
! c
      if (nbin/2 .eq. 0) nbin = nbin - 1
      if (logscl) then
	call lobato( nbin, alog( rmin ), alog( rmax ), r, wt )
	do 60 i = 1, nbin
	  r(i) = exp( r(i) )
	  wt(i) = wt(i)*r(i)
   60   continue
      else
        call lobato( nbin, rmin, rmax, r, wt )
      endif

! c
! c  Construct the sizedistribution.
! c	Once again, since analytical expression is not possible,
! c	we have to make do with histograms.
! c
      totcon = 0.0
      mom2 = 0.0
      mom3 = 0.0
      mom4 = 0.0
      do 70 i = 1, nbin
	ri = r(i)
	wi = wt(i)

	con = 0.0
	do 71 j = 0, n - 1
          con = con + conlog( const(j+1),&
                              param(j*3+2), param(j*3+3), ri )
   71   continue
	poly(i) = con

	con = con*wi
	totcon = totcon + con
	con = con*ri*ri
	mom2 = mom2 + con
	con = con*ri
	mom3 = mom3 + con
	con = con*ri
	mom4 = mom4 + con
   70 continue

      reff = mom3/mom2
      veff = mom4*mom2/mom3/mom3 - 1.0

      return
      end
