      subroutine lobato( n, a,b, ab, wt)
!
!     Abscissas and weights for lobatto quadrature on interval (a,b)
!     Ref.   Michels, Math. Comput. 17, 237 - 244 (1963)
!
!     input
!
!     n = angular resolution
!     a = left bound of interval
!     b = right bound of interval
!
!     output
!
!     ab = discrete angle array ( radians )
!     wt = quadrature weights ( radians )
!

!
!   Variables
!
      double precision	c1, c2, c3, c4, pi
      double precision	w, tol, npnm1, p,pm1, pm2, ppr, p2pri
      double precision	tmp, x,xi, prod
      dimension ab(1), wt(1)

!
!   Initialization
!
      data c1/-.375d0/,c2/.0234375d0/,c3/-.23027344d0/,c4/1.7013192d0/
      data tol/1.d-11/
      data pi/3.1415927d0/

!
!     Procedure
!
      nm1 = n - 1
      ab(1) = -1.0
      ab(n) = 1.0
      npnm1 = n*nm1
      w = 2.0/npnm1
      wt(1) = w
      wt(n) = w
      d = 1./sqrt((n - 0.5)**2 + (pi**2 - 4.)/(4.*pi**2))
      con1 = 0.5*(b - a)
      con2 = 0.5*(b + a)
      lim = (n - 2)/2
      do 5 k = 1, lim
        z = (k + 0.25)*pi
        zisq = 1./(z*z)

!
!     rootbf approximates the kth zero of the bessel function j1(x)
!     from mcmahon's expansions ( cf. Abramowitz, Handbook of Math. Funcs. )
!
      rootbf = z*(1. + zisq*(c1 + zisq*(c2 + zisq*(c3 + zisq*c4))))
!
!     Initial guess for kth root of derivative of p-sub-(n-1) (leg poly)
!
        x = cos(d*rootbf)

 1      pm2 = 1.0
        pm1 = x
        do 2 nn = 2, nm1
!
!     recursion relationship for legendre polynomials
!
          p = ((2*nn - 1)*x*pm1 - (nn - 1)*pm2)/nn
          pm2 = pm1
          pm1 = p
    2   continue
        tmp = 1.0/(1.0 - x*x)
        ppr = nm1*(pm2 - x*p)*tmp
        p2pri = (2.0*x*ppr - npnm1*p)*tmp
!
!     newton iterative improvement of root
!
        xi = x - ppr/p2pri
        if(dabs(xi - x) - tol) 4, 4, 3
 3      x = xi
        go to 1
 4      ab(k+1) = - x
        wt(k+1) = w/(p*p)
        ab(n-k) = x
        wt(n-k) = wt(k+1)
    5 continue
      if(mod(n, 2).eq.0) go to 7
      ab(lim+2) = 0.
      nm2 = n - 2
      prod = 1.0
      do 6 k = 1, nm2, 2
        fterm = nm1 - k
        prod =  dble(fterm)/(n - k)*prod
    6 continue
      wt(lim+2) = w/prod**2
    7 diff = b - a
      do 8 k = 1, n
        ab(k) = (ab(k) + 1.0)*diff/2.0 + a
        wt(k) = con1*wt(k)
    8 continue

      return
      end
