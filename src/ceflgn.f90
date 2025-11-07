      real function	ceflgn( nang, phase, wt, k, plgndr)
!
!     Legendre polynomial expansion coefficient for phase function
!
!     The coefficients are normalized by the p(0) term such that p(0)-norm
!     term is equal to 1.  The return array pl begins with the p(1)-norm term.
!
!     input
!
!	nang	: angular resolution
!	phase	: phase function array
!	wt	: quadrature weights
!	k	: the k-th coefficient
!	plgndr
!
!     output
!
!	c_sub_k	: the Legendre expansion coefficient
!
      real	plgndr(1),&		! phase ftn at lobato quadrature angles
     		wt(1)			! lobato quadrature weights
      real 	phase(1)	! P-sub-k evaluated at the angles

!
      const = float(k) + 0.5
      ceflgn = 0.0

!
!	Numerical integration using quadrature angles and weights.
!
      do 5 j = 1, nang
        ceflgn = ceflgn + wt(j)*plgndr(j)* phase(j)
    5 continue

      ceflgn = const*ceflgn

      return
      end
