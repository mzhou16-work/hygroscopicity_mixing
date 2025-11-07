      subroutine legndr( m, ab, plgndr )
!
!  Input
!	m	: maximum order of the Legendre polynomial
!	ab	: the variable
!
!  Output
!	plgndr	: the array containing Legendre polynomials of ab
!

        real	plgndr(0:0)

!
!	pk	: p_sub_k
!	pkm1	: p_sub_k-1
!	pkm2	: p_sub_k-2
!	mu	: the variable in double precision
!	dk	: k in double precision
!	dkm1	: (k-1) in double precision
!	dkm2	: (k-2) in double precision
!
      double precision pk, pkm1, pkm2, mu, dk, dkm1, d2km1

      mu = dble( ab )
!      write(*,*) mu
!
!	For k = 0
!

      k = 0
      pk = 1.0d0
      plgndr(k) = pk

!
!	For k = 1
!
      k = 1
      pkm1 = pk 
      pk = mu
      plgndr(k) = pk

!
!	For k greater than 1
!
      do 10 k = 2, m
        pkm2 = pkm1
        pkm1 = pk
        dk = dble( k )
        d2km1 = dble( 2*k-1 )
        dkm1 = dble( k - 1 )

!
!	  Use the recurrence relation of Legendre polynomials.
!
        pk = (d2km1*mu*pkm1 - dkm1*pkm2)/dk
        plgndr(k) = sngl( pk )
   10 continue

      return
      end
