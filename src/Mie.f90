
!***************************************************************************
! Using the Mie's theory, this subroutine compute the scattering and 
! extinction efficiency factors (usually written Qsca and Qext) and it also 
! compute the scattering intensity efficiency
      subroutine EXSCPHASE(X,nr,ni,Qext,Qsca,p11,cgaus,nang)
      integer , parameter:: nser=10000
      double precision Ren,Imn,X,Up,XnumRDnY,XnumIDnY
      double precision XdenDnY,coxj,Qsca,Qext,xJonH,XdenGNX
      double precision Xnum1An,Xnum2An,XdenAn,Xden1An,Xden2An,RAnb,IAnb
      double precision Xnum1Bn,Xnum2Bn,XdenBn,Xden1Bn,Xden2Bn,RBnb,IBnb
      double precision xmud,xpond,RS1,RS2,IS1,IS2,co_n,test
      double precision xj(0:nser),xy(-1:nser),Rn(0:nser)
      double precision IDnY(0:nser),RDnX(0:nser),RDnY(0:nser)
      double precision IGnX(0:nser),RGnX(0:nser)
      double precision RAn(0:nser),IAn(0:nser),RBn(0:nser),IBn(0:nser)
      double precision TAUn(0:nser),PIn(0:nser),p11(nang)
      real nr,ni,phasel,cgaus(nang)
      integer N,Np,mu,mub,mu1,mu2,k,nbmu,j

     

       nbmu = nang
      Ren=nr/(nr*nr+ni*ni)
      Imn=ni/(nr*nr+ni*ni)
      

! ---Identification of the greater order of computation (=mu)
!    as defined by F.J. Corbato, J. Assoc. Computing Machinery, 1959,
!    6, 366-375
	
      N=int(0.5D+00*(-1.D+00+dsqrt(1.D+00+4.D+00*X*X)))+1
	

      if (N.eq.1)N=2

      mu2=1000000
      Np=N
      Up=2.D+00*X/(2.D+00*Np+1.D+00)
      mu1=int(Np+30.*(0.10+0.35*Up*(2-Up*Up)/2./(1-Up)))
      Np=int(X-0.5D+00+dsqrt(30.*0.35*X))
      if (Np.gt.N)then
       Up=2.D+00*X/(2.D+00*Np+1.D+00)
       mu2=int(Np+30.*(0.10+0.35*Up*(2-Up*Up)/2./(1-Up)))
      endif
	
      mu=min0(mu1,mu2)
	

! --- Identification of the transition line. Below this line the Bessel 
!     function j behaves as oscillating functions. Above the behavior 
!     becomes monotonic. We start at a order greater than this transition 
!     line (order max=mu) because a downward recursion is called for.
      Rn(mu)=0.D+00
      k=mu+1
 149  continue
      k=k-1
      xj(k)=0.D+00
      Rn(k-1)=X/(2.D+00*k+1.D+00-X*Rn(k))
      if (k.eq.2)then
	  mub=mu
	  xj(mub+1)=0.D+00
	  xj(mub)=1.D+00
	  goto 150
      endif
      if (Rn(k-1).gt.1.D+00)then
	  mub=k-1
	  xj(mub+1)=Rn(mub)
	  xj(mub)=1.D+00
	  goto 150
      endif
      goto 149
 150  continue

      do k=mub,1,-1
	xj(k-1)=(2.D+00*k+1.D+00)*xj(k)/X-xj(k+1)
      enddo
      coxj=(xj(0)-X*xj(1))*dcos(X)+X*xj(0)*sin(X)

! --- Computation Dn(alpha) and Dn(alpha*m) (cf MIE's theory) 
!     downward recursion    - real and imaginary parts
      RDnY(mu)=0.D+00
      IDnY(mu)=0.D+00
      RDnX(mu)=0.D+00
      do k=mu,1,-1
	 RDnX(k-1)=k/X-1.D+00/(RDnX(k)+k/X)
	 XnumRDnY=RDnY(k)+Ren*k/X
	 XnumIDnY=IDnY(k)+Imn*k/X
	 XdenDnY=XnumRDnY*XnumRDnY+XnumIDnY*XnumIDnY
	 RDnY(k-1)=k*Ren/X-XnumRDnY/XdenDnY
	 IDnY(k-1)=k*Imn/X+XnumIDnY/XdenDnY

      enddo

! --- Initialization of the upward recursions
      xy(-1)=dsin(x)/x
      xy(0)=-dcos(x)/x
      RGnX(0)=0.D+00
      IGnX(0)=-1.D+00
      Qsca=0.D+00
      Qext=0.D+00
      do k=1,mu
	 if (k.le.mub)then
	   xj(k)=xj(k)/coxj
	 else
	   xj(k)=Rn(k-1)*xj(k-1)
	 endif

! --- Computation of bessel's function y(alpha)
	 xy(k)=(2.D+00*k-1.D+00)*xy(k-1)/X-xy(k-2)
	 xJonH=xj(k)/(xj(k)*xj(k)+xy(k)*xy(k))

! --- Computation of Gn(alpha), Real and Imaginary part
         XdenGNX=(RGnX(k-1)-k/X)**2.D+00+IGnX(k-1)*IGnX(k-1)
	 RGnX(k)=(k/X-RGnX(k-1))/XdenGNX-k/X
	 IGnX(k)=IGnX(k-1)/XdenGNX

! --- Computation of An(alpha) and Bn(alpha), Real and Imaginary part
	 Xnum1An=RDnY(k)-nr*RDnX(k)
	 Xnum2An=IDnY(k)+ni*RDnX(k)
	 Xden1An=RDnY(k)-nr*RGnX(k)-ni*IGnX(k)
	 Xden2An=IDnY(k)+ni*RGnX(k)-nr*IGnX(k)
	 XdenAn=Xden1An*Xden1An+Xden2An*Xden2An
	 RAnb=(Xnum1An*Xden1An+Xnum2An*Xden2An)/XdenAn
	 IAnb=(-Xnum1An*Xden2An+Xnum2An*Xden1An)/XdenAn
	 RAn(k)=xJonH*(xj(k)*RAnb-xy(k)*IAnb)
	 IAn(k)=xJonH*(xy(k)*RAnb+xj(k)*IAnb)

	 Xnum1Bn=nr*RDnY(k)+ni*IDnY(k)-RDnX(k)
	 Xnum2Bn=nr*IDnY(k)-ni*RDnY(k)
	 Xden1Bn=nr*RDnY(k)+ni*IDnY(k)-RGnX(k)
	 Xden2Bn=nr*IDnY(k)-ni*RDnY(k)-IGnX(k)
	 XdenBn=Xden1Bn*Xden1Bn+Xden2Bn*Xden2Bn
	 RBnb=(Xnum1Bn*Xden1Bn+Xnum2Bn*Xden2Bn)/XdenBn
	 IBnb=(-Xnum1Bn*Xden2Bn+Xnum2Bn*Xden1Bn)/XdenBn
	 RBn(k)=xJonH*(xj(k)*RBnb-xy(k)*IBnb)
	 IBn(k)=xJonH*(xy(k)*RBnb+xj(k)*IBnb)

! ---Criterion on the recursion formulas as defined by D. Deirmendjian 
!    et al., J. Opt. Soc. Am., 1961, 51, 6, 620-633
 	 test=(RAn(k)**2.+IAn(k)**2.+RBn(k)**2.+IBn(k)**2.)/k
 	 if (test.lt.1.0D-14)then
           mu=k
           goto 400
         endif
! --- Computation of the scattering and extinction efficiency factor
         xpond=2.D+00/X/X*(2.D+00*k+1)
         Qsca=Qsca+xpond*(RAn(k)**2.+IAn(k)**2.+RBn(k)**2.+IBn(k)**2.)
         Qext=Qext+xpond*(RAn(k)+RBn(k))

      enddo
 400  continue

! --- Computation of the amplitude functions S1 and S2 (cf MIE's theory)
!     defined by PIn, TAUn, An and Bn with PIn and TAUn related to the 
!     Legendre polynomials.
      do j=1,nbmu
	 xmud=cgaus(j)
	 RS1=0.D+00
	 RS2=0.D+00
	 IS1=0.D+00
	 IS2=0.D+00
	 PIn(0)=0.D+00
	 PIn(1)=1.D+00
	 TAUn(1)=xmud
	 do k=1,mu
          co_n=(2.D+00*k+1.D+00)/k/(k+1.D+00)
	  RS1=RS1+co_n*(RAn(k)*PIn(k)+RBn(k)*TAUn(k))
	  RS2=RS2+co_n*(RAn(k)*TAUn(k)+RBn(k)*PIn(k))
	  IS1=IS1+co_n*(IAn(k)*PIn(k)+IBn(k)*TAUn(k))
	  IS2=IS2+co_n*(IAn(k)*TAUn(k)+IBn(k)*PIn(k))
          PIn(k+1)=((2.D+00*k+1)*xmud*PIn(k)-(k+1.D+00)*PIn(k-1))/k
          TAUn(k+1)=(k+1.D+00)*xmud*PIn(k+1)-(k+2.D+00)*PIn(k)
         enddo
! --- Computation of the scattering intensity efficiency
         p11(j)=2.D+00*(RS1*RS1+IS1*IS1+RS2*RS2+IS2*IS2)/X/X
      enddo
      return
      end
