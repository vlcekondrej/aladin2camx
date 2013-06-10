      subroutine clddiag(nz,ta,qv,pr,kbot,ktop)
        USE module_global_variables
      IMPLICIT NONE
!
!-----CLDDIAG determines convective cloud top and bottom using a technique 
!     following CMAQ per RADM.  Cloud base is determined from the lifting 
!     condensation level, and cloud top is determined by following the moist 
!     adiabat to neutral bouyancy, or to the minimum altitude at which 
!     RH<65% or P<600 mb.
!
!     Input:
!        ta   Layer temperature (K)
!        qv   Layer humidity (kg/kg)
!        pr   Layer pressure (Pa)
!
!     Output:
!        kbot  Layer index of cloud bottom (0 = no cloud diagnosed)
!        ktop  Layer index of cloud top (0 = no cloud diagnosed)
!
!      include 'param.inc'
      real :: lv,mh2o,mair,lvocp,mvoma
      integer,intent(in) :: nz
      real,intent(in) :: ta(nz),qv(nz),pr(nz)
      integer,intent(out) :: kbot,ktop
      real :: tsat(nz)

      real :: rh,t1,dtdp,x1,qsat,esat
      real :: tbase,pbar,dpv,tbar,tlcl,plcl
      real :: dz,delta,tdlps,tdsrc,evap,etheta
      real :: tp,pp,psrc,qsrc
      real :: qpv,theta,tsrc,ksrc,gamma,rdocp,rv
      real :: esrc,evap0,cp,rd,grav,qpert,tpert
      integer :: istab,k
!
      data tpert /1.5/     !K
      data qpert /1.5e-3/  !kg/kg
      data grav  /9.8/     !m/s2
      data rd    /287./    !J/K/kg
      data cp    /1004./   !J/K/kg
      data lv    /2.5e6/   !J/kg
      data mh2o  /18./     !g(h2o)/mol
      data mair  /28.8/    !g(air)/mol
      data evap0 /611./    !Pa
      data rv    /461./    !J/K/kg
!
      rdocp = rd/cp
      lvocp = lv/cp
      mvoma = mh2o/mair
      gamma = grav/cp
!
!-----Determine cloud source level by determining max equivalent potential
!     temperature between surface and 650 mb
!

      ksrc = 1
      tsrc = ta(1) + tpert
      qsrc = qv(1) + qpert
      psrc = pr(1)
      theta = tsrc*(1.e5/psrc)**rdocp
      esrc = theta*exp(lvocp*qsrc/tsrc)

      do k = 2,nz
        pp = pr(k)
        if (pp.lt.6.5e4) goto 10
        tp = ta(k) + tpert
        qpv = qv(k) + qpert
        theta = tp*(1.e5/pp)**rdocp
        etheta = theta*exp(lvocp*qpv/tp)
        if (etheta.gt.esrc) then
          ksrc = k
          tsrc = tp
          qsrc = qpv
          psrc = pp
          esrc = etheta
        endif
      enddo
!
!-----Compute lifting condensation level:
!     First, determine dew point lapse rate
!
 10   evap = qsrc*psrc/(mvoma + qsrc)
      tdsrc = 1./(1./273. - (rv/lv)*alog(evap/evap0))
      tdsrc = amin1(tdsrc,tsrc)
      tdlps = (grav*tdsrc*tdsrc)/(mvoma*lv*tsrc)
!
!-----Second, compute difference between dry adiabatic and dew point lapse
!     rate, height increment above source level to reach LCL, and pressure
!     at LCL
!
      delta = gamma - tdlps
      if (delta.le.0.) then
        dz = 0.
        plcl = psrc
        tlcl = tsrc
      else
        dz = (tsrc - tdsrc)/delta
        tlcl = tsrc - gamma*dz
        tbar = 0.5*(tsrc + tlcl)
        plcl = psrc*exp(-(grav/rd)*dz/tbar)
      endif
!
!-----Determine layer containing cloud base
!
      kbot = 0
      ktop = 0
      do k = 2,nz
        if (pr(k).le.plcl) then
          kbot = k
          goto 20
        endif
      enddo
 20   if (kbot.eq.0) return
!
!-----Determine cloud top by following moist adiabat up from the base. 
!

      istab = 0
      do k = kbot,nz
        dpv=pr(k-1) - pr(k)
        pbar = 0.5*(pr(k-1) + pr(k))
        if (k.eq.kbot) then
          dpv=plcl - pr(k)
          pbar = 0.5*(plcl + pr(k))
          tbase = tlcl
        endif

        tbar = tbase - 6.5e-4*dpv/2.
        esat = evap0*exp((lv/rv)*(1./273.15 - 1./tbar))
        qsat = mvoma/(pbar/esat - 1.)
        x1 = lv*qsat/(rd*tbar)
        dtdp = (rd*tbar)/(pbar*cp)*((1. + x1)&
     &         /(1. + (mvoma*lvocp/tbar)*x1))
        tsat(k) = tbase - dpv*dtdp
        tbase = tsat(k)

        if (istab.eq.0) then
          if (tsat(k).gt.ta(k)) then
            istab = 1
          endif
        else
          t1 = tsat(k) - 0.5*tpert
          if (t1.lt.ta(k)) then
            ktop = k - 1
            goto 30
          endif
        endif
      enddo
      ktop = nz - 1


!
!-----If ISTAB is still = 0, we have a "stable" cloud: find cloud 
!     top by 65% relative humidity criterion, but do not let cloud top go above 
!     600 mb.

 30   if (istab.eq.0) then
        do k = ktop+1,nz
          esat = evap0*exp((lv/rv)*(1./273. - 1./ta(k)))
          qsat = mvoma/(pr(k)/esat - 1.)
          rh = qv(k)/qsat
          if (pr(k).le.60000.0 .or. rh.lt.0.65) then
            ktop = k - 1
            goto 40
          endif
        enddo
      endif
 40   continue

      return
     end
