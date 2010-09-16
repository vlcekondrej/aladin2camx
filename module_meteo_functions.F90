MODULE MODULE_METEO_FUNCTIONS
 !--------------------------------------------------------------------------------
 ! all in SI 
 !
 ! meteorological functions:
 ! -------------------------
 ! AirDens ....... air density                    from pressure and temperatute
 ! Tvirt_tq ...... virtual temperature            from temperature and specific humidity
 ! Tvirt_tm ...... virtual temperature            from temperature and mixing ratio
 ! WVP_pq ........ Water Vapor Pressure           from pressure and spefic humidity
 ! WVPs_t ........ saturated water vapor pressute from temperature
 ! Tpot .......... potential temperature          from pressure and temperature
 ! TfromTpot ..... air temperature                from pressure and potential temperature
 ! COD_chimere ... integrated cloud optical depth
 !--------------------------------------------------------------------------------

 USE module_standard_types
 USE module_physical_constants, ONLY: R_DryAir, &
                                      Rw2Rd, Rd2Rw, &
                                      cp_DryAir               

 IMPLICIT NONE

 CONTAINS
 


 elemental function AirDens (press, temp) 
  !-------------------------------------------------------------------------------
  ! PURPOSE: Calculates air density for 1) dry air, if Temp is air temperature
  !                                     2) moist air, if Temp is virtual temp.
  !
  ! Input :  press   = pressure       [Pa]
  !          temp    = temperatur     [K]
  !
  ! Output:  AirDens = air density    [kg/m**3]
  !-------------------------------------------------------------------------------
  implicit none
  real (kind=sp)             :: AirDens
  real (kind=sp), intent(in) :: press, temp
  !-------------------------------------------------------------------------------  
    AirDens = press / ( R_DryAir * temp )
 end  function AirDens

 ! = * = * = * = * = * = * = * = * = * = * = * = * = * = * = * = * = * = * = * = * 

 elemental function Tvirt_tq(Temp,SpHum)
  !-------------------------------------------------------------------------------
  ! PURPOSE: Calculates virtual temperature using temperature T and 
  !          specific humidity Q==RHOv/(RHOv+RHOd)
  !
  ! REF: Pechala a Bednar 1991: 5.65 - 5.68
  !      p = (RHOd*Rd + RHOv*Rv)*T                    ! ideal gas eq.       
  !      p = RHO*Rd*T * (RHOd/RHO + Rv/Rd * RHOv/RHO) ! RHO = RHOd+RHOv 
  !      p = RHO*Rd*T * (1-Q + Rv/Rd * Q)             ! Q == specific humidity == RHOv/RHO
  !      p = RHO*Rd*T * (1 + (Rv/Rd-1)*Q)
  !
  ! Input :  Temp    = temperature        [K]
  !          SpHum   = specific humidity
  !
  ! Output:  AirDens = air density        [kg/m**3]
  !-------------------------------------------------------------------------------
  implicit none
  real(kind=sp)             :: Tvirt_tq
  real(kind=sp), intent(in) :: temp, SpHum
  !-------------------------------------------------------------------------------  
    Tvirt_tq = Temp * (1.+(Rw2Rd-1.)*SpHum)
  end function Tvirt_tq

 ! = * = * = * = * = * = * = * = * = * = * = * = * = * = * = * = * = * = * = * = * 

 elemental function Tvirt_tm (Temp, MixR)
  !-------------------------------------------------------------------------------
  ! PURPOSE: Calculation od virtual temperature from ait temperature and     
  !          mixind ratio
  !
  ! REF: Rogers / Yau (1989) , A Short Course in Cloud Physics. (p.17)
  !
  ! Input : temp     = air temperature          [K]
  !         MixR     = Mixing ratio==RHOv/RHOd  [kg/kg]
  !
  ! Output: Tvirt_tm = virtual temperature      [K]
  !-------------------------------------------------------------------------------
  implicit none
  real (kind=sp)             :: Tvirt_tm
  real (kind=sp), intent(in) :: Temp, MixR
  !-------------------------------------------------------------------------------
    Tvirt_tm = temp * (1.0_sp + MixR*Rw2Rd) / (1.0_sp + MixR)
 end  function Tvirt_tm

 ! = * = * = * = * = * = * = * = * = * = * = * = * = * = * = * = * = * = * = * = * 

 elemental function WVP_pq(press,SpHum)
  !-------------------------------------------------------------------------------
  ! PURPOSE: Water Vapor Pressure from air pressure and spefic humidity
  !
  ! REF: Pechala a Bednar 1991: 5.57
  !      Q/(1-Q)==RHOv/RHOd = Rd/Rv * e/(p-e)
  !
  ! Input :  press   = pressure              [Pa]
  !          SpHum   = specific humidity
  !
  ! Output:  WVP_pq  = Water Vapor Pressure  [Pa]
  !-------------------------------------------------------------------------------
  real(kind=sp)             :: WVP_pq
  real(kind=sp), intent(in) :: SpHum, press  
  !-------------------------------------------------------------------------------
    WVP_pq = SpHum*Press / (Rd2Rw+SpHum*(1-Rd2Rw)) 
 end function WVP_pq

 ! = * = * = * = * = * = * = * = * = * = * = * = * = * = * = * = * = * = * = * = * 

 elemental function WVPs_t (Temp)
  !-------------------------------------------------------------------------------
  ! PURPOSE: Calculation of saturated water vapor pressure above (flat?) water surface
  ! 
  ! REF: Aspirations-Psychrometer-Tafeln, hrsg.vom Deutschen Wetterdienst. 7., vollst.
  !      ueberarb. Aufl. Braunschweig, Vieweg 1998, 271 S. ISBN 3-528-38231.
  !
  ! Input  : Temp   = Temperature                     [K]
  !
  ! Output : WVPs_t = saturated water vapro pressure  [Pa]
  !-------------------------------------------------------------------------------
  implicit none
  real (kind=sp)             :: WVPs_t
  real (kind=sp), intent(in) :: temp

  real (kind=sp), parameter :: c11=610.78_sp, c12=17.08085_sp, c13=234.175_sp, &
                            &  c21=610.78_sp, c22=17.84362_sp, c23=245.425_sp
  real(kind=sp) ::  tc
  !-------------------------------------------------------------------------------
    tc = temp - 273.15_sp    ! Celsius
    if (tc > 0.0_sp) then
       WVPs_t = c11*exp(c12*tc/(c13+tc))
    else
       WVPs_t = c21*exp(c22*tc/(c23+tc))
    endif
 end function WVPs_t

 ! = * = * = * = * = * = * = * = * = * = * = * = * = * = * = * = * = * = * = * = * 

 elemental function Tpot (temp, press)
  !-------------------------------------------------------------------------------
  ! RUPPOSE: Calculates Potential Temperature from temperature and pressure
  !
  ! Input :  press  = pressure           [Pa]
  !          temp   = temperature        [K]
  !
  ! Output:  Tpot   = potential temperature  [K]
  !-------------------------------------------------------------------------------
  implicit none
  real (kind=sp)             :: Tpot
  real (kind=sp), intent(in) :: temp, press
  !-------------------------------------------------------------------------------
    Tpot = temp * (100000._sp/press)**(R_DryAir/cp_DryAir)
 end  function Tpot

 ! = * = * = * = * = * = * = * = * = * = * = * = * = * = * = * = * = * = * = * = * 

 elemental function TfromTpot (tpot, press)
  !-------------------------------------------------------------------------------
  ! RUPPOSE: Calculates temperature from Potential Temperature and pressure
  !
  ! Input : press     = pressure           [Pa]
  !         tpot      = temperature        [K]
  !
  ! Output: TfromTpor = air temperature    [K]
  !-------------------------------------------------------------------------------
  implicit none
  real (kind=sp)             :: TfromTpot
  real (kind=sp), intent(in) :: tpot, press
  !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    TfromTpot = tpot * (press/100000._sp)**R_DryAir/cp_dryAir
 end function TfromTpot

 ! = * = * = * = * = * = * = * = * = * = * = * = * = * = * = * = * = * = * = * = * 

END  module MODULE_METEO_FUNCTIONS
!
