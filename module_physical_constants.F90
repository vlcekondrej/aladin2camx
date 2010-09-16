MODULE MODULE_PHYSICAL_CONSTANTS
 USE module_standard_types
 IMPLICIT NONE

 ! all in SI !!!

 ! primary constants
 REAL(KIND=sp), PARAMETER :: MR_DryAir = 0.0289644_sp     ! molar mass of dry air             [kg/mol]  ALADIN consistent
 REAL(KIND=sp), PARAMETER :: MR_H2O    = 0.0180153_sp     ! molar mass of waternt             [kg/mol]  ALADIN consistent
 REAL(KIND=sp), PARAMETER :: T_0DegC   = 273.15_sp        ! zero degree of Celsius in Kelvin  [K]      
 REAL(KIND=sp), PARAMETER :: Boltzmann = 1.380658e-23     ! Boltzmann's constant              [J/K]     ALADIN consistent
 REAL(KIND=sp), PARAMETER :: Avogadro  = 6.0221367e23     ! Avogadro's number                 [1/mol]   ALADIN consistent
 REAL(KIND=sp), PARAMETER :: grav      = 9.80665_sp       ! gravitational acceleration        [m/s**2]  ALADIN consistent

 ! derived constants 
 REAL(KIND=sp), PARAMETER :: R_uni     = Boltzmann*Avogadro     ! universal gas constant                              [J/(mol*K)]  ALADIN consistent
 REAL(KIND=sp), PARAMETER :: cp_DryAir = 3.5_sp*R_uni/MR_DryAir ! specific heat of dry air when pressure is constant  [J/(kg*K)]   ALADIN consistent
 REAL(KIND=sp), PARAMETER :: cv_DryAir = 2.5_sp*R_uni/MR_DryAir ! specific heat of dry air when volume is constant    [J/(kg*K)]   ALADIN consistent
 REAL(KIND=sp), PARAMETER :: R_DryAir  = R_uni/MR_DryAir        ! gas constant for dry air                            [J/(kg*K)]   ALADIN consistent 
 REAL(KIND=sp), PARAMETER :: R_H2O     = R_uni/MR_H2O           ! gas constant for water vapor                        [J/(kg*K)]   ALADIN consistent
 REAL(KIND=sp), PARAMETER :: Rd2Rw     = MR_H2O/MR_DryAir       ! = R_DryAir/R_H2O              
 REAL(KIND=sp), PARAMETER :: Rw2Rd     = MR_DryAir/MR_H2O       ! = R_H2O/R_DryAir  

END MODULE MODULE_PHYSICAL_CONSTANTS
