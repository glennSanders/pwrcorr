#' Determine the Saturated Vapour Pressure of Water
#'
#' The formula used inside the equation is valid between 0 and 272 degrees Celcius and is from
#' the IAPWS standard \insertCite{wagner_iapws_2002}{pwrcorr}.
#'
#' @param temp Temperature in Kelvin, or alternatively a variable with temperature units from the \code{units} package.
#' @return Returns the saturated vapour pressure in Pa. If a temperature with units is entered then units are also returned in the output.
#' @references
#' \insertRef{wagner_iapws_2002}{pwrcorr}
#' @import units
#' @export
sat_vap_press <- function(temp) {
  units <- FALSE
  if(class(temp)=="units") {
    temp <- units::set_units(temp,"degK")
    units <- TRUE
  }
  temp <- as.numeric(temp)
  stopifnot(temp<646.15,temp>273.15) # 373/0degC
  vartheta <- 1 - temp/T_c
  p <- as.numeric(p_c * exp(T_c/temp *
                 a %*% t(matrix(unlist(
                   lapply(exponents,function(x,y) y^x, vartheta)),
                   nrow = length(vartheta)))))
  if(units) {
    p <- set_units(p,"Pa")
  }
  return(p)
}

#' Power Correction Reference
#'
#' The function allows for the definition of various reference conditions based on
#' the reference pressure, temperature, humidity, and friction. \code{NA} in humidity means that
#' the barrometer pressure is not reduced by teh saturated vapour pressure. \code{NA} in friction
#' means that friction losses are not considered
#'
#' @param ref_temp Temperature in Kelvin, or alternatively a variable with temperature units from the \code{units} package.
#' @param ref_press Pressure in Pascals, or alternatively a variable with pressure units from the \code{units} package.
#' @param humidity The Relative Humidity stipulated in the reference conditions.
#' @param friction The friction losses used in the the reference conditions
#' @return A function that outputs a correction factor
#' @import units
#' @export
pwr_corr <- function(ref_temp,ref_press,humidity=NA,friction=NA) {
  if(class(ref_temp)=="units") {
    ref_temp <- units::set_units(ref_temp,"degK")
  }
  if(class(ref_press)=="units") {
    ref_press <- units::set_units(ref_press,"Pa")
  }
  ref_temp <- as.numeric(ref_temp)
  ref_press <- as.numeric(ref_press)

  function(obs_temp,obs_press,obs_hum=0,obs_fric=NA) {
    if(class(obs_temp)=="units") {
      obs_temp <- units::set_units(obs_temp,"degK")
    }
    if(class(obs_press)=="units") {
      obs_press <- units::set_units(obs_press,"Pa")
    }
    obs_temp <- as.numeric(obs_temp)
    obs_press <- as.numeric(obs_press)
    if(!is.na(humidity)){
      obs_press <- obs_press - obs_hum*sat_vap_press(obs_temp)
    }
    cf <- (ref_press/obs_press)*(obs_temp/ref_temp)^0.5
    if (!is.na(friction)) {
      if(is.na(obs_fric)) {
        friction_cor <- (1-friction)/friction
      } else {
        friction_cor <- (1-obs_fric)/obs_fric
      }
      cf <- (1+friction_cor)*cf-friction_cor
    }
    return(cf)
  }
}

#' Reference Standards
#'
#' The various correction factors found in the literature
#' @param obs_temp The observed temperature in Degrees Kelvin or a temperature \code{units} object
#' @param obs_press The observed pressure in Pascals or a pressure \code{units} object
#' @param obs_hum The observed humidity as a percentage between 0 and 1
#' @param obs_fric The measured efficiency of the engine if available as a percenatge between 0 and 1
#' @import units
#' @name ref_std
NULL

#' @describeIn ref_std SAE J607 Standard without any consideration of humidity
j607 <- pwr_corr(ref_temp = units::set_units(60,"degF"),
                 ref_press = units::set_units(29.92,"inHg"))

#' @describeIn ref_std SAE J1349(1990) standard with efficiency and corrected pressure
j1349_1990 <- pwr_corr(ref_temp = units::set_units(77,"degF"),
                 ref_press = units::set_units(990,"hPa"),
                 friction = .8475)

#' @describeIn ref_std SAE J1349(2004) standard with revised efficiency
j1349_2004 <- pwr_corr(ref_temp = units::set_units(77,"degF"),
                       ref_press = units::set_units(990,"hPa"),
                       friction = 0.85)

#' @describeIn ref_std Motorsports Standard Atmosphere
msa <- pwr_corr(ref_temp = units::set_units(60,"degF"),
                       ref_press = units::set_units(29.92,"inHg"))

#' @describeIn ref_std STD Air Correction
std <- pwr_corr(ref_temp = units::set_units(68,"degF"),
                ref_press = units::set_units(29.92,"inHg"))

