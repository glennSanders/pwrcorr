% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/correction.R
\name{ref_std}
\alias{ref_std}
\alias{j607}
\alias{j1349_1990}
\alias{j1349_2004}
\alias{msa}
\alias{std}
\title{Reference Standards}
\usage{
j607(obs_temp, obs_press, obs_hum = 0, obs_fric = NA)

j1349_1990(obs_temp, obs_press, obs_hum = 0, obs_fric = NA)

j1349_2004(obs_temp, obs_press, obs_hum = 0, obs_fric = NA)

msa(obs_temp, obs_press, obs_hum = 0, obs_fric = NA)

std(obs_temp, obs_press, obs_hum = 0, obs_fric = NA)
}
\arguments{
\item{obs_temp}{The observed temperature in Degrees Kelvin or a temperature \code{units} object}

\item{obs_press}{The observed pressure in Pascals or a pressure \code{units} object}

\item{obs_hum}{The observed humidity as a percentage between 0 and 1}

\item{obs_fric}{The measured efficiency of the engine if available as a percenatge between 0 and 1}
}
\description{
The various correction factors found in the literature
}
\section{Functions}{
\itemize{
\item \code{j607}: SAE J607 Standard without any consideration of humidity

\item \code{j1349_1990}: SAE J1349(1990) standard with efficiency and corrected pressure

\item \code{j1349_2004}: SAE J1349(2004) standard with revised efficiency

\item \code{msa}: Motorsports Standard Atmosphere

\item \code{std}: STD Air Correction
}}
