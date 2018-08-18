# Ref IAPWS Eqn 2.5
T_c <- 647.096 # K
p_c <- 22.064e6 # Pa
a <- c(-7.85951783,
       1.84408259,
       -11.7866497,
       22.6807411,
       -15.9618719,
       1.80122502)
exponents <- c(1,1.5,3,3.5,4,7.5)

devtools::use_data(T_c,p_c,a,exponents,	internal	=	TRUE)
