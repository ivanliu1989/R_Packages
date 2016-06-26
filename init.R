library(installr)
updateR()


install.packages(c("devtools", "roxygen2", "testthat", "knitr"))

install.packages("rstudioapi")
rstudioapi::isAvailable("0.99.149")

devtools::install_github("hadley/devtools")

library(devtools)
has_devel()

library(roxygen2)
library(testthat)
devtools::session_info()
