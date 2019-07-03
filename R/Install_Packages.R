if(FALSE) {


# Manually copy from old version of R:
# etc\Rprofile.site # Check to see if the new default Rprofile.site has changed.

install.packages('devtools') # Need CRAN devtools package to install the one that works below!


install.packages(c("acepack","akima", "bootstrap", "boot","chron", "debug", "Design", "foreign", "gdata",
	    "gmodels","gplots", "gregmisc","gstat","gtools","Hmisc","KernSmooth","mvbutils","outliers",
	    "panel", "RColorBrewer", "rpart", "RODBC", "sp", "statmod", "tweedie", "MCMCpack", "mcmc",
	    "ade4", "car", "gam", "stashR", "lme4", "MCMCglmm", "ggplot2", "maps", "mapproj","gamm4",
        "rggobi", "latticeExtra", "PBSmodelling", "PBSadmb", "glmmBUGS", "BRugs", "R2WinBUGS", "gamlss", "gpclib", "sqldf", "googleVis", 
        "gmp", "Rmpfr", "codetools", "snow", "rgeos", "mgcv", "numDeriv", "bbmle", "mvtnorm", "roxygen2", "RDCOMClient", "R2wd", "actuar",
        "Rcpp", "RcppArmadillo", "compare", "trust", 'XML', 'pbapply', 'rgl', 'RandomFields', 'TMB', 'data.table', 'RCurl', 'dplyr',
        "mapdata", "rgdal", "SDMTools", "polyclip", "plotrix", "raster", "TeachingDemos", "glmTMB", "pso", "truncnorm" ))

# No longer used packages?: "SASmixed"
         
install.packages("INLA", repos="https://inla.r-inla-download.org/R/stable")

# devtools::install_github("glmmTMB/glmmTMB",subdir="glmmTMB")
# devtools::install_github("glmmTMB/glmmTMB")
# install.packages("glmmTMB", repos = "http://glmmtmb.github.io/glmmTMB/repos/")


# Old package 'debug'
install.packages("mvbutils")
install.packages("https://cran.r-project.org/src/contrib/Archive/debug/debug_1.3.1.tar.gz")
library(debug)
mtrace('function to trac')


# RStan
dotR <- file.path(Sys.getenv("HOME"), ".R")
if (!file.exists(dotR)) dir.create(dotR)
M <- file.path(dotR, "Makevars.win")
if (!file.exists(M)) file.create(M)
cat("\nCXX14FLAGS=-O3 -march=native",
    "CXX14 = g++ -m$(WIN) -std=c++1y",
    "CXX11FLAGS=-O3 -march=native",
    file = M, sep = "\n", append = FALSE)

# install.packages('rstan', repos = 'https://cloud.r-project.org/', dependencies = TRUE)
install.packages("rstan", type = "source")



update.packages(ask = F)  # ask = F will re-install the devtools that doesn't work on my work PC, so moved the 'sha-checking' version to below

# *** devtool install on work PC only ***		 
# For this issue: install_github does not skip package installation with same SHA in case "ref" is defined
# Found here: 	 https://github.com/r-lib/devtools/issues/1624
devtools::install_github("dracodoc/devtools", ref = "sha-checking")	 

devtools::install_github("John-R-Wallace/JRWToolBox")

require(JRWToolBox)

lib("John-R-Wallace/Imap")
lib("r4ss/r4ss")

# TMB CRAN version installed above - may be older than on GitHub
lib("james-thorson/VAST", quiet = F)  # Try first

# Failed - asking for these packages:
install.packages(c('deldir', 'PBSmapping', 'RANN', 'shape', 'mixtools', 'pander', 'formatR' ))

# Try again
lib("james-thorson/VAST", quiet = F)  # Worked with an auto-install of FIshStatsUtils


lib("kaskr/adcomp/TMB", quiet = F) # Not intalled with VAST

lib("kaskr/TMB_contrib_R/TMBhelper", quiet = F) # ?? Old install not sure
lib("kaskr/TMB_contrib_R/TMBdebug", quiet = F)  # Not installed with VAST - TMBdebug needs to be earlier in the search path then TMB
lib("kaskr/TMB_contrib_R/TMBphase", quiet = F)  # Not installed with VAST

# devtools::install_github("gavinfay/TMBphase")
lib("james-thorson/utilities",  Package.Name = 'ThorsonUtilities', quiet = F) # ?? Old install not sure
lib("nwfsc-assess/geostatistical_delta-GLMM", Package.Name = 'SpatialDeltaGLMM', quiet = F)  # Fixed - see above - With devtools() force = FALSE broken: ThorsonUtilities, TMBhelper, and SpatialDFA were all reinstalled or in the case of 'SpatialFDA' installed 


# lib("james-thorson/MIST")


# -------------------- OLD -----------------------------------------


devtools::install_github("r4ss/r4ss")

devtools::install_github("kaskr/TMB_contrib_R/TMBhelper")
devtools::install_github("kaskr/TMB_contrib_R/TMBdebug")
devtools::install_github("kaskr/TMB_contrib_R/TMBphase")
# devtools::install_github("gavinfay/TMBphase")
devtools::install_github("james-thorson/VAST")
devtools::install_github("nwfsc-assess/geostatistical_delta-GLMM")
devtools::install_github("james-thorson/utilities") # Should be loaded with VAST

devtools::install_github("james-thorson/MIST")




# To install TMB first edit DOS path to remove paths added by 'install_windows.R' or edit that file
# In 'install_windows.R' edit the reference of the C drive to the W drive
# setwd("W:/R/ADCOMP/adcomp-master")
setwd("W:/R/adcomp-master")
source('install_windows.R')


# ================== Update DOS path in Windows =========================

# PATH=W:\Rtools\bin;W:\Rtools\gcc-4.6.3\bin;W:\Bin;W:\R\R-3.0.3\bin........

# ================== Update DOS path in Sublime 2 =========================

# gmp: (Gnu) Multiple Precision Arithmetic

# The R package Rmpfr allows to use arbitrarily precise numbers instead of R’s double precision numbers in many R computations and functions.
# http://cran.r-project.org/web/packages/Rmpfr/vignettes/Rmpfr-pkg.pdf


# Other packages I may want:
# install.packages("rgl")
# install.packages("RWinEdt")

# ============ Windows path for clang++ ========

"W:/Rtools/bin;W:/Bin;W:/R/R-3.1.1/bin;W:/MinGW/bin;W:/MinGW/msys/1.0/bin;W:/ADMB/bin;W:/ADMB/utilities;C:/Program Files/Java/jre8/bin;c:/oracle_instant_client_11_2;C:/Windows/system32;C:/Windows;C:/Windows/System32/Wbem;C:/Windows/System32/WindowsPowerShell/v1.0/;C:/Program Files (x86)/Common Files/Roxio Shared/DLLShared/;C:/Program Files (x86)/Common Files/Roxio Shared/OEM/DLLShared/;C:/Program Files (x86)/Common Files/Roxio Shared/OEM/DLLShared/;C:/Program Files (x86)/Common Files/Roxio Shared/OEM/12.0/DLLShared/;C:/Program Files (x86)/Roxio/OEM/AudioCore/;C:/Program Files (x86)/Bitvise Tunnelier;C:/Program Files (x86)/Tumbleweed/Desktop Validator/;C:/Program Files/ActivIdentity/ActivClient/;C:/Program Files (x86)/ActivIdentity/ActivClient/;W:/Win_apps/doxygen/bin;W:/Win_apps/Mscgen;W:/Win_apps/Git/cmd;W:/Win_apps/Git/bin;W:/Win_apps/MiKTeX 2.9/miktex/bin/;C:/Program Files (x86)/QuickTime/QTSystem/;W:/LLVM/bin"


}
