
# define variables here ---------------------------------------------------

pkg = 'e:/Huashan/Stat/R/packages/huashan/'
pkg = 'e:/Huashan/Stat/R/packages/gdtel/'
pkg = 'e:/Huashan/Stat/R/packages/pkgutils/'

COxygenize = F
CMakeMarkdown = FALSE
CIncrementBvdNum = TRUE

# Compile and install package ---------------------------------------------

#library(pkgutils)
#library(stringr)
#debug(pkgutils::install_packages)
pkgutils::install_packages(pkg, oxygenize=COxygenize, incrementBvdNum = CIncrementBvdNum)
if (CMakeMarkdown) pkgutils::md_roclet(pkg)

