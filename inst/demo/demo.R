
# define variables here ---------------------------------------------------

pkg = 'e:/Huashan/Stat/R/packages/huashan/'
pkg = 'e:/Huashan/Stat/R/packages/gdtel/'
pkg = 'e:/Huashan/Stat/R/packages/pkgutils/'

COxygenize = T
CMakeMarkdown = FALSE
CIncrementBvdNum = TRUE

# Compile and install package ---------------------------------------------

#library(pkgutils)
#library(stringr)

pkgutils::install_packages(pkg, oxygenize=COxygenize, incrementBvdNum = CIncrementBvdNum)

if (CMakeMarkdown) {
  fn = pkgutils::md_roclet(pkg)
  pkgutils::md2context(fn, cmdCxtInit = 'setuptex2.bat')
}

# generte rd only
do_roxygenize<-function(pkg){
  oldloc<-Sys.getlocale('LC_CTYPE')
  # 如果 LC_CTYPE 设置不对，则后续其他操作写入文件时，会破坏 R 文本的自动编码转换  
  on.exit(Sys.setlocale('LC_CTYPE', 'locale' = oldloc))
  Sys.setlocale('LC_CTYPE', locale="C")
  roxygen2::roxygenize(pkg, roclets = c('rd'))
}

do_roxygenize(pkg)
