% Generated by roxygen2 (4.0.1): do not edit by hand
\name{install_packages}
\alias{install_packages}
\title{编译安装包}
\usage{
install_packages(pkg, oxygenize = FALSE)
}
\arguments{
\item{pkg}{要编译的包路径}
}
\description{
当帮助中含有中文时，`R:tools` 包所编译的帮助文件存在中文乱码现象。
通过修改生成 `00Index.html` 的函数 `.writePkgIndices` 修正此问题。
}
\examples{
install_packages('e:/R/packages/pkgutils/', TRUE)
}
\seealso{
Other package: \code{\link{findRefs}};
  \code{\link{md2context}}; \code{\link{md_roclet}};
  \code{\link{pkgBackup}}; \code{\link{pkgLoad}};
  \code{\link{pkgRestore}}; \code{\link{roxygen_fix}};
  \code{\link{viewCode}}
}

