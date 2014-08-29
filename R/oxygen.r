# ==============================================================================
# Backup installed R packages for version upgrade
#
# Author: Chen, Huashan, 2005-7-22
# ==============================================================================

#' 备份已安装包名
#' 
#' 将已安装的包名备份并保存到当前路径下的 `Rpkgs.RData` 文件。
#' @export
#' @family package
pkgBackup<-function(){
  # get installed packages
  pkgs<-attr(installed.packages(), 'dimnames')[[1]]
  #write.csv(pkgs, 'd:/Rpkgs.txt')
  save(pkgs, file = 'Rpkgs.RData')
}

#' 根据已备份的包名称，安装包
#' 
#' @export
#' @family package
pkgRestore<-function() {
  # read from backup
  #pkgs<-read.csv('d:/Rpkgs.txt')
  load('Rpkgs.RData')
  # paste(pkgs, collapse=",")
  
  for (i in 1:length(pkgs)) {
    if (file.access(paste0(.Library, '/', pkgs[i]), mode=0) != 0) {
      install.packages(paste(pkgs[i]))
    }
  }
}

#' 载入或安装包
#' 
#' @export
#' @family package
pkgLoad <- function(pkg){
  if(!require(pkg, character.only=TRUE)){
    install.packages(pkg)
    library(pkg, character.only=TRUE)
  }
  return(TRUE)
}

#' find reference of function within a package
#' 
#' 在指定包中查找调用 fn 的函数
#' 
#' @export
#' @family package
#' @examples
#' findRefs('knitr', 'auto_out_name')
findRefs <- function(pkg, fn) {
  ns <- getNamespace(pkg)
  found <- vapply(ls(ns, all.names=TRUE), function(n) {
    f <- get(n, ns)
    is.function(f) && fn %in% all.names(body(f))
  }, logical(1))
  
  names(found[found])
}

#' View code of specified package
#' 
#' @export
#' @family package
viewCode<-function(fun, package){
  funfoo<-get(fun, getNamespace(package))
  print(funfoo)
}

#' 修正 `Rd` 文件的中文乱码
#'
#' 默认 \code{roxygen2} 生成的文件为 ASCII 编码。该函数将 \code{Rd}  转换为 \code{UTF-8}，
#' 并在首行添加 \code{\\encoding}。
#'
#' \code{roxygen2::roxygenize} 写入的 `rd` 文件编码取决于 `R` 文件的编码。若 `R` 文件为 `UTF-8` 编码，则
#' `rd` 文件也为 `UTF-8` 编码。此时，无需再做编码转换，在 `DESCRIPTION` 文件中添加 `Encoding` 
#' 字段或直接为 `rd` 文件添加 `\\encoding{}` 即可。对于这种情形，无需调用本函数。
#'
#' \code{roxygen2} 生成 `R` help 支持中文说明
#' 
#' + 方式一：设置 `R` 文件及 `DESCRIPTION` 为 \code{utf-8} 编码。
#' 
#' + 方式二：
#' 
#'     - \code{rd} 文件头添加 \code{\\encoding{utf-8}}
#'     
#'     - \code{rd} 文件编码为 \code{utf-8}，无 BOM
#'     
#'     - \code{title} 的中文在 R help index 中仍为乱码，通过替换 \code{tools::.writePkgIndices()} 解决。
#'     
#'     - 若 `DESCRIPTION` 文件中设定了 \code{Encoding:utf-8}，则 \code{rd} 文件无需添加 \code{\\encoding{utf-8}}
#' @param pkgdir 需要修正编码的包路径，或 \code{package_path/man/}。
#' @export
#' @family package
roxygen_fix<-function(pkgdir){

  encToInt <- function(x, encoding = localeToCharset()){
    utf8ToInt(iconv(x, encoding, "UTF-8"))
  }
  
  nCjkChar<-function(s, ...) {
    if (!inherits(s, 'character')) {
      warning('parameter must be characters')
      return(FALSE)
    }
    # Convert character to integer vector
    # Optional encoding specifies encoding of x, defaults to current locale
    #encToInt <- function(x, encoding=localeToCharset()){
    #  utf8ToInt(iconv(x, encoding, "UTF-8"))
    #}
    x<-lapply(s, encToInt, ...)
    unlist(lapply(x, FUN=function(x)
      sum(((x >= 12288) & (x <= 12329)) |
            ((x >= 12352) & (x <= 12543)) |
            ((x >= 13312) & (x <= 19903)) |
            ((x >= 19968) & (x <= 40959)) |
            ((x >= 44032) & (x <= 55295)) |
            ((x >= 63744) & (x <= 64255)) |
            ((x >= 194560) & (x <= 195101)))
    ))
  }
  
  if (!grepl('/man', pkgdir)) {
    pkgdir = paste0(pkgdir, 'man/')
  }
  rdfiles=list.files(pkgdir, '*.Rd$', full.names=T)
  if (length(rdfiles) == 0) stop('no Rd files found')
  
  fileHasCjk<-function(fn){
    #require(huashan)
    f = paste0(readLines(fn), collapse='\n')
    ret = nCjkChar(f) > 0
    ifelse(is.na(ret), FALSE, ret)
  }
  
  filter=unlist(lapply(rdfiles, fileHasCjk))
  
  file2utf8<-function(fn){
    #if (is.na(fn)) return(0)
    f = paste0('\\encoding{utf-8}\n', paste0(readLines(fn), collapse='\n'))
    
    con=file(fn, 'w', encoding='UTF-8')
    cat(f, file=con)
    #cat(iconv(f, from='gb2312', to='UTF-8'), file=con)
    close(con)
    return(paste(fn, 'fixed.'))
  }
  
  unlist(lapply(rdfiles[filter], file2utf8))
}

#' alias for md_roclet
#'
#' @seealso md_roclet
oxygenize_markdown<-function(pkgdir){
  md_roclet(pkgdir)
}

#' oxygenize `markdown` 格式文档
#' 
#' 根据 `R` 脚本中的 `oxygen` 语法生成 `markdown` 格式说明文档。
#' 代码重复利用自 `roxygen2` 包。
#' 
#' 返回值为生成的 `markdown` 文件名
#' @param pkgdir 需要生成文档的包路径
#' @export
#' @family package
#' @examples
#' fn = md_roclet(base_path)
#' md2context(fn)
md_roclet<-function(pkgdir){
  
  pkg_get_desc<-function(path){
    desc = roxygen2:::read.description(paste0(path, 'DESCRIPTION'))
    return(list(md=str_c('% ', desc$Title, '\n', '% ', desc$Author, '\n',
                         '% ', desc$Date, '\n\nVersion: ', desc$Version, '\n\n',
                         desc$Description, '\n\n'),
                encoding=desc$Encoding))
    # Version Description Maintainer
  }
  
  # modified from roxygen2::roc_output()
  roc_output_my <- function(roclet, results, base_path, options = list()) {
    #man <- normalizePath(file.path(base_path, "man"))
    contents <- vapply(results, format, wrap = options$wrap, 
                       FUN.VALUE = character(1))
    contents
  }  
  # return empty string so that not to output alias tag
  xformat.alias_tag <- function(x, ...) {
    #x$values <- str_replace_all(x$values, fixed("%"), "\\%")
    #format_rd(x, ...)
    return("")
  }
  
  xformat.usage_tag <- function(x, ...) {
    xrd_tag(x$tag, str_c('```\n', roxygen2:::build_rd(x$values, collapse = "\n\n"), '\n```'), 
            space = F)
  }
  
  xformat.examples_tag <- function(x, ...) {
    values <- str_c(x$values, collapse = "\n")
    xrd_tag(x$tag, str_c('```\n', values, '\n```'), space = TRUE)
  }
  
  xrd_tag<-function (tag, ..., space = FALSE) {
    require(R.utils)
    if (space) {
      values <- str_c(str_c(..., collapse = "\n"), "\n")
    }
    else {
      values <- str_trim(c(...))
    }
    values = gsub('\\\\code\\{([^\\s]*)\\}', '\\1', values, perl=T)
    values = gsub('\\\\link\\{([^\\s]*)\\}', '[\\1]', values, perl=T)
    # replace illegal context characters like: \ { }
    values = gsub('([^\\\\])\\{', '\\1\\\\{', values, perl = T)
    values = gsub('([^\\\\])\\}', '\\1\\\\}', values, perl = T)
    
    if (tag == '') {
      str_c(values, "\n\n", collapse = "")}
    else {
      str_c("## ", capitalize(tag), '\n', str_c("\n", values, "\n", collapse = ""), 
            "\n")}
  }
  
  # 参数说明
  # 在 roxygen2 4.0 版本中，format.arguments_tag 改为 format.param_tag，并且参数多了 wrap
  xformat.arguments_tag <- function(x, ..., wrap = TRUE) {
    names <- names(x$values)
    dups <- duplicated(names)
    
    #items <- str_c('`', names, "`\n:   ", x$values, "", collapse = "\n\n")
    items <- str_wrap(x$values, exdent = 4)
    items = str_c('`', names, '`\n:   ', items, collapse = '\n\n')
    xrd_tag("arguments", items, space = F)
  }
  
  # 函数名
  xformat.name_tag <- function(x, ...) {
    x$values <- str_replace_all(x$values, fixed("%"), "\\%")
    #format_first(x, ...)
    str_c('\n# ', x$values[[1L]], '\n\n')
  }
  
  xformat.title_tag <- function(x, ...){
    xrd_tag('', x$values[1])
  }
  
  xformat.details_tag <- function(x, ..., indent = 0, exdent = 0, wrap = TRUE) {
    values <- str_c(x$values, collapse = "\n\n")
    xrd_tag(x$tag, values, space = TRUE)
  }
  
  require(R.utils)
  require(roxygen2)
  assignInNamespace('format.alias_tag', xformat.alias_tag, 'roxygen2')
  assignInNamespace('format.usage_tag', xformat.usage_tag, 'roxygen2')
  assignInNamespace('format.examples_tag', xformat.examples_tag, 'roxygen2')
  assignInNamespace('format.param_tag', xformat.arguments_tag, 'roxygen2')
  assignInNamespace('format.name_tag', xformat.name_tag, 'roxygen2')
  assignInNamespace('format.title_tag', xformat.title_tag, 'roxygen2')
  #assignInNamespace('format.details_tag', xformat.details_tag, 'roxygen2')
  assignInNamespace('rd_tag', xrd_tag, 'roxygen2')
  
  require(stringr)
  desc = pkg_get_desc(pkgdir)
  
  options = roxygen2:::load_options(pkgdir)
  roc = roxygen2:::rd_roclet()

	oldloc<-Sys.getlocale('LC_CTYPE')
	# 如果 LC_CTYPE 设置不对，则后续其他操作写入文件时，会破坏 R 文本的自动编码转换  
	on.exit(Sys.setlocale('LC_CTYPE', 'locale' = oldloc))
	Sys.setlocale('LC_CTYPE', locale="C")
	
  parsed <- roxygen2:::parse_package(pkgdir, roxygen2:::source_package)
  results = roxygen2:::roc_process(roc, parsed, pkgdir, options = options)
  results = results[order(names(results))]
  ss = roc_output_my(roc, results, pkgdir, options = options) 
  
  if (is.null(desc$encoding) || desc$encoding != 'UTF-8') {
    ss = iconv(ss, 'gb2312', to='UTF-8')
  }
  
  ss = sapply(ss, function(x) gsub('% Generated by roxygen.*\\n\\n', '', x, perl=T))
  
  fn = file.path(pkgdir, 'doc/doc.md')
  dir.create(path=dirname(fn), recursive=T, showWarnings=F)
  outcon = file(fn, 'wb') 
  writeBin(charToRaw(paste0(desc$md, paste0(ss, collapse='\n'))), outcon)
  close(outcon)
  
  return(fn)
}

#' 将 `markdown` 文档转换为`ConTeXt`
#' 
#' 调用 `pandoc` 将 `markdown` 文档转换为 `ConTeXt` 格式。
#' @note `pandoc` 及 `ConTeXt` 已在系统路径中。`ConTeXt` 环境初始化命令由 cmdCxtInit 参数决定。 
#' @param fn 待转换的文件名
#' @param template `ConTeXt` 模板，默认为 `r_pkg`
#' @param toPdf 是否自动调用 `ConTeXt` 编译 pdf。默认为 TRUE。
#' @param cmdCxtInit `ConTeXt` 初始化命令，默认为 `setuptex.bat`。
#' @export
#' @family package
#' @seealso oxygenize_markdown
# todo: check system path for pandoc and context
md2context<-function(fn, template='r_pkg', toPdf = TRUE, cmdCxtInit = 'setuptex.bat'){
	basename.noext <- function(filename){
  	ret = basename(filename)
  	paste(head(unlist(strsplit(ret, split='\\.')), -1L), collapse='.')
	}
  fn_tex = paste0(basename.noext(fn), '.tex')
  fn_pdf = paste0(basename.noext(fn), '.pdf')
  oldwd = getwd()
  on.exit(setwd(oldwd))
  
  template = ifelse(template == '', '', paste0('--template=', template))  
  setwd(dirname(fn))
  system(paste('pandoc -s -w context ', basename(fn), '-o ', fn_tex, template))  
  # make pdf
  if (toPdf) {
    ret = system(paste(cmdCxtInit, ' && context ', fn_tex), intern=T)
    if (!is.null(attr(ret, 'status'))) stop('ConText failed to generate pdf.')
    if (file.exists(fn_pdf)) {
      dest_fn = file.path(dirname(dirname(fn)), 'inst/doc', fn_pdf)
      dir.create(dirname(dest_fn), showWarnings = FALSE, recursive = TRUE)
      file.copy(fn_pdf, dest_fn, overwrite = T, copy.date = T)
    }
  }
}
