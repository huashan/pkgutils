.x.writePkgIndices <-function(dir, outDir, OS = .Platform$OS.type, html = TRUE) {
  re <- function(x)
  {
    ## sort order for topics, a little tricky
    ## FALSE sorts before TRUE
    xx <- rep(TRUE, length(x))
    xx[grep("-package", x, fixed = TRUE)] <- FALSE
    order(xx, toupper(x), x)
  }
  
  ## encode some entries.
  htmlize <- function(x, backtick)
  {
    #enc=Encoding(x)
    x <- gsub("&", "&amp;", x, fixed = TRUE, useBytes=T)
    x <- gsub("<", "&lt;", x, fixed = TRUE, useBytes=T)
    x <- gsub(">", "&gt;", x, fixed = TRUE, useBytes=T)
    if (backtick) {
      x <- gsub("---", "-", x, fixed = TRUE, useBytes=T)
      x <- gsub("--", "-", x, fixed = TRUE, useBytes=T)
      ## these have been changed in the Rd parser
      #x <- gsub("``", "&ldquo;", x, fixed = TRUE)
      #x <- gsub("''", "&rdquo;", x, fixed = TRUE)
      #x <- gsub("\\`([^']+)'", "&lsquo;\\1&rsquo;", x)
      #x <- gsub("`", "'", x, fixed = TRUE)
    }
    Encoding(x)='UTF-8'
    x
  }
  
  html_header <- function(pkg, title, version, conn)
  {
    cat(paste(tools:::HTMLheader(title, Rhome="../../..",
                                 up="../../../doc/html/packages.html",
                                 css = "R.css"),
              collapse = "\n"),
        '<h2>Documentation for package &lsquo;', pkg, '&rsquo; version ',
        version, '</h2>\n\n', sep = "", file = conn)
    
    cat('<ul><li><a href="../DESCRIPTION">DESCRIPTION file</a>.</li>\n', file=conn)
    if (file.exists(file.path(outDir, "doc")))
      cat('<li><a href="../doc/index.html">User guides, package vignettes and other documentation.</a></li>\n', file=conn)
    if (file.exists(file.path(outDir, "demo")))
      cat('<li><a href="../demo">Code demos</a>.  Use <a href="../../utils/help/demo">demo()</a> to run them.</li>\n',
          sep = "", file=conn)
    if (any(file.exists(c(file.path(outDir, "NEWS"), file.path(outDir, "NEWS.Rd")))))
      cat('<li><a href="../NEWS">Package NEWS</a>.</li>\n',
          sep = "", file=conn)
    
    cat('</ul>\n\n<h2>Help Pages</h2>\n\n\n',
        sep ="", file = conn)
  }
  
  firstLetterCategory <- function(x)
  {
    x[grep("-package$", x)] <- " "
    x <- toupper(substr(x, 1, 1))
    x[x > "Z"] <- "misc"
    x[x < "A" & x != " "] <- "misc"
    x
  }
  
  ## This may well already have been done:
  Rd <- if (file.exists(f <- file.path(outDir, "Meta", "Rd.rds")))
    readRDS(f)
  else {
    ## Keep this in sync with .install_package_Rd_indices().
    ## Rd objects should already have been installed.
    db <- tryCatch(tools::Rd_db(basename(outDir), lib.loc = dirname(outDir)),
                   error = function(e) NULL)
    ## If not, we build the Rd db from the sources:
    if (is.null(db)) db <- tools::Rd_db(dir = dir)
    Rd <- tools:::Rd_contents(db)
    saveRDS(Rd, file.path(outDir, "Meta", "Rd.rds"))
    Rd
  }
  
  topics <- Rd$Aliases
  M <- if (!length(topics)) {
    data.frame(Topic = character(),
               File = character(),
               Title = character(),
               Internal = character(),
               stringsAsFactors = FALSE)
  } else {
    lens <- sapply(topics, length)
    files <- sub("\\.[Rr]d$", "", Rd$File)
    internal <- sapply(Rd$Keywords, function(x) "internal" %in% x)
    data.frame(Topic = unlist(topics),
               File = rep.int(files, lens),
               Title = rep.int(Rd$Title, lens),
               Internal = rep.int(internal, lens),
               stringsAsFactors = FALSE)
  }
  ## FIXME duplicated aliases warning
  outman <- file.path(outDir, "help")
  dir.create(outman, showWarnings = FALSE)
  MM <- M[re(M[, 1L]), 1:2]
  write.table(MM, file.path(outman, "AnIndex"),
              quote = FALSE, row.names = FALSE, col.names = FALSE, sep = "\t")
  a <- structure(MM[, 2L], names=MM[, 1L])
  saveRDS(a, file.path(outman, "aliases.rds"))
  
  ## have HTML index even if no help pages
  outman <- file.path(outDir, "html")
  dir.create(outman, showWarnings = FALSE)
  outcon <- file(file.path(outman, "00Index.html"), "wt", encoding='UTF-8')
  on.exit(close(outcon))
  ## we know we have a valid file by now.
  desc <- read.dcf(file.path(outDir, "DESCRIPTION"))[1L, ]
  ## re-encode if necessary
  if(!is.na(enc <- desc["Encoding"])) {
    ## should be valid in UTF-8, might be invalid in declared encoding
    desc <- iconv(desc, enc, "UTF-8", sub = "byte")
  }
  # Huashan: added and then deprecated
  # else{
  #  Encoding(desc) <- desc['Encoding']
  #}
  
  ## drop internal entries
  M <- M[!M[, 4L], ]
  if (desc["Package"] %in% c("base", "graphics", "stats", "utils")) {
    for(pass in 1:2) {
      ## we skip method aliases
      gen <- gsub("\\.data\\.frame", ".data_frame", M$Topic)
      gen <- sub("\\.model\\.matrix$", ".modelmatrix", gen)
      gen <- sub("^(all|as|is|file|Sys|row|na|model)\\.", "\\1_", gen)
      gen <- sub("^(.*)\\.test", "\\1_test", gen)
      gen <- sub("([-[:alnum:]]+)\\.[^.]+$", "\\1", gen)
      last <- nrow(M)
      nongen <- gen %in% c("ar", "bw", "contr", "dyn", "lm", "qr", "ts", "which", ".Call", ".External", ".Library", ".First", ".Last")
      nc <- nchar(gen)
      asg <- (nc > 3) & substr(gen, nc-1, nc) == "<-"
      skip <- (gen == c("", gen[-last])) & (M$File == c("", M$File[-last])) & !nongen
      skip <- skip | asg
      ##N <- cbind(M$Topic, gen, c("", gen[-last]), skip)
      M <- M[!skip, ]
    }
  }
  
  # Collapse method links into unique (generic, file) pairs
  M$Topic <- sub("^([^,]*),.*-method$", "\\1-method", M$Topic)
  M <- M[!duplicated(M[, c("Topic", "File")]),]
  M <- M[re(M[, 1L]), ]
  
  M$HTopic <- htmlize(M$Topic, FALSE)
  M$Title <- htmlize(M$Title, TRUE)
  
  ## No need to handle encodings: everything is in UTF-8
  
  html_header(desc["Package"], desc["Title"], desc["Version"], outcon)
  
  use_alpha <- (nrow(M) > 100)
  if (use_alpha) {
    first <- firstLetterCategory(M$Topic)
    nm <- sort(names(table(first)))
    m <- match(" ", nm, 0L) # -package
    if (m) nm <- c(" ", nm[-m])
    m <- match("misc", nm, 0L) # force last in all locales.
    if (m) nm <- c(nm[-m], "misc")
    writeLines(c("<p align=\"center\">",
                 paste0("<a href=\"#", nm, "\">", nm, "</a>"),
                 "</p>\n"), outcon)
    for (f in nm) {
      MM <- M[first == f, ]
      if (f != " ")
        cat("\n<h2><a name=\"", f, "\">-- ", f, " --</a></h2>\n\n",
            sep = "", file = outcon)
      writeLines(c('<table width="100%">',
                   paste0('<tr><td width="25%"><a href="', MM[, 2L], '.html">',
                          MM$HTopic, '</a></td>\n<td>', MM[, 3L],'</td></tr>'),
                   "</table>"), outcon)
    }
  } else if (nrow(M)) {
    writeLines(c('<table width="100%">',
                 paste0('<tr><td width="25%"><a href="', M[, 2L], '.html">',
                        M$HTopic, '</a></td>\n<td>', M[, 3L],'</td></tr>'),
                 "</table>"), outcon)
  } else { # no rows
    writeLines("There are no help pages in this package", outcon)
  }
  writeLines('</body></html>', outcon)
  file.copy(file.path(R.home("doc"), "html", "R.css"), outman)
  invisible(NULL)
}

.do_oxygen <- function(pkg){
  oldloc<-Sys.getlocale('LC_CTYPE')
  # 如果 LC_CTYPE 设置不对，则后续其他操作写入文件时，会破坏 R 文本的自动编码转换  
  on.exit(Sys.setlocale('LC_CTYPE', 'locale' = oldloc))
  
  Sys.setlocale('LC_CTYPE', locale="C")
  
  # `roxygenize` 写入的 `rd` 文件编码取决于 `R` 文件的编码。若 `R` 文件为 `UTF-8` 编码，则
  # `rd` 文件也为 `UTF-8` 编码。此时，无需再做编码转换，在 `DESCRIPTION` 文件中添加 `Encoding` 
  # 字段或直接为 `rd` 文件添加 `\encoding{}` 即可。
  roxygen2::roxygenize(pkg, roclets='rd')
  
  desc = roxygen2:::read.description(paste0(pkg, 'DESCRIPTION'))
  if (is.null(desc$Encoding) || (desc$Encoding != 'UTF-8')) {
    roxygen_fix(pkg)
  }  
}

incrementBuildNumber<-function(pkg){
	desc = roxygen2:::read.description(file.path(pkg, 'DESCRIPTION'))
	ver = unlist(strsplit(desc$Version, '-'))
	ver.main = head(ver, 1L)
	ver.bvd = as.integer(tail(ver, 1L)) + 1	
	desc$Version = paste(c(ver.main, ver.bvd), collapse = '-')	
	if (!is.null(desc$Encoding)) desc = lapply(desc, function(x) {Encoding(x)=desc$Encoding; x})
	roxygen2:::write.description(desc, file.path(pkg, 'DESCRIPTION'))
}

#' 编译安装包
#' 
#' 当帮助中含有中文时，`R:tools` 包所编译的帮助文件存在中文乱码现象。
#' 通过修改生成 `00Index.html` 的函数 `.writePkgIndices` 修正此问题。
#' 
#' @param pkg 要编译的包路径
#' @param incrementBvdNum 是否自动增加构建版本号
#' @examples
#' install_packages('e:/R/packages/pkgutils/', TRUE)
#' @export
#' @family package
install_packages<-function(pkg, oxygenize = FALSE, incrementBvdNum = TRUE){

  if (oxygenize) .do_oxygen(pkg)
  if (incrementBvdNum) incrementBuildNumber(pkg)
  
  ns = asNamespace('tools')
  unlockBinding('.writePkgIndices', ns)
  assign('.writePkgIndices', .x.writePkgIndices, ns)
  lockBinding('.writePkgIndices', ns)
  # 当在函数外面时，可使用：
  #assignInNamespace('.writePkgIndices', .x.writePkgIndices, 'tools')
  
  path = paste(Sys.getenv('path'), 'D:/ToolsStat/RTools/gcc-4.6.3/bin;D:/ToolsStat/R3/bin;D:/ToolsStat/RTools/bin;', sep=';')
  Sys.setenv('path' = path)
  
  oldwd = getwd()
  on.exit(setwd(oldwd))
  setwd(dirname(pkg))

  # --no-clean-on-error',
  tools:::.install_packages(c('--build', "--no-test-load", "--no-multiarch", pkg))
}

# 当 pkg/data 下有文件时，R 会运行下述两个命令。
# 其中，`utils::data` 将会使用 `sys.source()` 读入 `*.r` 文件，从而可能造成
# 打包进程崩溃。例如，若`*.r` 中存在 `stop()` 的语句，则会令编译进程异常退出。
#debug(tools:::.install_packages)
#debug(tools:::.install_package_indices)
#debug(tools:::.install_package_Rd_indices)
#debug(tools:::list_data_in_pkg)
#debug(utils::data)


