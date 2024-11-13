#' Generate BibTeX bibliography databases for R packages
#'
#' Call [utils::citation()] and [utils::toBibtex()] to create bib entries for R
#' packages and write them in a file. It can facilitate the auto-generation of
#' bibliography databases for R packages, and it is easy to regenerate all the
#' citations after updating R packages.
#'
#' For a package, the keyword `R-pkgname` is used for its bib item, where
#' `pkgname` is the name of the package. Citation entries specified in the
#' \file{CITATION} file of the package are also included. The main purpose of
#' this function is to automate the generation of the package citation
#' information because it often changes (e.g., the author, year, package
#' version, and so on).
#'
#' There are at least two different uses for the URL in a reference list. You
#' might want to tell users where to go for more information. In that case, use
#' the default `packageURL = TRUE`, and the first URL listed in the
#' \file{DESCRIPTION} file will be used. Be careful: some authors don't put the
#' most relevant URL first. Alternatively, you might want to identify exactly
#' which version of the package was used in the document. If it was installed
#' from CRAN or some other repositories, the version number identifies it, and
#' `packageURL = FALSE` will use the repository URL (as used by
#' [utils::citation()]).
#' @param x Package names. Packages which are not installed are ignored.
#' @param file The (\file{.bib}) file to write. By default, or if `NULL`, output
#'   is written to the R console.
#' @param tweak Whether to fix some known problems in the citations, especially
#'   non-standard format of author names.
#' @param width Width of lines in bibliography entries. If `NULL`, lines will
#'   not be wrapped.
#' @param prefix Prefix string for keys in BibTeX entries; by default, it is
#'   `R-` unless `option('xfun.bib.prefix')` has been set to another string.
#' @param lib.loc A vector of path names of R libraries.
#' @param packageURL Use the `URL` field from the \file{DESCRIPTION} file. See
#'   Details below.
#' @return A list containing the citations. Citations are also written to the
#'   `file` as a side effect.
#' @note Some packages on CRAN do not have standard bib entries, which was once
#'   reported by Michael Friendly at
#'   \url{https://stat.ethz.ch/pipermail/r-devel/2010-November/058977.html}. I
#'   find this a pain, and there are no easy solutions except contacting package
#'   authors to modify their DESCRIPTION files. The argument `tweak` has
#'   provided hacks to deal with known packages with non-standard bib entries;
#'   `tweak = TRUE` is by no means intended to hide or modify the original
#'   citation information. It is just due to the loose requirements on package
#'   authors for the DESCRIPTION file. On one hand, I apologize if it really
#'   mangles the information about certain packages; on the other, I strongly
#'   recommend package authors to consider the `Authors@@R` field (see the
#'   manual _Writing R Extensions_) to make it easier for other people to cite R
#'   packages.
#' @export
#' @author Yihui Xie and Michael Friendly
#' @examples
#' pkg_bib(c('base', 'MASS', 'xfun'))
#' pkg_bib('cluster', prefix = 'R-pkg-')  # a different prefix
pkg_bib = function(
  x = .packages(), file = '', tweak = TRUE, width = NULL,
  prefix = getOption('xfun.bib.prefix', 'R-'), lib.loc = NULL,
  packageURL = TRUE
) {
  system.file = function(...) base::system.file(..., lib.loc = lib.loc)
  citation = function(...) utils::citation(..., lib.loc = lib.loc)
  x = x[nzchar(x)] # remove possible empty string
  idx = mapply(system.file, package = x) == ''
  if (any(idx)) {
    warning('package(s) ', paste(x[idx], collapse = ', '), ' not found')
    x = x[!idx]
  }
  # no need to write bib for packages in base R other than `base` itself
  x = setdiff(x, setdiff(base_pkgs(), 'base'))
  x = sort(x)
  bib = sapply(x, function(pkg) {
    meta = packageDescription(pkg, lib.loc = lib.loc)
    # don't use the citation() URL if the package has provided its own URL
    cite = citation(pkg, auto = if (is.null(meta$URL)) meta else {
      if (packageURL) meta$Repository = meta$RemoteType = NULL
      # use the first URL in case the package provided multiple URLs
      meta$URL = sub('[, \t\n].*', '', meta$URL)
      meta
    })
    if (tweak) {
      # e.g. gpairs has "gpairs: " in the title
      cite$title = gsub(sprintf('^(%s: )(\\1)', pkg), '\\1', cite$title)
    }
    entry = toBibtex(cite)
    entry[1] = sub('\\{,$', sprintf('{%s%s,', prefix, pkg), entry[1])
    entry
  }, simplify = FALSE)
  if (tweak) {
    # packages with non-standard author fields; use maintainer instead
    pkgs = c(
      "ash", "cat", "CircStats", "Fahrmeir", "flashClust", "leaps", "mapproj",
      "maps", "pbivnorm", "quadprog", "R2WinBUGS", "Sleuth2", "sm"
    )
    for (i in intersect(pkgs, x)) {
      message('tweaking ', i)
      maintainer = packageDescription(i, lib.loc, fields = 'Maintainer')
      bib[[i]]['author'] = sprintf('  author = {%s},', gsub('\\s+<.*>\\s*$', '', maintainer))
    }
    bib = lapply(bib, function(b) {
      b['author'] = sub('Duncan Temple Lang', 'Duncan {Temple Lang}', b['author'])
      # remove the ugly single quotes required by CRAN policy
      b['title'] = gsub("(^|\\W)'([^']+)'(\\W|$)", '\\1\\2\\3', b['title'])
      # keep the first URL if multiple are provided
      if (!is.na(b['note'])) b['note'] = gsub(
        '(^.*?https?://.*?),\\s+https?://.*?(},\\s*)$', '\\1\\2', b['note']
      )
      if (!('year' %in% names(b))) b['year'] = .this.year
      b
    })
  }
  # also read citation entries from the CITATION file if provided
  bib2 = lapply(x, function(pkg) {
    if (pkg == 'base') return()
    if (system.file('CITATION', package = pkg) == '') return()
    cites = citation(pkg, auto = FALSE)
    cites = Filter(x = cites, function(cite) {
      # exclude entries identical to citation(pkg, auto = TRUE)
      !isTRUE(grepl('R package version', cite$note))
    })
    s = make_unique(unlist(lapply(cites, function(cite) {
      if (is.null(cite$year)) format(Sys.Date(), '%Y') else cite$year
    })))
    mapply(cites, s, FUN = function(cite, suffix) {
      # the entry is likely to be the same as citation(pkg, auto = TRUE)
      if (isTRUE(grepl('R package version', cite$note))) return()
      entry = toBibtex(cite)
      entry[1] = sub('\\{,$', sprintf('{%s%s,', pkg, suffix), entry[1])
      entry
    }, SIMPLIFY = FALSE)
  })
  bib = c(bib, unlist(bib2, recursive = FALSE))
  bib = lapply(bib, function(b) {
    idx = which(names(b) == '')
    if (!is.null(width)) b[-idx] = str_wrap(b[-idx], width, 2, 4)
    lines = c(b[idx[1L]], b[-idx], b[idx[2L]], '')
    if (tweak) {
      # e.g. KernSmooth and spam has & in the title and the journal, respectively
      lines = gsub('(?<!\\\\)&', '\\\\&', lines, perl = TRUE)
    }
    structure(lines, class = 'Bibtex')
  })
  if (!is.null(file) && length(x)) write_utf8(unlist(bib), file)
  invisible(bib)
}

.this.year = sprintf('  year = {%s},', format(Sys.Date(), '%Y'))

# c(1, 1, 1, 2, 3, 3) -> c(1a, 1b, 1c, 2a, 3a, 3b)
make_unique = function(x) {
  if (length(x) == 0) return(x)
  x2 = make.unique(x)
  if (all(i <- x2 == x)) return(x)
  x2[i] = paste0(x2[i], '.0')
  i = as.numeric(sub('.*[.]([0-9]+)$', '\\1', x2)) + 1
  s = letters[i]
  s = ifelse(is.na(s), i, s)
  paste0(x, s)
}
