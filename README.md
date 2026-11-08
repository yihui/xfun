# xfun

<!-- badges: start -->
[![R-CMD-check](https://github.com/yihui/xfun/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/yihui/xfun/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/yihui/xfun/branch/main/graph/badge.svg)](https://app.codecov.io/gh/yihui/xfun?branch=main)
[![CRAN release](https://www.r-pkg.org/badges/version/xfun)](https://cran.r-project.org/package=xfun)
[![Build site](https://github.com/yihui/xfun/actions/workflows/litedown-site.yml/badge.svg)](https://github.com/yihui/xfun/actions/workflows/litedown-site.yml)
<!-- badges: end -->

**xfun** is a collection of small utility functions that I use across my own packages (**knitr**, **litedown**, **tinytex**, and others), plus miscellaneous helpers that I reach for in everyday scripting. It only imports base R packages (**grDevices**, **stats**, **tools**), and a few features use optional packages such as **knitr**, **jsonlite**, or **yaml**, so you can use it without bloating your own package's dependencies.

Browse the manual, the book, and a live playground at <https://pkg.yihui.org/xfun/>.

## Installation

```r
install.packages('xfun')
```

To install the development version from R-universe:

```r
install.packages('xfun', repos = 'https://yihui.r-universe.dev')
```

## A taste of what is inside

Turn numbers into words, and join words into a sentence:

```r
xfun::numbers_to_words(c(12, 305))
#> [1] "twelve"             "three hundred five"
xfun::join_words(c('apples', 'pears', 'plums'))
#> [1] "apples, pears, and plums"
```

Write a Markdown table without any dependencies:

```r
cat(xfun::md_table(data.frame(name = c('xfun', 'knitr'), stars = c(1.5, 2))), sep = '\n')
#> |name|stars|
#> |---|--:|
#> |xfun|1.5|
#> |knitr|2.0|
```

Serialize to JSON and parse YAML with no extra packages:

```r
cat(xfun::tojson(list(a = 1:3, b = 'x')))
#> {
#>   "a": [1, 2, 3],
#>   "b": "x"
#> }
str(xfun::taml_load('title: Hello\nn: 3\n'))
#> List of 2
#>  $ title: chr "Hello"
#>  $ n    : int 3
```

Cache a slow computation on disk; the second call returns instantly because the expression has not changed:

```r
x = xfun::cache_rds({ Sys.sleep(10); 42 })
x = xfun::cache_rds({ Sys.sleep(10); 42 })  # reads cache/cache_<hash>.rds
```

Retry something flaky:

```r
xfun::retry(function() download.file(url, 'data.zip'), .times = 5, .pause = 2)
```

Search and replace in a file, rewriting it only when the content changes:

```r
xfun::gsub_file('README.md', 'colour', 'color', fixed = TRUE)
```

Turn the output of `str()` into a tree diagram:

```r
xfun::tree(list(a = 1, b = list(c = 'x', d = 1:3)))
#> List of 2
#>  |-a: num 1
#>  |-b:List of 2
#>     |-c: chr "x"
#>     |-d: int [1:3] 1 2 3
```

Avoid partial matching bugs with a strict list:

```r
l = xfun::strict_list(abc = 1)
l$abc
#> [1] 1
l$a  # NULL, not 1
#> NULL
```

Embed a file in an HTML page as a base64 download link, with no external file needed:

```r
xfun::embed_file('data.csv', text = 'Download the data')
#> <a href="data:text/csv;base64,..." download="data.csv">Download the data</a>
```

Compute relative paths and strip extensions:

```r
xfun::relative_path('/a/b/c', '/a/d')
#> [1] "../b/c"
xfun::sans_ext('a/b.tar.gz')
#> [1] "a/b"
```

And much more: record R output as a structured object (`record()`), run R in a fresh session (`Rscript_call()`), print web pages to PDF with a headless browser (`browser_print()`), serve a directory (`serve_dir()`), check reverse dependencies (`rev_check()`), and so on. The manual groups every function by topic.
