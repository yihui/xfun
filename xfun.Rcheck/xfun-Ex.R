pkgname <- "xfun"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('xfun')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("Rscript")
### * Rscript

flush(stderr()); flush(stdout())

### Name: Rscript
### Title: Run the commands 'Rscript' and 'R CMD'
### Aliases: Rscript Rcmd

### ** Examples

library(xfun)
Rscript(c("-e", "1+1"))
Rcmd(c("build", "--help"))



cleanEx()
nameEx("Rscript_call")
### * Rscript_call

flush(stderr()); flush(stdout())

### Name: Rscript_call
### Title: Call a function in a new R session via 'Rscript()'
### Aliases: Rscript_call

### ** Examples

factorial(10)
# should return the same value
xfun::Rscript_call("factorial", list(10))

# the first argument can be either a character string or a function
xfun::Rscript_call(factorial, list(10))

# Run Rscript starting a vanilla R session
xfun::Rscript_call(factorial, list(10), options = c("--vanilla"))



cleanEx()
nameEx("alnum_id")
### * alnum_id

flush(stderr()); flush(stdout())

### Name: alnum_id
### Title: Generate ID strings
### Aliases: alnum_id

### ** Examples

x = c("Hello world 123!", "a  &b*^##c 456")
xfun::alnum_id(x)
xfun::alnum_id(x, "[^[:alpha:]]+")  # only keep alphabetical chars
# when text contains HTML tags
xfun::alnum_id("<h1>Hello <strong>world</strong>!")



cleanEx()
nameEx("attr2")
### * attr2

flush(stderr()); flush(stdout())

### Name: attr2
### Title: Obtain an attribute of an object without partial matching
### Aliases: attr2

### ** Examples

z = structure(list(a = 1), foo = 2)
base::attr(z, "f")  # 2
xfun::attr2(z, "f")  # NULL
xfun::attr2(z, "foo")  # 2



cleanEx()
nameEx("base64_encode")
### * base64_encode

flush(stderr()); flush(stdout())

### Name: base64_encode
### Title: Encode/decode data into/from base64 encoding.
### Aliases: base64_encode base64_decode

### ** Examples

xfun::base64_encode(as.raw(1:10))
logo = xfun:::R_logo()
xfun::base64_encode(logo)
xfun::base64_decode("AQIDBAUGBwgJCg==")



cleanEx()
nameEx("base64_uri")
### * base64_uri

flush(stderr()); flush(stdout())

### Name: base64_uri
### Title: Generate the Data URI for a file
### Aliases: base64_uri

### ** Examples

logo = xfun:::R_logo()
img = xfun::html_tag("img", src = xfun::base64_uri(logo), alt = "R logo")
if (interactive()) xfun::html_view(img)



cleanEx()
nameEx("base_pkgs")
### * base_pkgs

flush(stderr()); flush(stdout())

### Name: base_pkgs
### Title: Get base R package names
### Aliases: base_pkgs

### ** Examples

xfun::base_pkgs()



cleanEx()
nameEx("browser_print")
### * browser_print

flush(stderr()); flush(stdout())

### Name: browser_print
### Title: Print a web page to PDF/PNG/JPEG
### Aliases: browser_print

### ** Examples
## Don't show: 
if (interactive()) {
## End(Don't show)
xfun::browser_print("https://www.r-project.org")
## Don't show: 
}
## End(Don't show)



cleanEx()
nameEx("bump_version")
### * bump_version

flush(stderr()); flush(stdout())

### Name: bump_version
### Title: Bump version numbers
### Aliases: bump_version

### ** Examples

xfun::bump_version(c("0.1", "91.2.14"))



cleanEx()
nameEx("cache_exec")
### * cache_exec

flush(stderr()); flush(stdout())

### Name: cache_exec
### Title: Cache the execution of an expression in memory or on disk
### Aliases: cache_exec

### ** Examples

# the first run takes about 1 second
y1 = xfun::cache_exec({
    x = rnorm(1e+05)
    Sys.sleep(1)
    x
}, path = ":memory:", id = "sim-norm")

# the second run takes almost no time
y2 = xfun::cache_exec({
    # comments won't affect caching
    x = rnorm(1e+05)
    Sys.sleep(1)
    x
}, path = ":memory:", id = "sim-norm")

# y1, y2, and x should be identical
stopifnot(identical(y1, y2), identical(y1, x))



cleanEx()
nameEx("cache_rds")
### * cache_rds

flush(stderr()); flush(stdout())

### Name: cache_rds
### Title: Cache the value of an R expression to an RDS file
### Aliases: cache_rds
### Keywords: internal

### ** Examples

f = tempfile()  # the cache file
compute = function(...) {
    res = xfun::cache_rds({
        Sys.sleep(1)
        1:10
    }, file = f, dir = "", ...)
    res
}
compute()  # takes one second
compute()  # returns 1:10 immediately
compute()  # fast again
compute(rerun = TRUE)  # one second to rerun
compute()
unlink(paste0(f, "_*.rds"))



cleanEx()
nameEx("csv_options")
### * csv_options

flush(stderr()); flush(stdout())

### Name: csv_options
### Title: Parse comma-separated chunk options
### Aliases: csv_options

### ** Examples

xfun::csv_options("foo, eval=TRUE, fig.width=5, echo=if (TRUE) FALSE")



cleanEx()
nameEx("decimal_dot")
### * decimal_dot

flush(stderr()); flush(stdout())

### Name: decimal_dot
### Title: Evaluate an expression after forcing the decimal point to be a
###   dot
### Aliases: decimal_dot

### ** Examples

opts = options(OutDec = ",")
as.character(1.234)  # using ',' as the decimal separator
print(1.234)  # same
xfun::decimal_dot(as.character(1.234))  # using dot
xfun::decimal_dot(print(1.234))  # using dot
options(opts)



cleanEx()
nameEx("divide_chunk")
### * divide_chunk

flush(stderr()); flush(stdout())

### Name: divide_chunk
### Title: Divide chunk options from the code chunk body
### Aliases: divide_chunk

### ** Examples

# parse yaml-like items
yaml_like = c("#| label: mine", "#| echo: true", "#| fig.width: 8", "#| foo: bar",
    "1 + 1")
writeLines(yaml_like)
xfun::divide_chunk("r", yaml_like)

# parse CSV syntax
csv_like = c("#| mine, echo = TRUE, fig.width = 8, foo = 'bar'", "1 + 1")
writeLines(csv_like)
xfun::divide_chunk("r", csv_like)



cleanEx()
nameEx("do_once")
### * do_once

flush(stderr()); flush(stdout())

### Name: do_once
### Title: Perform a task once in an R session
### Aliases: do_once

### ** Examples

do_once(message("Today's date is ", Sys.Date()), "xfun.date.reminder")
# if you run it again, it will not emit the message again
do_once(message("Today's date is ", Sys.Date()), "xfun.date.reminder")

do_once({
    Sys.sleep(2)
    1 + 1
}, "xfun.task.1plus1")
do_once({
    Sys.sleep(2)
    1 + 1
}, "xfun.task.1plus1")



cleanEx()
nameEx("download_cache")
### * download_cache

flush(stderr()); flush(stdout())

### Name: download_cache
### Title: Download a file from a URL and cache it on disk
### Aliases: download_cache
### Keywords: datasets

### ** Examples
## Don't show: 
if (interactive()) {
## End(Don't show)
# the first time it may take a few seconds
x1 = xfun::download_cache$get("https://www.r-project.org/")
head(x1)

# now you can get the cached content
x2 = xfun::download_cache$get("https://www.r-project.org/")
identical(x1, x2)  # TRUE

# a binary file
x3 = xfun::download_cache$get("https://yihui.org/images/logo.png", "raw")
length(x3)

# show a summary
xfun::download_cache$summary()
# remove a specific cache file
xfun::download_cache$remove("https://yihui.org/images/logo.png", "raw")
# remove all cache files
xfun::download_cache$purge()
## Don't show: 
}
## End(Don't show)



cleanEx()
nameEx("embed_file")
### * embed_file

flush(stderr()); flush(stdout())

### Name: embed_file
### Title: Embed a file, multiple files, or directory on an HTML page
### Aliases: embed_file embed_dir embed_files

### ** Examples

logo = xfun:::R_logo()
link = xfun::embed_file(logo, text = "Download R logo")
link
if (interactive()) xfun::html_view(link)



cleanEx()
nameEx("env_option")
### * env_option

flush(stderr()); flush(stdout())

### Name: env_option
### Title: Retrieve a global option from both 'options()' and environment
###   variables
### Aliases: env_option

### ** Examples

xfun::env_option("xfun.test.option")  # NULL

Sys.setenv(R_XFUN_TEST_OPTION = "1234")
xfun::env_option("xfun.test.option")  # 1234

options(xfun.test.option = TRUE)
xfun::env_option("xfun.test.option")  # TRUE (from options())
options(xfun.test.option = NULL)  # reset the option
xfun::env_option("xfun.test.option")  # 1234 (from env var)

Sys.unsetenv("R_XFUN_TEST_OPTION")
xfun::env_option("xfun.test.option")  # NULL again

xfun::env_option("xfun.test.option", FALSE)  # use default



cleanEx()
nameEx("existing_files")
### * existing_files

flush(stderr()); flush(stdout())

### Name: existing_files
### Title: Find file paths that exist
### Aliases: existing_files

### ** Examples

xfun::existing_files(c("foo.txt", system.file("DESCRIPTION", package = "xfun")))



cleanEx()
nameEx("exit_call")
### * exit_call

flush(stderr()); flush(stdout())

### Name: exit_call
### Title: Call 'on.exit()' in a parent function
### Aliases: exit_call

### ** Examples

f = function(x) {
    print(x)
    xfun::exit_call(function() print("The parent function is exiting!"))
}
g = function(y) {
    f(y)
    print("f() has been called!")
}
g("An argument of g()!")



cleanEx()
nameEx("fenced_block")
### * fenced_block

flush(stderr()); flush(stdout())

### Name: fenced_block
### Title: Create a fenced block in Markdown
### Aliases: fenced_block fenced_div make_fence

### ** Examples

# code block with class 'r' and ID 'foo'
xfun::fenced_block("1+1", c(".r", "#foo"))
# fenced Div
xfun::fenced_block("This is a **Div**.", char = ":")
# three backticks by default
xfun::make_fence("1+1")
# needs five backticks for the fences because content has four
xfun::make_fence(c("````r", "1+1", "````"))



cleanEx()
nameEx("file_ext")
### * file_ext

flush(stderr()); flush(stdout())

### Name: file_ext
### Title: Manipulate filename extensions
### Aliases: file_ext sans_ext with_ext

### ** Examples

library(xfun)
p = c("abc.doc", "def123.tex", "path/to/foo.Rmd", "backup.ppt~", "pkg.tar.xz")
file_ext(p)
sans_ext(p)
with_ext(p, ".txt")
with_ext(p, c(".ppt", ".sty", ".Rnw", "doc", "zip"))
with_ext(p, "html")

# allow for more characters in extensions
p = c("a.c++", "b.c--", "c.e##")
file_ext(p)  # -/+/# not recognized by default
file_ext(p, extra = "-+#")



cleanEx()
nameEx("file_string")
### * file_string

flush(stderr()); flush(stdout())

### Name: file_string
### Title: Read a text file and concatenate the lines by "\n"
### Aliases: file_string

### ** Examples

xfun::file_string(system.file("DESCRIPTION", package = "xfun"))



cleanEx()
nameEx("find_globals")
### * find_globals

flush(stderr()); flush(stdout())

### Name: find_globals
### Title: Find global/local variables in R code
### Aliases: find_globals find_locals

### ** Examples

x = 2
xfun::find_globals("y = x + 1")
xfun::find_globals("y = get('x') + 1")  # x is not recognized
xfun::find_globals("y = zzz + 1")  # zzz doesn't exist

xfun::find_locals("y = x + 1")
xfun::find_locals("assign('y', x + 1)")  # it works
xfun::find_locals("assign('y', x + 1, new.env())")  # still smart
xfun::find_locals("eval(parse(text = 'y = x + 1'))")  # no way



cleanEx()
nameEx("format_bytes")
### * format_bytes

flush(stderr()); flush(stdout())

### Name: format_bytes
### Title: Format numbers of bytes using a specified unit
### Aliases: format_bytes

### ** Examples

xfun::format_bytes(c(1, 1024, 2000, 1e+06, 2e+08))
xfun::format_bytes(c(1, 1024, 2000, 1e+06, 2e+08), units = "KB")



cleanEx()
nameEx("from_root")
### * from_root

flush(stderr()); flush(stdout())

### Name: from_root
### Title: Get the relative path of a path in a project relative to the
###   current working directory
### Aliases: from_root

### ** Examples

## Not run: 
##D xfun::from_root("data", "mtcars.csv")
## End(Not run)



cleanEx()
nameEx("github_releases")
### * github_releases

flush(stderr()); flush(stdout())

### Name: github_releases
### Title: Get the tags of GitHub releases of a repository
### Aliases: github_releases

### ** Examples
## Don't show: 
if (interactive()) {
## End(Don't show)
xfun::github_releases("yihui/litedown")
xfun::github_releases("gohugoio/hugo", "latest")
## Don't show: 
}
## End(Don't show)



cleanEx()
nameEx("grep_sub")
### * grep_sub

flush(stderr()); flush(stdout())

### Name: grep_sub
### Title: Perform replacement with 'gsub()' on elements matched from
###   'grep()'
### Aliases: grep_sub

### ** Examples

# find elements that matches 'a[b]+c' and capitalize 'b' with perl regex
xfun::grep_sub("a([b]+)c", "a\\U\\1c", c("abc", "abbbc", "addc", "123"), perl = TRUE)



cleanEx()
nameEx("gsub_file")
### * gsub_file

flush(stderr()); flush(stdout())

### Name: gsub_file
### Title: Search and replace strings in files
### Aliases: gsub_file gsub_files gsub_dir gsub_ext

### ** Examples

library(xfun)
f = tempfile()
writeLines(c("hello", "world"), f)
gsub_file(f, "world", "woRld", fixed = TRUE)
readLines(f)



cleanEx()
nameEx("html_tag")
### * html_tag

flush(stderr()); flush(stdout())

### Name: html_tag
### Title: Tools for HTML tags
### Aliases: html_tag html_value html_escape html_view

### ** Examples

xfun::html_tag("a", "<R Project>", href = "https://www.r-project.org", target = "_blank")
xfun::html_tag("br")
xfun::html_tag("a", xfun::html_tag("strong", "R Project"), href = "#")
xfun::html_tag("a", list("<text>", xfun::html_tag("b", "R Project")), href = "#")
xfun::html_escape("\" quotes \" & brackets < >")
xfun::html_escape("\" & < > \r \n", attr = TRUE)



cleanEx()
nameEx("in_dir")
### * in_dir

flush(stderr()); flush(stdout())

### Name: in_dir
### Title: Evaluate an expression under a specified working directory
### Aliases: in_dir

### ** Examples

library(xfun)
in_dir(tempdir(), {
    print(getwd())
    list.files()
})



cleanEx()
nameEx("is_abs_path")
### * is_abs_path

flush(stderr()); flush(stdout())

### Name: is_abs_path
### Title: Test if paths are relative or absolute
### Aliases: is_abs_path is_rel_path

### ** Examples

xfun::is_abs_path(c("C:/foo", "foo.txt", "/Users/john/", tempdir()))
xfun::is_rel_path(c("C:/foo", "foo.txt", "/Users/john/", tempdir()))



cleanEx()
nameEx("is_ascii")
### * is_ascii

flush(stderr()); flush(stdout())

### Name: is_ascii
### Title: Check if a character vector consists of entirely ASCII
###   characters
### Aliases: is_ascii

### ** Examples

library(xfun)
is_ascii(letters)  # yes
is_ascii(intToUtf8(8212))  # no



cleanEx()
nameEx("is_blank")
### * is_blank

flush(stderr()); flush(stdout())

### Name: is_blank
### Title: Test if a character vector consists of blank strings
### Aliases: is_blank

### ** Examples

xfun::is_blank("")
xfun::is_blank("abc")
xfun::is_blank(c("", "  ", "\n\t"))
xfun::is_blank(c("", " ", "abc"))



cleanEx()
nameEx("is_sub_path")
### * is_sub_path

flush(stderr()); flush(stdout())

### Name: is_sub_path
### Title: Test if a path is a subpath of a dir
### Aliases: is_sub_path

### ** Examples

xfun::is_sub_path("a/b/c.txt", "a/b")  # TRUE
xfun::is_sub_path("a/b/c.txt", "d/b")  # FALSE
xfun::is_sub_path("a/b/c.txt", "a\\b")  # FALSE (even on Windows)



cleanEx()
nameEx("is_web_path")
### * is_web_path

flush(stderr()); flush(stdout())

### Name: is_web_path
### Title: Test if a path is a web path
### Aliases: is_web_path

### ** Examples

xfun::is_web_path("https://www.r-project.org")  # TRUE
xfun::is_web_path("www.r-project.org")  # FALSE



cleanEx()
nameEx("join_words")
### * join_words

flush(stderr()); flush(stdout())

### Name: join_words
### Title: Join multiple words into a single string
### Aliases: join_words

### ** Examples

join_words("a")
join_words(c("a", "b"))
join_words(c("a", "b", "c"))
join_words(c("a", "b", "c"), sep = " / ", and = "")
join_words(c("a", "b", "c"), and = "")
join_words(c("a", "b", "c"), before = "\"", after = "\"")
join_words(c("a", "b", "c"), before = "\"", after = "\"", oxford_comma = FALSE)



cleanEx()
nameEx("magic_path")
### * magic_path

flush(stderr()); flush(stdout())

### Name: magic_path
### Title: Find a file or directory under a root directory
### Aliases: magic_path

### ** Examples

## Not run: 
##D xfun::magic_path("mtcars.csv")  # find any file that has the base name mtcars.csv
## End(Not run)



cleanEx()
nameEx("mark_dirs")
### * mark_dirs

flush(stderr()); flush(stdout())

### Name: mark_dirs
### Title: Mark some paths as directories
### Aliases: mark_dirs

### ** Examples

mark_dirs(list.files(find.package("xfun"), full.names = TRUE))



cleanEx()
nameEx("md5")
### * md5

flush(stderr()); flush(stdout())

### Name: md5
### Title: Calculate the MD5 checksums of R objects
### Aliases: md5

### ** Examples

x1 = 1
x2 = 1:10
x3 = seq(1, 10)
x4 = iris
x5 = paste
(m = xfun::md5(x1, x2, x3, x4, x5))
stopifnot(m[2] == m[3])  # x2 and x3 should be identical

xfun::md5(x1 = x1, x2 = x2)  # named arguments



cleanEx()
nameEx("md_table")
### * md_table

flush(stderr()); flush(stdout())

### Name: md_table
### Title: Generate a simple Markdown pipe table
### Aliases: md_table

### ** Examples

xfun::md_table(head(iris))
xfun::md_table(mtcars, limit = c(10, 6))



cleanEx()
nameEx("mime_type")
### * mime_type

flush(stderr()); flush(stdout())

### Name: mime_type
### Title: Get the MIME types of files
### Aliases: mime_type

### ** Examples
## Don't show: 
if (tolower(Sys.getenv('CI')) == 'true') {
## End(Don't show)
f = list.files(R.home("doc"), full.names = TRUE)
mime_type(f)
mime_type(f, FALSE)  # don't use mime
mime_type(f, FALSE, NA)  # run command for files without extension
## Don't show: 
}
## End(Don't show)



cleanEx()
nameEx("msg_cat")
### * msg_cat

flush(stderr()); flush(stdout())

### Name: msg_cat
### Title: Generate a message with 'cat()'
### Aliases: msg_cat

### ** Examples

{
    # a message without a newline at the end
    xfun::msg_cat("Hello world!")
    # add a newline at the end
    xfun::msg_cat(" This message appears right after the previous one.\n")
}
suppressMessages(xfun::msg_cat("Hello world!"))



cleanEx()
nameEx("native_encode")
### * native_encode

flush(stderr()); flush(stdout())

### Name: native_encode
### Title: Try to use the system native encoding to represent a character
###   vector
### Aliases: native_encode

### ** Examples

library(xfun)
s = intToUtf8(c(20320, 22909))
Encoding(s)

s2 = native_encode(s)
Encoding(s2)



cleanEx()
nameEx("news2md")
### * news2md

flush(stderr()); flush(stdout())

### Name: news2md
### Title: Convert package news to the Markdown format
### Aliases: news2md

### ** Examples
## Don't show: 
if (interactive()) {
## End(Don't show)
# news for the current version of R
xfun::news2md("R", Version == getRversion(), output = NA)
## Don't show: 
}
## End(Don't show)



cleanEx()
nameEx("normalize_path")
### * normalize_path

flush(stderr()); flush(stdout())

### Name: normalize_path
### Title: Normalize paths
### Aliases: normalize_path

### ** Examples

library(xfun)
normalize_path("~")



cleanEx()
nameEx("numbers_to_words")
### * numbers_to_words

flush(stderr()); flush(stdout())

### Name: numbers_to_words
### Title: Convert numbers to English words
### Aliases: numbers_to_words n2w

### ** Examples

library(xfun)
n2w(0, cap = TRUE)
n2w(0:121, and = TRUE)
n2w(1e+06)
n2w(1e+11 + 12345678)
n2w(-987654321)
n2w(1e+15 - 1)
n2w(123.456)
n2w(123.45678901)
n2w(123.456789098765)



cleanEx()
nameEx("os")
### * os

flush(stderr()); flush(stdout())

### Name: is_windows
### Title: Test for types of operating systems
### Aliases: is_windows is_unix is_macos is_linux is_arm64

### ** Examples

library(xfun)
# only one of the following statements should be true
is_windows()
is_unix() && is_macos()
is_linux()
# In newer Macs, CPU can be either Intel or Apple
is_arm64()  # TRUE on Apple silicone machines



cleanEx()
nameEx("parse_only")
### * parse_only

flush(stderr()); flush(stdout())

### Name: parse_only
### Title: Parse R code and do not keep the source
### Aliases: parse_only

### ** Examples

library(xfun)
parse_only("1+1")
parse_only(c("y~x", "1:5 # a comment"))
parse_only(character(0))



cleanEx()
nameEx("pkg_attach")
### * pkg_attach

flush(stderr()); flush(stdout())

### Name: pkg_attach
### Title: Attach or load packages, and automatically install missing
###   packages if requested
### Aliases: pkg_attach pkg_load loadable pkg_available pkg_attach2
###   pkg_load2

### ** Examples

library(xfun)
pkg_attach("stats", "graphics")
# pkg_attach2('servr') # automatically install servr if it is not installed

(pkg_load("stats", "graphics"))



cleanEx()
nameEx("pkg_bib")
### * pkg_bib

flush(stderr()); flush(stdout())

### Name: pkg_bib
### Title: Generate BibTeX bibliography databases for R packages
### Aliases: pkg_bib

### ** Examples

pkg_bib(c("base", "MASS", "xfun"))
pkg_bib("cluster", prefix = "R-pkg-")  # a different prefix
pkg_bib("xfun", date = "package")



cleanEx()
nameEx("process_file")
### * process_file

flush(stderr()); flush(stdout())

### Name: process_file
### Title: Read a text file, process the text with a function, and write
###   the text back
### Aliases: process_file sort_file

### ** Examples

f = tempfile()
xfun::write_utf8("Hello World", f)
xfun::process_file(f, function(x) gsub("World", "woRld", x))
xfun::read_utf8(f)  # see if it has been updated
file.remove(f)



cleanEx()
nameEx("prose_index")
### * prose_index

flush(stderr()); flush(stdout())

### Name: prose_index
### Title: Find the indices of lines in Markdown that are prose (not code
###   blocks)
### Aliases: prose_index

### ** Examples

library(xfun)
prose_index(c("a", "```", "b", "```", "c"))
prose_index(c("a", "````", "```r", "1+1", "```", "````", "c"))



cleanEx()
nameEx("protect_math")
### * protect_math

flush(stderr()); flush(stdout())

### Name: protect_math
### Title: Protect math expressions in pairs of backticks in Markdown
### Aliases: protect_math

### ** Examples

library(xfun)
protect_math(c("hi $a+b$", "hello $$\\alpha$$", "no math here: $x is $10 dollars"))
protect_math(c("hi $$", "\\begin{equation}", "x + y = z", "\\end{equation}"))
protect_math("$a+b$", "===")



cleanEx()
nameEx("rand_unit")
### * rand_unit

flush(stderr()); flush(stdout())

### Name: rand_unit
### Title: Pseudo-random numbers on [0, 1) based on a linear congruential
###   generator
### Aliases: rand_unit

### ** Examples

rand_unit(10)
rand_unit(10, seed = 0)
rand_unit(10, seed = 0)  # identical results
rand_unit(10, seed = Sys.getpid())



cleanEx()
nameEx("raw_string")
### * raw_string

flush(stderr()); flush(stdout())

### Name: raw_string
### Title: Print a character vector in its raw form
### Aliases: raw_string print.xfun_raw_string

### ** Examples

library(xfun)
raw_string(head(LETTERS))
raw_string(c("a \"b\"", "hello\tworld!"))



cleanEx()
nameEx("read_all")
### * read_all

flush(stderr()); flush(stdout())

### Name: read_all
### Title: Read all text files and concatenate their content
### Aliases: read_all

### ** Examples

# two files in this package
fs = system.file("scripts", c("call-fun.R", "child-pids.sh"), package = "xfun")
xfun::read_all(fs)

# add file paths before file content and an empty line after content
xfun::read_all(fs, before = function(f) paste("#-----", f, "-----"), after = "")

# add constants
xfun::read_all(fs, before = "/*", after = c("*/", ""))



cleanEx()
nameEx("read_bin")
### * read_bin

flush(stderr()); flush(stdout())

### Name: read_bin
### Title: Read all records of a binary file as a raw vector by default
### Aliases: read_bin

### ** Examples

f = tempfile()
cat("abc", file = f)
xfun::read_bin(f)
unlink(f)



cleanEx()
nameEx("record")
### * record

flush(stderr()); flush(stdout())

### Name: record
### Title: Run R code and record the results
### Aliases: record format.xfun_record_results print.xfun_record_results

### ** Examples

code = c("# a warning test", "1:2 + 1:3", "par(mar = c(4, 4, 1, .2))",
    "barplot(5:1, col = 2:6, horiz = TRUE)", "head(iris)",
    "sunflowerplot(iris[, 3:4], seg.col = 'purple')",
    "if (TRUE) {\n  message('Hello, xfun::record()!')\n}",
    "# throw an error", "1 + 'a'")
res = xfun::record(code, dev.args = list(width = 9, height = 6.75),
    error = TRUE)
xfun::tree(res)
format(res)
# find and clean up plot files
plots = Filter(function(x) inherits(x, "record_plot"),
    res)
file.remove(unlist(plots))



graphics::par(get("par.postscript", pos = 'CheckExEnv'))
cleanEx()
nameEx("relative_path")
### * relative_path

flush(stderr()); flush(stdout())

### Name: relative_path
### Title: Get the relative path of a path relative to a directory
### Aliases: relative_path

### ** Examples

xfun::relative_path("foo/bar.txt", "foo/")
xfun::relative_path("foo/bar/a.txt", "foo/haha/")
xfun::relative_path(getwd())



cleanEx()
nameEx("rename_seq")
### * rename_seq

flush(stderr()); flush(stdout())

### Name: rename_seq
### Title: Rename files with a sequential numeric prefix
### Aliases: rename_seq

### ** Examples

xfun::rename_seq()
xfun::rename_seq("[.](jpeg|png)$", format = "%04d")



cleanEx()
nameEx("rest_api")
### * rest_api

flush(stderr()); flush(stdout())

### Name: rest_api
### Title: Get data from a REST API
### Aliases: rest_api rest_api_raw github_api

### ** Examples
## Don't show: 
if (interactive()) {
## End(Don't show)
# a normal GET request
xfun::rest_api("https://mockhttp.org", "/get")
xfun::rest_api_raw("https://mockhttp.org", "/get")

# send the request with an auth header
xfun::rest_api("https://mockhttp.org", "/headers", "OPEN SESAME!")

# with query parameters
xfun::rest_api("https://mockhttp.org", "/response-headers", params = list(foo = "bar"))

# get the rate limit info from GitHub
xfun::github_api("/rate_limit")
## Don't show: 
}
## End(Don't show)



cleanEx()
nameEx("retry")
### * retry

flush(stderr()); flush(stdout())

### Name: retry
### Title: Retry calling a function for a number of times
### Aliases: retry

### ** Examples
## Don't show: 
if (interactive()) {
## End(Don't show)
# read the GitHub releases info of the repo yihui/xfun
xfun::retry(xfun::github_releases, "yihui/xfun")
## Don't show: 
}
## End(Don't show)



cleanEx()
nameEx("rstudio_type")
### * rstudio_type

flush(stderr()); flush(stdout())

### Name: rstudio_type
### Title: Type a character vector into the RStudio source editor
### Aliases: rstudio_type

### ** Examples

library(xfun)
if (loadable("rstudioapi") && rstudioapi::isAvailable()) {
    rstudio_type("Hello, RStudio! xfun::rstudio_type() looks pretty cool!",
        pause = function() runif(1, 0, 0.5), mistake = 0.1)
}



cleanEx()
nameEx("same_path")
### * same_path

flush(stderr()); flush(stdout())

### Name: same_path
### Title: Test if two paths are the same after they are normalized
### Aliases: same_path

### ** Examples

library(xfun)
same_path("~/foo", file.path(Sys.getenv("HOME"), "foo"))



cleanEx()
nameEx("session_info")
### * session_info

flush(stderr()); flush(stdout())

### Name: session_info
### Title: An alternative to sessionInfo() to print session information
### Aliases: session_info

### ** Examples
## Don't show: 
if (interactive()) {
## End(Don't show)
xfun::session_info()
if (xfun::loadable("MASS")) xfun::session_info("MASS")
## Don't show: 
}
## End(Don't show)



cleanEx()
nameEx("set_envvar")
### * set_envvar

flush(stderr()); flush(stdout())

### Name: set_envvar
### Title: Set environment variables
### Aliases: set_envvar

### ** Examples

vars = xfun::set_envvar(c(FOO = "1234"))
Sys.getenv("FOO")
xfun::set_envvar(vars)
Sys.getenv("FOO")



cleanEx()
nameEx("shrink_images")
### * shrink_images

flush(stderr()); flush(stdout())

### Name: shrink_images
### Title: Shrink images to a maximum width
### Aliases: shrink_images

### ** Examples
## Don't show: 
if (xfun::loadable('magick')) {
## End(Don't show)
f = xfun:::all_files("[.](png|jpe?g)$", R.home("doc"))
file.copy(f, tempdir())
f = file.path(tempdir(), basename(f))
magick::image_info(magick::image_read(f))  # some widths are larger than 300
xfun::shrink_images(300, files = f)
magick::image_info(magick::image_read(f))  # all widths <= 300 now
file.remove(f)
## Don't show: 
}
## End(Don't show)



cleanEx()
nameEx("split_lines")
### * split_lines

flush(stderr()); flush(stdout())

### Name: split_lines
### Title: Split a character vector by line breaks
### Aliases: split_lines

### ** Examples

xfun::split_lines(c("a", "b\nc"))



cleanEx()
nameEx("split_source")
### * split_source

flush(stderr()); flush(stdout())

### Name: split_source
### Title: Split source lines into complete expressions
### Aliases: split_source

### ** Examples

code = c("# comment 1", "# comment 2", "if (TRUE) {", "1 + 1", "}", "print(1:5)")
xfun::split_source(code)
xfun::split_source(code, merge_comments = TRUE)



cleanEx()
nameEx("str_wrap")
### * str_wrap

flush(stderr()); flush(stdout())

### Name: str_wrap
### Title: Wrap character vectors
### Aliases: str_wrap

### ** Examples

x = sample(c(letters, " "), 200, TRUE, c(rep(0.5/26, 26), 0.5))
x = rep(paste(x, collapse = ""), 2)
strwrap(x, width = 30)
xfun::str_wrap(x, width = 30)  # same length as x



cleanEx()
nameEx("strict_list")
### * strict_list

flush(stderr()); flush(stdout())

### Name: strict_list
### Title: Strict lists
### Aliases: strict_list as_strict_list $.xfun_strict_list
###   print.xfun_strict_list

### ** Examples

library(xfun)
(z = strict_list(aaa = "I am aaa", b = 1:5))
z$a  # NULL!
z$aaa  # I am aaa
z$b
z$c = "create a new element"

z2 = unclass(z)  # a normal list
z2$a  # partial matching

z3 = as_strict_list(z2)  # a strict list again
z3$a  # NULL again!



cleanEx()
nameEx("strip_html")
### * strip_html

flush(stderr()); flush(stdout())

### Name: strip_html
### Title: Strip HTML tags
### Aliases: strip_html

### ** Examples

xfun::strip_html("<a href=\"#\">Hello <!-- comment -->world!</a>")



cleanEx()
nameEx("system3")
### * system3

flush(stderr()); flush(stdout())

### Name: system3
### Title: Run 'system2()' and mark its character output as UTF-8 if
###   appropriate
### Aliases: system3

### ** Examples
## Don't show: 
if (interactive()) {
## End(Don't show)
a = shQuote(c("-e", "print(intToUtf8(c(20320, 22909)))"))
x2 = system2("Rscript", a, stdout = TRUE)
Encoding(x2)  # unknown

x3 = xfun::system3("Rscript", a, stdout = TRUE)
# encoding of x3 should be UTF-8 if the current locale is UTF-8
!l10n_info()[["UTF-8"]] || Encoding(x3) == "UTF-8"  # should be TRUE
## Don't show: 
}
## End(Don't show)



cleanEx()
nameEx("tabset")
### * tabset

flush(stderr()); flush(stdout())

### Name: tabset
### Title: Represent a (recursive) list with (nested) tabsets
### Aliases: tabset tab_content

### ** Examples

xfun::tabset(iris)
xfun::tabset(iris, dput)
xfun::tabset(iris, print)

# a deeply nested list
plot(1:10)
p = recordPlot()
xfun::tabset(p)

# custom tab content
xfun::tabset(iris, function(x) {
    if (is.factor(x)) {
        res = c("A factor with levels: ", xfun::join_words(levels(x), before = "`"))
        xfun::tab_content(res)
    } else print(summary(x))
})



cleanEx()
nameEx("taml_load")
### * taml_load

flush(stderr()); flush(stdout())

### Name: taml_load
### Title: A simple YAML reader and writer
### Aliases: taml_load taml_file taml_save

### ** Examples

(res = taml_load("a: 1"))
taml_save(res)

(res = taml_load("a: 1\nb: \"foo\"\nc: null"))
taml_save(res)

(res = taml_load("a:\n  b: false\n  c: true\n  d: 1.234\ne: bar"))
taml_save(res)
taml_save(res, indent = "\t")

taml_load("a: !expr paste(1:10, collapse = \", \")")
taml_load("a: [1, 3, 4, 2]")
taml_load("a: [1, \"abc\", 4, 2]")
taml_load("a: [\"foo\", \"bar\"]")
taml_load("a: [true, false, true]")
# the other form of array is not supported
taml_load("a:\n  - b\n  - c")
# and you must use the yaml package
if (loadable("yaml")) yaml_load("a:\n  - b\n  - c")



cleanEx()
nameEx("tinify")
### * tinify

flush(stderr()); flush(stdout())

### Name: tinify
### Title: Use the Tinify API to compress PNG and JPEG images
### Aliases: tinify tinify_dir

### ** Examples

## Not run: 
##D f = xfun:::R_logo("jpg$")
##D xfun::tinify(f)  # remember to set the API key before trying this
## End(Not run)



cleanEx()
nameEx("tojson")
### * tojson

flush(stderr()); flush(stdout())

### Name: tojson
### Title: A simple JSON serializer
### Aliases: tojson js json_vector

### ** Examples

library(xfun)
tojson(NULL)
tojson(1:10)
tojson(TRUE)
tojson(FALSE)
tojson(list(a = 1, b = list(c = 1:3, d = "abc")))
tojson(list(c("a", "b"), 1:5, TRUE, Sys.Date() + 1:3))
tojson(head(iris))  # each column is in an element
tojson(unname(head(iris)))  # each row is in an element
tojson(matrix(1:12, 3))

# literal JS code
tojson(list(a = 1:5, b = js("function() {return true;}")))



cleanEx()
nameEx("tree")
### * tree

flush(stderr()); flush(stdout())

### Name: tree
### Title: Turn the output of 'str()' into a tree diagram
### Aliases: tree

### ** Examples

fit = lsfit(1:9, 1:9)
str(fit)
xfun::tree(fit)

fit = lm(dist ~ speed, data = cars)
str(fit)
xfun::tree(fit)

# some trivial examples
xfun::tree(1:10)
xfun::tree(iris)



cleanEx()
nameEx("try_error")
### * try_error

flush(stderr()); flush(stdout())

### Name: try_error
### Title: Try an expression and see if it throws an error
### Aliases: try_error

### ** Examples

xfun::try_error(stop("foo"))  # TRUE
xfun::try_error(1:10)  # FALSE



cleanEx()
nameEx("try_silent")
### * try_silent

flush(stderr()); flush(stdout())

### Name: try_silent
### Title: Try to evaluate an expression silently
### Aliases: try_silent

### ** Examples

library(xfun)
z = try_silent(stop("Wrong!"))
inherits(z, "try-error")



cleanEx()
nameEx("upload_imgur")
### * upload_imgur

flush(stderr()); flush(stdout())

### Name: upload_imgur
### Title: Upload an image to imgur.com
### Aliases: upload_imgur

### ** Examples

## Not run: 
##D f = tempfile(fileext = ".png")
##D png(f)
##D plot(rnorm(100), main = R.version.string)
##D dev.off()
##D 
##D res = imgur_upload(f, include_xml = TRUE)
##D res  # link to original URL of the image
##D xfun::attr2(res, "XML")  # all information
##D if (interactive())
##D     browseURL(res)
##D 
##D # to use your own key
##D options(xfun.upload_imgur.key = "your imgur key")
## End(Not run)



cleanEx()
nameEx("url_accessible")
### * url_accessible

flush(stderr()); flush(stdout())

### Name: url_accessible
### Title: Test if a URL is accessible
### Aliases: url_accessible

### ** Examples

xfun::url_accessible("https://yihui.org")



cleanEx()
nameEx("url_destination")
### * url_destination

flush(stderr()); flush(stdout())

### Name: url_destination
### Title: Get the final destination of a URL
### Aliases: url_destination

### ** Examples

u = "https://tinytex.yihui.org"  # redirected to https://yihui.org/tinytex/
if (url_accessible(u)) url_destination(u)



cleanEx()
nameEx("url_filename")
### * url_filename

flush(stderr()); flush(stdout())

### Name: url_filename
### Title: Extract filenames from a URLs
### Aliases: url_filename

### ** Examples

xfun::url_filename("https://yihui.org/images/logo.png")
xfun::url_filename("https://yihui.org/index.html")
xfun::url_filename("https://yihui.org/index.html?foo=bar")
xfun::url_filename("https://yihui.org/index.html#about")
xfun::url_filename("https://yihui.org")
xfun::url_filename("https://yihui.org/")



cleanEx()
nameEx("valid_syntax")
### * valid_syntax

flush(stderr()); flush(stdout())

### Name: valid_syntax
### Title: Check if the syntax of the code is valid
### Aliases: valid_syntax

### ** Examples

xfun::valid_syntax("1+1")
xfun::valid_syntax("1+")
xfun::valid_syntax(c("if(T){1+1}", "else {2+2}"), silent = FALSE)



cleanEx()
nameEx("yaml_body")
### * yaml_body

flush(stderr()); flush(stdout())

### Name: yaml_body
### Title: Partition the YAML metadata and the body in a document
### Aliases: yaml_body

### ** Examples

xfun::yaml_body(c("---", "title: Hello", "output: litedown::html_format", "---",
    "", "Content."))



cleanEx()
nameEx("yaml_load")
### * yaml_load

flush(stderr()); flush(stdout())

### Name: yaml_load
### Title: Read YAML data
### Aliases: yaml_load

### ** Examples

yaml_load("a: 1")
yaml_load("a: 1", use_yaml = FALSE)



### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
