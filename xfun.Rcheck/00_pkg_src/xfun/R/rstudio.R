#' Type a character vector into the RStudio source editor
#'
#' Use the \pkg{rstudioapi} package to insert characters one by one into the
#' RStudio source editor, as if they were typed by a human.
#' @param x A character vector.
#' @param pause A function to return a number in seconds to pause after typing
#'   each character.
#' @param mistake The probability of making random mistakes when typing the next
#'   character. A random mistake is a random string typed into the editor and
#'   deleted immediately.
#' @param save The probability of saving the document after typing each
#'   character. Note that If a document is not opened from a file, it will never
#'   be saved.
#' @export
#' @import stats
#' @examples library(xfun)
#' if (loadable('rstudioapi') && rstudioapi::isAvailable()) {
#'   rstudio_type('Hello, RStudio! xfun::rstudio_type() looks pretty cool!',
#'     pause = function() runif(1, 0, .5), mistake = .1)
#' }
rstudio_type = function(x, pause = function() .1, mistake = 0, save = 0) {
  get_ctx = function() rstudioapi::getSourceEditorContext()
  ctx = get_ctx()
  if (is.null(id <- ctx$id)) {
    message('Please make sure an RStudio editor tab is open')
    return()
  }

  save_it = function(prob = 1) {
    if (ctx$path == '' || (rbinom(1, 1, prob) == 0)) return()
    ctx = get_ctx()
    # in case a new line is automatically added at the end when saving the doc
    on.exit(rstudioapi::setSelectionRanges(ctx$selection[[1]]$range, id), add = TRUE)
    rstudioapi::documentSave(id)
  }

  type_one = function(x) {
    rstudioapi::insertText(text = x, id = id)
    Sys.sleep(pause())
  }

  type_mistake = function() {
    n = sample(1:10, 1)
    x = sample(ascii_chars, n, replace = TRUE)
    for (i in x) type_one(i)
    Sys.sleep(.5)
    ctx = rstudioapi::getSourceEditorContext()
    r = ctx$selection[[1]]$range
    r$start[2] = r$start[2] - n
    rstudioapi::modifyRange(r, '', id)
    Sys.sleep(.5)
  }

  x = paste(x, collapse = '\n')
  for (i in unlist(strsplit(x, ''))) {
    type_one(i); save_it(save)
    if (runif(1) < mistake) type_mistake()
  }

  save_it(as.integer(save > 0))  # if prob is non-zero, save it finally

  invisible()
}

ascii_chars = intToUtf8(32:126, TRUE)

