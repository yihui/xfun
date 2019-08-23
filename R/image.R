# add a border to an image via ImageMagick
add_border = function(input, pixels = 1, color = 'black', output) {
  input = normalizePath(input)
  if (missing(output))
    output = paste0(sans_ext(input), '-output.', file_ext(input))
  system2('convert', shQuote(c(
    input, '-shave', paste(pixels, pixels, sep = 'x'), '-bordercolor', color,
    '-border', pixels, output)
  ))
  optipng(dirname(output))
}
