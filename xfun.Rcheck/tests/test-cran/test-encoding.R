library(testit)

assert('native_encode() warns against characters that cannot be represented in native encoding', {
  latin_str = 'fa\u00e7ile'  # Latin-1
  cn_str = '\u4e2d\u6587\u5b57\u7b26'  # UTF-8 Chinese
  # when the locale is not UTF-8, the above strings cannot be converted to native encoding
  (isTRUE(l10n_info()[['UTF-8']]) ||
      has_warning({native_encode(cn_str); native_encode(latin_str)}))

  gb2312_raw = as.raw(c(0xd6, 0xd0, 0xce, 0xc4, 0xd7, 0xd6, 0xb7, 0xfb))
  is_gb2312_native = identical(gb2312_raw, charToRaw(enc2native(cn_str)))
  no_need_test = !(is_windows() && is_gb2312_native)
  cn_str_native = native_encode(cn_str)
  (no_need_test || Encoding(cn_str_native) %==% 'unknown')
  (no_need_test || charToRaw(cn_str_native) %==% gb2312_raw)
})

assert('is_ascii() can identify ascii strings', {
  ascii_str = c('aaa', 'bbb', 'ccc')
  latin_str = 'fa\u00e7ile'
  cn_str = '\u4e2d\u6587\u5b57\u7b26'
  mixed_str = c(ascii_str, latin_str)
  (is_ascii(ascii_str) %==% c(TRUE, TRUE, TRUE))
  (!is_ascii(latin_str))
  (!is_ascii(cn_str))
  (is_ascii(mixed_str) %==% c(TRUE, TRUE, TRUE, FALSE))
  (is_ascii(c(NA_character_, 'a')) %==% c(NA, TRUE))
})
