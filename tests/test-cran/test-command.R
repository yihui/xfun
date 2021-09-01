library(testit)

assert("Rscript() correctly calls Rscript with system2()", {
  (Rscript(c("-e", "1+1"), stdout = TRUE) %==% "[1] 2")
})

assert("Rscript_call() correctly calls Rscript()", {
  (Rscript_call(function() 1+1) %==% 2)
  loaded = Rscript_call(
    function() loadedNamespaces(), options = "--default-packages=xfun"
  )
  ("xfun" %in% loaded)
})
