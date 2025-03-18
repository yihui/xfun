#' Pseudo-random numbers on `[0, 1)` based on a linear congruential generator
#'
#' Generate pseudo-random numbers from \eqn{X_{i+1} = (aX_i + c) \bmod{m}}{X[i +
#' 1] = (a * X[i] + c) %% m}, where \eqn{X_1}{X[1]} is the initial value (seed).
#' @param n The desired length of the sequence.
#' @param a,c,m Parameters for the generator (see References for the default
#'   values).
#' @param seed The seed. The default is the system time when this function is
#'   called for the first time in the current session. For subsequent calls, the
#'   last \eqn{X_i}{X[i]} of the previous call will be used as the default seed.
#' @return Random numbers on `[0, 1)` (i.e., \eqn{X/m} instead of \eqn{X}). Note
#'   the unit interval is open on the right (excluding 1).
#' @note All argument values must be smaller than \eqn{2^{64}}{2^64} as they
#'   will be coerced to 64-bit integers.
#' @references Steele, Guy L. Jr.; Vigna, Sebastiano (2022). "Computationally
#'   easy, spectrally good multipliers for congruential pseudorandom number
#'   generators". _Software: Practice and Experience._ 52 (2): 443–458.
#' @export
#' @examples
#' rand_unit(10)
#' rand_unit(10, seed = 0)
#' rand_unit(10, seed = 0)  # identical results
#' rand_unit(10, seed = Sys.getpid())
rand_unit = local({
  .seed = NULL
  function(n = 1, a = 0xd9f5, c = 0, m = 4294967296, seed = NULL) {
    .seed <<- seed %||% .seed %||% as.numeric(Sys.time())
    x = .Call('rand_lcg', n, .seed, a, c, m, PACKAGE = 'xfun')
    if (n > 0) .seed <<- x[n]
    x / m
  }
})

# n random integers from 1 to M
rand_n = function(n, M) floor(rand_unit(n) * M) + 1
