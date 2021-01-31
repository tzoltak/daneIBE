# funkcja zaokrągla wartości wektora i sprawdza, czy ich suma jest równa
# analogicznie zaokrąglonej sumie wartości tego wektora
# jeśli nie, dokonuje drobynych zmian zaokrąglonych wartości, tak aby:
# 1) te same wartości przed zaokrągleniem miały te same wartości po zaokrągleniu
#    (o ile to możliwe, jeżeli nie, nie dokona zaokrągleń)
# 2) dokonane zmiany były jak najmniejsze
#' @importFrom utils combn
zaokraglij_do_sumy = function(x, d, ostatniSuma = FALSE) {
  stopifnot(is.numeric(x), !anyNA(x),
            is.numeric(d), length(d) == 1, as.integer(d) == d,
            ostatniSuma %in% c(FALSE, TRUE))
  if (ostatniSuma) {
    ostatni = round(x[length(x)], d)
    x = x[-length(x)]
  } else {
    ostatni = vector(mode = "numeric", length = 0L)
  }
  xr = round(x, d)
  r = sum(xr) - round(sum(x), d)
  if (r != 0) {
    k = combn(seq_along(x)[sign(xr - x) == sign(r)], abs(r))
    maska = apply(k, 2, function(m, x) {return(!(any(x[m] %in% x[-m])))}, x = x)
    if (any(maska)) {
      k = k[, maska, drop = FALSE]
      maska = apply(k, 2, function(m, r) {return(sum(r[m]))},
                    r = abs(x - xr))
      maska = k[, which.min(maska)]
      if (r > 0) {
        x[maska] = floor(x[maska])
      } else if (r < 0) {
        x[maska] = ceiling(x[maska])
      }
    }
  }
  x = round(x, d)

  return(c(x, ostatni))
}
