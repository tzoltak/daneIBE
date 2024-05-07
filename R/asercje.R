# nieeksportowane funkcje wykonujące asercje
#' @importFrom rlang as_name
assert_tab_df = function(x, zmienna) {
  if (length(zmienna) == 0) stop("Nie podano zmiennej.")
  if (length(zmienna) > 1) stop("Podano więcej niż jedną zmienną.")
  if (!(as_name(zmienna[[1]]) %in% colnames(x))) stop("Podanej zmiennej nie ma w podanej ramce danych.")
}
assert_tab_wektor = function(procenty, suma, etykietaSuma, etykietaBD) {
  stopifnot("Argument `procenty` musi być wartością logiczną." = all(procenty %in% c(TRUE, FALSE)),
            "Argument `procenty` musi być pojedynczą wartością." = length(procenty) == 1,
            "Argument `suma` musi być wartością logiczną." = all(suma %in% c(TRUE, FALSE)),
            "Argument `sumy` musi być pojedynczą wartością." = length(suma) == 1,
            "Argument `etykietaSuma` musi być ciągiem znaków." = is.character(etykietaSuma),
            "Argument `etykietaSuma` musi być wektorem jednoelementowym." = length(etykietaSuma) == 1)
}
#' @importFrom rlang as_name
assert_w = function(w, x) {
  if (is.symbol(w)) {
    if (!(as_name(w) %in% names(x))) {
      stop("Zmiennej podanej argumentem `w` nie ma w podanej ramce danych.")
    }
    return(x[[as_name(w)]])
  } else if (!is.null(w)) {
    stopifnot("Wektor podany argumentm `w` musi być tej samej długości, co wektor podany argumentem `x`." = length(w) == length(x),
              "Argument `w` musi być wektorem liczb." = is.numeric(w),
              "Wektor podany argumentem `w` nie może zawierać braków danych." = !anyNA(w),
              "Wszystkie elementy wektora podanego argumentem `w` muszą być nieujemne." = all(w >= 0),
              "Wszystkie elementy wektora podanego argumentem `w` muszą być skończone." = all(is.finite(w)))
    return(w)
  } else {
    return(rep(1, length(x)))
  }
}
assert_print_tab = function(dProcenty, dLiczby, decimal.mark) {
  stopifnot("Argument `dProcenty` musi być liczbą całkowitą." = is.numeric(dProcenty),
            "Argument `dProcenty` musi być pojedynczą wartością." = length(dProcenty) == 1,
            "Argument `dLiczby` musi być liczbą całkowitą." = is.numeric(dLiczby),
            "Argument `dLiczby` musi być pojedynczą wartością." = length(dLiczby) == 1,
            "Argument `decimal.mark` musi być ciągiem znaków." = is.character(decimal.mark),
            "Argument `decimal.mark` musi być wektorem jednoelementowym." = length(decimal.mark) == 1)
  stopifnot("Argument `dProcenty` musi być liczbą całkowitą." = as.integer(dProcenty) == dProcenty,
            "Argument `dProcenty` musi być liczbą nieujemną." = dProcenty >= 0,
            "Argument `dLiczby` musi być liczbą całkowitą." = as.integer(dLiczby) == dLiczby,
            "Argument `dLiczby` musi być liczbą nieujemną." = dLiczby >= 0)
}
#' @importFrom rlang as_name
assert_tab2 = function(names, zmW, zmK, liczby, procenty, etykietaSuma,
                       etykietaOgolem, etykietaBD) {
  if (!(as_name(zmW) %in% names)) stop("Zmiennej podanej argumentem `zmW` nie ma w podanym obiekcie (ramce danych).")
  if (!(as_name(zmK) %in% names)) stop("Zmiennej podanej argumentem `zmK` nie ma w podanym obiekcie (ramce danych).")
  stopifnot("Argument `liczby` musi być wartością logiczną." = all(liczby %in% c(TRUE, FALSE)),
            "Argument `liczby` musi być pojedynczą wartością." = length(liczby) == 1,
            "Argument `procenty` musi być wartością logiczną." = all(procenty %in% c(TRUE, FALSE)),
            "Argument `procenty` musi być pojedynczą wartością." = length(procenty) == 1,
            "Argument `etykietaSuma` musi być ciągiem znaków." = is.character(etykietaSuma),
            "Argument `etykietaSuma` musi być wektorem jednoelementowym." = length(etykietaSuma) == 1)
  if (!is.null(etykietaOgolem)) {
    stopifnot("Argument `etykietaOgolem` musi być wektorem jednoelementowym lub wartością NULL." = length(etykietaOgolem) == 1)
    if (is.na(etykietaOgolem)) {
      etykietaOgolem = NA_character_
    }
    stopifnot("Argument `etykietaOgolem` musi być ciagiem znaków lub wartością NULL." = is.character(etykietaOgolem))
  } else {
    etykietaOgolem = NA_character_
  }
  if (!is.null(etykietaBD)) {
    stopifnot("Argument `etykietaBD` musi być wektorem jednoelementowym lub wartością NULL." = length(etykietaBD) == 1)
    if (is.na(etykietaBD)) {
      etykietaBD = NA_character_
    }
    stopifnot("Argument `etykietaBD` musi być ciagiem znaków lub wartością NULL." = is.character(etykietaBD))
  } else {
    etykietaBD = NA_character_
  }
  return(list(etykietaOgolem = etykietaOgolem, etykietaBD = etykietaBD))
}
