# nieeksportowane funkcje wykonujące asercje
#' @importFrom rlang as_name
assert_tab_df = function(x, zmienna) {
  if (length(zmienna) == 0) stop("Nie podano zmiennej.")
  if (length(zmienna) > 1) stop("Podano więcej niż jedną zmienną.")
  if (!(as_name(zmienna[[1]]) %in% colnames(x))) stop("Podanej zmiennej nie ma w podanej ramce danych.")
}
assert_tab_wektor = function(procenty, suma, etykietaSuma, etykietaBD) {
  stopifnot(is.logical(procenty), length(procenty) == 1,
            is.logical(suma), length(suma) == 1,
            is.character(etykietaSuma), length(etykietaSuma) == 1)
  stopifnot(procenty %in% c(TRUE, FALSE),
            suma %in% c(TRUE, FALSE))
}
#' @importFrom rlang as_name
assert_w = function(w, x) {
  if (is.symbol(w)) {
    if (!(as_name(w) %in% names(x))) {
      stop("Zmiennej podanej argumentem `w` nie ma w podanej ramce danych.")
    }
    return(x[[as_name(w)]])
  } else if (!is.null(w)) {
    stopifnot(length(w) == length(x),
              is.numeric(w),
              all(w >= 0),
              all(is.finite(w)))
    return(w)
  } else {
    return(rep(1, length(x)))
  }
}
assert_print_tab = function(dProcenty, dLiczby, decimal.mark) {
  stopifnot(is.numeric(dProcenty), length(dProcenty) == 1,
            is.numeric(dLiczby), length(dLiczby) == 1,
            is.character(decimal.mark), length(decimal.mark) == 1)
  stopifnot(as.integer(dProcenty) == dProcenty, dProcenty >= 0,
            as.integer(dLiczby) == dLiczby, dLiczby >= 0)
}
#' @importFrom rlang as_name
assert_tab2 = function(names, zmW, zmK, liczby, procenty, etykietaSuma,
                       etykietaOgolem, etykietaBD) {
  if (!(as_name(zmW) %in% names)) stop("Zmiennej podanej argumentem `zmW` nie ma w podanym obiekcie (ramce danych).")
  if (!(as_name(zmK) %in% names)) stop("Zmiennej podanej argumentem `zmK` nie ma w podanym obiekcie (ramce danych).")
  stopifnot(is.logical(liczby), length(liczby) == 1,
            is.logical(procenty), length(procenty) == 1,
            is.character(etykietaSuma), length(etykietaSuma) == 1)
  stopifnot(liczby %in% c(TRUE, FALSE),
            procenty %in% c(TRUE, FALSE))
  if (!is.null(etykietaOgolem)) {
    stopifnot(length(etykietaOgolem) == 1)
    if (is.na(etykietaOgolem)) {
      etykietaOgolem = NA_character_
    }
    stopifnot(is.character(etykietaOgolem))
  } else {
    etykietaOgolem = NA_character_
  }
  if (!is.null(etykietaBD)) {
    stopifnot(length(etykietaBD) == 1)
    if (is.na(etykietaBD)) {
      etykietaBD = NA_character_
    }
    stopifnot(is.character(etykietaBD))
  } else {
    etykietaBD = NA_character_
  }
  return(list(etykietaOgolem = etykietaOgolem, etykietaBD = etykietaBD))
}
