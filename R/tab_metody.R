#' @rdname tab
#' @param dProcenty liczba miejsc dziesiętnych, do jakiej zostaną zaokrąglone
#' wartości kolumn z rozkładami częstości
#' @param dLiczby liczba miejsc dziesiętnych, do jakiej zostaną zaokrąglone
#' wartości kolumn z rozkładami liczebności (to mogą być liczby niecałkowite,
#' jeśli przy tworzeniu rozkładu stosowano ważenie)
#' @param decimal.mark znak miejsca dziesiętnego - przekazywany do
#' \code{\link[base]{format}}
#' @param scipen liczba całkowita decydująca o skłonności R do zapisywania liczb
#' w notacji naukowej (czym większa, tym rzadziej R sosuje notację naukową - p.
#' \code{\link[base]{options}})
#' @importFrom utils hasName
#' @export
print.tab_lbl = function(x, dProcenty = 1, dLiczby = 0, decimal.mark = ",",
                         scipen = 100, ...) {
  assert_print_tab(dProcenty, dLiczby, decimal.mark)

  if (hasName(x, "etykieta")) {
    if (hasName(attributes(x), "suma")) {
      if (attributes(x)$suma) {
        dl = max(nchar(x$etykieta))
        x$etykieta =
          c(format(x$etykieta[-nrow(x)], width = dl, justify = "left"),
            format(x$etykieta[nrow(x)], width = dl, justify = "right"))
      }
    }
  }
  optScipen = options()$scipen
  on.exit(options(scipen = optScipen))
  options(scipen = scipen)
  if (hasName(attributes(x), "suma")) {
    if (attributes(x)$suma) {
      dl = max(nchar(x$`wartość`))
      if (
        all(!is.na(suppressWarnings(
          as.numeric(setdiff(x$`wartość`,
                             attributes(x)$etykietaSuma)))))) {
        x$`wartość` = format(x$`wartość`, width = dl, justify = "right")
      } else {
        x$`wartość` =
          c(format(x$`wartość`[-nrow(x)], width = dl, justify = "left"),
            format(x$`wartość`[nrow(x)], width = dl, justify = "right"))
      }
      x$`liczebność` =
        format(zaokraglij_do_sumy(x$`liczebność`, dLiczby, ostatniSuma = TRUE),
               digits = 0, nsmall = dLiczby, decimal.mark = decimal.mark, ...)
      if (hasName(x, "częstość")) {
        x$`częstość` =
          format(zaokraglij_do_sumy(x$`częstość`, dProcenty, ostatniSuma = TRUE),
                 digits = 0, nsmall = dProcenty, decimal.mark = decimal.mark,
                 ...)
      }
    }
  }
  if (!is.character(x$`liczebność`)) { # gdy nie wiadomo, czy ma wiersz sumy
    x$`liczebność` = format(round(x$`liczebność`, dLiczby), digits = 0,
                            nsmall = dLiczby, decimal.mark = decimal.mark, ...)
    x$`częstość` = format(round(x$`częstość`, dProcenty), digits = 0,
                          nsmall = dProcenty, decimal.mark = decimal.mark, ...)
  }
  if (!is.null(label(x))) {
    cat(label(x), "\n\n")
  }
  NextMethod(row.names = FALSE)
}
#' @rdname labels
#' @importFrom utils hasName
#' @export
labels.tab_lbl = function(object, ...) {
  if (hasName(object, "etykieta")) {
    return(setdiff(object$etykieta, attributes(object)$etykietaSuma))
  } else {
    return(NULL)
  }
}
