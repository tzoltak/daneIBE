#' @rdname tab2
#' @inheritParams print.tab_lbl
#' @export
print.tab_lbl2 = function(x, dProcenty = 1, dLiczby = 0, decimal.mark = ",",
                          scipen = 100, ...) {
  assert_print_tab(dProcenty, dLiczby, decimal.mark)

  cat(label(x), "\n\n", sep = "")
  optScipen = options()$scipen
  on.exit(options(scipen = optScipen))
  options(scipen = scipen)
  if (attributes(x)$sumowanie %in% "kolumny") {
    for (i in grep("^n_", names(x))) {
      x[, i] = zaokraglij_do_sumy(x[, i], dLiczby, ostatniSuma = TRUE)
    }
    for (i in grep("^pct_", names(x))) {
      x[, i] = zaokraglij_do_sumy(x[, i], dProcenty, ostatniSuma = TRUE)
    }
  } else if (attributes(x)$sumowanie %in% "wiersze") {
    for (i in 1:nrow(x)) {
      kolumnyN = grep("^n_", names(x))
      kolumnyPct = grep("^pct_", names(x))
      x[i, kolumnyN] = zaokraglij_do_sumy(unlist(x[i, kolumnyN]),
                                          dLiczby, ostatniSuma = TRUE)
      x[i, kolumnyPct] = zaokraglij_do_sumy(unlist(x[i, kolumnyPct]),
                                            dProcenty, ostatniSuma = TRUE)
    }
  }
  for (i in grep("^n_", names(x))) {
    x[, i] = format(round(x[, i], dLiczby), nsmall = dLiczby,
                    decimal.mark = decimal.mark, ...)
  }
  for (i in grep("^pct_", names(x))) {
    x[, i] = format(round(x[, i], dProcenty), nsmall = dProcenty,
                    decimal.mark = decimal.mark, ...)
  }
  print.data.frame(x, row.names = FALSE)
}
#' @rdname labels
#' @export
label.tab_lbl2 = function(object, ...) {
  labels = lapply(labels(object),
                  function(x) {return(ifelse(x == "", names(x), x))})
  if (attributes(object)$sumowanie %in% "wiersze") {
    return(paste(labels$w, "x", labels$k))
  } else {
    return(paste(labels$k, "x", labels$w))
  }
}
#' @rdname labels
#' @export
labels.tab_lbl2 = function(object, ...) {
  labelW = ifelse(is.null(attributes(object)$label$w),
                  "", attributes(object)$label$w)
  labelK = ifelse(is.null(attributes(object)$label$k),
                  "", attributes(object)$label$k)
  return(list(w = setNames(labelW, attributes(object)$nazwyZm$w),
              k = setNames(labelK, attributes(object)$nazwyZm$k)))
}
#' @rdname tab2
#' @param usunSuma wartość logiczna - czy usunąć ze zwróconej ramki danych
#' wiersze opisujące sumy rozkładów?
#' @param usunOgolem wartość logiczna - czy usunąć ze zwróconej ramki danych
#' wiersze opisujące rozkład brzegowy zmiennej zależnej?
#' @param wartoscBD wektor jedno- lub dwuelementowy: wartości, przy pomocy
#' których mają być reprezentowane ew. braki danych w zróconej ramce danych;
#' jeśli zostanie podany tylko jeden element, zostanie użyty zarówno
#' w odniesieniu do zmiennej prezentowanej w wierszach, jak i zmiennej
#' prezentowanej w kolumnach; jeśli zostaną podane dwa elementy, pierwszy
#' zostanie zastosowany do zmiennej prezentowanej w wierszach, a drugi do
#' zmiennej prezentowanej w kolumnach
#' @param .rows wyłącznie dla zgodności ze wzorcem (\emph{generic}) metody
#' \code{as_tibble}
#' @param .name_repair przekazywana jako argument \code{names_repair} do
#' \code{\link[tidyr]{pivot_longer}}
#' @param rownames wyłącznie dla zgodności ze wzorcem (\emph{generic}) metody
#' \code{as_tibble}
#' @method as_tibble tab_lbl2
#' @importFrom tidyr pivot_longer
#' @export
as_tibble.tab_lbl2 = function(x, ..., .rows = NULL,
                              .name_repair = c("check_unique", "unique",
                                               "universal", "minimal"),
                              rownames = NULL, usunSuma = FALSE,
                              usunOgolem = FALSE, wartoscBD = NULL) {
  stopifnot(is.logical(usunSuma), length(usunSuma) == 1,
            is.logical(usunOgolem), length(usunOgolem) == 1)
  stopifnot(usunSuma %in% c(TRUE, FALSE),
            usunOgolem %in% c(TRUE, FALSE))
  if (!is.null(wartoscBD)) {
    stopifnot(is.vector(wartoscBD), length(wartoscBD) %in% 1:2)
    if (length(wartoscBD) == 1) wartoscBD = rep(wartoscBD, 2)
    wartoscBD = list(w = wartoscBD[1],
                     k = wartoscBD[2])
  } else {
    wartoscBD = list(w = NULL,
                     k = NULL)
  }
  atrybuty = attributes(x)

  x = pivot_longer(as.data.frame(unclass(x), check.names = FALSE),
                   -1, names_to = c("rozklad", attributes(x)$nazwyZm$k),
                   names_pattern = "^(n|pct)_(.*)$",
                   names_repair = .name_repair)
  # ew. usuwanie wierszy sumy i rozkładu brzegowego zm. zal.
  if (usunSuma & !is.na(atrybuty$etykietaSuma)) {
    x = x[!(x[[atrybuty$nazwyZm$w]] %in% atrybuty$etykietaSuma), ]
    x = x[!(x[[atrybuty$nazwyZm$k]] %in% atrybuty$etykietaSuma), ]
  }
  if (usunOgolem & !is.na(atrybuty$etykietaOgolem)) {
    if (atrybuty$sumowanie %in% "kolumny") {
      x = x[!(x[[atrybuty$nazwyZm$k]] %in% atrybuty$etykietaOgolem), ]
    } else if (atrybuty$sumowanie %in% "wiersze") {
      x = x[!(x[[atrybuty$nazwyZm$w]] %in% atrybuty$etykietaOgolem), ]
    }
  }
  # ew. zamiana wartości braków danych
  if (!is.null(wartoscBD$w)) {
    if (is.factor(x[[atrybuty$nazwyZm$w]])) {
      x[[atrybuty$nazwyZm$w]] =
        levels(x[[atrybuty$nazwyZm$w]])[x[[atrybuty$nazwyZm$w]]]
    }
    x[[atrybuty$nazwyZm$w]] =
      ifelse(x[[atrybuty$nazwyZm$w]] %in% atrybuty$etykietaBD,
             wartoscBD$w,
             x[[atrybuty$nazwyZm$w]])
  }
  if (!is.null(wartoscBD$k)) {
    x[[atrybuty$nazwyZm$k]] =
      ifelse(x[[atrybuty$nazwyZm$k]] %in% as.character(as.name(atrybuty$etykietaBD)),
             wartoscBD$k,
             x[[atrybuty$nazwyZm$k]])
  } else {
    x[[atrybuty$nazwyZm$k]] =
      ifelse(x[[atrybuty$nazwyZm$k]] %in% as.character(as.name(atrybuty$etykietaBD)),
             atrybuty$etykietaBD,
             x[[atrybuty$nazwyZm$k]])
  }
  # konwersja typów kolumn
  if (any(atrybuty$klasyZm$w %in% c("integer", "numeric")) &
      ((atrybuty$sumowanie %in% "ogółem" & (is.na(atrybuty$etykietaSuma) | usunSuma)) |
       (atrybuty$sumowanie %in% "kolumny" & (is.na(atrybuty$etykietaSuma) | usunSuma)) |
       (atrybuty$sumowanie %in% "wiersze" & (is.na(atrybuty$etykietaOgolem) | usunOgolem)))) {
    # w przypadku zmiennej prezentowanej w wierszach trzeba obsłużyć czynniki
    if (is.factor(x[[atrybuty$nazwyZm$w]])) {
      temp = levels(x[[atrybuty$nazwyZm$w]])[unique(x[[atrybuty$nazwyZm$w]])]
    } else {
      temp = unique(x[[atrybuty$nazwyZm$w]])
    }
    temp = temp[!is.na(temp)]
    temp = suppressWarnings(
      do.call(paste0("as.", atrybuty$klasyZm$w[length(atrybuty$klasyZm$w)]),
              list(x = temp)))
    if (!any(is.na(temp))) {
      if (is.factor(x[[atrybuty$nazwyZm$w]])) {
        x[[atrybuty$nazwyZm$w]] =
          levels(x[[atrybuty$nazwyZm$w]])[x[[atrybuty$nazwyZm$w]]]
      }
      x[[atrybuty$nazwyZm$w]] =
        do.call(paste0("as.", atrybuty$klasyZm$w[length(atrybuty$klasyZm$w)]),
                list(x = x[[atrybuty$nazwyZm$w]]))
    }
  } else {
    x[[atrybuty$nazwyZm$w]] = factor(x[[atrybuty$nazwyZm$w]],
                                     unique(x[[atrybuty$nazwyZm$w]]))
    x[[atrybuty$nazwyZm$w]] = addNA(x[[atrybuty$nazwyZm$w]])
  }
  if (any(atrybuty$klasyZm$k %in% c("integer", "numeric")) &
      ((atrybuty$sumowanie %in% "ogółem" & (is.na(atrybuty$etykietaSuma) | usunSuma)) |
       (atrybuty$sumowanie %in% "kolumny" & (is.na(atrybuty$etykietaOgolem) | usunOgolem)) |
       (atrybuty$sumowanie %in% "wiersze" & (is.na(atrybuty$etykietaSuma) | usunSuma)))) {
    # wiadomo, że zmienna prezentowana w kolumnach nie jest czynnikiem
    temp = unique(x[[atrybuty$nazwyZm$k]])
    temp = temp[!is.na(temp)]
    temp = suppressWarnings(
      do.call(paste0("as.", atrybuty$klasyZm$k[length(atrybuty$klasyZm$k)]),
              list(x = temp)))
    if (!any(is.na(temp))) {
      x[[atrybuty$nazwyZm$k]] =
        do.call(paste0("as.", atrybuty$klasyZm$k[length(atrybuty$klasyZm$k)]),
                list(x = x[[atrybuty$nazwyZm$k]]))
    }
  } else {
    x[[atrybuty$nazwyZm$k]] = factor(x[[atrybuty$nazwyZm$k]],
                                     unique(x[[atrybuty$nazwyZm$k]]))
    x[[atrybuty$nazwyZm$k]] = addNA(x[[atrybuty$nazwyZm$k]])
  }

  # zmiana kolejności wierszy i kolumn adekwatnie do kierunku sumowania
  if (atrybuty$sumowanie %in% "kolumny") {
    kolejnoscKolumn = c(atrybuty$nazwyZm$k, atrybuty$nazwyZm$w)
    x = x[order(x$rozklad, x[[atrybuty$nazwyZm$k]]), ]
  } else {
    kolejnoscKolumn = c(atrybuty$nazwyZm$w, atrybuty$nazwyZm$k)
    x = x[order(x$rozklad, x[[atrybuty$nazwyZm$w]]), ]
  }
  x = x[, c("rozklad", kolejnoscKolumn, "value")]
  attributes(x)$label = atrybuty$label
  return(x)
}
#' @rdname tab2
#' @param row.names wyłącznie dla zgodności ze wzorcem (\emph{generic}) metody
#' \code{as.data.frame}
#' @param optional wyłącznie dla zgodności ze wzorcem (\emph{generic}) metody
#' \code{as.data.frame}
#' @param niePrzeksztalcaj wartość logiczna - czy funkcja ma tylko usunąć
#' z obiektu klasę \code{tab_lbl2} i zwrócić go jako \emph{zwykłą} ramkę danych,
#' \strong{bez} dokonywania przekształcenia z postaci \emph{szerokiej} do
#' \emph{długiej})? (domyślnie funkcja dokonuje przekształcenia)
#' @method as.data.frame tab_lbl2
#' @importFrom tidyr as_tibble
#' @export
as.data.frame.tab_lbl2 = function(x, row.names = NULL, optional = FALSE,
                                  ..., usunSuma = FALSE, usunOgolem = FALSE,
                                  wartoscBD = NULL, niePrzeksztalcaj = TRUE) {
  stopifnot(is.logical(niePrzeksztalcaj), length(niePrzeksztalcaj) == 1)
  stopifnot(niePrzeksztalcaj %in% c(TRUE, FALSE))
  if (niePrzeksztalcaj) {
    class(x) = "data.frame"
    return(x)
  } else {
    return(as.data.frame(as_tibble(x, usunSuma = usunSuma,
                                   usunOgolem = usunOgolem,
                                   wartoscBD = wartoscBD,
                                   niePrzeksztalcaj = niePrzeksztalcaj)))
  }
}
#' @export
`[.tab_lbl2` = function(x, i, j, drop = TRUE) {
  temp = attributes(x)
  x = `[.data.frame`(x, i, j, drop)
  attributes(x)$nazwyZm = temp$nazwyZm
  attributes(x)$klasyZm = temp$klasyZm
  attributes(x)$label = temp$label
  attributes(x)$sumowanie = temp$sumowanie
  attributes(x)$etykietaSuma = temp$etykietaSuma
  attributes(x)$etykietaOgolem = temp$etykietaOgolem
  attributes(x)$etykietaBD = temp$etykietaBD
  return(x)
}
#' @export
`[<-.tab_lbl2` = function(x, i, j, value) {
  temp = attributes(x)
  x = `[<-.data.frame`(x, i, j, value)
  attributes(x)$nazwyZm = temp$nazwyZm
  attributes(x)$klasyZm = temp$klasyZm
  attributes(x)$label = temp$label
  attributes(x)$sumowanie = temp$sumowanie
  attributes(x)$etykietaSuma = temp$etykietaSuma
  attributes(x)$etykietaOgolem = temp$etykietaOgolem
  attributes(x)$etykietaBD = temp$etykietaBD
  return(x)
}
