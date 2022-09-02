#' @rdname tab_n
#' @inheritParams print.tab_lbl
#' @export
print.tab_lbl_n = function(x, dProcenty = 1, dLiczby = 0, decimal.mark = ",",
                           scipen = 100, ...) {
  assert_print_tab(dProcenty, dLiczby, decimal.mark)

  cat(label(x), "\n\n", sep = "")
  optScipen = options()$scipen
  on.exit(options(scipen = optScipen))
  options(scipen = scipen)
  if (attributes(x)$kierunek %in% "kolumny") {
    for (i in grep("^n_", names(x))) {
      x[, i] = zaokraglij_do_sumy(x[, i], dLiczby, ostatniSuma = TRUE)
    }
    for (i in grep("^pct_", names(x))) {
      x[, i] = zaokraglij_do_sumy(x[, i], dProcenty, ostatniSuma = TRUE)
    }
  } else if (attributes(x)$kierunek %in% "wiersze") {
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
label.tab_lbl_n = function(object, ...) {
  labels = labels(object)
  return(paste0(names(labels), ifelse(names(labels) != "", ": ", ""),
                labels, collapse = "\n"))
}
#' @rdname labels
#' @export
labels.tab_lbl_n = function(object, ...) {
  labels = sapply(attributes(object)$label,
                  function(x) {return(ifelse(is.null(x), "", x))},
                  USE.NAMES = FALSE)
  return(labels)
}

#' @rdname tab_n
#' @param usunSuma wartość logiczna - czy usunąć ze zwróconej ramki danych
#' wiersze opisujące sumy rozkładów?
#' @param wartoscBD wektor jednoelementowy: wartość, przy pomocy
#' których mają być reprezentowane ew. braki danych w zróconej ramce danych
#' @param .rows wyłącznie dla zgodności ze wzorcem (\emph{generic}) metody
#' \code{as_tibble}
#' @param .name_repair przekazywana jako argument \code{names_repair} do
#' \code{\link[tidyr]{pivot_longer}}
#' @param rownames wyłącznie dla zgodności ze wzorcem (\emph{generic}) metody
#' \code{as_tibble}
#' @method as_tibble tab_lbl_n
#' @importFrom tidyr pivot_longer
#' @export
as_tibble.tab_lbl_n = function(x, ..., .rows = NULL,
                               .name_repair = c("check_unique", "unique",
                                                "universal", "minimal"),
                               rownames = NULL, usunSuma = FALSE,
                               wartoscBD = NULL) {
  stopifnot(is.logical(usunSuma), length(usunSuma) == 1)
  stopifnot(usunSuma %in% c(TRUE, FALSE))
  if (!is.null(wartoscBD)) {
    stopifnot(is.vector(wartoscBD), length(wartoscBD) == 1)
  }
  atrybuty = attributes(x)

  # obsługa obiektów z rozkładami wielokrotnych odpowiedzi
  if (!is.null(atrybuty$etykietaOgolem)) {
    zm = "pozycja"
    if (atrybuty$kierunek %in% "kolumny") {
      atrybuty$kierunek = "wiersze"
    } else {
      atrybuty$kierunek = "kolumny"
    }
  } else {
    zm = "zmienna"
  }
  # do formy długiej
  x = pivot_longer(as.data.frame(unclass(x), check.names = FALSE),
                   -1,
                   names_to = c("rozklad",
                                ifelse(atrybuty$kierunek %in% "kolumny",
                                       zm, "wartosc")),
                   names_pattern = "^(n|pct)_(.*)$",
                   names_repair = .name_repair)
  # ew. usuwanie wierszy sumy
  if (usunSuma & !is.na(atrybuty$etykietaSuma)) {
    x = x[!(x$wartosc %in% atrybuty$etykietaSuma), ]
  }
  # ew. konwersja kolumny wartosci na factor
  if (!is.factor(x$wartosc)) {
    wartosci = unique(x$wartosc)
    if (atrybuty$kierunek %in% "wiersze" &
        is.na(atrybuty$etykietaBD)) {
      wartosci = setdiff(wartosci, "NA")
    }
    x$wartosc = factor(x$wartosc, wartosci)
  } else {
    wartosci = levels(x$wartosc)
  }
  # ew. zamiana wartości braków danych
  if (!is.null(wartoscBD)) {
    x$wartosc = wartosci[x$wartosc]
    x$wartosc =
      ifelse(x$wartosc %in% atrybuty$etykietaBD, wartoscBD, x$wartosc)
    x$wartosc = factor(x$wartosc, c(setdiff(wartosci, atrybuty$etykietaSuma),
                                    wartoscBD, atrybuty$etykietaSuma))
  }

  # zmiana kolejności wierszy i kolumn
  if (!is.factor(x[[zm]])) {
    x[[zm]] = factor(x[[zm]], unique(x[[zm]]))
  }
  x = x[order(x$rozklad, x[[zm]]), ]
  x = x[, c("rozklad", zm, "wartosc", "value")]
  attributes(x)$label = atrybuty$label
  return(x)
}
#' @rdname tab_n
#' @param row.names wyłącznie dla zgodności ze wzorcem (\emph{generic}) metody
#' \code{as.data.frame}
#' @param optional wyłącznie dla zgodności ze wzorcem (\emph{generic}) metody
#' \code{as.data.frame}
#' @param niePrzeksztalcaj wartość logiczna - czy funkcja ma tylko usunąć
#' z obiektu klasę \code{tab_lbl_n} i zwrócić go jako \emph{zwykłą} ramkę danych,
#' \strong{bez} dokonywania przekształcenia z postaci \emph{szerokiej} do
#' \emph{długiej})? (domyślnie funkcja dokonuje przekształcenia)
#' @method as.data.frame tab_lbl_n
#' @importFrom tidyr as_tibble
#' @export
as.data.frame.tab_lbl_n = function(x, row.names = NULL, optional = FALSE,
                                   ..., usunSuma = FALSE, wartoscBD = NULL,
                                   niePrzeksztalcaj = TRUE) {
  stopifnot(is.logical(niePrzeksztalcaj), length(niePrzeksztalcaj) == 1)
  stopifnot(niePrzeksztalcaj %in% c(TRUE, FALSE))
  if (niePrzeksztalcaj) {
    class(x) = "data.frame"
    return(x)
  } else {
    return(as.data.frame(as_tibble(x, usunSuma = usunSuma,
                                   wartoscBD = wartoscBD,
                                   niePrzeksztalcaj = niePrzeksztalcaj)))
  }
}
#' @export
`[.tab_lbl_n` = function(x, i, j, drop = TRUE) {
  temp = attributes(x)
  x = `[.data.frame`(x, i, j, drop)
  attributes(x)$nazwyZm = temp$nazwyZm
  attributes(x)$klasyZm = temp$klasyZm
  attributes(x)$label = temp$label
  attributes(x)$kierunek = temp$kierunek
  attributes(x)$etykietaSuma = temp$etykietaSuma
  attributes(x)$etykietaBD = temp$etykietaBD
  attributes(x)$etykietaOgolem = temp$etykietaOgolem
  return(x)
}
#' @export
`[<-.tab_lbl_n` = function(x, i, j, value) {
  temp = attributes(x)
  x = `[<-.data.frame`(x, i, j, value)
  attributes(x)$nazwyZm = temp$nazwyZm
  attributes(x)$klasyZm = temp$klasyZm
  attributes(x)$label = temp$label
  attributes(x)$kierunek = temp$kierunek
  attributes(x)$etykietaSuma = temp$etykietaSuma
  attributes(x)$etykietaBD = temp$etykietaBD
  attributes(x)$etykietaOgolem = temp$etykietaOgolem
  return(x)
}
