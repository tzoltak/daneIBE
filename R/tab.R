#' @title Tabela z rozkladem dla zmiennych etykietowanych
#' @description
#' Funkcja generuje rozkład jednej zmiennej - liczebności i częstości -
#' uwzględniając etykiety wartości.
#' @param x obiekt klasy \code{labelled}, wektor lub ramka danych
#' @param ... tylko jeśli \code{x} jest ramką danych - kolumna, której rozkład
#' ma zostać wygenerowany
#' @param procenty wartość logiczna - czy wyświetlić również rozkład częstości?
#' @param d liczba całkowita - liczba miejsc dziesiętnych, z jaką raportowane
#' będą procenty (argument jest ignowowany, jeśli \code{procenty} równe
#' \code{FALSE})
#' @param suma wartość logiczna - czy wyświetlić wiersz z sumą?
#' @param etykietaSuma ciąg znaków - etykieta dla wiersza z sumą (argument jest
#' ignorowany, jeśli \code{suma} równe \code{FALSE})
#' @return data.frame z rozkładami
#' @name tab
#' @export
tab = function(x, ..., procenty = TRUE, d = 1, suma = TRUE,
               etykietaSuma = "ŁĄCZNIE") {
  UseMethod("tab")
}
#' @rdname tab
#' @importFrom rlang ensyms
#' @export
tab.data.frame = function(x, ..., procenty = TRUE, d = 1, suma = TRUE,
               etykietaSuma = "ŁĄCZNIE") {
  zmienna = ensyms(...)
  stopifnot(length(zmienna) == 1,
            !!zmienna %in% names(x))
  tab.default(x[[zmienna[[1]]]])
}
#' @rdname tab
#' @importFrom stats setNames
#' @importFrom haven is.labelled
#' @export
tab.default = function(x, ..., procenty = TRUE, d = 1, suma = TRUE,
                       etykietaSuma = "ŁĄCZNIE") {
  stopifnot(is.numeric(x) | is.integer(x) | is.character(x) | is.labelled(x) | is.factor(x),
            is.logical(procenty), length(procenty) == 1,
            is.numeric(d), length(d) == 1,
            is.logical(suma), length(suma) == 1,
            is.character(etykietaSuma), length(etykietaSuma) == 1)
  stopifnot(procenty %in% c(TRUE, FALSE),
            as.integer(d) == d,
            suma %in% c(TRUE, FALSE))

  tab = table(x, exclude = NULL)
  tab = as.data.frame(tab, stringsAsFactors = FALSE)
  if (suma) {
    sum = data.frame(x = "", Freq = sum(tab$Freq))
  }
  if (procenty) {
    nazwyKolumn = c("wartość", "liczebność", "częstość")
    tab$pr = format(round(100 * tab$Freq / sum(tab$Freq), d), nsmall = d)
    tab$pr = paste0(tab$pr, "%")
    if (suma) {
      sum$pr = paste0(format(100, nsmall = d), "%")
    }
  } else {
    nazwyKolumn = c("wartość", "liczebność")
  }

  if ("labels" %in% names(attributes(x))) {
    nazwyKolumn = c(nazwyKolumn[1], "etykieta", nazwyKolumn[-1])
    tab = merge(data.frame("etykieta" = names(attributes(x)$labels),
                           x = unname(attributes(x)$labels),
                           stringsAsFactors = FALSE),
                tab, all = TRUE)
    tab$Freq[is.na(tab$Freq)] = 0
    tab$pr[is.na(tab$pr)] = paste0(format(0, nsmall = d), "%")
    tab = tab[order(as.numeric(tab$x)), ]
    tab$etykieta[is.na(tab$etykieta)] = ""
    # obejście ew. problemów z kodowaniem etykiet
    if (any(is.na(nchar(tab$etykieta, "chars", TRUE)))) {
      Encoding(tab$etykieta) = sub("^[^.]*[.]", "", Sys.getlocale("LC_COLLATE"))
    }
    tab$etykieta = format(tab$etykieta, width = max(nchar(tab$etykieta)))
    if (suma) {
      sum$etykieta = etykietaSuma
    }
  } else {
    sum$x = etykietaSuma
  }
  if (suma) {
    tab = rbind(tab, sum)
  }
  tab = setNames(tab, nazwyKolumn)
  if ("label" %in% names(attributes(x))) {
    attributes(tab)$label = attributes(x)$label
  }
  class(tab) = c("table_labeled", class(tab))
  return(tab)
}
#' @rdname tab
#' @export
print.table_labeled = function(x, ...) {
  if (!is.null(label(x))) {
    cat(label(x), "\n\n")
  }
  NextMethod(row.names = FALSE)
}
