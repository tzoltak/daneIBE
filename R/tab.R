#' @title Tabela z rozkladem dla zmiennych etykietowanych
#' @description
#' Funkcja generuje rozkład jednej zmiennej - liczebności i częstości -
#' uwzględniając etykiety wartości.
#' @param x obiekt klasy \code{labelled}, wektor, ramka danych lub obiekt
#' klasy \code{tbl_svy}
#' @param ... tylko jeśli \code{x} jest \emph{ramką danych} - kolumna, której
#' rozkład ma zostać wygenerowany
#' @param procenty wartość logiczna - czy wyświetlić również rozkład częstości?
#' @param d liczba całkowita - liczba miejsc dziesiętnych, z jaką raportowane
#' będą procenty (argument jest ignowowany, jeśli \code{procenty} równe
#' \code{FALSE})
#' @param suma wartość logiczna - czy wyświetlić wiersz z sumą?
#' @param etykietaSuma ciąg znaków - etykieta dla wiersza z sumą (argument jest
#' ignorowany, jeśli \code{suma} równe \code{FALSE})
#' @param etykietaBD ciąg znaków - etykieta, którą w przygotowanym zestawieniu
#' mają być opisane braki danych (\code{NA}); domyślna wartość oznacza, że
#' zostaną one opisane jako "NA"; podanie \code{NULL} będzie skutkować
#' usunięciem kolumn i wierszy opisujących braki danych ze zwracanego
#' zestawienia
#' @return \code{data.frame} (klasy \code{tab_lbl}) z rozkładami
#' @name tab
#' @export
tab = function(x, ..., procenty = TRUE, d = 1, suma = TRUE,
               etykietaSuma, etykietaBD) {
  UseMethod("tab")
}
#' @rdname tab
#' @importFrom rlang ensyms
#' @export
tab.data.frame = function(x, ..., procenty = TRUE, d = 1, suma = TRUE,
                          etykietaSuma = "ŁĄCZNIE", etykietaBD = NA) {
  zmienna = ensyms(...)
  if (length(zmienna) == 0) stop("Nie podano zmiennej.")
  if (length(zmienna) > 1) stop("Podano więcej niż jedną zmienną.")
  if (!(!!zmienna %in% names(x))) stop("Podanej zmiennej nie ma w podanej ramce danych.")
  tab.default(x[[zmienna[[1]]]], procenty = procenty, d = d, suma = suma,
              etykietaSuma = etykietaSuma, etykietaBD = etykietaBD)
}
#' @rdname tab
#' @importFrom rlang as_name
#' @importFrom srvyr survey_count
#' @export
tab.tbl_svy = function(x, ..., procenty = TRUE, d = 1, suma = TRUE,
                       etykietaSuma = "ŁĄCZNIE", etykietaBD = NA) {
  zmienna = ensyms(...)
  if (length(zmienna) == 0) stop("Nie podano zmiennej.")
  if (length(zmienna) > 1) stop("Podano więcej niż jedną zmienną.")
  zmienna = zmienna[[1]]
  if (!(as_name(zmienna) %in% colnames(x))) {
    stop("Podanej zmiennej nie ma w podanej ramce danych.")
  }
  stopifnot(is.logical(procenty), length(procenty) == 1,
            is.numeric(d), length(d) == 1,
            is.logical(suma), length(suma) == 1,
            is.character(etykietaSuma), length(etykietaSuma) == 1)
  stopifnot(procenty %in% c(TRUE, FALSE),
            as.integer(d) == d,
            suma %in% c(TRUE, FALSE))

  # workaround na to, że survey_count() nie radzi sobie ze zm. numerycznymi
  # ani tekstowymi (jeśli zawierają braki danych)
  label = label(x$variables[[as.name(zmienna)]])
  valueLabels = value_labels(x$variables[[as.name(zmienna)]])
  if (is.numeric(x$variables[[as.name(zmienna)]]) |
      is.logical(x$variables[[as.name(zmienna)]])) {
    konwersja = mode(x$variables[[as.name(zmienna)]])
    x$variables[[as.name(zmienna)]] = factor(x$variables[[as.name(zmienna)]])
  } else if (is.character(x$variables[[as.name(zmienna)]])) {
    konwersja = "character"
    x$variables[[as.name(zmienna)]] = factor(x$variables[[as.name(zmienna)]])
    x$variables[[as.name(zmienna)]] = addNA(x$variables[[as.name(zmienna)]])
  } else {
    konwersja = NA_character_
  }
  # koniec I cz. workaroundu
  tab = suppressWarnings(
    survey_count(x, !!zmienna, name = "Freq", vartype = NULL))
  names(tab)[1] = 'x'
  tab = as.data.frame(tab, stringsAsFactors = FALSE)
  # II cz. workaroundu
  if (!is.na(konwersja)) {
    tab$x = do.call(paste0("as.", konwersja), list(x = tab$x))
  }
  # koniec workaroundu
  if (is.null(etykietaBD)) {
    tab = tab[!is.na(tab$x), ]
  }
  r = sum(round(tab$Freq, 0)) - round(sum(tab$Freq), 0)
  if (r > 0) {
    maska = order(((tab$Freq + 0.5) %% 1))[1:r]
    tab$Freq[maska] = floor(tab$Freq[maska])
  } else if (r < 0) {
    maska = order(-((tab$Freq + 0.5) %% 1))[1:abs(r)]
    tab$Freq[maska] = ceiling(tab$Freq[maska])
  }
  tab$Freq = round(tab$Freq, 0)
  tab = sformatuj_rozklad(tab, label, valueLabels, procenty, d, suma,
                          etykietaSuma, etykietaBD)
  return(tab)
}
#' @rdname tab
#' @importFrom stats setNames
#' @importFrom haven is.labelled
#' @export
tab.default = function(x, ..., procenty = TRUE, d = 1, suma = TRUE,
                       etykietaSuma = "ŁĄCZNIE", etykietaBD = NA) {
  stopifnot(is.numeric(x) | is.integer(x) | is.character(x) | is.labelled(x) | is.factor(x),
            is.logical(procenty), length(procenty) == 1,
            is.numeric(d), length(d) == 1,
            is.logical(suma), length(suma) == 1,
            is.character(etykietaSuma), length(etykietaSuma) == 1)
  stopifnot(procenty %in% c(TRUE, FALSE),
            as.integer(d) == d,
            suma %in% c(TRUE, FALSE))

  if (is.null(etykietaBD)) {
    exclude = NA
  } else {
    exclude = NULL
  }
  tab = table(x, exclude = exclude)
  tab = as.data.frame(tab, stringsAsFactors = FALSE)
  tab = sformatuj_rozklad(tab, label(x), value_labels(x),
                          procenty, d, suma, etykietaSuma, etykietaBD)
  return(tab)
}
#' @rdname tab
#' @export
print.tab_lbl = function(x, ...) {
  if (!is.null(label(x))) {
    cat(label(x), "\n\n")
  }
  NextMethod(row.names = FALSE)
}
# nieeksportowana funkcja odpowiadajaca za obudowanie rozkładu liczebności
# wszystkim, czego trzeba
sformatuj_rozklad = function(tab, label = NULL, value_labels = NULL,
                             procenty = TRUE, d = 1, suma = TRUE,
                             etykietaSuma = "ŁĄCZNIE", etykietaBD = NA) {
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

  if (!is.null(value_labels)) {
    nazwyKolumn = c(nazwyKolumn[1], "etykieta", nazwyKolumn[-1])
    tab = merge(data.frame("etykieta" = names(value_labels),
                           x = unname(value_labels),
                           stringsAsFactors = FALSE),
                tab, all = TRUE)
    tab$Freq[is.na(tab$Freq)] = 0
    tab$pr[is.na(tab$pr)] = paste0(format(0, nsmall = d), "%")
    tab = tab[order(tab$x), ]
    if (!is.null(etykietaBD)) {
      if (!is.na(etykietaBD)) {
        tab$etykieta[is.na(tab$etykieta)] = etykietaBD
      } else {
        tab$etykieta[is.na(tab$etykieta)] = "NA"
      }
    }
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
    if (!is.null(etykietaBD)) {
      if (!is.na(etykietaBD)) {
        tab$x[is.na(tab$x)] = etykietaBD
      }
    }
  }
  if (suma) {
    tab = rbind(tab, sum)
  }
  tab = setNames(tab, nazwyKolumn)
  if (!is.null(label)) {
    attributes(tab)$label = label
  }
  return(structure(tab,
                   class = c("tab_lbl", class(tab))))
}
