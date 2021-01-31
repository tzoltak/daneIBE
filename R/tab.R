#' @title Tabela z rozkladem dla zmiennych etykietowanych
#' @description
#' Funkcja generuje rozkład jednej zmiennej - liczebności i częstości -
#' uwzględniając etykiety wartości.
#' @param x obiekt klasy \code{labelled}, wektor, ramka danych lub obiekt
#' klasy \code{tbl_svy}
#' @param ... tylko jeśli \code{x} jest \emph{ramką danych} - kolumna, której
#' rozkład ma zostać wygenerowany
#' @param procenty wartość logiczna - czy wyświetlić również rozkład częstości?
#' @param suma wartość logiczna - czy wyświetlić wiersz z sumą?
#' @param etykietaSuma ciąg znaków - etykieta dla wiersza z sumą (argument jest
#' ignorowany, jeśli \code{suma} równe \code{FALSE})
#' @param etykietaBD ciąg znaków - etykieta, którą w przygotowanym zestawieniu
#' mają być opisane braki danych (\code{NA}); domyślna wartość oznacza, że
#' zostaną one opisane jako "NA"; podanie \code{NULL} będzie skutkować
#' usunięciem wierszy opisujących braki danych ze zwracanego zestawienia
#' @param w opcjonalnie wektor liczbowy lub kolumna obiektu \code{x}, której
#' wartości zawierają wagi obserwacji, które powinny zostać uwzględnione przy
#' obliczaniu rozkładu
#' @return \code{data.frame} (klasy \code{tab_lbl}) z rozkładami
#' @name tab
#' @export
tab = function(x, ..., procenty = TRUE, suma = TRUE, etykietaSuma, etykietaBD) {
  UseMethod("tab", x)
}
#' @rdname tab
#' @importFrom rlang ensyms
#' @export
tab.data.frame = function(x, ..., procenty = TRUE, suma = TRUE,
                          etykietaSuma = "ŁĄCZNIE", etykietaBD = NA, w = NULL) {
  zmienna = ensyms(...)
  assert_tab_df(x, zmienna)
  w = tryCatch(ensym(w),
               error = function(x) {return(NULL)})
  w = assert_w(w, x)
  x = x[[as_name(zmienna[[1]])]]
  NextMethod()
}
#' @rdname tab
#' @export
tab.table = function(x, ..., procenty = TRUE, suma = TRUE,
                     etykietaSuma = "ŁĄCZNIE", etykietaBD = NA) {
  x = as.data.frame(x)
  names(x)[ncol(x)] = "___Freq___"
  tab(x, ..., procenty = procenty, suma = suma, etykietaSuma = etykietaSuma,
      etykietaBD = etykietaBD, w = "___Freq___")
}
#' @rdname tab
#' @export
tab.ftable = function(x, ..., procenty = TRUE, suma = TRUE,
                      etykietaSuma = "ŁĄCZNIE", etykietaBD = NA) {
  x = as.data.frame(x)
  names(x)[ncol(x)] = "___Freq___"
  tab(x, ..., procenty = procenty, suma = suma, etykietaSuma = etykietaSuma,
      etykietaBD = etykietaBD, w = "___Freq___")
}
#' @rdname tab
#' @importFrom rlang as_name
#' @importFrom srvyr survey_count
#' @export
tab.tbl_svy = function(x, ..., procenty = TRUE, suma = TRUE,
                       etykietaSuma = "ŁĄCZNIE", etykietaBD = NA) {
  zmienna = ensyms(...)
  assert_tab_df(x, zmienna)
  zmienna = zmienna[[1]]
  assert_tab_wektor(procenty, suma, etykietaSuma, etykietaBD)
  if (is.null(etykietaBD)) {
    x = filter(x, !is.na(!!zmienna))
  }

  tab = survey_count(x, !!zmienna, name = "Freq", vartype = NULL)
  names(tab)[1] = 'x'
  tab = as.data.frame(tab, stringsAsFactors = FALSE)

  tab = sformatuj_rozklad(tab,
                          label(x$variables[[as.name(zmienna)]]),
                          value_labels(x$variables[[as.name(zmienna)]]),
                          procenty, suma, etykietaSuma, etykietaBD)
  return(tab)
}
#' @rdname tab
#' @importFrom srvyr as_survey
#' @export
tab.survey.design2 = function(x, ..., procenty = TRUE, suma = TRUE,
                              etykietaSuma = "ŁĄCZNIE", etykietaBD = NA) {
  tab(as_survey(x), ..., procenty = procenty, suma = suma,
      etykietaSuma = etykietaSuma, etykietaBD = etykietaBD)
}
#' @rdname tab
#' @importFrom srvyr as_survey
#' @export
tab.svyrep.design = function(x, ..., procenty = TRUE, suma = TRUE,
                             etykietaSuma = "ŁĄCZNIE", etykietaBD = NA) {
  tab(as_survey(x), ..., procenty = procenty, suma = suma,
      etykietaSuma = etykietaSuma, etykietaBD = etykietaBD)
}
#' @rdname tab
#' @importFrom srvyr as_survey
#' @export
tab.twophase2 = function(x, ..., procenty = TRUE, suma = TRUE,
                         etykietaSuma = "ŁĄCZNIE", etykietaBD = NA) {
  tab(as_survey(x), ..., procenty = procenty, suma = suma,
      etykietaSuma = etykietaSuma, etykietaBD = etykietaBD)
}
#' @rdname tab
#' @importFrom stats setNames
#' @importFrom haven is.labelled
#' @export
tab.default = function(x, ..., procenty = TRUE, suma = TRUE,
                       etykietaSuma = "ŁĄCZNIE", etykietaBD = NA, w = NULL) {
  if (is.null(x)) stop("Obiekt przekazany argumentem 'x' nie istnieje (ma wartość NULL).")
  assert_tab_wektor(procenty, suma, etykietaSuma, etykietaBD)
  w = assert_w(w, x)
  if (is.null(etykietaBD)) {
    w = w[!is.na(x)]
    x = x[!is.na(x)]
  }
  tab = tapply(w, addNA(as.factor(x), ifany = TRUE),
               sum, na.rm = TRUE, default = 0)
  tab = data.frame(x = rownames(tab),
                   Freq = as.vector(tab))
  tab = sformatuj_rozklad(tab, label(x), value_labels(x),
                          procenty, suma, etykietaSuma, etykietaBD)
  return(tab)
}
# nieeksportowana funkcja odpowiadajaca za obudowanie rozkładu liczebności
# wszystkim, czego trzeba
sformatuj_rozklad = function(tab, label = NULL, value_labels = NULL,
                             procenty = TRUE, suma = TRUE,
                             etykietaSuma = "ŁĄCZNIE", etykietaBD = NA) {
  if (suma) {
    sum = data.frame(x = "", Freq = sum(tab$Freq))
  }
  if (procenty) {
    nazwyKolumn = c("wartość", "liczebność", "częstość")
    tab$pr = 100 * tab$Freq / sum(tab$Freq)
    if (suma) {
      sum$pr = 100
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
    tab$pr[is.na(tab$pr)] = 0
    tab = tab[order(tab$x), ]
    if (!is.null(etykietaBD)) {
      if (!is.na(etykietaBD)) {
        tab$etykieta[is.na(tab$etykieta)] = etykietaBD
      } else {
        tab$etykieta[is.na(tab$etykieta) & is.na(tab$x)] = "NA"
      }
    }
    # obejście ew. problemów z kodowaniem etykiet
    # if (any(is.na(nchar(tab$etykieta, "chars", TRUE)))) {
    #   Encoding(tab$etykieta) = sub("^[^.]*[.]", "", Sys.getlocale("LC_COLLATE"))
    # }
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
  return(structure(tab,
                   class = c("tab_lbl", class(tab)),
                   label = label,
                   suma = suma,
                   procenty = procenty,
                   etykietaSuma = etykietaSuma,
                   etykietaBD = etykietaBD))
}
