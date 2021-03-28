#' @title Tabela rozkladow brzegowych wielu zmiennych (etykietowanych) o tym
#' samym zbiorze mozliwych wartosci
#' @description
#' Funkcja generuje tabelę zestawiającą ze sobą rozkłady brzegowe liczebności
#' i rozkład brzegowe częstości kilku zmiennych, które mogą przyjmować ten sam
#' zbiór wartości (a przynajmniej tak zakładamy). Jako pierwszy argument
#' przyjmuje obiekt \code{data.frame} lub \code{tbl_svy} (obiekt będący
#' połączeniem \emph{ramki danych} z informacjami o - zwykle złożonym -
#' schemacie doboru próby, tworzonym przez funkcje pakietu \emph{srvyr}). Radzi
#' też sobie ze zmiennymi etykietowanymi (konwertując je na \emph{czynniki}).
#'
#' Metoda \code{as_tibble} pozwala przekształcić zwracane zestawienie
#' w ramkę danych w postaci \emph{długiej}, przydatną np. do rysowania wykresów
#' przy pomocy \emph{ggplot2} (czyli działa analogicznie,jak metoda
#' \code{as.data.frame} dla obiektów zwracanych przez funkcję
#' \code{\link[base]{table}} z pakietu \emph{base}). Metoda \code{as.data.frame}
#' również jest dostępna, ale domyślnie nie przekształca tabeli (usuwa jedynie
#' klasę \code{tab_n}), gdyż inaczej przygotowanych tabel z rozkładami nie
#' dawałoby się bezproblemowo używać w połączeniu z funkcją
#' \code{\link[knitr]{kable}} z pakietu \emph{kable} (która to funkcja wywołuje
#' metodę \code{as.data.frame} na przekazywanym jej obiekcie).
#' @param x ramka danych lub obiekt klasy \code{tbl_svy}
#' @param ... zmienne, których rozkłady mają zostać zwrócone (można używać
#' selektorów pakietu \emph{dplyr}, np. \code{\link[dplyr]{starts_with}})
#' @param kierunek w którą stronę mają układać się w zwracanym zestawieniu
#' rozkłady brzegowe?
#' @param liczby wartość logiczna - czy zwracana tabela ma zawierać rozkłady
#' liczebności?
#' @param procenty wartość logiczna - czy zwracana tabela ma zawierać rozkłady
#' częstości?
#' @param etykietaSuma ciąg znaków - etykieta dla wiersza lub kolumy z sumą
#' @param etykietyZmiennych wartość logiczna - czy do opisania zmiennych
#' w zwracanym zestawieniu mają zostać wykorzystane ich etykiety (o ile są
#' dostępne)? jeśli \code{FALSE}, użyte zostaną nazwy zmiennych
#' @param etykietaBD ciąg znaków - etykieta, którą w przygotowanym zestawieniu
#' mają być opisane braki danych (\code{NA}); domyślna wartość oznacza, że
#' zostaną one opisane jako "NA"; podanie \code{NULL} będzie skutkować
#' usunięciem kolumn i wierszy opisujących braki danych ze zwracanego
#' zestawienia
#' @param w opcjonalnie kolumna obiektu \code{x}, której wartości zawierają
#' wagi obserwacji, które powinny zostać uwzględnione przy obliczaniu rozkładu
#' @return
#' \strong{tab_n}
#' \code{data.frame} (klasy \code{tab_lbl_n}) z rozkładami:
#' \itemize{
#'   \item{w zależności od wartości argumentu, \code{kierunek} w pierwszej
#'         kolumnie będą znajować się albo wartości zmiennych
#'         (\code{kierunek="kolumny"}) albo nazwy zmiennych
#'         (\code{kierunek="wiersze"}) - kolumna zawsze jest czynnikiem
#'         (którego kolejność poziomów odpowiada kolejności, w jakiej wybrane
#'         zostały zmienne w wywołaniu \code{\link{tab_n}}),}
#'   \item{nazwy kolejnych kolumn zaczynające się od "n_" opisują rozkłady
#'         liczebności,}
#'   \item{nazwy kolejnych kolumn zaczynające się od "pct_" opisują rozkłady
#'         częstości,}
#'   \item{nazwy tych kolumn powstały przez połącznie przedrostka "n_" lub
#'         "pct_" z nazwami zmiennych \code{kierunek="kolumny"} lub
#'         z wartościami zmiennych \code{kierunek="wiersze"} oraz
#'         ew. etykietą podaną argumentem \code{etkietaSuma}.}
#' }
#' \strong{metoda as_tibble}
#' W zależności od wartości parametru \code{niePrzeksztalcaj}:
#' \itemize{
#'   \item{\emph{tibble} z rozkładami przekształconymi do postaci \emph{długiej},}
#'   \item{obiekt klasy \code{data.frame} (i tylko tej jednej) z rozkładami
#'         w postacie \emph{szerokiej}.}
#' }
#' @name tab_n
#' @export
tab_n = function(x, ..., kierunek, liczby, procenty, etykietaSuma,
                 etykietyZmiennych, etykietaBD) {
  UseMethod("tab_n")
}
#' @rdname tab_n
#' @importFrom rlang as_name ensyms
#' @importFrom dplyr .data count select
#' @importFrom tidyr pivot_longer
#' @importFrom haven as_factor
#' @export
tab_n.data.frame = function(x, ..., kierunek = c("kolumny", "wiersze"),
                            liczby = TRUE, procenty = TRUE,
                            etykietaSuma = "SUMA", etykietyZmiennych = FALSE,
                            etykietaBD = NA, w = NULL) {
  kierunek = match.arg(kierunek)
  w = tryCatch(ensym(w),
               error = function(x) {return(NULL)})
  if (is.symbol(w)) {
    if (!(as_name(w) %in% names(x))) stop("Zmiennej podanej argumentem `w` nie ma w podanej ramce danych.")
    wagi = select(x, w)
  } else {
    wagi = select(x)
  }
  x = select(x, ...)
  zm = names(x)
  x = as_factor(x)
  klasyZm = setNames(lapply(x, class), zm)
  labels = labels(x)
  wartosci = lapply(x, function(x) {
    if (is.factor(x)) {
      return(levels(x))
    } else {
      return(sort(unique(x)))
    }
  })
  wszystkieWartosci = wartosci[[1]]
  if (ncol(x) > 1) {
    for (i in 2:ncol(x)) {
      if (length(wartosci[[1]]) != length(wartosci[[i]])) {
        warning("Podane zmienne mają różne zestawy wartości.")
      } else if (!all(wartosci[[1]] %in% wartosci[[i]])) {
        warning("Podane zmienne mają różne zbiory wartości.")
      }
      wszystkieWartosci = union(wszystkieWartosci, wartosci[[i]])
    }
  }

  stopifnot(is.logical(liczby), length(liczby) == 1,
            is.logical(procenty), length(procenty) == 1,
            is.logical(etykietyZmiennych), length(etykietyZmiennych) == 1,
            is.character(etykietaSuma), length(etykietaSuma) == 1)
  stopifnot(liczby %in% c(TRUE, FALSE),
            procenty %in% c(TRUE, FALSE),
            etykietyZmiennych %in% c(TRUE, FALSE))
  if (!is.null(etykietaBD)) {
    stopifnot(length(etykietaBD) == 1)
    if (is.na(etykietaBD)) {
      etykietaBD = NA_character_
    }
    stopifnot(is.character(etykietaBD))
  }

  x = bind_cols(x, wagi)
  if (length(unique(klasyZm)) == 1) {
    przeksztalc = list()
  } else {
    przeksztalc = list(wartosc = factor)
  }
  x = pivot_longer(x, -names(wagi), names_to = "zmienna", values_to = "wartosc",
                   values_transform = przeksztalc)
  # A tibble: 150 x 2)
  x$zmienna = factor(x$zmienna, zm)
  if (is.null(etykietaBD)) {
    x = x[!is.na(x$wartosc), ]
    etykietaBD = NA_character_
  }
  x = count(x, .data$zmienna, .data$wartosc, wt = !!w, .drop = FALSE)
  if (etykietyZmiennych) {
    x$zmienna = factor(x$zmienna, names(labels), labels)
  } else {
    x$zmienna = factor(x$zmienna, names(labels))
  }

  x = sformatuj_rozklad_n(x, zm, kierunek, liczby, procenty,
                          etykietaSuma, etykietaBD, klasyZm, labels,
                          wszystkieWartosci)
  return(x)
}
#' @rdname tab_n
#' @export
tab_n.table = function(x, ..., kierunek = c("kolumny", "wiersze"),
                       liczby = TRUE, procenty = TRUE,
                       etykietaSuma = "SUMA", etykietyZmiennych = FALSE,
                       etykietaBD = NA) {
  x = as.data.frame(x)
  names(x)[ncol(x)] = "___Freq___"
  tab_n(x, ..., kierunek = kierunek, liczby = liczby, procenty = procenty,
        etykietaSuma = etykietaSuma, etykietyZmiennych = etykietyZmiennych,
        etykietaBD = etykietaBD, w = "___Freq___")
}
#' @rdname tab_n
#' @export
tab_n.ftable = function(x, ..., kierunek = c("kolumny", "wiersze"),
                        liczby = TRUE, procenty = TRUE,
                        etykietaSuma = "SUMA", etykietyZmiennych = FALSE,
                        etykietaBD = NA) {
  x = as.data.frame(x)
  names(x)[ncol(x)] = "___Freq___"
  tab_n(x, ..., kierunek = kierunek, liczby = liczby, procenty = procenty,
        etykietaSuma = etykietaSuma, etykietyZmiennych = etykietyZmiennych,
        etykietaBD = etykietaBD, w = "___Freq___")
}
#' @rdname tab_n
#' @importFrom dplyr bind_rows
#' @importFrom haven as_factor
#' @importFrom srvyr survey_count select
#' @export
tab_n.tbl_svy = function(x, ..., kierunek = c("kolumny", "wiersze"),
                         liczby = TRUE, procenty = TRUE,
                         etykietaSuma = "SUMA", etykietyZmiennych = FALSE,
                         etykietaBD = NA) {
  kierunek = match.arg(kierunek)
  x = select(x, ...)
  zm = names(x$variables)
  x$variables = as_factor(x$variables)
  klasyZm = setNames(lapply(x$variables, class), zm)
  labels = labels(x$variables)
  wartosci = lapply(x$variables, function(x) {
    if (is.factor(x)) {
      return(levels(x))
    } else {
      return(sort(unique(x)))
    }
  })
  wszystkieWartosci = wartosci[[1]]
  if (ncol(x) > 1) {
    for (i in 2:ncol(x)) {
      if (length(wartosci[[1]]) != length(wartosci[[i]])) {
        warning("Podane zmienne mają różne zestawy wartości.")
      } else if (!all(wartosci[[1]] == wartosci[[i]])) {
        warning("Podane zmienne mają różne zestawy wartości.")
      }
      wszystkieWartosci = union(wszystkieWartosci, wartosci[[i]])
    }
  }

  stopifnot(is.logical(liczby), length(liczby) == 1,
            is.logical(procenty), length(procenty) == 1,
            is.logical(etykietyZmiennych), length(etykietyZmiennych) == 1,
            is.character(etykietaSuma), length(etykietaSuma) == 1)
  stopifnot(liczby %in% c(TRUE, FALSE),
            procenty %in% c(TRUE, FALSE),
            etykietyZmiennych %in% c(TRUE, FALSE))
  if (!is.null(etykietaBD)) {
    stopifnot(length(etykietaBD) == 1)
    if (is.na(etykietaBD)) {
      etykietaBD = NA_character_
    }
    stopifnot(is.character(etykietaBD))
  }

  rozklady = lapply(zm,
                    function(zm, dane) {
                      dane$variables[[zm]] = factor(dane$variables[[zm]])
                      dane = survey_count(dane, !!as.symbol(zm), .drop = FALSE)
                      dane = select(dane, wartosc = !!as.symbol(zm), .data$n)
                      return(dane)
                    }, dane = x)
  names(rozklady) = zm
  x = bind_rows(rozklady, .id = "zmienna")
  x$zmienna = factor(x$zmienna, zm)
  if (is.null(etykietaBD)) {
    x = x[!is.na(x$wartosc), ]
    etykietaBD = NA_character_
  }
  if (etykietyZmiennych) {
    x$zmienna = factor(x$zmienna, names(labels), labels)
  } else {
    x$zmienna = factor(x$zmienna, names(labels))
  }

  x = sformatuj_rozklad_n(x, zm, kierunek, liczby, procenty,
                          etykietaSuma, etykietaBD, klasyZm, labels,
                          wszystkieWartosci)
  return(x)
}
#' @rdname tab_n
#' @importFrom srvyr as_survey
#' @export
tab_n.survey.design2 = function(x, ..., kierunek = c("kolumny", "wiersze"),
                                liczby = TRUE, procenty = TRUE,
                                etykietaSuma = "SUMA", etykietyZmiennych = FALSE,
                                etykietaBD = NA) {
  tab_n(as_survey(x), ..., kierunek = kierunek,
        liczby = liczby, procenty = procenty, etykietaSuma = etykietaSuma,
        etykietyZmiennych = etykietyZmiennych, etykietaBD = etykietaBD)
}
#' @rdname tab_n
#' @importFrom srvyr as_survey
#' @export
tab_n.svyrep.design = function(x, ..., kierunek = c("kolumny", "wiersze"),
                               liczby = TRUE, procenty = TRUE,
                               etykietaSuma = "SUMA", etykietyZmiennych = FALSE,
                               etykietaBD = NA) {
  tab_n(as_survey(x), ..., kierunek = kierunek,
        liczby = liczby, procenty = procenty, etykietaSuma = etykietaSuma,
        etykietyZmiennych = etykietyZmiennych, etykietaBD = etykietaBD)
}
#' @rdname tab_n
#' @importFrom srvyr as_survey
#' @export
tab_n.twophase2 = function(x, ..., kierunek = c("kolumny", "wiersze"),
                           liczby = TRUE, procenty = TRUE,
                           etykietaSuma = "SUMA", etykietyZmiennych = FALSE,
                           etykietaBD = NA) {
  tab_n(as_survey(x), ..., kierunek = kierunek,
        liczby = liczby, procenty = procenty, etykietaSuma = etykietaSuma,
        etykietyZmiennych = etykietyZmiennych, etykietaBD = etykietaBD)
}
#' @importFrom dplyr .data group_by mutate summarise
#' @importFrom tidyr pivot_wider
sformatuj_rozklad_n = function(x, zm, kierunek, liczby, procenty,
                               etykietaSuma, etykietaBD, klasyZm, labels,
                               wartosci) {
  x = group_by(x, .data$zmienna)
  x = mutate(x, pct = 100 * .data$n / sum(.data$n))
  temp = summarise(x, n = sum(.data$n), pct = sum(.data$pct))
  x = ungroup(x)
  temp = ungroup(temp)
  temp$wartosc = as.character(etykietaSuma)
  x$wartosc = as.character(x$wartosc)
  x = bind_rows(x, temp)
  if (!is.na(etykietaBD)) {
    x$wartosc[is.na(x$wartosc)] = as.character(etykietaBD)
  }
  x$wartosc = factor(x$wartosc, c(wartosci, as.character(etykietaBD),
                                  as.character(etykietaSuma)))
  x$wartosc = addNA(x$wartosc, ifany = TRUE)

  if (kierunek %in% "wiersze") {
    x = pivot_wider(x, names_from = .data$wartosc,
                    values_from = c(.data$n, .data$pct),
                    values_fill = 0)
  } else {
    x = pivot_wider(x, names_from = .data$zmienna,
                    values_from = c(.data$n, .data$pct),
                    values_fill = 0)
  }
  if (!liczby) {
    x = select(x, -starts_with("n_"))
  }
  if (!procenty) {
    x = select(x, -starts_with("pct_"))
  }

  return(structure(as.data.frame(x,
                                 check.names = FALSE, stringsAsFactors = FALSE),
                   class = c("tab_lbl_n", class(x)),
                   nazwyZm = zm,
                   klasyZm = klasyZm,
                   label = labels,
                   kierunek = kierunek,
                   etykietaSuma = etykietaSuma,
                   etykietaBD = etykietaBD,
                   etykietaOgolem = NULL))
}
