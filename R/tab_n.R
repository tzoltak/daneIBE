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
#' @param  dProcenty liczba miejsc dziesiętnych, do jakiej zostaną zaokrąglone
#' wartości kolumn z rozkładami częstości
#' @param  dLiczby liczba miejsc dziesiętnych, do jakiej zostaną zaokrąglone
#' wartości kolumn z rozkładami liczebności (to mogą być liczby niecałkowite,
#' jeśli przy tworzeniu rozkładu stosowano ważenie)
#' @param decimal.mark znak miejsca dziesiętnego - przekazywany do
#' \code{\link[base]{format}}
#' @param ... w przypadku metody \code{print}: ew. inne parametry, przekazywane
#' do \code{\link[base]{format}} (wywołwanej na komórkach opisujących
#' liczebności lub częstości)
#' @export
print.tab_lbl_n = function(x, dProcenty = 1, dLiczby = 0, decimal.mark = ",",
                           ...) {
  stopifnot(is.numeric(dProcenty), length(dProcenty) == 1,
            is.numeric(dLiczby), length(dLiczby) == 1,
            is.character(decimal.mark), length(decimal.mark) == 1)
  stopifnot(as.integer(dProcenty) == dProcenty, dProcenty >= 0,
            as.integer(dLiczby) == dLiczby, dLiczby >= 0)

  cat(label(x), "\n\n", sep = "")
  for (i in grep("^n_", names(x))) {
    x[, i] = format(round(x[, i], dLiczby), digits = 0, nsmall = dLiczby,
                    decimal.mark = decimal.mark, ...)
  }
  for (i in grep("^pct_", names(x))) {
    x[, i] = format(round(x[, i], dProcenty), digits = 0, nsmall = dProcenty,
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

  x = structure(as.data.frame(x, check.names = FALSE, stringsAsFactors = FALSE),
                class = c("tab_lbl_n", class(x)),
                nazwyZm = zm,
                klasyZm = klasyZm,
                label = labels,
                kierunek = kierunek,
                etykietaSuma = etykietaSuma,
                etykietaBD = etykietaBD,
                etykietaOgolem = NULL)
  return(x)
}
