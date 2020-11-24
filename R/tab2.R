#' @title Tabela z rodzina rozkladow warunkowych dwoch zmiennych (etykietowanych)
#' @description
#' Funkcja generuje rozkład łączny liczebności i rozkład łączny lub rodzinę
#' warunkowych rozkładów częstości dwóch zmiennych. Jako pierwszy argument
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
#' klase \code{tab_n}), gdyż inaczej przygotowanych tabel z rozkładami nie
#' dawałyby się bezproblemowo używać w połączeniu z funkcją
#' \code{\link[knitr]{kable}} z pakietu \emph{kable} (która to funkcja wywołuje
#' wywołuje metodę \code{as.data.frame} na przekazywanym jej obiekcie).
#' @param x ramka danych lub obiekt klasy \code{tbl_svy}
#' @param zmW kolumna obiektu \code{x}, której wartości mają zostać umieszczone
#' w wierszach zwróconego rozkładu
#' @param zmK kolumna obiektu \code{x}, której wartości mają zostać umieszczone
#' w kolumnach zwróconego rozkładu
#' @param sumowanie w którą stronę, jeśli w ogóle, powinno zostać dokonane
#' sumowanie lub procentowanie? (wystarczy podać pierwszą literę)
#' @param liczby wartość logiczna - czy zwracana tabela ma zawierać rozkłady
#' liczebności?
#' @param procenty wartość logiczna - czy zwracana tabela ma zawierać rozkłady
#' częstości?
#' @param etykietaSuma ciąg znaków - etykieta dla wiersza lub kolumy z sumą
#' (argument jest ignorowany, jeśli \code{sumowanie} równe \code{"brak"} lub
#' \code{NULL})
#' @param etykietaOgolem ciąg znaków - etykieta dla wiersza lub kolumny
#' z rozkladem brzegowym zmiennej, której rodzina rozkładów warunkowowych jest
#' tworzona (argument jest ignorowany, jeśli \code{sumowanie} równe
#' \code{"brak"}, \code{"ogółem"} lub \code{NULL}); podanie wartości \code{NA}
#' lub \code{NULL} oznacza, że odpowiedni wiersz/kolumna nie powinna znaleźć
#' się w zwróconym zestawieniu
#' @param etykietaBD ciąg znaków - etykieta, którą w przygotowanym zestawieniu
#' mają być opisane braki danych (\code{NA}); domyślna wartość oznacza, że
#' zostaną one opisane jako "NA"; podanie \code{NULL} będzie skutkować
#' usunięciem kolumn i wierszy opisujących braki danych ze zwracanego
#' zestawienia
#' @param w opcjonalnie kolumna obiektu \code{x}, której wartości zawierają
#' wagi obserwacji, które powinny zostać uwzględnione przy obliczaniu rozkładu
#' @return
#' \strong{tab2}
#' \code{data.frame} (klasy \code{tab_lbl2}) z rozkładami:
#' \itemize{
#'   \item{pierwsza kolumna zawiera wartości zmiennej podanej argumentem
#'         \code{zmW} i ma taką samą nazwę, jak ta zmienna,}
#'   \item{nazwy kolejnych kolumn zaczynające się od "n_" opisują rozkład
#'         liczebności,}
#'   \item{nazwy kolejnych kolumn zaczynające się od "pct_" opisują rozkład
#'         częstości,}
#'   \item{nazwy tych kolumn powstały przez połącznie przedrostka "n_" lub
#'         "pct_" z wartościami zmiennej podanej argumentem \code{zmK} oraz
#'         ew. etykietą podaną argumentem \code{etkietaSuma}.}
#' }
#' Pierwsza zwracana kolumna rozkładu co do zasady jest \emph{czynnikiem}, chyba
#' że jednocześnie:
#' \itemize{
#'   \item{jako argument \code{sumowanie} zostało podane "brak" lub "wiersze"
#'         (ew. \code{NULL}),}
#'   \item{nie został podany argument \code{etykietaSumy} (ew. został podany
#'         jako \code{NA}),}
#'   \item{kolumna podana argumentem \code{zmW} jest klasy "character",
#'         "integer", "numeric" lub "logical".}
#' }
#' W takim przypadku będzie mieć ona takiego typu, jaki kolumna miała w danych
#' wejściowych.
#' \strong{metoda as_tibble}
#' W zależności od wartości parametru \code{niePrzeksztalcaj}:
#' \itemize{
#'   \item{\emph{tibble} z rozkładami przekształconymi do postaci \emph{długiej},}
#'   \item{obiekt klasy \code{data.frame} (i tylko tej jednej) z rozkładami
#'         w postacie \emph{szerokiej}.}
#' }
#' @name tab2
#' @export
tab2 = function(x, zmW, zmK, sumowanie, liczby, procenty, etykietaSuma,
                etykietaOgolem, etykietaBD, ...) {
  UseMethod("tab2")
}
#' @rdname tab2
#' @importFrom rlang as_name ensym
#' @importFrom dplyr count
#' @importFrom haven as_factor
#' @export
tab2.data.frame = function(x, zmW, zmK, sumowanie = c("brak", "kolumny",
                                                      "wiersze", "ogółem"),
                           liczby = TRUE, procenty = TRUE,
                           etykietaSuma = "SUMA", etykietaOgolem = "OGÓŁEM",
                           etykietaBD = NA, ...,
                           w = NULL) {
  zmW = ensym(zmW)
  zmK = ensym(zmK)
  sumowanie = match.arg(sumowanie)
  w = tryCatch(ensym(w),
               error = function(x) {return(NULL)})
  if (!(as_name(zmW) %in% names(x))) stop("Zmiennej podanej argumentem `zmW` nie ma w podanej ramce danych.")
  if (!(as_name(zmK) %in% names(x))) stop("Zmiennej podanej argumentem `zmK` nie ma w podanej ramce danych.")
  if (is.symbol(w)) {
    if (!(as_name(w) %in% names(x))) stop("Zmiennej podanej argumentem `w` nie ma w podanej ramce danych.")
  }
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
    x = x[!is.na(x[[as_name(zmW)]]) & !is.na(x[[as_name(zmK)]]), ]
    etykietaBD = NA_character_
  }
  labels = list(w = label(x[[as_name(zmW)]]),
                k = label(x[[as_name(zmK)]]))

  # obsługa zmiennych typu labelled
  if (is.labelled(x[[as_name(zmW)]])) {
    x[[as_name(zmW)]] = as_factor(x[[as_name(zmW)]])
  }
  if (is.labelled(x[[as_name(zmK)]])) {
    x[[as_name(zmK)]] = as_factor(x[[as_name(zmK)]])
  }

  # zapisanie klas zmiennych, aby umożliwić ich późniejszą "rekonwersję" (z czynników)
  klasyZm = list(w = class(x[[as_name(zmW)]])[length(class(x[[as_name(zmW)]]))],
                 k = class(x[[as_name(zmK)]])[length(class(x[[as_name(zmK)]]))])

  # konwersja na czynniki
  x[[as_name(zmW)]] = factor(x[[as_name(zmW)]],
                             sort(unique(x[[as_name(zmW)]])))
  x[[as_name(zmW)]] = addNA(x[[as_name(zmW)]], ifany = TRUE)
  x[[as_name(zmK)]] = factor(x[[as_name(zmK)]],
                             sort(unique(x[[as_name(zmK)]])))
  x[[as_name(zmK)]] = addNA(x[[as_name(zmK)]], ifany = TRUE)

  # samo przygotowanie rozkładu
  x = count(x, !!zmW, !!zmK, wt = !!w, .drop = FALSE)

  x = sformatuj_rozklad2(x, zmW, zmK, sumowanie, liczby, procenty,
                         etykietaSuma, etykietaOgolem, etykietaBD, klasyZm,
                         labels)
  return(x)
}
#' @rdname tab2
#' @importFrom haven as_factor
#' @importFrom srvyr filter survey_count
#' @export
tab2.tbl_svy = function(x, zmW, zmK, sumowanie = c("brak", "kolumny",
                                                   "wiersze", "ogółem"),
                        liczby = TRUE, procenty = TRUE,
                        etykietaSuma = "SUMA", etykietaOgolem = "OGÓŁEM",
                        etykietaBD = NA, ...) {
  zmW = ensym(zmW)
  zmK = ensym(zmK)
  sumowanie = match.arg(sumowanie)
  if (!(as_name(zmW) %in% colnames(x))) stop("Zmiennej podanej argumentem `zmW` nie ma w podanym obiekcie.")
  if (!(as_name(zmK) %in% colnames(x))) stop("Zmiennej podanej argumentem `zmK` nie ma w podanym obiekcie.")
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
    x = filter(x, !is.na(!!zmW) & !is.na(!!zmK))
    etykietaBD = NA_character_
  }
  labels = list(w = label(x$variables[[as_name(zmW)]]),
                k = label(x$variables[[as_name(zmK)]]))

  # obsługa zmiennych typu labelled
  if (is.labelled(x$variables[[as_name(zmW)]])) {
    x$variables[[as_name(zmW)]] = as_factor(x$variables[[as_name(zmW)]])
  }
  if (is.labelled(x$variables[[as_name(zmK)]])) {
    x$variables[[as_name(zmK)]] = as_factor(x$variables[[as_name(zmK)]])
  }

  # zapisanie klas zmiennych, aby umożliwić ich późniejszą "rekonwersję" (z czynników)
  klasyZm = list(w = class(x$variables[[as_name(zmW)]]),
                 k = class(x$variables[[as_name(zmK)]]))

  # konwersja na czynniki
  x$variables[[as_name(zmW)]] = factor(x$variables[[as_name(zmW)]],
                                       sort(unique(x$variables[[as_name(zmW)]])))
  x$variables[[as_name(zmW)]] = addNA(x$variables[[as_name(zmW)]], ifany = TRUE)
  x$variables[[as_name(zmK)]] = factor(x$variables[[as_name(zmK)]],
                                       sort(unique(x$variables[[as_name(zmK)]])))
  x$variables[[as_name(zmK)]] = addNA(x$variables[[as_name(zmK)]], ifany = TRUE)

  x = suppressWarnings(
    survey_count(x, !!zmW, !!zmK, vartype = NULL, .drop = FALSE))

  x = sformatuj_rozklad2(x, zmW, zmK, sumowanie, liczby, procenty,
                         etykietaSuma, etykietaOgolem, etykietaBD, klasyZm,
                         labels)
  return(x)
}
#' @rdname tab2
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
print.tab_lbl2 = function(x, dProcenty = 1, dLiczby = 0, decimal.mark = ",",
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
#' @importFrom rlang := as_name
#' @importFrom tidyr pivot_wider
#' @importFrom dplyr .data bind_cols bind_rows group_by mutate select
#' starts_with summarise summarise_if ungroup
sformatuj_rozklad2 = function(x, zmW, zmK, sumowanie, liczby, procenty,
                              etykietaSuma, etykietaOgolem, etykietaBD, klasyZm,
                              labels) {
  # obsługa etykiet braków danych
  if (!is.na(etykietaBD)) {
    if (is.factor(x[[as_name(zmW)]]) & any(x[[as_name(zmW)]] %in% NA)) {
      poziomy = c(levels(x[[as_name(zmW)]]), etykietaBD)
      x[[as_name(zmW)]] = levels(x[[as_name(zmW)]])[x[[as_name(zmW)]]]
      x[[as_name(zmW)]][is.na(x[[as_name(zmW)]])] = etykietaBD
      x[[as_name(zmW)]] = factor(x[[as_name(zmW)]], poziomy)
    } else if (!is.factor(x[[as_name(zmW)]])) {
      x[[as_name(zmW)]][is.na(x[[as_name(zmW)]])] = etykietaBD
    }
    if (is.factor(x[[as_name(zmK)]]) & any(x[[as_name(zmK)]] %in% NA)) {
      poziomy = c(levels(x[[as_name(zmK)]]), etykietaBD)
      x[[as_name(zmK)]] = levels(x[[as_name(zmK)]])[x[[as_name(zmK)]]]
      x[[as_name(zmK)]][is.na(x[[as_name(zmK)]])] = etykietaBD
      x[[as_name(zmK)]] = factor(x[[as_name(zmK)]], poziomy)
    } else if (!is.factor(x[[as_name(zmK)]])) {
      x[[as_name(zmK)]][is.na(x[[as_name(zmK)]])] = etykietaBD
    }
  }
  # procentowanie i sumowanie
  if (sumowanie %in% c("kolumny", "wiersze")) {
    if (sumowanie == "kolumny") {
      zmGr = zmK
      zmZal = zmW
    } else {
      zmGr = zmW
      zmZal = zmK
    }
  }
  if (sumowanie %in% c("wiersze", "kolumny") & !is.na(etykietaOgolem)) {
    temp = group_by(x, !!zmZal)
    temp = summarise(temp, n = sum(.data$n))
    temp = mutate(temp, !!zmGr := factor(etykietaOgolem,
                                         c(levels(x[[as_name(zmGr)]]),
                                           etykietaOgolem)))
    x[[as_name(zmGr)]] = factor(x[[as_name(zmGr)]],
                                levels(temp[[as_name(zmGr)]]))
    x = bind_rows(ungroup(x), temp)
  }
  if (sumowanie %in% c("kolumny", "wiersze")) {
    x = group_by(x, !!zmGr)
  }
  x = mutate(x, pct = 100 * .data$n / sum(.data$n))
  temp = group_by(x, !!zmW)
  temp = summarise(temp, n = sum(.data$n), pct = sum(.data$pct))
  temp = ungroup(temp)
  x = ungroup(x)
  x = x[order(x[[as_name(zmK)]]), ] # żeby mieć dobrą kolejność kolumn po pivocie
  x = pivot_wider(x, names_from = !!zmK, values_from = c(.data$n, .data$pct),
                  values_fill = list(n = 0, pct = 0))
  x = x[order(x[[as_name(zmW)]]), ] # żeby mieć dobrą kolejność wierszy
  x = mutate(x, !!zmW := as.character(!!zmW))
  if (sumowanie %in% c("wiersze", "ogółem")) {
    temp = select(temp, -!!zmW)
    names(temp) = paste(names(temp), etykietaSuma, sep = "_")
    x = bind_cols(x, temp)
    x = select(x, !!zmW, starts_with("n_"), starts_with("pct_"))
  }
  if (sumowanie %in% c("kolumny", "ogółem")) {
    temp = mutate(x, !!zmW := as.character(etykietaSuma))
    temp = group_by(temp, !!zmW)
    temp = summarise_if(temp, is.numeric, sum)
    temp = ungroup(temp)
    x = bind_rows(x, temp)
  }
  if (!liczby) {
    x = select(x, -starts_with("n_"))
  }
  if (!procenty) {
    x = select(x, -starts_with("pct_"))
  }
  # ew. rekonwersja pierwszej kolumny
  if (any(klasyZm$w %in% c("character", "integer", "numeric", "logical")) &
      is.na(etykietaBD) & is.na(etykietaOgolem) &
      sumowanie %in% c("brak", "wiersze")) {
    x[[as_name(zmW)]] =
      do.call(paste0("as.", klasyZm$w[length(klasyZm$w)]),
              list(x = x[[as_name(zmW)]]))
  } else {
    x[[as_name(zmW)]] = factor(x[[as_name(zmW)]], x[[as_name(zmW)]])
  }
  # czynności końcowe
  return(structure(as.data.frame(x,
                                 check.names = FALSE, stringsAsFactors = FALSE),
                   class = c("tab_lbl2", class(x)),
                   nazwyZm = list(w = as_name(zmW), k = as_name(zmK)),
                   klasyZm = klasyZm,
                   label = labels,
                   sumowanie = sumowanie,
                   etykietaSuma = etykietaSuma,
                   etykietaOgolem = etykietaOgolem,
                   etykietaBD = etykietaBD))
}
