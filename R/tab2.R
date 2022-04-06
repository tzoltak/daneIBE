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
                           etykietaBD = NA, ..., w = NULL) {
  zmW = ensym(zmW)
  zmK = ensym(zmK)
  sumowanie = match.arg(sumowanie)
  w = tryCatch(ensym(w),
               error = function(x) {return(NULL)})
  assert_w(w, x)
  if (is.null(etykietaBD)) {
    x = x[!is.na(x[[as_name(zmW)]]) & !is.na(x[[as_name(zmK)]]), ]
  }
  etykiety = assert_tab2(names(x), zmW, zmK, liczby, procenty, etykietaSuma,
                         etykietaOgolem, etykietaBD)
  etykietaOgolem = etykiety$etykietaOgolem
  etykietaBD = etykiety$etykietaBD

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
  if (!is.factor(x[[as_name(zmW)]])) {
    x[[as_name(zmW)]] = factor(x[[as_name(zmW)]],
                               sort(unique(x[[as_name(zmW)]])))
  }
  x[[as_name(zmW)]] = addNA(x[[as_name(zmW)]], ifany = TRUE)
  if (!is.factor(x[[as_name(zmK)]])) {
    x[[as_name(zmK)]] = factor(x[[as_name(zmK)]],
                               sort(unique(x[[as_name(zmK)]])))
  }
  x[[as_name(zmK)]] = addNA(x[[as_name(zmK)]], ifany = TRUE)

  # samo przygotowanie rozkładu
  x = count(x, !!zmW, !!zmK, wt = !!w, .drop = FALSE)

  x = sformatuj_rozklad2(x, zmW, zmK, sumowanie, liczby, procenty,
                         etykietaSuma, etykietaOgolem, etykietaBD, klasyZm,
                         labels)
  return(x)
}
#' @rdname tab2
#' @export
tab2.table = function(x, zmW, zmK, sumowanie = c("brak", "kolumny",
                                                 "wiersze", "ogółem"),
                      liczby = TRUE, procenty = TRUE,
                      etykietaSuma = "SUMA", etykietaOgolem = "OGÓŁEM",
                      etykietaBD = NA, ...) {
  zmW = ensym(zmW)
  zmK = ensym(zmK)
  x = as.data.frame(x)
  names(x)[ncol(x)] = "___Freq___"
  tab2(x, !!zmW, !!zmK, sumowanie = sumowanie, liczby = liczby,
       procenty = procenty, etykietaSuma = etykietaSuma,
       etykietaOgolem = etykietaOgolem, etykietaBD = etykietaBD,
       w = "___Freq___")
}
#' @rdname tab2
#' @export
tab2.ftable = function(x, zmW, zmK, sumowanie = c("brak", "kolumny",
                                                  "wiersze", "ogółem"),
                       liczby = TRUE, procenty = TRUE,
                       etykietaSuma = "SUMA", etykietaOgolem = "OGÓŁEM",
                       etykietaBD = NA, ...) {
  zmW = ensym(zmW)
  zmK = ensym(zmK)
  x = as.data.frame(x)
  names(x)[ncol(x)] = "___Freq___"
  tab2(x, !!zmW, !!zmK, sumowanie = sumowanie, liczby = liczby,
       procenty = procenty, etykietaSuma = etykietaSuma,
       etykietaOgolem = etykietaOgolem, etykietaBD = etykietaBD,
       w = "___Freq___")
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
  if (is.null(etykietaBD)) {
    x = filter(x, !is.na(!!zmW) & !is.na(!!zmK))
  }
  etykiety = assert_tab2(colnames(x), zmW, zmK, liczby, procenty, etykietaSuma,
                         etykietaOgolem, etykietaBD)
  etykietaOgolem = etykiety$etykietaOgolem
  etykietaBD = etykiety$etykietaBD

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
  if (!is.factor(x$variables[[as_name(zmW)]])) {
    x$variables[[as_name(zmW)]] = factor(x$variables[[as_name(zmW)]],
                                         sort(unique(x$variables[[as_name(zmW)]])))
  }
  x$variables[[as_name(zmW)]] = addNA(x$variables[[as_name(zmW)]], ifany = TRUE)
  if (!is.factor(x$variables[[as_name(zmK)]])) {
    x$variables[[as_name(zmK)]] = factor(x$variables[[as_name(zmK)]],
                                         sort(unique(x$variables[[as_name(zmK)]])))
  }
  x$variables[[as_name(zmK)]] = addNA(x$variables[[as_name(zmK)]], ifany = TRUE)

  x = survey_count(x, !!zmW, !!zmK, vartype = NULL, .drop = FALSE)

  x = sformatuj_rozklad2(x, zmW, zmK, sumowanie, liczby, procenty,
                         etykietaSuma, etykietaOgolem, etykietaBD, klasyZm,
                         labels)
  return(x)
}
#' @rdname tab2
#' @importFrom srvyr as_survey
#' @export
tab2.survey.design2 = function(x, zmW, zmK, sumowanie = c("brak", "kolumny",
                                                          "wiersze", "ogółem"),
                               liczby = TRUE, procenty = TRUE,
                               etykietaSuma = "SUMA", etykietaOgolem = "OGÓŁEM",
                               etykietaBD = NA, ...) {
  zmW = ensym(zmW)
  zmK = ensym(zmK)
  tab2(as_survey(x), !!zmW, !!zmK, sumowanie = sumowanie, liczby = liczby,
       procenty = procenty, etykietaSuma = etykietaSuma,
       etykietaOgolem = etykietaOgolem, etykietaBD = etykietaBD)
}
#' @rdname tab2
#' @importFrom srvyr as_survey
#' @export
tab2.svyrep.design = function(x, zmW, zmK, sumowanie = c("brak", "kolumny",
                                                         "wiersze", "ogółem"),
                              liczby = TRUE, procenty = TRUE,
                              etykietaSuma = "SUMA", etykietaOgolem = "OGÓŁEM",
                              etykietaBD = NA, ...) {
  zmW = ensym(zmW)
  zmK = ensym(zmK)
  tab2(as_survey(x), !!zmW, !!zmK, sumowanie = sumowanie, liczby = liczby,
       procenty = procenty, etykietaSuma = etykietaSuma,
       etykietaOgolem = etykietaOgolem, etykietaBD = etykietaBD)
}
#' @rdname tab2
#' @importFrom srvyr as_survey
#' @export
tab2.twophase2 = function(x, zmW, zmK, sumowanie = c("brak", "kolumny",
                                                     "wiersze", "ogółem"),
                          liczby = TRUE, procenty = TRUE,
                          etykietaSuma = "SUMA", etykietaOgolem = "OGÓŁEM",
                          etykietaBD = NA, ...) {
  zmW = ensym(zmW)
  zmK = ensym(zmK)
  tab2(as_survey(x), !!zmW, !!zmK, sumowanie = sumowanie, liczby = liczby,
       procenty = procenty, etykietaSuma = etykietaSuma,
       etykietaOgolem = etykietaOgolem, etykietaBD = etykietaBD)
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
