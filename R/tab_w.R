#' @title Tabela z rozkladem wielokrotnych odpowiedzi
#' @description
#' Na podstawie obiektu zwróconego przez funkcję \code{\link{tab_n}} funkcja
#' generuje tabelę zawierającą rozkład odpowiedzi na pytanie, w którym można
#' było wybrać więcej niż jedną pozycję kafeterii (tzw. wielokrotne odpowiedzi).
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
#' @param wybrane wektor wartości, które wskazują na to, że pozycja została
#' wybrana (jego elementami mogą być ciągi znaków, nazwy podawane jako wyrażenia
#' języka oraz selektory z pakietu \emph{dplyr}, np. \code{\link{starts_with}})
#' @param wyklucz wektor wartości, które \strong{nie} powinny być wliczane do
#' podstawy procentowania (jego elementami mogą być ciągi znaków, nazwy podawane
#' jako wyrażenia języka oraz selektory z pakietu \emph{dplyr}, np.
#' \code{\link{starts_with}})
#' @param kierunek w którą stronę mają układać się w zwracanym zestawieniu
#' pozycje kafeterii?
#' @param liczby wartość logiczna - czy zwracana tabela ma zawierać rozkłady
#' liczebności?
#' @param procenty wartość logiczna - czy zwracana tabela ma zawierać rozkłady
#' częstości?
#' @param etykietaOgolem ciąg znaków - etykieta dla wiersza lub kolumy z łączną
#' liczbą obserwacji (podstawą procentowania)
#' @param etykietyPozycji wartość logiczna - czy do opisania pozycji w kafterii
#' w zwracanym zestawieniu mają zostać wykorzystane etykiety (o ile są
#' dostępne)? jeśli \code{FALSE}, użyte zostaną nazwy odpowiednich zmiennych
#' @param prefiksPozycji ciąg znaków - część etykiet zmiennych (pozycji), które
#' opisują treść główki pytania - jeśli podany, zostanie usunięty z etykiet
#' pozycji i ustawiony jako etykieta całej tabeli
#' @param wykluczBD wartość logiczna - czy z podstawy procentowania wykluczać
#' również braki danych?
#' @return
#' \strong{tab_w}
#' \code{data.frame} (klasy \code{tab_lbl_n}) z rozkładami:
#'
#' \strong{metoda as_tibble}
#' W zależności od wartości parametru \code{niePrzeksztalcaj}:
#' \itemize{
#'   \item{\emph{tibble} z rozkładami przekształconymi do postaci \emph{długiej},}
#'   \item{obiekt klasy \code{data.frame} (i tylko tej jednej) z rozkładami
#'         w postacie \emph{szerokiej}.}
#' }
#' @name tab_w
#' @importFrom dplyr as_tibble bind_rows distinct group_by summarise ungroup
#' @importFrom rlang enquo quo_is_missing
#' @export
tab_w = function(x, wybrane, wyklucz, kierunek = c("kolumny", "wiersze"),
                 liczby = TRUE, procenty = TRUE, etykietaOgolem = "OGÓŁEM",
                 etykietyPozycji = TRUE, prefiksPozycji = "", wykluczBD = TRUE) {
  kierunek = match.arg(kierunek)
  wybrane = enquo(wybrane)
  wyklucz = enquo(wyklucz)
  if (quo_is_missing(wybrane)) {
    stop("Nie podano, które wartości mają być zliczone.")
  }
  # sztuczka, żeby liczby nie były interpretowane później jako indeksy
  wybrane = quo_liczby_na_tekst(wybrane)
  wyklucz = quo_liczby_na_tekst(wyklucz)
  stopifnot(inherits(x, "tab_lbl_n"), is.null(attributes(x)$etykietaOgolem),
            is.logical(liczby), length(liczby) == 1,
            is.logical(procenty), length(procenty) == 1,
            is.logical(etykietyPozycji), length(etykietyPozycji) == 1,
            is.logical(wykluczBD), length(wykluczBD) == 1,
            is.character(etykietaOgolem), length(etykietaOgolem) == 1,
            is.character(prefiksPozycji), length(prefiksPozycji) == 1)
  stopifnot(liczby %in% c(TRUE, FALSE),
            procenty %in% c(TRUE, FALSE),
            etykietyPozycji %in% c(TRUE, FALSE),
            wykluczBD %in% c(TRUE, FALSE))

  atrybuty = attributes(x)
  x = as_tibble(x)
  pozycje = unique(x$zmienna)
  wartosci =
    setdiff(unique(x$wartosc),
            c(atrybuty$etykietaSuma,
              ifelse(atrybuty$kierunek %in% "wiersze" & is.na(atrybuty$etykietaBD),
                     "NA", NA)))
  # sztuczka z tym, żeby móc wykorzystać "selection helpers" z dplyra
  # do wyboru wartości
  wartosci = lapply(setNames(vector(mode = "list", length = length(wartosci)),
                             wartosci),
                    function(x) {return(vector(length = 0))})
  wartosci = as.data.frame(wartosci, check.names = FALSE)
  doZliczania = tryCatch(names(select(wartosci, !!wybrane)),
                         error = function(x) {stop("Wybrano wartości do zliczania, które nie występują w zestawieniu.")})
  doWykluczenia = tryCatch(names(select(wartosci, !!wyklucz)),
                           error = function(x) {stop("Wybrano wartości do wykluczenia, które nie występują w zestawieniu.")})
  if (wykluczBD) {
    doWykluczenia =
      c(doWykluczenia,
        ifelse(atrybuty$kierunek %in% "kolumny" & is.na(atrybuty$etykietaBD),
               NA, "NA"),
        atrybuty$etykietaBD)
    doWykluczenia = unique(doWykluczenia)
  }

  x = x[!(x$wartosc %in% c(doWykluczenia, atrybuty$etykietaSuma)), ]
  x$wartosc = ifelse(x$wartosc %in% doZliczania, 1, 0)
  x = group_by(x, .data$rozklad, .data$zmienna)
  x = summarise(x,
                wybrane = sum(.data$value[.data$wartosc %in% 1]),
                wszystkie = sum(.data$value))
  x = ungroup(x)
  temp = x
  temp$zmienna = etykietaOgolem
  temp$wybrane = temp$wszystkie
  temp = distinct(temp)
  if (any(table(temp$rozklad) > 1)) {
    warning("W poszczególnych zmiennych jest różna podstawa procentowania.")
  } else {
    x = bind_rows(x, temp)
  }
  x$value = ifelse(x$rozklad %in% "pct",
                   100 * x$wybrane / x$wszystkie,
                   x$wybrane)
  x = x[, c("rozklad", "zmienna", "value")]

  # ew. zamiana nazw zmiennych na etykiety (lub odwrotnie)
  if (etykietyPozycji & all(pozycje %in% names(atrybuty$label))) {
    x$zmienna = factor(x$zmienna,
                       c(names(atrybuty$label), etykietaOgolem),
                       c(atrybuty$label, etykietaOgolem))
    x$zmienna = levels(x$zmienna)[x$zmienna]
  } else if (!etykietyPozycji & all(pozycje %in% atrybuty$label)) {
    x$zmienna = factor(x$zmienna,
                       c(atrybuty$label, etykietaOgolem),
                       c(names(atrybuty$label), etykietaOgolem))
    x$zmienna = levels(x$zmienna)[x$zmienna]
  }
  # ew. wyciągenie prefiksu z etykiet pozycji
  if (!(prefiksPozycji %in% c("", NA))) {
    atrybuty$label = trimws(sub("[ -:]+$", "", prefiksPozycji))
    if (etykietyPozycji) {
      x$zmienna = trimws(sub(prefiksPozycji, "", x$zmienna, fixed = TRUE))
    }
  }
  x$zmienna = factor(x$zmienna, unique(x$zmienna))

  if (!liczby) {
    x = x[!x$rozklad %in% "n", ]
  }
  if (!procenty) {
    x = x[!x$rozklad %in% "pct", ]
  }
  # przekształcanie do postaci szerokiej
  if (kierunek %in% "wiersze") {
    x$wartosc = "wybrane"
    x = pivot_wider(x, names_from = c(.data$rozklad, .data$zmienna),
                    values_from = .data$value)
  } else {
    x$rozklad = paste0(x$rozklad, "_wybrane")
    x = pivot_wider(x, names_from = .data$rozklad, values_from = .data$value)
    names(x)[1] = "pozycja"
  }

  class(x) = c("tab_lbl_n", class(x))
  attributes(x)$nazwyZm = pozycje
  attributes(x)$klasyZm = atrybuty$klasyZm
  attributes(x)$label = atrybuty$label
  attributes(x)$kierunek = kierunek
  attributes(x)$etykietaSuma = NA
  attributes(x)$etykietaBD = NA
  attributes(x)$etykietaOgolem = etykietaOgolem
  return(x)
}
#' @importFrom rlang is_quosure quo_is_missing
quo_liczby_na_tekst = function(x) {
  stopifnot(is_quosure(x))
  if (!quo_is_missing(x)) {
    for (i in 1:length(x[[2]])) {
      if (is.numeric(x[[2]][[i]]) || is.logical(x[[2]][[i]])) {
        x[[2]][[i]] = as.character(x[[2]][[i]])
      }
    }
  }
  return(x)
}
