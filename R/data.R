#' @title Przykladowe dane
#' @description Całkowicie artefaktualny zbiór, zawierający niezależnie od
#' siebie wygenerowane zmienne, za to różnych typów. Pozwala pokazać, jak
#' zachowują się funkcje pakietu, gdy są wywoływane z kolumnami różnych typów.
#' @format Ramka danych ze 100 wierszami i 6 kolumnami:
#' \itemize{
#'   \item{\code{id} - identyfikator obserwacji,}
#'   \item{\code{zm1} - zmienna numeryczna, zawiera braki danych,}
#'   \item{\code{zm2} - czynnik (\code{factor}), bez braków danych,}
#'   \item{\code{zm3} - czynnik (\code{factor}), zawiera danych,}
#'   \item{\code{zm4} - zmienna etykietowana (\code{haven_labelled}), bez braków
#'         danych,}
#'   \item{\code{waga} - zmienna numeryczna, która może być wykorzystana jako
#'         wagi obserwacji.}
#' }
"przyklad"
#' @title Przykladowe dane - wielokrotne odpowiedzi
#' @description Całkowicie artefaktualny zbiór symulujący wyniki odpowiedzi na
#' dwa pytania ankiety, w których respondent mógł zaznaczyć więcej niż jedną
#' odpowiedź. Zawierający niezależnie od siebie wygenerowane zmienne, za to
#' różnych typów. Pozwala pokazać, jak zachowują się funkcje pakietu, gdy są
#' wywoływane z kolumnami różnych typów.
#' @format Ramka danych ze 50 wierszami i 8 kolumnami:
#' \itemize{
#'   \item{\code{id} - identyfikator obserwacji,}
#'   \item{\code{P1_1} - zmienna tekstowa,}
#'   \item{\code{P1_2} - czynnik (\code{factor}),}
#'   \item{\code{P1_3} - zmienna etykietowana (\code{haven_labelled})},
#'   \item{\code{P2_1} - zmienna numeryczna,}
#'   \item{\code{P2_2} - czynnik (\code{factor}),}
#'   \item{\code{P2_3} - zmienna etykietowana (\code{haven_labelled}),}
#'   \item{\code{waga} - zmienna numeryczna, która może być wykorzystana jako
#'         wagi obserwacji.}
#' }
#' Zmienne \code{P1_1}-\code{P1_3} różnią się formatem, ale przyjmują wartości
#' z tego samego zbioru (\code{P1_3} - po konwersji na czynnik). Zmienne
#' \code{P2_1}-\code{P2_3} różnią się formatem i przyjmują różne wartości.
#' Wszystkie zmienne zawierają braki danych (które występują w dokładnie tych
#' samych obserwacjach w całym zbiorze).
"przykladWO"
