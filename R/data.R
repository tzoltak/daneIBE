#' @title Przykladowe dane.
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
