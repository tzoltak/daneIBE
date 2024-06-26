% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tab_n.R, R/tab_n_metody.R
\name{tab_n}
\alias{tab_n}
\alias{tab_n.data.frame}
\alias{tab_n.table}
\alias{tab_n.ftable}
\alias{tab_n.tbl_svy}
\alias{tab_n.survey.design2}
\alias{tab_n.svyrep.design}
\alias{tab_n.twophase2}
\alias{print.tab_lbl_n}
\alias{as_tibble.tab_lbl_n}
\alias{as.data.frame.tab_lbl_n}
\title{Tabela rozkladow brzegowych wielu zmiennych (etykietowanych) o tym
samym zbiorze mozliwych wartosci}
\usage{
tab_n(
  x,
  ...,
  kierunek,
  liczby,
  procenty,
  etykietaSuma,
  etykietyZmiennych,
  etykietaBD
)

\method{tab_n}{data.frame}(
  x,
  ...,
  kierunek = c("kolumny", "wiersze"),
  liczby = TRUE,
  procenty = TRUE,
  etykietaSuma = "SUMA",
  etykietyZmiennych = FALSE,
  etykietaBD = NA,
  w = NULL
)

\method{tab_n}{table}(
  x,
  ...,
  kierunek = c("kolumny", "wiersze"),
  liczby = TRUE,
  procenty = TRUE,
  etykietaSuma = "SUMA",
  etykietyZmiennych = FALSE,
  etykietaBD = NA
)

\method{tab_n}{ftable}(
  x,
  ...,
  kierunek = c("kolumny", "wiersze"),
  liczby = TRUE,
  procenty = TRUE,
  etykietaSuma = "SUMA",
  etykietyZmiennych = FALSE,
  etykietaBD = NA
)

\method{tab_n}{tbl_svy}(
  x,
  ...,
  kierunek = c("kolumny", "wiersze"),
  liczby = TRUE,
  procenty = TRUE,
  etykietaSuma = "SUMA",
  etykietyZmiennych = FALSE,
  etykietaBD = NA
)

\method{tab_n}{survey.design2}(
  x,
  ...,
  kierunek = c("kolumny", "wiersze"),
  liczby = TRUE,
  procenty = TRUE,
  etykietaSuma = "SUMA",
  etykietyZmiennych = FALSE,
  etykietaBD = NA
)

\method{tab_n}{svyrep.design}(
  x,
  ...,
  kierunek = c("kolumny", "wiersze"),
  liczby = TRUE,
  procenty = TRUE,
  etykietaSuma = "SUMA",
  etykietyZmiennych = FALSE,
  etykietaBD = NA
)

\method{tab_n}{twophase2}(
  x,
  ...,
  kierunek = c("kolumny", "wiersze"),
  liczby = TRUE,
  procenty = TRUE,
  etykietaSuma = "SUMA",
  etykietyZmiennych = FALSE,
  etykietaBD = NA
)

\method{print}{tab_lbl_n}(x, dProcenty = 1, dLiczby = 0, decimal.mark = ",", scipen = 100, ...)

\method{as_tibble}{tab_lbl_n}(
  x,
  ...,
  .rows = NULL,
  .name_repair = c("check_unique", "unique", "universal", "minimal"),
  rownames = NULL,
  usunSuma = FALSE,
  wartoscBD = NULL
)

\method{as.data.frame}{tab_lbl_n}(
  x,
  row.names = NULL,
  optional = FALSE,
  ...,
  usunSuma = FALSE,
  wartoscBD = NULL,
  niePrzeksztalcaj = TRUE
)
}
\arguments{
\item{x}{ramka danych lub obiekt klasy \code{tbl_svy}}

\item{...}{zmienne, których rozkłady mają zostać zwrócone (można używać
selektorów pakietu \emph{dplyr}, np. \code{\link[dplyr]{starts_with}})}

\item{kierunek}{w którą stronę mają układać się w zwracanym zestawieniu
rozkłady brzegowe?}

\item{liczby}{wartość logiczna - czy zwracana tabela ma zawierać rozkłady
liczebności?}

\item{procenty}{wartość logiczna - czy zwracana tabela ma zawierać rozkłady
częstości?}

\item{etykietaSuma}{ciąg znaków - etykieta dla wiersza lub kolumy z sumą}

\item{etykietyZmiennych}{wartość logiczna - czy do opisania zmiennych
w zwracanym zestawieniu mają zostać wykorzystane ich etykiety (o ile są
dostępne)? jeśli \code{FALSE}, użyte zostaną nazwy zmiennych}

\item{etykietaBD}{ciąg znaków - etykieta, którą w przygotowanym zestawieniu
mają być opisane braki danych (\code{NA}); domyślna wartość oznacza, że
zostaną one opisane jako "NA"; podanie \code{NULL} będzie skutkować
usunięciem kolumn i wierszy opisujących braki danych ze zwracanego
zestawienia}

\item{w}{opcjonalnie kolumna obiektu \code{x}, której wartości zawierają
wagi obserwacji, które powinny zostać uwzględnione przy obliczaniu rozkładu}

\item{dProcenty}{liczba miejsc dziesiętnych, do jakiej zostaną zaokrąglone
wartości kolumn z rozkładami częstości}

\item{dLiczby}{liczba miejsc dziesiętnych, do jakiej zostaną zaokrąglone
wartości kolumn z rozkładami liczebności (to mogą być liczby niecałkowite,
jeśli przy tworzeniu rozkładu stosowano ważenie)}

\item{decimal.mark}{znak miejsca dziesiętnego - przekazywany do
\code{\link[base]{format}}}

\item{scipen}{liczba całkowita decydująca o skłonności R do zapisywania liczb
w notacji naukowej (czym większa, tym rzadziej R sosuje notację naukową - p.
\code{\link[base]{options}})}

\item{.rows}{wyłącznie dla zgodności ze wzorcem (\emph{generic}) metody
\code{as_tibble}}

\item{.name_repair}{przekazywana jako argument \code{names_repair} do
\code{\link[tidyr]{pivot_longer}}}

\item{rownames}{wyłącznie dla zgodności ze wzorcem (\emph{generic}) metody
\code{as_tibble}}

\item{usunSuma}{wartość logiczna - czy usunąć ze zwróconej ramki danych
wiersze opisujące sumy rozkładów?}

\item{wartoscBD}{wektor jednoelementowy: wartość, przy pomocy
których mają być reprezentowane ew. braki danych w zróconej ramce danych}

\item{row.names}{wyłącznie dla zgodności ze wzorcem (\emph{generic}) metody
\code{as.data.frame}}

\item{optional}{wyłącznie dla zgodności ze wzorcem (\emph{generic}) metody
\code{as.data.frame}}

\item{niePrzeksztalcaj}{wartość logiczna - czy funkcja ma tylko usunąć
z obiektu klasę \code{tab_lbl_n} i zwrócić go jako \emph{zwykłą} ramkę danych,
\strong{bez} dokonywania przekształcenia z postaci \emph{szerokiej} do
\emph{długiej})? (domyślnie funkcja dokonuje przekształcenia)}
}
\value{
\strong{tab_n}
\code{data.frame} (klasy \code{tab_lbl_n}) z rozkładami:
\itemize{
  \item{w zależności od wartości argumentu, \code{kierunek} w pierwszej
        kolumnie będą znajować się albo wartości zmiennych
        (\code{kierunek="kolumny"}) albo nazwy zmiennych
        (\code{kierunek="wiersze"}) - kolumna zawsze jest czynnikiem
        (którego kolejność poziomów odpowiada kolejności, w jakiej wybrane
        zostały zmienne w wywołaniu \code{\link{tab_n}}),}
  \item{nazwy kolejnych kolumn zaczynające się od "n_" opisują rozkłady
        liczebności,}
  \item{nazwy kolejnych kolumn zaczynające się od "pct_" opisują rozkłady
        częstości,}
  \item{nazwy tych kolumn powstały przez połącznie przedrostka "n_" lub
        "pct_" z nazwami zmiennych \code{kierunek="kolumny"} lub
        z wartościami zmiennych \code{kierunek="wiersze"} oraz
        ew. etykietą podaną argumentem \code{etkietaSuma}.}
}
\strong{metoda as_tibble}
W zależności od wartości parametru \code{niePrzeksztalcaj}:
\itemize{
  \item{\emph{tibble} z rozkładami przekształconymi do postaci \emph{długiej},}
  \item{obiekt klasy \code{data.frame} (i tylko tej jednej) z rozkładami
        w postacie \emph{szerokiej}.}
}
}
\description{
Funkcja generuje tabelę zestawiającą ze sobą rozkłady brzegowe liczebności
i rozkład brzegowe częstości kilku zmiennych, które mogą przyjmować ten sam
zbiór wartości (a przynajmniej tak zakładamy). Jako pierwszy argument
przyjmuje obiekt \code{data.frame} lub \code{tbl_svy} (obiekt będący
połączeniem \emph{ramki danych} z informacjami o - zwykle złożonym -
schemacie doboru próby, tworzonym przez funkcje pakietu \emph{srvyr}). Radzi
też sobie ze zmiennymi etykietowanymi (konwertując je na \emph{czynniki}).

Metoda \code{as_tibble} pozwala przekształcić zwracane zestawienie
w ramkę danych w postaci \emph{długiej}, przydatną np. do rysowania wykresów
przy pomocy \emph{ggplot2} (czyli działa analogicznie,jak metoda
\code{as.data.frame} dla obiektów zwracanych przez funkcję
\code{\link[base]{table}} z pakietu \emph{base}). Metoda \code{as.data.frame}
również jest dostępna, ale domyślnie nie przekształca tabeli (usuwa jedynie
klasę \code{tab_n}), gdyż inaczej przygotowanych tabel z rozkładami nie
dawałoby się bezproblemowo używać w połączeniu z funkcją
\code{\link[knitr]{kable}} z pakietu \emph{kable} (która to funkcja wywołuje
metodę \code{as.data.frame} na przekazywanym jej obiekcie).
}
\examples{
tab_n(przykladWO, P1_1, P1_2, P1_3)
tab_n(przykladWO, starts_with("P1_"))
tab_n(przykladWO, P1_1, P1_2, P1_3, kierunek = "w")
tab_n(przykladWO, P1_1, P1_2, P1_3, kierunek = "w",  etykietyZmiennych = TRUE)
tab_n(przykladWO, P1_1, P1_2, P1_3, procenty = FALSE)
tab_n(przykladWO, P1_1, P1_2, P1_3, etykietaSuma = "Suma")
tab_n(przykladWO, P1_1, P1_2, P1_3, etykietaBD = "brak danych")

tab_n(przykladWO, P1_1, P1_2, P1_3, w = waga)
as.data.frame(tab_n(przykladWO, P1_1, P1_2, P1_3))
# aby uzyskać obiekt nadający się do wykorzystania
# jako źródło danych do wykresu tworzonego przy pomocy pakietu ggplot2:
dplyr::as_tibble(tab_n(przykladWO, P1_1, P1_2, P1_3), usunSuma = TRUE)
}
