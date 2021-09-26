# daneIBE

Funkcje wspomagajęce przetwarzanie danych sondażowych, w szczególności pewne elemnty pracy ze zmiennymi etykietowanymi i szybkie tworzenie rozkładów zmiennych.

## Instalacja

Zakładając, że masz zainstalowany pakiet *remotes* (jeśli nie, zainstaluj go: `install.pcakges("remotes")`), wywołaj w konsoli R:

```{r}
remotes::install_github("tzoltak/daneIBE")
```

Aby załadować zainstalowany pakiet do danej sesji R, wywołaj:

```{r}
library(daneIBE)
```

## Funkcje pakietu

### Tabele z rozkładami

#### Funkcja tab()

Funkcja `tab()` tworzy tabelę z rozkładem liczebności i rozkładem częstości jednej zmiennej.

Jest przeznaczona przede wszystkim do analiz eksploracyjnych z wykorzystaniem zmiennych *etykietowanych* - pozwala obejrzeć ich rozkład, wraz z mapowaniem wartości-etykiety bez konieczności konwertowania ich na inny typ zmiennej.

Może być wywołana na:

-   wektorze (w tym *czynniku*),

-   *ramce danych*,

-   obiekcie *tbl\_svy* (czyli obiekcie będącym połączeniem *data.frame* z informacjami o złożonym schemacie doboru próby, tworzonym przez funkcje pakietu *srvyr*);

-   obiekcie *survey.design* lub *svyrep.design* (obiektach ze złożonymi schematami doboru próby, tworzonymi przez funkcje pakietu *survey*)

W każdym poza pierwsztm spośród ww. przypadków drugim argumentem podaje się zmienną (kolumnę), której rozkład ma być wygenerowany (nazwa kolumny może być podana jako wyrażenie języka lub jako ciąg znaków, tj. w odniesieniu do swojego drugiego argumentu `tab()` obsługuje *tidy evaluation*).

W przypadku obiektów ze schematami doboru próby (*tbl\_svy*, *survey.design* lub *svyrep.design*) przy tworzeniu rozkładu automatycznie uwzględnione zostanie ważenie wynikające ze schematu doboru próby. W innych przypadkach wektor wag można podać dodatkowym argumentem `w`.

Przykład użycia:

```{r}
tab(mtcars$cyl)
tab(mtcars, cyl)
tab(mtcars, cyl, etykietaSuma = "Sum")

library(haven)
mtcars$cyl = labelled(mtcars$cyl, c("cztery" = 4, "sześć" = 6, "osiem" = 8),
                      "liczba cylindrów")
tab(mtcars, cyl)

library(survey)
mtcarsSvy = svydesign(~1, 1, data = mtcars)
tab(mtcarsSvy, cyl)

library(srvyr)
mtcarsSrv = as_survey(mtcarsSvy)
tab(mtcarsSrv, cyl)
```

#### Funkcja tab2()

Funkcja generuje rozkład łączny liczebności i rozkład łączny lub rodzinę warunkowych rozkładów częstości dwóch zmiennych. Jako pierwszy argument przyjmuje *ramkę danych*, obiekt *tbl\_svy*, *survey.design* lub *svyrep.design* (będące połączeniem *data.frame* z informacjami o złożonym schemacie doboru próby, tworzony przez funkcje pakietów, odpowiednio *srvyr* lub *survey*) albo *table* lub *ftable* (zwracane przez funkcje o tych samych nazwach, zawarte w podstawowej instlacji R). Radzi też sobie ze zmiennymi etykietowanymi (konwertując je na *czynniki*).

Metoda `as_tibble` pozwala przekształcić zwracane zestawienie w *ramkę danych* (ściśle w *tibble*) w postaci *długiej*, przydatną np. do rysowania wykresów przy pomocy *ggplot2* (czyli działa analogicznie, jak metoda `as.data.frame` dla obiektów zwracanych przez funkcję `table` z pakietu *base*).

Argumenty:

1.  *Ramka danych*, obiekt *tbl\_svy*, *survey.design*, *svyrep.design*, *table* lub *ftable*.

2.  Zmienna, której wartości mają zostać umieszczone w wierszach tabeli.

3.  Zmienna, której wartości mają zostać umieszczone w kolumnach tabeli.

4.  Kierunek procentowania (w wywołaniu funkcji wystarczy podać pierwszą literę):

    -   "brak" (domyślnie),
    -   "kolumny",
    -   "wiersze",
    -   "ogółem".

5.  Dalsze argumenty - p. dokumentacja (aby ją obejrzeć wywołaj: `?tab2`).

Przykłady użycia:

```{r}
library(haven)
mtcars$cyl = labelled(mtcars$cyl, c("cztery" = 4, "sześć" = 6, "osiem" = 8),
                      "liczba cylindrów")
tab2(mtcars, carb, cyl, "k")
tab2(mtcars, carb, cyl, "w")
tab2(mtcars, carb, cyl, "o")
tab2(mtcars, carb, cyl)
tab2(mtcars, carb, cyl, "k", liczby = FALSE)
tab2(mtcars, carb, cyl, "k", procenty = FALSE)
tab2(mtcars, carb, cyl, "k", etykietaSuma = "Suma")
mtcars$carb[1:2] = NA
tab2(mtcars, carb, cyl, "k")
tab2(mtcars, carb, cyl, "k", etykietaBD = "brak danych")

# choć hp jako wagi obserwacji są bez sensu
tab2(mtcars, carb, cyl, "k", w = hp)

as.data.frame(tab2(mtcars, cyl, carb, "k"))
# aby uzyskać obiekt nadający się do wykorzystania
# jako źródło danych do wykresu tworzonego przy pomocy pakietu ggplot2:
as_tibble(tab2(mtcars, cyl, carb, "k"), usunSuma = TRUE)
# lub
as_tibble(tab2(mtcars, cyl, carb, "k"), usunSuma = TRUE, usunOgolem = TRUE)
```

#### Funkcja tab\_n()

Funkcja generuje tabelę zestawiającą ze sobą rozkłady brzegowe liczebności i rozkład brzegowe częstości kilku zmiennych, które mogą przyjmować ten sam zbiór wartości (funkcja radzi sobie również, gdy zbiory wartości są różne, ale powstałe zestawienie raczej nie będzie wedy czytelne). Jako pierwszy argument przyjmuje *ramkę danych* lub obiekt *tbl\_svy* (będący połączeniem *data.frame* z informacjami o złożonym schemacie doboru próby, tworzony przez funkcje pakietu *srvyr*). Radzi też sobie ze zmiennymi etykietowanymi (konwertując je na *czynniki*).

Należy skorzystać z niej jako pierwszego kroku na drodze do uzyskania tabeli z zestawieniem *wielokrotnych odpowiedzi* (p. opis funkcji `tab_w()`).

Metoda `as_tibble` pozwala przekształcić zwracane zestawienie w *ramkę danych* (ściśle w *tibble*) w postaci *długiej*, przydatną np. do rysowania wykresów przy pomocy *ggplot2* (czyli działa analogicznie, jak metoda `as.data.frame` dla obiektów zwracanych przez funkcję `table` z pakietu *base*).

Metoda `as.data.frame` również jest dostępna, ale domyślnie nie przekształca tabeli (usuwa jedynie klasę *tab\_n*), gdyż inaczej przygotowanych tabel z rozkładami nie dawałoby się bezproblemowo używać w połączeniu z funkcją `kable` z pakietu *kable* (która to funkcja metodę `as.data.frame` na przekazywanym jej obiekcie).

Argumenty:

1.  *Ramka danych*, obiekt *tbl\_svy*, *survey.design*, *svyrep.design*, *table* lub *ftable*.

2.  Zmienne, których rozkłady mają zostać zwrócone podawane jako kolejne argumenty (można używać selektorów pakietu *dplyr*, np. `starts_with`).

3.  Dalsze argumenty - p. dokumentacja (aby ją obejrzeć wywołaj: `?tab_n`).

Przykłady użycia:

```{r}
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
as_tibble(tab_n(przykladWO, P1_1, P1_2, P1_3), usunSuma = TRUE)
```

#### Funkcja tab\_w()

Na podstawie obiektu zwróconego przez funkcję `tab_n()` funkcja generuje tabelę zawierającą rozkład odpowiedzi na pytanie, w którym można było wybrać więcej niż jedną pozycję kafeterii (tzw. wielokrotne odpowiedzi).

Również dla tego typu zestawień można wykorzystać metodę `as_tibble`, aby uzyskać *ramkę danych* (ściśle w *tibble*) w postaci *długiej*, przydatną np. do rysowania wykresów przy pomocy *ggplot2* (por. opisy funkcji powyżej).

Argumenty:

1.  Obiekt zwrócony przez funkcję `tab_n()`.

2.  Wektor wartości, które wskazują na to, że pozycja została wybrana (jego elementami mogą być ciągi znaków, nazwy podawane jako wyrażenia języka oraz selektory z pakietu *dplyr*, np. `starts_with`).

3.  Opcjonalnie wektor wartości, które \strong{nie} powinny być wliczane do podstawy procentowania (jego elementami mogą być ciągi znaków, nazwy podawane jako wyrażenia języka oraz selektory z pakietu *dplyr*, np. `starts_with`).

4.  Dalsze argumenty - p. dokumentacja (aby ją obejrzeć wywołaj: `?tab_w`).

```{r}
polprodukt = tab_n(przykladWO, starts_with("P1_"))
polprodukt

tab_w(polprodukt, "T")
tab_w(polprodukt, "T", wykluczBD = FALSE)
tab_w(polprodukt, "T", prefiksPozycji = "Jakie kwiaty lubisz:")
tab_w(polprodukt, "T", kierunek = "w", prefiksPozycji = "Jakie kwiaty lubisz:")
tab_w(polprodukt, "T", procenty = FALSE)
tab_w(polprodukt, "T", etykietaOgolem = "Ogółem")

# nieco bardziej skomplikowana sytuacja
polprodukt2 = tab_n(przykladWO, starts_with("P2_"))
polprodukt2
tab_w(polprodukt2, c("T", 1, "Tak"))

# nie ma to tutaj merytorycznego sensu, i prowadzi do ostrzeżenia ale jako 
# przykład, że można podać wartości, które mają zostać wykluczone z podstawy
#procentowania
tab_w(polprodukt2, c("T", 1, "Tak"), c(starts_with("n"), 0))

# aby uzyskać obiekt nadający się do wykorzystania
# jako źródło danych do wykresu tworzonego przy pomocy pakietu ggplot2:
 %>% 
```

## Funkcje wspomagające pracę ze zmiennymi etykietowanymi wczytanych przy pomocy pakietu *haven*

#### Etykiety zmiennych

##### Funkcja labels()

-   Wywołana na obiekcie *data.frame* zwraca listę etykiet zmiennych (etykiety są zawarte w elementach listy, a nazwy zmiennych w nazwach tych elementów).
-   Wywołana na pojedynczej zmiennej (kolumnie) obiektu *data.frame* zwraca wektor z etykietami wartości (wartości są zawarte w elementach wektora, a powiązane z nimi etykiety stanowią nazwy tych elementów).

Obiekt zwrócony przez funkcję `labels()` (efekt działania tej funkcji) można następnie:

-   wyświetlić na konsoli - zawarta w pakiecie metoda `print()` zapewnia przyjazne sformatowanie wyniku, jeśli tylko etykiety nie są bardzo długie (uwaga: wywołanie samego `labels(coś)` jest równoznaczne z żądaniem wyświetlenia zwróconego przez funkcję obiektu na konsoli);
-   skonwertować na ramkę danych z kolumnami *variable* i *label* (etykiety zmiennych) lub *value* i *label* (etykiety wartości) przy pomocy funkcji `as.data.frame()`;
-   wyświetlić w podglądzie danych RStudio przy pomocy funkcji `View()`.

**Funkcja `label()`:**

Wywołana na pojedynczej zmiennej (kolumnie) obiektu *data.frame* zwraca etykietę tej zmiennej (zwykle jednoelementowy wektor tekstowy).

Możliwe jest też przypisanie etykiety zmiennej, z wykorzystaniem składni:

```{r}
label(ramka_danych$kolumna) = "etykieta"
```

##### Funkcja value\_labels()

Wywołana na obiekcie *data.frame* zwraca listę etykiet wartości poszczególnych zmiennych (kolumn) tego obiektu (zbioru).

Obiekt zwrócony przez funkcję `value_labels()` (efekt działania tej funkcji) można następnie:

-   wyświetlić na konsoli - zawarta w pakiecie metoda `print()` zapewnia przyjazne sformatowanie wyniku, w którym zaznaczane są bloki (sąsiadujących ze sobą) zmiennych posiadających te same etykiety (uwaga: wywołanie samego `value_labels(coś)` jest równoznaczne z żądaniem wyświetlenia zwróconego przez funkcję obiektu na konsoli);
-   wybrać z niej podzbiór elementów (zestawów etykiet zmiennych) przy pomocy operatora `[]` (zwrócony obiekt wciąż będzie elegancko wyświetlał się na konsoli);
-   wybrać z niego jeden element (zestaw etykiet zmiennej) przy pomocy operatora `[[]]` lub `$` - taki pojedynczy element będzie zachowywał się tak, jak obiekt zwracany przez funkcję `labels()` wywołaną na kolumnie obiektu *data.frame*.

Funkcja `value_labels()` może też zostać wywołana na pojedynczej zmiennej (kolumnie) obiektu *data.frame* - będzie wtedy działać, jak funkcja `labels()`.

### Inne funkcje

Funkcja `zastosuj_codebook()` pozwala tworzyć obiekty *data.frame* zawierające etykiety zmiennych i etykiety wartości na podstawie zbiorów w formacie .csv i powiązanych z nimi codebooków przygotowanych w formacie, który kiedyś był wykorzystywany w IBE.

Funkcja `labelled_na_foreigndata()` była użyteczna kiedyś, kiedy pakiet *haven* bardzo słabo wspierał zapis do formatów .dta i .sav.
