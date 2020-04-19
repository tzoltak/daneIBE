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

**Funkcja `tab()`:**

Funkcja `tab()` tworzy tabelę z rozkładem liczebności i rozkładem częstości jednej zmiennej. 

Jest przeznaczona przede wszystkim do analiz eksploracyjnych z wykorzystaniem zmiennych *etykietowanych* - pozwala obejrzeć ich rozkład, wraz z mapowaniem wartości-etykiety bez konieczności konwertowania ich na inny typ zmiennej. Na *normalnych

Może być wywołana na:

- wektorze (w tym *czynniku*),
- *ramce danych*,
- obiekcie *tbl_svy* (czyli obiekcie będącym połączeniem *data.frame* z informacjami o złożonym schemacie doboru próby, tworzonym przez funkcje pakietu *srvyr*);
  - pozwala to uwzględnić ważenie.

W przypadku drugiego i trzeciego z ww. drugim argumentem podaje się zmienną (kolumnę), której rozkład ma być wygenerowany (nazwa kolumny może być podana jako wyrażenie języka lub jako ciąg znaków, tj. w odniesieniu do swojego drugiego argumentu `tab()` obsługuje *tidy evaluation*).

Przykład użycia:

```{r}
tab(mtcars$cyl)
tab(mtcars, cyl)
tab(mtcars, cyl, d = 2, etykietaSuma = "Sum")

library(haven)
mtcars$cyl = labelled(mtcars$cyl, c("cztery" = 4, "sześć" = 6, "osiem" = 8),
                      "liczba cylindrów")
tab(mtcars, cyl)
```

**Funkcja `tab2()`:**

Funkcja generuje rozkład łączny liczebności i rozkład łączny lub rodzinę warunkowych rozkładów częstości dwóch zmiennych. Jako pierwszy argument przyjmuje *ramkę danych* lub obiekt *tbl_svy* (będący połączeniem *data.frame* z informacjami o złożonym schemacie doboru próby, tworzony przez funkcje pakietu *srvyr*). Radzi też sobie ze zmiennymi etykietowanymi (konwertując je na *czynniki*).

Metoda `as.data.frame` pozwala przekształcić zwracane zestawienie w *ramkę danych* (ściśle w *tibble*) w postaci *długiej*, przydatną np. do rysowania wykresów przy pomocy *ggplot2* (czyli działa analogicznie, jak metoda `as.data.frame` dla obiektów zwracanych przez funkcję `table` z pakietu *base*).

Argumenty:

1. *Ramka danych* lub obiekt *tbl_svy*.
2. Zmienna, której wartości mają zostać umieszczone w wierszach tabeli.
3. Zmienna, której wartości mają zostać umieszczone w kolumnach tabeli.
4. Kierunek procentowania (w wywołaniu funkcji wystarczy podać pierwszą literę):
   - "brak" (domyślnie),
   - "kolumny",
   - "wiersze",
   - "ogółem".
5. Dalsze argumenty - p. dokumentacja (aby ją obejrzeć wywołaj: `?tab2`).

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
```

### Funkcje wspomagające pracę ze zmiennymi etykietowanymi wczytanych przy pomocy pakietu *haven*

#### Etykiety zmiennych

**Funkcja `labels()`:**

- Wywołana na obiekcie *data.frame* zwraca listę etykiet zmiennych (etykiety są zawarte w elementach listy, a nazwy zmiennych w nazwach tych elementów).
- Wywołana na pojedynczej zmiennej (kolumnie) obiektu *data.frame* zwraca wektor z etykietami wartości (wartości są zawarte w elementach wektora, a powiązane z nimi etykiety stanowią nazwy tych elementów).

Obiekt zwrócony przez funkcję `labels()` (efekt działania tej funkcji) można następnie:
  
- wyświetlić na konsoli - zawarta w pakiecie metoda `print()` zapewnia przyjazne sformatowanie wyniku, jeśli tylko etykiety nie są bardzo długie (uwaga: wywołanie samego `labels(coś)` jest równoznaczne z żądaniem wyświetlenia zwróconego przez funkcję obiektu na konsoli);
- skonwertować na ramkę danych z kolumnami *variable* i *label* (etykiety zmiennych) lub *value* i *label* (etykiety wartości) przy pomocy funkcji `as.data.frame()`;
- wyświetlić w podglądzie danych RStudio przy pomocy funkcji `View()`.

**Funkcja `label()`:**

Wywołana na pojedynczej zmiennej (kolumnie) obiektu *data.frame* zwraca etykietę tej zmiennej (zwykle jednoelementowy wektor tekstowy).

Możliwe jest też przypisanie etykiety zmiennej, z wykorzystaniem składni:

```{r}
label(ramka_danych$kolumna) = "etykieta"
```

**Funkcja `value_labels()`:**

Wywołana na obiekcie *data.frame* zwraca listę etykiet wartości poszczególnych zmiennych (kolumn) tego obiektu (zbioru).

Obiekt zwrócony przez funkcję `value_labels()` (efekt działania tej funkcji) można następnie:
  
- wyświetlić na konsoli - zawarta w pakiecie metoda `print()` zapewnia przyjazne sformatowanie wyniku, w którym zaznaczane są bloki (sąsiadujących ze sobą) zmiennych posiadających te same etykiety (uwaga: wywołanie samego `value_labels(coś)` jest równoznaczne z żądaniem wyświetlenia zwróconego przez funkcję obiektu na konsoli);
- wybrać z niej podzbiór elementów (zestawów etykiet zmiennych) przy pomocy operatora `[]` (zwrócony obiekt wciąż będzie elegancko wyświetlał się na konsoli);
- wybrać z niego jeden element (zestaw etykiet zmiennej) przy pomocy operatora `[[]]` lub `$` - taki pojedynczy element będzie zachowywał się tak, jak obiekt zwracany przez funkcję `labels()` wywołaną na kolumnie obiektu *data.frame*.

Funkcja `value_labels()` może też zostać wywołana na pojedynczej zmiennej (kolumnie) obiektu *data.frame* - będzie wtedy działać, jak funkcja `labels()`.

### Inne funkcje

Funkcja `zastosuj_codebook()` pozwala tworzyć obiekty *data.frame* zawierające etykiety zmiennych i etykiety wartości na podstawie zbiorów w formacie .csv i powiązanych z nimi codebooków przygotowanych w formacie, który kiedyś był wykorzystywany w IBE.

Funkcja `labelled_na_foreigndata()` była użyteczna kiedyś, kiedy pakiet *haven* bardzo słabo wspierał zapis do formatów .dta i .sav.
