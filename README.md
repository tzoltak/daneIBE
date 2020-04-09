# daneIBE

Pakiet zawiera następujące grupy funkcji:

## Funkcje wspomagające eksplorację danych ze zmiennymi etykietowanymi wczytanych przy pomocy pakietu *haven*

### Etykiety zmiennych

**Funkcja `labels()`:**

- Wywołana na obiekcie *data.frame* zwraca listę etykiet zmiennych (etykiety są zawarte w elementach listy, a nazwy zmiennych w nazwach tych elementów).
- Wywołana na pojedynczej zmiennej (kolumnie) obiektu *data.frame* zwraca wektor z etykietami wartości (wartości są zawarte w elementach wektora, a powiązane z nimi etykiety stanowią nazwy tych elementów).

Obiekt zwrócony przez funkcję `labels()` (efekt działania tej funkcji) można następnie:
  
- wyświetlić na konsoli - zawarta w pakiecie metoda `print()` zapewnia przyjazne sformatowanie wyniku, jeśli tylko etykiety nie są bardzo długie (uwaga: wywołanie samego `labels(coś)` jest równoznaczne z żądaniem wyświetlenia zwróconego przez funkcję obiektu na konsoli);
- skonwertować na ramkę danych z kolumnami *variable* i *label* (etykiety zmiennych) lub *value* i *label* (etykiety wartości) przy pomocy funkcji `as.data.frame()`;
- wyświetlić w podglądzie danych RStudio przy pomocy funkcji `View()`.

**Funkcja `label()`:**

Wywołana na pojedynczej zmiennej (kolumnie) obiektu *data.frame* zwraca etykietę tej zmiennej (co do zasady jednoelementowy wektor tekstowy).

**Funkcja `value_labels()`:**

Wywołana na obiekcie *data.frame* zwraca listę etykiet wartości poszczególnych zmiennych (kolumn) tego obiektu (zbioru).

Obiekt zwrócony przez funkcję `value_labels()` (efekt działania tej funkcji) można następnie:
  
- wyświetlić na konsoli - zawarta w pakiecie metoda `print()` zapewnia przyjazne sformatowanie wyniku, w którym zaznaczane są bloki (sąsiadujących ze sobą) zmiennych posiadających te same etykiety (uwaga: wywołanie samego `value_labels(coś)` jest równoznaczne z żądaniem wyświetlenia zwróconego przez funkcję obiektu na konsoli);
- wybrać z niej podzbiór elementów (zestawów etykiet zmiennych) przy pomocy operatora `[]` (zwrócony obiekt wciąż będzie elegancko wyświetlał się na konsoli);
- wybrać z niego jeden element (zestaw etykiet zmiennej) przy pomocy operatora `[[]]` lub `$` - taki pojedynczy element będzie zachowywał się tak, jak obiekt zwracany przez funkcję `labels()` wywołaną na kolumnie obiektu *data.frame*.

Funkcja `value_labels()` może też zostać wywołana na pojedynczej zmiennej (kolumnie) obiektu *data.frame* - będzie wtedy działać, jak funkcja `labels()`.

### Tabela z rozkładem (i etykietami)

Funkcja `tab()`, wywołana na pojedynczej zmiennej (kolumnie) obiektu *data.frame* wyświetla tabelę z rozkładem tej zmiennej uwzględniającą (jeśli elementy te zostały zdefiniowane):

- etykietę zmiennej,
- wartości,
- etykiety wartości,
- liczebności,
- procenty.

Może też zostać wywołana na obiekcie *data.frame* lub obiekcie *tbl_svy* (czyli obiekcie będącym połączeniem *data.frame* z informacjami o złożonym schemacie doboru próby, tworzonym przez funkcje pakietu *srvyr*) z podaniem zmiennej (kolumny), której rozkład ma być wygenerowany przy pomocy drugiego argumentu (nazwa kolumny może być podana jako wyrażenie języka lub jako ciąg znaków, tj. w odniesieniu do swojego drugiego argumentu `tab()` obsłuje *tidy evaluation*).

Ograniczenie zmiennej stanowi fakt, że nie pozwala ona uwzględnić ważenia, ani tworzyć rozkładów warunkowych.

## Inne funkcje

Funkcja `zastosuj_codebook()` pozwala tworzyć obiekty *data.frame* zawierające etykiety zmiennych i etykiety wartości na podstawie zbiorów w formacie .csv i powiązanych z nimi codebooków przygotowanych w formacie, który kiedyś był wykorzystywany w IBE.

Funkcja `labelled_na_foreigndata()` była użyteczna kiedyś, kiedy pakiet *haven* bardzo słabo wspierał zapis do formatów .dta i .sav.
