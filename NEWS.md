Do zrobienia:

-   funkcja do kable'owania (i kable'owania Extra) tabelek (rozbudować metody print?);
-   zbadać kompatybilność funkcji od etykiet z tymi z pakietu labels.

------------------------------------------------------------------------

# daneIBE 0.7.3 (31.1.2021)

## Udoskonalenia

-   *Inteligentne zaokrąglanie* stosowane jest również przez metody `print()` dla obiektów zwracanych przez `tab_n()` i `tab2()`, przy czym dla tych drugich tylko wtedy, kiedy wybrane zostało procentowanie w wierszach lub w kolumnach.
-   Metoda `print()` dla obiektów zwracanych przez `tab()` wyświetla <brak> jako etykiety dla wartości, które występują w danych, ale nie posiadają zdefiniowanej etykiety wartości.

## Naprawione błędy

-   Funkcja `tab()` znów poprawnie obsługuje braki danych i argument `etykietaBD = NULL` (poprawka po regresji powstałej w wyniku dodania do `tab()` możliwości użycia wag).
-   Metoda `print()` dla obiektów zwracanych przez `tab()` wykrywa więcej sytuacji, w których powinna wyrównać wartości kolumny *wartość* do prawej.

## Zmiany wewnętrzne

-   Porządki po zmianie API `tab()`.
-   Funkcja `zaokraglij_do_sumy()` otwarcie komunikuje, że nie toleruje braków danych w zaokrąglanym wektorze.

------------------------------------------------------------------------

# daneIBE 0.7.2 (8.1.2021)

## Zmiany interfejsu

-   Ujednolicenie API tab() i tab2()/tab_n()/tab_w():

    -   funkja `tab()` otrzymała argument `w`, umożliwiający podanie kolumny zawierającej wagi lub wektora wag;

    -   funkcja `tab()` utraciła argument `d` - zaokrąglanie wartości komórek przeniesiono do metody `print()` zwracanych przez nią obiektów klasy *tab_lbl*, przy czm można teraz podać oddzielnie dokładność dla liczebności i dla procentów;

## Udoskonalenia

-   `tab()`, `tab2()`, i `tab_n()` obsługują jako dane wejściowe również obiekty schematów doboru próby pakietu *survey* oraz tabele zwracane przez funkcję `tabel()` lub `ftable()`;

-   metody `print()`, odpowiedzialne za wyświetlanie obiektów zwracanych przez `tab()`, `tab2()`, `tab_n()` i `tab_w()` pilnują, żeby nie zwracać liczb zapisanych w notacji naukowej (choć można im wskazać, żeby tego nie robiły, przy pomocy nowego argumentu `scipen`);

-   *Inteligetne zaokrąglanie* kolumn przy wyświetlaniu zestawień zwracanych przez `tab()` bierze teraz pod uwagę dwie kwestie:

    -   dąży do tego, aby wartości po zaokrągleniu sumowały się do zaokrąglenia ich sumy (tak jak to się działo wcześniej),
    -   dba, aby te same wartości przed zaokrągleniem miały te same wartości po zaokrągleniu - jeśli nie da się tego osiągnąć, nie wprowadza korekt zaokrągleń.

## Zmiany wewnętrzne

-   aserecje z pokrewnych funkcji wyodrębnione do oddzielnych funkcji (i pliku);
-   metody dotyczące obiektów klas *tab_lbl*, *tab_lbl2* i *tab_lbl_n* wyodrębnione do oddzielnych plików;
-   nowa funkcja zaokraglij_do_sumy() realizująca *inteligetne zaokrąglanie*;
-   usunięto z `tab2()` workaroundy na kiepskie radzenie sobie przez `survey_count()` ze zmiennymi niebędącymi wektorami tekstowymi lub czynnikami - wiąże się to z podniesieniem zależność od *srvyr* do wersji 1.0.0;

------------------------------------------------------------------------

# daneIBE 0.7.1 (18.11.2020)

## Naprawione błędy

-   tab() znów działa (poprawiony błąd z wywołaniem structure() przy zwracaniu wyników działania) i znów reaguje na dodatkowe argumenty w metodzie dla ramki danych; poza tym lepiej obsługuje argument etykietaBD.

------------------------------------------------------------------------

# daneIBE 0.7.0 (2.10.2020)

## Nowe funkcje

-   tab_n() pozwala tworzyć zestawienia rozkładów brzegowych wielu zmiennych (co może być sensowne zwłaszcza, jeśli mają ten sam zbiór możliwych odpowiedzi); wywołanie jej stanowi też pierwszy krok na drodze do uzyskania zestawienia odpowiedzi na pytanie "wielokrotnego wyboru";

    -   funkcja obsługuje "tidy evaluation" w wyborze zmiennych;
    -   zwracane zestawienia są innej klasy (tab_lbl_n), niż te zwracane przez tab2() (tab_lbl2);

-   tab_w() pozwala na podstawie wyniku działania tab_n() przygotować zestawienie odpowiedzi na pytanie "wielokrotnego wyboru"

    -   funkcja nieco nieortodoksyjnie wykorzystuje "tidy evaluation" do wskazywania przez użytkownika, które wartości mają być zliczane (jako wybrane przez respondenta), a które ew. wykluczane z podstawy procentowania.

## Udoskonalenia

-   metoda as.data.frame() domyślnie nie przeprowadza przekształcenia na formę długą, a jedynie sprowadza obiekt do formy "standardowej ramki danych", co pozwala bez problemy używać obiekty zwracane przez tab2(), tab_n() i tab_w() w wywołaniach kable().

    -   aby przekształcić zestawienie na formę długą najlepiej używać metody as_tibble() (lub as.data.frame(obiekt_tab, niePrzeksztalcaj = FALSE));

-   nowy zbiór danych (przykladWO) do prezentowania sposobów użycia funkcji tab_n() i tab_w().

## Naprawione błędy

-   metody as_tibble() i as.data.frame() nie zamieniają wartości zmiennych, które zostały zapisane w nazwach kolumn zestawienia na nazwy będące poprawnymi wyrażeniami języka R.

------------------------------------------------------------------------

# daneIBE 0.6.4 (30.09.2020)

## Udoskonalenia

-   tab() otrzymała dodatkowy argument `etykietaBD`, działający analogicznie, jak ten sam argument w tab2() (w szczególności pozwala usunąć braki danych ze zwracanego rozkładu).

## Naprawione błędy

-   tab() najpierw konwertuje zmienne etykietowane na czynniki, a dopiero potem zbiera informacje o klasach zmiennych ("przywracanie formatów" i tak nie jest zaimplementowane dla zmiennych etykietowanych);
-   trochę poprawek w dokumentacji tab();
-   trochę poprawek w obsłudze metod.

------------------------------------------------------------------------

# daneIBE 0.6.3 (5.05.2020)

## Udoskonalenia

-   tab2() w trybie tworzenia rodziny rozkładów warunkowych (sumowanie równe "kolumny" lub "wiersze") zwraca również kolumnę/wiersz z rozkładem brzegowym zmiennej zależnej - etykietę kolumny/wiersza można podać argumentem `etykietaOgolem`, albo ustawić jego wartość na NA lub NULL, aby wskazać, że zwrócony rozkład ma nie zawierać takiej kolumny/takiego wiersza;

    -   aby zmniejszyć ryzyko nieporozumień domyślną etykietę kolumny/wiersza sumy zmieniono z "ŁĄCZNIE" na "SUMA";
    -   metody as_tibble() i as.data.frame() zyskały nowy argument usunLacznie, który pozwala pozbyć się wierszy opisujących rozkład brzegowy zmiennej zależnej ze zwracanej ramki danych.

## Naprawione błędy

-   as_tibble() (i as.data.frame()) wywołane na obiekcie zwróconym przez tab2() poprawnie radzą sobie z przywracaniem formatu zmiennej opisywanej w wierszach tabeli (jeśli takie przywrócenie formatu jest możliwe).

------------------------------------------------------------------------

# daneIBE 0.6.2 (5.05.2020)

## Naprawione błędy

-   tab() nie radzi sobie ze zmiennymi tekstowymi ani liczbowymi w ramach obiektów tbl_svy, jeśli zawierają braki danych (przyczyna problemu to nieradzenie sobie w takiej sytuacji przez funkcję survey_count() z pakietu srvyr - aby to obejść, tab() wewnętrznie konwertuje takie zmienne na czynniki).

------------------------------------------------------------------------

# daneIBE 0.6.1 (28.04.2020)

## Udoskonalenia

-   obiekty zwracane przez tab2() otrzymały metodę as_tibble(), które zwraca tibble, w odróżnieniu od metody as.data.frame(), która zwraca "base'ową" ramkę danych - pozwala to uniknąć stosowania kuriozalnej składni z wywołaniem as.data.frame() na wynikach wywołania as.data.frame(), żeby uzyskać "base'ową" ramkę danych.

## Naprawione błędy

-   tab() nie radzi sobie z przywróceniem typu, jaki miały wartości liczbowe zmiennej klasy haven_labelled.

------------------------------------------------------------------------

# daneIBE 0.6.0 (19.04.2020)

## Nowe funkcje

-   tab2() pozwala tworzyć łączne rozkłady liczebności i łączne rozkłady częstości lub rodziny warunkowych rozkładów częstości dwóch zmiennych:

    -   posiada metody dla ramek danych i obiektów tbl_svy;
    -   wskazywanie kolumn, których rozkład ma zostać obliczony obsługuje "tidy evaluation";
    -   pozwala uwzględnić ważenie, również w metodzie dla ramki danych;

-   metoda as.data.frame dla obiektów zwracanych przez tab2() - pozwala przekształcić je do postaci długiej, przydatnej np. do tworzenia wykresów w ggplot2;

-   funkcja label()\<- pozwalająca przypisać wartość atrybutowi "label" obiektu.

## Udoskonalenia

-   Uzupełniona dokumentacja i plik README.md.

------------------------------------------------------------------------

# daneIBE 0.5.0 (9.04.2020)

## Nowe funkcje

-   metoda tab() dla ramek danych (drugi argument wskazuje kolumnę, której rozkład ma zostać wygenerowany - obsługuje "tidy evaluation"),
-   metoda tab() dla obiektów tbl_svy tworzonych przez pakiet srvyr (API identyczne, jak w metodzie dla ramek danych),
-   label() - zwraca etykietę przypisaną do wektora (do zastosowania w odniesieniu do obiektu, który był kolumną ramki danych, ale został już z niej "wybrany").

## Udoskonalenia

-   tab() nie drukuje tabeli na konsoli, lecz odpowiada za to metoda print() zwracanego przez nią obiektu.

------------------------------------------------------------------------

# daneIBE 0.4.0 (11.12.2019)

## Aktualizacje

-   funkcje i metody działające na etykietach współpracują z pakietem haven w wersjach \>= 2.0.0 (tj. odwołują się do klasy "haven_labelled").

## Udoskonalenia

-   metoda print() dla listy etykiet wartości zmiennych.

------------------------------------------------------------------------

# daneIBE 0.3.1 (4.09.2018)

## Poprawione błędy

-   zastosuj_codebook() radzi sobie z kolumnami w wejściowym zbiorze, które wczytują się jako innego typu niż numeric lub character.

------------------------------------------------------------------------

# daneIBE 0.3.0 (6.12.2017)

## Nowe funkcje

-   metody funkcji labels() dla ramek danych, list i obiektów klasy labelled i funkcja value_labels(), a także powiązane z wynikami ich działania metody funkcji print() pozwalają na wygodny dostęp do etykiet zmiennych i etykiet wartości.

------------------------------------------------------------------------

# daneIBE 0.2.0 (5.12.2017)

## Nowe funkcje

-   tab() generuje rozkład(y) liczebności i częstości jednej zmiennej w ramach tej samej tabeli, w miarę możliwości wykorzystując też etykiety wartości.

------------------------------------------------------------------------

# daneIBE 0.1.2 (22.11.2017)

## Udoskonalenia

-   zastosuj_codebook() nieco bardziej wyrozumiała wobec nazw kolumn w pliku z codebookiem.

------------------------------------------------------------------------

# daneIBE 0.1.1 (30.10.2017)

## Naprawione błędy

-   zastosuj_codebook() używa wartości kolumny 'opis' z codebooka, gdy kolumna 'krotki opis' jest pusta;
-   labelled_na_foreigndata() zachowuje etykiety wartości dla wszystkich zmiennych.

------------------------------------------------------------------------

# Publikacja daneIBE 0.1.0 (07.10.2017)

## Nowe funkcje

-   zastosuj_codebook(); labelled_na_foreigndta().
