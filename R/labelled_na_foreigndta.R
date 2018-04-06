#' @title Konwersja ramki danych ze zmiennymi etykietowanymi do postaci dajacej
#' sie zapisac do pliku DTA
#' @description
#' Funkcja konweruje ramkę danych zawierającą zmienne \emph{etykietowane}
#' (typowo wczytane funkcją \code{\link[haven]{read_spss}},
#' \code{\link[haven]{read_dta}}, lub \code{\link{zastosuj_codebook}})
#' do postaci odpowiadającej wynikowi działania funkcji
#' \code{\link[foreign]{read.dta}}. Tak przetworzoną ramkę danych można potem
#' w miarę bezproblemowo zapisać do formatu DTA korzystając z funkcji
#' \code{\link[foreign]{write.dta}}.
#' @param x ramka danych ze zmiennymi \emph{etykietowanymi}
#' @param zachowajWartosciPustymiPoziomami wartość logiczna: jeśli \code{FALSE},
#' zmienne \emph{etykietowane} po prostu zostaną skonwertowane na
#' \emph{czynnniki}, co może prowadzić do zmian wartości związanych
#' z poszczególnymi etykietami (p. sekcja \emph{Details}); jeśli \code{TRUE},
#' pierwotne wartości zostaną zachowane, jednak kosztem utworzenia dodatkowych
#' pustych poziomów \emph{czynników} dla wartości, które nie miały przypisanych
#' etykiet
#' @param naBrakiDanych lista wektorów tekstowych zawierających wartości etykiet,
#' które mają zostać przekodowane na różne typy braków danych Staty (choć ze
#' względu na ograniczenia funkcji \code{\link[foreign]{write.dta}} przy zapisie
#' mogą zostać uwzględnione tylko braki \emph{systemowe}, oznaczane jako ".");
#' nazwa elemntu listy podaje rodzaj braku danych (".", ".a", ... ".z")
#' @param pusteCiagiZnakow ciąg znaków, na który zostaną zamienione puste ciągi
#' znaków w zmiennych tekstowych (Stata nie jest w stanie zapisać pustego ciągu
#' znaków jako wartości zmiennej tekstowej)
#' @details
#' Zapis danych do formatu DTA poprzez funkcję \code{\link[foreign]{write.dta}}
#' został sklepany dosyć siermiężnie i jako etapu pośredniego wymaga konwersji
#' zmiennych \emph{etykietowanych} na \emph{czynniki}. Jeśli wartości, którym
#' przypisano etykiety nie są ciągiem kolejnych (poczynając od 1) liczb
#' naturalnych, wiąże się to albo z koniecznością pogodzenia się z tym, że
#' w zapisanym (tu: przygotowanym do zapisu) zbiorze nie będą zgadzać się z tymi,
#' jakie występowały w pierwotnym zbiorze, albo brzydko obchodzić ten problem
#' dodając puste etykiety wartości jako \emph{wypełniacz}. O tym, które
#' rozwiązanie funkcja (stara się) zastosować decyduje wartość argumentu
#' \code{zachowajWartosciPustymiPoziomami}.
#'
#' Nawet jeśli argument \code{zachowajWartosciPustymiPoziomami=TRUE}, dodatkowe
#' puste poziomy zostaną utworzone tylko dla zmiennych, których wszystkie
#' wartości to dodatnie liczby całkowite.
#'
#' Warto odnotować, że jeśli argument
#' \code{zachowajWartosciPustymiPoziomami=TRUE}, funkcja będzie czynić zło, tzn.
#' tworzyć \emph{czynniki} ze zduplikowanymi wartościami poziomów (co w zasadzie
#' jest niedozwolone).
#'
#' Po zapisaniu zbioru funkcją \code{\link[foreign]{write.dta}} (należy pamiętać
#' o podaniu argumentu \code{convert.factors = "labels"}), warto jeszcze
#' otworzyć go w Stacie i wydać polecenie \code{compresss}, bo funkcja
#' \code{\link[foreign]{write.dta}} bardzo \emph{hojnie} obdziela zmienne
#' w zbiorze przestrzenią (nie próbuje sprawdzać, czy nie starczyłby
#' \emph{krótszy} format).
#' @return data.frame ze zmiennymi \emph{etykietowanymi} przekształconymi na
#' \emph{czynniki} i przypisanymi atrybutami analogicznymi do tych, jakie
#' mają przypisane ramki danych zwracane przez \code{\link[foreign]{read.dta}}
#' @examples
#' \dontrun{
#' dane = zastosuj_codebook("plik z danymi.csv", "codebook.csv")
#' daneDoDta = labelled_na_foreigndta(dane)
#' write.dta(daneDoDta, "nazwa pliku.dta", version = 10, convert.factors = "labels")
#' }
#' @export
labelled_na_foreigndta = function(x, zachowajWartosciPustymiPoziomami = FALSE,
                                  naBrakiDanych = list(. = c("brak odpowiedzi",
                                                             "odmowa odpowiedzi",
                                                             "nie dotyczy", "ndt.")),
                                  pusteCiagiZnakow = ".") {
  stopifnot(is.data.frame(x),
            is.logical(zachowajWartosciPustymiPoziomami),
            length(zachowajWartosciPustymiPoziomami) == 1,
            is.list(naBrakiDanych),
            length(pusteCiagiZnakow) == 1)
  stopifnot(zachowajWartosciPustymiPoziomami %in% c(TRUE, FALSE),
            all(names(naBrakiDanych) %in% paste0(".", c("", letters))),
            !any(duplicated(naBrakiDanych)),
            is.character(pusteCiagiZnakow) | is.na(pusteCiagiZnakow))

  etykietyZmiennych = sapply(x, function(x) {
    return(ifelse("label" %in% names(attributes(x)), attributes(x)$label, ""))})

  maskaString = sapply(x, is.character)
  x[maskaString] = lapply(x[maskaString], function(x, pusteCiagiZnakow) {
    return(ifelse(x %in% "", pusteCiagiZnakow, enc2native(x)))},
    pusteCiagiZnakow = pusteCiagiZnakow)

  # walka z kodowaniem etykiet
  maskaFactory = sapply(x, is.factor)
  x[maskaFactory] = lapply(x[maskaFactory], function(x) {
    levels(x) = enc2native(levels(x))
    return(x)
  })

  naBrakiDanych = lapply(naBrakiDanych, function(x) {
    return(tolower(gsub("[[:blank:][:cntrl:][:punct:][:space:]]", "", x)))})

  # (prawie całkiem) zbędny trud - write.dta() i tak nie używa tych informacji!
  zEtykietami = sapply(as.list(x), function(x) {
    if ("labels" %in% names(attributes(x))) {
      return(length(attributes(x)$labels) > 0)
    } else {
      return(FALSE)
    }
  })
  x[zEtykietami] = lapply(x[zEtykietami], function(x) {
    names(attributes(x)$labels) = enc2native(names(attributes(x)$labels))
    return(x)
  })
  labelTable = lapply(x[zEtykietami], function(x) {
    return(attributes(x)$labels)
  })
  missing = lapply(x, function(x, naBrakiDanych) {
    y = ifelse(is.na(x), 0, NA)
    for (i in 1:length(naBrakiDanych)) {
      nr = which(names(naBrakiDanych)[i] == paste0(".", c("", letters))) - 1
      if ("labels" %in% names(attributes(x))) {
        wartosciBrakow =
          attributes(x)$labels[which(
            tolower(gsub("[[:blank:][:cntrl:][:punct:][:space:]]", "",
                         names(attributes(x)$labels))) %in% naBrakiDanych[[i]])]
      } else {
        wartosciBrakow = vector(mode = "character", length = 0)
      }
      y = ifelse(x %in% wartosciBrakow, rep(nr, length(y)), y)
    }
    return(y)
  }, naBrakiDanych = naBrakiDanych)
  # koniec zbędnego trudu

  # w praktyce trzeba to zrobić tak
  x[zEtykietami] = lapply(x[zEtykietami], function(x, zwpp, naBrakiDanych) {
    poziomy = sort(unique(c(x, attributes(x)$labels)))
    poziomy = poziomy[!is.na(poziomy)]
    if (zwpp) {
      normalnePoziomy = poziomy
      if (length(poziomy) != max(poziomy)) {
        if (all(poziomy %in% (1:max(poziomy)))) {
          poziomy = 1:max(poziomy)
        }
      }
      pustePoziomy = setdiff(poziomy, normalnePoziomy)
    }
    if ("." %in% names(naBrakiDanych)) {
      poziomy = poziomy[!(poziomy %in% attributes(x)$labels[which(
        tolower(gsub("[[:blank:][:cntrl:][:punct:][:space:]]", "",
                     names(attributes(x)$labels))) %in% naBrakiDanych[["."]])])]
    }
    etykiety = poziomy
    etykiety[etykiety %in% attributes(x)$labels] =
      names(attributes(x)$labels)[order(attributes(x)$labels)][attributes(x)$labels %in% etykiety]
    x = factor(x, poziomy, etykiety)
    if (zwpp) {
      attributes(x)$levels[attributes(x)$levels %in% pustePoziomy] = "" # ZŁO!!!
    }
    return(x)
  }, zwpp = zachowajWartosciPustymiPoziomami, naBrakiDanych = naBrakiDanych)

  attributes(x)$datalabel = "Przygotowano w pakiecie daneIBE"
  attributes(x)$time.stamp = format(Sys.time(), "%d %B %Y %X")
  attributes(x)$val.labels = names(x)
  attributes(x)$var.labels = enc2native(etykietyZmiennych)
  attributes(x)$version = 10L
  attributes(x)$label.table = labelTable # zbędny trud
  attributes(x)$missing = missing # zbędny trud

  return(x)
}
