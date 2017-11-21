#' @title Utworzenie ramki danych ze zmiennymi etykietowanymi z pliku CSV
#' @description
#' Funkcja tworzy ramkę danych ze zmiennymi \emph{etykietowanymi}
#' (\emph{labelled}, dokładnie klasy \emph{labelled_spss}) na podstawie pliku
#' CSV z danymi i pliku CSV z codebookiem (w formacie \emph{IBE'owym}).
#' @param dane nazwa (ścieżka) do pliku CSV z danymi
#' @param codebook nazwa (ścieżka) do pliku CSV z codebookiem
#' @param encoding ciąg znaków opisujący stronę kodową, w której zapisane są
#' plik z danymi i codebook (typowo "windows-1250" lub "UTF-8")
#' @param etykietyBrakowDanych wektor tekstowy zawierający wartości etykiet,
#' które mają zostać uznane za \emph{braki danych użytkownika}
#' @return data.frame z etykietami nazw zmiennych i (o ile zostały zdefiniowane
#' w codebooku) wartości zmiennych
#' @examples
#' \dontrun{
#' dane = zastosuj_codebook("plik z danymi.csv", "codebook.csv")
#' naBrakiDanych = c("nie wiem", "trudno powiedzieć", "brak odpowiedzi",
#'   "odmowa odpowiedzi", "nie potrafię ocenić", "nie pamiętam",
#'   "nie wiem/trudno powiedzieć", "nie wiem/nie pamiętam")
#' dane = zastosuj_codebook("plik z danymi.csv", "codebook.csv", "UTF-8", naBrakiDanych)
#' }
#' @importFrom utils read.csv2
#' @importFrom stats setNames
#' @importFrom haven labelled_spss
#' @export
zastosuj_codebook = function(dane, codebook, encoding = "windows-1250",
                             etykietyBrakowDanych = "") {
  stopifnot(file.exists(dane),
            file.exists(codebook),
            is.character(encoding), length(encoding) == 1,
            is.character(etykietyBrakowDanych))
  stopifnot(file.access(dane, 4) == 0,
            file.access(codebook, 4) == 0)

  codebook = read.csv2(codebook, stringsAsFactors = FALSE, encoding = encoding)
  names(codebook) = tolower(names(codebook))
  names(codebook) = sub(" zmiennej$", "", names(codebook))
  names(codebook) = sub("krótki", "krotki", names(codebook))
  names(codebook) = sub("słowa", "slowa", names(codebook))
  names(codebook) = sub("dokładność", "dokladnosc", names(codebook))
  names(codebook) = sub("[.](zmiennej|wartości)", "", names(codebook))
  names(codebook) = sub("(wartość[.]|)minimalna", "min", names(codebook))
  names(codebook) = sub("(wartość[.]|)maksymalna", "max", names(codebook))

  stopifnot(all(c("nazwa", "opis", "krotki.opis", "min", "max", "etykiety") %in%
                  names(codebook)))
  codebook$opis = ifelse(is.na(codebook$krotki.opis) | codebook$krotki.opis == "",
                         codebook$opis, codebook$krotki.opis)

  dane = read.csv2(dane, stringsAsFactors = FALSE, encoding = encoding)

  stopifnot("krotki.opis" %in% names(codebook),
            "opis" %in% names(codebook),
            "etykiety" %in% names(codebook),
            "min" %in% names(codebook),
            "max" %in% names(codebook),
            nrow(codebook) == ncol(dane))
  codebook$krotki.opis =
    ifelse(is.na(codebook$krotki.opis) | codebook$krotki.opis == "",
           codebook$opis, codebook$krotki.opis)
  if (!(all(names(dane) %in% codebook$nazwa))) {
    names(dane) = tolower(names(dane))
    codebook$nazwa = tolower(codebook$nazwa)
    warning("Nazwy zmiennych w zbiorze danych i w codebooku nie pasowały do siebie.\n",
            "  Skonwertowano nazwy zmiennych na pisane małymi literami.",
            immediate. = TRUE)
    stopifnot(all(names(dane) %in% codebook$nazwa))
  }

  etykietyBrakowDanych = tolower(gsub("[[:blank:][:cntrl:][:punct:][:space:]]", "",
                                      etykietyBrakowDanych))

  for (i in codebook$nazwa) {
    j = which(codebook$nazwa == i)
    attributes(dane[[i]])$label = codebook$krotki.opis[j]
    if (codebook$etykiety[j] != "" & !is.na(codebook$etykiety[j]) |
        (codebook$min[j] != "" & !is.na(codebook$min[j])) |
        (codebook$max[j] != "" & !is.na(codebook$max[j]))) {
      etykiety = strsplit(codebook$etykiety[j], "\n")[[1]]
      etykiety = strsplit(etykiety, ":( |)")
      etykiety = setNames(sapply(etykiety, function(x) {return(as.numeric(x[1]))}),
                          sapply(etykiety, function(x) {return(paste(x[2:length(x)],
                                                                     sep = ": "))}))
      if (length(etykiety) == 0) {
        etykiety = setNames(vector(mode = "numeric", length = 0),
                            vector(mode = "character", length = 0))
      }
      braki = which(tolower(gsub("[[:blank:][:cntrl:][:punct:][:space:]]", "",
                                 names(etykiety))) %in% etykietyBrakowDanych)
      braki = unname(etykiety[braki])
      if (codebook$min[j] != "" & !is.na(codebook$min[j])) {
        min = min(dane[[i]], na.rm = TRUE)
        if (is.finite(min) & min < codebook$min[j]) {
          brakiMin = unique(dane[[i]][dane[[i]] < codebook$min[j]])
          braki = c(braki, brakiMin)
          maskaEtykiety = !(brakiMin %in% etykiety)
          if (any(maskaEtykiety)) {
            etykiety = c(etykiety, setNames(brakiMin[maskaEtykiety],
                                            paste("poza dozwolonym zakresem -",
                                                  brakiMin[maskaEtykiety])))
          }
        }
      }
      if (codebook$max[j] != "" & !is.na(codebook$max[j])) {
        max = max(dane[[i]], na.rm = TRUE)
        if (is.finite(max) & max > codebook$max[j]) {
          brakiMax = unique(dane[[i]][dane[[i]] > codebook$max[j]])
          braki = c(braki, brakiMax)
          maskaEtykiety = !(brakiMax %in% etykiety)
          if (any(maskaEtykiety)) {
            etykiety = c(etykiety, setNames(brakiMax[maskaEtykiety],
                                            paste("poza dozwolonym zakresem -",
                                                  brakiMax[maskaEtykiety])))
          }
        }
      }
      dane[[i]] = labelled_spss(dane[[i]], etykiety, braki[!is.na(braki)])
    }
  }
  return(dane)
}
