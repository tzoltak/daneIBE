#' @title Operacje na etykietach zmiennych i etykietach wartosci
#' @description
#' Funkcja \code{labels} pozwala w wygodny sposób uzyskać dostęp do etykiet
#' zmienych lub etykiet wartości pojedynczej zmiennej.
#' @param object obiekt potencjalnie posiadający zdefiniowane etykiety zmiennych
#' lub etykiety wartości
#' @param SIMPLIFY wartość logiczna - czy etykiety zmiennych mają być zwrócone
#' jako wektor? (jeśli \code{FALSE}, zostaną zwrócone jako lista)
#' @param ... wyłącznie dla zachowania zgodności z funkcją \emph{generic}
#' @return Funkcja \code{labels} (wywołana na obiektach udokumentowanych tu
#' klas) zwraca listę lub wektor (z przypisaną klasą \emph{labels})
#' @name labels
#' @method labels data.frame
#' @export
labels.data.frame = function(object, SIMPLIFY = TRUE, ...) {
  stopifnot(is.logical(SIMPLIFY), length(SIMPLIFY) == 1)
  stopifnot(SIMPLIFY %in% c(TRUE, FALSE))
  lab = lapply(object, function(x) {
    if (is.null(attributes(x))) {
      return(NULL)
    }
    if (exists("label", attributes(x))) {
      return(attributes(x)$label)
    } else {
      return(NULL)
    }
  })
  if (SIMPLIFY & max(sapply(lab, length)) == 1) {
    lab = unlist(lapply(lab, function(x) {
      if (is.null(x)) {
        return(NA)
      } else {
        return(x)
      }
    }))
  }
  class(lab) = c("var_labels", "labels", class(lab))
  return(lab)
}
#' @rdname labels
#' @export
labels.list = function(object, SIMPLIFY = TRUE, ...) {
  stopifnot(is.logical(SIMPLIFY), length(SIMPLIFY) == 1)
  stopifnot(SIMPLIFY %in% c(TRUE, FALSE))
  lab = lapply(object, function(x) {
    if (is.null(attributes(x))) {
      return(NULL)
    }
    if (exists("label", attributes(x))) {
      return(attributes(x)$label)
    } else {
      return(NULL)
    }
  })
  if (SIMPLIFY & max(sapply(lab, length)) == 1) {
    lab = unlist(lapply(lab, function(x) {
      if (is.null(x)) {
        return(NA)
      } else {
        return(x)
      }
    }))
  }
  class(lab) = c("var_labels", "labels", class(lab))
  return(lab)
}
#' @rdname labels
#' @export
labels.labelled = function(object, ...) {
  if (is.null(attributes(object))) {
    return(NULL)
  }
  if (!exists("labels", attributes(object))) {
    return(NULL)
  }
  lab = attributes(object)$labels
  class(lab) = c("val_labels", "labels", class(lab))
  return(lab)
}
#' @rdname labels
#' @export
value_labels = function(object, ...) {
  UseMethod("value_labels", object)
}
#' @rdname labels
#' @export
value_labels.data.frame = function(object, ...) {
  return(lapply(object, function(x) {
    if ("labelled" %in% class(x)) {
      return(labels(x))
    } else {
      return(NULL)
    }
  }))
}
#' @rdname labels
#' @export
value_labels.default = function(object, ...) {
  return(labels(object, ...))
}
#' @rdname labels
#' @param x obiekt klasy \emph{var_labels} lub  \emph{val_labels}
#' @export
print.var_labels = function(x, ...) {
  if (is.list(x)) {
    x = lapply(x, function(x) {
      if (is.null(x)) {
        return(NA)
      } else {
        return(x)
      }
    })
  }
  x = data.frame(label = x, row.names = names(x))
  print(x, right = FALSE)
  invisible(x)
}
#' @rdname labels
#' @export
print.val_labels = function(x, ...) {
  x = data.frame(value = x, label = as.character(names(x)))
  x$label = format(x$label,
                   width = max(nchar(c(" ", x$label)), na.rm = TRUE))
  print(x, row.names = FALSE)
  invisible(x)
}
