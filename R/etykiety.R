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
labels.haven_labelled = function(object, ...) {
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
  object = lapply(object, function(x) {
    if ("labelled" %in% class(x) | "haven_labelled" %in% class(x)) {
      x = attributes(x)$labels
      class(x) = c("val_labels", "labels", class(x))
      return(x)
    } else {
      return(NULL)
    }
  })
  class(object) = c("val_labels_list", class(object))
  return(object)
}
#' @rdname labels
#' @export
value_labels.default = function(object, ...) {
  return(labels(object, ...))
}
#' @rdname labels
#' @export
`[.val_labels_list` = function(x, ...) {
  x = NextMethod("[")
  class(x) = c("val_labels_list", class(x))
  return(x)
}
#' @rdname labels
#' @export
as.data.frame.var_labels = function(x, ...) {
  if (is.list(x)) {
    x = lapply(x, function(x) {
      if (is.null(x)) {
        return(NA)
      } else {
        return(x)
      }
    })
  }
  x = data.frame(variable = names(x), label = unclass(x), row.names = NULL,
                 stringsAsFactors = FALSE)
  return(x)
}
#' @rdname labels
#' @export
as.data.frame.val_labels = function(x, ...) {
  x = data.frame(value = unclass(x), label = names(x), row.names = NULL,
                 stringsAsFactors = FALSE)
  return(x)
}
#' @rdname labels
#' @param x obiekt klasy \emph{var_labels}, \emph{var_labels} lub \emph{val_labels_list}
#' @export
print.var_labels = function(x, ...) {
  print(as.data.frame(x), right = FALSE, row.names = TRUE)
  invisible(x)
}
#' @rdname labels
#' @export
print.val_labels = function(x, ...) {
  print(as.data.frame(x), right = FALSE, row.names = FALSE)
  invisible(x)
}
#' @rdname labels
#' @export
print.val_labels_list = function(x, ...) {
  stopifnot(is.list(x))
  if (length(x) > 0) {
    cat("$", names(x)[1], sep = "")
    labels = x[[1]]
  }
  if (length(x) == 1) {
    cat("\n")
    print(labels)
  } else {
    for (i in 2:length(x)) {
      if (isTRUE(all.equal(labels, x[[i]]))) {
        cat(", $", names(x)[i], sep = "")
      } else {
        cat("\n")
        print(labels)
        cat("\n$", names(x)[i], sep = "")
        labels = x[[i]]
      }
    }
    cat("\n")
    print(labels)
  }
  invisible(x)
}
