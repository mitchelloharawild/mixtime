check_tz_name <- function(zone) {
  # Naive time zone
  if (is.na(zone)) return(invisible(TRUE))
  
  # Specified time zone
  if (!zone %in% tzdb::tzdb_names()) {
    cli::cli_abort(
      c(
        "Timezone {.val {zone}} not found in timezone database.",
        "i" = "Valid timezone names can be found with {.fn tzdb::tzdb_names}."
      ),
      call = NULL
    )
  }
  invisible(TRUE)
}

#' Compute circular rolling sums
#'
#' Calculates rolling sums of length `k` for all contiguous subsequences
#' around a circular vector. Returns sums for each valid k-element window
#' that wraps around the vector as if arranged in a circle.
#'
#' @param x A numeric vector to compute circular sums over.
#' @param size Integer; the window size (number of consecutive elements to sum).
#' @param step Integer; the step size (the increment in starting index for each sum).
#'
#' @return A numeric vector containing the sum of each contiguous subsequence 
#'   around the circle. The length of the resulting vector is the number of
#'   combinations until the pattern between `x` and `step` repeats
#'
#' @examples
#' # Simple circular sum with window of 2
#' circsum(c(1, 2, 3, 4), 2)
#' # Returns: 3 7 (1+2, 3+4)
#'
#' # Window of 3 elements
#' circsum(c(1, 2, 3, 4, 5), 3)
#' # Returns: 6 10 9 8 12 (1+2+3, 4+5+1, 2+3+4, 5+1+2, 3+4+5)
#' 
#' @export
circsum <- function(x, size, step = size) {
  n <- length(x)
  if (n == 0L || size <= 0L || step <= 0L) {
    return(numeric(0))
  }
  
  if (size == 1L) {
    if (step == 1L) {
      return(x)
    }
    num_windows <- n / gcd(n, step)
    indices <- ((seq_len(num_windows) - 1L) * step) %% n + 1L
    return(x[indices])
  }
  
  # Number of unique windows before pattern repeats
  num_windows <- n / gcd(n, step)
  
  # Generate all window sums
  vapply(seq_len(num_windows), function(i) {
    start <- ((i - 1L) * step) %% n
    indices <- (start + seq_len(size) - 1L) %% n + 1L
    sum(x[indices])
  }, numeric(1))
}

gcd <- function(a, b) {
  while (b != 0L) {
    temp <- b
    b <- a %% b
    a <- temp
  }
  a
}

# Evaluate time units in a calendar context
eval_cal <- function(expr, cal) {
  eval_tidy({{expr}}, data = cal)
}

quo_add_dots <- function(quo, ...){
  dots <- enquos(...)
  expr <- rlang::quo_get_expr(quo)
  rlang::quo_set_expr(
    quo, 
    as.call(c(as.list(expr), list2(...)))
  )
}

# Similar to S7::new_S3_class to define S4 methods without depending on packages
new_S4_class <- function(className, package) {
  methods::newClassRepresentation(
    className = className, package = package
  )
}

# From S7:::topNamespaceName, required for new_time_unit wrapper
# Wrapper is required to patch https://github.com/RConsortium/S7/issues/609
topNamespaceName <- function (env = parent.frame()) {
  env <- topenv(env)
  if (!isNamespace(env)) {
    return()
  }
  as.character(getNamespaceName(env))
}

# Resolve a cli-style {?singular/plural} or {?zero/singular/plural} tokens
str_plural <- function(template, n) {
  m <- regexpr("\\{\\?[^}]*\\}", template, perl = TRUE)
  if (m[[1L]] == -1L) return(rep_len(template, length(n)))

  ml <- attr(m, "match.length")
  parts <- strsplit(substr(template, m + 2L, m + ml - 2L), "/", fixed = TRUE)[[1L]]
  prefix <- substr(template, 1L, m - 1L)
  suffix <- substr(template, m + ml, nchar(template))
  
  # +s suffix if no plural form is provided
  if (length(parts) == 1L) {
    parts <- c(parts, paste0(parts, "s"))
  }

  idx <- if (length(parts) == 3L) {
    ifelse(n == 0, 1L, ifelse(n == 1L, 2L, 3L))
  } else {
    ifelse(n == 1L, 1L, 2L)
  }

  paste0(prefix, parts[idx], suffix)
}