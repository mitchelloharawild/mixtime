check_tz_name <- function(zone) {
  if (identical(zone, "")) return(invisible(TRUE))
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