#' @importFrom S7 props props<-
granule_inherit_props <- function(x, parent) {
  naive_props <- is.naive(props <- props(x))
  parent_props <- props(parent)
  inherit_props <- intersect(names(props)[naive_props], names(parent_props))
  props[inherit_props] <- parent_props[inherit_props]
  props(x) <- props
  x
}