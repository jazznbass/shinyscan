

get_call <- function(func) {
  args <- names(formals(func))
  values <- formals(func)
  for(i in seq_along(values)) {
    if (inherits(values[[i]], "character"))
      values[[i]] <- paste0("\"", values[[i]], "\"")
  }
  values <- as.character(values)

  out <- paste0(func, "(", paste0(args, " = ", values, collapse = ", "), ")")
  out <- gsub(" = ,", ",", out)
  out <- gsub(" = )", ")", out)
  out
}


