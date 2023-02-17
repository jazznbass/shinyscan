#
#
# get_call <- function(func) {
#   args <- names(formals(func))
#   values <- formals(func)
#
#   id <- which(!args %in% c("dvar", "pvar", "mvar", "phases", "meta_method",
#                            "data", "scdf", "data.l2", "offset", "lag.max",
#                            "graph", "output", "seed", "startpoints"))
#   args <- args[id]
#   values <- values[id]
#
#   if (length(args) == 0) return(paste0(func, " arguments: none"))
#   for(i in seq_along(values)) {
#     if (inherits(values[[i]], "character"))
#       values[[i]] <- paste0("\"", values[[i]], "\"")
#   }
#   values <- as.character(values)
#
#   args <- paste0(args, collapse = "\n")
#   out <- paste0(func, " arguments:\n", args)
#   out <- gsub(" = ,", ",", out)
#   out <- gsub(" = )", ")", out)
#   out
# }
#
#
