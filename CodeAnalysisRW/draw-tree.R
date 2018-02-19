library(stringr)

call_tree_mod <- function(x, width = getOption("width")) {
  if (is.expression(x) || is.list(x)) {
    trees <- vapply(x, tree_mod, character(1), width = width)
    out <- str_c(trees, collapse = "\n\n")
  } else {
    out <- tree_mod(x, width = width)
  }

  
  #cat(out, "\n") #uncomment for original code
  return(out)
}

ast_mod <- function(x) call_tree_mod(substitute(x))

str_trunc_mod <- function(x, width = getOption("width")) {
  ifelse(str_length(x) <= width, x, str_c(str_sub(x, 1, width - 3), "..."))
}

tree_mod <- function(x, level = 1, width = getOption("width"), branch = "| ") {
    indent <- str_c(str_dup("  ", level - 1), branch)

    if (is.atomic(x) && length(x) == 1) {
          label <- paste0(" ", deparse(x)[1])
          children <- NULL


    } else if (is.name(x)) {
          x <- as.character(x)
          if (x == "") {
            # Special case the missing argument
            label <- "`MISSING"
          } else {
            label <- paste0("", as.character(x))
          }

          children <- NULL

    } else if (is.call(x)) {
          
          label <- "()"
          children <-  vapply(as.list(x), tree_mod, character(1), level = level + 1, width = width - 3)

    } else if (is.pairlist(x)) {
          label <- "[]"

          branches <- paste("\\", format(names(x)), "=")
          children <- character(length(x))
          for (i in seq_along(x)) {
              children[i] <- tree_mod(x[[i]], level = level + 1, width = width - 3, branch = branches[i])
          }

    } else {
          # Special case for srcrefs, since they're commonly seen
          if (inherits(x, "srcref")) {
            label <- "<srcref>"
          } else {
            label <- paste0("<", typeof(x), ">")
          }
          children <- NULL
    }

    label <- str_trunc_mod(label, width - 3)

    if (is.null(children)) {
      paste0(indent, label)
    } else {
      paste0(indent, label, "\n", paste0(children, collapse = "\n"))
    }
}


#call_tree_mod()


AddItemDoubling <- function(item){
    Counter <- 0
    Result <- list(NULL)
    Size <- 1


    if( .GlobalEnv$Counter == .GlobalEnv$Size )
    {
        length(.GlobalEnv$Result) <- .GlobalEnv$Size <- .GlobalEnv$Size * 2
    }

    .GlobalEnv$Counter <- .GlobalEnv$Counter + 1

    .GlobalEnv$Result[[.GlobalEnv$Counter]] <- item
}