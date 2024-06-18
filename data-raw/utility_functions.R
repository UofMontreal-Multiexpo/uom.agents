## Utility functions for data preparation.


#' Capitalize a character string
#' 
#' Change the first letter of a string to uppercase.
#' 
#' @param x Character string to be capitalized.
#' @return The character string corresponding to the argument `x`, starting with
#'  a capital letter.
#' 
#' @author Gauthier Magnin
#' @template function_not_exported
#' 
cap = function(x) {
  y = paste0(toupper(substring(x, 1, 1)), substring(x, 2))
  y[is.na(x)] = NA_character_
  return(y)
}


#' Replace NA elements in list with empty vectors
#' 
#' Replace NA elements in a list with empty vectors of the specified type.
#'  Only elements containing a single NA value are replaced, other NA values
#'  remain as is.
#' 
#' @param x List to be modified.
#' @param type Type of vectors to be used to replace NA elements (numeric,
#'  character...).
#' @return List `x` whose NA elements have been replaced with empty vectors.
#' 
#' @author Gauthier Magnin
#' @template function_not_exported
#' 
replace_na_in_list = function(x, type = "numeric") {
  x[is.na(x)] = rep(list(eval(parse(text = paste0(type, "(0)")))),
                    sum(is.na(x)))
  return(x)
}


#' Extend character values
#' 
#' Extend character values to the left by adding a repeated character until
#'  they get a given number of characters.
#' 
#' @details
#' `NA` values always remain unchanged.
#' 
#' @param x Character values to extend.
#' @param char Character to use to extend values from `x`.
#' @param nb Number of characters each value must have at the end of the
#'  process.
#' @return Vector corresponding to `x` in which all values have the same number
#'  of characters.
#' 
#' @author Gauthier Magnin
#' @template function_not_exported
#' 
extend_left = function(x, char, nb = max(nchar(x))) {
  x[!is.na(x)] = gsub(" ", char, sprintf(paste0("%", nb, "s"), x[!is.na(x)]))
  return(x)
}


#' Convert data type
#' 
#' Change the type of a vector.
#' 
#' @param x Vector to typecast.
#' @param type Type to which to convert `x`.
#' @param date Date format if `x` has to be converted from `character` to
#'  `Date`.
#' @return Vector corresponding to `x` whose type is `type`.
#' 
#' @author Gauthier Magnin
#' @template function_not_exported
#' 
typecast = function(x, type, date = "%Y%m%d") {
  switch(type,
         factor  = as.factor(x),
         Date    = as.Date(x, format = date),
         numeric = {
           if (class(x) == "factor") as.numeric(as.character(x))
           else as.numeric(x)
         },
         as(x, type))
}


#' Check the validity of keys identifying each row
#' 
#' Check the existence of a key for each row and the uniqueness of each key.
#'  Print diagnostic messages if keys are not valid.
#' 
#' @details
#' Keys are considered as invalid if any of the following conditions is met.
#'  * A key appears more than once.
#'  * There is data associated with no key (i.e., the key or one of its part is
#'    `NA`).
#' 
#' @param table Data frame containing data to verify.
#' @param keys Numbers of the columns containing the identifiers (i.e., forming
#'  the unique keys).
#' @param name Optional. Name of the data frame to use in the diagnostic
#'  message.
#' @return `TRUE` or `FALSE` whether keys are valid.
#' 
#' @author Gauthier Magnin
#' @template function_not_exported
#' 
validate_keys = function(table, keys = 1, name = NULL) {
  
  name = if (!is.null(name)) paste0(name, " ") else character(0)
  valid = TRUE
  
  if (any(duplicated(table[keys]))) {
    message("Invalid ", name, "table. ",
            "Certain keys appear more than once.")
    valid = FALSE
  }
  
  if (any(apply(table, 1, function(row) any(is.na(row[keys])) & any(!is.na(row[-keys]))))) {
    message("Invalid ", name, "table. ",
            "There is data associated with no key (i.e., the key or one of its part is NA).")
    valid = FALSE
  }
  
  return(valid)
}


#' Turn a two-column structure into a named vector
#' 
#' Turn a two-column data frame or matrix into a named vector using one column
#'  as names and the other one as values.
#' 
#' @param x Data frame or matrix to turn into a vector.
#' @param names Number of the column of `x` to use as names.
#' @param values Number of the column of `x` to use as values.
#' @return Named vector.
#' 
#' @author Gauthier Magnin
#' @template function_not_exported
#' 
turn_2c_into_vec = function(x, names = 1, values = 2) {
  return(stats::setNames(x[, values], x[, names]))
}


#' Resave data with the best compression method
#' 
#' Search for the best compression method to save existing '.RData' or '.rda'
#'  files and resave them with this method.
#' 
#' @details
#' Use of the maximum compression level (9).
#' 
#' @param paths A character vector of paths to found data and save files.
#' 
#' @author Gauthier Magnin
#' @seealso [`tools::resaveRdaFiles`], [`tools::checkRdaFiles`].
#' @template function_not_exported
#' 
resave_with_best_compression = function(paths){
  
  # Checking the existence of the package tools
  # (included in "Suggests" field of the DESCRIPTION file)
  if (!requireNamespace("tools", quietly = TRUE)) {
    stop("Package \"tools\" needed for this function to work. Please install it.",
         call. = FALSE)
  }
  
  # Compression methods
  methods = c("gzip", "bzip2", "xz")
  
  # For each file
  for (p in paths) {
    
    # Display current file
    if (length(paths) != 1) cat("File:", p,"\n")
    cat("File size according to compression method:\n")
    
    # For each compression method, compress the data and get the file size
    sizes = sapply(methods, function(m) {
      tools::resaveRdaFiles(p, compress = m, compression_level = 9)
      return(tools::checkRdaFiles(p)$size)
    })
    names(sizes) = methods
    
    # Selecting the best compression method
    best = methods[which.min(sizes)]
    
    # Display of results and optimal choice
    print(sizes)
    cat("Use of '", best, "' compression method.\n", sep = "")
    if (p != paths[length(paths)]) cat("\n")
    
    # Compress again using the best method
    if (best != methods[3]) {
      tools::resaveRdaFiles(p, compress = best, compression_level = 9)
    }
  }
}
