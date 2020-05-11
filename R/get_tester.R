#' @title Has dimension?
#' 
#' @details Internal function. 
#' 
#' @param x an R object
#' @keywords internal
#' @export
#'
has_dimension <- function(x) {
  if (!is.null(dim(x))) TRUE else FALSE
}

has_dim <- function(x) {
  has_dimension(x)
}

lacks_dimension <- function(x) {
  !has_dimension(x)
}

lacks_dim <- function(x) {
  !has_dimension(x)
}
#' @title Has missing values, NA, NaN, Inf
#' 
#' @details Internal function. 
#' 
#' 
#' @param x an R object
#' @keywords internal
#' @export
#'
has_missing <- function(x) {
  if (sum(is.na(x)) > 0) TRUE else FALSE
}

has_infinite <- function(x) {
  if (sum(is.infinite(x)) > 0) TRUE else FALSE
}

has_not_a_number <- function(x) {
  if (sum(is.nan(x)) > 0) TRUE else FALSE
}

has_NA <- function(x) {
  has_missing(x)
}

has_Inf <- function(x) {
  has_infinite(x)
}

has_NaN <- function(x) {
  has_not_a_number(x)
}

has_nas <- function(x) {
  has_NA(x) | has_Inf(x) | has_NaN(x)
}
#' @title Has or lacks names?
#' 
#' @details Internal function. 
#' 
#' @param x an R object
#' @keywords internal
#' @export
#'
has_names <- function(x) {
  if (!is.null(names(x))) TRUE else FALSE
}

lacks_names <- function(x) {
  !has_names(x)
}
#' @title Has or lacks row/column names?
#' 
#' @details Internal function. 
#' 
#' @param x an R object
#' @keywords internal
#' @export
#'
has_rownames <- function(x) {
  if (!is.null(rownames(x))) TRUE else FALSE
}

has_colnames <- function(x) {
  if (!is.null(colnames(x))) TRUE else FALSE
}

has_dimnames <- function(x) {
  if (!is.null(dimnames(x))) TRUE else FALSE
}

lacks_rownames <- function(x) {
  !has_rownames(x)
}

lacks_colnames <- function(x) {
  !has_colnames(x)
}

lacks_dimnames <- function(x) {
  !has_dimnames(x)
}

#' @title Has factors?
#' 
#' @details Internal function. 
#' 
#' @param x an R object
#' @keywords internal
#' @export
#'
has_factors <- function(x) {
  if (is.data.frame(x) | is.list(x)) {
    factors = unlist(lapply(x, is.factor))
    if (sum(factors) > 0) TRUE else FALSE
  } else FALSE
}
#' @title Is class
#' 
#' @details Internal function. 
#' 
#' @param x an R object
#' @param name string giving the class to be tested
#' @keywords internal
#' @export
#'
is_class <- function(x, name=NULL) {
  if (is.null(name))
    stop("\n'name' is missing with no default")
  if (is_string(name)) {
    if (class(x) == name) TRUE else FALSE    
  } else {
    stop("\n'name' must be a string")
  }
}
#' @title Is data frame
#' 
#' @details Internal function. 
#' 
#' @param x an R object
#' @keywords internal
#' @export
#'
is_dataframe <- function(x) {
  if (is.data.frame(x)) TRUE else FALSE
}

is_numeric_dataframe <- function(x) {
  if (is.data.frame(x)) {
    numerics = unlist(lapply(x, is.numeric))
    if (sum(numerics) == dim(x)[2L]) TRUE else FALSE
  } else FALSE
}

is_string_dataframe <- function(x) {
  if (is.data.frame(x)) {
    characters = unlist(lapply(x, is.character))
    if (sum(characters) == dim(x)[2L]) TRUE else FALSE
  } else FALSE
}

is_factor_dataframe <- function(x) {
  if (is.data.frame(x)) {
    factors = unlist(lapply(x, is.factor))
    if (sum(factors) == dim(x)[2L]) TRUE else FALSE
  } else FALSE
}

is_not_dataframe <- function(x) {
  !is_dataframe(x)
}
#' @title Is decimal
#' 
#' @details Internal function. 
#' 
#' @details decimal is any number in the intervals (-1,0) and (0,1) 
#' @param x an R object
#' @keywords internal
#' @export
#'
is_decimal <- function(x) {
  UseMethod("is_decimal", x)
}

#' @S3method is_decimal default
is_decimal.default <- function(x) {
  if (mode(x) != "numeric") FALSE
}

#' @S3method is_decimal factor
is_decimal.factor <- function(x) {
  FALSE
}

#' @S3method is_decimal numeric
is_decimal.numeric <- function(x) {
  (abs(x) > 0 & abs(x) < 1) 
}

is_not_decimal <- function(x) {
  !is_decimal(x)
}
#' @title Is positive decimal
#' 
#' @details Internal function. 
#' 
#' @param x an R object
#' @keywords internal
#' @export
#'
is_positive_decimal <- function(x) {
  UseMethod("is_positive_decimal", x)
}

#' @S3method is_positive_decimal default
is_positive_decimal.default <- function(x) {
  if (is_positive(x) & is_decimal(x)) TRUE else FALSE
}
#' @title Is negative decimal
#' 
#' @details Internal function. 
#' 
#' @param x an R object
#' @keywords internal
#' @export
#'
is_negative_decimal <- function(x) {
  UseMethod("is_negative_decimal", x)
}

#' @S3method is_negative_decimal default
is_negative_decimal.default <- function(x) {
  if (is_negative(x) & is_decimal(x)) TRUE else FALSE
}
#' @title Is integer
#' @details Internal function. 
#' 
#' @param x an R object
#' @keywords internal
#' @export
#'
is_integer <- function(x) {
  UseMethod("is_integer", x)
}

#' @S3method is_integer default
is_integer.default <- function(x) {
  if (mode(x) != "numeric") FALSE
}

#' @S3method is_integer factor
is_integer.factor <- function(x) {
  FALSE
}

#' @S3method is_integer numeric
is_integer.numeric <- function(x) {
  (x %% 1) == 0
}

is_not_integer <- function(x) {
  !is_integer(x)
}
#' @title Is positive integer
#' 
#' @details Internal function. 
#' 
#' @param x an R object
#' @keywords internal
#' @export
#'
is_positive_integer <- function(x) {
  (is_positive(x) & is_integer(x))
}
#' @title Is negative integer
#' @description Test if is a positive integer
#' @param x an R object
#' @keywords internal
#' @export
#'
is_negative_integer <- function(x) {
  (is_negative(x) & is_integer(x))
}
#' @title Is matrix
#' 
#' @details Internal function. 
#' 
#' @param x an R object
#' @keywords internal
#' @export
#'
is_matrix <- function(x) {
  is.matrix(x)
}

is_numeric_matrix <- function(x) {
  if (!is.matrix(x)) return(FALSE)
  is.numeric(x)
}

is_string_matrix <- function(x) {
  if (!is.matrix(x)) return(FALSE)
  is.character(x)
}

is_logical_matrix <- function(x) {
  if (!is.matrix(x)) return(FALSE)
  is.logical(x)
}

is_not_matrix <- function(x) {
  !is_matrix(x)
}

#' @title Is multiple
#' 
#' @details Internal function. 
#' 
#' @param x a numeric object
#' @param of a given number
#' @keywords internal
#' @export
#'
is_multiple <- function(x, of) 
{
  if (is_not_scalar(of))
    stop("\n'of' must be a scalar")
  
  (x %% of) == 0
}
#' @title Is natural
#' 
#' @details Internal function. 
#' 
#' @param x an R object
#' @keywords internal
#' @export
#'
is_natural <- function(x) {
  UseMethod("is_natural", x)
}

#' @S3method is_natural default
is_natural.default <- function(x) {
  if (mode(x) != "numeric") FALSE
}

#' @S3method is_natural factor
is_natural.factor <- function(x) {
  FALSE
}

#' @S3method is_natural numeric
is_natural.numeric <- function(x) {
  ints <- (x %% 1) == 0
  ints & (x > 0)
}

is_not_natural <- function(x) {
  !is_natural(x)
}
#' @title Test if an object has one-dimension
#' 
#' @details Internal function. 
#' 
#' @param x an R object
#' @return whether x is one-dimensional
#' @keywords internal
#' @export
#'
is_one_dim <- function(x)
{
  one_dim = TRUE
  if (lacks_dim(x)) {
    if (is.list(x)) one_dim = FALSE
  } else {
    if (dim(x)[1L] > 1 && dim(x)[2L] > 1)
      one_dim = FALSE
  }
  # output
  one_dim
}
#' @title Test if an object is multi-dimensional
#' 
#' @details Internal function. 
#' 
#' @param x an R object
#' @return whether x is multi-dimensional
#' @keywords internal
#' @export
#'
is_multidim <- function(x) 
{
  multidim = TRUE
  if (lacks_dim(x)) {
    return(FALSE)
  } else {
    if (dim(x)[1L] > 1 && dim(x)[2L] > 1) {
      multidim = TRUE
    } else {
      multidim = FALSE
    }
  }
  # output
  multidim
}
#' @title Is positive
#' 
#' @details Internal function. 
#' 
#' @param x an R object
#' @keywords internal
#' @export
#'
is_positive <- function(x) {
  UseMethod("is_positive", x)
}

#' @S3method is_positive default
is_positive.default <- function(x) {
  if (mode(x) != "numeric") FALSE
}

#' @S3method is_positive numeric
is_positive.numeric <- function(x) {
  x > 0
}

#' @S3method is_positive matrix
is_positive.matrix <- function(x) {
  x > 0
}

#' @S3method is_positive factor
is_positive.factor <- function(x) {
  FALSE
}

is_not_positive <- function(x) {
  !is_positive(x)
}

#' @title Is negative
#' 
#' @details Internal function. 
#' 
#' @param x an R object
#' @keywords internal
#' @export
#'
is_negative <- function(x) {
  UseMethod("is_negative", x)
}

#' @S3method is_negative default
is_negative.default <- function(x) {
  if (mode(x) != "numeric") FALSE
}

#' @S3method is_negative numeric
is_negative.numeric <- function(x) {
  x < 0
}

#' @S3method is_negative matrix
is_negative.matrix <- function(x) {
  x < 0
}

#' @S3method is_negative factor
is_negative.factor <- function(x) {
  FALSE
}

is_not_negative <- function(x) {
  !is_negative(x)
}
#' @title Is scalar
#' 
#' @details Internal function. 
#' 
#' @param x an R object
#' @keywords internal
#' @export
#'
is_scalar <- function(x) {
  is_single_number(x)
}

is_not_scalar <- function(x) {
  !is_single_number(x)
}

is_positive_scalar <- function(x) {
  is_single_positive(x)
}

is_negative_scalar <- function(x) {
  is_single_negative(x)
}
#' @title Is single
#' 
#' @details Internal function. 
#' 
#' @param x an R object
#' @keywords internal
#' @export
#'
is_single <- function(x) {
  (length(x) == 1)
}
#' @title Is single string
#' 
#' @details Internal function. 
#' 
#' @param x an R object
#' @keywords internal
#' @export
#'
is_single_string <- function(x) {
  if (is_single(x)) {
    is_string(x)
  } else FALSE
}
#' @title Is single number
#' 
#' @details Internal function. 
#' 
#' 
#' @param x an R object
#' @keywords internal
#' @export
#'
is_single_number <- function(x) {
  if (is_single(x)) {
    (is.numeric(x) && is.finite(x))
  } else FALSE
}
#' @title Is single positive number
#' 
#' @details Internal function. 
#' 
#' @param x an R object
#' @keywords internal
#' @export
#'
is_single_positive <- function(x) {
  if (is_single(x)) {
    is_positive(x)
  } else FALSE
}
#' @title Is single negative number
#' 
#' @details Internal function. 
#' 
#' @param x an R object
#' @keywords internal
#' @export
#'
is_single_negative <- function(x) {
  if (is_single(x)) {
    is_negative(x)
  } else FALSE
}
#' @title Is single decimal
#' 
#' @details Internal function. 
#' 
#' @param x an R object
#' @keywords internal
#' @export
#'
is_single_decimal <- function(x) {
  if (is_single(x)) {
    is_decimal(x)
  } else FALSE
}
#' @title Is single positive decimal
#' 
#' @details Internal function. 
#' 
#' @param x an R object
#' @keywords internal
#' @export
#'
is_single_positive_decimal <- function(x) {
  if (is_single(x)) {
    is_positive_decimal(x)
  } else FALSE
}
#' @title Is single negative decimal
#' 
#' @details Internal function. 
#' 
#' @param x an R object
#' @keywords internal
#' @export
#'
is_single_negative_decimal <- function(x) {
  if (is_single(x)) {
    is_negative_decimal(x)
  } else FALSE
}
#' @title Is single positive integer
#' 
#' @details Internal function. 
#' 
#' @param x an R object
#' @keywords internal
#' @export
#'
is_single_positive_integer <- function(x) {
  if (is_single(x)) {
    is_positive_integer(x)
  } else FALSE
}
#' @title Is single negative integer
#' 
#' @details Internal function. 
#' 
#' @param x an R object
#' @keywords internal
#' @export
#'
is_single_negative_integer <- function(x) {
  if (is_single(x)) {
    is_negative_integer(x)
  } else FALSE
}
#' @title Is single odd
#' 
#' @details Internal function. 
#' 
#' @param x an R object
#' @keywords internal
#' @export
#'
is_single_odd <- function(x) {
  if (is_single(x)) {
    is_odd(x)
  } else FALSE
}
#' @title Is single even
#' 
#' @details Internal function. 
#' 
#' @param x an R object
#' @keywords internal
#' @export
#'
is_single_even <- function(x) {
  if (is_single(x)) {
    is_even(x)
  } else FALSE
}
#' @title Is single logical
#' 
#' @details Internal function. 
#' 
#' @param x an R object
#' @keywords internal
#' @export
#'
is_single_logical <- function(x) {
  if (is_single(x)) {
    if (is.na(x)) {
      FALSE
    } else {
      is.logical(x)      
    }
  } else FALSE
}
#' @title Is single true
#' 
#' @details Internal function. 
#' 
#' @param x an R object
#' @keywords internal
#' @export
#'
is_single_true <- function(x) {
  if (is_single(x)) {
    is_TRUE(x)
  } else FALSE
}
#' @title Is single false
#' 
#' @details Internal function. 
#' 
#' @param x an R object
#' @keywords internal
#' @export
#'
is_single_false <- function(x) {
  if (is_single(x)) {
    is_FALSE(x)
  } else FALSE
}
#' @title Is string
#' @details Internal function. 
#' 
#' @param x an R object
#' @keywords internal
#' @export
#'
is_string <- function(x) {
  if (is.character(x)) TRUE else FALSE
}

is_not_string <- function(x) {
  !is_string(x)
}
#' @title Is tabular
#' 
#' @details Internal function. 

#' @param x an R object
#' @keywords internal
#' @export
#'
is_tabular <- function(x) {
  if (is.matrix(x) | is.data.frame(x)) {
    TRUE
  } else FALSE  
}

is_numeric_tabular <- function(x) {
  if (is_numeric_matrix(x) | is_numeric_dataframe(x)) {
    TRUE
  } else FALSE  
}

is_string_tabular <- function(x) {
  if (is_string_matrix(x) | is_string_dataframe(x)) {
    TRUE
  } else FALSE  
}

is_not_tabular <- function(x) {
  !is_tabular(x)  
}
#' @title Is triangular matrix
#' 
#' @details Internal function. 
#' 
#' @param x a matrix
#' @param diag should the diagonal be included? (\code{FALSE} by default)
#' @keywords internal
#' @export
#'
is_lower_triangular <- function(x, diag = FALSE) {
  if (is.matrix(x)) {
    all(x[upper.tri(x, diag = diag)] == 0)
  } else FALSE
}

is_upper_triangular <- function(x, diag = FALSE) {
  if (is.matrix(x)) {
    all(x[lower.tri(x, diag = diag)] == 0)
  } else FALSE
}

is_triangular_matrix <- function(x, diag = FALSE) {
  if (is.matrix(x)) {
    is_lower_triangular(x) | is_upper_triangular(x)
  } else FALSE
}

#' @title Is vector
#' 
#' @details Internal function. 
#' 
#' @param x an R object
#' @keywords internal
#' @export
#'
is_vector <- function(x) {
  if (is.vector(x)) TRUE else FALSE
}

is_numeric_vector <- function(x) {
  if (is.vector(x) & is.numeric(x)) TRUE else FALSE
}

is_string_vector <- function(x) {
  if (is.vector(x) & is.character(x)) TRUE else FALSE
}

is_logical_vector <- function(x) {
  if (is.vector(x) & is.logical(x)) TRUE else FALSE
}

is_not_vector <- function(x) {
  !is_vector(x)
}
#' @title Is rectangular matrix
#' 
#' @details Internal function. 
#' 
#' @param x an R object
#' @keywords internal
#' @export
#'
is_rectangular_matrix <- function(x) {
  if (is.matrix(x)) {
    if (nrow(x) != ncol(x)) TRUE else FALSE      
  } else FALSE
}

is_not_rectangular_matrix <- function(x) {
  if (is.matrix(x)) {
    if (nrow(x) == ncol(x)) TRUE else FALSE      
  } else TRUE
}

is_tall_matrix <- function(x) {
  if (is.matrix(x)) {
    if (nrow(x) > ncol(x)) TRUE else FALSE
  } else FALSE
}

is_wide_matrix <- function(x) {
  if (is.matrix(x)) {
    if (nrow(x) < ncol(x)) TRUE else FALSE
  } else FALSE
}
#' @title Is square matrix
#' 
#' @details Internal function. 
#' 
#' @param x an R object
#' @keywords internal
#' @export
#'
is_square_matrix <- function(x) {
  if (is.matrix(x)) {
    if (nrow(x) == ncol(x)) TRUE else FALSE      
  } else FALSE
}
is_not_square_matrix <- function(x) {
  if (is.matrix(x)) {
    if (nrow(x) != ncol(x)) TRUE else FALSE      
  } else TRUE
}
#' @title Is square numeric matrix
#' 
#' @details Internal function. 
#' 
#' @param x an R object
#' @keywords internal
#' @export
#'
is_square_numeric_matrix <- function(x) {
  if (is_numeric_matrix(x)) {
    if (nrow(x) == ncol(x)) TRUE else FALSE      
  } else FALSE
}
is_not_square_numeric_matrix <- function(x) {
  !is_square_numeric_matrix(x)
}
#' @title Is diagonal matrix
#' 
#' @details Internal function. 
#' 
#' @param x an R object
#' @keywords internal
#' @export
#'
is_diagonal <- function(x) {
  if (is_square_matrix(x)) {
    above = sum(x[upper.tri(x)])
    below = sum(x[lower.tri(x)])
    if (above > 0 | below > 0) FALSE else TRUE      
  } else FALSE
}
is_not_diagonal <- function(x) {
  !is_diagonal(x)
}
#' @title List of vectors
#' 
#' @details Internal function. 
#' 
#' @param x an R object
#' @keywords internal
#' @export
#'
list_of_vectors <- function(x) {
  if (is.list(x)) {
    vectors = unlist(lapply(x, is.vector))
    if (sum(vectors) == length(x)) TRUE else FALSE    
  } else FALSE
}

list_of_numeric_vectors <- function(x) {
  if (is.list(x)) {
    vectors = unlist(lapply(x, is_numeric_vector))
    if (sum(vectors) == length(x)) TRUE else FALSE    
  } else FALSE
}

list_of_string_vectors <- function(x) {
  if (is.list(x)) {
    vectors = unlist(lapply(x, is_string_vector))
    if (sum(vectors) == length(x)) TRUE else FALSE    
  } else FALSE
}

list_of_logical_vectors <- function(x) {
  if (is.list(x)) {
    vectors = unlist(lapply(x, is_logical_vector))
    if (sum(vectors) == length(x)) TRUE else FALSE    
  } else FALSE
}
#' @title List with vectors
#' 
#' @details Internal function. 
#' 
#' @param x an R object
#' @keywords internal
#' @export
#'
list_with_vectors <- function(x) {
  if (is.list(x)) {
    vectors = unlist(lapply(x, is.vector))
    if (sum(vectors) > 0) TRUE else FALSE    
  } else FALSE
}

list_with_numeric_vectors <- function(x) {
  if (is.list(x)) {
    vectors = unlist(lapply(x, is_numeric_vector))
    if (sum(vectors) > 0) TRUE else FALSE    
  } else FALSE
}

list_with_string_vectors <- function(x) {
  if (is.list(x)) {
    vectors = unlist(lapply(x, is_string_vector))
    if (sum(vectors) > 0) TRUE else FALSE    
  } else FALSE
}
#' @title Is even
#' 
#' @details Internal function. 
#' 
#' @param x an R object
#' @keywords internal
#' @export
#'
is_even <- function(x) {
  UseMethod("is_even", x)
}

#' @S3method is_even default
is_even.default <- function(x) {
  if (mode(x) != "numeric") FALSE
}

#' @S3method is_even numeric
is_even.numeric <- function(x) {
  (x %% 2) == 0
}

#' @S3method is_even matrix
is_even.matrix <- function(x) {
  (x %% 2) == 0
}

#' @S3method is_even factor
is_even.factor <- function(x) {
  FALSE
}

is_not_even <- function(x) {
  !is_even(x)
}
#' @title Is even
#' 
#' @details Internal function. 
#' 
#' @param x an R object
#' @keywords internal
#' @export
#'
is_odd <- function(x) {
  UseMethod("is_odd", x)
}

#' @S3method is_odd default
is_odd.default <- function(x) {
  if (mode(x) != "numeric") FALSE
}

#' @S3method is_odd numeric
is_odd.numeric <- function(x) {
  (x %% 2) != 0
}

#' @S3method is_odd matrix
is_odd.matrix <- function(x) {
  (x %% 2) != 0
}

#' @S3method is_odd factor
is_odd.factor <- function(x) {
  FALSE
}

is_not_odd <- function(x) {
  !is_odd(x)
}


#' @title Same Class
#' 
#' @details Internal function. 
#' 
#' @param x an R object
#' @param y an R object
#' @keywords internal
#' @export
#'
same_class <- function(x, y) {
  identical(class(x), class(y))
}

different_class <- function(x, y) {
  !same_class(x, y)
}
#' @title Same Dimension
#' 
#' @details Internal function. 
#' 
#' @param x a matrix
#' @param y a matrix
#' @keywords internal
#' @export
#'
same_dim <- function(x, y)
{
  if (is_not_tabular(x) || is_not_tabular(y))
    stop("\n'same_dim()' requires matrices or data frames")
  # output
  identical(dim(x), dim(y))
}

different_dim <- function(x, y) {
  !same_dim(x, y)
}

#' @title Same Number of Rows / Columns
#' 
#' @details Internal function. 
#' 
#' @param x a matrix
#' @param y a matrix
#' @keywords internal
#' @export
#'
same_nrow <- function(x, y)
{
  if (is_not_tabular(x) || is_not_tabular(y))
    stop("\n'same_nrow()' requires two matrices (or data frames)")
  # output
  (nrow(x) == nrow(y))
}

different_nrow<- function(x, y) {
  !same_nrow(x, y)
}

same_ncol <- function(x, y)
{
  if (is_not_tabular(x) || is_not_tabular(y))
    stop("\n'same_ncol()' requires two matrices (or data frames)")
  # output
  (ncol(x) == ncol(y))
}

different_ncol<- function(x, y) {
  !same_ncol(x, y)
}
#' @title Same Length
#' 
#' @details Internal function. 
#' 
#' @param x a matrix
#' @param y a matrix
#' @keywords internal
#' @export
#'
same_length <- function(x, y) {
  (length(x) == length(y))
}

different_length <- function(x, y) {
  !same_length(x, y)
}
#' @title Same Mode
#' 
#' @details Internal function. 
#' 
#' @param x an R object
#' @param y an R object
#' @keywords internal
#' @export
#'
same_mode <- function(x, y) {
  (mode(x) == mode(y))
}

different_mode <- function(x, y) {
  !same_mode(x, y)
}
#' @title Same Type
#' 
#' @details Internal function. 
#' 
#' @param x an R object
#' @param y an R object
#' @keywords internal
#' @export
#'
same_type <- function(x, y) {
  (typeof(x) == typeof(y))
}

different_type <- function(x, y) {
  !same_type(x, y)
}
#' @title If TRUE or FALSE
#' 
#' @details Internal function. 
#' ' 
#' @param x an R object
#' @keywords internal
#' @export
#'
NULL

is_TRUE <- function(x) {
  if (is.logical(x)) {
    if (is.na(x)) {
      FALSE
    } else {
      if (x == TRUE) TRUE else FALSE      
    }
  } else FALSE
}

is_FALSE <- function(x) {
  if (is.logical(x)) {
    if (is.na(x)) {
      FALSE
    } else {
      if (x == FALSE) TRUE else FALSE
    }
  } else FALSE
}

is_true <- function(x) is_TRUE(x)

is_false <- function(x) is_FALSE(x)

true_or_false <- function(x) {
  if (is.logical(x)) x else FALSE
}