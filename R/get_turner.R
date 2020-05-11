#' @title Split a data frame into blocks
#' 
#' @details Internal function. 
#' 
#' @param DataFrame a data frame to split
#' @param blocks either a list or a vector indicating the blocks. If 
#' \code{blocks} is a list of vectors, then the length of each vector defines 
#' the size of the blocks. If \code{blocks} is a vector, then each element 
#' represents the size of the blocks.
#' @param byrow logical. If \code{TRUE} (the default) the data frame 
#' is split by rows, otherwise the data frame is split by columns
#' @return A list of data frames
#' @keywords internal
#' @export
#'
df_to_blocks <- function(DataFrame, blocks, byrow = TRUE)
{
  if (is_not_dataframe(DataFrame))
    stop("\n'df_to_blocks()' requires a data frame")
  if (!is.list(blocks) && !is.numeric(blocks))
    stop("\n'df_to_blocks()' requires a list (or vector)")
  
  num_blocks = length(blocks)
  if (is.list(blocks)) {
    size = listsize(blocks)
    start_end = from_to(blocks)
    from = start_end$from
    to = start_end$to    
  }
  if (is.numeric(blocks)) {
    size = sum(blocks)
    to = cumsum(blocks)
    from = to - blocks + 1
  }
  # empty list to store results
  blocks_list = vector(mode="list", length=num_blocks)
  
  for (k in 1:num_blocks) {
    if (byrow) {
      if (size != nrow(DataFrame))
        stop("\nNumber of rows in 'DataFrame' doesn't match 'blocks'")
      blocks_list[[k]] = DataFrame[from[k]:to[k],]
    } else {
      if (size != ncol(DataFrame))
        stop("\nNumber of columns in 'DataFrame' doesn't match 'blocks'")
      blocks_list[[k]] = DataFrame[,from[k]:to[k]]      
    }
  }
  # output
  blocks_list
}
#' @title Create an indexed list from a dummy matrix
#' 
#' @details Internal function. 
#' 
#' @param Dummy matrix (dummy by columns)
#' @return A list of indexed vectors
#' @keywords internal
#' @export
#'
dummy_to_list <- function(Dummy)
{
  if (is_not_matrix(Dummy))
    stop("\n'dummy_to_list()' requires a (dummy) matrix")
  
  indices = apply(Dummy, 2, function(x) sum(x != 0))
  listify(indices)
}
#' @title Create a dummy matrix from the elements in a factor
#' 
#' @details Internal function. 
#'
#' @param afactor a factor (preferably of vectors)
#' @return A matrix of dummy variables
#' @keywords internal
#' @export
#'
factor_to_dummy <- function(afactor)
{
  if (!is.factor(afactor))
    stop("\n'factor_to_dummy()' requires a factor")
  
  num_obs = length(afactor)
  categs = levels(afactor)
  num_categs = length(categs)
  obs_per_categ = tabulate(afactor)
  
  # build dummy matrix
  dummy_matrix = matrix(0, num_obs, num_categs)
  for (k in 1:num_categs) {
    tmp <- afactor == categs[k]
    dummy_matrix[tmp,k] = 1
  }
  colnames(dummy_matrix) = levels(afactor)
  rownames(dummy_matrix) = 1:num_obs
  # output
  dummy_matrix
}
#' @title Starting and ending positions
#'
#' @details Internal function. 
#'
#' @param x a numeric vector or a list of vectors
#' @param ... further arguments are ignored
#' @return A list with two vectors: '$from' and '$to'.
#' '$from' contains the indices with starting positions.
#' '$to' contains the indices with ending positions.
#' @keywords internal
#' @export
#'
from_to <- function(x, ...) {
  UseMethod("from_to", x)  
}
#' @S3method from_to default
from_to.default <- function(x, ...)
{
  if (!is_numeric_vector(x) || !list_of_vectors(x))
    stop("\n'from_to()' requires a numeric vector or a list of vectors")
}
#' @S3method from_to numeric
from_to.numeric <- function(x, ...)
{
  if (!is_numeric_vector(x))
    stop("\n'from_to()' requires a numeric vector")
  
  to = cumsum(x)
  from = to - x + 1
  list(from=from, to=to)
}
#' @S3method from_to list
from_to.list <- function(x, ...)
{
  if (!list_of_vectors(x))
    stop("\n'from_to()' requires a list of vectors")
  
  aux = unlist(lapply(x, length))
  to = cumsum(aux)
  from = to - aux + 1
  list(from=from, to=to)
}
#' @title Apply a function to all elements in a list
#' 
#' @details Internal function. 
#' 
#' @param alist a list
#' @param f a function to be applied
#' @param ... further arguments passed on to \code{f}
#' @return value
#' @keywords internal
#' @export
#'
funlist <- function(alist, f, ...)
{
  if (!is.list(alist))
    stop("\nA list is required")
  if (!is.function(f))
    stop('\nA function is requried')
  
  f(unlist(alist), ...)
}
#' @title Sum of all elements in a list
#' 
#' @details Internal function. 
#' 
#' @param alist a list
#' @param na.rm logical indicating whether missing values should be removed
#' @return the sum
#' @keywords internal
#' @export
#'
sumlist <- function(alist, na.rm = FALSE)
{
  funlist(alist, sum, na.rm = na.rm)
}
#' @title Product of all elements in a list
#' 
#' @details Internal function. 
#' 
#' @param alist a list
#' @param na.rm logical indicating whether missing values should be removed
#' @return the product
#' @keywords internal
#' @export
#'
prodlist <- function(alist, na.rm = FALSE)
{
  funlist(alist, prod, na.rm = na.rm)
}
#' @title Maximum of all elements in a list
#' 
#' @details Internal function. 
#' 
#' @param alist a list
#' @param na.rm logical indicating whether missing values should be removed
#' @return the maximum
#' @keywords internal
#' @export
#'
maxlist <- function(alist, na.rm = FALSE)
{
  funlist(alist, max, na.rm = na.rm)
}
#' @title Minimum of all elements in a list
#' 
#' @details Internal function. 
#' 
#' @param alist a list
#' @param na.rm logical indicating whether missing values should be removed
#' @return the minimum
#' @keywords internal
#' @export
#'
minlist <- function(alist, na.rm=FALSE)
{
  funlist(alist, min, na.rm=na.rm)
}
#' @title Mean of all elements in a list
#' 
#' @details Internal function. 
#' 
#' @param alist a list
#' @param na.rm logical indicating whether missing values should be removed
#' @return the mean
#' @keywords internal
#' @export
#'
meanlist <- function(alist, na.rm=FALSE)
{
  funlist(alist, mean, na.rm=na.rm)
}
#' @title Create indices for elements in a vector or list
#' 
#' @details Internal function. 
#'
#' @param x a numeric vector or list of vectors
#' @param out string indicating the output format 
#' (\code{"vector"} or \code{"list"})
#' @return A vector (or list) of indexed numbers
#' @keywords internal
#' @export
#'
indexify <- function(x, out) {
  UseMethod("indexify", x)  
}
#' @S3method indexify default
indexify.default <- function(x, ...)
{
  if (!is_numeric_vector(x) || !list_of_vectors(x))
    stop("\n'indexify()' requires a numeric vector or a list of vectors")
}
#' @S3method indexify numeric
indexify.numeric <- function(x, out = "vector")
{
  if (!is_numeric_vector(x))
    stop("\n'indexify()' requires a numeric vector")
  
  if (out == "vector")
    rep(seq_along(x), x)
  else mapply(rep, seq_along(x), x)
}
#' @S3method indexify list
indexify.list <- function(x, out = "vector")
{
  if (!list_of_vectors(x))
    stop("\n'indexify()' requires a list of vectors")
  
  aux = unlist(lapply(x, length))
  if (out == "vector")
    rep(seq_along(aux), aux)
  else mapply(rep, seq_along(aux), aux)
}
#' @title Length of each element within a list
#' 
#' @details Internal function. 
#'
#' @param alist a list
#' @param out string indicating the format of the output (\code{"vector"} or 
#' \code{"list"})
#' @return A vector (or list) with the lengths of the elements in \code{alist}
#' @keywords internal
#' @export
#'
lengths <- function(alist, out = "vector")
{
  if (!is.list(alist)) 
    stop("\n'lengths()' requires a list")
  
  bad_out <- !(out %in% c('vector', 'list'))
  if (bad_out) out = 'vector'
  
  if (out == "vector")
    unlist(lapply(alist, length))
  else lapply(alist, length)
}
#' @title List with vectors of ones
#' 
#' @details Internal function. 
#'
#' @param x a numeric vector
#' @return A list of vectors with ones
#' @keywords internal
#' @export
#'
list_ones <- function(x)
{
  if (!is_numeric_vector(x))
    stop("\n'list_ones()' requires a numeric vector")
  # output
  lapply(x, function(u) rep(1, u))
}
#' @title Create a dummy matrix from the elements in a list
#' 
#' @details Internal function. 
#'
#' @param alist a list of vectors
#' @return A matrix of dummy variables
#' @keywords internal
#' @export
#'
list_to_dummy <- function(alist)
{
  if (!list_of_vectors(alist))
    stop("\n'list_to_dummy()' requires a list of vectors")
  
  aux = lengths(alist)
  to = cumsum(aux)
  from = to - aux + 1
  dummy_matrix = matrix(0, sum(aux), length(aux))
  for (j in seq_along(aux))
    dummy_matrix[from[j]:to[j], j] = 1
  dummy_matrix
}
#' @title Design-type matrix from the elements in a list
#' 
#' @details Internal function. 
#'
#' @param alist a list of numeric vectors
#' @return A design-type matrix
#' @keywords internal
#' @export
#'
list_to_matrix <- function(alist)
{
  if (!list_of_numeric_vectors(alist))
    stop("\n'list_to_matrix()' requires a list of numeric vectors")
  
  aux = lengths(alist)
  to = cumsum(aux)
  from = to - aux + 1
  linked_matrix = matrix(0, sum(aux), length(aux))
  for (j in seq_along(aux))
    linked_matrix[from[j]:to[j], j] = alist[[j]]
  linked_matrix
}
#' @title Create a list from a vector of integers
#' 
#' @details Internal function. 
#'
#' @param indices a vector of integers indicating the length of each vector
#' in the produced list
#' @return A list of index vectors
#' @keywords internal
#' @export
#'
listify <- function(indices)
{
  if (!all(is_positive_integer(indices)))
    stop("\n'listify()' requires a vector of positive integers")
  
  mapply(rep, seq_along(indices), indices, SIMPLIFY=FALSE) 
}
#' @title Size: total number of elements in a list
#' 
#' @details Internal function. 
#'
#' @param alist a list
#' @return number of elements in \code{alist}.
#' @keywords internal
#' @export
#'
listsize <- sizelist <- function(alist)
{
  if (!is.list(alist))
    stop("\n'listsize()' requires a list")
  
  length(unlist(alist))
}
#' @title Split a matrix into blocks
#' 
#' @details Internal function. 
#'
#' @param Matrix a matrix to split
#' @param blocks either a list or a vector indicating the blocks. 
#' If \code{blocks} is a list of vectors, then the length of each vector 
#' defines the size of the blocks. If \code{blocks} is a vector, then each 
#' element represents the size of the blocks.
#' @param byrow logical. If \code{TRUE} (the default) the matrix is split 
#' by rows, otherwise the matrix is split by columns
#' @return A list of matrices
#' @keywords internal
#' @export
#'
matrix_to_blocks <- function(Matrix, blocks, byrow = TRUE)
{
  if (is_not_matrix(Matrix))
    stop("\n'matrix_to_blocks()' requires a matrix")
  if (!is.list(blocks) && !is.numeric(blocks))
    stop("\n'matrix_to_blocks()' requires a list (or vector)")
  
  num_blocks = length(blocks)
  if (is.list(blocks)) {
    size = listsize(blocks)
    start_end = from_to(blocks)
    from = start_end$from
    to = start_end$to    
  }
  if (is.numeric(blocks)) {
    size = sum(blocks)
    to = cumsum(blocks)
    from = to - blocks + 1
  }
  
  # empty list to store results
  blocks_list = vector(mode="list", length=num_blocks)
  
  for (k in 1:num_blocks) {
    if (byrow) {
      if (size != nrow(Matrix))
        stop("\nNumber of rows in 'Matrix' doesn't match 'blocks'")
      blocks_list[[k]] = Matrix[from[k]:to[k],]
    } else {
      if (size != ncol(Matrix))
        stop("\nNumber of columns in 'Matrix' doesn't match 'blocks'")
      blocks_list[[k]] = Matrix[,from[k]:to[k]]      
    }
  }
  # output
  blocks_list
}
#' @title Create a dummy matrix from the elements in a vector
#' 
#' @details Internal function. 
#'
#' @param avector a numeric vector
#' @return A matrix of dummy variables
#' @keywords internal
#' @export
#'
vector_to_dummy <- function(avector)
{
  if (!is_numeric_vector(avector))
    stop("\n'vector_to_dummy()' requires a numeric vector")
  
  num_rows = sum(avector)
  num_cols = length(avector)
  
  # starting-and-ending positions
  start_end = from_to(avector)
  from = start_end$from
  to = start_end$to
  
  # build dummy matrix
  dummy_matrix = matrix(0, num_rows, num_cols)
  for (k in 1:num_cols) {
    dummy_matrix[from[k]:to[k],k] = 1
  }
  dummy_matrix
}