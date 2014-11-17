#' Append a position to a list
#' @description adds a a new list position with object without nesting.
#' @param lst list
#' @param obj object to be appended
#' @export 

##Append a position to a list
lappend <- function(lst, obj) {
  lst[[length(lst)+1]] <- obj
  return(lst)
}