# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'
#   Shortcut Reference:        'Alt + Shift + K'
#   Re-load the code:          'Ctrl + Shift + L'
#   Jump to a function:        'F2 / Ctrl + .'
#   Convert roxygen comments   'Ctrl + Shift + D' devtools::document()
#   Reflow comments            'Ctrl + Shift + /'
#   Unit Test                  'Ctrl + Shift + T' devtools::test()
hello <- function() {
  print("Hello, world!")
}


#' Add together two numbers.
#'
#' @param x A number.
#' @param y A number.
#' @return The sum of \code{x} and \code{y}.
#' @examples
#' add(1, 1)
#' add(10, 1)
add <- function(x, y) {
  x + y
}
