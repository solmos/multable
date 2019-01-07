#' Generating the values of the multiplication table
#'
#' Creates a data frame with the values of the multiplication table and identifies the factors for given k.
#'
#' @param k Numeric vector specifying the values of k
#' @param xmax Max value of x to be generated
#' @param ymax Max value of y to be generated
#' @param show_combinations Whether to show or not the number of combinations of k
#'
#' @return Data frame
#'
#' @examples
#' create_table(2, 10, 10)
#'
#' @export

create_table <- function(k, xmax, ymax, show_combinations = FALSE) {
    # Create the grid of values and their product
    values <- expand.grid(x = 0:xmax,
                          y = 0:ymax)
    values$product <- values$x * values$y

    # Whether a value x is a multiple of k
    find_multiples <- function(x, k) x %% k == 0

    # Apply find_multiples() for all values of vector k
    multiples <- sapply(k, find_multiples, x = values$product)
    multiples <- as.data.frame(multiples)
    col_names <- paste0("m", as.character(k))
    colnames(multiples) <- col_names

    # Get a factor specifying for which values of k
    # each product is a multiple of
    combination <- interaction(multiples)

    table <- data.frame(values, multiples, combination)
    if (show_combinations == TRUE) {
        cat("There were", length(levels(table$combination)),
            "observed combinations.")
    }
    table
}
