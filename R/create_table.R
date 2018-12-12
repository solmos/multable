
#' @export
create_table <- function(k, xmax, ymax) {
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
    cat("There were", length(levels(table$combination)), "observed combinations.")
    table
}