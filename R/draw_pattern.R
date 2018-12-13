#' Plotting colored multiplication table
#'
#' Visualize the multiplication table coloring factors of k.
#'
#' @param k Values for which to find factors
#' @param xmax Max value of x to show in the plot
#' @param ymax Max value of y to show in the plot
#' @param colors Colors to be assigned to factors of k. Defaults to a random palette from wesanderson package
#' @param show_title Show title in the plot?
#' @param show_numbers Show corresponding numbers in the cells?
#' @param show_legend Show legend identifying the color mapping
#'
#' @importFrom numbers mLCM
#' @importFrom ggplot2 ggplot aes geom_tile scale_y_continuous scale_fill_manual
#' @importFrom ggplot2 coord_fixed theme_void ggtitle guides

#' @export
draw_pattern <- function(k, xmax, ymax,
                         colors, show_title = TRUE,
                         show_numbers = FALSE,
                         show_legend = FALSE) {

    if (missing(xmax)) {
        xmax <- numbers::mLCM(k) * 4
        # If xmax/ymax is very large it will take a lot of time
        # and the plot will be ugly
        # Set maximal range to 200 cells long
        xmax <- ifelse(xmax < 200, xmax, 200)
    }
    if (missing(ymax)) {
        ymax <- numbers::mLCM(k) * 4
        ymax <- ifelse(ymax < 200, ymax, xmax)
    }
    # Grid of values with corresponding products and identified factors
    table <- create_table(k, xmax, ymax)

    # Default colors: random color palette from wesanderson package
    if (missing(colors)) {
        n_colors <- length(levels(table$combination))
        palette_names <- names(wesanderson::wes_palettes)
        random_palette_name <- sample(palette_names, 1)
        chosen_colors <- wesanderson::wes_palette(random_palette_name,
                                                  n_colors, "continuous")
    } else {
        chosen_colors <- colors
        n_combinations <- length(levels(table$combination))
        if (length(chosen_colors) != n_combinations) {
            cat("Warning: There were", n_combinations,
                "observed combinations, but",
                length(chosen_colors), "colors introduced.")
        }
    }

    plt <- ggplot(table, aes(x, y)) +
        geom_tile(aes(fill = combination)) +
        scale_y_continuous(trans = "reverse") +
        scale_fill_manual(values = chosen_colors) +
        coord_fixed() +
        theme_void()

    # Show title option
    if (show_title == TRUE) {
        if (missing(colors)) {
            k_as_string <- paste(sort(k), collapse = ", ")
            plt_title <- paste0("Multiples of ", k_as_string,
                                " (", random_palette_name, ")")
        } else {
            k_as_string <- paste(sort(k), collapse = ", ")
            plt_title <- paste0("Multiples of ", k_as_string)
        }
        plt <- plt + ggtitle(plt_title)
    }

    # Show legend option
    if (show_legend == FALSE) {
        plt <- plt + guides(fill = FALSE)
    }
    plt
}
