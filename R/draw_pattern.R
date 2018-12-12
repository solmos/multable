#' @importFrom numbers mLCM
#' @import ggplot2

#' @export
draw_pattern <- function(k, xmax, ymax,
                         colors, show_title = TRUE,
                         show_numbers = FALSE,
                         show_legend = FALSE) {

    if (missing(xmax)) {
        xmax <- numbers::mLCM(k) * 4
        # If xmax/ymax is very large it will take a lot of time
        # and the plot will be ugly
        xmax <- ifelse(xmax < 200, xmax, 200)
    }
    if (missing(ymax)) {
        ymax <- numbers::mLCM(k) * 4
        ymax <- ifelse(ymax < 200, ymax, xmax)
    }
    # Grid of values with corresponding products
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
            cat("There were", n_combinations, "observed combinations, but",
                length(chosen_colors), "colors introduced.")
        }
    }

    plt <- ggplot2::ggplot(table, aes(x, y)) +
        ggplot2::geom_tile(aes(fill = combination)) +
        ggplot2::scale_y_continuous(trans = "reverse") +
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
