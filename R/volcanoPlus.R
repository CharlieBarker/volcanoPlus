#' Create a custom volcano plot with asymptotic thresholds and optional labeling
#'
#' `plot_volcano_plus` generates a volcano plot using `ggplot2`, applying custom
#' asymptotic thresholds for identifying significant points, and optionally labeling
#' specific points based on additional criteria.
#'
#' @param to_plot A data frame containing at least the following columns:
#'   `logFC` (log fold change) and `adj.P.Val` (adjusted p-value).
#'   Optionally, a `Gene` column can be included for labeling specific genes.
#' @param title The title of the volcano plot.
#' @param significance_thresholds A named list of parameters for defining the significance threshold function.
#'   This list should contain:
#'   - `horizontal_asymptote`: Controls the horizontal threshold for significance (default is `2`).
#'   - `vertical_asymptote`: Controls the vertical threshold for significance (default is `0.5`).
#' @param labeling_criteria An optional named list of parameters for labeling specific points.
#'   This list should contain:
#'   - `horizontal_asymptote`: Controls the horizontal threshold for labeling (default is `1.5`).
#'   - `vertical_asymptote`: Controls the vertical threshold for labeling (default is `0.4`).
#'   If not provided, no points will be labeled.
#' @return A `ggplot` object representing the volcano plot.
#' @details The function identifies significant points by applying a mirrored
#'   asymptotic function to the log fold change (`logFC`) values. Points that fall below
#'   this threshold are considered insignificant and are plotted with reduced transparency.
#'   Optionally, specific points can be labeled based on additional asymptotic criteria.
#' @import ggplot2
#' @examples
#' # Example usage:
#' # Assuming `data` is a data frame with `logFC`, `adj.P.Val`, and `Gene` columns.
#' significance_thresholds <- list("horizontal_asymptote" = 2, "vertical_asymptote" = 0.5)
#' labeling_criteria <- list("horizontal_asymptote" = 1.5, "vertical_asymptote" = 0.4)
#' plot_volcano_plus(data, "Volcano Plot Example", significance_thresholds, labeling_criteria)
#' @export
plot_volcano_plus <- function(to_plot, title,
                              significance_thresholds = list("horizontal_asymptote" = 2,   # Horizontal threshold for significance.
                                                             "vertical_asymptote" = 0.5), # Vertical threshold for significance.
                              labeling_criteria = NULL         # Parameters for labeling points.
) {

  # Internal function to define the mirrored asymptotic function
  mirrored_asymptotic_function <- function(x,
                                           horizontal_asymptote = significance_thresholds$horizontal_asymptote,
                                           vertical_asymptote = significance_thresholds$vertical_asymptote) {
    y <- horizontal_asymptote / (abs(x) - vertical_asymptote)
    return(y)
  }

  # Identify points above and below the mirrored function
  threshold_values <- mirrored_asymptotic_function(to_plot$logFC)
  to_plot$below <- -log10(to_plot$adj.P.Val) < threshold_values
  to_plot$below[abs(to_plot$logFC) < significance_thresholds$vertical_asymptote] <- TRUE

  # Define alpha values based on whether points are below the threshold
  alpha_values <- ifelse(to_plot$below, 0.1, 0.5)

  # Initialize the label column
  to_plot$label <- ""

  # If labeling criteria are provided, add labels
  if (!is.null(labeling_criteria)) {
    labeling_threshold_values <- mirrored_asymptotic_function(to_plot$logFC,
                                                              horizontal_asymptote = labeling_criteria$horizontal_asymptote,
                                                              vertical_asymptote = labeling_criteria$vertical_asymptote)
    to_plot$to_label <- -log10(to_plot$adj.P.Val) < labeling_threshold_values
    to_plot$below[abs(to_plot$logFC) < labeling_criteria$vertical_asymptote] <- TRUE
    to_plot$label[!to_plot$below] <- to_plot$Gene[!to_plot$below]
  }

  # Basic volcano plot using ggplot2
  volcano_plot <- ggplot(to_plot, aes(x = logFC, y = -log10(adj.P.Val), color = below, label = label)) +
    geom_point(alpha = alpha_values) +  # Add points with transparency based on significance
    geom_function(fun = mirrored_asymptotic_function, alpha = 0.5) +
    ylim(0, max(-log10(to_plot$adj.P.Val)))

  # Optionally add labels if labeling_criteria is provided
  if (!is.null(labeling_criteria)) {
    volcano_plot <- volcano_plot + geom_text(aes(label = label), size = 3, hjust = 1.5, vjust = 1.5)
  }

  return(volcano_plot)
}
