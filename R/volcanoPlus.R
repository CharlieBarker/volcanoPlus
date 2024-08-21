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
#' @param significant_parameters A named list of parameters for defining the asymptotic
#'   significance threshold function. This list should contain:
#'   - `a`: Controls the horizontal asymptote (default is `2`).
#'   - `b`: Controls the vertical asymptote (default is `0.5`).
#' @param labelling_parameters An optional named list of parameters for labeling specific points.
#'   This list should contain:
#'   - `a`: Controls the horizontal asymptote for labeling.
#'   - `b`: Controls the vertical asymptote for labeling.
#'   If not provided, no points will be labeled.
#' @return A `ggplot` object representing the volcano plot.
#' @details The function identifies significant points by applying a mirrored
#'   asymptotic function to the log fold change (`logFC`) values. Points that fall below
#'   this threshold are considered insignificant and are plotted with reduced transparency.
#'   Optionally, specific points can be labeled based on additional asymptotic criteria.
#' @import ggplot2
#' @import cowplot
#' @import ggrepel
#' @examples
#' # Example usage:
#' # Assuming `data` is a data frame with `logFC`, `adj.P.Val`, and `Gene` columns.
#' significant_params <- list("a" = 2, "b" = 0.5)
#' labelling_params <- list("a" = 1.5, "b" = 0.4)
#' plot_volcano_plus(data, "Volcano Plot Example", significant_params, labelling_params)
#' @export
plot_volcano_plus <- function(to_plot, title,
                              significant_parameters = list("a"=2,   # Horizontal asymptote.
                                                            "b"=.5), # Vertical asymptote.
                              labelling_parameters = NULL         # Separate parameters for labeling.
) {

  # Define the parameters
  c <- 0   # Y-intercept

  # Internal function to define the mirrored function
  mirrored_asymptotic_function <- function(x,
                                           a=significant_parameters$a,
                                           b=significant_parameters$b
  ) {
    c=0
    y <- a / (abs(x) - b) + c
    return(y)
  }

  # Identify points above and below the mirrored function
  which_significant <- mirrored_asymptotic_function(to_plot$logFC)
  to_plot$below <- -log10(to_plot$adj.P.Val) < which_significant
  to_plot$below[abs(to_plot$logFC) < significant_parameters$b] <- TRUE

  # Define alpha values based on 'below'
  alpha_values <- ifelse(to_plot$below, 0.1, 0.5)

  to_plot$label <- ""

  # If labeling parameters are supplied, add labels
  if (!is.null(labelling_parameters)) {
    which_labelled <- mirrored_asymptotic_function(to_plot$logFC, a=labelling_parameters$a, b=labelling_parameters$b)
    to_plot$to_label <- -log10(to_plot$adj.P.Val) < which_labelled
    to_plot$below[abs(to_plot$logFC) < labelling_parameters$b] <- TRUE
    to_plot$label[!to_plot$below] <- to_plot$Gene[!to_plot$below]
  }

  # Create the volcano plot
  volcano_plot <- ggplot(to_plot,
                         aes(logFC, -log10(adj.P.Val), color = below, label = label)) +
    geom_point(alpha = alpha_values) +
    cowplot::theme_cowplot() +
    geom_text_repel(min.segment.length = 0, seed = 42, box.padding = 0.5,
                    color = "black",
                    size=2) +
    geom_vline(xintercept = 0, linetype = 'dotted', col = 'darkred') +
    theme(legend.position = "none") +
    ylab("Log10(Adjusted P value)") + xlab("Log fold change") +
    geom_function(fun = mirrored_asymptotic_function,
                  colour = ghibli_palettes$YesterdayDark[4], alpha = 0.5) +
    scale_color_manual(values = c("TRUE" = "lightgrey", "FALSE" = "darkblue")) +
    ylim(0, max(-log10(to_plot$adj.P.Val))) +
    ggtitle(title) +
    theme(plot.title = element_text(size = 12, face = "bold"))

  return(volcano_plot)
}
