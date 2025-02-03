# Load the volcanoPlus package
library(volcanoPlus)
library(ghibli)
library(ggplot2)

# Download the data we will use for plotting
download.file("https://raw.githubusercontent.com/biocorecrg/CRG_RIntroduction/master/de_df_for_volcano.rds",
              "de_df_for_volcano.rds", method="curl")

# The RDS format is used to save a single R object to a file, and to restore it.
# Extract that object in the current session:
tmp <- readRDS("de_df_for_volcano.rds")

# Remove rows that contain NA values
de <- tmp[complete.cases(tmp), ]

# Example Data (Replace this with actual data)
# For this example, we assume a data frame with `logFC`, `adj.P.Val`, and `Gene` columns.
data <- data.frame(
  logFC = de$log2FoldChange,
  adj.P.Val = de$pvalue,
  Gene = de$gene_symbol
)

# Set significance thresholds for the volcano plot
significance_thresholds <- list(horizontal_asymptote = 2, vertical_asymptote = 0.2)

# Optional: Set parameters for labeling specific points
labeling_criteria <- list(horizontal_asymptote = 2, vertical_asymptote = 0.2)

# Create the volcano plot
volcano_plot <- plot_volcano_plus(data,
                                  title = "",
                                  significance_thresholds = significance_thresholds,
                                  labeling_criteria = labeling_criteria)

# Show the volcano plot
print(volcano_plot  +
        geom_vline(xintercept = 0, linetype = 'dotted', color = 'darkred') +  # Vertical line at x = 0
        xlab("Log fold change") + ylab("Log10(Adjusted P value)") +
        theme_minimal() +  # Minimal theme (no extra packages)
        theme(legend.position = "none"))
