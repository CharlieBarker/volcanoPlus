<p align="center">
  <img src="man/images/logo.png" width="50%" />
</p>

# volcanoPlus

`plot_volcano_plus` is an R function that generates customizable volcano plots for visualizing differential expression data. This function offers enhanced functionality, including asymptotic thresholds for determining significance, and the ability to label specific points based on additional criteria. It is designed for use in genomics and other fields where visualizing the relationship between fold change and statistical significance is important.

## Features

- **Custom Asymptotic Thresholding:** Define mirrored asymptotic functions to identify significant points based on log fold change and adjusted p-values.
- **Flexible Point Labeling:** Highlight and label specific points (e.g., genes) based on user-defined criteria.
- **Enhanced Visualization:** Control the appearance of significant and non-significant points using alpha transparency, custom colors, and asymptotic curves.

## Installation

To install the development version from GitHub, use the following command:

```r
# Install the devtools package if you haven't already
install.packages("devtools")

# Install volcanoPlus from GitHub
devtools::install_github("CharlieBarker/volcanoPlus")
