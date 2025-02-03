<p align="center">
  <img src="man/images/logo.png" width="50%" />
</p>

VolcanoPlus Package: A Custom Volcano Plot Generator
====================================================

Introduction
------------

The `volcanoPlus` package allows you to create volcano plots with custom asymptotic thresholds and optional labeling of specific points. This tutorial will guide you through generating a volcano plot from a dataset, setting significance thresholds, and visualizing the result.

## Features

- **Custom Asymptotic Thresholding:** Define mirrored asymptotic functions to identify significant points based on log fold change and adjusted p-values.
- **Flexible Point Labeling:** Highlight and label specific points (e.g., genes) based on user-defined criteria.
- **Enhanced Visualization:** Control the appearance of significant and non-significant points using alpha transparency, custom colors, and asymptotic curves.


Step 1: Install and Load the Package
------------------------------------

Before you start, make sure you have the `volcanoPlus` package installed. You can install it from GitHub or your local directory:

```r
# Install the devtools package if you haven't already
install.packages("devtools")

# Install volcanoPlus from GitHub
devtools::install_github("CharlieBarker/volcanoPlus")


## Custom Pathway Layouts

Organize nodes in a biologically meaningful way, based on their roles (e.g., receptors, transcription factors).

Load packages: 
```r
# Load the volcanoPlus package
library(volcanoPlus)
library(ghibli)
library(ggplot2)

```

Step 6: Saving the Plot to PNG
------------------------------

If you want to save the plot to a PNG file, use the following code:

```r
ggsave("volcano_plot.png", plot = volcano_plot, width = 8, height = 6, dpi = 300)
```
The volcano plot is now saved as `volcano_plot.png` in your working directory.

Example Output
--------------

Here's an example of the resulting volcano plot:

output :
<p align="center">
  <img src="man/images/logo.png" width="50%" />
</p>
This plot shows the log fold change on the x-axis and the -log10 adjusted p-value on the y-axis. Points that fall below the asymptotic threshold are considered insignificant and are plotted with reduced transparency.
---

Notes:
------

- The threshold parameters (`horizontal_asymptote` and `vertical_asymptote`) can be adjusted according to your dataset to highlight the most relevant points.
- The `plot_volcano_plus` function allows you to customize the plot, including the color of significant points and the inclusion of gene labels.
