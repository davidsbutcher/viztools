viztools
================

This package provides tools for visualization of results from top-down
proteomics studies of prefractionated biological samples and is based on
novel visualizations developed for evaluation of the [PEPPI-MS
prefractionation method](https://doi.org/10.1021/acs.jproteome.0c00303).
Also suitable for visualization of samples fractionated using
[GELFrEE](https://doi.org/10.1021/ac702197w) or comparison of biological
or technical replicates. The package is based around the following
visualizations:

### UpSet plots

A novel method for visualization of intersecting sets developed by [Lex,
Gehlenborg, et al.](https://doi.org/10.1109/TVCG.2014.2346248) and
implemented using the excellent [UpSetR
package](https://doi.org/10.1093/bioinformatics/btx364). Provides
improved readability in comparison to Euler and Venn diagrams,
especially for visualization of large numbers of sets. The [PEPPI-MS
paper](https://doi.org/10.1021/acs.jproteome.0c00303) introduced the use
of UpSet plots to show the occurrences and intersections of proteoform
identifications across multiple molecular weight-based fractions.

### Intersection Degree plots

Useful for showing the intersection degrees of proteoform
identifications, i.e. the percentage of identifications occurring in one
fraction, two fractions, etc.

### Molecular weight heatmaps

Used to visualize the distribution of proteoform identifications by
molecular weight. Can be made in a vertical orientation for comparison
to SDS-PAGE gels:

<img src="C:/Users/ranar/Documents/R/win-library/3.6/viztools/extdata/heatmap_example.jpg" width="1717" />

### Waffle plots

Used for visualizing quantity and subcellular localization of proteoform
identifications by fraction.

<img src="C:/Users/ranar/Documents/R/win-library/3.6/viztools/extdata/waffle_example.png" width="5577" />

## Installation

Install from GitHub:

``` r
remotes::install_github("davidsbutcher/viztools")
```

## Usage

### Formatting input spreadsheet files

Input files for `make_UpSet_plot()` and
`make_intersection_degree_plot()` should have column names corresponding
to fraction/replicate designations and row values corresponding to
unique protein/proteoform identifiers,
e.g. [UniProt](https://www.uniprot.org/) accession numbers or
[CTDP](https://www.topdownproteomics.org/) proteoform record numbers.

An input file for `make_heatmap()` should have a column providing
molecular weights and a column providing the fraction/replicate number.
Default column names are “mass” and “fraction” but can be specified in
the function arguments.

An input file for `waffle_iron()` should have a column providing the
fraction/replicate number and columns providing subcellular localization
counts. Column names other than “fraction” are used for legend labels,
so I recommend naming them “Cytosol”, “Membrane”, etc.

Example input files for each visualization type can be found in the
`extdata` folder in the package directory.

### Loading and visualizing data

Load an input spreadsheet file as an R object using an appropriate
function, e.g. `readxl::read_xlsx()` for XLSX files or
`readr::read_csv()` for CSV files. Then, pass the object to the
appropriate visualization function:

``` r
# Read an XLSX

df <- 
   readxl::read_xlsx(
      "C:\Users\YourName\Documents\protein_data.xlsx"
   )

# Read a CSV

df <- 
   readr::read_csv(
      "C:\Users\YourName\Documents\protein_data.csv"
   )

# Use data frame as argument for a visualization function

make_UpSet_plot(df)
```

### Saving plots

Plots created using `viztools` can be saved by setting the argument
`savePDF = TRUE`:

``` r
make_UpSet_plot(df, savePDF = TRUE)
```

With the exception of UpSet plots, they can also be saved using the
`ggplot2::ggsave()` function:

``` r
make_heatmap(df)

ggplot2::ggsave(
   "heatmap.png",
   dpi = 300,
   height = 5,
   width = 8
)
```

## Dependencies

`viztools` utilizes the package
[`UpSetR`](http://github.com/hms-dbmi/UpSetR) for generating UpSet plots
and [`waffle`](https://github.com/hrbrmstr/waffle/tree/cran) for
generating Waffle plots. Other visualizations are generated using
`ggplot2`. Additional functions are imported from `dplyr`, `tibble`,
`purrr`, `glue`, `tidyr`, `magrittr`, `assertthat`, and `scales`.

## License and attribution

Package developed by David S. Butcher and licensed under [CC
BY 4.0](https://creativecommons.org/licenses/by/4.0/). Imported packages
are licensed separately.
