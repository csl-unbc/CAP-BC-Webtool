
<!--- README.md is generated from README.Rmd. Please edit that file -->

## locationmisc: Classes, widgets, and functions for the Location App

[![lifecycle](https://img.shields.io/badge/Lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/experimental)

### Overview

This package provides classes, widgets, and functions for the [Location
App](https://github.com/NCC-CNC/location-app).

### Installation

The package is only available from an [online coding
repository](https://github.com/NCC-CNC/locationmisc). To install it, use
the following R code.

``` r
if (!require(remotes))
  install.packages("remotes")
remotes::install_github("NCC-CNC/locationmisc")
```

### Usage

This package provides the following widgets:

-   `sidebar()`: Constructs a widget that forms a sidebar on a `leaflet`
    map. It contains the following widgets for customizing its
    appearance:
    -   `sidebarTab()` Constructs a widget that forms a tab in a
        sidebar. This is used to group together widgets (e.g. those
        specified below and standard `shiny` widgets) that have related
        functionality (e.g. a side bar tab could contain widgets for
        controlling the appearance of a `leaflet` map).
-   `newSolutionManager()`: Constructs a widget for creating new
    solutions. It forms a panel that contains – in addition to standard
    `shiny` widgets (e.g. a button to generate a new solution) – the
    following widgets for setting goals and weights:
-   `mapManager()`: Constructs a widget for controlling the overall
    appearance of a `leaflet` map. It forms a panel that contains – in
    addition to standard `shiny` widgets (e.g. a button to hide all
    solutions) – the widgets for controlling the appearnace of weights,
    themes, and solutions.
