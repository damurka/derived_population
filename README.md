# derived_population Shiny App

A Shiny app built with R.\
The project uses `renv` for a reproducible environment with pinned package versions.

## Prerequisites

-   R installed

Install `renv` once

``` r
install.packages("renv")
```

## Setup and Installation

### 1) Get the project

Option A. Clone with Git

``` bash
git clone https://github.com/damurka/derived_population.git
cd derived_population
```

Option B. Download ZIP

-   <https://github.com/damurka/derived_population/archive/refs/heads/master.zip>\
-   Unzip and open the `derived_population` folder

### 2) Restore the environment

Run in the project directory

``` r
renv::restore()
```

This installs the exact package versions recorded in `renv.lock`.

## Run the app

Open the project in RStudio by clicking `derived_population.Rproj`, or start R in the project folder, then run

``` r
shiny::runApp()
```

The app opens in your browser or the RStudio Viewer.
