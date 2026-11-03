# Setup for the rapid workflow: install and attach packages, then source src/.
# Run from the repo root. The Rmds read data/... paths, so the repo root needs a
# `data` link to the shared data folder (see README.md).

packageLoad <-
  function(x) {
    for (i in 1:length(x)) {
      if (!x[i] %in% installed.packages()) {
        install.packages(x[i])
      }
      library(x[i], character.only = TRUE)
    }
  }

# Packages the Rmds and notebooks use without `pkg::`. tidyverse goes last, so
# dplyr's select(), filter() and so on win over the other packages' versions.
packages <- c(
  "arrow",
  "cowplot",
  "curl",
  "data.table",
  "elevatr",
  "exactextractr",
  "furrr",
  "geosphere",
  "ggpubr",
  "ggrepel",
  "glue",
  "gridExtra",
  "here",
  "kableExtra",
  "knitr",
  "leaflet",
  "mapview",
  "ncdf4",
  "nngeo",
  "patchwork",
  "plotly",
  "progress",
  "readxl",
  "scales",
  "sf",
  "SPEI",
  "terra",
  "tigris",
  "trend",
  "tidyverse"
)

packageLoad(packages)

# Only called as pkg::fun(), so installed but not attached.
install_only <- c(
  "broom",
  "DT",
  "httr",
  "igraph",
  "jsonlite",
  "leafpop",
  "leafsync",
  "maptiles",
  "remotes",
  "viridisLite"
)
missing_packages <- setdiff(install_only, rownames(installed.packages()))
if (length(missing_packages) > 0) install.packages(missing_packages)

# The shared core: get_wsd(), get_park_system(), get_unit_visitation(),
# get_park_boundary() and wava_data_path().
if (!"npswavacore" %in% rownames(installed.packages())) {
  remotes::install_github("Water-Vulnerability-Analytics/npswavacore")
}
library(npswavacore)

# Source all functions
purrr::walk(
  list.files(path = "src", pattern = "\\.R$", full.names = TRUE),
  source
)
