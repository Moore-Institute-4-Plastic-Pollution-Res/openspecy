safe_library <- function(pkg) {
  suppressPackageStartupMessages(
    tryCatch(library(pkg, character.only = TRUE),
             error = function(e)
               warning(sprintf("Package '%s' not available.", pkg)))
  )
}

pkgs <- c(
  "shiny", "shinyWidgets", "bslib", "caTools", "data.table",
  "glmnet", "hyperSpec", "mmand", "plotly", "signal",
  "bs4Dash", "digest", "shinyjs", "dplyr", "shinyBS",
  "jsonlite", "OpenSpecy", "DT"
)

vapply(pkgs, safe_library, logical(1))

lapply(list.files("R", full.names = TRUE), source)

fetch_lib <- function(name) {
  path <- file.path("data", paste0(name, ".rds"))
  if (!file.exists(path)) {
    try(get_lib(name, mode = "w", path = "data/", aws = TRUE),
        silent = TRUE)
  }
  if (file.exists(path)) read_any(path) else NULL
}


load_data <- function() {
  data("raman_hdpe")
  testdata <- data.table(wavenumber = raman_hdpe$wavenumber,
                         intensity = raman_hdpe$spectra$intensity)
  
  # Inject variables into the parent environment
  invisible(list2env(as.list(environment()), parent.frame()))
}


# Workaround for Chromium Issue 468227
downloadButton <- function(...) {
  tag <- shiny::downloadButton(...)
  tag$attribs$download <- NULL
  tag
}

# Define the custom theme
theme_black_minimal <- function(base_size = 11, base_family = "") {
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      plot.background = element_rect(fill = "black", color = NA),
      panel.background = element_rect(fill = "black", color = NA),
      panel.grid.major = element_line(color = "white"),
      panel.grid.minor = element_line(color = "white"),
      axis.line = element_line(color = "white"),
      axis.ticks = element_line(color = "white"),
      axis.text = element_text(color = "white"),
      axis.title = element_text(color = "white"),
      plot.title = element_text(color = "white", hjust = 0.5),
      plot.subtitle = element_text(color = "white", hjust = 0.5),
      plot.caption = element_text(color = "white"),
      legend.text = element_text(color = "white"),
      legend.title = element_text(color = "white"),
      legend.background = element_rect(fill = "black"),
      legend.key = element_rect(fill = "black"),
      strip.background = element_rect(fill = "black", color = NA),
      strip.text = element_text(color = "white")
    )
}

# # Name keys for human readable column names ----
citation <- 
  HTML("Cowger W, Steinmetz Z, Gray A, Munno K, Lynch J, Hapich H, Primpke S, De
  Frond H, Rochman C, Herodotou O (2021). “Microplastic Spectral
  Classification Needs an Open Source Community: Open Specy to the Rescue!”
  <i>Analytical Chemistry</i>, <b>93</b>(21), 7543–7548. doi:
  <a href='https://doi.org/10.1021/acs.analchem.1c00123'>10.1021/acs.analchem.1c00123</a>.")
