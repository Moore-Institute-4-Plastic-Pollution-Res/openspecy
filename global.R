# Check for Auth Tokens and setup, you can change these to test the triggering
# of functions without removing the files.
#translate <- file.exists("www/googletranslate.html")

#remotes::install_github("wincowgerDEV/OpenSpecy-package@vignettes")

# global.R (top)
is_shinylive <- isTRUE(getOption("shinylive.enabled", FALSE))

# real loads — all webR-safe
library(shiny); library(bs4Dash); library(bslib)
library(shinyWidgets); library(shinyjs); library(DT); library(plotly); library(jsonlite)
library(magrittr); library(dplyr); library(scales); library(reshape2); library(digest)

# force Shinylive to bundle these (even if you don't call ggplotly yet)
# if (FALSE) {
#     library(ggplot2)
#     library(munsell)   # ggplot2 depends on it; explicit keeps the bundler honest
# }
# 
# Load supporting packages
if (!is_shinylive) {
     library(glmnet); library(hyperSpec); library(mmand); library(signal)
     library(caTools); library(data.table); library(OpenSpecy)
     library(ggplot2); library(munsell)
} else {
     pkgs <- c("glmnet", "hyperSpec", "mmand", "signal", "caTools",
               "data.table", "OpenSpecy", "ggplot2", "munsell")
     lapply(pkgs, function(pkg) try(library(pkg), silent = TRUE))
     lapply(list.files("R", pattern = "\\.R$", full.names = TRUE), source)
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

# Load all data ----
load_data <- function() {
    raman_hdpe <- readRDS("data/raman_hdpe.rds")

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

# Name keys for human readable column names ----

version <- paste0("Open Specy v 1.5")#, packageVersion("OpenSpecy"))
citation <- HTML(
    'Cowger, W., Karapetrova, A., Lincoln, C., Chamas, A., Sherrod, H., Leong, N., Lasdin, K. S., 
  Knauss, C., Teofilović, V., Arienzo, M. M., Steinmetz, Z., Primpke, S., 
  Darjany, L., Murphy-Hagan, C., Moore, S., Moore, C., Lattin, G., 
  Gray, A., Kozloski, R., Bryksa, J., Maurer, B. (2025). 
  "Open Specy 1.0: Automated (Hyper)spectroscopy for Microplastics." 
  <i>Analytical Chemistry.</i> doi:
  <a href="https://doi.org/10.1021/acs.analchem.5c00962">10.1021/acs.analchem.5c00962</a>.'
)
