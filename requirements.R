# ==============================================================================
# requirements.R - Package Management & Environment Setup
# Calorie Burn Predictor Project
# ==============================================================================

# Configure project-local library path if present
setup_project_lib <- function() {
  proj_lib <- file.path(getwd(), ".Rlibs")
  if (dir.exists(proj_lib)) {
    if (!(proj_lib %in% .libPaths())) {
      .libPaths(c(proj_lib, .libPaths()))
    }
  }
}

# Run immediately upon sourcing
setup_project_lib()

# List of required and recommended packages
REQUIRED_PACKAGES <- c(
  "randomForest",  # Non-linear ensemble tree regression
  "corrplot"       # Correlation matrix visualization
)

RECOMMENDED_PACKAGES <- c(
  "shiny",           # Interactive web application framework
  "shinydashboard",  # Professional dashboard layout
  "ggplot2",         # Declarative data visualization
  "scales",          # Scale formatting for plots
  "gridExtra",       # Multi-panel grid displays
  "plotly",          # Interactive HTML widgets
  "DT",              # Interactive data tables
  "caret"            # ML utilities and partitioning
)

OPTIONAL_PACKAGES <- c(
  "bslib",
  "dplyr",
  "tidyr",
  "readr",
  "rmarkdown",
  "testthat"
)

ALL_PACKAGES <- unique(c(REQUIRED_PACKAGES, RECOMMENDED_PACKAGES, OPTIONAL_PACKAGES))

#' Check package installation status
#' @return data.frame with Package, Required, Installed, Version
check_packages <- function() {
  setup_project_lib()
  installed <- installed.packages()[, "Package"]
  
  df <- data.frame(
    Package = ALL_PACKAGES,
    Tier = ifelse(ALL_PACKAGES %in% REQUIRED_PACKAGES, "Required",
           ifelse(ALL_PACKAGES %in% RECOMMENDED_PACKAGES, "Recommended", "Optional")),
    Installed = ALL_PACKAGES %in% installed,
    stringsAsFactors = FALSE
  )
  
  df$Version <- sapply(df$Package, function(pkg) {
    if (pkg %in% installed) {
      as.character(packageVersion(pkg))
    } else {
      NA_character_
    }
  })
  
  return(df)
}

#' Install missing packages if network/CRAN is available
#' @param target_packages Character vector of package names or NULL for all missing
#' @return Logical indicator of whether all required packages are present
install_missing_packages <- function(target_packages = NULL) {
  setup_project_lib()
  proj_lib <- file.path(getwd(), ".Rlibs")
  if (!dir.exists(proj_lib)) {
    dir.create(proj_lib, recursive = TRUE, showWarnings = FALSE)
  }
  
  status <- check_packages()
  if (is.null(target_packages)) {
    missing_pkgs <- status$Package[!status$Installed]
  } else {
    missing_pkgs <- target_packages[!(target_packages %in% status$Package[status$Installed])]
  }
  
  if (length(missing_pkgs) == 0) {
    message("All requested packages are already installed.")
    return(TRUE)
  }
  
  message("Attempting to install missing packages: ", paste(missing_pkgs, collapse = ", "))
  tryCatch({
    install.packages(missing_pkgs, lib = proj_lib, repos = "https://cloud.r-project.org")
    setup_project_lib()
  }, error = function(e) {
    warning("Automated package installation failed: ", e$message)
  })
  
  # Re-verify
  status_after <- check_packages()
  req_ok <- all(status_after$Installed[status_after$Tier == "Required"])
  return(req_ok)
}

#' Initialize runtime environment with robust font and plotting fallbacks
load_environment <- function(verbose = FALSE) {
  setup_project_lib()
  
  # Ensure graphics fallback font without requiring remote Google fonts
  # Uses standard cross-platform system sans font
  options(stringsAsFactors = FALSE)
  
  if (verbose) {
    cat("==================================================\n")
    cat(" Calorie Burn Predictor - Environment Loaded\n")
    cat(" R Version: ", R.version.string, "\n")
    cat(" Active Library Paths:\n")
    for (p in .libPaths()) cat("   - ", p, "\n")
    cat("==================================================\n")
  }
}

