<!-- badges: start -->
[![R-CMD-check](https://github.com/notPlancha/settingsSync/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/notPlancha/settingsSync/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

# settingsSync
R package and Rstudio Addin to sync its settings, keymaps and keyboard shortcuts, using Google Drive.

## Installation
Planning to submit to CRAN, but for now you can install it from github:
```r
devtools::install_github("notPlancha/settingsSync")
# or
pak::pak("notPlancha/settingsSync")
```

## Usage
You can directly call `settingsSync::sync()`, or use the Rstudio Addin (they do the same thing); and follow the instructions. The extension pulls the settings from Google Drive (if any), tries to merge them with the local settings (the package has conflict resolution), and then pushes the merged settings to Google Drive.

## Contributing
Feel free to open an issue or a PR.
