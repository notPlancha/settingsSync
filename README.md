<!-- badges: start -->
[![R-CMD-check](https://github.com/notPlancha/settingsSync/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/notPlancha/settingsSync/actions/workflows/R-CMD-check.yaml)
[![CRAN status](https://www.r-pkg.org/badges/version/settingsSync)](https://CRAN.R-project.org/package=settingsSync)
<!-- badges: end -->

# settingsSync
R package and Rstudio Addin to sync its settings, keymaps and keyboard shortcuts, using Google Drive.

## Installation
```r
install.packages("settinsSync")
# or
pak::pak("notPlancha/settingsSync")
# or pak::pak("notPlancha/settingsSync@2.0.0") If you want a specific version
```
Version 2.0.0 is incompatible with CRAN policies, because it writes the files by default but it has a generally better api because of it.
Otherwise it's generally the same IMO.

## Usage
You can directly call `settingsSync::sync()`, or use the Rstudio Addin (they do the same thing); and follow the instructions. The extension pulls the settings from Google Drive (if any), tries to merge them with the local settings (the package has conflict resolution), and then pushes the merged settings to Google Drive.

## Contributing
I'm using [pixi](https://github.com/prefix-dev/pixi) to ease development, and [pre-commit](https://pre-commit.com/). So after cloning:

```bash
# scoop install pixi
pixi install
pixi run hook-install
```
should do the trick. After that open the project in Rstudio. If you want to 
install the exact package versions I used, then run `pak::lockfile_install()`.

All of this is optional, it just helps upkeep for me. If you know what you're doing,
don't worry about it.

> [!NOTE]  
> I didn't add R to pixi because [Windows versions are not updated](https://github.com/conda-forge/r-base-feedstock/issues/248) and they don't work well  with rstudio (and [rstudio in conda is also not updated as of writing](https://github.com/conda-forge/rstudio-feedstock/issues/29)).

Feel free to open an issue or a PR.
