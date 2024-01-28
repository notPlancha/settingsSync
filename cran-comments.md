## Test environments
- R-hub windows-x86_64-devel (r-devel)
- R-hub ubuntu-gcc-release (r-release)
- R-hub fedora-clang-devel (r-devel)

## R CMD check results
❯ On windows-x86_64-devel (r-devel)
  checking CRAN incoming feasibility ... NOTE
  Maintainer: 'notPlancha <Andre_Plancha@iscte-iul.pt>'
  
  New submission
  
  Possibly misspelled words in DESCRIPTION:
    Addin (3:16, 9:22)
    Keymaps (3:43)
    Rstudio (3:8, 9:14)
    keymaps (9:46)
    
> dev note: not misspelled 

❯ On windows-x86_64-devel (r-devel)
  checking for non-standard things in the check directory ... NOTE
  Found the following files/directories:
    ''NULL''

> dev note: rhub thing https://github.com/r-hub/rhub/issues/560

❯ On windows-x86_64-devel (r-devel)
  checking for detritus in the temp directory ... NOTE
  Found the following files/directories:
    'lastMiKTeXException'

> dev note: rhub bug https://github.com/r-hub/rhub/issues/503

❯ On ubuntu-gcc-release (r-release)
  checking CRAN incoming feasibility ... [6s/27s] NOTE
  Maintainer: ‘notPlancha <Andre_Plancha@iscte-iul.pt>’
  
  New submission
  
  Possibly misspelled words in DESCRIPTION:
    Addin (3:16, 9:22)
    keymaps (9:46)
    Keymaps (3:43)
    Rstudio (3:8, 9:14)

> dev note: not misspelled 


❯ On ubuntu-gcc-release (r-release), fedora-clang-devel (r-devel)
  checking HTML version of manual ... NOTE
  Skipping checking HTML validation: no command 'tidy' found

> dev note: rhub bug https://github.com/r-hub/rhub/issues/548

❯ On fedora-clang-devel (r-devel)
  checking CRAN incoming feasibility ... [6s/34s] NOTE
  Maintainer: ‘notPlancha <Andre_Plancha@iscte-iul.pt>’
  
  New submission
  
  Possibly misspelled words in DESCRIPTION:
    Addin (3:16, 9:22)
    Keymaps (3:43)
    Rstudio (3:8, 9:14)
    keymaps (9:46)

> dev note: not misspelled

0 errors ✔ | 0 warnings ✔ | 6 notes ✖
