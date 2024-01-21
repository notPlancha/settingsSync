# none are True (returns true if all x is False)
none <- \(x) x |> all() |> isFALSE()

# readr::empty_file, modified
empty_json_file <- function(x) {
  is.character(x) && file.exists(x) && file.info(x, extra_cols = FALSE)$size <= 2
}

"%ni%" <- Negate("%in%")

# usethis::is_windows copy
is_windows <- function() {
  .Platform$OS.type == "windows"
}

rstudio_path <- function() {
  rappdirs::user_config_dir() |> file.path(ifelse(is_windows(), "Rstudio", "rstudio"))
}
