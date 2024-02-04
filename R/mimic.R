
#' Simulate the functions before truly running them
#'
#' These functions are used to simulate the main functions before truly running
#' them, to disable changes to the files and to the cloud.
#'
#' This are used in most examples so the user settings don't change
#' when running [utils::example()]. Internally, these change the options
#' "ss.mimic", "ss.mimic.local" and "ss.mimic.cloud".
#'
#'
#' @name dry-run-on-off
#' @examples
#' mimic_on(local = "{}", cloud = "{}")
#' if(interactive()) {
#'   sync()
#' }
#' mimic_off()
NULL

#' @describeIn dry-run-on-off Enable dry run and write mimic files
#' @export
mimic_on <- function() {
  options("ss.mimic" = TRUE)
  # write to mimiqued local and cloud
  write_file(
    '{"insertPipeOperator": "Shift+Tab"}',
    file.path(get_mimic_folder_gd(), "rstudio", "rstudio_bindings.json")
  )
  write_file(
    '{"insertPipeOperator": "Ctrl+Tab"}',
    file.path(get_mimic_folder_local(), "keybindings","rstudio_bindings.json")
  )
}
#' @describeIn dry-run-on-off Disable dry run
#' @export
mimic_off <- function() {
  options("ss.mimic" = FALSE)
}

#' @describeIn dry-run-on-off Check if dry run is on
#' @export
is_mimic_on <- \() {
  getOption("ss.mimic") |> as.logical()
}

get_mimic_folder_gd <- function() {
  file.path(tempdir(), "mimic_gd")
}
get_mimic_folder_local <- function() {
  file.path(tempdir(), "mimic_local")
}

gd <- list(
  # auth = \(...) {if (is_mimic_on()) return() else googledrive::drive_auth(...)},
  mkdir = \(name, ...) {
    if (is_mimic_on()) {
      dir.create(file.path(get_mimic_folder_gd(), name), recursive = TRUE)
    } else {
      googledrive::drive_mkdir(name, ...)
    }
  },
  put = \(media, path = NULL, ...) {
    if (is_mimic_on()) {
      write_file(media, file.path(get_mimic_folder_gd(), path))
    } else {
      googledrive::drive_put(media, path, ...)
    }
  },
  quiet = \(...) {
    if (is_mimic_on()) {
      invisible()
    } else {
      googledrive::local_drive_quiet(...)
    }
  },
  get = \(file, ...) {
    if (is_mimic_on()) {
      local_path <- file.path(get_mimic_folder_gd(), file)
      if (file.exists(local_path)) {
        tibble::tibble(
          name = basename(local_path),
          path = file
          # id = googledrive::as_id(file),
          # drive_resource = list()
        )
      } else {
        tibble::tibble(
          name = character(),
          path = character()
        )
      }
    } else {
      googledrive::drive_get(file, ...)
    }
  },
  read = \(file, encoding = "UTF-8", ...) {
    # file can be a path or a tibble (or a dribble)
    if (is_mimic_on()) {
      if (tibble::is_tibble(file)) {
        path <- file.path(get_mimic_folder_gd(), file$path)
      }
      return(read_file(path))
    } else {
      googledrive::drive_read_string(file, ...)
    }
  }
)

mimic_read_files <- function() {
  path <- file.path(get_mimic_folder_local(), "keybindings", "rstudio_bindings.json") |> normalizePath("/")
  print(paste(path, ":",path |> read_file()))
  path <- file.path(get_mimic_folder_gd(), "rstudio", "rstudio_bindings.json") |> normalizePath("/")
  print(paste(path, ":",path |> read_file()))
}

