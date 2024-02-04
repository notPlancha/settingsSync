
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
#' @name mimic-on-off
#' @examples
#' mimic_on()
#' if(interactive()) {
#'   sync()
#' }
#' mimic_off()
NULL

#' @describeIn mimic-on-off Enable mimic and write mimic files
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
#' @describeIn mimic-on-off Disable mimic
#' @export
mimic_off <- function() {
  options("ss.mimic" = FALSE)
}

#' @describeIn mimic-on-off Check if mimic is on
#' @export
is_mimic_on <- \() {
  getOption("ss.mimic") |> as.logical()
}


#'@noRd
get_mimic_folder_gd <- function() {
  file.path(tempdir(), "mimic_gd")
}
#'@noRd
get_mimic_folder_local <- function() {
  file.path(tempdir(), "mimic_local")
}

# https://github.com/hadley/r-pkgs/issues/828
#'@noRd
ignore_unused_import_for_gd <- function() {
  googledrive::drive_auth
  googledrive::drive_mkdir
  googledrive::drive_put
  googledrive::local_drive_quiet
  googledrive::drive_get
  googledrive::drive_read_string
  googledrive::as_id
  tibble::is_tibble
  tibble::tibble
}

#'@noRd
gd <- list(
  # auth = \(...) {if (is_mimic_on()) return() else googledrive::drive_auth(...)},
  mkdir = \(name, ...) {
    if (is_mimic_on()) {
      dir.create(file.path(get_mimic_folder_gd(), name), recursive = TRUE)
    } else {
      googledrive::drive_mkdir(name, ...)
    }
  },
  put = \(file, path = NULL, ...) {
    # put puts the file directly, without reading it (differs from read)
    if (is_mimic_on()) {
      write_file(file |> read_file(), file.path(get_mimic_folder_gd(), path))
    } else {
      googledrive::drive_put(file, path, ...)
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
    # get gets the metadata of the file
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
    # read reads the file as a string
    # file can be a path or a tibble (or a dribble) (metadata)
    if (is_mimic_on()) {
      if (tibble::is_tibble(file)) {
        file <- file.path(get_mimic_folder_gd(), file$path)
      }
      return(read_file(file))
    } else {
      googledrive::drive_read_string(file, ...)
    }
  }
)

#'@noRd
mimic_read_files <- function() {
  path <- file.path(get_mimic_folder_local(), "keybindings", "rstudio_bindings.json") |> normalizePath("/")
  print(paste(path, ":",path |> read_file()))
  path <- file.path(get_mimic_folder_gd(), "rstudio", "rstudio_bindings.json") |> normalizePath("/")
  print(paste(path, ":",path |> read_file()))
}

