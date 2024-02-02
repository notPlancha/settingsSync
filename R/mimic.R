
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

#' @describeIn dry-run-on-off Enable dry run
#' @param local,cloud character, the json that will be used.
#' If NULL, it defaults to a conflicted json. Defaults to such.
#' @export
mimic_on <- function(local = NULL, cloud = NULL) {
  options("ss.mimic" = TRUE)
  if (is.null(local)) {
    local <- '{"insertPipeOperator": "Shift+Tab"}'
  }
  if (is.null(cloud)) {
    cloud <- '{"insertPipeOperator": "Ctrl+Shift+M"}'
  }
  options("ss.mimic.local" = local)
  options("ss.mimic.cloud" = cloud)
}
#' @describeIn dry-run-on-off Disable dry run
#' @export
mimic_off <- function() {
  options("ss.mimic" = FALSE)
  options("ss.mimic.local" = NULL)
  options("ss.mimic.cloud" = NULL)
}

#' @describeIn dry-run-on-off Check if dry run is on
#' @export
is_mimic_on <- \(x) {
  x |> getOption("ss.mimic") |> as.logical()
}

mimic_path_gd <- function() {
  file.path(tempdir(), "gd_mimic")
}

gd_wrapper <- list(
  # auth = \(...) {if (is_mimic_on()) return() else googledrive::drive_auth(...)},
  mkdir = \(name, ...) {
    if (is_mimic_on()) {
      dir.create(file.path(mimic_path_gd(), name))
    } else {
      googledrive::drive_mkdir(name, ...)
    }
  },
  put = \(media, path = NULL, ...) {
    if (is_mimic_on()) {
      cat(media, file = file.path(mimic_path_gd(), path))
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
      local_path <- file.path(mimic_path_gd(), file)
      if (file.exists(local_path)) {
        tibble(
          name = basename(path),
          path = file,
          mimic_ = local_path
          # id = googledrive::as_id(file),
          # drive_resource = list()
        )
      } else {
        tibble(
          name = character(),
          path = character(),
          mimic_ = character()
        )
      }
    } else {
      googledrive::drive_get(path, ...)
    }
  },
  read = \(path, encoding, ...) {
    if (is_mimic_on()) {
      read_file(file.path(mimic_path_gd(), path))
    } else {
      googledrive::drive_read_string(path, ...)
    }
  }
)



