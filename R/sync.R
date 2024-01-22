#' Push Rstudio Settings
#'
#' Pushes Rstudio settings to Google Drive, without pulling. This is just a
#' helper function for [sync()], the main function, but can be used alone, although
#' this fucntion will not do any checking, and will just override.
#' Because of the way [googledrive::drive_put] is built, this function reads from
#' the files directly.
#'
#' @param auth boolean, if TRUE will authenticate with Google Drive, if FALSE will
#' use the already authenticated. Default is FALSE, because [sync()]
#' will authenticate.
#' @param do_addins,do_editor_bindings,do_rstudio_bindings booleans, if TRUE will
#' push the respective file. Default is TRUE.
#' @param progBar function, designed to work with [progress_bar()]. Runs after
#' each file is pushed.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   push()
#'   # will push all settings to gd, overwriting them
#'   push(do_addins = F)
#'   # will push only editor and rstudio bindings
#'   push(auth = T)
#'   # will authenticate and push all settings
#' }
push <- function(auth = F, do_addins = T, do_editor_bindings = T, do_rstudio_bindings = T, progBar = NULL) {
  if(progBar |> is.null()) progBar <- \() {invisible()}
  if(auth) {googledrive::drive_auth()}
  rstudio_path() -> path

  addins            <- file.path(path, "keybindings", "addins.json")
  editor_bindings   <- file.path(path, "keybindings", "editor_bindings.json")
  rstudio_bindings  <- file.path(path, "keybindings", "rstudio_bindings.json")

  tryCatch(googledrive::drive_mkdir("rstudio"), error = \(x) cli::cli_alert_warning("Folder already exists on GoogleDrive, will substitute"))
  if(do_addins)            googledrive::drive_put(path = "rstudio/addins.json", media = addins)
  progBar()
  if(do_editor_bindings)   googledrive::drive_put(path = "rstudio/editor_bindings.json", media = editor_bindings)
  progBar()
  if(do_rstudio_bindings)  googledrive::drive_put(path = "rstudio/rstudio_bindings.json", media = rstudio_bindings)
  progBar()
}

#' Pull Rstudio Settings
#'
#' Pulls Rstudio settings from Google Drive, without pushing. This is just a
#' helper function for [sync()], the main function, but can be used alone, although
#' this fucntion will not do any checking, and will just override.
#'
#' @inheritParams push
#' @param addins_gd,editor_bindings_gd,rstudio_bindings_gd character, the json
#' string to be written to the respective file; if NULL, will read from
#' Google Drive. Can also be FALSE to not write that specific file.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'  pull()
#'  # will pull all settings from gd, overwriting them
#'  pull(auth = T)
#'  # will authenticate and pull all settings
#'  pull(addins_gd = '{"insertPipeOperator": "Shift+Tab"}', editor_bindings_gd = F, rstudio_bindings_gd = F)
#'  # will write to addins.json the string '{"insertPipeOperator": "Shift+Tab"}'
#'  pull(addins_gd = '{"insertPipeOperator": "Shift+Tab"}')
#'  # will write to addins.json the string '{"insertPipeOperator": "Shift+Tab"}',
#'  # and pull the other 2 files from gd
#' }
#' @seealso [read_from_gd()], [sync()], [push()]
pull <- function(
    auth = F,
    addins_gd = NULL, editor_bindings_gd =NULL, rstudio_bindings_gd = NULL
) {
  if(auth) {googledrive::drive_auth()}
  rstudio_path() -> path

  addins            <- file.path(path, "keybindings", "addins.json")
  editor_bindings   <- file.path(path, "keybindings", "editor_bindings.json")
  rstudio_bindings  <- file.path(path, "keybindings", "rstudio_bindings.json")
  if (is.null(addins_gd)){
    addins_gd <- read_from_gd("addins")
  }
  if (is.null(editor_bindings_gd)){
    editor_bindings_gd <- read_from_gd("editor_bindings")
  }
  if (is.null(rstudio_bindings_gd)){
    rstudio_bindings_gd <-read_from_gd("rstudio_bindings")
  }
  if(!isFALSE(addins_gd))           cat(addins_gd, file= addins)
  if(!isFALSE(editor_bindings_gd))  cat(editor_bindings_gd, file=editor_bindings)
  if(!isFALSE(rstudio_bindings_gd)) cat(rstudio_bindings_gd, file=rstudio_bindings)
}

# TODO make prog bar optional?
#' Sync Rstudio Settings
#'
#' Gets the settings from Google Drive and from the local files, and merges them.
#' If there are conflicts, will ask the user to resolve them. Finally, will
#' write the merged settings to the local files, and push them to Google Drive.
#'
#' @param auth boolean, if TRUE will authenticate with Google Drive, if FALSE will
#' use the already authenticated. Default is TRUE, which difers from [push()] and
#' [pull()].
#' @param write boolean, if TRUE will write the merged settings to the local files,
#' and push them to Google Drive. FALSE essentially just makes conflict resolution,
#' without changing any files (basically a dry run). Default is TRUE.
#' @export
#'
#' @seealso [push()], [pull()]
#' @examples
sync <- function(auth = T, write = T) {
  progBar <- progress_bar(6)
  # pull, merge and push
  if (auth) googledrive::drive_auth()
  addins_gd           <- read_from_gd("addins", progBar = progBar)
  addins_local        <- read_from_local("addins")
  # check if they're the same
  if(identical(addins_gd, addins_local)) {
    addins_to_sync <- FALSE
  } else {
    addins_merged       <- dplyr::bind_rows(addins_local, addins_gd)
    addins_to_sync      <- full_choose(addins_merged) |> jsonlite::toJSON(pretty = T, auto_unbox =T)
  }

  rstudio_bindings_gd <- read_from_gd("rstudio_bindings", progBar = progBar)
  rstudio_bindings_local <- read_from_local("rstudio_bindings")
  if(identical(rstudio_bindings_gd, rstudio_bindings_local)) {
    rstudio_bindings_to_sync <- FALSE
  } else {
    rstudio_bindings_merged <- dplyr::bind_rows(rstudio_bindings_local, rstudio_bindings_gd)
    rstudio_bindings_to_sync <- full_choose(rstudio_bindings_merged) |> jsonlite::toJSON(pretty = T, auto_unbox = T)
  }

  editor_bindings_gd  <- read_from_gd("editor_bindings", progBar = progBar)
  editor_bindings_local  <- read_from_local("editor_bindings")
  if(identical(editor_bindings_gd, editor_bindings_local)) {
    editor_bindings_to_sync <- FALSE
  } else {
    editor_bindings_merged  <- dplyr::bind_rows(editor_bindings_local, editor_bindings_gd)
    editor_bindings_to_sync <- full_choose(editor_bindings_merged) |> jsonlite::toJSON(pretty = T, auto_unbox = T)
  }
  if(!write) {return(invisible())}
  if(
    list(addins_to_sync, rstudio_bindings_to_sync, editor_bindings_to_sync) |>
    lapply(\(x) x |> isFALSE()) |>
    as.logical() |>
    none()
  ) {
    pull(
      auth = F,
      addins_gd = addins_to_sync,
      editor_bindings_gd = editor_bindings_to_sync,
      rstudio_bindings_gd = rstudio_bindings_to_sync
    )
    push(
      auth = F,
      do_addins = !isFALSE(addins_to_sync),
      do_editor_bindings = !isFALSE(editor_bindings_to_sync),
      do_rstudio_bindings = !isFALSE(rstudio_bindings_to_sync),
      progBar = progBar
    )
    cli::cli_alert_success("Done, restart Rstudio to apply")
  } else {
    cli::cli_alert_info("No changes in any setting detected, both in cloud and local")
    progBar(finish = T)
  }
}


# rstudio_bindings, editor_bindings, addins
#' Read from Google Drive a JSON file inside rstudio folder
#'
#' This is a helper function for [pull()] and [sync()] that:
#' * reads a json file from Google Drive inside the rstudio folder
#' * converts it to a data frame (to resolve conflicts in the future)
#'
#' @param what One of c("rstudio_bindings", "editor_bindings", "addins"),
#' the file to read
#' @inheritParams push
#'
#' @returns A data frame with the contents of the file, converted from json
#' @export
#'
#' @examples
#' \dontrun{
#'  read_from_gd("rstudio_bindings")
#'  read_from_gd("editor_bindings")
#'  read_from_gd("addins")
#' }
#' @seealso [read_from_local()], [sync()], [jsonlite::fromJSON()], [googledrive::drive_read_string()]
read_from_gd <- function(what, auth = F, progBar = NULL) {
  if (auth) {googledrive::drive_auth()}
  googledrive::local_drive_quiet()
  file <- paste0("rstudio/", what,".json") |>
    googledrive::drive_get()
  if (nrow(file) == 0) {return(data.frame())}
  file |>
    googledrive::drive_read_string(encoding = "UTF-8") |>
    jsonlite::fromJSON() |>
    as.data.frame() -> ret
  if (progBar |> is.null() |> isFALSE()) progBar()
  ret
}

#' Read from local a JSON file inside rstudio folder
#'
#' This is a helper function for [sync()] that:
#' * reads a json file from the local rstudio folder
#' * converts it to a data frame (to resolve conflicts in the future)
#'
#' @inheritParams read_from_gd
#' @seealso [read_from_gd()], [push()], [sync()], [jsonlite::read_json()]
#' @export
#'
#' @examples
#' \dontrun{
#'   read_from_local("rstudio_bindings")
#'   read_from_local("editor_bindings")
#'   read_from_local("addins")
#' }
read_from_local <- function() {
  rstudio_path() |> file.path("keybindings", paste0(what,".json")) -> path
  if (path |> empty_json_file()){
    # return empty data frame
    data.frame()
  } else {
    jsonlite::read_json(path) |> as.data.frame()
  }
}


#' Interactice Conflict Resolution
#'
#' Used to resolve conflicts in [sync()]. Will ask the user to choose between cloud
#' or local, or to choose all cloud or all local.
#'
#' @param df A merged data frame with 2 rows, with the contents of the local and cloud files
#'
#' @returns df, ready to be jsoned, without conflicts
#'
#' @examples
#' \dontrun{
#'   full_choose(data.frame(a = c(1,2), b = c(3,4)))
#' }
#' @seealso [sync()], [choose()]
# dont export
full_choose <- function(df) {
  if (nrow(df) <= 1) return(df)
  # if a column has NA choose the other option
  # get first non NA from each column
  df |> dplyr::mutate_all(~ifelse(is.na(.), .[!is.na(.)][1], .)) -> df
  # if a column is different then let user choose between the 2
  forAllChosen <- NULL
  ret <- list()
  for (i in 1:ncol(df)) {
    name <- names(df)[i]
    chosen <- NULL
    if (df[1, i] != df[2, i]) {
      if (!is.null(forAllChosen)) {
        if(forAllChosen == "cloud") {
          chosen <- df[2, i]
        } else {
          chosen <- df[1, i]
        }
      } else {
        choose(names(df)[i], df[1, i], df[2, i]) |> switch(
          "1" = df[1, i],
          "2" = df[2, i],
          "3" = {forAllChosen <- "cloud"; df[1, i]},
          "4" = {forAllChosen <- "local"; df[2, i]},
          "0" = stop("Sync cancelled"),
        ) -> chosen
      }
    } else {
      chosen <- df[1,i]
    }
    ret[[name]] <- chosen
  }
  ret
}

#' Iteration of [choose()]
#'
#' @param keybind Conflictive keybind
#' @param option1 First option
#' @param option2 Second option
#'
#' @return The chosen option
#'
#' @examples
#' \dontrun{
#'  choose("Ctrl+Shift+M", "Insert magrittr pipe", "Insert pipe operator")
#' }
# dont export
choose <- function(keybind, option1, option2) {
  option1 <- ifelse(trimws(option1) == "", "[Removed assigment]", option1)
  option2 <- ifelse(trimws(option2) == "", "[Removed assigment]", option2)
  menu(
    title = glue::glue("There are conflicts for {keybind}, which option should stay?"),
    choices = c(
      glue::glue("{option1} (cloud)"),
      glue::glue("{option2} (local)"),
      "Choose all cloud",
      "Choose all local"
    )
  )
}


#' Progress Bar Closure
#'
#' Each time the closure is called will update the progress bar
#'
#' @param n Total number of iterations
#'
#' @return A function/closure that when called will update the progress bar
#'
#' @examples
#'  pb <- progress_bar(10)
#'  for (i in 1:10) {
#'   pb()
#'   Sys.sleep(0.1)
#'  }
#'  pb(finish = T)

progress_bar <- \(n) {
  id <- cli::cli_progress_bar(total = n, .auto_close = F)
  counter <- 0
  \(finish = F) {
    if (finish) counter = n
    counter <<- counter + 1
    if(counter >= n) {
      cli::cli_progress_done(id = id)
      cli::cli_progress_cleanup()
    } else {
      cli::cli_progress_update(id = id)
    }
  }
}

