#' Push Rstudio Settings
#'
#' Pushes Rstudio settings to Google Drive, without pulling. This is just a
#' helper function for [sync()], the main function, but can be used alone, although
#' this function will not do any checking, and will just override.
#' Because of the way [googledrive::drive_put] is built, this function reads from
#' the files directly.
#'
#' @param do_all boolean, if TRUE will push all settings to gd, overwriting them.
#' This param overrides the values of the other params. Default is FALSE.
#' @param do_addins,do_editor_bindings,do_rstudio_bindings booleans, if TRUE will
#' push the respective file. Default is FALSE
#' @param progBar function, designed to work with [progress_bar()]. Runs after
#' each file is pushed.
#'
#' @export
#'
#' @examples
#' mimic_on()
#'   push()              # does nothing
#'   push(do_all = TRUE) # will push all settings to gd, overwriting them
#'   push(do_editor_bindings = FALSE, do_rstudio_bindings = FALSE)
#'   # will push only editor and rstudio bindings
#' mimic_off()
#' @returns nothing
push <- function(do_all = FALSE, do_addins= FALSE, do_editor_bindings= FALSE, do_rstudio_bindings= FALSE, progBar = NULL) {
  if (do_all) {
    do_addins <- TRUE
    do_editor_bindings <- TRUE
    do_rstudio_bindings <- TRUE
  }

  if(progBar |> is.null()) progBar <- \() {invisible()}
  rstudio_path() -> path

  addins            <- file.path(path, "keybindings", "addins.json")
  editor_bindings   <- file.path(path, "keybindings", "editor_bindings.json")
  rstudio_bindings  <- file.path(path, "keybindings", "rstudio_bindings.json")

  message <- \(x) cli::cli_alert_warning("Folder already exists on GoogleDrive, will substitute")

  tryCatch(gd$mkdir("rstudio"), error = message, warning = message)
  if(do_addins)            gd$put(path = "rstudio/addins.json", file = addins)
  progBar()
  if(do_editor_bindings)   gd$put(path = "rstudio/editor_bindings.json", file = editor_bindings)
  progBar()
  if(do_rstudio_bindings)  gd$put(path = "rstudio/rstudio_bindings.json", file = rstudio_bindings)
  progBar()
}

#' Pull Rstudio Settings
#'
#' Pulls Rstudio settings from Google Drive, without pushing. This is just a
#' helper function for [sync()], the main function, but can be used alone, although
#' this function will not do any checking, and will just override.
#'
#' @param all boolean, if TRUE will pull all settings from gd, overwriting them.
#' This param overrides the values of the other params. Default is FALSE.
#'
#' @param addins_gd,editor_bindings_gd,rstudio_bindings_gd character or boolean,
#' the json string to be written to the respective file or TRUE/FALSE;
#' if TRUE, will read from Google Drive;
#' if FALSE to not write that specific file. Default is FALSE
#'
#' @export
#'
#' @examples
#' mimic_on()
#'   pull()           # does nothing
#'   pull(all = TRUE) # will pull all settings from gd, overwriting them
#'   pull(addins_gd = '{"insertPipeOperator": "Shift+Tab"}',)
#'   # will write to addins.json the string
#'   pull(
#'     addins_gd = '{"insertPipeOperator": "Shift+Tab"}',
#'     editor_bindings_gd= TRUE
#'   )
#'   # will write to addins.json the string and pull editor_bindings from gd
#' mimic_off()
#' @seealso [read_from_gd()], [sync()], [push()]
#' @returns nothing
pull <- function(
    all = FALSE,
    addins_gd = FALSE, editor_bindings_gd =FALSE, rstudio_bindings_gd = FALSE
) {
  if (all) {
    addins_gd <- TRUE
    editor_bindings_gd <- TRUE
    rstudio_bindings_gd <- TRUE
  }
  rstudio_path() -> path

  addins            <- file.path(path, "keybindings", "addins.json")
  editor_bindings   <- file.path(path, "keybindings", "editor_bindings.json")
  rstudio_bindings  <- file.path(path, "keybindings", "rstudio_bindings.json")
  if (isTRUE(addins_gd)){
    # This unjsons it, and then jsons it again, no point in optimizing though
    # at least it grants it's valid json
    addins_gd <- read_from_gd("addins")
    if(nrow(addins_gd) == 0) {
      addins_gd <- "{}"
    } else {
      addins_gd <- addins_gd |> jsonlite::unbox() |> jsonlite::toJSON(pretty= TRUE)
    }
  }
  if (isTRUE(editor_bindings_gd)){
    editor_bindings_gd <- read_from_gd("editor_bindings")
    if(nrow(editor_bindings_gd) == 0) {
      editor_bindings_gd <- "{}"
    } else {
      editor_bindings_gd <- editor_bindings_gd |> jsonlite::unbox() |> jsonlite::toJSON(pretty= TRUE)
    }
  }
  if (isTRUE(rstudio_bindings_gd)){
    rstudio_bindings_gd <- read_from_gd("rstudio_bindings")
    if(nrow(rstudio_bindings_gd) == 0) {
      rstudio_bindings_gd <- "{}"
    } else {
      rstudio_bindings_gd <- rstudio_bindings_gd |> jsonlite::unbox() |> jsonlite::toJSON(pretty= TRUE)
    }
  }

  if(!isFALSE(addins_gd))           write_file(addins_gd, addins)
  if(!isFALSE(editor_bindings_gd))  write_file(editor_bindings_gd, editor_bindings)
  if(!isFALSE(rstudio_bindings_gd)) write_file(rstudio_bindings_gd, rstudio_bindings)
}

#' Sync Rstudio Settings
#'
#' Gets the settings from Google Drive and from the local files, and merges them.
#' If there are conflicts, will ask the user to resolve them. Finally, will
#' write the merged settings to the local files, and push them to Google Drive.
#' Will first ask for confirmation if interactive. This function is what's called
#' by the addin.
#'
#' NOTE: if it's not interactive, it won't write to files because of CRAN policies.
#'
#' @param write boolean, if TRUE will write the merged settings to the local files,
#' and push them to Google Drive. FALSE essentially just makes conflict resolution,
#' without changing any files (basically a dry run). If a value other than NULL
#' is provided, this will skip confirmation.
#' @param useProgBar boolean, if TRUE will show a progress bar. Default is TRUE.
#' @export
#'
#' @seealso [push()], [pull()]
#' @examples
#' mimic_on()
#' if(interactive()) {
#'   sync(write = TRUE)   # will immediately try to sync all settings
#'   sync(write = FALSE)  # dry run, will not write to files or push to gd
#'   sync()               # will ask for confirmation, then sync all settings
#' }
#' mimic_off()
#' @returns nothing
sync <- function(write = NULL, useProgBar = TRUE) {
  if (is.null(write)) {
    if (interactive()) {
      if (yesno::yesno("This will pull, try to merge and push your settings; continue?")) {
        write <- TRUE
      } else {
        return(invisible())
      }
    } else {
      write <- FALSE
    }
  }



  progBar <- ifelse(useProgBar, progress_bar(6), NULL)
  # pull, merge and push
  addins_gd           <- read_from_gd("addins", progBar = progBar)
  addins_local        <- read_from_local("addins")
  # check if they're the same
  if(identical(addins_gd, addins_local)) {
    addins_to_sync <- FALSE
  } else {
    addins_merged       <- dplyr::bind_rows(addins_local, addins_gd)
    addins_to_sync      <- full_choose(addins_merged) |>
      jsonlite::toJSON(pretty= TRUE, auto_unbox =T)
  }

  rstudio_bindings_gd <- read_from_gd("rstudio_bindings", progBar = progBar)
  rstudio_bindings_local <- read_from_local("rstudio_bindings")
  if(identical(rstudio_bindings_gd, rstudio_bindings_local)) {
    rstudio_bindings_to_sync <- FALSE
  } else {
    rstudio_bindings_merged <- dplyr::bind_rows(
      rstudio_bindings_local, rstudio_bindings_gd
    )
    rstudio_bindings_to_sync <- full_choose(rstudio_bindings_merged) |>
      jsonlite::toJSON(pretty= TRUE, auto_unbox= TRUE)
  }

  editor_bindings_gd  <- read_from_gd("editor_bindings", progBar = progBar)
  editor_bindings_local  <- read_from_local("editor_bindings")
  if(identical(editor_bindings_gd, editor_bindings_local)) {
    editor_bindings_to_sync <- FALSE
  } else {
    editor_bindings_merged  <- dplyr::bind_rows(
      editor_bindings_local, editor_bindings_gd
    )
    editor_bindings_to_sync <- full_choose(editor_bindings_merged) |>
      jsonlite::toJSON(pretty= TRUE, auto_unbox= TRUE)
  }
  if(!write) {return(invisible())}
  if(
    list(addins_to_sync, rstudio_bindings_to_sync, editor_bindings_to_sync) |>
    lapply(\(x) x |> isFALSE()) |>
    as.logical() |>
    none()
  ) {
    pull(
      addins_gd = addins_to_sync,
      editor_bindings_gd = editor_bindings_to_sync,
      rstudio_bindings_gd = rstudio_bindings_to_sync
    )
    push(
      do_addins = !isFALSE(addins_to_sync),
      do_editor_bindings = !isFALSE(editor_bindings_to_sync),
      do_rstudio_bindings = !isFALSE(rstudio_bindings_to_sync),
      progBar = progBar
    )
    cli::cli_alert_success("Done, restart Rstudio to apply")
  } else {
    cli::cli_alert_info(
      "No changes in any setting detected, both in cloud and local"
    )
    progBar(finish= TRUE)
  }
}

#' Read from Google Drive a JSON file inside rstudio folder
#'
#' This is a helper function for [pull()] and [sync()] that:
#' * reads a json file from Google Drive inside the rstudio folder
#' * converts it to a data frame (to resolve conflicts in the future)
#'
#' @param what One of c("rstudio_bindings", "editor_bindings", "addins"),
#' the file to read
#' @param progBar function, designed to work with [progress_bar()]. Runs after
#' the file is read.
#' @returns A data frame with the contents of the file, converted from json
#' @export
#'
#' @examples
#' mimic_on()
#'   read_from_gd("rstudio_bindings")
#'   read_from_gd("editor_bindings")
#'   read_from_gd("addins")
#' mimic_off()
#' @seealso [read_from_local()] [sync()]
#' [jsonlite::fromJSON()] [googledrive::drive_read_string()]
read_from_gd <- function(what, progBar = NULL) {
  gd$quiet()
  file <- paste0("rstudio/", what,".json") |>
    gd$get()
  if (nrow(file) == 0) {return(data.frame())}
  file |>
    gd$read(encoding = "UTF-8") |>
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
#'
#' @examples
#' mimic_on()
#'   read_from_local("rstudio_bindings")
#'   read_from_local("editor_bindings")
#'   read_from_local("addins")
#' mimic_off()
#' @export
#' @returns A data frame with the contents of the file, converted from json
read_from_local <- function(what) {
  rstudio_path() |> file.path("keybindings", paste0(what,".json")) -> path
  if (!file.exists(path) || path |> empty_json_file()){
    # return empty data frame
    data.frame()
  } else {
    path |> jsonlite::read_json() |> as.data.frame()
  }
}

#' Interactice Conflict Resolution
#'
#' Used to resolve conflicts in [sync()]. Will ask the user to choose between cloud
#' or local, or to choose all cloud or all local.
#'
#' @param df A merged data frame with 2 rows, with the contents of the local and cloud files
#'
#' @returns df, ready to be json'ed, without conflicts
#'
#' @seealso [sync()], [choose()]
#' @noRd
full_choose <- function(df) {
  # if(interactive()) {
  #   full_choose(data.frame(a = c(1,2), b = c(3,4)))
  # }
  if (nrow(df) <= 1) return(df)
  # if a column has NA choose the other option
  # get first non NA from each column
  df |> dplyr::mutate_all(~ifelse(is.na(.), .[!is.na(.)][1], .)) -> df
  # if a column is different then let user choose between the 2
  forAllChosen <- NULL
  ret <- list()
  for (i in seq_along(df)) {
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
#' @noRd
choose <- function(keybind, option1, option2) {
  # if(interactive()) {
  #  choose("Ctrl+Shift+M", "Insert magrittr pipe", "Insert pipe operator")
  # }
  option1 <- ifelse(trimws(option1) == "", "[Removed assigment]", option1)
  option2 <- ifelse(trimws(option2) == "", "[Removed assigment]", option2)
  utils::menu(
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
#' @noRd
progress_bar <- \(n) {
  #  pb <- settingsSync:::progress_bar(10)
  #  for (i in 1:10) {
  #   pb()
  #   Sys.sleep(0.1)
  #  }
  #  pb(finish = TRUE)
  id <- cli::cli_progress_bar(total = n, .auto_close= FALSE)
  counter <- 0
  \(finish= FALSE) {
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


