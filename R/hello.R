push <- function(auth = F, do_addins = T, do_editor_bindings = T, do_rstudio_bindings = T, bar = NULL) {
  if(bar |> is.null()) bar <- \() {invisible()}
  if(auth) {googledrive::drive_auth()}
  rstudio_path() -> path

  addins            <- file.path(path, "keybindings", "addins.json")
  editor_bindings   <- file.path(path, "keybindings", "editor_bindings.json")
  rstudio_bindings  <- file.path(path, "keybindings", "rstudio_bindings.json")

  tryCatch(googledrive::drive_mkdir("rstudio"), error = \(x) cli::cli_alert_warning("Folder already exists on GoogleDrive, will substitute"))
  if(do_addins)            googledrive::drive_put(path = "rstudio/addins.json", media = addins)
  bar()
  if(do_editor_bindings)   googledrive::drive_put(path = "rstudio/editor_bindings.json", media = editor_bindings)
  bar()
  if(do_rstudio_bindings)  googledrive::drive_put(path = "rstudio/rstudio_bindings.json", media = rstudio_bindings)
  bar()
}

pull <- function(
    auth = F,
    addins_gd = NULL, editor_bindings_gd =NULL, rstudio_bindings_gd = NULL) {
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

# funcao principal, so auth fica true
sync <- function(auth = T) {
  bar <- progress_bar(6)
  # pull, merge and push
  if (auth) googledrive::drive_auth()
  addins_gd           <- read_from_gd("addins", bar = bar)
  addins_local        <- read_from_local("addins")
  # check if they're the same
  if(identical(addins_gd, addins_local)) {
    addins_to_sync <- FALSE
  } else {
    addins_merged       <- dplyr::bind_rows(addins_local, addins_gd)
    addins_to_sync      <- full_choose(addins_merged) |> jsonlite::toJSON(pretty = T, auto_unbox =T)
  }

  rstudio_bindings_gd <- read_from_gd("rstudio_bindings", bar = bar)
  rstudio_bindings_local <- read_from_local("rstudio_bindings")
  if(identical(rstudio_bindings_gd, rstudio_bindings_local)) {
    rstudio_bindings_to_sync <- FALSE
  } else {
    rstudio_bindings_merged <- dplyr::bind_rows(rstudio_bindings_local, rstudio_bindings_gd)
    rstudio_bindings_to_sync <- full_choose(rstudio_bindings_merged) |> jsonlite::toJSON(pretty = T, auto_unbox = T)
  }

  editor_bindings_gd  <- read_from_gd("editor_bindings", bar = bar)
  editor_bindings_local  <- read_from_local("editor_bindings")
  if(identical(editor_bindings_gd, editor_bindings_local)) {
    editor_bindings_to_sync <- FALSE
  } else {
    editor_bindings_merged  <- dplyr::bind_rows(editor_bindings_local, editor_bindings_gd)
    editor_bindings_to_sync <- full_choose(editor_bindings_merged) |> jsonlite::toJSON(pretty = T, auto_unbox = T)
  }
  # if none didn't change
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
      bar = bar
    )
    cli::cli_alert_success("Done, restart Rstudio to apply")
  } else {
    cli::cli_alert_info("No changes in any setting detected, both in cloud and local")
    bar(finish = T)
  }
}


# rstudio_bindings, editor_bindings, addins
read_from_gd <- function(what = "rstudio_bindings", auth = F, bar = NULL) {
  if (auth) {googledrive::drive_auth()}
  googledrive::local_drive_quiet()
  file <- paste0("rstudio/", what,".json") |>
    googledrive::drive_get()
  if (nrow(file) == 0) {return(data.frame())}
  file |>
    googledrive::drive_read_string(encoding = "UTF-8") |>
    jsonlite::fromJSON() |>
    as.data.frame() -> ret
  if (bar |> is.null() |> isFALSE()) bar()
  ret
}

# rstudio_bindings, editor_bindings, addins
read_from_local <- function(what = "rstudio_bindings") {
  rstudio_path() |> file.path("keybindings", paste0(what,".json")) -> path
  if (path |> empty_json_file()){
    # return empty data frame
    data.frame()
  } else {
    jsonlite::read_json(path) |> as.data.frame()
  }
}

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

# usethis::is_windows copy
is_windows <- function() {
  .Platform$OS.type == "windows"
}


rstudio_path <- function() {
  rappdirs::user_config_dir() |> file.path(ifelse(is_windows(), "Rstudio", "rstudio"))
}


"%ni%" <- Negate("%in%")


# readr::empty_file, modified
empty_json_file <- function(x) {
  is.character(x) && file.exists(x) && file.info(x, extra_cols = FALSE)$size <= 2
}


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

# none are True (returns true if all x is False)
none <- \(x) x |> all() |> isFALSE()

