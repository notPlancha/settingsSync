push <- function(auth = F, do_addins = T, do_editor_bindings = T, do_rstudio_bindings = T) {
  if(auth) {googledrive::drive_auth()}
  rstudio.prefs::rstudio_config_path() -> path

  addins            <- file.path(path, "keybindings", "addins.json")
  editor_bindings   <- file.path(path, "keybindings", "editor_bindings.json")
  rstudio_bindings  <- file.path(path, "keybindings", "rstudio_bindings.json")

  tryCatch(googledrive::drive_mkdir("rstudio"), error = \(x) print("Folder already exist, will substitute"))
  if(do_addins)            googledrive::drive_put(path = "rstudio/addins.json", media = addins)
  if(do_editor_bindings)   googledrive::drive_put(path = "rstudio/editor_bindings.json", media = editor_bindings)
  if(do_rstudio_bindings)  googledrive::drive_put(path = "rstudio/rstudio_bindings.json", media = rstudio_bindings)
}

pull <- function(
    auth = F,
    addins_gd = NULL, editor_bindings_gd =NULL, rstudio_bindings_gd = NULL) {
  if(auth) {googledrive::drive_auth()}
  rstudio.prefs::rstudio_config_path() -> path

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
  if(!isFALSE(rstudio_bindings_gd)) cat(rstudio_bindings_gd, file=rstudio_bindings_gd)
}

# funcao principal, so auth fica true
sync <- function(auth = T) {
  # pull, merge and push
  if (auth) googledrive::drive_auth()

  addins_gd           <- read_from_gd("addins")
  addins_local        <- read_from_local("addins")
  if(identical(addins_gd, addins_local)) {
    addins_to_sync <- FALSE
  } else {
    addins_merged       <- dplyr::bind_rows(addins_local, addins_gd)
    addins_to_sync      <- full_choose(addins_merged) |> rjson::toJSON()
  }

  rstudio_bindings_gd <- read_from_gd("rstudio_bindings")
  rstudio_bindings_local <- read_from_local("rstudio_bindings")
  if(identical(rstudio_bindings_gd, rstudio_bindings_local)) {
    rstudio_bindings_to_sync <- FALSE
  } else {
    rstudio_bindings_merged <- dplyr::bind_rows(rstudio_bindings_local, rstudio_bindings_gd)
    rstudio_bindings_to_sync <- full_choose(rstudio_bindings_merged) |> rjson::toJSON()
  }

  editor_bindings_gd  <- read_from_gd("editor_bindings")
  editor_bindings_local  <- read_from_local("editor_bindings")
  if(identical(editor_bindings_gd, editor_bindings_local)) {
    editor_bindings_to_sync <- FALSE
  } else {
    editor_bindings_merged  <- dplyr::bind_rows(editor_bindings_local, editor_bindings_gd)
    editor_bindings_to_sync <- full_choose(editor_bindings_merged) |> rjson::toJSON()
  }

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
    do_rstudio_bindings = !isFALSE(rstudio_bindings_to_sync)
  )
}


# rstudio_bindings, editor_bindings, addins
read_from_gd <- function(what = "rstudio_bindings", auth = F) {
  if (auth) {googledrive::drive_auth()}
  file <- paste0("rstudio/", what,".json") |>
    googledrive::drive_get()
  if (nrow(file) == 0) {return(data.frame())}
  paste0("rstudio/", what,".json") |>
    googledrive::drive_get() |>
    googledrive::drive_read_string(encoding = "UTF-8") |>
    rjson::fromJSON() |>
    as.data.frame()
}

# rstudio_bindings, editor_bindings, addins
read_from_local <- function(what = "rstudio_bindings") {
  rstudio.prefs::rstudio_config_path() -> path
  rjson::fromJSON(file = file.path(path, "keybindings", paste0(what,".json"))) |> as.data.frame()
}

full_choose <- function(df) {
  if (ncol(df) == 0) return(df)

  # if a column has NA choose the other option
  # get first non NA from each column
  df |> dplyr::mutate_all(~ifelse(is.na(.), .[!is.na(.)][1], .)) -> df
  # if a column is different then let user choose between the 2
  forAllChosen <- NULL
  ret <- list()
  for (i in 1:ncol(df)) {
    name <- names(df)[i]
    chosen <- NULL
    if (df[1,i] != df[2,i]) {
      if (!is.null(forAllChosen)) {
        if(forAllChosen == "cloud") {
          chosen <- df[2,i]
        } else {
          chosen <- df[1,i]
        }
      } else {choose(names(df)[i], df[1,i], df[2,i]) |> switch(
        "1" = df[1,i],
        "2" = df[2,i],
        "3" = {forAllChosen <- "cloud"; df[1,i]},
        "4" = {forAllChosen <- "local"; df[2,i]},
        "0" = stop("Sync cancelled")
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
  rstudioapi::showPrompt("Sync conflicts", paste(
    "There are conflicts for", keybind, "\n",
    "(1)", option1, "(cloud) \n",
    "(2)", option2, "(local) \n",
    "(3) Choose all cloud \n",
    "(4) Choose all local \n",
    "(0) Cancel"
  ))
}
