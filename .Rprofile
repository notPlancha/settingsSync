if (interactive()) {
  require(pak)
  require(devtools)
  require(conflicted)
  require(desc)
  require(icecream)
  options(icecream.always.include.context = T)
  options(error = function() traceback())
  # pak("r-lib/desc") # theres a bug
  # pak("r-lib/revdepcheck")
  # pak("jumpingrivers/inteRgrate")
  ## https://www.r-bloggers.com/2020/07/how-to-write-your-own-r-package-and-publish-it-on-cran/
  ## https://github.com/DavisVaughan/extrachecks
  d <- desc::desc()
  tryit <- \(enable_mimic = T) {
    devtools::load_all()
    cli::cli_alert_info("mimic" |> paste(ifelse(enable_mimic, "enabled", "disabled")))
    if(enable_mimic) {
      mimic_on()
      cli::cli_alert("mimic_read_files() to read the mimic files")
    } else {
      mimic_off()
    }
    cli::cli_inform("ic_disable() to disable debug prints \n ic_enable() for the opposite")

  }
  build_docs     <- \() {devtools::document()}
  local_check    <- \() {devtools::check()}
  multi_check    <- \() {
    ch <- devtools::check_rhub()
    ch
  }
  print_cran_comments <- \(ch) {
    ch$cran_summary()
  }
  #revdep <- \() {revdepcheck::use_revdep()} # n percebi o q faz e n consegui correr
  devel_check <- \() {
    devtools::check_win_devel()
  }

  create_gh_action <- \() {
    usethis::use_github_action()
  }

  goodpractice_check <- \() {
    goodpractice::gp()
  }
}
