if (interactive()) {
  require(pak)
  require(devtools)
  require(conflicted)
  require(desc)
  require(icecream)
  require(rhub)
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
  build_docs     <- \() {devtools::document(roclets = c('rd', 'collate', 'namespace'))}
  # n sei se este document esta certo mas e o que o rstudio chama
  local_check    <- \() {devtools::check(remote = T, cran = T)}
  multi_check    <- \() {
    # melhor e por no github e ver, e ignorar isto
    ch <<- rhub::rhub_check()
    ch
  }
  print_cran_comments <- \(ch) {
    # ja nao fuinciona, tirar no futuro
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

  build_manual <- \() {
    build_docs()
    callr::rcmd("Rd2pdf", cmdargs = c( ".\\", "--no-clean"))
  }
}
