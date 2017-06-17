soundgen_app = function() {
  appDir = system.file("shiny", "soundgen_main", package = "soundgen")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `soundgen`.",
         call. = FALSE)
  }
  shiny::runApp(appDir, display.mode = "normal")
}
