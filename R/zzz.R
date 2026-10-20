.onLoad <- function(libname, pkgname) {
  if (is.null(getOption("templecbe.cox_ties"))) {
    options(templecbe.cox_ties = "breslow")
  }
  # Register the coxnet engine with parsnip whichever package loads first.
  if (isNamespaceLoaded("parsnip")) {
    register_coxnet_engine()
  } else {
    setHook(
      packageEvent("parsnip", "onLoad"),
      function(...) register_coxnet_engine()
    )
  }
  invisible()
}
