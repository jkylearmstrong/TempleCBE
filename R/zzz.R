.onLoad <- function(libname, pkgname) {
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
