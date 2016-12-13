load_library = function(lib) {
  # First, check if package is installed, if not install, otherwise just load
  if (is.null(lib)) {
    return (FALSE)
  }
  message('Loading ', lib)
  if (!lib %in% installed.packages()) {
    message('Package ', lib, ' is not installed. Installing')
    install.packages(lib)
  } else {
    library(package = lib,
            character.only = TRUE,
            quietly = TRUE)
  }
  return (TRUE)
}

libs = array(c('MASS', 'lava'))
apply(libs, 1, load_library)