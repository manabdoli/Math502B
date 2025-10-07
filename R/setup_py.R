install <- FALSE
library(reticulate)
version <- "3.9.6"
if(install) {
  install_python(version)
  virtualenv_create(envname = "pyEnv", version = version)
}

use_virtualenv("pyEnv", required = TRUE)
if(install) {
  virtualenv_install(envname = "pyEnv", packages = "matplotlib",
                     ignore_installed = FALSE, pip_options = character())
  virtualenv_install(envname = "pyEnv", packages = "numpy",
                     ignore_installed = FALSE, pip_options = character())
  virtualenv_install(envname = "pyEnv", packages = "pandas",
                     ignore_installed = FALSE, pip_options = character())
}
