#
# Run this script to setup the keras/tensorflow environment in RStudio Cloud
#


# Install packages --------------------------------------------------------
install.packages(c(
  "keras",     # Only this package is needed for keras/tensorflow
  "rmarkdown",
  "rprojroot",
  "tidyverse",
  "patchwork",
  "ggseqlogo"
))


# Define virtual environment ----------------------------------------------
root <- rprojroot::find_rstudio_root_file()
reticulate::virtualenv_create("r-reticulate")
line <- "reticulate::use_virtualenv(\"r-reticulate\")"
write(line, file.path(root, ".Rprofile"), append = TRUE)
rstudioapi::restartSession()


# Install keras / tensorflow ----------------------------------------------
keras::install_keras(method = "virtualenv", envname = "r-reticulate")
rstudioapi::restartSession()


# Set python environment --------------------------------------------------
path_python = "~/.virtualenvs/r-reticulate/bin/python"
Sys.setenv(RETICULATE_PYTHON = path_python)
library("reticulate")
use_python(path_python, required = TRUE)


# Check installation ------------------------------------------------------
tensorflow::tf_config()
