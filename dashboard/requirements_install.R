requirements_install <- function(path_to_file = "requirements.txt") {
  stopifnot(file.exists(path_to_file))
  requirements <- readLines(file(path_to_file))
  missing_packages <- !requirements %in% installed.packages()
  install.packages(requirements[missing_packages],
                   repos="http://cran.us.r-project.org")
}

requirements_install()
