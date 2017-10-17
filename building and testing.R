if (FALSE) 
{
  # R --vanilla
  library(colorout)

  pkg_full_path <- "~/Development/rpkgs/getfunced"
  setwd(pkg_full_path)

  ## PACKAGES TO USE
  if (FALSE) {
    devtools::use_package("magrittr", pkg=pkg_full_path)
    devtools::use_package("stats",    pkg=pkg_full_path)
    devtools::use_package("utils",    pkg=pkg_full_path)
    devtools::use_package("collectArgs", pkg=pkg_full_path)
  }

  ## SET UP DOCUMENTATION
  devtools::document(pkg=pkg_full_path)

  ## VIGNETTE
  if (FALSE) {
    devtools::use_vignette("getfunced", pkg=pkg_full_path)
    devtools::build_vignettes(pkg=pkg_full_path)
  }

  ## TESTING
  if (FALSE) {
    devtools::use_testthat(pkg=pkg_full_path)
    devtools::test(pkg=pkg_full_path)
  }

  if (FALSE)
    devtools::install_local(pkg_full_path)


  devtools::document(pkg=pkg_full_path)
  devtools::check(pkg=pkg_full_path)

  ## REMEMBER TO CHECK OUT THE LATEST STABLE BRANCH BEFORE BUILDING
  if (FALSE) {
    rsupkg_next_version("getfunced", .test_run=TRUE)
    # rsutils:::rsupkg_next_version("getfunced", .test_run=FALSE)
    # devtools::build(pkg=pkg_full_path)
  }
}

