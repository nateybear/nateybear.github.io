suppressPackageStartupMessages({
  library(targets)
  library(pkgdown)
  library(devtools)
  library(knitr)
  library(here)
  library(fs)
  library(magrittr)

  load_all() # install probstats4econ

  document() # document the package

  tar_source(here("R/build")) # source R code
})

# "dummy" because you can list everything
# in the command that should trigger a rebuild
dummy_build_site <- \(...) {
  build_site(preview = FALSE)
}

list(
  tar_target(
    name = line_struct,
    command = build_line_struct(depth = 1),
    cue = tar_cue("always")
  ),
  tar_target(
    name = r_code,
    command = write_r_files(line_struct),
    format = "file"
  ),
  tar_target(
    name = code_zip,
    command = {
      dst <- path(
        here("pkgdown/assets"),
        "ps4e_r_files",
        ext = "zip"
      )
      if (file_exists(dst)) {
        file_delete(dst)
      }
      zip(dst, r_code, flags = "-rj9X") # "junk" the folder structure
      dst
    },
    format = "file"
  ),
  tar_target(
    name = code_template,
    command = here("pkgdown/r_code_template.Rmd"),
    format = "file"
  ),
  tar_target(
    name = code_md,
    command = build_r_code(line_struct, code_template),
    format = "file"
  ),
  tar_target(
    name = data_spec,
    command = as_pkgdown() %$%
      topics %>%
      filter(is_data_topic(name)),
    cue = tar_cue("always")
  ),
  tar_target(
    name = datasets,
    command = write_datasets(data_spec),
    format = "file"
  ),
  tar_target(
    name = datasets_template,
    command = here("pkgdown/datasets_template.Rmd"),
    format = "file"
  ),
  tar_target(
    name = data_md,
    command = {
      knit(
        datasets_template,
        here("datasets.md"),
        envir = env(data_spec = data_spec)
      )
      here("datasets.md")
    },
    format = "file"
  ),
  tar_target(
    name = index_md,
    command = here("index.md"),
    format = "file"
  ),
  tar_target(
    name = package_rmd,
    command = here("package.Rmd"),
    format = "file"
  ),
  tar_target(
    name = package_md,
    command = knit(
      package_rmd,
      here("package.md"),
      envir = env(
        version = package_cran_version
      )
    ),
    format = "file"
  ),
  tar_target(
    name = pkgdown_config,
    command = here("_pkgdown.yml"),
    format = "file"
  ),
  tar_target(
    name = favicon,
    command = here("logo.svg"),
    format = "file"
  ),
  tar_target(
    name = check_pkgdown,
    command = check_pkgdown(),
    cue = tar_cue("always")
  ),
  tar_target(
    name = package_dev_version,
    command = as_pkgdown()$version,
    cue = tar_cue("always")
  ),
  tar_target(
    name = package_cran_version,
    command = get_package_cran_version(),
    cue = tar_cue("always")
  ),
  tar_target(
    name = r_package,
    command = {
      force(package_dev_version) # rebuild only when we version bump
      dst <- here("pkgdown/assets/package.tar.gz")
      build(".", path = dst)
      dst
    },
    format = "file"
  ),
  tar_target(
    name = build_site,
    command = dummy_build_site(
      check_pkgdown,
      pkgdown_config,
      favicon,
      index_md,
      package_md,
      r_code,
      datasets,
      code_md,
      data_md,
      r_package
    ),
  )
)
