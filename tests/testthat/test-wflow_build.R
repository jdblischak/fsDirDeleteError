context("wflow_build")

# Setup ------------------------------------------------------------------------

library(workflowr)

# start project in a tempdir
site_dir <- tempfile("test-wflow_build-")
suppressMessages(wflow_start(site_dir, change_wd = FALSE, user.name = "Test Name",
                             user.email = "test@email"))
on.exit(unlink(site_dir, recursive = TRUE, force = TRUE))
site_dir <- workflowr:::absolute(site_dir)
s <- wflow_status(project = site_dir)

rmd <- rownames(s$status)
stopifnot(length(rmd) > 0)
# Expected html files
html <- workflowr:::to_html(rmd, outdir = s$docs)

# Test wflow_build -------------------------------------------------------------

test_that("wflow_build deletes cache when delete_cache = TRUE", {

  skip_on_cran()

  # Build a file that has cached chunks
  file_w_cache <- file.path(s$analysis, "cache.Rmd")
  fs::file_copy("cache-all-chunks.Rmd", file_w_cache)
  build_v01 <- wflow_build(file_w_cache, view = FALSE, project = site_dir)
  dir_cache <- fs::path_ext_remove(file_w_cache)
  dir_cache <- glue::glue("{dir_cache}_cache")
  expect_true(fs::dir_exists(dir_cache))

  # By default, cache directory is not affected
  dir_cache_mod_pre <- fs::file_info(dir_cache)$modification_time
  expect_message(
    build_v02 <- wflow_build(file_w_cache, view = FALSE, project = site_dir),
    "  - Note: This file has a cache directory"
  )
  expect_false(build_v02$delete_cache)
  expect_true(fs::dir_exists(dir_cache))
  dir_cache_mod_post <- fs::file_info(dir_cache)$modification_time
  expect_equal(dir_cache_mod_post, dir_cache_mod_pre)

  # delete_cache deletes cache directory prior to building (it gets re-created)
  # problem <- fs::path(
  #   dir_cache,
  #   "html",
  #   "session-info-chunk-inserted-by-workflowr_dcb4b1f4c87754270381f62dd2d3f529.RData"
  # )
  expect_true(fs::dir_exists(dir_cache))
  expect_true(fs::dir_exists(fs::path(dir_cache, "html")))
  session_info_files <- fs::dir_ls(path = fs::path(dir_cache, "html"),
                                   regexp = "session-info")
  expect_equal(length(session_info_files), 6)
  expect_true(fs::dir_exists(dir_cache))
  problem <- fs::path(dir_cache, "html/session-info-chunk-inserted-by-workflowr_dcb4b1f4c87754270381f62dd2d3f529.RData")
  expect_true(fs::file_exists(problem))
  dir_cache_mod_pre <- fs::file_info(dir_cache)$modification_time
  expect_message(
    build_v03 <- wflow_build(file_w_cache, view = FALSE, delete_cache = TRUE,
                             project = site_dir),
    "  - Note: Deleted the cache directory before building"
  )
  expect_true(build_v03$delete_cache)
  expect_true(fs::dir_exists(dir_cache))
  dir_cache_mod_post <- fs::file_info(dir_cache)$modification_time
  expect_true(dir_cache_mod_post > dir_cache_mod_pre)

  # Cleanup
  wflow_remove(file_w_cache, project = site_dir)
})
