# Entrypoint for the test suite. This repo isn't a package, so run it from the
# repo root with:
#   Rscript -e 'testthat::test_dir("tests/testthat")'
#
# helper-src.R loads the repo first (setup.R). The golden tests also need the
# shared data folder, and golden files on this machine: see
# tests/testthat/fixtures/README.md.

library(testthat)

test_dir("tests/testthat")
