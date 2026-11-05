# Load the repo the way the Rmds do: setup.R attaches the packages and sources
# everything in src/ into the global environment. It uses paths from the repo
# root, and testthat runs from tests/testthat.
withr::with_dir(test_path("..", ".."), source("setup.R"))
