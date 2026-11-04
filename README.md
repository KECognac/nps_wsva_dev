# nps_wsva_dev

**Development repo for the NPS Water Supply Vulnerability Assessment (WSVA) App**

The rapid, nationwide water supply vulnerability assessment of National Park
Service (NPS) units: the indicator functions, the workflow that runs them, and
the notebooks used to explore them. It produces the indicators and the data that
[`nps_wsva_app`](https://github.com/Water-Vulnerability-Analytics/nps_wsva_app)
reads.

Part of [Water Vulnerability Analytics (WaVA)](https://github.com/Water-Vulnerability-Analytics).
The code was split out of
[`nps_water_vulnerability`](https://github.com/Water-Vulnerability-Analytics/nps_water_vulnerability)
with its commit history. This repo is not an R package.

## Contents

- `setup.R`: installs and attaches the packages, and sources everything in `src/`
- `rapid_data_processing.Rmd`: preprocesses large datasets (run once, or when
  the source data changes)
- `rapid_workflow.Rmd`: calculates the indicators
- `app_data_prep.Rmd`: prepares the indicators and writes them to the app repo
- `src/`: the functions
  - `calc_*_exposure()` and `calc_*_sensitivity()`: the indicators
  - `process_*()`: prepare the input data that some indicators need
  - `add_standardized_indicators()`: adds normalized and ranked columns to an
    indicator table
- `Explore/`: notebooks for checking data processing and indicator outputs
- `tests/`: unit tests, and golden tests against the code from before the split

## Getting started

Clone this repo next to `nps_wsva_app`. `app_data_prep.Rmd` writes to
`../nps_wsva_app/app_data`. One Explore notebook reads from that folder and
sources the app's `calc_vulnerability_index.R`.

The Rmds read their inputs from the shared WaVA data folder, which is not in
this repo. Link it into the repo root as `data`:

```bash
ln -s "/path/to/shared/data" data
```

Then, from the repo root in R:

```r
source("setup.R")
```

This installs any missing packages, including
[`npswavacore`](https://github.com/Water-Vulnerability-Analytics/npswavacore),
which provides `get_wsd()`, `get_park_system()`, `get_unit_visitation()` and
`wava_data_path()`. Now run `rapid_data_processing.Rmd` if needed, then
`rapid_workflow.Rmd` and `app_data_prep.Rmd`.

Default paths in `src/` come from `npswavacore::wava_data_path()`, which looks
for the data folder in this order: `options(wava.data_dir = ...)`, the
`WAVA_DATA_DIR` environment variable, then `data` in the working directory.
Every input and output path is also an argument.

Two packages are needed only by `Explore/explore_temperature_visitation_exposure.Rmd`,
and `setup.R` doesn't install them:
[`npswava`](https://github.com/Water-Vulnerability-Analytics/npswava) and
[`climateR`](https://github.com/mikejohnson51/climateR).

## Running the tests

From the repo root:

```bash
Rscript -e 'testthat::test_dir("tests/testthat")'
```

The golden tests skip until you make their files, which aren't in git, and they
take about half an hour. See
[`tests/testthat/fixtures/README.md`](tests/testthat/fixtures/README.md).

## AI Use Disclosure

AI-assisted coding tools (Claude Code, using Anthropic's Claude models) were used
during development of this repository, beginning in 2026, to help set up the
repository, write tests, and draft documentation. All AI-assisted code and text
was reviewed, tested, and validated by the authors, who made all design and
scientific decisions and remain responsible for the content.

## License

MIT. See [LICENSE](LICENSE).
