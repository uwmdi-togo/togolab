# togolab

Shared utilities for the Bjornstad-Pyle-Tommerdahl (Togo) lab. Resolves
per-user file paths from a config file, sets up access to the lab S3 (Kopah)
store, loads/collapses the harmonized dataset, and bundles helpers the lab
reuses across projects.

## Install

```r
# install.packages("remotes")
remotes::install_github("uwmdi-togo/togolab")
```

## Quick start

```r
library(togolab)

p <- togo_paths()        # matches your OS username, sets up S3 automatically
p$root_path
p$git_path
p$keys                  # parsed contents of your keys.json

# Load + collapse the harmonized dataset to one row per record_id x visit:
dat <- togo_load_harmonized()
```

### Drop-in replacement for the old `source()` workflow

If you have existing scripts that expect `root_path`, `git_path`, and `keys`
as plain variables:

```r
togolab::togo_paths(assign_globals = TRUE)
# root_path, git_path, keys now exist in your environment
```

## Harmonized dataset

`togo_load_harmonized()` reads
`<root_path>/Data Harmonization/Data Clean/harmonized_dataset.csv`
(with `na.strings = ""`) and, by default, collapses it to one row per group.

```r
togo_load_harmonized()                          # collapsed: char = last non-NA, numeric = mean
togo_load_harmonized(summarize = FALSE)         # raw rows, no collapsing
togo_load_harmonized(num_fun = "median")        # numeric columns -> median
togo_load_harmonized(by = "record_id")          # group by record_id only
togo_load_harmonized(char_fun = "first", num_fun = "max")
```

`num_fun` accepts `mean`, `median`, `first`, `last`, `max`, `min`, `sum`;
`char_fun` accepts `last`, `first`, `max`, `min`. The grouping is set by `by`
(default `c("record_id", "visit")`). `togo_collapse()` applies the same logic
to any data frame you already have in memory.

## Adding or editing your paths

Paths live in a single YAML file, `inst/config/togo_paths.yml`. The key for
each person is their OS username (`Sys.info()[["user"]]`). Edit **only your own
entry**, then commit and push.

```yaml
users:
  yourusername:
    root_path: "/path/to/data"
    git_path:  "/path/to/CHCO-Code/Petter Bjornstad"
    keys:      "/path/to/keys.json"
```

Work on more than one machine (e.g. laptop + Hyak)? Add one entry per machine,
each keyed by that machine's username.

## No-reinstall config (recommended for the lab)

The package ships a copy of `togo_paths.yml`, but editing that requires a
reinstall. To make path edits take effect immediately, keep the canonical
`togo_paths.yml` in the lab GitHub repo and point each machine at it once:

```r
# In ~/.Renviron  (run usethis::edit_r_environ() to open it)
TOGO_PATHS_CONFIG=/path/to/CHCO-Code/Petter Bjornstad/togo_paths.yml
```

Resolution order: `config=` argument → `options(togolab.config=)` →
`TOGO_PATHS_CONFIG` env var → the copy shipped in the package.

## Analysis & plotting helpers

Generalized from the lab's `attempt_functions.R`, these work for both the
ATTEMPT (treatment x visit) and PB90 (disease-group) designs. Heavy packages
(Seurat, nebula, fgsea, slingshot, ggplot2, ggrepel, ggtext, …) are **Suggests**:
they're only needed when you call a function that uses them, and each function
errors with a clear install message if its package is missing.

Palettes & theme: `togo_pal_disease`, `togo_pal_treatment`, `togo_colors_5/9`,
`togo_scale_color_disease()` / `_fill_` / `_treatment` scales, and
`theme_togo_transparent()`.

scRNA: `togo_make_subsets()`, `togo_run_doubletfinder()`, `togo_run_nebula()`
(single formula-driven NEBULA fit), `togo_run_nebula_parallel()`,
`togo_process_nebula_results()`.

UMAP & composition: `togo_prepare_umap_metadata()`, `togo_plot_feature_umap()`,
`togo_celltype_proportions()`, `togo_celltype_pie()`.

Volcano & pathways: `togo_plot_volcano()`, `togo_prepare_gmt()`,
`togo_matrix_to_list()`, `togo_clean_pathway_names()`,
`togo_filter_redundant_pathways()`, `togo_plot_fgsea()`.

Trajectory: `togo_slingshot_setup()`, `togo_run_slingshot()`.

S3: `togo_s3_read_rds()`, `togo_s3_save_plot()` (plus `read_s3_csv()`).

Utilities: `togo_unregister_dopar()`, `togo_get_legend()`, `%||%`.

> Not ported (too ATTEMPT-specific to generalize): the SomaScan/limma
> proteomics functions and the PRE/POST-by-treatment pseudotime *plots*. Ask if
> you want generalized versions.

After adding/editing functions, run `devtools::document()` to regenerate the
`man/` help pages and `NAMESPACE` from the roxygen comments.

## Continuous integration

`.github/workflows/R-CMD-check.yaml` runs `R CMD check` on macOS and Ubuntu for
every push and pull request, so broken changes are caught automatically.

## Security

Never commit `keys.json` or `.Renviron`. They are in `.gitignore`. The config
only stores the *path* to each person's keys file, never the keys themselves.
```
