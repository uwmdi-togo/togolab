# togolab

Shared utilities for the Bjornstad-Pyle-Tommerdahl (BPT) lab. Resolves
per-user file paths from a config file, sets up access to the lab S3 (Kopah)
store, and bundles helpers the lab reuses across projects.

## Install

```r
# install.packages("remotes")
remotes::install_github("CHCO-Code/bptlab")   # <- update to your repo
```

## Quick start

```r
library(bptlab)

p <- bpt_paths()        # matches your OS username, sets up S3 automatically
p$root_path
p$git_path
p$keys                  # parsed contents of your keys.json
```

That single call replaces sourcing `bpt_usr_paths.R`.

### Drop-in replacement for the old `source()` workflow

If you have existing scripts that expect `root_path`, `git_path`, and `keys`
as plain variables:

```r
bptlab::bpt_paths(assign_globals = TRUE)
# root_path, git_path, keys now exist in your environment
```

## Adding or editing your paths

Paths live in a single YAML file, `inst/config/bpt_paths.yml`. The key for
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

The package ships a copy of `bpt_paths.yml`, but editing that requires a
reinstall. To make path edits take effect immediately, keep the canonical
`bpt_paths.yml` in the lab GitHub repo and point each machine at it once:

```r
# In ~/.Renviron  (run usethis::edit_r_environ() to open it)
BPT_PATHS_CONFIG=/path/to/CHCO-Code/Petter Bjornstad/bpt_paths.yml
```

Resolution order: `config=` argument → `options(bptlab.config=)` →
`BPT_PATHS_CONFIG` env var → the copy shipped in the package.

## Security

Never commit `keys.json` or `.Renviron`. They are in `.gitignore`. The config
only stores the *path* to each person's keys file, never the keys themselves.
```
