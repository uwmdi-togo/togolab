# Building the `togolab` R package from scratch

This guide takes you from an empty folder to a lab-wide R package on GitHub
that any member can install with one line. A complete, working skeleton is in
the `togolab/` folder next to this file — you can either follow along and build
it yourself, or just drop that folder into your repo and skip to Step 5.

---

## 0. One-time setup of your tools

Install the development packages (once per machine):

```r
install.packages(c("devtools", "usethis", "roxygen2", "testthat", "yaml", "jsonlite"))
```

`devtools`/`usethis` automate almost everything below. `roxygen2` turns special
comments above your functions into the help pages and the `NAMESPACE` file.

---

## 1. Understand what an R package actually is

A package is just a folder with a required structure:

```
togolab/
├── DESCRIPTION        # metadata: name, version, dependencies (REQUIRED)
├── NAMESPACE          # what the package exports/imports (auto-generated)
├── LICENSE            # license terms
├── R/                 # all your .R source files with functions
├── man/               # help pages (.Rd) — auto-generated from comments
├── inst/              # arbitrary files installed alongside the package
│   └── config/
│       └── togo_paths.yml   # the user-paths config
├── tests/             # automated tests
└── README.md
```

You never hand-write `NAMESPACE` or `man/` long-term — `roxygen2` generates
them from comments in `R/`. (The skeleton ships pre-generated copies so it
installs immediately, but Step 4 regenerates them.)

---

## 2. Create the skeleton

In R, from the parent directory where you want the package to live:

```r
usethis::create_package("~/GitHub/uwmdi-togo/togolab")
```

This makes the folder, `DESCRIPTION`, `NAMESPACE`, and an `.Rproj`. Then set up
the basics:

```r
usethis::use_mit_license("togo Lab")    # writes LICENSE + LICENSE.md
usethis::use_testthat()                # creates tests/ scaffolding
usethis::use_readme_md()               # creates README.md
```

Edit `DESCRIPTION` to describe the package and declare dependencies. Add the
packages your functions need under `Imports:` (loaded with your package) and
testing-only ones under `Suggests:`:

```r
usethis::use_package("yaml")           # adds to Imports
usethis::use_package("jsonlite")
usethis::use_package("aws.s3", "Suggests")
```

> **Why the path logic becomes a function, not a sourced script.** Sourcing
> `togo_usr_paths.R` dumps variables into the global environment and breaks if
> the file moves. A package function is versioned, documented, testable, and
> callable as `togolab::togo_paths()` from anywhere.

---

## 3. Write the functions

Put `.R` files in `R/`. The key design choice we made: **paths live in an
external YAML file, not hard-coded in the function.** That means when someone's
path changes, you edit a YAML file and commit — no code edit, and (if you use
the `TOGO_PATHS_CONFIG` option in Step 6) no reinstall.

The skeleton splits the code into:

- `R/togo_paths.R` — `togo_config_path()` finds the YAML; `togo_paths()` reads it,
  matches `Sys.info()[["user"]]`, expands `~`, loads `keys.json`, and returns a
  list. This is the direct replacement for your old `if (user == ...)` block.
- `R/aws.R` — `togo_setup_s3()` sets the AWS env vars (called automatically by
  `togo_paths()`); `read_s3_csv()` is an example shared helper.
- `R/utils.R` — small helpers like `%||%`.
- `inst/config/togo_paths.yml` — the per-user path table, one entry per username.

Each function is preceded by a roxygen comment block (`#'`). The `@export` tag
is what makes a function public:

```r
#' Resolve togo lab paths for the current user
#' @param user OS username. Defaults to the current user.
#' @return A list with root_path, git_path, keys, ...
#' @export
togo_paths <- function(user = Sys.info()[["user"]], ...) { ... }
```

Anything you reference from another package inside your code should be either
called fully-qualified (`yaml::read_yaml()`) or imported with
`@importFrom yaml read_yaml`.

To add a new shared lab function later: drop it in a file under `R/`, give it a
roxygen block with `@export`, re-run Step 4, commit. That's the whole workflow.

---

## 4. Generate docs + check the package

```r
devtools::document()    # regenerates NAMESPACE and man/*.Rd from your comments
devtools::load_all()    # loads the package for interactive testing (no install)
devtools::test()        # runs everything in tests/
devtools::check()       # full R CMD check — do this before every release
```

`check()` is the gold standard; aim for "0 errors, 0 warnings". Fix anything it
flags. Test it really works on your machine:

```r
devtools::load_all()
togo_paths()             # should return your paths and configure S3
```

---

## 5. Put it on GitHub

Initialize git and push. This package lives at `uwmdi-togo/togolab`, so create
the repo under the `uwmdi-togo` org (or push to an existing empty one).

```r
usethis::use_git()                          # git init + first commit
usethis::use_github(organisation = "uwmdi-togo")   # creates the remote repo
```

(If you prefer the command line / GitHub web UI, create an empty repo named
`togolab` under uwmdi-togo and `git push` to it — same result.)

**Important — secrets:** the `.gitignore` excludes `keys.json` and `.Renviron`.
The config file only stores the *path* to each person's keys, never the keys.
Double-check nothing sensitive is staged before your first push.

---

## 6. How lab members install and use it

Anyone in the lab now runs, once:

```r
# install.packages("remotes")
remotes::install_github("uwmdi-togo/togolab")
```

Then in any script:

```r
library(togolab)
p <- togo_paths()        # paths for the current user + S3 configured
p$root_path; p$git_path; p$keys
```

Or, to mimic the old sourced-variable style exactly:

```r
togolab::togo_paths(assign_globals = TRUE)   # creates root_path, git_path, keys
```

### Recommended: no-reinstall path edits

So that editing paths doesn't require everyone to reinstall, keep the canonical
`togo_paths.yml` in the lab repo (e.g. in `CHCO-Code/Petter Bjornstad`) and have
each person point at it once in their `~/.Renviron`:

```r
usethis::edit_r_environ()    # opens ~/.Renviron; add the line below, then restart R
```

```
TOGO_PATHS_CONFIG=/full/path/to/CHCO-Code/Petter Bjornstad/togo_paths.yml
```

Now `togo_paths()` reads the live repo file. To change a path, edit that YAML,
`git pull`/`push`, done — no `install_github` needed.

---

## 7. Adding a new person or a new function later

**New person / path:** edit `inst/config/togo_paths.yml` (or the repo copy from
Step 6), add an entry keyed by their `Sys.info()[["user"]]`, commit, push.

**New shared function:**
1. Add it to a file in `R/` with a roxygen block ending in `@export`.
2. `devtools::document()` then `devtools::check()`.
3. Bump the `Version:` in `DESCRIPTION` (e.g. 0.1.0 → 0.1.1).
4. Commit and push. Members re-run `install_github` to get it.

---

## Cheat sheet

| Goal | Command |
|---|---|
| Create skeleton | `usethis::create_package(path)` |
| Add a dependency | `usethis::use_package("pkg")` |
| Regenerate docs + NAMESPACE | `devtools::document()` |
| Load without installing | `devtools::load_all()` |
| Run tests | `devtools::test()` |
| Full validation | `devtools::check()` |
| Publish to GitHub | `usethis::use_github(organisation = "uwmdi-togo")` |
| Install (members) | `remotes::install_github("uwmdi-togo/togolab")` |
