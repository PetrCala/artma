<div align="center">
    <h1>
        Automatic Replication Tools for Meta-analysis
    </h1>
    <h4>
    Developer Documentation
    </h4>
</div>

- [How to run](#how-to-run)
  - [Using the `run.sh` sript](#using-the-runsh-sript)
  - [Creating an alias](#creating-an-alias)
- [Required packages](#required-packages)
  - [Runtime Dependencies (`Imports`)](#runtime-dependencies-imports)
  - [Development Dependencies (`Suggests`)](#development-dependencies-suggests)
- [Importing modules](#importing-modules)
- [Validating Conditions](#validating-conditions)
  - [Using the `validate` Function](#using-the-validate-function)
  - [Examples using the validate function](#examples-using-the-validate-function)
    - [Valid Conditions](#valid-conditions)
    - [Invalid Conditions](#invalid-conditions)
  - [Using the `assert` Function](#using-the-assert-function)
  - [Examples using the assert function](#examples-using-the-assert-function)
- [Formatting code](#formatting-code)
- [Understanding the folder structure](#understanding-the-folder-structure)
- [Using the options template](#using-the-options-template)
- [Caching heavy computations](#caching-heavy-computations)
  - [Wrapping functions with cache\_cli](#wrapping-functions-with-cache_cli)
  - [Inspecting cached artifacts](#inspecting-cached-artifacts)
  - [Invalidation and configuration](#invalidation-and-configuration)
- [Using `lintr` for Code Quality](#using-lintr-for-code-quality)
  - [Installation](#installation)
  - [Usage](#usage)
  - [Set up box paths](#set-up-box-paths)
- [Docstrings and documentation](#docstrings-and-documentation)
  - [Annotating function argument types](#annotating-function-argument-types)
- [Creating a new package version](#creating-a-new-package-version)
- [Generating package news](#generating-package-news)
- [Formatting commits](#formatting-commits)
  - [Commit lints](#commit-lints)
- [Running tests](#running-tests)
- [Code of Conduct](#code-of-conduct)

Welcome to the developer documentation for artma (Automatic Replication Tools for Meta-analysis). This guide covers setup, development workflows, code standards, and other technical details needed for contributing to the project.

# How to run

1. Clone the repository using

   ```bash
   git clone https://github.com/PetrCala/artma.git
   ```

1. Navigate to the project root

   ```bash
   cd artma
   ```

1. Set up the local environment by executing

   ```bash
   ./run.sh setup
   ```

1. See the list of available commands by running

   ```bash
   ./run.sh help
   ```

## Using the `run.sh` sript

All major actions are ran using the `run.sh` script at the project root. To execute an actions, simply run

```bash
./run.sh <action-name>
```

in your terminal.

For example, `./run.sh test` will run all tests in the project.

If you encounter an error saying that the file is not executable, run

```bash
chmod +x run.sh
```

to make it so.

## Creating an alias

You can streamline the invocation of actions even further by creating a terminal alias. For this, open your `.bashrc` file on linux-based systems, or your `.zshrc` file on macOS-based systems, or the `.bash-profile` on Windows based systems. There, add the following line

```bash
# In your terminal profile file
alias artma='./run.sh' # Or specify the full path to make this even more reliable
```

Then run

```bash
source .bashrc # or .zshrc or .bash-profile
```

to make these changes take effect. Now, you can run the action commands using

```bash
artma test
artma lint
# etc.
```

# Required packages

Below is a list of required packages and reasoning for why they are needed.

## Runtime Dependencies (`Imports`)

These packages are required for the package to function correctly when used by others.

- `cli` – For styled and structured console output (messages, warnings, etc.)
- `digest` – For stable cache key generation.
- `glue` – For efficient and readable string interpolation.
- `lintr` – Runtime linting hooks that surface style issues inside the package.
- `memoise` – For caching heavy computations and replayable console logs.
- `metafor` – Meta-analytic estimators and diagnostics.
- `purrr` – Vector preprocessing utilities.
- `rlang` – Modern condition handling and tidy evaluation helpers.
- `stringr` – Consistent string manipulation functions.
- `usethis` – Automates development-time file generation that is exposed to users.
- `withr` – Temporarily change global state (e.g., options, env vars) within a controlled context.
- `yaml` – Parsing and reading `.yaml` configuration files.

## Development Dependencies (`Suggests`)

These packages are used for development only.

- `box` – Package and module handling.
- `box.linters` – Box-specific linting support.
- `covr` – Code coverage reporting.
- `devtools` – Package development tools (e.g., `load_all()`, `check()`, `test()`).
- `fs` – File system handling with a consistent API.
- `here` – Reliable file path construction within a project.
- `knitr` – Dynamic report generation, especially for vignettes or R Markdown.
- `languageserver` – Provides LSP support for IDE features like autocomplete and linting.
- `mathjaxr` – MathJax support for documentation previews (e.g., vignettes).
- `optparse` – Command line argument parsing for scripts.
- `pkgbuild` – Tools for building R packages.
- `remotes` – Install packages from GitHub or other remote sources.
- `rex` – Human-readable regular expression construction (useful in testing/linting tools).
- `rmarkdown` – Rendering and previewing Markdown-based reports.
- `roxygen2` – Inline documentation generation.
- `testthat` – Unit testing framework.

### Dependency versioning policy

The `DESCRIPTION` file lists the **minimum** versions that guarantee the
features we rely on rather than pinning dependencies to an exact release.
Locking to a single version is discouraged for R packages because:

1. CRAN and most user installations resolve dependencies from the most recent
   release. Requiring an exact version would force users to manually locate and
   install archived tarballs whenever upstream publishes a patch release.
2. Continuous integration environments (including CRAN checks) always install
   the latest dependency versions. Exact pins would therefore cause installation
   failures the moment a dependency increments, blocking releases and automated
   QA.
3. Upstream security and bug fixes would be missed until the package is
   republished with refreshed pins, which is the opposite of reproducibility.

Instead, we track the smallest version that exposes the APIs we call so that the
package stays installable while still guaranteeing the required functionality.

# Importing modules

For any imports within the project, we use [the **box** package](https://klmr.me/box/articles/box.html). This emulates Python-like module imports, allowing us to maintain a complex, yet transparent structure of the project. Here, each script behaves as a standalone module, and only the necessary functions are imported from it. This keeps the workspace clean, as it does the source of all functions used across the project. To read more on how to use box, see [the official documentation](https://klmr.me/box/articles/box.html).

# Validating Conditions

In this project, we use several custom validation function to ensure that certain conditions hold true before proceeding with further computations or operations. These help catch errors as early as possible. Inspired by modern error handling practices in R, we leverage the `rlang` package for structured error messages.

## Using the `validate` Function

To quickly check that a condition is met, use the `validate` function. This function checks whether each argument passed to it is either a single logical value (TRUE or FALSE). It validates each condition and aborts with an appropriate error message if any condition does not hold. In case of validating an object type (such as through `is.character`, `is.logical`, etc.), the function prints a verbose message to the user.

## Examples using the validate function

### Valid Conditions

```r
validate(TRUE, 1 == 1, is.function(print))
```

### Invalid Conditions

The following examples will abort with an error message:

```r
validate(FALSE)
validate(TRUE, 1 == 2, FALSE)
validate("not a condition")
```

## Using the `assert` Function

To check that a condition is met, and print a custom verbose message at the same time, use the `assert` function. This works similarly to the assert functions in other languages, such as [Python](https://www.w3schools.com/python/ref_keyword_assert.asp).

## Examples using the assert function

```r
# The following pass
assert(TRUE, "This condition is TRUE")
assert(x == 1, "'x' should be equal to 1") # Passes if x is equal to 1
assert(grep('word$', 'a string that ends in a custom word'), "The string should end with 'word'")

# The following fail with an error message
assert(FALSE, "This error message will be printed")
assert(x == 1, "'x' should be equal to 1") # Fails if x is not equal to 1
```

# Formatting code

We use `styler` for code formatting. See [the package website here](https://github.com/r-lib/styler?tab=readme-ov-file).

Depending on your IDE of choice, the setup for using _styler_ may differ, so we highly recommend you read through the documentation.

# Understanding the folder structure

This package is structured with most files located in the `inst/artma` folder, following the design principles encouraged by the box package. This setup allows for a modular and clean organization of the package's components. By keeping the R directory focused on exported functions and placing the core logic and internal scripts in the `inst/pkgname folder`, the package leverages box's module-based approach to encapsulate functionality. This structure promotes better code reuse, easier debugging, and improved separation of concerns, aligning with modern software development practices.

During the package installation, the `inst` folder gets bundled too, and becomes thus available fox `box` imports.

# Using the options template

User options are generated in the project from a template file (potentially `options_template.yaml`). This is a nested yaml file, where the end nodes have the following keys:

- **name** (str): Option name.
- **type** (str): Option type. Must be one of the supported R types (such as `"character"`, `"logical"`,...)
- **default** (any, optional): Default value for the option.
- **fixed** (bool, optional): If `true`, this option may not be overwritten by the user in any of the derived user files. Any overwritten fixed options will be reverted to their default upon file validation.
- **allow_na** (bool, optional): If `true`, this option may be set to `.na`. Otherwise a non-na value will be required when creating the user options file.
- **prompt** (character, optional): Specifies how a value for this option should be asked for. Accepts the following values:
  - **"readline"** (default behavior): The user will be prompted through `readline()` with a simple message.
  - **"file"**: The user will be asked to provide a file path through `tcltk` interactive window.
  - **"directory"**: The user will be asked to provide a directory path through `tcltk` interactive window.
- **help** (str, optional): Option help.

# Caching heavy computations

Artma's heavy modelling helpers often emit rich CLI output (alerts, tables,
and progress messages). The `artma::cache_cli()` wrapper memoises both the
return value _and_ the console story so repeated runs feel identical to the
first execution while avoiding expensive recomputation.

## Wrapping functions with cache_cli

Wrap any expensive function with `cache_cli()` when exporting it from a
module. The wrapper records CLI output as the function runs and stores the
resulting `cached_artifact` object on disk using `memoise`.

```r
# inside a module
run_models <- cache_cli(
  .run_models_impl,
  extra_keys = list(pkg_version = utils::packageVersion("artma"))
)

# at call site
run_models(df, formula)
```

Key behaviours:

- CLI output still prints on the first (cold) execution; cached runs replay
  the recorded log so the console experience matches the original run.
- Provide a custom `cache` (e.g. an in-memory cache) if the default user cache
  directory is not suitable.
- `extra_keys` lets you add memoisation key components such as package
  versions or configuration hashes to avoid sharing stale results.

## Inspecting cached artifacts

`cache_cli()` stores a `cached_artifact` containing the computed value, the
replay log, and metadata (timestamp, session info, and cache settings). Use
`get_artifact()` to retrieve artefacts by key and `replay_log()` to reprint
previous CLI output when debugging.

```r
cache <- memoise::cache_filesystem(PATHS$DIR_USR_CACHE)
key <- cache$keys()[[1]]
artifact <- get_artifact(cache, key)

artifact$value          # original return value
replay_log(artifact$log) # emit the stored CLI story
```

The log stores both `cli_message` events and direct `cli::cat_*()` helper
calls, so complex nested output is reproduced faithfully.

## Invalidation and configuration

Several mechanisms keep cached artefacts fresh:

- `invalidate_fun` (optional) receives the call arguments and should return
  `TRUE` when the cache must be bypassed (e.g. negative inputs). When triggered
  the memoised store is cleared before recomputing so subsequent calls rebuild
  fresh artefacts.
- `max_age` enforces a time-to-live in seconds. Set it explicitly when
  wrapping a function or globally via the `artma.cache.max_age` option.
- Disable caching entirely with `options(artma.cache.use_cache = FALSE)` when
  debugging or benchmarking; the original function is returned untouched.

Combine `invalidate_fun`, `max_age`, and `extra_keys` to model domain-specific
refresh rules without manually clearing the cache.

# Using `lintr` for Code Quality

This project uses the `lintr` package to ensure code quality and adherence to style guidelines. Below are the steps to set up and use `lintr` in this project.

## Installation

First, install the `lintr` package:

```r
install.packages("lintr")
```

This package is also automatically installed through `./run.sh setup`.

## Usage

To lint all R files in your project directory, run the following command in R:

```r
lintr::lint_dir("path/to/your/project")
```

To lint a specific file, run the following in R:

```r
lintr::lint("path/to/your/file.R")
```

To lint the whole package, run the following **in a shell terminal**:

```bash
./run.sh lint
```

## Set up box paths

To make the lints valid for the `box.linters` package, R expects `box.path` to be set to the `inst` folder base. This makes the relative box imports work correctly. During runtime, this is handled by the `ensure_valid_boxpath`, but in development, you must set this path manually.

To do so, put the following into your `.Rprofile`:

```.Rprofile
# ~/.Rprofile
option(box.path="<path-to-the-artma-package>/inst")
```

# Docstrings and documentation

## Annotating function argument types

To denote the expected type of a function argument, use the following syntax in the function docstring:

```R
#' @param some_arg *\[character, optional\]* This argument does the following. Defaults to `NULL`.
```

# Creating a new package version

A new version of the package can be created upon merging a pull request to the master branch with the tag `release:new-version`. For details, read through the [release cycle vignette](https://cran.r-project.org/web/packages/artma/vignettes/release-cycle.html).

# Generating package news

We use [**git-chglog**](https://github.com/git-chglog/git-chglog) to automatically update the `NEWS.md` file from the commit history upon a new version creation. This is done automatically within the build and tag deploy cycle.

# Formatting commits

Commits should follow the [conventional commit format](https://www.conventionalcommits.org/en/v1.0.0/), using specific prefixes to indicate the type of change. Common prefixes include:

- `build:` for changes that affect the build system or external dependencies
- `chore:` for maintenance tasks, dependency updates, etc.
- `ci:` for changes to CI configuration files and scripts
- `docs:` for documentation changes
- `feat:` for new features
- `fix:` for bug fixes
- `perf:` for performance improvements
- `refactor:` for code restructuring without behavior changes
- `revert:` for reverting previous commits
- `style:` for formatting changes that don't affect code behavior
- `test:` for adding or modifying tests

The commit message should have a clear, concise subject line following the format `<type>: <description>`. For example: `feat: add support for custom linting rules`. If needed, add a more detailed description in the commit body, separated from the subject by a blank line. Breaking changes should be noted with a `BREAKING CHANGE:` footer.

## Commit lints

To ensure consistent commit wording across the project, we use a [**commit lint workflow**](.github/workflows/commit-lint.yaml). Here, we follow the `@commitlint/config-conventional` set of lint rules for commit formatting. If any of your commits do not adhere to these rules, your pull request will be rejected.

To see a full list of the commit lint rules, visit [this link](https://github.com/conventional-changelog/commitlint/tree/master/%40commitlint/config-conventional).

You may feel like these requirements are a little too strict, but keeping a unified commit message format **allows us to automate the package release cycle**. To be more specific, we construct the package changelog automatically from the commit history upon every new release. As such, a standardized formatting is required.

# Running tests

Tests can be run in several ways depending on your needs:

1. **Run all tests**:

   ```bash
   ./run.sh test
   ```

   This will execute all test files in the project.

1. **Run a specific test file**:

   ```bash
   ./run.sh test --file path/to/test_file.R
   ```

   Replace `path/to/test_file.R` with the relative path to your test file from the `tests` directory.

1. **Run tests with a filter**:

   ```bash
   ./run.sh test --filter "test_name"
   ```

   This will run only the tests that match the specified filter pattern.

The test script uses `devtools::test()` under the hood, which means you can also run tests directly from R:

```r
# Run all tests
devtools::test()

# Run a specific test file
devtools::test_active_file("tests/testthat/test_file.R")

# Run tests with a filter
devtools::test(filter = "test_name")
```

For more information about writing and organizing tests, refer to the [testthat documentation](https://testthat.r-lib.org/).

# Code of Conduct

Please note that the artma project is released with a [Contributor Code of Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html). By contributing to this project, you agree to abide by its terms.
istor Code of Conduct](<https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html>). By contributing to this project, yFor more information about writing and organizing tests, refer to
