<div align="center">
    <h1>
        Automatic Replication Tools for Meta-analysis
    </h1>
    <h4>
    Data in, models out, with ease
    <!-- Effortlessly bridging the gap between data and models -->
    </h4>
</div>

- [Prerequisites](#prerequisites)
- [How to run](#how-to-run)
  - [Using the `run.sh` sript](#using-the-runsh-sript)
    - [Creating an alias](#creating-an-alias)
- [How to install](#how-to-install)
  - [Locally](#locally)
  - [From GitHub](#from-github)
- [Importing modules](#importing-modules)
- [Using options](#using-options)
  - [How options are read and applied](#how-options-are-read-and-applied)
- [Validating Conditions](#validating-conditions)
  - [How to Use the `validate` Function](#how-to-use-the-validate-function)
  - [Examples](#examples)
    - [Valid Conditions](#valid-conditions)
    - [Invalid Conditions](#invalid-conditions)
- [Formatting code](#formatting-code)
- [Understanding the folder structure](#understanding-the-folder-structure)
- [Using `lintr` for Code Quality](#using-lintr-for-code-quality)
  - [Installation](#installation)
  - [Usage](#usage)
  - [Set up box paths](#set-up-box-paths)
  - [Automate Linting (Optional)](#automate-linting-optional)
- [Creating a new package version](#creating-a-new-package-version)

# Prerequisites

To run the analysis, you must have several applications installed on your device. These include:

<!-- - Python: [install here](https://www.python.org/downloads/) -->

- R: [install here](https://cran.r-project.org)

  To verify the installation was successful, run the following commands in your terminal:

  ```bash
  R --version
  ```

  This command should print out the version of the relevant executable. If not, refer to the R installation guides.

- devtools: [install here](https://devtools.r-lib.org)

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
   echo "alias artma='./run.sh'" >>~/.zshrc
   chmod +x run.sh
   artma setup
   ```

1. See the list of available commands by running

   ```bash
   artma help
   ```

<!-- 5. Choose an action to run out of the [Available Actions section](#available-actions). Run it using:

```bash
Rscript run.R <action> [--args]
```

For example, to run the Chris analysis, do

```bash
Rscript run.R analyse Chris
```

6. Find the results in ... -->

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

### Creating an alias

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

# How to install

## Locally

You can use the [\*devtools\*\*](https://devtools.r-lib.org) package to install this project locally as a package. To do so, simply call

```R
devtools::load_all()
```

## From GitHub

To install the package from GitHub, leverage the **remotes** package and call

```R
GH_REPO_PATH <- "PetrCala/artma"
remotes::install_github(GH_REPO_PATH)
```

Feel free to add any other arguments of the `install_github` function to install from different branches, tags, etc.

# Importing modules

For any imports within the project, we use [the **box** package](https://klmr.me/box/articles/box.html). This emulates Python-like module imports, allowing us to maintain a complex, yet transparent structure of the project. Here, each script behaves as a standalone module, and only the necessary functions are imported from it. This keeps the workspace clean, as it does the source of all functions used across the project. To read more on how to use box, see [the official documentation](https://klmr.me/box/articles/box.html).

# Using options

- All options are defined in the `config.yaml` file in the root of the R folder. Upon each script run, these options are loaded into R, validated, and assigned to the global options namespace.

- The options are parsed from the yaml file, and stored so that the levels of the yaml hierarchy create the name under which the option is stored. Further, each option is prefixed by the name of the package. For example:

  ```yaml
  # In `config.yaml`
  general:
    specific:
      option1: "value1"
  ```

  This option would end up being parsed in to `artma.general.specific.option1`. Notice the package name at the front. This is to ensure that the options do not mix with other packages, or R base options.

- Accessing this option in R would then be done as follows:

  ```R
  option_value <- getOption("artma.general.specific.option1") # Stores 'value1'
  ```

  Notice that the **package prefix is necessary when accessing the options**.

## How options are read and applied

You can apply user options through the `load_user_options` function. Here, provide the name of the user options file, and optionally the directory to look for, and the function loads the relevant options.

If no file name is provided, the function instead loads the current options, which are stored under the name `current.yaml`. If no such file exists, the function instead loads the default options, stored under a static template (`options_default.yaml`).

# Validating Conditions

In this project, we use the `validate` function to ensure that certain conditions hold true before proceeding with further computations or operations. The `validate` function helps in maintaining the integrity of the program by aborting execution if any condition is not met. This function is inspired by modern error handling practices in R and leverages the `rlang` package for structured error messages.

## How to Use the `validate` Function

The `validate` function checks whether each argument passed to it is either a single logical value (TRUE or FALSE). It validates each condition and aborts with an appropriate error message if any condition does not hold.

## Examples

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

# Formatting code

We use `styler` for code formatting. See [the package website here](https://github.com/r-lib/styler?tab=readme-ov-file).

Depending on your IDE of choice, the setup for using _styler_ may differ, so we highly recommend you read through the documentation.

# Understanding the folder structure

This package is structured with most files located in the `inst/artma` folder, following the design principles encouraged by the box package. This setup allows for a modular and clean organization of the package's components. By keeping the R directory focused on exported functions and placing the core logic and internal scripts in the `inst/pkgname folder`, the package leverages box's module-based approach to encapsulate functionality. This structure promotes better code reuse, easier debugging, and improved separation of concerns, aligning with modern software development practices.

During the package installation, the `inst` folder gets bundled too, and becomes thus available fox `box` imports.

# Using `lintr` for Code Quality

This project uses the `lintr` package to ensure code quality and adherence to style guidelines. Below are the steps to set up and use `lintr` in this project.

## Installation

First, install the `lintr` package:

```r
install.packages("lintr")
```

## Usage

To lint all R files in your project directory, run the following command:

```r
lintr::lint_dir("path/to/your/project")
```

To lint a specific file, run:

```r
lintr::lint("path/to/your/file.R")
```

## Set up box paths

To make the lints valid for the `box.linters` package, R expects `box.path` to be set to the `inst` folder base. This makes the relative box imports work correctly. During runtime, this is handled by the `ensure_valid_boxpath`, but in development, you must set this path manually.

To do so, put the following into your `.Rprofile`:

```.Rprofile
# ~/.Rprofile
option(box.path="<path-to-the-artma-package>/inst")
```

## Automate Linting (Optional)

You can automate linting using Git pre-commit hooks with the `precommit` package. First, install `precommit`:

```r
install.packages("precommit")
precommit::use_precommit()
```

Edit the `.pre-commit-config.yaml` file to include `lintr`:

```yaml
- repo: https://github.com/jimhester/lintr
  rev: v3.0.0 # Replace with the latest version
  hooks:
    - id: lintr
```

This setup will ensure that your R files are linted before every commit, helping you maintain consistent code quality.

# Creating a new package version

To create a new version of this package, run `./run.sh bump-version <semver-level>`.
