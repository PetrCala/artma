<div align="center">
    <h1>
        Automatic Replication Tools for Meta-analysis
    </h1>
    <h4>
    Effortlessly bridging the gap between data and models
    </h4>

  <!-- badges: start -->

[![R build status](https://github.com/PetrCala/artma/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/PetrCala/artma/actions/workflows/R-CMD-check.yaml)
[![Lifecycle](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html)
[![codecov](https://codecov.io/gh/PetrCala/artma/graph/badge.svg?token=6XNXVDOT80)](https://codecov.io/gh/PetrCala/artma)
[![Version](https://img.shields.io/github/r-package/v/PetrCala/artma)](https://github.com/PetrCala/artma)

  <!-- badges: end -->

<!-- To use CRAN version, once published -->
<!-- [![CRAN
Status](https://www.r-pkg.org/badges/version/artma)](https://cran.r-project.org/package=artma) -->

<!-- example workflows https://github.com/r-lib/actions/blob/v2/examples/README.md -->

</div>

- [For Users](#for-users)
  - [Prerequisites](#prerequisites)
  - [How to install](#how-to-install)
    - [From GitHub](#from-github)
    - [Locally](#locally)
    - [From CRAN](#from-cran)
  - [Using options](#using-options)
    - [Creating an options file](#creating-an-options-file)
    - [Loading an options file](#loading-an-options-file)
    - [Adding new options](#adding-new-options)
  - [Using methods](#using-methods)
    - [Using available methods](#using-available-methods)
    - [Defining custom methods](#defining-custom-methods)
- [For Developers](#for-developers)
  - [How to run](#how-to-run)
    - [Using the `run.sh` sript](#using-the-runsh-sript)
      - [Creating an alias](#creating-an-alias)
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
  - [Using `lintr` for Code Quality](#using-lintr-for-code-quality)
    - [Installation](#installation)
    - [Usage](#usage)
    - [Set up box paths](#set-up-box-paths)
  - [Creating a new package version](#creating-a-new-package-version)
  - [Code of Conduct](#code-of-conduct)

# For Users

## Prerequisites

To run the analysis, you must have several applications installed on your device. These include:

- R: [install here](https://cran.r-project.org)

  To verify the installation was successful, run the following commands in your terminal:

  ```bash
  R --version
  ```

  This command should print out the version of the relevant executable. If not, refer to the R installation guides.

- devtools: [install here](https://devtools.r-lib.org)

## How to install

ARTMA is still under heavy development, so that main suggested method of installation is through [GitHub](https://github.com), as described [in this section](#from-github). We provide a couple of other methods as well, but these may require additional setup.

### From GitHub

To install the package from GitHub, leverage the [**remotes**](https://cran.r-project.org/web/packages/remotes/index.html) package and call

```R
GH_REPO_PATH <- "PetrCala/artma"
remotes::install_github(GH_REPO_PATH)
```

Note that you can pass a number of arguments to the `install_github` function. This allows you to install from different branches, tags, etc., if you so desire.

### Locally

You can use the [**devtools**](https://devtools.r-lib.org) package to install this project locally as a package. To do so, clone the repository onto your machine as described in [this section for developers](#how-to-run), navigate to the cloned folder in your R console, and call:

```R
devtools::load_all()

### The folowing should now work
artma::run()
artma::methods.list()
# etc.
```

### From CRAN

ARTMA is **not yet available as a CRAN package**, but in the future, we expect that calls to `install.packages('artma')` should work.

## Using options

We leverage the in-built R options namespace to define custom runtime options. However, ARTMA provides a vast number of custom options to fit your needs, so we use a custom-tailored solution to ensure your options are easily accessible and do not interfere with other R runtime options.

All ARTMA related options are stored in hierarchical `.yaml` files, stored in a temporary `options` folder. This folder, together with the options files themselves, is created at runtime, such as when invoking `artma::options.create()`.

A user options file holds a hierarchical structure, such as in the following exmample:

```yaml
# In a custom user options .yaml file
general:
  specific:
    option1: "value1"
```

### Creating an options file

If you do not have a user options file at a runtime of an ARTMA runtime method, you will be prompted to create one upon the call to that function, right before that function main body runs.

In case you wish to do so explicitly, you can call `artma::options.create()`.

### Loading an options file

You can load an options file by providing its name to runtime functions that require it, such as `artma::run()`. We explicitly **advice against loading the the options in a persistent manner**, so that different options do not interfere with each other across different sessions or invocations.

For example, providing a name of a custom user options file would look as follows:

```R
artma::run(
  options_file_name="my_custom_options.yaml", # if not provided, you will be prompted to enter the name through the R console
  options_dir="path/to/your/options" # optional, defaults to a temporary folder
)
```

Upon providing the name of the options file to use, this file is read, and the options loaded into the `options()` namespace just for the duration of the invocation of the given function. **For the duration of that function function**, his makes the options available like so:

```R
# Within a function that loads an option file
option_value <- getOption("artma.general.specific.option1") # Stores 'value1'
```

Notice that each option is **automatically prefixed by the name of the package**. This is to ensure that the options do not mix with other packages, or R base options.

### Adding new options

<!-- TODO -->

## Using methods

Methods (or _runtime methods_) is what we recognize as the main executable functionality of the package. In other words, these methods are what the package suppports and recognizes during its runtime.

All of these are defined in `inst/artma/methods`. The contents of this folder, namely its `.R` scripts, are imported during runtime, and loaded as recognized methods. Consequently, the contents folder should ideally be used **exclusively to store the runtime methods**, and nothing else.

### Using available methods

To see what methods are available, you can run `artma::methods.list()`. The output of this function should mirror the contents of the `methods` folder.

### Defining custom methods

If you wish to use a custom method in the ARTMA package, it should be enough to add it to the `methods` folder. However, it must adhere to several principles in order to be parsed corretly:

- Each method (module) recognized by ARTMA must have a `run` function. This serves as the entrypoint for the method. The function **must accept the several specific parameters**, common across all runtime methods. To see these, open the definition of any of the existing methods and search for non-default parameters. These are the ones you have to use in every case.

If you wish to use any custom parameters for your function, you can define them through options. To understand how to do so, see the [Adding new options section](#adding-new-options).

---

# For Developers

## How to run

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

### Using the `run.sh` sript

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

#### Creating an alias

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

## Importing modules

For any imports within the project, we use [the **box** package](https://klmr.me/box/articles/box.html). This emulates Python-like module imports, allowing us to maintain a complex, yet transparent structure of the project. Here, each script behaves as a standalone module, and only the necessary functions are imported from it. This keeps the workspace clean, as it does the source of all functions used across the project. To read more on how to use box, see [the official documentation](https://klmr.me/box/articles/box.html).

## Validating Conditions

In this project, we use several custom validation function to ensure that certain conditions hold true before proceeding with further computations or operations. These help catch errors as early as possible. Inspired by modern error handling practices in R, we leverage the `rlang` package for structured error messages.

### Using the `validate` Function

To quickly check that a condition is met, use the `validate` function. This function checks whether each argument passed to it is either a single logical value (TRUE or FALSE). It validates each condition and aborts with an appropriate error message if any condition does not hold. In case of validating an object type (such as through `is.character`, `is.logical`, etc.), the function prints a verbose message to the user.

#### Examples using the validate function

##### Valid Conditions

```r
validate(TRUE, 1 == 1, is.function(print))
```

##### Invalid Conditions

The following examples will abort with an error message:

```r
validate(FALSE)
validate(TRUE, 1 == 2, FALSE)
validate("not a condition")
```

### Using the `assert` Function

To check that a condition is met, and print a custom verbose message at the same time, use the `assert` function. This works similarly to the assert functions in other languages, such as [Python](https://www.w3schools.com/python/ref_keyword_assert.asp).

#### Examples using the assert function

```r
# The following pass
assert(TRUE, "This condition is TRUE")
assert(x == 1, "'x' should be equal to 1") # Passes if x is equal to 1
assert(grep('word$', 'a string that ends in a custom word'), "The string should end with 'word'")

# The following fail with an error message
assert(FALSE, "This error message will be printed")
assert(x == 1, "'x' should be equal to 1") # Fails if x is not equal to 1
```

## Formatting code

We use `styler` for code formatting. See [the package website here](https://github.com/r-lib/styler?tab=readme-ov-file).

Depending on your IDE of choice, the setup for using _styler_ may differ, so we highly recommend you read through the documentation.

## Understanding the folder structure

This package is structured with most files located in the `inst/artma` folder, following the design principles encouraged by the box package. This setup allows for a modular and clean organization of the package's components. By keeping the R directory focused on exported functions and placing the core logic and internal scripts in the `inst/pkgname folder`, the package leverages box's module-based approach to encapsulate functionality. This structure promotes better code reuse, easier debugging, and improved separation of concerns, aligning with modern software development practices.

During the package installation, the `inst` folder gets bundled too, and becomes thus available fox `box` imports.

## Using the options template

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

## Using `lintr` for Code Quality

This project uses the `lintr` package to ensure code quality and adherence to style guidelines. Below are the steps to set up and use `lintr` in this project.

### Installation

First, install the `lintr` package:

```r
install.packages("lintr")
```

This package is also automatically installed through `./run.sh setup`.

### Usage

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

### Set up box paths

To make the lints valid for the `box.linters` package, R expects `box.path` to be set to the `inst` folder base. This makes the relative box imports work correctly. During runtime, this is handled by the `ensure_valid_boxpath`, but in development, you must set this path manually.

To do so, put the following into your `.Rprofile`:

```.Rprofile
# ~/.Rprofile
option(box.path="<path-to-the-artma-package>/inst")
```

## Creating a new package version

To create a new version of this package, run `./run.sh bump-version <semver-level>`.

## Code of Conduct

Please note that the artma project is released with a [Contributor Code of Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html). By contributing to this project, you agree to abide by its terms.
