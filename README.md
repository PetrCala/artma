<div align="center">
    <h1>
        Automatic Replication Tools for Meta-analysis
    </h1>
    <h4>
    Effortlessly bridging the gap between data and models
    </h4>

  <!-- badges: start -->

[![R build status](https://github.com/PetrCala/artma/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/PetrCala/artma/actions/workflows/R-CMD-check.yaml)
[![CRAN Status](https://www.r-pkg.org/badges/version/artma)](https://cran.r-project.org/package=artma)
[![codecov](https://codecov.io/gh/PetrCala/artma/graph/badge.svg?token=6XNXVDOT80)](https://codecov.io/gh/PetrCala/artma)

<!-- [![Lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html) -->
<!-- [![Version](https://img.shields.io/github/r-package/v/PetrCala/artma)](https://github.com/PetrCala/artma) -->

  <!-- badges: end -->

</div>

- [How to install](#how-to-install)
  - [From CRAN](#from-cran)
  - [From GitHub](#from-github)
  - [Locally](#locally)
- [Understanding artma](#understanding-artma)
  - [If you are a user](#if-you-are-a-user)
  - [If you are a developer](#if-you-are-a-developer)
- [Using options](#using-options)
  - [Creating an options file](#creating-an-options-file)
  - [Loading an options file](#loading-an-options-file)
  - [Adding new options](#adding-new-options)
  - [Controlling verbosity](#controlling-verbosity)
- [Using methods](#using-methods)
  - [Using available methods](#using-available-methods)
  - [Defining custom methods](#defining-custom-methods)

# How to install

## From CRAN

You can install `artma` directly from CRAN using

```R
install.packages("artma")
```

## From GitHub

To install the package from GitHub, leverage the [**remotes**](https://cran.r-project.org/web/packages/remotes/index.html) package and call

```R
GH_REPO_PATH <- "PetrCala/artma"
remotes::install_github(GH_REPO_PATH)
```

Note that you can pass a number of arguments to the `install_github` function. This allows you to install from different branches, tags, etc., if you so desire.

## Locally

You can use the [**devtools**](https://devtools.r-lib.org) package to install this project locally as a package. To do so, clone the repository onto your machine as described in [README-dev.md](README-dev.md#how-to-run), navigate to the cloned folder in your R console, and call:

```R
devtools::load_all()

## The folowing should now work
artma::run()
artma::methods.list()
# etc.
```

# Understanding artma

## If you are a user

The package includes several comprehensive vignettes that we highly recommend reading to understand the core philosophy and functionality. These vignettes provide detailed explanations, examples, and best practices for using the package effectively. You can access them through [the standard R documentation](https://cran.r-project.org/web/packages/artma/vignettes/) system or [on our website](https://github.com/PetrCala/artma/).

## If you are a developer

We have a whole [README file](README-dev.md) for developers, so feel free to take a look there.

# Using options

We leverage the in-built R options namespace to define custom runtime options. However, artma provides a vast number of custom options to fit your needs, so we use a custom-tailored solution to ensure your options are easily accessible and do not interfere with other R runtime options.

All artma related options are stored in hierarchical `.yaml` files, stored in a temporary `options` folder. This folder, together with the options files themselves, is created at runtime, such as when invoking `artma::options.create()`.

A user options file holds a hierarchical structure, such as in the following exmample:

```yaml
# In a custom user options .yaml file
general:
  specific:
    option1: "value1"
```

## Creating an options file

If you do not have a user options file at a runtime of an artma runtime method, you will be prompted to create one upon the call to that function, right before that function main body runs.

In case you wish to do so explicitly, you can call `artma::options.create()`.

## Loading an options file

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

## Adding new options

<!-- TODO -->

## Controlling verbosity

The package provides configurable verbosity levels through the options file. You can control how much information is displayed during package operations by setting the `verbose` option in your options file. The verbosity levels are:

| Verbosity Level | Description                                         |
| --------------- | --------------------------------------------------- |
| 1               | Errors only - only stop() conditions                |
| 2               | Warnings + errors - warning() and stop() conditions |
| 3               | Info - short progress/high-level info (default)     |
| 4               | Debug/trace - everything including internals        |

The verbosity level is controlled by an integer value, with higher numbers indicating more verbose output. The default level is 3.

To change the verbosity level, simply modify the `verbose` value in your options file. For example, to set minimal verbosity (only errors), you would set:

```yaml
verbose: 1
```

The verbosity level can be set differently for each options file, allowing you to have different levels of detail for different analyses or workflows.

Remember that you can set the option value using the following call:

```R
artma::options.modify(
  option_file_name="your_opt_file.yaml",
  list('verbose' = 1)
)
```

To check the current verbosity level, run `getOption('artma.verbose')`.

# Using methods

Methods (or _runtime methods_) is what we recognize as the main executable functionality of the package. In other words, these methods are what the package suppports and recognizes during its runtime.

All of these are defined in `inst/artma/methods`. The contents of this folder, namely its `.R` scripts, are imported during runtime, and loaded as recognized methods. Consequently, the contents folder should ideally be used **exclusively to store the runtime methods**, and nothing else.

## Using available methods

To see what methods are available, you can run `artma::methods.list()`. The output of this function should mirror the contents of the `methods` folder.

## Defining custom methods

If you wish to use a custom method in the artma package, it should be enough to add it to the `methods` folder. However, it must adhere to several principles in order to be parsed corretly:

- Each method (module) recognized by artma must have a `run` function. This serves as the entrypoint for the method. The function **must accept the several specific parameters**, common across all runtime methods. To see these, open the definition of any of the existing methods and search for non-default parameters. These are the ones you have to use in every case.

If you wish to use any custom parameters for your function, you can define them through options. To understand how to do so, see the [Adding new options section](#adding-new-options).
