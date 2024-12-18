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
- [Importing modules](#importing-modules)
- [Using options](#using-options)
- [Validating Conditions](#validating-conditions)
  - [How to Use the `validate` Function](#how-to-use-the-validate-function)
  - [Examples](#examples)
    - [Valid Conditions](#valid-conditions)
    - [Invalid Conditions](#invalid-conditions)
- [Formatting code](#formatting-code)
- [Using `lintr` for Code Quality](#using-lintr-for-code-quality)
  - [Installation](#installation)
  - [Usage](#usage)
  - [Automate Linting (Optional)](#automate-linting-optional)

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

<!-- # Available actions

Here is a list of all the currently supported actions:

- **`analyse`**: Run a full analysis of a dataset.
  - _Args_:
    - Analysis name - e.g., `Chris`,...
  - _Example_:
    - `python main.py analyse Chris`
- **`r`**: Call an R script:
  - _Args_:
    - R action name - e.g., `add`, `subtract`,...
    - Arguments passed to the R action
  - _Example_:
    - `python main.py r add 1 2`

Here is a list of all currently available analyses is:

- `Chris` -->

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
  box::use(base / options[get_option])

  option_value <- get_option("general.specific.option1") # Stores 'value1'
  ```

  Notice that the **package prefix is not needed when accessing the options**.

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
