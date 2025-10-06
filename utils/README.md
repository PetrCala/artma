- [Installing R](#installing-r)
- [VSCode](#vscode)
	- [Useful settings](#useful-settings)
- [Aliases](#aliases)
- [Rprofile](#rprofile)
- [Workflows](#workflows)
- [.Renviron](#renviron)
- [Installing various R packages](#installing-various-r-packages)
	- [`systemfonts` on MacOS](#systemfonts-on-macos)
	- [Other packages on MacOS](#other-packages-on-macos)
- [Building the package](#building-the-package)
	- [To exclude a file from the build](#to-exclude-a-file-from-the-build)

## Installing R

- On MacOS, the best way to install R is through the following command:

  ```bash
  brew install --cask r
  ```

  It is quite vital that the _cask_ flag is used, as this will allow R to later install packages through binaries, instead of from source with compilation.

## VSCode

### Useful settings

Here is a list of settings I use in VSCode for better R handling:

```json
// In settings.json
{
	"r.lsp.enabled": true,
	"r.lsp.diagnostics": true,
	"r.lsp.debug": true,
	"r.helpPanel.enableHoverLinks": true,
	"r.plot.defaults.colorTheme": "vscode",
	"r.rterm.option": ["--no-save", "--no-restore"],
	"r.rterm.mac": "/opt/homebrew/bin/R",
	"r.alwaysUseActiveTerminal": true,
	"[r]": {
		"editor.wordWrap": "wordWrapColumn",
		"editor.snippetSuggestions": "top",
		"editor.defaultFormatter": "REditorSupport.r",
		"editor.formatOnSave": true
	}
}
```

## Aliases

Here is a useful alias for faster R invocations:

```bash
alias R="$(/usr/bin/which R) --no-save --no-init-file"
```

## Rprofile

The following .Rprofile setting may help you get around with package installation and loading:

```.Rprofile
options(repos=c(CRAN="https://cran.r-project.org"))
options(pkgType = "source")

suppressWarnings(suppressMessages({
    # Silence a warning in the data.table that causes single-thread processing on Mac
    library(data.table)
    library(rlang)
    # Silence other commonly used packages
    library(utils)
    library(stats)
    library(graphics)
    library(grDevices)
    library(methods)
    library(base)
}))
```

## Workflows

To see example R GitHub workflows, follow [this link](https://github.com/r-lib/actions/blob/v2/examples/README.md).

## .Renviron

To correctly specify installation paths, you may want to add the following to your `~/.Renviron` file:

```env
R_LIBS_USER=/opt/homebrew/lib/R/4.4/site-library
_R_CHECK_USE_CODETOOLS_=FALSE
_R_CHECK_SYSTEM_CLOCK_=0
```

## Installing various R packages

### `systemfonts` on MacOS

Add the following to `~/.R/Makevars` for _systemfonts_ package installation to succeed:

```make
OBJCXXFLAGS += ${CXX17STD}
```

### Other packages on MacOS

For some packages, the clang compiler is set to **gnu23** by default, which runs into compilation issues.

To avoid this, set the compiler to **gnu17** by adding the following line into your `~/.R/Makevars` file:

```make
CFLAGS = -std=gnu17
```

## Building the package

### To exclude a file from the build

Files can be exclude from the package build by adding them to `.Rbuildignore`.

ou can do this manually or by using the function `usethis::use_build_ignore("filename.R")`. The function accepts either a single character string or a character vector containing file or directory names that you want to exclude from the build process.
