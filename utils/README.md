- [Installing R](#installing-r)
- [VSCode](#vscode)
	- [Invoking the run script](#invoking-the-run-script)
		- [Interactively](#interactively)
		- [Non-interactively](#non-interactively)
	- [Closing an open interactive script](#closing-an-open-interactive-script)
	- [Useful settings](#useful-settings)
- [Aliases](#aliases)
- [Rprofile](#rprofile)
- [.Renviron](#renviron)
- [Installing various R packages](#installing-various-r-packages)
	- [`systemfonts` on MacOS](#systemfonts-on-macos)
- [Building the package](#building-the-package)
	- [To exclude a file from the build](#to-exclude-a-file-from-the-build)

## Installing R

- On MacOS, the best way to install R is through the following command:

  ```bash
  brew install --cask r
  ```

  It is quite vital that the _cask_ flag is used, as this will allow R to later install packages through binaries, instead of from source with compilation.

## VSCode

There are many utilities in VScode that allow a quick execution of the files in the project. Here are but several of them.

### Invoking the run script

To invoke `run.R` quickly from anywhere, you can choose from two options - interactive, and non-interactive. The former should be used for debugging, while the latter is useful when you are certain your code will run smoothly.

#### Interactively

To run the script interactively, you need the **multi-commands** extension. In the _VS Code Extensions_, search for _multi-command_, and install the extension.

In `settings.json`, add the following:

```json
"multiCommand.commands": [
  {
    "command": "multiCommand.runRSource",
    "sequence": [
      "r.createRTerm", // Create an interactive terminal
      {
        "command": "workbench.action.terminal.sendSequence",
        "args": {
          "text": "source('<path-to-the-project>/artma/R/run.R')\u000D"
        }
      }
    ]
  }
]
```

Make sure to modify the `<path-to-the-project>` with the actual file to the `meta-facilitator` folder.

Now, go to `keybindings.json`, and add the following:

```json
{
	"key": "cmd+shift+r", // Or any shortcut of your choice.
	"command": "extension.multiCommand.execute",
	"args": { "command": "multiCommand.runRSource" }
}
```

Pressing the shortcut will now invoke the `run.R` script in an interactive R terminal, enabling functionality such as `browser()`.

#### Non-interactively

In the `.vscode` folder, create a file called `tasks.json` with the following contents:

```json
{
	"version": "2.0.0",
	"tasks": [
		{
			"label": "Run Default R Script Interactively",
			"type": "shell",
			"command": "R",
			"args": ["-e", "\"source('${workspaceFolder}/R/run.R')\""],
			"group": {
				"kind": "build",
				"isDefault": true
			},
			"presentation": {
				"echo": true,
				"reveal": "always",
				"focus": false,
				"panel": "shared"
			},
			"problemMatcher": []
		}
	]
}
```

Then, in `keybindings.json`, add the following keybinding.

```json
{
	"key": "ctrl+shift+r", // or the key binding of your choice
	"command": "workbench.action.tasks.runTask",
	"args": "Run Default R Script Interactively"
}
```

Now, pressing the keybinding (here, `ctrl+shift+r`) will run the R script in a non-interactive window.

### Closing an open interactive script

To close an interactive R terminal, such as when browsing during debugging, you can automate the process of sending commands to the terminal by utilizing a custom keybinding. For example,

```json
// keybindings.json
{
	"key": "cmd+shift+e",
	"command": "workbench.action.terminal.sendSequence",
	"args": {
		"text": "Q\nq()\n"
	},
	"description": "Quit R Debugger and Console",
	"note": "Run only when the active open terminal is an R terminal"
}
```

Pressing `cmd+shift+e` will now send a sequence to the R interactive terminal that will cause it to close. This is useful, for example, when you want to quickly restart the current session when making changes to the code.

_Note: You may even chain the closing/opening of the terminal to create a fully automated reset functionality._

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

Here are a bunch of useful aliases for faster script/R invocations:

```bash
alias artma="./run.sh"
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

## .Renviron

To correctly specify installation paths, you may want to add the following to your `~/.Renviron` file:

```env
R_LIBS_USER=/opt/homebrew/lib/R/4.4/site-library
_R_CHECK_USE_CODETOOLS_=FALSE
```

## Installing various R packages

### `systemfonts` on MacOS

Add the following to `~/.R/Makevars` for _systemfonts_ package installation to succeed:

```make
OBJCXXFLAGS += ${CXX17STD}
```

## Building the package

### To exclude a file from the build

Files can be exclude from the package build by adding them to `.Rbuildignore`.

ou can do this manually or by using the function `usethis::use_build_ignore("filename.R")`. The function accepts either a single character string or a character vector containing file or directory names that you want to exclude from the build process.
