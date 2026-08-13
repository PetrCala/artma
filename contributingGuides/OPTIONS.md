# Options System

Options live in hierarchical YAML files. The template (`inst/artma/options/templates/`) defines every available option with its type, default, and validation rules; user options files are created at runtime and validated against it. When loaded, options sit in the R `options()` namespace prefixed with `artma.`:

```r
conf_level <- getOption("artma.methods.effect_summary_stats.conf_level", 0.95)
opt <- get_option_group("artma.methods.effect_summary_stats")
```

## Template nodes

The template files are self-describing, so copy an existing node when adding an option. End nodes have the following keys:

- **name** (str): option name.
- **type** (str): option type; one of the supported R types (`"character"`, `"logical"`, ...).
- **default** (any, optional): default value for the option.
- **fixed** (bool, optional): if `true`, this option may not be overwritten by the user in any of the derived user files. Any overwritten fixed options are reverted to their default upon file validation.
- **allow_na** (bool, optional): if `true`, this option may be set to `.na`. Otherwise a non-NA value is required when creating the user options file.
- **prompt** (str, optional): how a value for this option should be asked for: `"readline"` (the default), `"file"`, or `"directory"` (the latter two open a `tcltk` window).
- **help** (str, optional): option help text.

## Access convention

Always provide a default matching the template default:

```r
# CORRECT: always provide a default
round_to <- getOption("artma.output.number_of_decimals", 3)

# CORRECT: with get_option_group, use %||% per field
opt <- get_option_group("artma.methods.box_plot")
max_per_plot <- opt$max_boxes_per_plot %||% 60L

# WRONG: returns NULL if the option is not set
round_to <- getOption("artma.output.number_of_decimals")
```

This keeps the package functional when a user's options file predates newly added options.

The exceptions are runtime-populated options (`artma.temp.*`, `artma.data.config`, `artma.data.source_path`), where `NULL` is the expected "not yet set" sentinel; read those with `require_option()` when they must exist.
