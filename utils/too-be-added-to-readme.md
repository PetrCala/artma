# Overview

This is a list of concepts used around the project the user should know, but that have not been put into the main README file yet.

## List

- By default, the user should have exactly one dataset per one user options file. To run runtime methods over multiple configurations, the user can either create multiple options files, or we might later add a method that will allow looping with only some options modified (such as different priors, ticks,...).

## Template parsing paradigms

- If an option is fixed, use the default
- If a user input is provided, use that input
- If an option template definition has a default, use that default. If the option has the 'confirm_default' key set to `true`, ask for confirmation and allow selection of other values.
- Check the type of prompt for that option and ask for a value using that prompt type

### Other useful info

- The package may provide option presets, which could be defined as a list of options recommended to use for the package. No preset <=> default preset, where the user is asked for minimum required information. Possibly this is something to ask before creating the options file - how much customization would you like? Meaning what type of preset to use.
