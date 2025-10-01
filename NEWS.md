
<a name="v0.2.1"></a>
## [v0.2.1](https://github.com/PetrCala/artma/compare/v0.2.0...v0.2.1)

> 2025-04-25


<a name="v0.2.0"></a>
## [v0.2.0](https://github.com/PetrCala/artma/compare/v0.1.30...v0.2.0)

> 2025-04-24

### Bug Fixes

* fix the data config top level module import error
* parsing option templates is now done through leaf paths
* remove an unimported function
* add a missing CONST import to the options utils module
* add a missing argument to the main function
* parsing the user options now properly displays help, modify the template to accommodate that
* options now accept enum types

### Code Refactoring

* change the parsing of the options template to allow nested nodes
* change the way the data config parsing function is structured
* the 'assert' function now accepts a 'msg' argument

### Features

* partially enable data preprocessing, clean up cli messages
* add functions for modifying, fixing, and fetching data configurations
* add temporary options for better path awareness
* implement a function to get a data config from the options file
* prompts can now be defined through custom options
* remove the unused data config methods - update through opt files now
* add a runtime method for running linear publication bias tests with bootstrap support

### BREAKING CHANGE


the new templates are now incompatible with old ones


<a name="v0.1.30"></a>
## [v0.1.30](https://github.com/PetrCala/artma/compare/v0.1.29...v0.1.30)

> 2025-04-16

### Bug Fixes

* add a missing import to the data config parsing function
* add a missing export from the string module
* rewrite the data config validation test to use the renamed function

### Code Refactoring

* rename the determine df type function (data_type -> df_type)
* rename the data config file update function to better reflect its purpose
* move the data config write function to a standalone module
* decouple logic in data config writing/parsing
* regex validation now happens against a more robust const object, add tests
* rename the data config filename validation function
* replace the internal 'is_empty' function with the in-built rlang function
* remove the test constant and move the data into CONST.R

### Features

* add the body of data config parsing, add verbose string creation function
* add various utility functions for parsing and writing the data config files
* by default, the options copy now asks for overwrite permission in interactive mode
* add functions for parsing data frames into data configs
* add a function that validates an existence of a data config file
* add a function for validating data config filenames
* add a placeholder folder for data config files handling
* remove the logger package, use 'cli' for all console logging


<a name="v0.1.29"></a>
## [v0.1.29](https://github.com/PetrCala/artma/compare/v0.1.28...v0.1.29)

> 2025-04-15

### Bug Fixes

* the cran submission workflow now parses the correct artifact name


<a name="v0.1.28"></a>
## [v0.1.28](https://github.com/PetrCala/artma/compare/v0.1.27...v0.1.28)

> 2025-04-15


<a name="v0.1.27"></a>
## [v0.1.27](https://github.com/PetrCala/artma/compare/v0.1.26...v0.1.27)

> 2025-04-15


<a name="v0.1.26"></a>
## [v0.1.26](https://github.com/PetrCala/artma/compare/v0.1.25...v0.1.26)

> 2025-04-15


<a name="v0.1.25"></a>
## [v0.1.25](https://github.com/PetrCala/artma/compare/v0.1.24...v0.1.25)

> 2025-04-15

### Bug Fixes

* add a missing newline to the release artifacts text file
* the github workflow artifacts read now uses a more robust code
* the build artifacts read workflow step now reads from a correct file
* the gh release artifacts are now read from a single source of truth file
* attempt to fix the build artifact upload path


<a name="v0.1.24"></a>
## [v0.1.24](https://github.com/PetrCala/artma/compare/v0.1.23...v0.1.24)

> 2025-04-14

### Bug Fixes

* the cran comments build workflow should now target the file correctly


<a name="v0.1.23"></a>
## [v0.1.23](https://github.com/PetrCala/artma/compare/v0.1.22...v0.1.23)

> 2025-04-14

### Bug Fixes

* change the release notes creation script to be more lenient
* the update news action now updates with all the existing GH tags
* replace the invalid slash in the git origin path in the news update workflow
* remove the unused 'replace' argument in mock df creation
* ArtmaBot now authenticates through the deploy key when updating the news file
* the random vector generation now generates floats (was int) from a uniform distribution
* use the official homebrew action to install homebrew
* provide explicit pacakge method assignment when reading data
* remove the homebrew install from the news update workflow
* minor changes in the open PR script
* comment out a PR open automation script irrelevant line
* remove the mishandled 'v' tag
* workflows to use a composite news creation action
* more workflow issues
* secret passing to the build workflow
* the news update secrets passing
* the cran submission workflow cli package call

### Features

* reading the data now validates the data frame as well


<a name="v0.1.22"></a>
## [v0.1.22](https://github.com/PetrCala/artma/compare/v0.1.21...v0.1.22)

> 2025-04-14

### Code Refactoring

* modify the build workflow to release from the CRAN version as opposed to the current one
* the runtime module methods invocation function is now better formatted


<a name="v0.1.21"></a>
## [v0.1.21](https://github.com/PetrCala/artma/compare/v0.1.20...v0.1.21)

> 2025-04-11


<a name="v0.1.20"></a>
## [v0.1.20](https://github.com/PetrCala/artma/compare/v0.1.19...v0.1.20)

> 2025-04-10


<a name="v0.1.19"></a>
## [v0.1.19](https://github.com/PetrCala/artma/compare/v0.1.18...v0.1.19)

> 2025-04-10


<a name="v0.1.18"></a>
## [v0.1.18](https://github.com/PetrCala/artma/compare/v0.1.17...v0.1.18)

> 2025-04-09


<a name="v0.1.17"></a>
## [v0.1.17](https://github.com/PetrCala/artma/compare/v0.1.16...v0.1.17)

> 2025-04-09


<a name="v0.1.16"></a>
## [v0.1.16](https://github.com/PetrCala/artma/compare/v0.1.15...v0.1.16)

> 2025-04-09


<a name="v0.1.15"></a>
## [v0.1.15](https://github.com/PetrCala/artma/compare/v0.1.14...v0.1.15)

> 2025-04-09


<a name="v0.1.14"></a>
## [v0.1.14](https://github.com/PetrCala/artma/compare/v0.1.13...v0.1.14)

> 2025-04-08


<a name="v0.1.13"></a>
## [v0.1.13](https://github.com/PetrCala/artma/compare/v0.1.12...v0.1.13)

> 2025-04-08


<a name="v0.1.12"></a>
## [v0.1.12](https://github.com/PetrCala/artma/compare/v0.1.11...v0.1.12)

> 2025-04-07


<a name="v0.1.11"></a>
## [v0.1.11](https://github.com/PetrCala/artma/compare/v0.1.10...v0.1.11)

> 2025-04-07


<a name="v0.1.10"></a>
## [v0.1.10](https://github.com/PetrCala/artma/compare/v0.1.9...v0.1.10)

> 2025-04-05


<a name="v0.1.9"></a>
## [v0.1.9](https://github.com/PetrCala/artma/compare/v0.1.8...v0.1.9)

> 2025-04-05


<a name="v0.1.8"></a>
## [v0.1.8](https://github.com/PetrCala/artma/compare/v0.1.7...v0.1.8)

> 2025-04-05


<a name="v0.1.7"></a>
## v0.1.7

> 2025-04-04

