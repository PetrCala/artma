# Deprecated dotted-name aliases

artma 0.4.0 renamed the exported API from dotted names such as
`options.create()` to snake_case names such as
[`options_create()`](https://petrcala.github.io/artma/reference/options_create.md).
The dotted spellings looked like S3 methods
([`print.artma_box_plot()`](https://petrcala.github.io/artma/reference/print.artma_box_plot.md)
is a real one) without being methods, so they were retired as primary
names.

Every dotted function listed here still works: it forwards to its
snake_case replacement after emitting a deprecation warning once per
session. Replace the dot with an underscore to get the new name. The
aliases will be removed after the 1.0 release, except `cli.run()`, which
stays indefinitely because installed CLI launchers reference it.

## Usage

``` r
autonomy.get(...)

autonomy.set(...)

autonomy.is_set(...)

autonomy.is_full(...)

cli.install(...)

cli.run(...)

config.get(...)

config.set(...)

config.reset(...)

config.overrides(...)

config.fix(...)

data.preview(...)

methods.list(...)

options.validate(...)

options.copy(...)

options.delete(...)

options.list(...)

options.diff(...)

options.load(...)

options.modify(...)

options.open(...)

options.help(...)

options.print_default_dir(...)

options.fix(...)

options.create(...)

report.render(...)

results.dir(...)

results.open(...)

viz.get(...)

viz.set(...)

viz.themes(...)
```
