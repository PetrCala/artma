# Caching

`cache_cli()` (`inst/artma/libs/infrastructure/cache.R`) memoises a function on disk via `memoise` while preserving its CLI output: the wrapped function's own output prints on the first (cold) execution; a cache hit emits a single `Using cached results` notice instead, respecting the current verbosity. Caching is on by default (`artma.cache.use_cache`).

## Wrapping functions with `cache_cli`

Wrap an expensive function with `cache_cli()` when exporting it from a module. The wrapper stores the computed value in a `cached_artifact` object on disk using `memoise::cache_filesystem`.

```r
# inside a module
run_models <- cache_cli(
  .run_models_impl,
  extra_keys = list(pkg_version = utils::packageVersion("artma"))
)

# at call site
run_models(df, formula)
```

- Provide a custom `cache` (e.g. an in-memory cache) if the default user cache directory is not suitable.
- `extra_keys` adds memoisation key components such as package versions or configuration hashes. A `stage` entry, when present, names the workflow in the cache hit notice.

## `cache_cli_runner` for reusable wrappers

For modules that share a cache signature or need to compute additional key components before invoking the underlying implementation, prefer `cache_cli_runner()`. It introduces the `cache_signature` argument to the memoised layer and accepts a `key_builder` callback so repeated patterns stay DRY:

```r
box::use(
  artma / libs / infrastructure / cache[cache_cli_runner],
  artma / data / cache_signatures[build_data_cache_signature]
)

run_summary <- cache_cli_runner(
  summary_impl,
  stage = "my_stage",
  key_builder = function(...) build_data_cache_signature()
)

run_summary(df)
```

`cache_cli_runner()` exposes all tuning parameters from `cache_cli()`, so callers can set a custom cache backend, provide invalidation predicates, or specify a `max_age` timeout while keeping call sites concise.

## The cache key

Caching is on by default, so the cache key has to be exhaustive. `memoise` hashes the arguments of the memoised call, which covers the data frame and any upstream `<dependency>_result` the orchestrator injects. Everything else reaches the key through the `cache_signature` built by `build_data_cache_signature()` (`inst/artma/data/cache_signatures.R`):

- the data source path and its modification time;
- the user-authored data config entries;
- the whole user-authored `artma.*` option group, not a per-method subset, so an option read indirectly through a shared helper cannot be missed;
- the installed package version;
- `package_source_fingerprint()`, a hash of every R file under `inst/artma`.

`register_runtime_method()` appends `method_source_hash(stage)` on top, so editing one method file invalidates that method alone. Fingerprints are memoised per session, which matches when a source edit actually takes effect (`box` caches loaded modules for the session too).

When adding a cached workflow, route new inputs through the signature rather than relying on the argument hash alone.

## Invalidation and configuration

- `invalidate_fun` (optional) receives the call arguments and should return `TRUE` when the cache must be bypassed (e.g. negative inputs). When triggered, the memoised store is cleared before recomputing so subsequent calls rebuild fresh artifacts.
- `max_age` enforces a time-to-live in seconds. Set it explicitly when wrapping a function or globally via the `artma.cache.max_age` option. It is a backstop for inputs that change without the signature noticing, such as a data file edited in place with its modification time preserved.
- Disable caching entirely with `options(artma.cache.use_cache = FALSE)` when debugging or benchmarking. The option is read on every call, so toggling it mid-session takes effect immediately.
- Wipe the on-disk cache with `make clear-cache`.

Combine `invalidate_fun`, `max_age`, and `extra_keys` to model domain-specific refresh rules without manually clearing the cache.

## Output files

Graphics are written during method execution, so they are not replayed by a cache hit. Writers (e.g. `save_plot()`) call `record_output_file()` (`inst/artma/libs/infrastructure/output_files.R`); `cache_cli()` stores the recorded paths on the artifact and reruns the implementation when any of them have since disappeared, so a cache hit never reports success with its plots absent. Any new file-writing side effect in a cached path needs the same `record_output_file()` call.

## Inspecting cached artifacts

`cache_cli()` stores a `cached_artifact` containing the computed value and metadata (timestamp, extra key material, and the TTL used). Use `get_artifact()` to retrieve an artifact by key when debugging the on-disk cache.

```r
cache <- memoise::cache_filesystem(PATHS$DIR_USR_CACHE)
key <- cache$keys()[[1]]
artifact <- get_artifact(cache, key)

artifact$value # original return value
artifact$meta  # timestamp, extra keys, and cache settings
```
