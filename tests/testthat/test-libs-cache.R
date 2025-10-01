box::use(
  testthat[test_that, expect_s3_class, expect_equal, expect_type, expect_length, expect_identical, expect_match, expect_true]
)

# The fixtures and other package modules must be loaded here separately to avoid linter issues
box::use(artma / testing / fixtures / index[FIXTURES])

test_that("new_artifact constructs the correct S3 object", {
  box::use(artma / libs / cache[new_artifact])

  a <- new_artifact(1:3, list(), list(pkg = "demo"))
  expect_s3_class(a, "cached_artifact")
  expect_equal(a$value, 1:3)
  expect_equal(a$log, list())
  expect_equal(a$meta$pkg, "demo")
})

test_that("capture_cli traps cli conditions and replay_log re-emits them", {
  box::use(artma / libs / cache[capture_cli, replay_log])

  FIXTURES$local_cli_silence()

  res <- capture_cli({
    cli::cli_alert_info("Hello")
    42
  }, emit = FALSE)

  # ----- value --------------------------------------------------------------
  expect_equal(res$value, 42)

  # ----- log handling -------------------------------------------------------
  expect_type(res$log, "list")
  expect_length(res$log, 1L)
  expect_type(res$log[[1]], "list")
  expect_identical(res$log[[1]]$kind, "condition")
  expect_type(res$log[[1]]$cli_type, "character")
  expect_type(res$log[[1]]$args, "list")
  expect_match(res$log[[1]]$message, "Hello", fixed = TRUE)

  # replay should print exactly once
  out <- testthat::capture_messages(replay_log(res$log))
  expect_match(out, "Hello", fixed = TRUE)
})

test_that("capture_cli captures cat helpers and replays them faithfully", {
  box::use(artma / libs / cache[capture_cli, replay_log])

  FIXTURES$local_cli_silence()

  res <- capture_cli({
    cli::cat_rule("demo")
    "done"
  }, emit = FALSE)

  expect_equal(res$value, "done")
  expect_length(res$log, 1L)

  entry <- res$log[[1]]
  expect_identical(entry$kind, "call")
  expect_identical(entry$fun, "cat_rule")
  expect_type(entry$args, "list")
  expect_identical(entry$args[[1]], "demo")

  replayed <- testthat::capture_output(replay_log(res$log))
  original <- testthat::capture_output(cli::cat_rule("demo"))
  expect_identical(replayed, original)
})

test_that("cache_cli memoises value + log and replays on hit", {
  box::use(artma / libs / cache[cache_cli, get_artifact])

  FIXTURES$local_cli_silence()

  # use an *ephemeral* cache so tests are self-contained
  tmp_cache <- memoise::cache_filesystem(withr::local_tempdir())

  cached_modeller <- cache_cli(FIXTURES$fake_modeller, cache = tmp_cache)

  ## --- 1st call: cold ------------------------------------------------------
  first_console <- testthat::capture_messages(
    v1 <- cached_modeller(10)
  )
  expect_equal(v1, 20)

  # cache should now contain exactly one key
  expect_length(tmp_cache$keys(), 1L)

  ## --- 2nd call: warm ------------------------------------------------------
  second_console <- testthat::capture_messages(
    v2 <- cached_modeller(10)
  )
  expect_equal(v2, 20)

  # console chatter must be *identical* because we replayed
  expect_identical(first_console, second_console)

  # still only one artifact stored
  expect_length(tmp_cache$keys(), 1L)

  ## --- inspect the artifact -----------------------------------------------
  key <- tmp_cache$keys()[[1]]
  art <- get_artifact(tmp_cache, key)
  expect_s3_class(art, "cached_artifact")
  expect_length(art$log, 1L)
  expect_identical(art$log[[1]]$kind, "condition")
  expect_match(art$log[[1]]$message, "Running model", fixed = TRUE)
  expect_equal(art$meta$cache$max_age, Inf)
})

test_that("invalidate_fun forces recomputation for selected arguments", {
  box::use(artma / libs / cache[cache_cli])

  FIXTURES$local_cli_silence()

  tmp_cache <- memoise::cache_filesystem(withr::local_tempdir())

  hits <- 0L
  counted_modeller <- function(x) {
    hits <<- hits + 1L
    cli::cli_alert("count {hits}")
    x * 2
  }

  invalidate_when_neg <- function(x) x < 0

  cached_modeller <- cache_cli(counted_modeller,
    invalidate_fun = invalidate_when_neg,
    cache = tmp_cache
  )

  testthat::capture_messages(cached_modeller(1))
  expect_equal(hits, 1L)
  expect_length(tmp_cache$keys(), 1L)

  testthat::capture_messages(cached_modeller(1))
  expect_equal(hits, 1L)

  testthat::capture_messages(cached_modeller(-1))
  expect_equal(hits, 2L)
  expect_length(tmp_cache$keys(), 1L)
})

test_that("print.cached_artifact produces a human-readable summary", {
  FIXTURES$local_cli_silence()

  box::use(artma / libs / cache[new_artifact, print.cached_artifact])

  art <- new_artifact(99, list(), list(note = "demo"))
  out <- testthat::capture_messages(print.cached_artifact(art))

  expect_true(any(grepl("Artifact", out, fixed = TRUE)))
  expect_true(any(grepl("Value:", out, fixed = TRUE)))
  expect_true(any(grepl("Log:", out, fixed = TRUE)))
})

test_that("cache_cli honours max_age to refresh stale artifacts", {
  box::use(artma / libs / cache[cache_cli])

  FIXTURES$local_cli_silence()

  tmp_cache <- memoise::cache_filesystem(withr::local_tempdir())

  hits <- 0L
  tracked <- function(x) {
    hits <<- hits + 1L
    cli::cli_alert_success("call {hits}")
    x + hits
  }

  cached <- cache_cli(tracked, cache = tmp_cache, max_age = 0)

  first <- testthat::capture_messages(cached(5))
  expect_equal(hits, 1L)
  expect_length(tmp_cache$keys(), 1L)

  second <- testthat::capture_messages(cached(5))
  expect_equal(hits, 2L)
  expect_identical(first, second) # recomputation produces the same console story
})

test_that("cache_cli bypasses caching when disabled via option", {
  box::use(artma / libs / cache[cache_cli])

  FIXTURES$local_cli_silence()

  tmp_cache <- memoise::cache_filesystem(withr::local_tempdir())

  hits <- 0L
  tracked <- function(x) {
    hits <<- hits + 1L
    cli::cli_alert_warning("run {hits}")
    x
  }

  withr::with_options(list(artma.cache.use_cache = FALSE), {
    cached <- cache_cli(tracked, cache = tmp_cache)
    testthat::capture_messages(cached(3))
    testthat::capture_messages(cached(3))
  })

  expect_equal(hits, 2L)
  expect_length(tmp_cache$keys(), 0L)
})

test_that("cache_cli_runner injects reusable cache signature metadata", {
  box::use(artma / libs / cache[cache_cli_runner, get_artifact])

  FIXTURES$local_cli_silence()

  tmp_cache <- memoise::cache_memory()
  hits <- 0L
  last_signature <- NULL

  counted <- function(cache_signature = NULL, data) {
    hits <<- hits + 1L
    last_signature <<- cache_signature
    data
  }

  builder <- function(data) list(rows = nrow(data))

  cached <- cache_cli_runner(
    counted,
    stage = "runner_test",
    key_builder = builder,
    cache = tmp_cache
  )

  sample_data <- head(iris)

  cached(sample_data)
  expect_equal(hits, 1L)
  expect_identical(last_signature, list(rows = nrow(sample_data)))
  expect_length(tmp_cache$keys(), 1L)

  cached(sample_data)
  expect_equal(hits, 1L)

  key <- tmp_cache$keys()[[1]]
  art <- get_artifact(tmp_cache, key)
  expect_identical(art$meta$extra$stage, "runner_test")
})

test_that("cache_cli_runner works with implementations that ignore cache_signature", {
  box::use(artma / libs / cache[cache_cli_runner])

  FIXTURES$local_cli_silence()

  tmp_cache <- memoise::cache_memory()
  hits <- 0L

  counted <- function(x) {
    hits <<- hits + 1L
    x * 2
  }

  cached <- cache_cli_runner(
    counted,
    stage = "no_sig",
    key_builder = function(x) list(value = x),
    cache = tmp_cache
  )

  cached(2)
  expect_equal(hits, 1L)
  cached(2)
  expect_equal(hits, 1L)
})

test_that("cache_cli_runner isolates caches across stages", {
  box::use(artma / libs / cache[cache_cli_runner])

  FIXTURES$local_cli_silence()

  shared_cache <- memoise::cache_memory()
  counts <- new.env(parent = emptyenv())
  counts$first <- 0L
  counts$second <- 0L

  make_impl <- function(name) {
    function(df) {
      counts[[name]] <- counts[[name]] + 1L
      sprintf("%s_result", name)
    }
  }

  key_builder <- function(df) list(rows = nrow(df))

  first_runner <- cache_cli_runner(
    make_impl("first"),
    stage = "stage_one",
    key_builder = key_builder,
    cache = shared_cache
  )

  second_runner <- cache_cli_runner(
    make_impl("second"),
    stage = "stage_two",
    key_builder = key_builder,
    cache = shared_cache
  )

  sample_df <- data.frame(x = 1)

  expect_identical(first_runner(sample_df), "first_result")
  expect_identical(second_runner(sample_df), "second_result")
  expect_equal(counts$first, 1L)
  expect_equal(counts$second, 1L)

  # cache hits should not trigger additional executions
  expect_identical(first_runner(sample_df), "first_result")
  expect_identical(second_runner(sample_df), "second_result")
  expect_equal(counts$first, 1L)
  expect_equal(counts$second, 1L)
})
