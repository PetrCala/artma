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

  local_cli_silence()

  res <- capture_cli({
    cli::cli_alert_info("Hello")
    42
  })

  # ----- value --------------------------------------------------------------
  expect_equal(res$value, 42)

  # ----- log handling -------------------------------------------------------
  expect_type(res$log, "list")
  expect_length(res$log, 1L)
  expect_type(res$log[[1]], "list")
  expect_type(res$log[[1]]$fun, "character")
  expect_type(res$log[[1]]$message, "character")

  # replay should print exactly once
  out <- testthat::capture_messages(replay_log(res$log))
  expect_match(out, "Hello", fixed = TRUE)
})

test_that("cache_cli memoises value + log and replays on hit", {
  box::use(artma / libs / cache[cache_cli, get_artifact])

  local_cli_silence()

  # use an *ephemeral* cache so tests are self-contained
  tmp_cache <- memoise::cache_filesystem(withr::local_tempdir())

  cached_modeller <- cache_cli(fake_modeller, cache = tmp_cache)

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
  expect_match(art$log[[1]]$message, "Running model", fixed = TRUE)
})

test_that("invalidate_fun forces recomputation and new artifact", {
  box::use(artma / libs / cache[cache_cli])

  local_cli_silence()

  tmp_cache <- memoise::cache_filesystem(withr::local_tempdir())

  # Invalidate when x is negative
  invalidate_when_neg <- function(x) x < 0

  cached_modeller <- cache_cli(fake_modeller,
    invalidate_fun = invalidate_when_neg,
    cache = tmp_cache
  )

  cached_modeller(1) # warm it
  keys_before <- tmp_cache$keys()

  cached_modeller(-1) # should bypass cache
  keys_after <- tmp_cache$keys()

  expect_gt(length(keys_after), length(keys_before))
})

test_that("print.cached_artifact produces a human-readable summary", {
  local_cli_silence()

  box::use(artma / libs / cache[new_artifact])

  art <- new_artifact(99, list(), list(note = "demo"))
  out <- testthat::capture_messages(print(art))

  expect_match(out, "Artifact", fixed = TRUE)
  expect_match(out, "Value:", fixed = TRUE)
  expect_match(out, "Log:", fixed = TRUE)
})
