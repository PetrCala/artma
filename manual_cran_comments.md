# NA

- This patch release addresses the two problems that CRAN’s clang
  sanitizer checks reported in third-party compiled code reached from
  artma’s test suite. There are no reverse dependencies.

- The clang-ASAN failure reported on 2026-10-13 was a
  `memcpy-param-overlap` in `agg::font_cache_pool::font()`
  (`agg_font_cache_manager.h:176`), which is part of ‘ragg’, not artma’s
  own compiled code. ragg 1.5.2 evicts the oldest entry of its
  process-wide font cache with an overlapping `memcpy()` once more than
  32 font configurations have been drawn, and a test worker that exports
  many plots goes past that limit. The test suite now renders PNG
  exports with
  [`grDevices::png()`](https://rdrr.io/r/grDevices/png.html) instead of
  ‘ragg’. ‘ragg’ stays in Suggests and is still preferred for exports
  outside the tests.

- The clang-san pre-test of the 0.5.1 submission then reported
  `call to function clear_console through pointer to incorrect function type 'void (*)(struct SEXPREC *)'`
  at `memory.c:1524`. That function belongs to ‘rjags’ (`src/jags.cc`),
  which registers `SEXP clear_console(SEXP)` as a `void (*)(SEXP)` C
  finalizer for its JAGS console pointer; the function-type sanitizer
  flags the mismatch when R runs the finalizer. The one test that fits a
  ‘RoBMA’ model, and so loads ‘rjags’ into the test process, is now
  skipped on CRAN and still runs in the package’s own continuous
  integration. No artma code changed for this.
