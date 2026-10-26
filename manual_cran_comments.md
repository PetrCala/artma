* This patch release fixes the clang-ASAN test failure that CRAN reported
  on 2026-10-13. There are no reverse dependencies.

* The AddressSanitizer report was a `memcpy-param-overlap` in
  `agg::font_cache_pool::font()` (`agg_font_cache_manager.h:176`), which is
  part of 'ragg', not artma's own compiled code. ragg 1.5.2 evicts the
  oldest entry of its process-wide font cache with an overlapping
  `memcpy()` once more than 32 font configurations have been drawn, and a
  test worker that exports many plots goes past that limit.

* The test suite now renders PNG exports with `grDevices::png()` instead of
  'ragg'. The two tests that draw inside forked child processes still use
  'ragg', but the parent never draws with it, so each child's font cache
  stays well below that limit. 'ragg' stays in Suggests and is still
  preferred for exports outside the tests.
