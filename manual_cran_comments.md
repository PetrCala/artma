* This patch release fixes the clang-ASAN test failure that CRAN reported
  on 2026-10-13. There are no reverse dependencies.

* The AddressSanitizer report was a `memcpy-param-overlap` in
  `agg::font_cache_pool::font()` (`agg_font_cache_manager.h:176`), which is
  part of 'ragg', not artma's own compiled code. ragg 1.5.2 evicts the
  oldest entry of its process-wide font cache with an overlapping
  `memcpy()` once more than 32 font configurations have been drawn, and a
  test worker that exports many plots goes past that limit.

* The test suite now renders every PNG export with `grDevices::png()`
  instead of 'ragg', so the tests no longer reach that code. 'ragg' stays in
  Suggests and is still preferred for exports outside the tests.
