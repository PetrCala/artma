* Resubmission. The previous submission was returned because
  `https://petrcala.github.io/artma/`, listed in the DESCRIPTION URL field,
  returned 404. The pkgdown site is not published yet, so the URL has been
  dropped from DESCRIPTION (and from the generated `man/artma-package.Rd`).
  It will be added back once the site is live.

* This release fixes the segmentation fault reported on
  r-devel-linux-x86_64-fedora-clang and r-devel-linux-x86_64-fedora-gcc.

  `data.preview()` (now `data_preview()`, see below) called `utils::View()`
  unconditionally. On a Unix build whose only viewer is the X11 data entry
  window, with no display reachable, that path aborts the session; the
  clang-ASAN run traces it to a null dereference in `initwin`
  (dataentry.c:1973). The call is now gated on an interactive session with a
  usable viewer and falls back to printing the first rows, so the check no
  longer reaches it.

* The version jumps from the 0.3.3 currently on CRAN to 0.4.0. Version 0.3.5
  was built and tagged, but it never reached CRAN: submissions were closed
  from Aug 5 to Aug 19, 2026 for CRAN team vacation and maintenance. This
  submission therefore also carries everything that was in 0.3.5, including
  the segmentation fault fix described above.

* The user-facing function names moved to snake_case in this release, so
  `options.list()` became `options_list()`, `cli.run()` became `cli_run()`,
  and so on across the autonomy, cli, config, data, methods, options,
  report, results and viz groups. Every previous name is retained as a
  deprecated alias
  through the lifecycle package, so existing user code keeps working and
  only emits a deprecation warning. There are no reverse dependencies.
