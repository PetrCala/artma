# NA

- Resubmission. The 0.4.0 submission was returned because
  `https://petrcala.github.io/artma/`, listed in the DESCRIPTION URL
  field, returned 404. The pkgdown site is now published and that URL
  resolves, so it is kept in DESCRIPTION.

- This release fixes the segmentation fault reported on
  r-devel-linux-x86_64-fedora-clang and r-devel-linux-x86_64-fedora-gcc.

  [`data.preview()`](https://petrcala.github.io/artma/reference/artma-deprecated.md)
  (now
  [`data_preview()`](https://petrcala.github.io/artma/reference/data_preview.md),
  see below) called [`utils::View()`](https://rdrr.io/r/utils/View.html)
  unconditionally. On a Unix build whose only viewer is the X11 data
  entry window, with no display reachable, that path aborts the session;
  the clang-ASAN run traces it to a null dereference in `initwin`
  (dataentry.c:1973). The call is now gated on an interactive session
  with a usable viewer and falls back to printing the first rows, so the
  check no longer reaches it.

- The version jumps from the 0.3.3 currently on CRAN to 0.4.1. Versions
  0.3.5 and 0.4.0 were built and tagged but neither reached CRAN:
  submissions were closed from Aug 5 to Aug 19, 2026 for CRAN team
  vacation and maintenance, and 0.4.0 was returned over the invalid URL
  noted above. This submission therefore carries everything from both,
  including the segmentation fault fix described above.

- The user-facing function names moved to snake_case in this release, so
  [`options.list()`](https://petrcala.github.io/artma/reference/artma-deprecated.md)
  became
  [`options_list()`](https://petrcala.github.io/artma/reference/options_list.md),
  [`cli.run()`](https://petrcala.github.io/artma/reference/artma-deprecated.md)
  became
  [`cli_run()`](https://petrcala.github.io/artma/reference/cli_run.md),
  and so on across the autonomy, cli, config, data, methods, options,
  report, results and viz groups. Every previous name is retained as a
  deprecated alias through the lifecycle package, so existing user code
  keeps working and only emits a deprecation warning. There are no
  reverse dependencies.
