* This release fixes the segmentation fault reported on
  r-devel-linux-x86_64-fedora-clang and r-devel-linux-x86_64-fedora-gcc.

  `data.preview()` called `utils::View()` unconditionally. On a Unix build
  whose only viewer is the X11 data entry window, with no display reachable,
  that path aborts the session; the clang-ASAN run traces it to a null
  dereference in `initwin` (dataentry.c:1973). The call is now gated on an
  interactive session with a usable viewer and falls back to printing the
  first rows, so the check no longer reaches it.
