# Shared documentation topic for deprecated dotted-name aliases.
#
# The exported API uses snake_case names (options_create, config_set). The
# original dotted names (options.create, config.set) are kept as thin aliases
# that forward to the snake_case functions after a lifecycle deprecation
# warning. Each alias attaches to this topic via @rdname artma-deprecated.
# Policy and rationale: contributingGuides/API.md.

#' @title Deprecated dotted-name aliases
#' @description
#' artma 0.4.0 renamed the exported API from dotted names such as
#' `options.create()` to snake_case names such as `options_create()`. The
#' dotted spellings looked like S3 methods (`print.artma_box_plot()` is a real
#' one) without being methods, so they were retired as primary names.
#'
#' Every dotted function listed here still works: it forwards to its
#' snake_case replacement after emitting a deprecation warning once per
#' session. Replace the dot with an underscore to get the new name. The
#' aliases will be removed after the 1.0 release, except `cli.run()`, which
#' stays indefinitely because installed CLI launchers reference it.
#' @name artma-deprecated
#' @keywords internal
NULL
