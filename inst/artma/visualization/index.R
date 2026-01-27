#' @title Visualization Module
#' @description
#' Shared utilities for artma visualizations including theming, colors, and export.

box::use(
  artma / visualization / colors[
    VALID_THEMES,
    BACKGROUNDS,
    PALETTES,
    validate_theme,
    get_colors,
    get_background,
    get_vline_color
  ],
  artma / visualization / theme[get_theme],
  artma / visualization / export[
    ensure_export_dir,
    build_export_filename,
    save_plot
  ]
)

box::export(
  VALID_THEMES,
  BACKGROUNDS,
  PALETTES,
  validate_theme,
  get_colors,
  get_background,
  get_vline_color,
  get_theme,
  ensure_export_dir,
  build_export_filename,
  save_plot
)
