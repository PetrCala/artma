#' Interactive CLI Menu System
#'
#' This module provides interactive command-line menus for R,
#' inspired by inquirer.js, Python's pick, and Go's survey libraries.

box::use(
  ./menu[menu],
  ./checkbox[checkbox],
  ./select[select]
)

box::export(
  menu,
  checkbox,
  select
)
