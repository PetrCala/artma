source("base/paths.R")
source("base/const.R")
source("libs/utils.R")

is_special_key <- function(node_name) node_name %in% CONST$CONFIG_SPECIAL_KEYS

#' Extract special keys from anode
#'
#' @param node [list] A list to extract special keys from
#' @return [list] A list with two elements: keys and node. The node element is the original node with the special keys removed. The keys element is a list with the special keys.
extract_special_keys <- function(node) {
  stopifnot(is.list(node))
  keys_ <- list()
  nodes_ <- list()
  names_ <- names(node)
  for (i in seq_along(node)) {
    node_name <- names_[[i]]
    if (is_special_key(node_name)) {
      keys_[[node_name]] <- node[[i]]
    } else {
      nodes_[[node_name]] <- node[[i]]
    }
  }
  return(list(
    keys = keys_,
    nodes = nodes_
  ))
}

#' Parse a single node of the source config file
#'
#' @param node [list] The node to parse
#' @param node_name [character] The name of the node
#' @return [list] The parsed node
parse_node <- function(node, node_name) {
  if (!is.list(node)) rlang::abort(paste(node_name, "must be a list"))
  paste("Parsing node", node_name)
  if (is_special_key(node_name)) rlang::abort("Should have been parsed before")

  if (length(node) == 0) {
    stop(paste("Empty node", node_name))
  }

  sk_list <- extract_special_keys(node)
  special_keys <- sk_list$keys # Special keys of this node
  child_nodes <- sk_list$nodes # Node without special keys

  if (length(child_nodes) == 0) { # Final node
    stopifnot(length(special_keys) != 0)
    if ("default" %in% names(special_keys)) {
      return(special_keys$default)
    }
    if (!is.null(special_keys$optional) && special_keys$optional) {
      return(NULL) # Only when the optional is truthy
    }
    msg <- paste0(node_name, ": ")
    val <- readline(msg)
    return(val)
  }

  nullable_lapply(child_nodes, function(i, child_node) {
    parse_node(child_node, names(child_nodes)[[i]])
  })
}

#' Parse a source config file and return it as a list
#'
#' @return [list] A list with the parsed source config
parse_src_config <- function() {
  src_config_path <- PATHS$R_CONFIG_SRC
  print(paste("Parsing source config", src_config_path))
  src <- yaml::read_yaml(file = src_config_path)
  parse_node(src, "root") # The key name is arbitrary (can't be a special key)
}

#' Create a new setup file
#' @description
#' This function reads the source config file and creates a new setup file based on the source config file.
#'
#' @return [character] The name of the new setup file
#' @export
create_new_setup_file <- function() {
  parsed_src_config <- parse_src_config()
  new_file_name <- parsed_src_config$headers$source_file
  print(paste("Writing", new_file_name))
  yaml::write_yaml(parsed_src_config, file = new_file_name)
  return(new_file_name)
}
