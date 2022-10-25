#' Open and write file
#'
#' Adapted from source: https://community.rstudio.com/t/using-rstudioapi-to-create-a-new-file-and-insert-text/41549/3
#'
#' @param path string Name of file or full file path with name of file at end (
#'                    ending with '.R' file suffix)
#' @param text string Text to insert into file
#' @param save_and_close boolean If TRUE, saves and closes file (default). If FALSE,
#'                               file remains open in RStudio.
#'
#' @return
#' @importFrom fs file_create
#' @importFrom rstudioapi getSourceEditorContext navigateToFile insertText documentClose
#' @export
#'
#' @examples
open_and_write <- function(path, text, save_and_close = TRUE){
  fs::file_create(path)
  id <- rstudioapi::getSourceEditorContext()$id
  rstudioapi::navigateToFile(path)

  while(rstudioapi::getSourceEditorContext()$id == id){
    next()
    Sys.sleep(0.1) # slow down the while loop to avoid over-processing & allow focus
  }
  id <- rstudioapi::getSourceEditorContext()$id
  rstudioapi::insertText(c(1L,1L), text, id)
  if (save_and_close) {
    rstudioapi::documentClose(id = id, save = TRUE)
  }
}
