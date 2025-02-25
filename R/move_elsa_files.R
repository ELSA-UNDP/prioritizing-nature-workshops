#' Move Specified ELSA Files from Source to Destination
#'
#' This function moves specified files from a source directory to a destination directory 
#' by matching filenames listed in `feat_df`. It checks if each file exists in the source 
#' directory before attempting to copy it and provides informative messages about each operation.
#'
#' @param feat_df A data frame containing a column `feat_name` with filenames (e.g., "filename.tif") 
#'        that need to be copied. This can be used directly from, and defaults to, the `feats_df` output of `pre_global.R`.
#' @param source_dir A character string specifying the source directory path where files are located.
#' @param dest_dir A character string specifying the destination directory path where files will be copied.
#'        Defaults to `"data/elsa_inputs/"`.
#'
#' @return NULL. The function performs file copying as a side effect and provides messages on success 
#'         or warnings if a file is not found.
#' @export
#'
#' @examples
#' # Example usage
#' source_dir <- "~/source_directory_path"
#' move_elsa_files(feat_df, source_dir)
move_elsa_files <- function(feat_df = feat_df, attr = "feat_name", source_dir, dest_dir = "data/elsa_inputs/") {
  
  # Check if the specified attribute exists in feat_df
  if (!attr %in% names(feat_df)) {
    stop(paste0("The attribute '", attr, "' does not exist in feat_df."))
  }
  
  # Get a list of all .tif files in the source directory with full paths
  files <- list.files(source_dir, pattern = '*tif', full.names = TRUE)
  
  # Filter only the filenames that are listed in feat_df[[attr]]
  matching_files <- files[basename(files) %in% feat_df[[attr]]]
  
  # Loop over each filename in feat_df to check and copy matching files
  lapply(feat_df[[attr]], function(filename) {
    
    # Construct full path for the source file and destination file
    file_from <- file.path(source_dir, filename)
    file_to <- file.path(dest_dir, filename)
    
    # Check if the file exists in the source directory
    if (file.exists(file_from)) {
      
      # Check if the destination file exists and compare modification times
      if (file.exists(file_to)) {
        source_mtime <- file.info(file_from)$mtime
        dest_mtime <- file.info(file_to)$mtime
        if (source_mtime > dest_mtime) {
          file.copy(from = file_from, to = file_to, overwrite = TRUE)
          message(paste0(filename, " replaced successfully!"))
        } else {
          message(paste0(filename, " in destination is up to date."))
        }
      } else {
        # Copy if the destination file doesn't exist
        file.copy(from = file_from, to = file_to)
        message(paste0(filename, " copied successfully!"))
      }
      
    } else {
      # Issue a warning if the file is not found in the source directory
      warning(paste0(filename, " not found in source directory."))
    }
  })
  
  # Return NULL as the function performs file operations as a side effect
  invisible(NULL)
}


