# feature idea: remove chunk dirs with incomplete outputs
rm_empty_dirs = function(parent_dir = file.path("output", "v1", "0_chunks")){
  cdirs = list.dirs(parent_dir)
  cdirs = cdirs[grepl(paste(foa$name, collapse = "|"), cdirs)]
  
  cat = purrr::map(cdirs, .progress=T,
    .f = function(x) {
    dir_files = list.files(x, full.names = T)
      if (!any(grepl("_RandigOutputs.tif$", dir_files))){
        unlink(x, recursive=T)
        #message("Remove empty chunk dir: ", x)
        return(x)
      }
    }
  )
  cat_vec = unlist(cat)
  if (length(cat_vec > 0)){
    message("Removed ", length(cat_vec), " empty directories (out of ",length(cdirs),"):")
    return(cat_vec)
  } else {
    message("No empty directories found (among ", length(cdirs), ").")
  }
}