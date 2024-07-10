current_project_paths <- function(code_path = NA,
                                  my_paths = NA) {
  if (is.na(my_paths)) {
    my_paths <- auxfunctions::set_work_dir()
  }
  
  if (is.na(code_path)) {
    #' get this project name
    current_project_dir_name <- this.path::this.dir()
  }
  else {
    current_project_dir_name = code_path
  }
  
  #' find its base name
  current_project_name <-
    basename(current_project_dir_name)
  
  #' use current_project_name to create input and output paths
  curr_proj_input_path <- file.path(my_paths$inputs, current_project_name)
  
  auxfunctions::create_dir_if_not(curr_proj_input_path)
  
  curr_proj_output_path <- file.path(my_paths$outputs, current_project_name)
  
  auxfunctions::create_dir_if_not(curr_proj_output_path)
  
  current_proj_paths <-
    list("code" = current_project_dir_name,
         "input" = curr_proj_input_path,
         "output" = curr_proj_output_path)
  
  return(current_proj_paths)
}
