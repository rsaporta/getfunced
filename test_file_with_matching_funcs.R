# test_file_with_matching_funcs.R


function_one <- function(file_full_path, filename="name 1", folder=getOption("wrkDir", default="~/test_dir"), unqiue_param, wrongly_shared= list(1, LETTERS, "ac")) {
  ### Comments
  "STUFF"

  ## more comments
  inner_function <- function(this_should_not_be_used) {
    "more stuff"
  }

  ## more more comments
  "more more stuff"
}



function_two <- function(
  file_full_path
, filename="NAME 2"
, folder=getOption("wrkDir", default="~/test_dir")
, unqiue_param_number_2
, wrongly_shared="some param"
, uneven_param = "this is just in 2nd"
) {
  "bar"

  list(foo = "bar")

  ### Comments
  "STUFF"

  ## more comments
  inner_function <- function(this_should_not_be_used) {
    "more stuff"
  }

  ## more more comments
  "more more stuff"
}




function_three <- function(file_full_path, filename="NAME 2", folder=getOption("wrkDir", default="~/test_dir")) {
  "blaaaaahhhhhh"
}

.func_starting_with_dot <- function(param_one, other_param_, etc) {

}


