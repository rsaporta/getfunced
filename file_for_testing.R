
"~/Development/rpkgs/getfunced/file_for_testing.R"
test_func <- function(
  arg_one = list(a = 1, b = 2, c = 3)
  , vec_arg = 1:5L
  , arg_with_n="before\nafter"
  , just_linebreak="\n"
  , seq_arg=seq(from=1L, to=5L)
  , empty_string=""
  , empty_string2=c("")
  , empty_string3=''
  , empty_string4=c('')
  , arg_two_no_default
  , arg_three = "Default value for two"
  , x
  , y=12345L
  , z=12345
  , DT_arg = data.table(col1=1:6, col2=7L, col3=letters[1:2], col_string="hello world")
  , quoted_quotes="\"\"\"\""
  , quoted_quotes2="\'\"\"\'"
  , quoted_quotes3="\"something wrapped in quotes\""
) {

}


.dot_function <- function(param_one, param_two) {
  "hello world"
}

outter_function <- function(param_abc, cdkfdj) {
  inner_function <- function(error_if, you_are, seeing_this) {
    "stuff"
  }

  "more stuff"
}