read_an_answer <-
function(my_prompt) {
  ANSWER <- readline(my_prompt)
  if (tolower(substr(ANSWER, 1, 1)) == "n")
    ANSWER = "no"
  else
    ANSWER = "yes"
}
