# update variable choices automatically
varsUpdate <-
  function(UI_name) {
    updateSelectInput(session, UI_name, choices = vars())
  }
