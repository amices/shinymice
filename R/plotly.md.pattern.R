# make md plot interactive

interactive_md_plot <- function(mdp){
  ax <- list(
    tickfont = list(size=11.7),
    titlefont=list(size=14.6),
    overlaying = "x",
    ticktext = unique(mdp$p$data$name),
    tickvals = (0:length(unique(mdp$p$data$name))) + .5,
    #nticks = length(mdp$second_y),
    side = "top",
    title = ""#Number of missing entries per pattern"
  )
  
  ay <- list(
    tickfont = list(size=11.7),
    titlefont=list(size=14.6),
    overlaying = "y",
    ticktext = mdp$second_y,
    tickvals = (1:length(mdp$second_y)) - .5,
    #nticks = length(mdp$second_y),
    side = "right",
    title = ""#Number of missing entries per pattern"
  )
  
  ggplotly(mdp$p) %>%
    add_lines(
      x =  ~ name,
      y =  ~ id,
      colors = NULL,
      alpha = 0,
      yaxis = "y2",
      xaxis = "x2",
      data = mdp$p$data,
      mode = "markers",
      type = "scatter",
      showlegend = FALSE,
      inherit = FALSE
    ) %>%
    layout(yaxis2 = ay, xaxis2 = ax)# %>% 
  # fig.update_xaxes(
  #   showgrid=True,
  #   ticks="outside",
  #   tickson="boundaries",
  #   ticklen=20
  # )
}
