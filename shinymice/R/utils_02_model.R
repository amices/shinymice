#' Title Plot missing data pattern of incomplete data
#'
#' @param dat An incomplete dataset of class dataframe
#'
#' @return A ggplot object visualizing the missing data pattern matrix
#' @export
plot_md_pattern <- function(dat) {
  # escape function if dataset is complete
  if (!any(is.na(dat))) {
    return(plot_a_mouse())
  }
  # get md pattern and store additional info
  pat <- mice::md.pattern(dat, plot = FALSE)
  vrb <- colnames(pat)[-ncol(pat)]
  colnames(pat) <- c(vrb, "NA_per_pat")
  pat_freq <- as.numeric(rownames(pat))[-nrow(pat)]
  NA_per_pat <- as.numeric(pat[, ncol(pat)])[-nrow(pat)]
  NA_per_vrb <- as.numeric(pat[nrow(pat), ])[-ncol(pat)]
  NA_total <- pat[nrow(pat), ncol(pat)]
  # make the pattern tidy
  long_pat <- pat[-nrow(pat), ] %>%
    cbind(., pat_freq, pat_nr = 1:nrow(.)) %>%
    as.data.frame() %>%
    tidyr::pivot_longer(cols = all_of(vrb),
                        names_to = "vrb",
                        values_to = "obs") %>%
    cbind(., NA_per_vrb)
  # plot the md pattern
  p <- long_pat %>%
    ggplot2::ggplot() +
    ggplot2::geom_tile(ggplot2::aes(
      x = vrb,
      y = pat_nr,
      fill = as.factor(obs),
      group = NA_per_pat
    ),
    color = "black") +
    # set axes
    ggplot2::scale_x_discrete(
      limits = vrb,
      position = "bottom",
      labels = as.character(NA_per_vrb),
      expand = c(0, 0)
    ) +
    ggplot2::scale_y_reverse(
      breaks = 1:max(long_pat$pat_nr),
      labels = as.character(pat_freq),
      expand = c(0, 0),
      sec.axis = ggplot2::dup_axis(labels = as.character(NA_per_pat),
                                   name = "Number of missing entries per pattern")
    ) +
    ggplot2::coord_cartesian(clip = "off") +
    # add labels
    ggplot2::labs(
      x = "Number of missing entries per variable",
      y = "Pattern frequency",
      title = paste0(
        "Missing data pattern (total number of missing cells = ",
        NA_total,
        ")\n"
      )
    ) +
    ggplot2::geom_text(
      ggplot2::aes(
        x = vrb,
        y = -Inf,
        label = abbreviate(.data[["vrb"]])
      ),
      data = long_pat[1:length(vrb),],
      vjust = -0.5
    ) +
    # add styling
    ggplot2::theme(
      legend.position = "none",
      plot.margin = ggplot2::margin(
        t = 20,
        l = 10,
        b = 10,
        r = 10,
        unit = "pt"
      ),
      axis.title.y.right = ggplot2::element_text(margin = ggplot2::margin(l = 10)),
      panel.grid = ggplot2::element_blank()
    ) +
    ggplot2::scale_fill_manual(values = c("1" = mice:::mdc(1), "0" = mice:::mdc(2)))
  # output
  return(p)
}

#' Title Plot in- and outflux of an incomplete dataset
#'
#' @param dat An incomplete dataset of class dataframe
#'
#' @return A ggplot object with influx plotted against outflux
#' @export
plot_flux <- function(dat) {
  # escape function if dataset is complete
  # if(!any(is.na(dat))){return(plot_a_mouse())}
  # plot in and outflux
  flx <- mice::flux(dat) %>% cbind(variable = rownames(.))
  p <- flx %>% ggplot2::ggplot() +
    ggplot2::geom_text(
      ggplot2::aes(x = influx,
                   y = outflux,
                   label = variable),
      position = ggplot2::position_jitter(width = 0.01, height = 0.01)
    ) +
    ggplot2::geom_abline(intercept = 1,
                         slope = -1,
                         linetype = "dashed") +
    ggplot2::lims(x = c(-0.01, 1.01), y = c(-0.01, 1.01)) +
    ggplot2::theme_classic()
  # output
  return(p)
}

#' Title Plot the predictor matrix for the imputation model
#'
#' @param d A predictor matrix obtained from a mids object or using mice::quickpred()
#'
#' @return A ggplot object visualizing the predictor matrix
#' @export
plot_pred_matrix <- function(pred) {
  # make the data tidy to plot
  long_pred <- pred %>%
    as.data.frame() %>%
    cbind(vrb_to_imp = rownames(.), .) %>%
    tidyr::pivot_longer(
      cols = names(.)[-1],
      names_to = "predictor",
      names_transform = list(predictor = as.factor)
    )
  # plot the predictor matrix
  p <- long_pred %>%
    ggplot2::ggplot() +
    ggplot2::geom_tile(ggplot2::aes(
      x = predictor,
      y = vrb_to_imp,
      fill = as.factor(value)
    ),
    color = "black") +
    ggplot2::theme(legend.position = "none",
                   panel.grid = ggplot2::element_blank()) +
    ggplot2::scale_y_discrete(
      limits = rev(colnames(pred)),
      expand = c(0, 0),
      label = abbreviate(rev(colnames(pred)))
    ) +
    ggplot2::scale_x_discrete(
      limits = colnames(pred),
      position = "top",
      expand = c(0, 0),
      label = abbreviate(colnames(pred))
    ) +
    ggplot2::scale_fill_manual(values = c("1" = mice:::mdc(1), "0" = mice:::mdc(2))) +
    ggplot2::labs(x = "Predictor", y = "Variable to impute")#, title = "Predictor matrix")
  # output
  return(p)
}