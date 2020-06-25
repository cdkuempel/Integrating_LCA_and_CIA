theme_nlm_nis <- function(base_family = NA,
                               base_size = 11.5,
                               plot_title_family = base_family,
                               plot_title_size = 18,
                               plot_title_face = "bold",
                               plot_title_margin = 10,
                               subtitle_family =NA,
                               subtitle_size = 13,
                               subtitle_face = "plain",
                               subtitle_margin = 15,
                               strip_text_family = base_family,
                               strip_text_size = 12,
                               strip_text_face = "plain",
                               strip.background = "grey80",
                               caption_family =NA,
                               caption_size = 9,
                               caption_face = "plain",
                               caption_margin = 10,
                               axis_text_size = base_size,
                               axis_title_family = base_family,
                               axis_title_size = 9,
                               axis_title_face = "plain",
                               axis_title_just = "rt",
                               plot_margin = ggplot2::unit(c(0, 0, 0, 0), "lines"),
                               grid_col = "#cccccc",
                               grid = TRUE,
                               axis_col = "#cccccc",
                               axis = FALSE,
                               ticks = FALSE,
                               legend_title = "Z",
                               legend_labels = NULL,
                               legend_text_size  = 8,
                               legend_title_size = 10,
                               ratio = 1,
                               viridis_scale = "D",
                               ...) {
  # start with minimal theme
  ret <-
    ggplot2::theme_minimal(base_family = base_family, base_size = base_size)
  
  
  xj <- switch(tolower(substr(axis_title_just, 1, 1)), b = 0, l = 0, m = 0.5, c = 0.5, r = 1, t = 1)
  yj <- switch(tolower(substr(axis_title_just, 2, 2)), b = 0, l = 0, m = 0.5, c = 0.5, r = 1, t = 1)
  
  
  # extend it
  theme_base <- ret + ggplot2::theme(
    legend.background = ggplot2::element_blank(),
    legend.text  = ggplot2::element_text(size = legend_text_size),
    legend.title = ggplot2::element_text(size = legend_title_size),
    aspect.ratio = ratio,
    plot.margin = plot_margin,
    strip.text = ggplot2::element_text(
      hjust = 0,
      size = strip_text_size,
      face = strip_text_face,
      family = strip_text_family
    ),
    strip.background = ggplot2::element_rect(fill = strip.background),
    panel.spacing = grid::unit(2, "lines"),
    plot.title = ggplot2::element_text(
      hjust = 0,
      size = plot_title_size,
      margin = ggplot2::margin(b = plot_title_margin),
      family = plot_title_family,
      face = plot_title_face
    ),
    plot.subtitle = ggplot2::element_text(
      hjust = 0,
      size = subtitle_size,
      margin = ggplot2::margin(b = subtitle_margin),
      family = subtitle_family,
      face = subtitle_face
    ),
    plot.caption = ggplot2::element_text(
      hjust = 1,
      size = caption_size,
      margin = ggplot2::margin(t = caption_margin),
      family = caption_family,
      face = caption_face
    ),
    axis.text.x = ggplot2::element_text(size = axis_text_size, margin = ggplot2::margin(t = 0)),
    axis.text.y = ggplot2::element_text(size = axis_text_size, margin = ggplot2::margin(r = 0)),
    axis.title = ggplot2::element_text(size = axis_title_size, family = axis_title_family),
    axis.title.x = ggplot2::element_text(hjust = xj, size = axis_title_size,
                                         family = axis_title_family, face = axis_title_face),
    axis.title.y = ggplot2::element_text(hjust = yj, size = axis_title_size,
                                         family = axis_title_family, face = axis_title_face),
    axis.title.y.right = ggplot2::element_text(hjust = yj, size = axis_title_size, angle = 90,
                                               family = axis_title_family, face = axis_title_face),
    ...
  )
  
  # # define color scale
  # theme_color <- ggplot2::scale_fill_viridis_d(
  #   option = viridis_scale,
  #   direction = 1,
  #   na.value = "transparent",
  #   labels = if (is.null(legend_labels)) {
  #     ggplot2::waiver()
  #   } else {
  #     legend_labels
  #   },
  #   name = legend_title
  # )
  
  # return as list
  list(theme_base)
  
}