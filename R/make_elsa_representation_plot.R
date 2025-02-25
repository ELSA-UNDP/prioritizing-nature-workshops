#' Create a Bar Plot of Feature Representation
#'
#' This function generates a stacked bar plot displaying the representation of each input
#' feature, broken up into specified actions (e.g., "Protect," "Restore," "Manage,"
#' and optionally "Urban Greening").
#'
#' @param feature_rep_tabl A dataframe containing representation values for each feature.
#' Columns should include numeric data for each feature and action.
#' @param input The input object of an R Shiny app containing budget information for each action.
#' This is used to label the budget percentage of each action in the plot legend.
#'
#' @return A ggplot object with a stacked bar chart showing feature representation percentages.
#' @export
make_elsa_representation_plot <- function(feature_rep_tabl, input) {
  # Reshape data to a long format for plotting, including optional urban greening column
  data_long <- feature_rep_tabl |>
    tidyr::pivot_longer(
      cols = c(4, 5, 6, 7),
      names_to = get_translation('category'),
      values_to = get_translation('pct')
    ) |>
    dplyr::mutate(
      across(1, ~ factor(., levels = sort(unique(.), decreasing = TRUE))), # Need to reverse factor levels so they plot correctly - ggplot will flip them.
      across(c(2, 4), as.factor)
      ) |> 
    dplyr::arrange(desc(dplyr::across(1)))
  
  # Create legend text dynamically based on user-defined zone budgets
  protect_budget <- glue::glue("{get_translation('protect')} {round(input$zone_1_target, 1)}%")
  restore_budget <- glue::glue("{get_translation('restore')} {round(input$zone_2_target, 1)}%")
  manage_budget <- glue::glue("{get_translation('manage')} {round(input$zone_3_target, 1)}%")
  pr_budget <- glue::glue("{get_translation('pr')}")
  
  # Define fill colors for each zone in the plot
  fill_values <- c(
    "Protect" = pal.elsa$colour[1],
    "Restore" = pal.elsa$colour[2],
    "Manage" = pal.elsa$colour[3],
    "Protect and Restore" = pal.elsa$colour[4]
  )
  
  # Define legend items and breaks in the legend
  breaks <- c(
    get_translation('protect'),
    get_translation('restore'),
    get_translation('manage'),
    get_translation('pr')
  )
  
  # Ensure fill values are in the correct language
  names(fill_values) <- breaks
  
  # Create labels with budget information for each legend item
  labels <- c(protect_budget, restore_budget, manage_budget, pr_budget)
  
  # Create a ggplot stacked bar chart
  elsa_representation_plot <- ggplot2::ggplot(
    data_long,
    aes(
      x = .data[[names(data_long)[1]]], 
      y = .data[[names(data_long)[5]]], 
      fill = .data[[names(data_long)[4]]] 
    )) +
    ggplot2::geom_bar(stat = "identity") + # Add bars with values
    ggplot2::scale_fill_manual(
      values = fill_values,
      name = glue::glue("ELSA {get_translation('action')}"),  # Legend title
      labels = labels,  # Set legend labels with budget
      breaks = breaks  # Set legend order
    ) +
    ggplot2::coord_flip() + # Flip the coordinates for a horizontal bar chart
    ggplot2::scale_y_continuous(
      limits = c(0, 100),
      breaks = seq(0, 100, by = 25)
    ) +
    ggplot2::labs(
      x = " ",  # No label on x-axis
      y = glue::glue("{get_translation('rep')} %")  # Label for y-axis
    ) +
    ggplot2::theme_minimal() + # Minimal theme for cleaner look
    ggplot2::theme(
      text = element_text(family = "Roboto", colour = '#495057'), # Font styling
      legend.position = "top", # Place legend above the plot
      legend.background = ggplot2::element_rect(color = "#495057", linewidth = 0.5), # Legend box style
      legend.text = ggplot2::element_text(size = 14, face = "bold"),
      legend.title = ggplot2::element_text(size = 14, face = "bold"),
      axis.text = ggplot2::element_text(size = 12),
      axis.title = ggplot2::element_text(size = 12),
      panel.border = ggplot2::element_rect(color = "#495057", linewidth = 1, fill = NA) # Plot border styling
    )
  
  return(elsa_representation_plot) # Return the completed plot
}
