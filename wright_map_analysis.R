# Load required libraries
library(eRm)
library(WrightMap)
library(RColorBrewer)

# Define the function for Wright Map analysis
run_wright_map_analysis <- function(data, item_columns, dim_labels, label_colors, dim_colors, output_file = NULL, item_labels = NULL, idpattern, extend, abstract, sample_info = NULL, group_info = NULL, item_shapes, item_names, show_plot = FALSE) {
  # Check if item_labels are provided; if not, use default column names without prefixes
  if (is.null(item_labels)) {
    # Extract only the item numbers from the column names just to print numbers
    #item_labels <- as.numeric(gsub("Item", "", item_columns))
  }

  # Subset the items based on specified columns
  item_data <- as.matrix(data[, item_columns])
  
  # Fit the Rasch model
  rasch_model <- RM(item_data)
  
  # Extract person abilities (theta values) for all respondents
  person_parameters <- person.parameter(rasch_model)$theta.table$`Person Parameter`
  
  # Compute person abilities for each dimension
  person_abilities_idpattern <- rowMeans(item_data[, idpattern])
  person_abilities_extend <- rowMeans(item_data[, extend])
  person_abilities_abstract <- rowMeans(item_data[, abstract])
  
  
  # Combine the person abilities into a matrix
  person_abilities_thetas <- cbind(person_abilities_idpattern, person_abilities_extend, person_abilities_abstract)
  
  # Extract item difficulties
  item_difficulties <- coef(rasch_model)
  
  # Calculate and display mean and SD
  respondent_mean_idpattern <- mean(person_abilities_idpattern, na.rm = TRUE)
  respondent_sd_idpattern <- sd(person_abilities_idpattern, na.rm = TRUE)
  respondent_mean_extend <- mean(person_abilities_extend, na.rm = TRUE)
  respondent_sd_extend <- sd(person_abilities_extend, na.rm = TRUE)
  respondent_mean_abstract <- mean(person_abilities_abstract, na.rm = TRUE)
  respondent_sd_abstract <- sd(person_abilities_abstract, na.rm = TRUE)
  
  item_mean <- mean(item_difficulties, na.rm = TRUE)
  item_sd <- sd(item_difficulties, na.rm = TRUE)

  # Create a dynamic title
  sample_size <- nrow(item_data)  # Number of respondents
  dimension_info <- paste(dim_labels, collapse = ", ")
  dynamic_title <- if (!is.null(sample_info)) {
    paste("Wright Map -", sample_info, "- Sample Size:", sample_size)
  } else {
    paste("Wright Map - Sample Size:", sample_size, "- Dimensions:", dimension_info)
  }
  
  # Rename the `item_difficulties` to use `item_labels`
  names(item_difficulties) <- item_names  # Replace "beta Item" labels with custom `item_labels`
  
  # Construct title dynamically
  title_text <- sprintf("Wright Map: %s \n %s Group (n = %d)", sample_info, group_info, nrow(data))
  
  # Generate the Wright Map (display inline in Quarto)
  if (show_plot) {
    wrightMap(
      person_abilities_thetas,
      item_difficulties,
      item.labels = item_names,  # Use simplified labels here
      item.prop = 0.5,
      thr.sym.pch = item_shapes,
      dim.names = dim_labels,
      dim.color = label_colors,
      show.thr.lab = FALSE,
      thr.sym.col.fg = dim_colors,
      thr.sym.cex = 2,
      thr.lab.col = dim_colors,
      thr.lab.text = item_names,
      cex.axis = 1,
      cex.lab = 1,
      person.side = personDens,
      label.items.row = 2,
      main.title = NULL,   # The title dyanmically created below
    )
    
    #Add a dynamic title
    title(main = title_text, line = 2, cex.main = 1)  # Adjust line and size
    
    # Annotate respondent graph (left panel)
    mtext(sprintf("M: %.2f, SD: %.2f", respondent_mean_idpattern, respondent_sd_idpattern), side = 3, line = 0, adj = 0.06, cex = 0.8, col = "red")
    mtext(sprintf("M: %.2f, SD: %.2f", respondent_mean_extend, respondent_sd_extend), side = 3, line = 0, adj = 0.2, cex = 0.8, col = "blue")
    mtext(sprintf("M: %.2f, SD: %.2f", respondent_mean_abstract, respondent_sd_abstract), side = 3, line = 0, adj = 0.3, cex = 0.8, col = "darkgreen")
    
    # Annotate item difficulty graph (right panel)
    mtext(sprintf("Mean %.2f, SD: %.2f", item_mean, item_sd), side = 3, line = 0, adj = .75, cex = 0.8, col = "black")
  }
  # Save the Wright Map to PNG (if `output_file` is specified)
  if (!is.null(output_file)) {
    # Generate the Wright Map
    cat("Saving Wright Map to:", output_file, "\n")
    png(output_file, width = 3000, height = 2000, res = 300)
    
    wrightMap(
      thetas = person_abilities_thetas,
      item_difficulties,
      item.labels = item_names,      # Use simplified labels here
      item.prop = 0.6,
      thr.lab.text = item_names,
      thr.sym.pch = item_shapes,
      dim.names = dim_labels,
      dim.color = label_colors,
      show.thr.lab = FALSE, # Use to show labels on data points
      thr.sym.col.fg = dim_colors,
      thr.sym.col.bg = dim_colors,
      thr.sym.cex = 2,
      cex.axis = 1,
      cex.lab = 1,
      person.side = personDens,
      label.items.row = 2,
      main.title = NULL,   # The title dyanmically created below
      axis.items = ""
    )

    #Add a dynamic title
    title(main = title_text, line = 2, cex.main = 1)  # Adjust line and size
    
    # Annotate respondent graph (left panel)
    mtext(sprintf("M: %.2f, SD: %.2f", respondent_mean_idpattern, respondent_sd_idpattern), side = 3, line = 0, adj = 0.07, cex = 0.75, col = "red")
    mtext(sprintf("M: %.2f, SD: %.2f", respondent_mean_extend, respondent_sd_extend), side = 3, line = 0, adj = 0.19, cex = 0.75, col = "blue")
    mtext(sprintf("M: %.2f, SD: %.2f", respondent_mean_abstract, respondent_sd_abstract), side = 3, line = 0, adj = 0.32, cex = 0.75, col = "darkgreen")
    
    # Annotate item difficulty graph (right panel)
    mtext(sprintf("Mean %.2f, SD: %.2f", item_mean, item_sd), side = 3, line = 0, adj = .75, cex = 0.8, col = "black")
    
    dev.off()
  }
  
  # Return results if needed for further analysis
  return(list(
    #person_abilities = person_abilities_thetas,
    #item_difficulties = item_difficulties,
    rasch_model = rasch_model
  ))
}