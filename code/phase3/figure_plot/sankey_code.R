library(tidyverse)
library(ggsankey)
library(patchwork)

# Read data
df0 <- read_csv("juece.csv") %>% 
  select(number, Conventional, DynaFusion, RS) %>% 
  distinct()

# Prepare Sankey data using ggsankey's make_long()
# make_long() converts wide format to the long format required by geom_sankey
prepare_sankey <- function(data, prefix) {
  data %>% 
    select(number, source = !!sym(prefix), target = RS) %>% 
    # Prefix nodes to distinguish source and target columns
    mutate(
      source = paste0(prefix, ": ", source),
      target = paste0("RS: ", target)
    ) %>% 
    make_long(source, target)
}

sankey_Conventional <- prepare_sankey(df0, "Conventional")
sankey_DynaFusion <- prepare_sankey(df0, "DynaFusion")

# Color mapping for diagnosis categories
node_colors <- c(
  "LE"            = "#ffcc99",
  "TME"           = "#b3e2b0",
  "NACRT"         = "#d9b8d4",
  "WandW_or_LE"   = "#a6cee3"
)

# Helper function to extract category from node label
# e.g., "Conventional: TME" -> "TME"
get_category <- function(node) {
  if (is.na(node)) return(NA_character_)
  parts <- strsplit(node, ": ")[[1]]
  if (length(parts) == 2) {
    return(parts[2])
  }
  return(node)
}

# Plot function for Sankey diagram
plot_sankey <- function(sankey_data, title) {

  # Add category column for fill coloring
  sankey_data <- sankey_data %>% 
    mutate(category = map_chr(node, get_category))

  ggplot(sankey_data, aes(x = x,
                          next_x = next_x,
                          node = node,
                          next_node = next_node,
                          fill = category,
                          label = node)) +
    geom_sankey(flow.alpha = 0.5, 
                node.color = "black",
                width = 0.15) +
    geom_sankey_label(size = 3, 
                      color = "black", 
                      fill = "white",
                      fontface = "bold") +
    scale_fill_manual(values = node_colors,
                      na.translate = FALSE) +
    labs(title = title,
         x = NULL) +
    theme_sankey(base_size = 14) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      legend.position = "none"
    )
}

# Create individual plots
plot_Conventional <- plot_sankey(sankey_Conventional, 
                                 "Conventional vs. Gold Standard")
plot_DynaFusion <- plot_sankey(sankey_DynaFusion, 
                               "DynaFusion vs. Gold Standard")

# Combine plots with English annotations
combined_plot <- plot_Conventional / plot_DynaFusion +
  plot_annotation(
    title = "Diagnostic Pathways vs. Gold Standard",
    subtitle = "Flow from diagnostic method to reference standard (RS)",
    theme = theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
      plot.subtitle = element_text(hjust = 0.5, color = "gray40", size = 12)
    )
  )

print(combined_plot)
