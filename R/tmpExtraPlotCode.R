# gg <- ggplot() +
#   geom_sf(
#     data = plot_data_sf,
#     aes(fill = DisturbanceYear),
#     color = "black",      # Outline color for polygons
#     linewidth = 0.5       # Outline thickness
#   ) +
#   scale_fill_manual(
#     name = legend_title,          # Legend title
#     values = named_colors_for_plot,
#     labels = actual_factor_levels,
#     drop = FALSE
#   ) +
#   labs(
#     x = NULL, # Or specific labels if desired
#     y = NULL
#   ) +
#   theme_minimal(base_size = 11) + # Start with a clean theme
#   theme(
#     # --- Legend Customization for BOTTOM placement ---
#     legend.position = "bottom",                 # Place legend at the bottom
#     legend.direction = "horizontal",            # Arrange legend items horizontally
#     legend.title = element_text(face = "bold", size = rel(0.9), vjust = 0.75), # Style title, vjust to align if needed
#     legend.text = element_text(size = rel(0.8)),
#     legend.background = element_blank(),        # No background fill for legend area
#     legend.box.background = element_blank(),    # No outer box around legend area (if legend.box used)
#     legend.key = element_blank(),               # No background for individual legend keys (swatches)
#     legend.box.just = "center",                 # Justify the legend box in the center of allocated space
#     legend.spacing.x = unit(0.2, 'cm'),         # Adjust horizontal spacing between legend items if needed
# 
#     # Other theme elements to mimic base R if desired
#     panel.grid.major = element_blank(),
#     panel.grid.minor = element_blank(),
#     axis.line = element_line(colour = "black"),
#     axis.ticks = element_line(colour = "black"),
#     plot.background = element_rect(fill = "white", colour = NA),
# 
#     # Add some margin at the bottom of the plot if legend feels too cramped
#     plot.margin = margin(t = 5, r = 5, b = 10, l = 5, unit = "pt") # t,r,b,l for top,right,bottom,left
#   ) +
#   coord_sf(
#     expand = FALSE # Tries to fit data snugly
#   )

# # If fire_stack_cropped becomes NULL or empty after cropping, handle it gracefully
# if (is.null(fire_stack_cropped) || nlyr(fire_stack_cropped) == 0) {
#   warning("Cropped fire raster is empty; fire data will not be plotted.")
#   # Create a dummy empty ggplot object or handle this case as appropriate
#   gg_faceted <- ggplot() +
#     geom_sf(
#       data = plot_data_sf,
#       aes(fill = DisturbanceYear),
#       color = "black",
#       linewidth = 0.5
#     ) +
#     scale_fill_manual(
#       name = legend_title,
#       values = named_colors_for_plot,
#       labels = actual_factor_levels,
#       drop = FALSE
#     ) +
#     labs(
#       x = NULL,
#       y = NULL,
#       title = "Disturbances (Fire data not available/empty after crop)"
#     ) +
#     theme_minimal(base_size = 11) +
#     theme(
#       legend.position = "bottom", legend.direction = "horizontal",
#       legend.title = element_text(face = "bold", size = rel(0.9), vjust = 0.75),
#       legend.text = element_text(size = rel(0.8)),
#       legend.background = element_blank(), legend.box.background = element_blank(),
#       legend.key = element_blank(), legend.box.just = "center",
#       legend.spacing.x = unit(0.2, 'cm'),
#       panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#       axis.line = element_line(colour = "black"), axis.ticks = element_line(colour = "black"),
#       plot.background = element_rect(fill = "white", colour = NA),
#       plot.margin = margin(t = 5, r = 5, b = 10, l = 5, unit = "pt")
#     ) +
#     coord_sf(expand = FALSE)
# } else {
#   # Proceed with plotting if the cropped raster is not empty
#   gg_faceted <- ggplot() +
#     # 1. Plot the cropped fire data first (behind the polygons)
#     geom_spatraster(data = fire_stack_cropped, aes(fill = after_stat(value))) +
#     scale_fill_gradient(
#       name = "Burn Count",
#       low = "white", high = "red",
#       na.value = NA
#     ) +
#     
#     new_scale_fill() + # Declare a new fill scale
#     
#     # 2. Plot your disturbance polygons on top
#     geom_sf(
#       data = plot_data_sf,
#       aes(fill = DisturbanceYear),
#       color = "black",
#       linewidth = 0.5
#     ) +
#     scale_fill_manual(
#       name = legend_title,
#       values = named_colors_for_plot,
#       labels = actual_factor_levels,
#       drop = FALSE
#     ) +
#     facet_wrap(~lyr) +
#     labs(
#       x = NULL,
#       y = NULL,
#       title = "Overlap of Disturbances and Fire Regimes by Period"
#     ) +
#     theme_minimal(base_size = 11) +
#     theme(
#       legend.position = "bottom",
#       legend.direction = "horizontal",
#       legend.title = element_text(face = "bold", size = rel(0.9), vjust = 0.75),
#       legend.text = element_text(size = rel(0.8)),
#       legend.background = element_blank(),
#       legend.box.background = element_blank(),
#       legend.key = element_blank(),
#       legend.box.just = "center",
#       legend.spacing.x = unit(0.2, 'cm'),
#       legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "cm"),
#       
#       strip.background = element_rect(fill = "grey90", color = "black"),
#       strip.text = element_text(face = "bold"),
#       
#       panel.grid.major = element_blank(),
#       panel.grid.minor = element_blank(),
#       axis.line = element_line(colour = "black"),
#       axis.ticks = element_line(colour = "black"),
#       plot.background = element_rect(fill = "white", colour = NA),
#       plot.margin = margin(t = 5, r = 5, b = 10, l = 5, unit = "pt")
#     ) +
#     coord_sf(expand = FALSE)
# }
# 
# print(gg_faceted)
# 


