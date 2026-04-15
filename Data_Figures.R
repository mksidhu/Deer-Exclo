

library(ggplot2)
library(patchwork)
library(forcats)

source("Named_Functions.R")


##when doing all three, go in order of control, two sided, four sided


#avg heights in-out browsed-out unbrowse of only woody spp in common
common_avg_heights_fig <- function(df_east, df_west){
  title <- which_treatment(df_east)
  
  inside <- unite_treatment(df_east, df_west, TRUE)
  outside <- unite_treatment(df_east, df_west, FALSE)
  inside_richness <- species_richness(inside, quiet = TRUE)
  outside_richness <- species_richness(outside, quiet = TRUE)
  inside_heights <- avg_inside_heights(inside_richness, inside)
  outside_heights <- percent_browse_outside(outside, outside_richness, quiet = TRUE)

  common_spp <- spp_in_common(inside_heights, outside_heights, outside_heights)
  
  inside_common <- inside_heights %>% filter(Species %in% common_spp)
  outside_common <- outside_heights %>% filter(Species %in% common_spp)
  
  legend_colors <- c("Ash" = "#FB9A99", "Cherry" = "#FDBF6F", "MfRose" = "#FF7F00",  
                     "VACreep"= "#E31A1C", "PoisonIvy" = "#CAB2D6")
  
  plot1 <- ggplot(inside_common, aes(x = reorder(Species, Average_Height),
                                     y = Average_Height, fill = Species)) + 
    geom_col(position="dodge") + labs(x = "Inside") + 
    geom_errorbar(aes(ymin = Average_Height - SE, ymax = Average_Height + SE), 
                  position = "dodge", color = "black") + 
    scale_fill_manual(values = legend_colors) + theme(axis.text.x = element_blank(),
                                                  axis.ticks.x = element_blank(),
                                                  axis.title.y = element_blank()) + 
    coord_cartesian(ylim = c(0, 100)) + theme(text = element_text(size = 12))
  
  plot2 <- ggplot((outside_common %>% filter(Browse == 1)), aes(x = 
                                                              reorder(Species,
                                                                    AvgHeight),
                                                                y = AvgHeight,
                                                              color = Species)) +
    geom_col(position="dodge", stat = "identity", fill = "white", 
             linewidth = 1.25) + scale_color_manual(values = legend_colors) +
    geom_errorbar(aes(ymin = AvgHeight - SE, ymax = AvgHeight + SE), 
                  position = "dodge", color = "black") + 
    labs(x = "Outside - Browsed", title = glue("Average Common Woody Heights 
                                             by Transect and Browse - {title}"),
                                      y = "Average Height (cm)") + 
    theme(legend.position = "none", axis.text.x = element_blank(), 
          axis.ticks.x = element_blank()) + coord_cartesian(ylim = c(0, 100)) +
    theme(text = element_text(size = 12))
  
  plot3 <- ggplot((outside_common %>% filter(Browse == 0)), aes(x = 
                                                                reorder(Species,
                                                                    AvgHeight), 
                                                                y = AvgHeight, 
                                                            color = Species)) + 
    geom_col(position="dodge", stat = "identity", fill = "white", 
             linewidth = 1.25) + labs(x = "Outside - Unbrowsed")  +
    geom_errorbar(aes(ymin = AvgHeight - SE, ymax = AvgHeight + SE), 
                  position = "dodge", color = "black") + 
    scale_color_manual(values = legend_colors) + theme(legend.position = "none", 
                                                  axis.text.x = element_blank(),
                                                  axis.ticks.x = element_blank(),
                                                  axis.title.y = element_blank()) + 
    coord_cartesian(ylim = c(0, 100)) + theme(text = element_text(size = 12))
  
  plot4 <- (plot2 + plot3 + plot1) + plot_annotation(tag_levels = "A") & 
    theme(plot.tag = element_text(size = 12, face = "bold"), 
          plot.tag.position = "bottomleft")
  return(plot4)
}




#ocular averages inside and out total averages of two dfs (like control and four-sided)
#inside and outside juxtaposed in one graph
tot_cover_avg_fig <- function(df1_east, df1_west, df2_east, df2_west) {
  tots_table1 <- pivot_longer(cover_avgs(df1_east, df1_west),
                              cols = c(contains("_mean"), contains("_SE")),
                              names_to = c("Category", ".value"),
                              names_sep = "_%_") %>% 
    filter(str_detect(Location, "Total"))
  
  tots_table2 <- pivot_longer(cover_avgs(df2_east, df2_west),
                              cols = c(contains("_mean"), contains("_SE")),
                              names_to = c("Category", ".value"),
                              names_sep = "_%_") %>% 
    filter(str_detect(Location, "Total"))

  df1_name <- which_treatment(df1_east)
  df2_name <- which_treatment(df2_east)
  
  legend_colors <- c("Forbs" ="#B2DF8A", "Grasses" =  "#1F78B4", "NVGround" 
                     = "#FB9A99", "Other" = "#A6CEE3", "Woody" = "#33A02C")
  
  tots_df <- bind_rows(df1 = tots_table1, df2 = tots_table2, 
            .id = "Plots") 
  
  tots_df$Plots <- str_replace_all(tots_df$Plots,"df1", {{df1_name}}) 
  tots_df$Plots <- str_replace_all(tots_df$Plots, "df2", {{df2_name}})
  
  plot1 <- tots_df %>% 
    ggplot(aes(x = Plots, y = mean, fill = interaction(list(Location,
                                                            fct_reorder(Category, mean)),
                                                       sep = " "), color = Category)) + 
    geom_col(position="dodge", stat = "identity", linewidth = 1.25) + 
    geom_errorbar(aes(ymin = mean - SE, ymax = mean + SE), position =
                    position_dodge(), color = "black") +
                    labs(title = glue("{df1_name} and {df2_name} Treatment Plots
                                      Plant Cover Total Averages"), 
                         y = "Percent Averages") + 
    scale_fill_manual(values = c("Inside Total Forbs" = "#B2DF8A", 
                                 "Inside Total Grasses" = "#1F78B4", 
                                 "Inside Total NVGround" = "#FB9A99", 
                                 "Inside Total Other" = "#A6CEE3", 
                                 "Inside Total Woody" = "#33A02C", 
                                 "Outside Total Forbs" = "white",
                                 "Outside Total Grasses" = "white",
                                 "Outside Total NVGround" = "white", 
                                 "Outside Total Other" = "white", 
                                 "Outside Total Woody" = "white"), 
                      name = "Plant Categories") + 
    scale_colour_manual(values = legend_colors, guide = "none") + 
    coord_cartesian(ylim = c(0, 100)) + theme(text = element_text(size = 12))
  
  legend_fill_names <- levels(interaction(list(factor(tots_df$Location), 
                                               fct_reorder(tots_df$Category, 
                                                           tots_df$mean))))
  legend_fill <- list()
  
  for (orderedname in legend_fill_names){
    plant_category <- str_split_i(orderedname, "\\.", 2)
    legend_name <- str_which(plant_category, names(legend_colors))
    orderedcolor <- legend_colors[legend_name]
    legend_fill <- append(legend_fill, orderedcolor)
    }
  

  return(plot1 + guides(fill = 
                          guide_legend(override.aes = 
                                         list(color = unlist(legend_fill)))))
}



##creates figures of average ocular covers - maybe could make the combined use
#optional parameters for df2 and df3 so dont need separate function for within figs
#faceted but outside is white bars with outlines. legend fill colors set to control--
#need to fix for others
alt_within_oc_fig <- function(df_east, df_west){
  oc_df <- cover_avgs(df_east, df_west)
  tidy_ocular <- pivot_longer(oc_df,  cols = c(contains("_mean"), contains("_SE")), 
                              names_to = c("Category", ".value"), names_sep = "_%_")
 
  title <- which_treatment(df_east)
  legend_colors <- c("Forbs" = "#B2DF8A", "Grasses" = "#1F78B4", 
                     "NVGround" = "#FB9A99", "Other" = "#A6CEE3", "Woody" = "#33A02C")
  
  plot1 <- tidy_ocular %>% filter(str_detect(Location, "Inside")) %>% 
    ggplot(aes(x = Location, y = mean, fill = reorder(Category, mean))) + 
    geom_col(position="dodge", stat = "identity") + 
    geom_errorbar(aes(ymin = mean - SE, ymax = mean + SE), position = 
                    position_dodge(),color = "black") +  
    theme(axis.title.y = element_blank()) + labs(x = "Inside Transects") + 
    scale_fill_manual(values = legend_colors, name = "Plant Categories") + 
    coord_cartesian(ylim = c(0, 100)) + theme(text = element_text(size = 12))

  plot2 <- tidy_ocular %>% filter(str_detect(Location, "Outside")) %>% 
    ggplot(aes(x = Location, y = mean, color = reorder(Category, mean))) + 
    geom_col(position= "dodge", stat = "identity", fill = "white", 
             linewidth = 1.25) + geom_errorbar(aes(ymin = mean - SE, 
                                                   ymax = mean + SE, group = 
                                                     reorder(Category, mean)), 
                                               position = position_dodge(),
                                               color = "black") +
    scale_colour_manual(values = legend_colors) + theme(legend.position = "none") + 
    labs(title = glue("{title} Treatment Plant Cover Averages"), 
         y = "Percent Averages", x = "Outside Transects") + 
    coord_cartesian(ylim = c(0, 100)) + theme(text = element_text(size = 12))
     
  plot3 <- (plot2 + plot1) + plot_annotation(tag_levels = "A") & 
    theme(plot.tag = element_text(size = 12, face = "bold"), 
          plot.tag.position = "bottomleft")
  return(plot3)
}


#faceted average cover for all three df
btwn_oc_fig <- function(df1_east, df1_west, df2_east, df2_west, df3_east, 
                        df3_west, inside = TRUE) {
  
  tidy_table1 <- pivot_longer(cover_avgs(df1_east, df1_west),
                              cols = c(contains("_mean"), contains("_SE")),
                              names_to = c("Category", ".value"),
                              names_sep = "_%_") %>% 
    filter(str_detect(Location, "Total"))
  title1 <- which_treatment(df1_east)
  
  tidy_table2 <- pivot_longer(cover_avgs(df2_east, df2_west),
                              cols = c(contains("_mean"), contains("_SE")),
                              names_to = c("Category", ".value"),
                              names_sep = "_%_") %>% 
    filter(str_detect(Location, "Total"))
  title2 <- which_treatment(df2_east)
  
  tidy_table3 <- pivot_longer(cover_avgs(df3_east, df3_west),
                              cols = c(contains("_mean"), contains("_SE")),
                              names_to = c("Category", ".value"),
                              names_sep = "_%_")%>% 
    filter(str_detect(Location, "Total"))
  title3 <- which_treatment(df3_east)
  
  legend_colors <- c("Forbs" = "#B2DF8A", "Grasses" = "#1F78B4", "NVGround" 
                     = "#FB9A99", "Other" = "#A6CEE3", "Woody" = "#33A02C")
  
  if (inside == TRUE){
    tr <- "Inside"
    
    plot1 <- tidy_table1 %>% filter(str_detect(Location, tr)) %>% 
      ggplot(aes(x = reorder(Category, mean), y = mean, fill = Category)) + 
      geom_col(position="dodge") + labs(title = 
                                          glue("{tr} Plots Plant Cover Averages"), 
                                        x = glue("{title1}"),
                                        y = "Percent Averages") + 
      theme(legend.position = "none", axis.text.x = element_blank(),
            axis.ticks.x = element_blank()) + 
      geom_errorbar(aes(ymin = mean - SE, ymax = mean + SE), position =
                      position_dodge(), color = "black") +
      scale_fill_manual(values = legend_colors) +
      coord_cartesian(ylim = c(0, 100)) + theme(text = element_text(size = 12))
    
    
    plot2 <- tidy_table2 %>% filter(str_detect(Location, tr)) %>% 
      ggplot(aes(x = reorder(Category, mean), y = mean, fill = Category)) + 
      geom_col(position="dodge") + labs(x = glue("{title2}")) + 
      theme(legend.position = "none", axis.title.y = element_blank(), 
            axis.text.x = element_blank(), axis.ticks.x = element_blank()) + 
      geom_errorbar(aes(ymin = mean - SE, ymax = mean + SE), position =
                      position_dodge(), color = "black") +
      scale_fill_manual(values = legend_colors) +
      coord_cartesian(ylim = c(0, 100)) + theme(text = element_text(size = 12))
    
    plot3 <- tidy_table3 %>% filter(str_detect(Location, tr)) %>% 
      ggplot(aes(x = reorder(Category, mean), y = mean, fill = Category)) + 
      geom_col(position="dodge") + scale_fill_manual(values = legend_colors, 
                                                     name = "Plant Category")  +
      geom_errorbar(aes(ymin = mean - SE, ymax = mean + SE), position =
                      position_dodge(), color = "black") +
      labs(x = glue("{title3}")) + theme(axis.title.y = element_blank(), 
                                         axis.text.x = element_blank(),
                                         axis.ticks.x = element_blank()) +
      coord_cartesian(ylim = c(0, 100)) + theme(text = element_text(size = 12))
    
  }
  else {
    tr <- "Outside"
    
    plot1 <- tidy_table1 %>% filter(str_detect(Location, tr)) %>% 
      ggplot(aes(x = reorder(Category, mean), y = mean, color = Category)) + 
      geom_col(position="dodge", stat = "identity", fill = "white", 
               linewidth = 1.25) + labs(title = 
                                        glue("{tr} Plots Plant Cover Averages"), 
                                        x = glue("{title1}"),
                                        y = "Percent Averages") + 
      theme(legend.position = "none", axis.text.x = element_blank(),
            axis.ticks.x = element_blank()) + 
      geom_errorbar(aes(ymin = mean - SE, ymax = mean + SE, group = Category),
                        position = position_dodge(), color = "black") +
      scale_color_manual(values = legend_colors) +
      coord_cartesian(ylim = c(0, 100)) + theme(text = element_text(size = 12))
    
    
    plot2 <- tidy_table2 %>% filter(str_detect(Location, tr)) %>% 
      ggplot(aes(x = reorder(Category, mean), y = mean, color = Category)) + 
      geom_col(position="dodge", stat = "identity", fill = "white", 
               linewidth = 1.25) + labs(x = glue("{title2}")) + 
      theme(legend.position = "none", axis.title.y = element_blank(), 
            axis.text.x = element_blank(), axis.ticks.x = element_blank()) + 
      geom_errorbar(aes(ymin = mean - SE, ymax = mean + SE, group = Category), 
                    position = position_dodge(), color = "black") +
      scale_color_manual(values = legend_colors) +
      coord_cartesian(ylim = c(0, 100)) + theme(text = element_text(size = 12))
    
    plot3 <- tidy_table3 %>% filter(str_detect(Location, tr)) %>% 
      ggplot(aes(x = reorder(Category, mean), y = mean, color = Category)) + 
      geom_col(position="dodge", stat = "identity", fill = "white", 
               linewidth = 1.25) + scale_color_manual(values = legend_colors, 
                                                     name = "Plant Category")  +
      geom_errorbar(aes(ymin = mean - SE, ymax = mean + SE, group = Category), 
                    position = position_dodge(), color = "black") +
      labs(x = glue("{title3}")) + theme(axis.title.y = element_blank(), 
                                         axis.text.x = element_blank(),
                                         axis.ticks.x = element_blank()) +
      guides(color = guide_legend(override.aes = list(fill = legend_colors)))+ 
      coord_cartesian(ylim = c(0, 100)) + theme(text = element_text(size = 12))
  }


  plot4 <- (plot1 + plot2 + plot3) + plot_annotation(tag_levels = "A") & 
    theme(plot.tag = element_text(size = 12, face = "bold"), 
          plot.tag.position = "bottomleft")
  
  return(plot4)
}






#model function for hollow-outside, solid-inside figure style
model <- function(df1_east, df1_west, df2_east, df2_west) {
  tots_table1 <- pivot_longer(cover_avgs(df1_east, df1_west),
                              cols = c(contains("_mean"), contains("_SE")),
                              names_to = c("Category", ".value"),
                              names_sep = "_%_") %>% 
    filter(str_detect(Location, "Total"))
  
  tots_table2 <- pivot_longer(cover_avgs(df2_east, df2_west),
                              cols = c(contains("_mean"), contains("_SE")),
                              names_to = c("Category", ".value"),
                              names_sep = "_%_") %>% 
    filter(str_detect(Location, "Total"))
  
  df1_name <- which_treatment(df1_east)
  df2_name <- which_treatment(df2_east)
  
  legend_colors <- c("Forbs" ="#B2DF8A", "Grasses" =  "#1F78B4", "NVGround" 
                     = "#FB9A99", "Other" = "#A6CEE3", "Woody" = "#33A02C")
  
  tots_df <- bind_rows(df1 = tots_table1, df2 = tots_table2, 
                       .id = "Plots") 
  
  tots_df$Plots <- str_replace_all(tots_df$Plots,"df1", {{df1_name}}) 
  tots_df$Plots <- str_replace_all(tots_df$Plots, "df2", {{df2_name}})
  
  plot1 <- tots_df %>% 
    ggplot(aes(x = Plots, y = mean, fill = interaction(list(Location,
                                                            fct_reorder(Category, mean)),
                                                       sep = " "), color = Category)) + 
    geom_col(position="dodge", stat = "identity", linewidth = 1.25) + 
    geom_errorbar(aes(ymin = mean - SE, ymax = mean + SE), position =
                    position_dodge(), color = "black") + 
    geom_col_pattern(position="dodge",
                        pattern = "stripe", 
                        pattern_color = "gray",
                        pattern_fill = "gray",
                        fill = "white",
                        linewidth = 1.25, 
                        pattern_alpha = 0.2) +
    labs(title = glue("{df1_name} and {df2_name} Treatment Plots
                                      Plant Cover Total Averages"), 
         y = "Percent Averages") + 
    scale_fill_manual(values = c("Inside Total Forbs" = "#B2DF8A", 
                                 "Inside Total Grasses" = "#1F78B4", 
                                 "Inside Total NVGround" = "#FB9A99", 
                                 "Inside Total Other" = "#A6CEE3", 
                                 "Inside Total Woody" = "#33A02C", 
                                 "Outside Total Forbs" = "white",
                                 "Outside Total Grasses" = "white",
                                 "Outside Total NVGround" = "white", 
                                 "Outside Total Other" = "white", 
                                 "Outside Total Woody" = "white"), 
                      name = "Plant Categories") + 
    scale_colour_manual(values = legend_colors, guide = "none") + 
    guides(fill = guide_legend(override.aes = list(color = c("#A6CEE3",
                                                             "#A6CEE3",
                                                             "#B2DF8A",
                                                             "#B2DF8A",
                                                             "#1F78B4", 
                                                             "#1F78B4",
                                                             "#33A02C",
                                                             "#33A02C",
                                                             "#FB9A99",
                                                             "#FB9A99")))) +
    coord_cartesian(ylim = c(0, 1)) + theme(text = element_text(size = 12))
  
  return(plot1 + plot_annotation(tag_levels = "A") & 
           theme(plot.tag = element_text(size = 12, face = "bold"), 
                 plot.tag.position = "bottomleft"))
}



##figures displaying species richness (faceted)
alt_within_richness_fig <- function(df_east, df_west) {
  inside_df <- species_richness(unite_treatment(df_east, df_west), count = FALSE)
  outside_df <- species_richness(unite_treatment(df_east, df_west, FALSE), count = FALSE) 
  
  title <- which_treatment(df_east)
  legend_colors <- c("F" = "#B2DF8A", "G" = "#1F78B4", "O" = "#A6CEE3", 
                     "W" = "#33A02C")
  legend_labels <- c("F" = "Forb",  "W" = "Woody", "G" = "Grass", "O" = "Other (Sedges/Ferns)")
  
  plot1 <- ggplot(inside_df, aes(x = reorder(Key, Count), y = Count, fill = Key)) + 
    geom_col(position="dodge") + scale_fill_manual(values = legend_colors, 
                                                   labels = legend_labels, 
                                                   name = "Plant Category") + 
    labs(x = "Inside") + theme(axis.text.x = element_blank(),  
                               axis.title.y = element_blank(), 
                               axis.ticks.x = element_blank()) + 
    coord_cartesian(ylim = c(0, 15)) + theme(text = element_text(size = 12))

  
  plot2 <- ggplot(outside_df, aes(x = reorder(Key, Count), y = Count, color = Key)) + 
    geom_col(position="dodge", stat = "identity", fill = "white", 
             linewidth = 1.25) + labs(x = "Outside", 
                                      title = glue("{title} Species Richness")) + 
    scale_color_manual(values = legend_colors) + 
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
          legend.position = "none") + coord_cartesian(ylim = c(0, 15)) +
    theme(text = element_text(size = 12))

  
  plot3 <- (plot2 + plot1) + plot_annotation(tag_levels = "A") & 
    theme(plot.tag = element_text(size = 12, face = "bold"), 
          plot.tag.position = "bottomleft")
  
  return(plot3)
}




#faceted 
btwn_richness_fig <- function(df1_east, df1_west, df2_east, df2_west, df3_east, 
                              df3_west, inside = TRUE) {
  title1 <- which_treatment(df1_east)
  title2 <- which_treatment(df2_east)
  title3 <- which_treatment(df3_east)
  
  legend_colors <- c("F" = "#B2DF8A", "G" = "#1F78B4", "O" = "#A6CEE3", 
                     "W" = "#33A02C")
  legend_labels <- c("F" = "Forb",  "W" = "Woody", "G" = "Grass", "O" = "Other (Sedges/Ferns)")
  
  if (inside == TRUE){
    inside_df1 <- species_richness(unite_treatment(df1_east, df1_west), count = FALSE)
    inside_df2 <- species_richness(unite_treatment(df2_east, df2_west), count = FALSE)
    inside_df3 <- species_richness(unite_treatment(df3_east, df3_west), count = FALSE)
    plot1 <- ggplot(inside_df1, aes(x = reorder(Key, Count), y = Count, fill = Key)) + 
      geom_col(position="dodge") + labs(title = "Species Richness - Inside Plots", 
                                        x = glue("{title1}")) + 
      scale_fill_manual(values = legend_colors) + theme(legend.position = "none", 
                                                    axis.text.x = element_blank(),
                                                    axis.ticks.x = element_blank()) + 
      coord_cartesian(ylim = c(0, 15)) + theme(text = element_text(size = 12))
    plot2 <- ggplot(inside_df2, aes(x = reorder(Key, Count), y = Count, fill = Key)) + 
      geom_col(position="dodge") + labs(x = glue("{title2}")) + 
      scale_fill_manual(values = legend_colors) + theme(legend.position = "none", 
                                                    axis.text.x = element_blank(),
                                                    axis.ticks.x = element_blank(),
                                                    axis.title.y = element_blank()) +
      coord_cartesian(ylim = c(0, 15)) + theme(text = element_text(size = 12))
    plot3 <- ggplot(inside_df3, aes(x = reorder(Key, Count), y = Count, fill = Key)) + 
      geom_col(position="dodge") + labs(x = glue("{title3}")) + 
      scale_fill_manual(values = legend_colors, labels = legend_labels, 
                        name = "Plant Category") + theme(axis.text.x = element_blank(),
                                                    axis.ticks.x = element_blank(),
                                                    axis.title.y = element_blank()) + 
      coord_cartesian(ylim = c(0, 15)) + theme(text = element_text(size = 12))
  }
  else {
    outside_df1 <- species_richness(unite_treatment(df1_east, df1_west, FALSE), count = FALSE) 
    outside_df2 <- species_richness(unite_treatment(df2_east, df2_west, FALSE), count = FALSE) 
    outside_df3 <- species_richness(unite_treatment(df3_east, df3_west, FALSE), count = FALSE) 
    plot1 <- ggplot(outside_df1, aes(x = reorder(Key, Count), y = Count, color = Key)) + 
      geom_col(position="dodge", stat = "identity", fill = "white", 
               linewidth = 1.25) +
      labs(title = "Species Richness - Outside Plots", 
                                        x = glue("{title1}")) + 
      scale_color_manual(values = legend_colors) + theme(legend.position = "none", 
                                                    axis.text.x = element_blank(),
                                                    axis.ticks.x = element_blank()) + 
      coord_cartesian(ylim = c(0, 15)) + theme(text = element_text(size = 12))
    plot2 <- ggplot(outside_df2, aes(x = reorder(Key, Count), y = Count, color = Key)) + 
      geom_col(position="dodge", stat = "identity", fill = "white", 
               linewidth = 1.25) + labs(x = glue("{title2}")) +
      scale_color_manual(values = legend_colors) + theme(legend.position = "none", 
                                                    axis.text.x = element_blank(),
                                                    axis.ticks.x = element_blank(),
                                                    axis.title.y = element_blank()) + 
      coord_cartesian(ylim = c(0, 15)) + theme(text = element_text(size = 12))
    plot3 <- ggplot(outside_df3, aes(x = reorder(Key, Count), y = Count, color = Key)) + 
      geom_col(position="dodge", stat = "identity", fill = "white", 
               linewidth = 1.25) + labs(x = glue("{title3}")) + 
      scale_color_manual(values = legend_colors, labels = legend_labels, 
                        name = "Plant Category") + 
      theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
            axis.title.y = element_blank()) + 
      guides(color = guide_legend(override.aes = list(fill = legend_colors))) + 
      coord_cartesian(ylim = c(0, 15)) + theme(text = element_text(size = 12))
  }
  plot4 <- (plot1 + plot2 + plot3) + plot_annotation(tag_levels = "A") & 
    theme(plot.tag = element_text(size = 12, face = "bold"), 
          plot.tag.position = "bottomleft")
  return(plot4)
}







#figure of proportion of invasive species for only woodies (faceted)
woody_invasive_fig <- function(df1_east, df1_west, df2_east, df2_west, df3_east, 
                               df3_west) {
  title1 <- which_treatment(df1_east)
  title2 <- which_treatment(df2_east)
  title3 <- which_treatment(df3_east)
  
  df1_row_in <- invasive_percents(species_richness(unite_treatment(df1_east, df1_west))) %>% 
    filter(str_detect(Key, "W")) %>% mutate(Plot = "Inside")
  df1_row_out <- invasive_percents(species_richness(unite_treatment(df1_east, df1_west, FALSE))) %>% 
    filter(str_detect(Key, "W")) %>% mutate(Plot = "Outside")
  df1_props <- bind_rows(df1_row_in, df1_row_out)
  
  df2_row_in <- invasive_percents(species_richness(unite_treatment(df2_east, df2_west))) %>% 
    filter(str_detect(Key, "W")) %>% mutate(Plot = "Inside")
  df2_row_out <- invasive_percents(species_richness(unite_treatment(df2_east, df2_west, FALSE))) %>% 
    filter(str_detect(Key, "W")) %>% mutate(Plot = "Outside")
  df2_props <- bind_rows(df2_row_in, df2_row_out)
  
  df3_row_in <- invasive_percents(species_richness(unite_treatment(df3_east, df3_west))) %>% 
    filter(str_detect(Key, "W")) %>% mutate(Plot = "Inside")
  df3_row_out <- invasive_percents(species_richness(unite_treatment(df3_east, df3_west, FALSE))) %>% 
    filter(str_detect(Key, "W")) %>% mutate(Plot = "Outside")
  df3_props <- bind_rows(df3_row_in, df3_row_out)
  
  
  plot1 <- ggplot(df1_props, aes(x = Plot, y = Percent_Invasive, fill = Plot)) + 
    geom_col(position="dodge", stat = "identity", color = "#33A02C", 
             linewidth = 1.25) + 
    labs(title = "Proportions of Invasive Woody Species", x = glue("{title1}"), 
         y = "Proportion Invasive") + 
    scale_fill_manual(values = c("Inside" = "#33A02C", "Outside" = "white")) + 
    theme(legend.position = "none", axis.text.x = element_blank(),
          axis.ticks.x = element_blank()) + coord_cartesian(ylim = c(0, 100)) +
    theme(text = element_text(size = 12))
 
  plot2 <- ggplot(df2_props, aes(x = Plot, y = Percent_Invasive, fill = Plot)) + 
    geom_col(position="dodge", stat = "identity", color = "#33A02C", 
             linewidth = 1.25) + labs(x = glue("{title2}")) + 
    scale_fill_manual(values = c("Inside" = "#33A02C", "Outside" = "white")) + 
    theme(legend.position = "none", axis.text.x = element_blank(), 
          axis.ticks.x = element_blank(), axis.title.y = element_blank()) +
    coord_cartesian(ylim = c(0, 100)) + theme(text = element_text(size = 12))
  plot3 <- ggplot(df3_props, aes(x = Plot, y = Percent_Invasive, fill = Plot)) + 
    geom_col(position="dodge", stat = "identity", color = "#33A02C", 
             linewidth = 1.25) +  labs(x = glue("{title3}"), fill = "Plot") + 
    scale_fill_manual(values = c("Inside" = "#33A02C", "Outside" = "white")) + 
    guides(fill = guide_legend(override.aes = list(color = "#33A02C"))) +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
          axis.title.y = element_blank()) + coord_cartesian(ylim = c(0, 100)) +
    theme(text = element_text(size = 12))
  
  
  plot4 <- (plot1 + plot2 + plot3) + plot_annotation(tag_levels = "A") & 
    theme(plot.tag = element_text(size = 12, face = "bold"), 
          plot.tag.position = "bottomleft")
  return(plot4)
}





##avg heights of woody species present in all three treatment groups
common_in_w_fig <- function(df1_east, df1_west, df2_east, df2_west, df3_east, 
                            df3_west) {
  df1_in <- unite_treatment(df1_east, df1_west)
  df1_in_richness <- species_richness(df1_in, quiet = TRUE)
  df1_with_heights <- avg_inside_heights(df1_in_richness, df1_in)
  
  df2_in <- unite_treatment(df2_east, df2_west)
  df2_in_richness <- species_richness(df2_in, quiet = TRUE)
  df2_with_heights <- avg_inside_heights(df2_in_richness, df2_in)
  
  df3_in <- unite_treatment(df3_east, df3_west)
  df3_in_richness <- species_richness(df3_in, quiet = TRUE)
  df3_with_heights <- avg_inside_heights(df3_in_richness, df3_in)
  
  title1 <- which_treatment(df1_east)
  title2 <- which_treatment(df2_east)
  title3 <- which_treatment(df3_east)
  
  common_spp <- spp_in_common(df1_with_heights, df2_with_heights, df3_with_heights)
  
  df1_common <- df1_with_heights %>% filter(Species %in% common_spp)
  df2_common <- df2_with_heights %>% filter(Species %in% common_spp)
  df3_common <- df3_with_heights %>% filter(Species %in% common_spp)
  
  legend_colors <- c("Ash" = "#FB9A99", "Cherry" = "#FDBF6F", "MfRose" = "#FF7F00",  
                     "VACreep"= "#E31A1C", "PoisonIvy" = "#CAB2D6")
  
  plot1 <- ggplot(df1_common, aes(x = reorder(Species, Average_Height), 
                                  y = Average_Height, fill = Species)) + 
    geom_col(position="dodge") + geom_errorbar(aes(ymin = Average_Height - SE,
                                                   ymax = Average_Height + SE), 
                                               position = "dodge", color = "black") + 
    labs(title = "Average Heights of Common Woody Species - Inside Plots", 
                                      x = glue("{title1}"), y = "Average Height (cm)") + 
    scale_fill_manual(values = legend_colors) + theme(legend.position = "none", 
                                                  axis.text.x = element_blank(),
                                                  axis.ticks.x = element_blank()) + 
    coord_cartesian(ylim = c(0, 100)) + theme(text = element_text(size = 12))
  plot2 <- ggplot(df2_common, aes(x = reorder(Species, Average_Height), 
                                  y = Average_Height, fill = Species)) + 
    geom_col(position="dodge") + geom_errorbar(aes(ymin = Average_Height - SE, 
                                                   ymax = Average_Height + SE), 
                                               position = "dodge", color = "black") + 
    labs(x = glue("{title2}")) + 
    scale_fill_manual(values = legend_colors) + theme(legend.position = "none", 
                                                  axis.text.x = element_blank(),
                                                  axis.ticks.x = element_blank(),
                                                  axis.title.y = element_blank()) +
    coord_cartesian(ylim = c(0, 100)) + theme(text = element_text(size = 12))
  plot3 <- ggplot(df3_common, aes(x = reorder(Species, Average_Height), 
                                  y = Average_Height, fill = Species)) + 
    geom_col(position="dodge") + geom_errorbar(aes(ymin = Average_Height - SE, 
                                                   ymax = Average_Height + SE), 
                                               position = "dodge", color = "black") + 
    labs(x = glue("{title3}")) + 
    scale_fill_manual(values = legend_colors) + theme(axis.text.x = element_blank(),
                                                  axis.ticks.x = element_blank(),
                                                  axis.title.y = element_blank()) + 
    coord_cartesian(ylim = c(0, 100)) + theme(text = element_text(size = 12))
  
  
  plot4 <- (plot1 + plot2 + plot3) + plot_annotation(tag_levels = "A") & 
    theme(plot.tag = element_text(size = 12, face = "bold"), 
          plot.tag.position = "bottomleft")
  return(plot4)
}



#####legend is alphabetical, not modified by x variables order
##from most to least preferred: woody and forb spp browse comparing all three dfs
outside_brow_fig <- function(df1_east, df1_west, df2_east, df2_west, df3_east, 
                                  df3_west, Woody = TRUE) {
  title1 <- which_treatment(df1_east)
  title2 <- which_treatment(df2_east)
  title3 <- which_treatment(df3_east)
  
  df1_outside  <- unite_treatment(df1_east, df1_west, FALSE)
  df1_out_richness  <- species_richness(df1_outside, quiet = TRUE)
  df1_brow <- percent_browse_outside(df1_outside, df1_out_richness, FALSE)
  df2_outside  <- unite_treatment(df2_east, df2_west, FALSE)
  df2_out_richness  <- species_richness(df2_outside, quiet = TRUE)
  df2_brow <- percent_browse_outside(df2_outside, df2_out_richness, FALSE)
  df3_outside  <- unite_treatment(df3_east, df3_west, FALSE)
  df3_out_richness  <- species_richness(df3_outside, quiet = TRUE)
  df3_brow <- percent_browse_outside(df3_outside, df3_out_richness, FALSE)
  
  
  if (!Woody){
    vegetation <- "F"
    type <- "Forb"
    legend_colors <- c("#FB9A99", "#FDBF6F", "#FF7F00","#E31A1C", "#CAB2D6", 
                       "#6A3D9A", "#B15928")
  }
  else {
    vegetation <- "W"
    type <- "Woody"
    legend_colors <- c("Ash" = "#FB9A99", "Cherry" = "#FDBF6F", 
                       "MfRose" = "#FF7F00", "VACreep"= "#E31A1C", 
                       "PoisonIvy" = "#CAB2D6", "Rubus" = "#6A3D9A",
                       "BlkCherry" = "#B15928", "JBarb" = "#B2DF8A")
  }
  
  filtered_df1 <- df1_brow %>% filter(Key == vegetation)
  filtered_df2 <- df2_brow %>% filter(Key == vegetation)
  filtered_df3 <- df3_brow %>% filter(Key == vegetation)
  
  plotA <- ggplot((filtered_df1), aes(x = reorder(Species, -Percent_Browse), 
                                      y = Percent_Browse, color = Species)) + 
    geom_col(position="dodge", stat = "identity", fill = "white", 
             linewidth = 1.5) + 
    labs(title = glue("Percent Browse of Common {type} Species - Outside Plots"), 
                                      x = glue("{title1}"), y = "Percent Browse") + 
    scale_color_manual(values = legend_colors) + theme(axis.text.x = element_blank(),
                                                  axis.ticks.x = element_blank()) + 
    coord_cartesian(ylim = c(0, 100)) + theme(text = element_text(size = 12))
  plotB <- ggplot((filtered_df2), aes(x = reorder(Species, -Percent_Browse),
                      y = Percent_Browse, color = Species)) + 
    geom_col(position="dodge", stat = "identity", fill = "white", 
             linewidth = 1.5) + labs(x = glue("{title2}")) + 
    scale_color_manual(values = legend_colors) + theme(axis.text.x = element_blank(),
                                                  axis.ticks.x = element_blank(),
                                                  axis.title.y = element_blank()) +
    coord_cartesian(ylim = c(0, 100)) + theme(text = element_text(size = 12))
  plotC <- ggplot((filtered_df3), aes(x = reorder(Species, -Percent_Browse), 
                                       y = Percent_Browse, color = Species)) + 
  geom_col(position="dodge", stat = "identity", fill = "white", 
           linewidth = 1.5) + labs(x = glue("{title3}")) + 
  scale_color_manual(values = legend_colors) + theme(axis.text.x = 
                                                       element_blank(), 
                                                axis.ticks.x = element_blank(), 
          axis.title.y = element_blank()) + coord_cartesian(ylim = c(0, 100)) +
    theme(text = element_text(size = 12))
  

  
  plot1 <- plotA + 
    guides(color = guide_legend(override.aes = list(fill = 
                    unlist(dynamic_legend_colors(filtered_df1, 
                                                  legend_colors)))))
  
  plot2 <- plotB + 
    guides(color = guide_legend(override.aes = list(fill = 
                    unlist(dynamic_legend_colors(filtered_df2, 
                                                  legend_colors)))))
  
  plot3 <- plotC + 
    guides(color = guide_legend(override.aes = list(fill = 
                                unlist(dynamic_legend_colors(filtered_df3, 
                                                      legend_colors))))) 
  
  plot4 <- (plot1 + plot2 + plot3) + plot_annotation(tag_levels = "A") & 
    theme(plot.tag = element_text(size = 12, face = "bold"), 
          plot.tag.position = "bottomleft")
  
  return(plot4)
}


#alphabetical list for legend. shrinks size of legend colors to match guide_legend(override.aes)
dynamic_legend_colors <- function(filtered_df, named_list_legend_colors){
  ordered_legend_fill <- list()
  ordered_legend_colors <- sort_by(named_list_legend_colors,
                                   names(named_list_legend_colors))

  for (name in names(ordered_legend_colors)){
    if (name %in% filtered_df$Species){
      ordered_legend_fill <- append(ordered_legend_fill, 
                                    ordered_legend_colors[name])
    }
  }
  return(ordered_legend_fill)
}




##inside, outside browsed, and outside unbrowsed heights per df: all woody spp combined
#faceted
combined_avg_heights_fig <- function(df_east, df_west){
  title <- which_treatment(df_east)
  
  inside <- unite_treatment(df_east, df_west, TRUE)
  outside <- unite_treatment(df_east, df_west, FALSE)
  inside_richness <- species_richness(inside, quiet = TRUE)
  outside_richness <- species_richness(outside, quiet = TRUE)
  inside_heights <- avg_inside_heights(inside_richness, inside)
  outside_heights <- na.omit(percent_browse_outside(outside, outside_richness, quiet = TRUE))
  
  legend_colors <- c("Ash" = "#FB9A99", "Cherry" = "#FDBF6F", 
                     "MfRose" = "#FF7F00", "VACreep"= "#E31A1C", 
                     "PoisonIvy" = "#CAB2D6", "Rubus" = "#6A3D9A",
                     "BlkCherry" = "#B15928", "BlkhawVib" = "lightblue", "BlkGum"
                     = "cadetblue3", "FlowDog" = "goldenrod", "Sass" = "indianred3",
                     "JBarb" = "#B2DF8A")
  
  plot1 <- ggplot(inside_heights, aes(x = reorder(Species, Average_Height), 
                                      y = Average_Height, fill = Species)) + 
    geom_col(position="dodge") + geom_errorbar(aes(ymin = Average_Height - SE,
                                                   ymax = Average_Height + SE), 
                                          position = "dodge", color = "black") + 
    labs(title = glue("Average Woody Height by Transect and Browse - {title}"), 
                          x = "Inside Transects", y = "Average Height (cm)") + 
    scale_fill_manual(values = legend_colors) + theme(legend.position = "none") + 
    scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
    coord_cartesian(ylim = c(0, 100)) + theme(text = element_text(size = 12))
  plot2 <- ggplot((outside_heights %>% filter(Browse == 1)), aes(x = 
                                                    reorder(Species, AvgHeight),
                                                                 y = AvgHeight, 
                                                                 color = Species)) + 
    geom_col(position="dodge", stat = "identity", fill = "white", 
             linewidth = 1.25) + geom_errorbar(aes(ymin = AvgHeight - SE, 
                                                   ymax = AvgHeight + SE), 
                                        position = "dodge", color = "black") + 
    labs(x = "Outside Transects - Browsed") + 
    scale_color_manual(values = legend_colors) + theme(legend.position = "none",
                                                  axis.title.y = element_blank()) +
    coord_cartesian(ylim = c(0, 100)) + theme(text = element_text(size = 12))
  plot3 <- ggplot((outside_heights %>% filter(Browse == 0)), aes(x = 
                                                      reorder(Species, AvgHeight), 
                                                                 y = AvgHeight, 
                                                                 color = Species)) + 
    geom_col(position="dodge", stat = "identity", fill = "white", 
             linewidth = 1.25) + geom_errorbar(aes(ymin = AvgHeight - SE, 
                                                   ymax = AvgHeight + SE), 
                                        position = "dodge", color = "black") + 
    labs(x = "Outside Transects - Unbrowsed") + 
    scale_color_manual(values = legend_colors) + theme(legend.position = "none",
                                                  axis.title.y = element_blank()) + 
    coord_cartesian(ylim = c(0, 100)) + theme(text = element_text(size = 12))
  
  plot4 <- (plot1 + plot2 + plot3) + plot_annotation(tag_levels = "A") & 
    theme(plot.tag = element_text(size = 12, face = "bold"), 
          plot.tag.position = "bottomleft")
  return(plot4)
}

