#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(sf)
library(ggplot2)
library(readr)
library(dplyr)
library(ggiraph)
library(RColorBrewer)

shinyServer(function(input, output, session){
  
  output$scatterPlotCause <- renderGirafe({
    
    # Create a function for scatterplot for "Cause" using 3 inputs
    scatterPlot_func <- function(colorVar, year, varName){
      # Read shape file
      world_sf <-  st_read("data/world_shape_file/", quiet = TRUE)
      
      dataSet = if(varName == "Literacy Level"){
        "literacy"
      } else if (varName == "Production"){
        "production"
      } else if (varName == "Unemployment Rate"){
        "unemployment"
      }
      
      filename = paste(
        "data/cause/rrc-consumption-",
        dataSet,
        ".csv",
        sep = ""
      )
      
      dat <- read_csv(filename,
                      show_col_types = FALSE)
      
      world_sf <- world_sf %>%
        left_join(dat,
                  by = c("NAME" = "Country")) %>%
        mutate(toolt = paste(paste(toupper(NAME), sep = ""),
                             paste("Consumption: ", round(Consumption, 2), " L per capita", sep = ""),
                             if(varName == "Literacy Level"){
                               paste("Literacy rate: ", round(Literacy_Rate, 2), "%", sep = "")
                             } else if (varName == "Production"){
                               paste("Production: ", Production_Value, sep ="")
                             } else if (varName == "Unemployment Rate"){
                               paste("Unemployment rate: ", Unemployment, "%", sep = "")
                             }, 
                             sep = '\n'))
      
      world_sf_year <- world_sf %>%
        filter(Year == year)
      
      causePlot <- ggplot(data = world_sf_year,
                          aes(x = if(varName == "Literacy Level"){
                            Literacy_Rate
                          } else if (varName == "Production"){
                            Production_Value
                          } else if (varName == "Unemployment Rate"){
                            Unemployment
                          },
                          y = Consumption,
                          color = if(colorVar == "Region"){
                            Region
                          } else if (colorVar == "Major Religion"){
                            Major_Religion
                          })) +
        geom_point_interactive(size = 2.3, 
                               alpha = 0.7,
                               tooltip = world_sf_year$toolt,
                               data_id = world_sf_year$ISO3) +
        theme_classic() +
        theme(axis.line.x = element_line(arrow = arrow(type = "closed", length = unit(0.3, "cm"))),
              axis.line.y = element_line(arrow = arrow(type = "closed", length = unit(0.3, "cm"))),
              aspect.ratio = 1,
              legend.position = "top",
              legend.title = element_text(size = 11, face = 'bold'), 
              legend.text = element_text(size = 10),
              axis.title.x = element_text(size = 13, face = 'bold'),
              axis.title.y = element_text(size = 13, face = 'bold'),
              axis.text.x = element_text(size = 11, face = 'bold'),
              axis.text.y = element_text(size = 11, face = 'bold')) +
        xlab(varName) +
        guides(color = guide_legend(ncol = 3,
                                    title = colorVar)) +
        scale_color_brewer(name = "Color", palette = "Accent")
      
      plot1 <- girafe(ggobj = causePlot,
                  opts_tooltip(opacity = .7,
                               offx = 30, offy = -20,
                               use_fill = TRUE,
                               use_stroke = TRUE,
                               delay_mouseout = 1500))
      plot1 <- girafe_options(plot1,
                              opts_selection(type = "none"),
                              opts_sizing(rescale = FALSE),
                              width = 20.0,
                              height = 20.0)
      plot1
    }
    
    scatterPlot_func(input$colorCause,
                     input$yearCause,
                     input$cause)
    })
  
  
  
  output$mapCause <- renderGirafe({
    
    # Create a function for plotting choropleth for "Cause" using 2 inputs
    map_func <- function(year, varName){
      
      world_sf <-  st_read("data/world_shape_file/", quiet = TRUE)
      
      dataSet = if(varName == "Literacy Level"){
        "literacy"
      } else if (varName == "Production"){
        "production"
      } else if (varName == "Unemployment Rate"){
        "unemployment"
      }
      
      filename = paste(
        "data/cause/rrc-consumption-",
        dataSet,
        ".csv",
        sep = ""
      )
      
      dat <- read_csv(filename,
                      show_col_types = FALSE)
      
      world_sf <- world_sf %>%
        full_join(dat,
                  by = c("NAME" = "Country")) %>%
        mutate(toolt = paste(paste(toupper(NAME), sep = ""),
                             paste("Consumption: ", round(Consumption, 2), " L per capita", sep = ""),
                             if(varName == "Literacy Level"){
                               paste("Literacy rate: ", round(Literacy_Rate, 2), "%", sep = "")
                               } else if (varName == "Production"){
                                 paste("Production: ", Production_Value, sep ="")
                                 } else if (varName == "Unemployment Rate"){
                                   paste("Unemployment rate: ", Unemployment, "%", sep = "")
                                   }, 
                             sep = '\n'))
      
      world_sf_year <- world_sf %>%
        filter(Year == year)
      
      color_palette <- colorRampPalette(brewer.pal(n = 9, name = "Greens"))(300)
      
      causeMap <- ggplot(data = world_sf_year, 
                         aes(fill = Consumption),
                         fill = color_palette) +
        guides(size = guide_legend(title = varName)) +
        geom_sf(show.legend = TRUE,
                size = 0.2) +
        scale_fill_distiller(direction = 1,
                             palette = "Purples",
                             na.value = "transparent") +
        geom_point_interactive(data = world_sf_year,
                               aes(x = Longitude, 
                                   y = Latitude,
                                   size = (if(varName == "Literacy Level"){
                                     Literacy_Rate
                                     } else if (varName == "Production"){
                                       Production_Value
                                       } else if (varName == "Unemployment Rate"){
                                         Unemployment
                                         }
                                     )),
                               color = "#EC9706",
                               tooltip =   world_sf_year$toolt,
                               data_id = world_sf_year$ISO3) +
        scale_size_continuous(range = c(0.1, 2)) +
        scale_color_brewer(name = "Color", palette = "Dark2") +
        theme(legend.position = "top",
              panel.background = element_rect(fill = "#FAF0E6"),
              legend.title = element_text(size = 8, face = 'bold'), 
              legend.text = element_text(size = 6),
              axis.title.x = element_text(size = 9, face = 'bold'),
              axis.title.y = element_text(size = 9, face = 'bold'),
              axis.text.x = element_text(size = 8, face = 'bold'),
              axis.text.y = element_text(size = 8, face = 'bold'))
      
      plot2 <- girafe(ggobj = causeMap,
                      opts_tooltip(opacity = .7,
                                   offx = 30, offy = -20,
                                   use_fill = TRUE,
                                   use_stroke = TRUE,
                                   delay_mouseout = 1500))
      
      plot2 <- girafe_options(plot2,
                              opts_selection(type = "none"),
                              opts_sizing(rescale = TRUE, width = 1))
      
      plot2
    }
    
    map_func(input$yearCause,
             input$cause)
    })
  
  output$scatterPlotEffect <- renderGirafe({
    
    # Create a function for scatterplot for "Effect" using 3 inputs
    scatterPlot_func <- function(colorVar, year, varName){
      # Read shape file
      world_sf <-  st_read("data/world_shape_file/", quiet = TRUE)
      
      dataSet = if(varName == "Life Expectancy"){
        "life"
        } else if (varName == "Life Satisfaction"){
          "happiness"
          } else if (varName == "Liver Cirrhosis Deaths"){
            "cirrhosis2015"
            } else if (varName == "Traffic Crashes"){
              "road_death"
              }
      
      filename = paste(
        "data/effect/rrc-consumption-",
        dataSet,
        ".csv",
        sep = ""
      )
      
      dat <- read_csv(filename,
                      show_col_types = FALSE)
      
      world_sf <- world_sf %>%
        left_join(dat,
                  by = c("NAME" = "Country")) %>%
        mutate(toolt = paste(paste(toupper(NAME), sep = ""),
                             paste("Consumption: ", round(Consumption, 2), " L per capita", sep = ""),
                             if(varName == "Life Expectancy"){
                               paste("Life expectancy: ", round(Life_Expectancy, 2), " years", sep = "")
                               } else if (varName == "Life Satisfaction"){
                                 paste("Life satisfaction: ", round(Happiness, 2), " (Cantril ladder)", sep = "")
                                 } else if (varName == "Liver Cirrhosis Deaths"){
                                   paste("Alcohol-attributable fraction (15+): ", round(Cirrhosis, 1), "%", sep = "")
                                   }else if (varName == "Traffic Crashes"){
                                     paste("Alcohol-attributable road crashes: ", round(Road_Death, 1), "% of all road crashes", sep = "")
                                     }, 
                             sep = '\n'))
      
      world_sf_year <- world_sf %>%
        filter(Year == year)
      
      causePlot <- ggplot(data = world_sf_year,
                          aes(x = Consumption,
                              y = if(varName == "Life Expectancy"){
                                Life_Expectancy
                                } else if (varName == "Life Satisfaction"){
                                  Happiness
                                  } else if (varName == "Liver Cirrhosis Deaths"){
                                    Cirrhosis
                                    } else if (varName == "Traffic Crashes"){
                                      Road_Death
                                      },
                              color = if(colorVar == "Major Religion"){
                                Major_Religion
                                } else if (colorVar == "Region"){
                                  Region
                                  }
                              )) +
        geom_point_interactive(size = 2.3, 
                               alpha = 0.7,
                               tooltip = world_sf_year$toolt,
                               data_id = world_sf_year$ISO3) +
        geom_smooth(aes(color = NULL), se = FALSE) +
        theme_classic() +
        theme(axis.line.x = element_line(arrow = arrow(type = "closed", length = unit(0.3, "cm"))),
              axis.line.y = element_line(arrow = arrow(type = "closed", length = unit(0.3, "cm"))),
              aspect.ratio = 1,
              legend.position = "top",
              legend.title = element_text(size = 11, face = 'bold'), 
              legend.text = element_text(size = 10),
              axis.title.x = element_text(size = 13, face = 'bold'),
              axis.title.y = element_text(size = 13, face = 'bold'),
              axis.text.x = element_text(size = 11, face = 'bold'),
              axis.text.y = element_text(size = 11, face = 'bold')) +
        ylab(varName) +
        guides(color = guide_legend(ncol = 3,
                                    title = colorVar)) +
        scale_color_brewer(name = "Color", palette = "Accent")
      
      plot3 <- girafe(ggobj = causePlot,
                      opts_tooltip(opacity = .7,
                                   offx = 30, offy = -20,
                                   use_fill = TRUE,
                                   use_stroke = TRUE,
                                   delay_mouseout = 1500))
      
      plot3 <- girafe_options(plot3,
                              opts_selection(type = "none"),
                              opts_sizing(rescale = FALSE),
                              width = 20.0,
                              height = 20.0)
      plot3
      }
    
    scatterPlot_func(input$colorEffect,
                     input$yearEffect,
                     input$effect)
  })
  
  
  
  output$mapEffect <- renderGirafe({
    
    # Create a function for plotting choropleth for "Effect" using 2 inputs
    map_func <- function(year, varName){
      
      # Read shape file
      world_sf <-  st_read("data/world_shape_file/", quiet = TRUE)
      
      dataSet = if(varName == "Life Expectancy"){
        "life"
      } else if (varName == "Life Satisfaction"){
        "happiness"
      } else if (varName == "Liver Cirrhosis Deaths"){
        "cirrhosis2015"
      } else if (varName == "Traffic Crashes"){
        "road_death"
      }
      
      filename = paste(
        "data/effect/rrc-consumption-",
        dataSet,
        ".csv",
        sep = ""
      )
      
      dat <- read_csv(filename,
                      show_col_types = FALSE)
      
      world_sf <- world_sf %>%
        left_join(dat,
                  by = c("NAME" = "Country")) %>%
        mutate(toolt = paste(paste(toupper(NAME), sep = ""),
                             paste("Consumption: ", round(Consumption, 2), " L per capita", sep = ""),
                             if(varName == "Life Expectancy"){
                               paste("Life expectancy: ", round(Life_Expectancy, 2), " years", sep = "")
                             } else if (varName == "Life Satisfaction"){
                               paste("Life satisfaction: ", round(Happiness, 2), " (Cantril ladder)", sep = "")
                             } else if (varName == "Liver Cirrhosis Deaths"){
                               paste("Alcohol-attributable fraction (15+): ", round(Cirrhosis, 1), "%", sep = "")
                             }else if (varName == "Traffic Crashes"){
                               paste("Alcohol-attributable road crashes: ", Road_Death, "% of all road crashes", sep = "")
                             }, 
                             sep = '\n'))
      
      world_sf_year <- world_sf %>%
        filter(Year == year)
      
      color_palette <- colorRampPalette(brewer.pal(n = 9, name = "Greens"))(300)
      
      effectMap <- ggplot(data = world_sf_year, 
                         aes(fill = Consumption),
                         fill = color_palette) +
        guides(size = guide_legend(title = varName)) +
        geom_sf(show.legend = TRUE,
                size = 0.2) +
        scale_fill_distiller(direction = 1,
                             palette = "Purples",
                             na.value = "transparent") +
        geom_point_interactive(data = world_sf_year,
                               aes(x = Longitude, 
                                   y = Latitude,
                                   size = if(varName == "Life Expectancy"){
                                     Life_Expectancy
                                   } else if (varName == "Life Satisfaction"){
                                     Happiness
                                   } else if (varName == "Liver Cirrhosis Deaths"){
                                     Cirrhosis
                                   } else if (varName == "Traffic Crashes"){
                                     Road_Death
                                   }
                                   ),
                               color = "#8CE2EE",
                               tooltip = world_sf_year$toolt,
                               data_id = world_sf_year$ISO3) +
        scale_size_continuous(range = c(0.1, 2)) +
        scale_color_brewer(name = "Color", palette = "Dark2") +
        theme(legend.position = "top",
              panel.background = element_rect(fill = "#FAF0E6"),
              legend.title = element_text(size = 8, face = 'bold'), 
              legend.text = element_text(size = 6),
              axis.title.x = element_text(size = 9, face = 'bold'),
              axis.title.y = element_text(size = 9, face = 'bold'),
              axis.text.x = element_text(size = 8, face = 'bold'),
              axis.text.y = element_text(size = 8, face = 'bold'))
      
      x <- girafe(ggobj = effectMap,
                  opts_tooltip(opacity = .7,
                               offx = 30, offy = -20,
                               use_fill = TRUE,
                               use_stroke = TRUE,
                               delay_mouseout = 1500))
      x <- girafe_options(x,
                          opts_selection(type = "none"),
                          opts_sizing(rescale = TRUE, width = 1))
      
      x
    }
    
    map_func(input$yearEffect,
             input$effect)
  })
})



