shinyServer(function(input, output, session) {
  library(readr)
  library(tidyr)
  library(dplyr)
  library(ggplot2)
  options("readr.num_columns" = 0)
  
   output$port_pop_Plot <- renderPlot({
         
         portReaderData <- reactiveFileReader(10000, session,
             paste0("/Users/jmuirhead/Documents/Post-Doc\ Maryland/epidemiology_model/parameter00", 
                 input$param, "_ports_pop_trace.log"), read_log)
         
       port_pop_data <- setNames(portReaderData(), c("level", "timestamp", "parameter",
             "measurement", "iteration", "larva", "cyprid",
             "juvenile", "adult"))
 	  port_pop_melt <- port_pop_data %>% select(parameter, iteration, larva, cyprid, juvenile, adult) %>%
       	gather(lifestage, population, -parameter, -iteration, factor_key = TRUE)

 	  ggplot(port_pop_melt, aes(x = iteration, y = population, color = lifestage)) +
 		facet_wrap(~parameter) + geom_path(lwd = 2) + theme_dark(base_size = 20)

   })

  output$port_n_Plot <- renderPlot({
<<<<<<< HEAD
    port_n_ReaderData <- reactiveFileReader(10000, session,
            paste0("/Users/jmuirhead/Documents/Post-Doc\ Maryland/epidemiology_model/parameter00", 
                input$param,"_ports_n_trace.log"), read_log)
||||||| merged common ancestors
    port_n_ReaderData <- reactiveFileReader(5000, session,
            paste0("/Users/jmuirhead/Documents/Post-Doc\ Maryland/epidemiology_model/parameter00", input$param,"_ports_n_trace.log"), read_log)
=======
    port_n_ReaderData <- reactiveFileReader(5000, session,
            paste0("/Users/jmuirhead/Documents/Post-Doc\ Maryland/epidemiology_model/parameter00",
                input$param,"_ports_n_trace.log"), read_log)
>>>>>>> hydra
        
        
    port_pop_n_data <- setNames(port_n_ReaderData(), c("level", "timestamp", "parameter",
            "measurement", "iteration", "larva", "cyprid",
      "juvenile", "adult"))

	port_n_melt <- port_pop_n_data %>% select(parameter, iteration, larva, cyprid, juvenile, 
          adult) %>%
      	gather(lifestage, n_invaded, -parameter, -iteration, factor_key = TRUE)

	  ggplot(port_n_melt, aes(x = iteration, y = n_invaded, color = lifestage)) +
		facet_wrap(~parameter) + geom_path(lwd = 2) + theme_dark(base_size = 20)
  })

   output$shipPlot <- renderPlot({
         
    shipReaderData <- reactiveFileReader(10000, session,
        paste0("/Users/jmuirhead/Documents/Post-Doc\ Maryland/epidemiology_model/parameter00", 
            input$param, "_ships_pop_trace.log"), read_log)
         
         
    ship_pop_data <- setNames(shipReaderData(), c("level", "timestamp", "parameter",
             "measurement", "iteration", "larva", "cyprid","juvenile", "adult"))

 	ship_pop_melt <- ship_pop_data %>% select(parameter, iteration, larva, cyprid, juvenile, 
          adult) %>%
       	gather(lifestage, population, -parameter, -iteration)

 	  ggplot(ship_pop_melt, aes(x = iteration, y = population, color = lifestage)) +
 		facet_wrap(~parameter) + geom_path(lwd = 2) + theme_dark(base_size = 20)
   })

})