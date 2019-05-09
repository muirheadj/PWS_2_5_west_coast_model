shinyServer(function(input, output, session) {
  library(readr)
  library(tidyr)
  library(dplyr)
  library(ggplot2)
  library(rprojroot)
  library(fs)

  root_crit <- has_dirname("PWS_2_5_west_coast_model", subdir = "src")
  root_dir <- root_crit$make_fix_file()

  options("readr.num_columns" = 0)


   output$port_pop_Plot <- renderPlot({
     portReaderData <- reactiveFileReader(10000, session,
        path(root_dir(), "logs", paste0("parameter00", input$param, "_ports_pop_trace.log")),
        read_log)

    port_pop_data <- setNames(portReaderData(), c("level", "timestamp", "parameter",
             "measurement", "iteration", "larva", "cyprid",
             "juvenile", "adult"))

 	  port_pop_melt <- port_pop_data %>%
 	    select(parameter, iteration, larva, cyprid, juvenile, adult) %>%
      gather(lifestage, population, -parameter, -iteration, factor_key = TRUE)

 	  p1 <- ggplot(port_pop_melt, aes(x = iteration, y = population, color = lifestage)) +
 		  facet_wrap(~parameter) +
 	    geom_path(lwd = 2) +
 	    theme_dark(base_size = 20)
    p1

   })

  output$port_n_Plot <- renderPlot({

    port_n_ReaderData <- reactiveFileReader(5000, session,
            path(root_dir(), "logs", paste0("parameter00", input$param,
              "_ports_n_trace.log")), read_log)



    port_pop_n_data <- setNames(port_n_ReaderData(), c("level", "timestamp", "parameter",
            "measurement", "iteration", "larva", "cyprid",
      "juvenile", "adult"))

	port_n_melt <- port_pop_n_data %>%
	  select(parameter, iteration, larva, cyprid, juvenile, adult) %>%
    gather(lifestage, n_invaded, -parameter, -iteration, factor_key = TRUE)

	p2 <- ggplot(port_n_melt, aes(x = iteration, y = n_invaded,
      color = lifestage)) +
		facet_wrap(~parameter) +
	  geom_path(lwd = 2) +
	  theme_dark(base_size = 20)
 p2
  })
})
