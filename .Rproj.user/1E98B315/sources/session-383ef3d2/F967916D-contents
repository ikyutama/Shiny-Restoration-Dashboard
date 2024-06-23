shinyServer(function(input, output) {
  output$plot_top_co2e <- renderPlotly({
    species_co2e <- monitoring_clean %>% 
      group_by(species_name) %>% 
      summarize(total_co2e = sum(total_carbon_reduce)) %>% 
      arrange(-total_co2e) %>% 
      head(5) %>% 
      mutate(label = glue("Species Name: {species_name}
                      Total CO2e: {comma(total_co2e, accuracy = 0.01)} Ton")) 
    
    plot_overview_1 <- ggplot(data = species_co2e, mapping = aes(x = total_co2e, 
                                                    y = reorder(species_name, total_co2e), 
                                                    fill = total_co2e,
                                                    text = label))+ 
      geom_col() +
      scale_fill_gradient(low = "cyan",
                          high = "black") +
      scale_x_continuous(labels = comma) +
      labs(x = "Total CO2e (Ton)", 
           y = "") +
      theme_light() +
      theme(legend.position = "none") 
    
    ggplotly(plot_overview_1, 
             tooltip = "text")
  })
  
  output$doughnut_overview <- renderPlot({
    tree_by_species <- monitoring_clean %>% 
      group_by(species_name) %>% 
      summarize(totaltree = sum(total_tree)) %>% 
      filter(totaltree > 0) %>% 
      arrange(-totaltree) %>% 
      mutate(percent = round(totaltree / sum(totaltree) * 100, 2)) %>% 
      mutate(label = glue("{comma(totaltree)}")) %>% 
      mutate(ymax = cumsum(percent)) %>% 
      mutate(ymin = c(0, head(ymax, n= -1))) %>% 
      mutate(labelPosition = (ymax + ymin)/2)
    
    plot_overview_2 <- ggplot(tree_by_species, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=species_name)) +
      geom_rect() +
      geom_label( x=3.5, aes(y=labelPosition, label=label), size=4) +
      scale_fill_brewer(palette=4) +
      coord_polar(theta="y") +
      xlim(c(2, 4)) +
      theme_void() +
      theme(legend.title = element_text(size = 12),
            legend.text = element_text(size = 12)) +
      labs(fill = "Species name")
    plot_overview_2
  })
  
  #Menu Pohon
  output$visual_pohon_1 <- renderPlotly({
    height_by_species <- monitoring_clean %>%
      filter(date_monitoring >= input$daterange[1]) %>%
      filter(date_monitoring <= input$daterange[2]) %>%
      filter(tree_planting_system %in% input$select_planting) %>% 
      group_by(species_name) %>% 
      summarise(avgh = mean(avg_tree_height)) %>% 
      mutate(label = glue("Species name : {species_name}
                      Average Height: {comma(avgh, accuracy = 0.01)} meter"))
    
    plot_pohon_1 <- ggplot(data = height_by_species, mapping = aes(x = avgh, 
                                              y = reorder(species_name, avgh),
                                              fill = avgh,
                                              text = label)) +
      geom_col() +
      scale_fill_gradient(low = "red",
                          high = "black") +
      scale_x_continuous(labels = comma)+
      labs(title = glue("Rata-Rata Tinggi Pohon bedasarkan sistem tanam ",  input$select_planting, " pada tanggal ", as.character(input$daterange[1]) , ' sampai ', as.character(input$daterange[2])),
           x = "Rata-rata Tinggi (Meter)",
           y = "") + 
      theme_minimal() +
      theme(legend.position = "none")
    
    # Mengubah Visual Statis menjadi Interaktif
    ggplotly(plot_pohon_1, tooltip = 'text')
  })
  
  #menu carbon
  output$visual_carbon_1 <- renderPlotly({
    # Persiapan Data
    carbon_date <- 
      monitoring_clean %>% 
      filter(species_name %in% input$select_species) %>% 
      group_by(date_monitoring) %>% 
      summarise(total = sum(total_carbon_reduce)) %>% 
      mutate(label = glue("Tanggal Monitoring : {date_monitoring}
                      Total CO2e: {comma(total, accuracy = 0.01)} Ton"))
    
    # Pembuatan Visual Statis
    plot_carbon_1 <- 
      ggplot(data = carbon_date, mapping = aes(x = date_monitoring, 
                                                      y = total)) +
      geom_line(col = "green") +
      geom_point(mapping = aes(text = label)) + # parameter text untuk line plot hanya bisa diletakan di bagian geom_point()
      scale_y_continuous(label = comma)+ # untuk merapikan bagian sumbu y
      labs(title = glue("Total CO2e untuk jeni tanaman ", input$select_species,"  Bedasarkan Tanggal Monitoring"),
           x = "Tanggal Monitoring",
           y = "Total CO2e (Ton)") +
      theme_minimal()
    
    # Mengubah Visual Statis menjadi Interaktif
    ggplotly(plot_carbon_1, tooltip = 'text')
  })
  
  
  # datatable dataset
  output$dataset_table <- renderDataTable({
    
    datatable(monitoring_clean,
              options = list(scrollX = TRUE)) # memberikan opsi untuk melakukan scroll ke kanan atau kiri
    
  })
    
})