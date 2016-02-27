shinyServer(function(input, output, session) {
  
  # Example for using the lineChart
  output$mychart <- renderLineChart({
    # Return a data frame. Each column will be a series in the line chart.
    data.frame(
      Sine = sin(1:100/10 + input$sinePhase * pi/180) * input$sineAmplitude,
      Cosine = 0.5 * cos(1:100/10),
      "Sine 2" = sin(1:100/10) * 0.25 + 0.5
    )
 
  })
  
  output$image1 <- renderImage({
      return(list(
        src = "www/plato_large.png",
        contentType = "image/png",
        alt = "PLATO"
      ))
   }, deleteFile = FALSE)
  
})
