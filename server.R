server = function(input, output) {
data <- reactive({
  req(input$file1) 
  inFile <- input$file1
  df <- read.csv(inFile$datapath, TRUE,';','"')
  return(df)})

 output$grafica1 <-renderPlotly({
   Disciplina <- as.factor(data()[,names(data())[15]])
   p <- ggplot(data(),aes(Disciplina),fill = Disciplina)+
   geom_bar(stat = "count", fill = c("#0073C2FF", "#EFC000FF", "#868686FF", "#CD534CFF",
                                     "#eef9bf","#a7e9af","#75b79e","#6a8caf",
                                     "#deddfa","#dab8f3","#ea7ad7","#ce0f3d",
                                     "#ffbaba","#ffeadb","#678a74","#ffe196"))+
     
     theme_classic()+
   theme(axis.text.x=element_text(angle=45, hjust=1))+
     scale_color_viridis(discrete = TRUE)+ 
   labs(#title="Número de Issues por disciplina", 
       y="Issues", 
       x="Disciplines")}) 
 
 
 output$grafica2 <-renderPlotly({
   Proyecto <- as.factor(data()[,names(data())[2]])
   p <- ggplot(data(),aes(Proyecto),fill = as.factor(data()[,names(data())[15]]))+
     geom_bar(stat = "count", fill = c("#FFA500","#BDB76B", "#EEE8AA", "#FFDEAD", "#F5DEB3", "#DEB887", "#D2B48C", "#BC8F8F", "#F4A460"
                                       , "#DAA520", "#CD853F", "#D2691E", "#8B4513", "#A0522D", "#A52A2A", "#800000", "#FFE4E1", "#FFF0F5","#FFD700"))+
     theme_classic()+
     theme(axis.text.x=element_text(angle=45, hjust=1))+scale_color_viridis(discrete = TRUE)+ 
     labs(y="Issues", 
          x="Projects")+
     coord_flip()})
 
 
 
 output$grafica3 <-renderPlotly({
   Disciplina <- as.factor(data()[,names(data())[15]])
     Prioridad <- as.factor(data()[,names(data())[5]])
   p <- ggplot(data(), aes(x=Disciplina,
                           fill= Prioridad)) + 
     geom_bar(stat="count")+
     theme_classic()+
     scale_fill_manual(labels=c("High", "Low", "Normal"), 
                       values=c("#810f7c", "#8856a7","#8c96c6"),
                       name="Prioridad")+
     theme(axis.text.x=element_text(angle=45, hjust=1))+
     scale_color_viridis(discrete = TRUE) + 
     labs(y="Issues", 
          x="Projects", fill= c("High", "Low", "Normal"))
 })
 
 
 output$grafica4 <-renderPlotly({
   dato <- filter(data(), Status=="Blocked")
   bloqueo <- as.factor(dato[,names(dato)[15]])
   output$downloadBloqueos <- downloadHandler(
     filename = function() {
       paste("Blocks", ".csv", sep = "")
     },
     content = function(file) {
       write.csv(dato, file, row.names = FALSE)
     }
   )

   p <- ggplot(dato,aes(bloqueo),fill = bloqueo)+
     geom_bar(stat = "count", fill = c("#67000d","#a50f15", "#cb181d", "#ef3b2c", "#fb6a4a", "#fc9272", "#fcbba1", "#fee0d2"))+
     theme_classic()+
     theme(axis.text.x=element_text(angle=45, hjust=1))+scale_color_viridis(discrete = TRUE)+
     labs(y="Blocks", 
          x="Discipline")})

 
 output$grafica5 <-renderPlotly({
   df <- data() %>%
     mutate(Date = as.Date(Duedate, format = "%d/%m/%Y")) %>%
     group_by(Project) %>%
     filter(Date == max(Date)) %>%
     ungroup()
   
   p <- ggplot(df, aes(x = Date, y = Project, group = 1)) +
   geom_line()+
     geom_point()+
     theme_classic()+
     geom_line(color = "#00AFBB")+
     theme(axis.text.x=element_text(angle=45, hjust=1))+scale_color_viridis(discrete = TRUE)+
     labs(y="Projects", 
       x="Date")})
 
 output$approvalBox <- renderValueBox({
   valueBox(
      nrow(data()), "Total Number of Open Issues",
     color = "light-blue"
   )
 })


   output$userPlot <-renderPlotly({
     datoUser <- filter(data(), Assignee==input$usuario)
     filtro <- as.factor(filter(data(), Assignee==input$usuario)[,names(filter(data(), Assignee==input$usuario))[6]])
     output$downloadAsignadas <- downloadHandler(
       filename = function() {
         paste("Assigned", ".csv", sep = "")
       },
       content = function(file) {
         write.csv(datoUser, file, row.names = FALSE)
       }
     )
     p <- ggplot(datoUser,aes(filtro),fill = filtro)+
       geom_bar(stat = "count", fill = "#9e9ac8")+
       theme_classic()+
       theme(axis.text.x=element_text(hjust=1))+scale_color_viridis(discrete = TRUE)+ 
       labs(
            y="Issues",
            x=NULL)})
   
   output$tableIssues <- DT::renderDataTable(DT::datatable({
      datoUser <- filter(data(), Assignee==input$usuario)
      data <- select(datoUser, names(datoUser)[1],names(datoUser)[2],names(datoUser)[3],names(datoUser)[4],names(datoUser)[5])
   },
   options = list(pageLength = 2)
   ))
   

   output$fechaPlot <-renderPlotly({
     datoFecha <- filter(data(),as.Date(Duedate, format = "%d/%m/%Y")<as.Date(max(input$dateRange), format = "%d/%m/%Y") & as.Date(StartDate, format = "%d/%m/%Y")>as.Date(min(input$dateRange), format = "%d/%m/%Y"))
     fecha2 <- as.factor(datoFecha[,names(datoFecha)[6]])
     output$downloadFechas <- downloadHandler(
       filename = function() {
         paste("Assigned_in_date", ".csv", sep = "")
       },
       content = function(file) {
         write.csv(datoFecha, file, row.names = FALSE)
       }
     )
     p <- ggplot(datoFecha,aes(fecha2),fill = fecha2)+
       geom_bar(stat = "count", fill = "#9e9ac8")+
       theme_classic()+
       theme(axis.text.x=element_text(angle=45, hjust=1))+scale_color_viridis(discrete = TRUE)+ 
            labs( 
            y="Issues", 
            x="User")})
   
   
   output$tableUsuario <- DT::renderDataTable(DT::datatable({
     UsuarioFechaKO <- filter(data(),as.Date(Duedate, format = "%d/%m/%Y")<as.Date(Sys.Date(), format = "%d/%m/%Y"))
     output$downloadUsuarioFechaKO <- downloadHandler(
       filename = function() {
         paste("Issues_delayed", ".csv", sep = "")
       },
       content = function(file) {
         write.csv(UsuarioFechaKO, file, row.names = FALSE)
       }
     )
     data <- select(UsuarioFechaKO, names(UsuarioFechaKO)[1],names(UsuarioFechaKO)[6])
   },
   options = list(pageLength = 4)
   ))
   
   
   
   
   
   output$tableProyecto <- DT::renderDataTable(DT::datatable({
     ProyectoFechaKO <- filter(data(),as.Date(Duedate, format = "%d/%m/%Y")<as.Date(Sys.Date(), format = "%d/%m/%Y"))
     output$downloadProyectoFechaKO <- downloadHandler(
       filename = function() {
         paste("Issues_delayed", ".csv", sep = "")
       },
       content = function(file) {
         write.csv(ProyectoFechaKO, file, row.names = FALSE)
       }
     )

     data <- select(ProyectoFechaKO, names(ProyectoFechaKO)[1],names(ProyectoFechaKO)[2])
   },
   options = list(pageLength = 4)
   ))
   
   output$tableTiempo <- DT::renderDataTable(DT::datatable({
     UsuarioTiempoKO <- filter(data(),SpentTime>EstimatedTime)
     output$downloadUsuarioTiempoKO <- downloadHandler(
       filename = function() {
         paste("Issues_time_exceeded", ".csv", sep = "")
       },
       content = function(file) {
         write.csv(UsuarioTiempoKO, file, row.names = FALSE)
       }
     )
     
     data <- select(UsuarioTiempoKO, names(UsuarioTiempoKO)[1],names(UsuarioTiempoKO)[6])
   },
   options = list(pageLength = 4)
   ))
   
   output$tableNoCerrada<- DT::renderDataTable(DT::datatable({
     UsuarioCerradoKO <- filter(data(), Done==100)
     output$downloadNoCerrada <- downloadHandler(
       filename = function() {
         paste("Issues_no_Close", ".csv", sep = "")
       },
       content = function(file) {
         write.csv(UsuarioCerradoKO, file, row.names = FALSE)
       }
     )
     data <- select(UsuarioCerradoKO, names(UsuarioCerradoKO)[1],names(UsuarioCerradoKO)[6])
   },
   options = list(pageLength = 4)
   ))
   
   output$tableInProgress <- DT::renderDataTable(DT::datatable({
     UsuarioNoDone <- filter(data(),Status=="In Progress" & Done==0)
     output$downloadNoDone <- downloadHandler(
       filename = function() {
         paste("Issues_no_percentage", ".csv", sep = "")
       },
       content = function(file) {
         write.csv(UsuarioNoDone, file, row.names = FALSE)
       }
     )
     data <- select(UsuarioNoDone, names(UsuarioNoDone)[1],names(UsuarioNoDone)[6])
   },
   options = list(pageLength = 4)
   ))
   
   output$tableNoAsignada <- DT::renderDataTable(DT::datatable({
     NoAsignada <- filter(data(),Assignee=="")
     output$downloadNoAsignada <- downloadHandler(
       filename = function() {
         paste("Issues_no_assigned", ".csv", sep = "")
       },
       content = function(file) {
         write.csv(NoAsignada, file, row.names = FALSE)
       }
     )
     data <- select(NoAsignada, names(NoAsignada)[1],names(NoAsignada)[6])
   },
   options = list(pageLength = 4)
   ))
   
   
}





    


  


  
