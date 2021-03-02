
# The main page layout ####
output$pageStub <- renderUI({
  
  div(
    class="main-page",
    
    navbarPage(
      title=actionLink(inputId="main_link", label=strong(fname, "Portal")), id="main_page", position=c("static-top"), collapsible=TRUE, selected=subtab,
      
      ###About####
      tabPanel(
        title="About", value="about",
        source("ui_about.R", local=TRUE)$value
      ),
      
      ###Annotation#####
      tabPanel(
        title="Annotation", value="annotation",
        source("ui_annotation.R", local=TRUE)$value
      ),

      ###Chemical Explorer#####
      tabPanel(
        title = "Chemical Explorer", value="chemical_explorer",
        #"Hello World!"
        source("ui_chemical.R", local=TRUE)$value
      ),

      ###Marker Explorer####
      tabPanel(
        title = "Marker Explorer", value="marker_explorer",
        #"Hello World!"
        source("ui_marker.R", local=TRUE)$value
      ),

      ###Heatmap Explorer####
      tabPanel(
        title = "Heatmap Explorer", value="heatmap_explorer",
        #"Hello World!"
        source("ui_heatmap.R", local=TRUE)$value
      ),

      ###Taxonomic Clustering####
      navbarMenu(
        title = "Taxonomic Clustering",

        tabPanel(
          title = "K2 Taxanomer Results", value="k2_taxanomer_results",
          #"Hello World!"
          source("ui_taxonomic_clustering.R", local=TRUE)$value
        ),

        tabPanel(
          title = "Compare Multiple", value="compare_multiple",
          #"Hello World!"
          source("ui_compare_multiple.R", local=TRUE)$value
        )
      )
    )
  )
  
})

# Go back to home page when the logo link is clicked on ####
observeEvent(input$main_link, {
  
  updateQueryString(paste0("?page=", fname, "&tab=about"), mode="push")
  updateNavbarPage(session, inputId="main_page", selected="about")
  
}, ignoreInit=TRUE)

# # Go back to home page when the logo link is clicked on ####
observeEvent({
  input$main_page
  input$chem
  input$chemical_tab
}, {
  
  if(input$main_page == "chemical_explorer"){
    
    if(is.null(input$chem) | input$chem == ""){
      updateQueryString(paste0("?page=", fname, "&tab=", input$main_page), mode="push") 
    }else{
      updateQueryString(paste0("?page=", fname, "&tab=", input$main_page, "&chemical_id=", input$chem, "&stat=", input$chemical_tab), mode="push") 
    }

  }else{
    
    updateQueryString(paste0("?page=", fname, "&tab=", input$main_page), mode="push") 
    
  }
  
  if(input$main_page == "k2-taxanomer-results"){
    session$sendCustomMessage("ResizeK2Table", "clusteringTable")
  }
  
})
