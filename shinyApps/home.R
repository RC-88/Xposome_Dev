

## home page ####
output$pageStub <- renderUI({
  
  #####<!-- START HOME PAGE -->
  fluidRow(
    class="home-page",
    
    column(
      width=12,
      DT::dataTableOutput(outputId = "main_table"),
      div(
        class="hiddenLink", style="visibility: hidden;", 
        uiOutput("hidden_downloads")
      )
    )
    
  )#####<!-- END HOME PAGE -->

})

## output main table ####
output$main_table <- DT::renderDataTable({
  
  projectlist <- projectlist; Project <- NULL;
  
  #### print(projectlist)
  
  if(all(!is.na(projectlist$Project))){
    
    download_button <- lapply(
      1:nrow(projectlist),
      function(i){
        #i=1;
        paste0('<button type="button" style="background: none; border: none;" onclick=document.getElementById("downloadData_', projectlist$Portal[i], '").click()><img src="IMAGES/download.svg"></img></button>')
      }) %>% unlist()
    
    for(i in 1:nrow(projectlist)){
      Project <- c(Project, paste0('<a onclick="curlinkFun(&#39;', projectlist$Portal[i], '&#39;)" href="?page=', projectlist$Portal[i], '&tab=about" class="portal-link" id="', projectlist$Portal[i], '" value="', projectlist$Portal[i], '">', projectlist$Project[i], '</a>'))
    }
    
    table <- data.frame(
      Project=Project,
      Cell_line=projectlist$Cell_Line,
      Description=projectlist$Description
    ) %>% 
     mutate(Download=download_button)
            
  }else{
    
    table <- data.frame(
      Project=paste0("<br>"),
      Cell_line=paste0("<br>"),
      Description=paste0("<br>"),
      Download=paste0("<br>")
    )
    
  }
  
  colnames(table) <- c("Project", "Cell line", "Description", "Download")
  
  return(table)

}, escape = FALSE, server = TRUE, rownames=FALSE, selection = "none",
options = list(
  columnDefs = list(list(className = 'dt-center', targets = "_all")),
  deferRender = FALSE,
  paging = TRUE,
  searching = TRUE,
  ordering = TRUE,
  pageLength = 20,
  scrollX = TRUE,
  scrollY = 500,
  scrollCollapse = TRUE,
  dom = 'T<"clear">Blfrtip',
  buttons=c('copy','csv','print')
))

## download the datasets
output$hidden_downloads <- renderUI({
    
   projectlist <- projectlist;
 
   lapply(1:nrow(projectlist), function(i) {
      downloadLink(outputId=paste0("downloadData_", projectlist$Portal[i]), label="Download")
    }
  )

})

observe({

  projectlist <- projectlist;

  lapply(1:nrow(projectlist), function(i) {
    
    output[[paste0("downloadData_", projectlist$Portal[i])]] <- downloadHandler(

       filename = function() {
         paste0(projectlist$Project[i], "-Dataset.zip")
       },

       content = function(file) {
         
         withProgress(message = "Downloading: ", value = 0, {
           
           fs <- c()
           tmpdir <- tempdir()
  
           datasets <- listEntities("PortalDataset", portal=projectlist$Portal[i])
      
           # Sort by timestamp and extract most recent dataset to convenience object
           datasets <- datasets[order(sapply(datasets, slot, "timestamp"))]
           dataset <- datasets[[length(datasets)]]
      
           # increment the progress bar
           incProgress(1/6, detail = "profile annotation")
           
           # Read in the profile data ####
           profile_dat <- getWorkFileAsObject(
             hiveWorkFileID(dataset@ProfileAnnotationRDS)
           )

           saveRDS(profile_dat, file.path(tmpdir, "Profile_Annoation.RDS"))
           
           # increment the progress bar
           incProgress(2/6, detail = "chemical annotation")          
           
           # Read in the chemical data ####
           chemical_dat <- getWorkFileAsObject(
             hiveWorkFileID(dataset@ChemicalAnnotationRDS)
           )
           
           saveRDS(chemical_dat, file.path(tmpdir, "Chemical_Annoation.RDS"))
           
           # increment the progress bar
           incProgress(3/6, detail = "expression set")
           
           # Read in the expression data ####
           expression_dat <- getWorkFileAsObject(
             hiveWorkFileID(dataset@GeneExpressionRDS)
           )
           
           saveRDS(expression_dat, file.path(tmpdir, "Gene_Expression.RDS"))
           
           # increment the progress bar
           incProgress(4/6, detail = "connectivity map")
           
           # Read in the connectivity data ####
           connectivity_dat <- getWorkFileAsObject(
             hiveWorkFileID(dataset@ConnectivityRDS)
           )
           
           saveRDS(connectivity_dat, file.path(tmpdir, "Connectivity.RDS"))
           
           # increment the progress bar
           incProgress(5/6, detail = "gene set enrichment")
           
           # Read in the gs enrichment data ####
           gs_enrichment_dat <- getWorkFileAsObject(
             hiveWorkFileID(dataset@GeneSetEnrichmentRDS)
           )
           
           saveRDS(gs_enrichment_dat, file.path(tmpdir, "GeneSet_Enrichment.RDS"))
           
           # increment the progress bar
           incProgress(6/6, detail = "K2-taxonomer") 
           
           K2summary <- getWorkFileAsObject(
             hiveWorkFileID(dataset@K2TaxonomerResultsRDS)
           )
           
           saveRDS(K2summary, file.path(tmpdir, "K2Taxonomer.RDS"))
           
           # zip the files
           file_names <- c("Profile_Annoation", "Chemical_Annoation", "Gene_Expression", "GeneSet_Enrichment", "Connectivity", "K2Taxonomer") 
  
           for(names in file_names){
             path <- file.path(tmpdir, paste0(names, ".RDS"))
             fs <- c(fs, path)
           } 
  
           zip(zipfile=file, files=fs, flags = "-r9Xj")
           
         })
      },

      contentType = "application/zip"
    )
  })

})


