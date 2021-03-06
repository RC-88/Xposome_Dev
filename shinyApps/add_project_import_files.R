

#import introduction file#####
observeEvent(input$Add_Project_Add_Button, {
  
  inputfile <- input$add_intro_file;
  
  if(is.null(inputfile)){
    intro_file_msg("Please choose a file to import.")
    intro_file(NULL)
    return(NULL)
  }
  
})

observeEvent(input$add_intro_file, {
  
  inputfile <- input$add_intro_file;
  
  if(is.null(inputfile)){
    intro_file_msg("")
    intro_file(NULL)
    return(NULL)
  }
  
  tryCatch({
    
    extension <- grep(toupper(".rmd"), toupper(substr(inputfile$datapath, nchar(inputfile$datapath)-4, nchar(inputfile$datapath))), fixed = TRUE)
    
    if(length(extension) == 0){
      intro_file_msg("Incorrect file format. Please check your file again.")
      intro_file(NULL)
      return(NULL)
    }else{
      intro_file_msg("")
      intro_file(inputfile$datapath)
    }
    
  }, error=function(err){
    intro_file_msg("Import failed. Please check your file again.")
    intro_file(NULL)
    return(NULL)
  }, warning=function(war){
    intro_file_msg("Import failed. Please check your file again.")
    intro_file(NULL)
    return(NULL)
  })
  
})

##Output introduction warning message###
output$add_intro_file_msg <- renderUI({
  
  req(intro_file_msg())
  
  p(class="fileInputMsg",  HTML(intro_file_msg()))
  
})

#import profile annotation file#####
observeEvent(input$Add_Project_Add_Button, {
  
  inputfile <- input$add_pro_file;
  inputtype <- input$add_pro_file_type; 
  
  if(is.null(inputfile)){
    pro_file_msg("Please choose a file to import.")
    chem_file(NULL)
    pro_file(NULL)
    return(NULL)
  }
  
})

observeEvent({
  input$add_pro_file
  input$add_pro_file_type
}, {
  
  inputfile <- input$add_pro_file;
  inputtype <- input$add_pro_file_type; 
  
  if(is.null(inputfile)){
    updateSelectInput(session, inputId="add_variable_compound", choices=c("Import a profile annotation" = ""))
    updateSelectInput(session, inputId="add_variable_exposure", choices=c("Import a profile annotation" = ""))
    updateSelectInput(session, inputId="add_variable_exposure_phenotype", choices=c("Import a profile annotation" = ""))
    pro_file_msg("")
    chem_file(NULL)
    pro_file(NULL)
    return(NULL)
  }
  
  tryCatch({
    
    csv_ext <-  grep(toupper(".csv"), toupper(substr(inputfile$datapath, nchar(inputfile$datapath)-4, nchar(inputfile$datapath))), fixed = TRUE)
    rds_ext <-  grep(toupper(".rds"), toupper(substr(inputfile$datapath, nchar(inputfile$datapath)-4, nchar(inputfile$datapath))), fixed = TRUE)
    
    if(inputtype %in% ".csv" & length(csv_ext) > 0){
      dat <- read.csv(inputfile$datapath, header = TRUE, row.names = 1, check.names = FALSE, stringsAsFactors = FALSE)
    }else if(inputtype %in% ".RDS" & length(rds_ext) > 0){
      dat <- readRDS(inputfile$datapath)
    }else{
      updateSelectInput(session, inputId="add_variable_compound", choices=c("Import a profile annotation" = ""))
      updateSelectInput(session, inputId="add_variable_exposure", choices=c("Import a profile annotation" = ""))
      updateSelectInput(session, inputId="add_variable_exposure_phenotype", choices=c("Import a profile annotation" = ""))
      pro_file_msg("Incorrect file format. Please check your file again.")
      chem_file(NULL)
      pro_file(NULL)
      return(NULL)
    }
    
    variables <- c("Sig_Id", "Chemical_Id", "Chemical_Name", "BUID", "CAS")
    
    if(all(variables %in% colnames(dat))){
      exposure_variable <- unique(colnames(dat)[which(!colnames(dat) %in% c("Sig_Id", "Chemical_Id", "Chemical_Name", "BUID", "CAS", "TAS"))])
      updateSelectInput(session, inputId="add_variable_compound", choices=c("Please select an option below" = "", "Chemical_Id"))
      updateSelectInput(session, inputId="add_variable_exposure", choices=c("Please select an option below" = "", exposure_variable))
      updateSelectInput(session, inputId="add_variable_exposure_phenotype", choices = c("Please select an option below" = "", exposure_variable))
      chem_file(distinct(dat, Chemical_Id, Chemical_Name, BUID, CAS, .keep_all=TRUE) %>% select(-Sig_Id))
      pro_file_msg("")
      pro_file(dat)      
    }else{
      errorMsg <- paste0("One or more of the required variables: <em>", paste0(variables, collapse = ", "), "</em> are missing from the dataset.")
      updateSelectInput(session, inputId="add_variable_compound", choices=c("Import a profile annotation" = ""))
      updateSelectInput(session, inputId="add_variable_exposure", choices=c("Import a profile annotation" = ""))
      updateSelectInput(session, inputId="add_variable_exposure_phenotype", choices=c("Import a profile annotation" = ""))
      pro_file_msg(errorMsg)
      chem_file(NULL)
      pro_file(NULL)
      return(NULL)
    }
    
  }, error=function(err){
    pro_file_msg("Import failed. Please check your file again.")
    chem_file(NULL)
    pro_file(NULL)
    return(NULL)
  }, warning=function(war){
    pro_file_msg("Import failed. Please check your file again.")
    chem_file(NULL)
    pro_file(NULL)
    return(NULL)
  })
  
})

##Output profile warning message###
output$add_pro_file_msg <- renderUI({
  
  req(pro_file_msg())
  
  p(class="fileInputMsg", HTML(pro_file_msg()))
  
})

##Create cohorts option####
observeEvent(input$add_variable_exposure, {
  
  req(pro_file(), chem_file(), ge_file(), input$add_variable_exposure)
  
  pro_ann <- pro_file(); chem_ann <- chem_file(); gene_expression <- ge_file(); 
  var <- ifelse(all(colnames(gene_expression) %in% pro_ann$Sig_Id), "Sig_Id", "Chemical_Id");
  exposure <- input$add_variable_exposure;
  
  # Getting the number of replicates for each chemical
  if(var=="Sig_Id"){
    pro_ann$unique_ID_by_chem <- lapply(1:nrow(pro_ann), function(r){ paste0(unlist(pro_ann[r,exposure]), collapse="_") }) %>% unlist()
    chem_replicate <- pro_ann %>% group_by(Chemical_Id, unique_ID_by_chem) %>% summarise(Frequency=n()) %>% ungroup()
  }else{
    chem_ann$unique_ID_by_chem <- lapply(1:nrow(chem_ann), function(r){ paste0(unlist(chem_ann[r,exposure]), collapse="_") }) %>% unlist()
    chem_replicate <- chem_ann %>% group_by(Chemical_Id, unique_ID_by_chem) %>% summarise(Frequency=n()) %>% ungroup()
  }  
  
  if(any(chem_replicate$Frequency > 1)){
    
    cohorts("Chemical_Id")
    
  }else{
    
    cohorts(NULL)
    
  }
  
})

##Create Add TAS and ModZ option####
output$Add_Tas_Modz <- renderUI({
  
  req(cohorts())
  
  div(
    h4("Calculations:", style="padding-bottom: 10px;"),
    checkboxInput(inputId = "Add_TAS", label = "TAS", value=TRUE), 
    checkboxInput(inputId = "Add_Modzscores", label = "Mod-Zscores", value=TRUE), 
  )
  
})

##list of exposure phenotype statistical tests####
output$add_metavar_variable_test <- DT::renderDataTable({
  
  req(input$add_variable_exposure_phenotype, pro_file())
  
  # pro_ann = read.csv(paste0(path, "/Profile_Annotation_Testing.csv"), header = TRUE, row.names = 1, check.names = FALSE, stringsAsFactors = FALSE)
  # varlist = c("Carcinogenicity", "Genotoxicity"); test <- NULL; 
   
  pro_ann=pro_file(); varlist=input$add_variable_exposure_phenotype;
  
  test <- suppressWarnings({
    phenotype_test(pro_ann=pro_ann, varlist=varlist)
  })
    
  table = data.frame(Variable=varlist, test=test)
  colnames(table) <- c("Exposure Phenotype", "Statistical Test")
  return(table)
  
}, rownames=FALSE, server=FALSE, escape=FALSE, selection="none", 
options=list(
  dom="T", 
  columnDefs = list(list(className = 'dt-center', targets = "_all")),
  drawCallback = JS('function() { Shiny.bindAll(this.api().table().node()); }')
))


#import ge expression file#####
observeEvent(input$Add_Project_Add_Button, {
  
  inputfile <- input$add_ge_file;
  inputtype <- input$add_ge_file_type; 
  pro_ann <- pro_file();
  
  if(is.null(inputfile)){
    ge_file_msg("Please choose a file to import.")
    ge_file(NULL)
    return(NULL)
  }

})

observeEvent({
  input$add_ge_file
  input$add_ge_file_type
}, {
  
  inputfile <- input$add_ge_file;
  inputtype <- input$add_ge_file_type; 
  pro_ann <- pro_file();
  
  if(is.null(inputfile)){
    ge_file(NULL)
    return(NULL)
  }
  
  tryCatch({
    
    csv_ext <-  grep(toupper(".csv"), toupper(substr(inputfile$datapath, nchar(inputfile$datapath)-4, nchar(inputfile$datapath))), fixed = TRUE)
    rds_ext <-  grep(toupper(".rds"), toupper(substr(inputfile$datapath, nchar(inputfile$datapath)-4, nchar(inputfile$datapath))), fixed = TRUE)
    
    if(inputtype %in% ".csv" & length(csv_ext) > 0){
      dat <- read.csv(inputfile$datapath, header = TRUE, row.names = 1, check.names = FALSE, stringsAsFactors = FALSE)
    }else if(inputtype %in% ".RDS" & length(rds_ext) > 0){
      dat <- readRDS(inputfile$datapath)
    }else{
      ge_file_msg("Incorrect file format. Please check your file again.")
      ge_file(NULL)
      return(NULL)
    }
    
    if(is.null(pro_ann)){
      ge_file_msg("Need a profile annotation file to match.")
      ge_file(NULL)
      return(NULL)
    }
    
    if(all(colnames(dat) %in% pro_ann$Sig_Id) | all(colnames(dat)%in% pro_ann$Chemical_Id)){
      match_colnames <- TRUE
    }else{
      match_colnames <- FALSE
    }

    if(match_colnames == FALSE){
      errorMsg <- paste0("The expression set and profile annotation do not match. Please check your files again.")
      ge_file_msg(errorMsg)
      ge_file(NULL)
      return(NULL)
    }

    check_numeric <- all(TRUE %in% sapply(dat, 2, is.numeric))

    if(check_numeric == FALSE){
      errorMsg <- paste0("The expression set MUST all be numeric. Please check your files again.")
      ge_file_msg(errorMsg)
      ge_file(NULL)
      return(NULL)
    }

    if(match_colnames==TRUE && check_numeric==TRUE){
      ge_file_msg("")
      ge_file(dat)
    }
    
  }, error=function(err){
    ge_file_msg("Import failed. Please check your file again.")
    ge_file(NULL)
    return(NULL)
  }, warning=function(war){
    ge_file_msg("")
    ge_file(dat)
    return(NULL)
  })
  
})

##Output ge warning message####
output$add_ge_file_msg <- renderUI({
  
  req(ge_file_msg())
  
  p(class="fileInputMsg",  HTML(ge_file_msg()))
  
})

#import connectivity map - perturbagens class file#####
observeEvent(input$Add_Project_Add_Button, {
  
  req(input$add_conn_option %in% "Yes")
  
  inputfile <- input$add_conn_pcl_file;
  inputtype <- input$add_conn_pcl_file_type;
  pro_ann <- pro_file();
  
  if(is.null(inputfile)){
    conn_pcl_file_msg("Please choose a file to import.")
    conn_pcl_file(NULL)
    return(NULL)
  }
  
})

observeEvent({
  input$add_conn_pcl_file
  input$add_conn_pcl_file_type
}, {
  
  req(input$add_conn_option %in% "Yes")
  
  inputfile <- input$add_conn_pcl_file;
  inputtype <- input$add_conn_pcl_file_type;
  pro_ann <- pro_file();
  
  if(is.null(inputfile)){
    conn_pcl_file(NULL)
    return(NULL)
  }
  
  tryCatch({
    
    csv_ext <-  grep(toupper(".csv"), toupper(substr(inputfile$datapath, nchar(inputfile$datapath)-4, nchar(inputfile$datapath))), fixed = TRUE)
    rds_ext <-  grep(toupper(".rds"), toupper(substr(inputfile$datapath, nchar(inputfile$datapath)-4, nchar(inputfile$datapath))), fixed = TRUE)
    
    if(inputtype %in% ".csv" & length(csv_ext) > 0){
      dat <- read.csv(inputfile$datapath, header = TRUE, row.names = 1, check.names = FALSE, stringsAsFactors = FALSE)
    }else if(inputtype %in% ".RDS" & length(rds_ext) > 0){
      dat <- readRDS(inputfile$datapath)
    }else{
      conn_pcl_file_msg("Incorrect file format. Please check your file again.")
      conn_pcl_file(NULL)
      return(NULL)
    }
    
    if(is.null(pro_ann)){
      conn_pcl_file_msg("Need a profile annotation file to match.")
      conn_pcl_file(NULL)
      return(NULL)
    }
    
    match_colnames <- all(pro_ann$Sig_Id %in% colnames(dat)) | all(pro_ann$Chemical_Id %in% colnames(dat))
    
    if(match_colnames){
      conn_pcl_file_msg("")
      conn_pcl_file(dat)
    }else{
      errorMsg <- paste0("The connectivity map (perturbagens class) and profile annotation do not match. Please check your files again.")
      conn_pcl_file_msg(errorMsg)
      conn_pcl_file(NULL)
      return(NULL)
    }
    
  }, error=function(err){
    conn_pcl_file_msg("Import failed. Please check your file again.")
    conn_pcl_file(NULL)
    return(NULL)
  }, warning=function(war){
    conn_pcl_file_msg("Import failed. Please check your file again.")
    conn_pcl_file(NULL)
    return(NULL)
  })
  
}, ignoreNULL = FALSE)

##Output pcl warning message###
output$add_conn_pcl_file_msg <- renderUI({
  
  req(conn_pcl_file_msg())
  
  p(class="fileInputMsg",  HTML(conn_pcl_file_msg()))
  
})

#import connectivity map - perturbagens class file#####
observeEvent(input$Add_Project_Add_Button, {
  
  req(input$add_conn_option %in% "Yes")
  
  inputfile <- input$add_conn_pert_file;
  inputtype <- input$add_conn_pert_file_type;
  pro_ann <- pro_file();
  
  if(is.null(inputfile)){
    conn_pert_file_msg("Please choose a file to import.")
    conn_pert_file(NULL)
    return(NULL)
  }
  
})

observeEvent({
  input$add_conn_pert_file
  input$add_conn_pert_file_type
}, {
  
  req(input$add_conn_option %in% "Yes")
  
  inputfile <- input$add_conn_pert_file;
  inputtype <- input$add_conn_pert_file_type;
  pro_ann <- pro_file();
  
  if(is.null(inputfile)){
    conn_pert_file(NULL)
    return(NULL)
  }
  
  tryCatch({
    
    csv_ext <-  grep(toupper(".csv"), toupper(substr(inputfile$datapath, nchar(inputfile$datapath)-4, nchar(inputfile$datapath))), fixed = TRUE)
    rds_ext <-  grep(toupper(".rds"), toupper(substr(inputfile$datapath, nchar(inputfile$datapath)-4, nchar(inputfile$datapath))), fixed = TRUE)
    
    if(inputtype %in% ".csv" & length(csv_ext) > 0){
      dat <- read.csv(inputfile$datapath, header = TRUE, row.names = 1, check.names = FALSE, stringsAsFactors = FALSE)
    }else if(inputtype %in% ".RDS" & length(rds_ext) > 0){
      dat <- readRDS(inputfile$datapath)
    }else{
      conn_pert_file_msg("Incorrect file format. Please check your file again.")
      conn_pert_file(NULL)
      return(NULL)
    }
    
    if(is.null(pro_ann)){
      conn_pert_file_msg("Need a profile annotation file to match.")
      conn_pert_file(NULL)
      return(NULL)
    }
    
    match_colnames <- (all(colnames(dat) %in% pro_ann$Sig_Id) | all(colnames(dat)%in% pro_ann$Chemical_Id))
    
    if(match_colnames){
      conn_pert_file_msg("")
      conn_pert_file(dat)
    }else{
      errorMsg <- paste0("The connectivity map (perturbagens) and profile annotation do not match. Please check your files again.")
      conn_pert_file_msg(errorMsg)
      conn_pert_file(NULL)
      return(NULL)
    }
    
  }, error=function(err){
    conn_pert_file_msg("Import failed. Please check your file again.")
    conn_pert_file(NULL)
    return(NULL)
  }, warning=function(war){
    conn_pert_file_msg("Import failed. Please check your file again.")
    conn_pert_file(NULL)
    return(NULL)
  })
  
}, ignoreNULL = FALSE)

##Output pert warning message###
output$add_conn_pert_file_msg <- renderUI({
  
  req(conn_pert_file_msg())
  
  p(class="fileInputMsg",  HTML(conn_pert_file_msg()))
  
})

#import hallmark file#####
observeEvent(input$Add_Project_Add_Button, {
  
  req(input$add_cur_enrichment_option %in% "No")
  
  inputfile <- input$add_gs_collection_file;

  if(is.null(inputfile)){
    gs_collection_file_msg("Please choose a file to import.")
    gs_collection_file(NULL)
    return(NULL)
  }
  
})

observeEvent(input$add_gs_collection_file, {
  
  req(input$add_cur_enrichment_option %in% "No")
  
  inputfile <- input$add_gs_collection_file;

  if(is.null(inputfile)){
    gs_collection_file(NULL)
    return(NULL)
  }
  
  tryCatch({
  
    extension <- grep(toupper(".gmt"), toupper(substr(inputfile$datapath, nchar(inputfile$datapath)-4, nchar(inputfile$datapath))), fixed = TRUE)
    
    if(length(extension) == 0){
      gs_collection_file_msg("Incorrect file format. Please check your file again.")
      gs_collection_file(NULL)
      return(NULL)
    }else{
      gs_collection_file_msg("")
      data <- getGmt(inputfile$datapath)
      gs_collection_file(
        list(
          path=inputfile$datapath,
          data=data
        )
      )
    }
    
  }, error=function(err){
    gs_collection_file_msg("Import failed. Please check your file again.")
    gs_collection_file(NULL)
    return(NULL)
  }, warning=function(war){
    gs_collection_file_msg("Import failed. Please check your file again.")
    gs_collection_file(NULL)
    return(NULL)
  })
  
})

##Output hallmark warning message###
output$add_gs_collection_file_msg <- renderUI({
  
  req(gs_collection_file_msg())
  
  p(class="fileInputMsg",  HTML(gs_collection_file_msg()))
  
})

