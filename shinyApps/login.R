
loginUI <- function(id, title="Sign In", user_title="User Name", pass_title="Password",
                    login_title="Sign in", error_message="Invalid username or password!") {
  
  ns <- shiny::NS(id)

  fluidRow(
    class="signin-page", id=ns("panel"),
    
    column(
      width=4, offset=4, class="login-container",
      
      div(class="login-form", 
          div(class="login-form-title", strong('Sign In'))
      ),
      
      div(
        class="login-validate-form",
        
        shiny::textInput(inputId=ns("user_name"), label=shiny::tagList(shiny::icon("user"), user_title), width="auto"),

        tags$script('document.getElementById(ns("user_name")).focus();'),

        shiny::passwordInput(inputId=ns("password"), label=shiny::tagList(shiny::icon("unlock-alt"), pass_title),  width="auto"),
        
        shinyjs::hidden(
          shiny::div(
            id = ns("error"),
            shiny::tags$p(error_message, style = "color: red; font-weight: bold; padding-top: 5px;", class = "text-center")
          )
        ),
        
        br(),
        
        shiny::div(
          style = "text-align: center;",
          shiny::actionButton(inputId=ns("sign_in"), class="login-btn", label=strong(login_title), onkeypress="loginfunction(event)", width="auto")
        ),

        tags$script(
          paste0(
            'function loginfunction(e){',
              'if (e.which === 13) {',
                'document.getElementById("sign_in").click();',
              '}',
            '}'
          )
        ),
        
        br(), br(),

        shiny::tags$p(
          style="text-align: center",
          actionLink(inputId="ForgetPassword", label=strong("Forgot Password?"), width="auto")
        )
      )
    )
  )
}

login <- function(input, output, session, sodium_hashed=TRUE, log_out=NULL) {
  
  credentials <- shiny::reactiveValues(user_auth = FALSE, info = NULL)
  
  shiny::observeEvent(log_out(), {
    credentials$user_auth <- FALSE
    credentials$info <- NULL
    shiny::updateTextInput(session, "password", value = "")
  })
  
  shiny::observeEvent(credentials$user_auth, ignoreInit = TRUE, {
    shinyjs::toggle(id = "panel")
  })
  
  users <- "Username"
  pwds <- "Password"

  shiny::observeEvent(input$sign_in, {
    
    ##Read in the data
    data <- read_csv(paste0("data/User_Login_List.csv"))
    
    # ensure all text columns are character class
    data <- dplyr::mutate_if(data, is.factor, as.character)
    
    # check for match of input username to username column in data
    row_username <- which(dplyr::pull(data, !!users) == trimws(input$user_name))
    
    if (length(row_username)) {
      row_password <- dplyr::filter(data,dplyr::row_number() == row_username)
      row_password <- dplyr::pull(row_password, !!pwds)
      if (sodium_hashed) {
        password_match <- sodium::password_verify(row_password, input$password)
      } else {
        password_match <- identical(row_password, input$password)
      }
    } else {
      password_match <- FALSE
    }
    
    # if user name row and password name row are same, credentials are valid
    if (length(row_username) == 1 && password_match) {
      shinyjs::hide(id = "error")
      credentials$user_auth <- TRUE
      credentials$info <- dplyr::filter(data, !!users == input$user_name)
    } else { # if not valid temporarily show error message to user
      shinyjs::show(id = "error")
    }
    
  })
  
  # return reactive list containing auth boolean and user information
  shiny::reactive({
    shiny::reactiveValuesToList(credentials)
  })
  
}

sendpassword <- function(from_sender="montilab@bu.edu", to_recipient="montilab@bu.edu", recipient_first="Montilab", recipient_last="Montilab", recipient_account="Montilab", tmp_pwd){
  
  recipient=paste(recipient_first, recipient_last)
  
  msg <- mime_part(
    paste0(
      '<!DOCTYPE>',
      '<html>',
      '<head>',
      '<meta http-equiv="Content-Type" content="text/html; charset=utf-8"/>',
      '<meta name="viewport" content="width=device-width, initial-scale=1.0"/>',
      '<title>HTML MESSAGE</title>',
      '<style type="text/css">',
      '</style>',
      '</head>',
      '<body>',
      '<p>Hi <strong>', recipient_first, ',</strong></p>',
      '<p>The password for your Xposome account has changed.</p>',
      '<p></p>',
      '<p>Username: <strong>', recipient_account, '</strong></p>',
      '<p>Temporary password: <strong>', tmp_pwd, '</strong></p>',
      '<br>',
      '<p>Log back in? Follow this link, <strong>http://155.41.202.164/Xposome/?sign_in</strong></p>',
      '<br>',
      '<p>Best,</p>',
      '<p>Montilab Team</p>',
      '</body>',
      '</html>' 
    )
  )
  
  ## Override content type.
  msg[["headers"]][["Content-Type"]] <- "text/html"
  
  from <- paste0("\"Montilab Team\"<", from_sender, ">")
  to <- paste0("\"", recipient, "\"<", to_recipient, ">", collapse="")
  subject <- "Temporary password for Xposome"
  body <- list(msg)
  sendmail(from, to, subject, body, control=list(smtpServer="smtp.bu.edu", smtpPort="25"))
  
}

