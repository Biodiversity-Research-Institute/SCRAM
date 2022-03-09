#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# created: 16 Feb 2022
# A. Gilbert
# Biodiversity Research Institute
# 276 Canco Rd
# Portland, ME 04103
# This tool was modified from one largely developed by Chris Field at the University of Rhode Island and depends on code
# from the Stochastic Band model


source("helpers.R")
# currentdir <- normalizePath(getwd(), winslash = "/")
# source(file.path(currentdir, "helpers.R"))
options(shiny.trace = F)

ui <- dashboardPage(
  skin = "green",
  dashboardHeader(
    # div(style = "height:95px;padding-top: 10px", imageOutput("myImageHeader")),
    
    titleWidth = 400,
    # title =
    # tags$h4("Stochastic Collision Risk Assessment for Movement"
    # ),
    tags$li(
      class = "dropdown",
      actionLink("appvrsn", label = tags$b("Stochastic Collision Risk Assessment for Movement: v0.7 - Arbovitae"), style = "font-size: 19px"),
      style = "float: left"
    ),
    
    tags$li(
      class = "dropdown",
      a(
        icon('github', "fa-2x"),
        href = 'https://github.com/bltern/SCRAM',
        style = "padding-top: 10px; padding-bottom: 10px",
        target = '_blank',
        id = "lbl_codeLink"
      ),
      style = "float: left"
    ),
    
    tags$li(
      class = "dropdown",
      a(
        icon('bug', "fa-2x"),
        href = 'https://github.com/bltern/SCRAM/issues',
        #exclamation-circle
        style = "padding-top: 10px; padding-bottom: 10px",
        target = '_blank',
        id = "lbl_issuesLink"
      ),
      style = "float: left"
    ),
    
    # tags$li(class = "dropdown", actionLink("bookmark_btt", label = NULL, icon("bookmark", "fa-2x", lib = "font-awesome"),
    #                                        style = "padding-top: 10px; padding-bottom: 10px")),
    # tags$li(class = "dropdown", actionLink("saveInputs_btt", label = NULL, icon("save", "fa-2x", lib = "font-awesome"),
    #                                        style = "padding-top: 10px; padding-bottom: 10px")),
    # tags$li(class = "dropdown", actionLink("restoreInputs_btt", label = NULL, icon("window-restore", "fa-2x", lib = "font-awesome"),
    #                                        style = "padding-top: 10px; padding-bottom: 10px")),
    
    
    tags$li(
      class = "dropdown",
      a(
        img(src = "BRI_color_logo_no_words.png", height = "40px"),
        href = 'https://briwildlife.org',
        style = "padding-top: 5px; padding-bottom: 5px;",
        target = '_blank',
        id = "lbl_BRILogoLink"
      ),
      style = "float: right"
    ),
    tags$li(
      class = "dropdown",
      a(
        img(src = "URI.png", height = "40px"),
        href = 'https://URI.edu',
        style = "padding-top: 5px; padding-bottom: 5px;",
        target = '_blank',
        id = "lbl_URILogoLink"
      ),
      style = "float: right"
    ),
    tags$li(
      class = "dropdown",
      a(
        img(src = "USFWS.png", height = "40px"),
        href = 'https://www.fws.gov/',
        style = "padding-top: 5px; padding-bottom: 5px;",
        target = '_blank',
        id = "lbl_FWSLogoLink"
      ),
      style = "float: right"
    ),
    tags$li(
      class = "dropdown",
      a(
        img(src = "BOEM.png", height = "40px"),
        href = 'https://www.BOEM.gov',
        style = "padding-top: 5px; padding-bottom: 5px",
        target = '_blank',
        id = "lbl_BOEMLogoLink"
      ),
      style = "float: right"
    )
  ),
  
  dashboardSidebar(
    collapsed = F,
    width=400,
    sidebarMenu(
      id = "sidebar",
      tags$a(
        img(src = "SCRAM_logo_400px.png", alt="Stochastic Collision Risk Assessment for Movement", width = "400px", class="header_img"),
        href = 'https://briwildlife.org/SCRAM',
        style = "margin-bottom: 10px;",# padding-right:20px; margin-bottom: -10px; margin-top:-45px;",
        target = '_blank',
        id = "lbl_SCRAMLogoLink"
      ),
      
      h4("1) Select the species or load species data:", style = "padding-left: 10px;"),

      ################### Input: Select the species to model
      radioButtons(inputId = "species_input",
                   label ="Select included species data or your own:",
                   choices = c("Piping Plover" = "Piping_Plover", "Red Knot" = "Red_Knot", "Roseate Tern" = "Roseate_Tern", "Use your own species data" = "Other"),
                   selected = character(0)), #start with no items selected
      conditionalPanel(  #only expand if you selected other to hide the load button
        condition = "input.species_input == 'Other'",
        # downloadButton("downloadSpeciesExample", "Download example species input", 
        #                style = "margin-left: 20px; margin-top: 0px; background-color: blue; color: white; font-weight: bold;"), 
        fileInput("file_spp_param", "Upload species data and flight height distributions", accept = ".csv", multiple = TRUE, width = '90%')
        ),
      # checkboxInput("confirm_species_data", "Confirm check of species data"),
      hr(),
      # verbatimTextOutput("debug"),
      #################Enter wind farm parameters
      conditionalPanel( 
        #show only when species data have been inputted
        condition = 'input.species_input',
        h4("2) Load wind farm parameters:", style = "padding-left: 10px;"),
        # downloadButton("downloadTurbineExample", "Download example wind farm input", 
        #                style = "margin-left: 20px; margin-top: 0px; background-color: orange; color: white; font-weight: bold;"),
        fileInput("file_wf_param", "Upload wind farm data", accept = c('text/csv', 
                                                             'text/comma-separated-values,text/plain', 
                                                             '.csv'), width = '95%'),
        hr(),
      ),
      
      #################Enter CRM options
      conditionalPanel( 
        #show only when wind farm data have been inputted
        condition = "output.fileUploaded",
        h4("3) Select CRM parameter options:", style = "padding-left: 10px;"),
        radioButtons("optionradio", "Use complete flight height data?",
                     c("Yes" = "3", "No (faster)" = "1")),
        sliderInput("slider1", label = "Iterations", min = 100, 
                    max = 10000, value = 100, step=100, width = '95%'), 
        htmlOutput("message", style = "margin-left: 20px"),
        br(),
        numericInput(
          inputId = "inputthreshold",
          label = "Threshold",
          value = 0,
          min = 0,
          max = NA,
          step = NA,
          width = '50%'),
        hr(),
        h4("4) Run CRM:", style = "padding-left: 10px;"),
        fluidRow(column(4, 
                        uiOutput("runui")), 
                 column(4, 
                        uiOutput("cancel")))
      )

  )),

  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet",
                type = "text/css",
                href = "www/style.css")
    ),

    useShinyjs(),

    tabsetPanel(
      id = "tabsetpan",
      type = "tabs",
      selected = "species_panel",
      tabPanel(
        "Species Data",
        value = "species_panel",
        fluidRow(
          box(
            # title = "Instructions",
            status = "primary",
            solidHeader = F,
            collapsible = T,
            width = 12,
            style = "margin-top: -20px; padding-bottom: 20px",
            fluidRow(
              column(width = 8, htmlOutput("user_instructions", style = "margin-top: -14px")),
              
              column(width = 4, 
                     htmlOutput("downloads"), 
                     downloadButton("downloadDataX", "Manual",
                                    style = "margin-bottom: 8px; background-color: gray; color: black; font-weight: bold;"),
                     br(),
                     downloadButton("downloadSpeciesExample", "Example species input", 
                                    style = "margin-bottom: 8px; background-color: blue; color: white; font-weight: bold;"), 
                     br(),
                     downloadButton("downloadTurbineExample", "Example wind farm input", 
                                    style = "margin-bottom: 8px; background-color: orange; color: white; font-weight: bold;"),
              )
            )
          ) 
        ),
        fluidRow(
          #show the species data prior to modeling for checks
          column(6,
                 fluidRow(
                   box(
                     title = "Species Data",
                     status = "primary",
                     solidHeader = TRUE,
                     width = 12,
                     dataTableOutput("species_data")
                   )
                 )
          ),
          #Show the flight height data raw and as figure to help to make sure user check these before running
          column(6,
                 fluidRow(
                   box(
                     title = "Flight Height Data Plot",
                     status = "primary",
                     solidHeader = TRUE,
                     width = 12,
                     plotOutput("flt_ht_plot")
                   )
                 ),
                 fluidRow(
                   box(
                     title = "Flight Height Data Summary",
                     status = "success",
                     solidHeader = TRUE,
                     width = 12,
                     dataTableOutput("flt_ht_data")
                   )
                 )
          )
        )
      ),
        
      tabPanel(
        "Wind Farm Data",
        value = "wind_farm_panel",
        fluidRow(
          box(
            status = "primary",
            solidHeader = F,
            collapsible = T,
            width = 12,
            style = "margin-top: -20px; padding-bottom: 20px",
            htmlOutput("check_windfarm_instructions", style = "margin-top: -14px")
          )
        ),
        fluidRow(
          column(9,
                fluidRow(
                  # tabBox(
                  #   title = "Wind Farm Data",
                  #   id="wf_tab_box",
                  #   width = 12,
                  #   
                  #   # tabPanel("prelim"),
                  #   uiOutput("wf_runs_tabs")
                  # ),
                   box(
                     title = "Wind Farm Turbine and Project Area Data",
                     status = "success",
                     solidHeader = TRUE,
                     width = 12,
                     dataTableOutput("wind_farm_data1"),
                     dataTableOutput("wind_farm_data2"),
                     dataTableOutput("wind_farm_data3")
                   )
                 )
          ),
          column(3,
                 fluidRow(
                   box(
                     title = "Wind Farm Map",
                     status = "primary",
                     solidHeader = TRUE,
                     width = 12,
                     leafletOutput("studymap", width = "100%")
                   )
                 ))
        ),
        fluidRow(
          box(
            title = "Wind Farm Monthly Operational Data",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            dataTableOutput("ops_data")
          )
      )),
      
      #CRM results tab
      tabPanel("CRM Results", value = "crm_results",
               fluidRow(
                 box(
                   title = "Results",
                   status = "primary",
                   solidHeader = F,
                   collapsible = F,
                   width = 12,
                   fluidRow(column(
                     width = 12, verbatimTextOutput("hack", placeholder = TRUE), 
                     # verbatimTextOutput("study_design_report_txt")
                   ))
                 )
               ),
               fluidRow(
                 column(6,
                        h4("Output dashboard"), 
                        br(),
                        plotOutput("plot2", height = "300px", width = "460px")
                 ), 
                 column(6,
                        h4("Next steps:"), 
                        br(),
                        uiOutput("runGSA2"), 
                        br(),
                        uiOutput("download_output"), 
                        br(),
                        uiOutput("genreport"), 
                 ), 
               ), 
               fluidRow(
                 br(),
                 br(),
                 h4("This tool was developed by Biodiversity Research Institute, The University of Rhode Island, and 
                    U.S. Fish and Wildlife Service with funding from the Bureau of Ocean Energy Management.", 
                    style = "margin: 32px; color: steelblue; font-weight: bold;")

               )
      )
    )
  )
)


verbose <- T

# Define server logic
server <- function(input, output, session) {
  # mute option for receiving results by email until authorization key issue is fixed
  #  gm_auth_configure(key = "472195874087-13rt4dr5d4772egku8al5iqg74ugv9uj.apps.googleusercontent.com", secret = "2vihbdJYX2KSUTqOlmzGcbru", appname = "gmailr")
  
  # mute this after the folder is created for the first time
  #gm_auth(email = TRUE, cache = "Gmail_oauth.secret")
  
  #  options(
  #    gargle_oauth_cache = "Gmail_oauth.secret",
  #    gargle_oauth_email = "collidercrm@gmail.com"
  #  )
  
  #  gm_auth(email = "collidercrm@gmail.com")
  
  #  valid_email <- function(x){
  #    grepl("\\<[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}\\>", as.character(x), ignore.case=TRUE)
  #  }
  
  #  observeEvent({req(!is.null(CRM_fun()$monthCollsnReps_opt1))}, {
  #    if(input$email == TRUE){
  #      unlink(paste0(tempdir(),"/CollideR_results.zip"))
  #      tmpdir = tempdir()
  #      fnames4zip1 <- list()
  #      fnames4zip2 <- list()
  #      #fnames4zip3 <- list()
  #      for(e in 1:length(CRM_fun()[['CRSpecies']])){
  #        for(w in 1:length(CRM_fun()[['Turbines']])){
  #          sindex <- CRM_fun()[['CRSpecies']][e]
  #          tindex <- paste0("turbModel", CRM_fun()[['Turbines']][w])
  #          write.csv(CRM_fun()[[as.numeric(input$optionradio)]][[sindex]][[tindex]], file = paste0(tmpdir, "/", sindex, "_", tindex,".csv"), row.names = FALSE)
  #          write.csv(cbind(CRM_fun()[["sampledParamsTurbine"]][[sindex]][[tindex]],
  #                          CRM_fun()[["sampledParamsBird"]][[sindex]][[tindex]]), file = paste0(tmpdir, "/", sindex, "_", tindex,"_params.csv"), row.names = FALSE)
  #          fnames4zip1 <- c(fnames4zip1, paste0(tmpdir, "/", sindex, "_", tindex,".csv", sep=""))
  #          fnames4zip2 <- c(fnames4zip2, paste0(tmpdir, "/", sindex, "_", tindex,"_params.csv", sep=""))
  #          #fnames4zip3 <- c(fnames4zip3, paste0(tmpdir, "/", CRM_fun()[['CRSpecies']][e], "_", paste0("turbModel", CRM_fun()[['Turbines']][w]),"_sensitivity.csv"))
  #        }}
  #      zip(zipfile=paste0(tmpdir,"/CollideR_results.zip"), files=unlist(c(fnames4zip1, fnames4zip2)), flags = "-r9Xj")
  #    }
  #  })
  
  #  suppressWarnings(observeEvent({req(!is.null(CRM_fun()$monthCollsnReps_opt1))}, {
  #    if(input$email == TRUE){
  #      validate(need(valid_email(as.character(input$user_email)), "Please enter a valid email"))
  #      email <- gm_mime(From="collidercrm@gmail.com", To=as.character(input$user_email), Subject="CollideR CRM results") %>%
  #        gm_html_body("Results of model runs attached.") %>%
  #        gm_attach_file(paste0(tempdir(), "/CollideR_results.zip"))
  #        gm_send_message(email)
  #    }
  #  })
  # )
  
  output$fileUploaded  <- reactive({
    val <- !(is.null(input$file_wf_param))
  })
  outputOptions(output, 'fileUploaded', suspendWhenHidden=FALSE)
  
  # output$debug=renderPrint(output$fileUploaded)
  
  output$user_instructions <- renderText(
  "<h4>INSTRUCTIONS:</h4><br>
    1) Select the species of interest or select the option for providing your own species data.<br>
    2) Upload species data (optional if selecting from among the three target species).<br>
    3) Check the species data for correct values.<br>
    4) Upload turbine and array data.<br>
    5) Check the wind farm data for correct values.<br>
    6) Choose which version of the CRM to run.<br>
    6) Select the number of iterations (100-10,0000).<br>
    7) Set a threshold for the maximum acceptable number of collisions.<br>
    8) Run CRM.<br>
    9) Run sensitivity analyses (optional).<br>
    10) Generate summary report and/or download results for each iteration.<br>
    11) Check the CRM results.")
  
  output$check_windfarm_instructions <- renderText(
    "<h4>INSTRUCTIONS:</h4><br>
     Check the wind farm data carefully before running this tool to make sure it's correct.<br>
     Fix any data in your original data file and upload again."
  )
  
  output$downloads <- renderText("<h4>DOWNLOAD FILES:</h4><br>")
  
  # if send email is selected, render text box for entering email address
  observeEvent({req(input$email == TRUE)}, {
    output$user_email_ui <- renderUI({
      textInput("user_email", label=NULL, value = "asdf@emailclient.com",
                width = '75%')
    })
  })

  # # load and render the GitHub logo
  # output$myImage <- renderImage({
  #   list(src = "data/GitHub-Mark.png",
  #        contentType = 'image/png',
  #        width = 40,
  #        height = 40,
  #        alt = " ")
  # }, deleteFile = FALSE)
  # 
  # output$myImageHeader <- renderImage({
  #   list(src = "data/collider_header_bw.png",
  #        contentType = 'image/png',
  #        width = 625,
  #        height = 75,
  #        alt = " ")
  # }, deleteFile = FALSE)

  # load labels to display versions of species names without underscores
  SpeciesLabels <- read.table("data/SpeciesLabels.csv", sep =",")

  # main plot for annual collisions
  observeEvent(input$run, {output$plot2 <- renderPlot({
    if(!is.null(CRM_fun()$monthCollsnReps_opt1)){
      if(sum(CRM_fun()[[as.numeric(input$optionradio)]][[CRM_fun()[['CRSpecies']][1]]][[1]], na.rm=TRUE)>0){
        NA_index <- which(is.na(CRM_fun()[[as.numeric(input$optionradio)]][[CRM_fun()[['CRSpecies']][1]]][[1]][1,]))
        outvector <- round(rowSums(CRM_fun()[[as.numeric(input$optionradio)]][[CRM_fun()[['CRSpecies']][1]]][[1]], na.rm = TRUE))
        month_lab <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
        if(max(outvector) >= 1000){
          xmin <- round(min(outvector, na.rm=TRUE))
          xmax <- round(max(outvector, na.rm=TRUE))
          bin_wd <- round(max(outvector)/10)
          brks <- seq(xmin, xmax+bin_wd, by=bin_wd)
          pad_min <- round(xmin*0.1)
          pad_max <- round(xmax*0.1)
          steps <- round(((xmax + pad_max) - (xmin - pad_min))/4)
        }
        if(max(outvector) >= 10&max(outvector) < 1000){
          xmin <- round(min(outvector, na.rm=TRUE))
          xmax <- round(max(outvector, na.rm=TRUE))
          bin_wd <- round(max(outvector)/10)
          brks <- seq(xmin, xmax+bin_wd, by=bin_wd)
          #brks <- seq(xmin, xmax, by=1)
          pad_min <- round(xmin*0.1)
          pad_max <- round(xmax*0.1)
          steps <- round(((xmax + pad_max) - (xmin - pad_min))/4)
        }
        if(max(outvector) <= 2&max(outvector)>1){
          xmin <- 0
          xmax <- 2.1
          brks <- seq(0, 2, by=0.2)
          pad_min <- 0
          pad_max <- 0
        }
        if(max(outvector) <= 1){
          xmin <- 0
          xmax <- 1.1
          brks <- seq(0, 1, by=0.1)
          pad_min <- 0
          pad_max <- 0
        }
        if(max(outvector) < 10&max(outvector) > 2){
          xmin <- 0
          xmax <- 10.5
          brks <- seq(0, 10, by=1)
          pad_min <- 0
          pad_max <- 0
        }
        layout(matrix(c(1, 1, 1, 1, 1, 1, 1, 1, 1, 2), 10, 1))
        par(mar=c(5, 2, 4, 2))
        if(length(which(SpeciesLabels[,1] == CRM_fun()[['CRSpecies']][1]))>0){
          main_label <- SpeciesLabels[SpeciesLabels[,1] == CRM_fun()[['CRSpecies']][1], 2]
        }else{
          main_label <- CRM_fun()[['CRSpecies']][1]
        }
        hist(outvector, freq=FALSE, main = main_label, xlab=" ", ylab=" ", bty="n",
             xlim=c(xmin - pad_min, xmax + pad_max), xaxt="n", breaks=brks, border = "white", col="dark green", #rgb(175/255, 122/255, 197/255, 0.8),
             cex.axis=1.5, cex.main=1.7, yaxt="n")
        mtext(side=1, line=2.8, "Total collisions over months highlighted below")
        box(which="outer")
        if(max(outvector) >= 10){
          ticks <- round(c((xmin - pad_min), ((xmin - pad_min) +(1*steps)), ((xmin - pad_min) + (2*steps)), ((xmin - pad_min) +(3*steps)), (xmax + pad_max)), digits=1)
          axis(side=1, at = ticks, cex.axis=1.5)
        }
        if(max(outvector) <= 2&max(outvector) > 1){
          axis(side=1, labels = c(0, 0.4, 0.8, 1.2, 1.6, 2), at = c(0, 0.4, 0.8, 1.2, 1.6, 2)+0.1, cex.axis=1.5)
        }
        if(max(outvector) <= 1){
          axis(side=1, labels = c(0, 0.2, 0.4, 0.6, 0.8, 1), at = c(0, 0.2, 0.4, 0.6, 0.8, 1)+0.05, cex.axis=1.5)
        }
        if(max(outvector) < 10&max(outvector) > 2){
          axis(side=1, labels = c(0, 2, 4, 6, 8, 10), at = c(0, 2, 4, 6, 8, 10)+0.5, cex.axis=1.5)
        }
        polygon(x=c(input$inputthreshold, 0, 0, input$inputthreshold), y=c(0, 0, 1, 1), col=rgb(1, 1, 1, 0.65), border=rgb(0, 0, 0, 0))
        bold <- rep(2, 12)
        month_col <- rep("dark blue", 12)
        month_col[NA_index] <- rgb(0, 0, 0, 0.8)
        bold[NA_index] <- 1
        par(mar=c(0, 0, 0, 0))
        plot(-10, -10, col="white", xlim=c(0.5, 12.5), ylim=c(1, 3))
        text(1:12, rep(2, 12), month_lab, cex=1.6, font = bold, col = month_col)
      }else{
        par(mar=c(4, 4.5, 3, 1))
        plot(1:1, col="white", xlim=c(0, 10), ylim=c(0, 10), xaxt="n", yaxt="n", xlab=" ", ylab=" ", bty="n")
        text(4, 5, "Option not run", cex=2, adj=c(0.5, 0.5))
        box(which="outer")
      }
    }
  })
  })

  # render text to report the probability of collisions exceeding a user-specified threshold
  observeEvent(input$run, {output$prob <- renderText({
    if(!is.null(CRM_fun()$monthCollsnReps_opt1)){
      threshold_text <- length(which(rowSums(CRM_fun()[[as.numeric(input$optionradio)]][[CRM_fun()[['CRSpecies']][1]]][[1]], na.rm=TRUE) > input$inputthreshold))/
        length(rowSums(CRM_fun()[[as.numeric(input$optionradio)]][[CRM_fun()[['CRSpecies']][1]]][[1]]))
      if(threshold_text == 1){
        threshold_text <- paste("<", isolate(round(1 - 1/input$slider1, log10(input$slider1))), sep=" ")
      }
      if(threshold_text == 0){
        threshold_text <- paste("<", isolate(round(((1/input$slider1)), log10(input$slider1))), sep=" ")
      }
      paste("The probability of exceeding specified threshold is ", threshold_text, ".", sep="")
    }
  })
  })

  # dialog box for sensitivity analyses
  observeEvent(input$runGSA, {
    {showModal(modalDialog(
      title = "Sensitivity analyses ran successfully",
      footer = modalButton("OK"),
      paste("Global sensivity analyses for ", option_labels[as.numeric(input$optionradio)], " ran successfully. Results ready to download.", sep="")
    ))}
  })

  # mute more complex option selection to instead limit options to one best and one fast
  # update the radio button selection based on whether a flight height distribution is available
  #observeEvent(input$FHD, {
  #  if(input$FHD == TRUE){
  #    updateRadioButtons(session, "optionradio",
  #                       label = "Collision risk model",
  #                       choiceNames = c("Approximated", "Individual-based"),
  #                       choiceValues = c(3, 6))
  #  }
  #  if(input$FHD == FALSE){
  #    updateRadioButtons(session, "optionradio",
  #                       label = "Collision risk model",
  #                       choiceNames = c("Basic", "Approximated", "Individual-based"),
  #                       choiceValues = c(1, 2, 5))
  #  }
  #})

  # approximate the run time for 1 species and 1 turbine; reactive with user input for number of iterations
  times <- c(0.1, 1.2, 0.8, 5.9, 1.9, 3.1)
  option_labels <- c("Basic CRM", "NA", "CRM with full FHD")

  # load scripts for the main collision risk computation and the global sensitivity analyses
  source("BandModel_function_cf.R")
  source("scripts/GSA.R")

  # create reactive objects to be used in the main risk computation script, with the ability to update with user inputs
  # species input
  speciesreact <- reactiveValues()
  speciesreact <- eventReactive(c(input$species_input,input$file_spp_param$datapath), {if(length(which(input$species_input=="Other"))==0){
    input$species_input
  }else{
    suppressWarnings(read.table(input$file_spp_param[[which(
      sapply(1:length(input$file_spp_param$datapath), function(x) length(suppressWarnings(read.table(input$file_spp_param[[x,"datapath"]], header=TRUE, sep = ","))[1,]))
      == 10)[1],"datapath"]], header=TRUE, sep = ","))[,1]
  }
  })
  # an object that indexes whether at least one file has been uploaded for both species and turbine data
  bothdataup <- reactiveVal()
  bothdataup  <- eventReactive(c(input$"file_spp_param", input$"file_wf_param"), {length(input$file_spp_param$datapath) > 0&length(input$file_wf_param$datapath) > 0})
  # number of iterations
  sliderreact <- reactiveVal()
  sliderreact <- eventReactive(input$slider1, {as.numeric(input$slider1)})
  # input for the model options
  radioreact <- reactiveVal()
  radioreact <- eventReactive(input$optionradio, {input$optionradio})
  # turbine data
  tablereact <- reactiveValues()
  tablereact <- eventReactive(input$file_wf_param, {
    suppressWarnings(read.table(input$file_wf_param$datapath, header=TRUE, sep = ","))
    })
  
  wind_farm_df <- reactiveValues()
  wind_farm_df <- eventReactive(input$file_wf_param, {
    infile <- input$file_wf_param
    if (is.null(infile)) {
      return(NULL)
    }
    suppressWarnings(read.csv(infile$datapath, header=TRUE))
  })

  observeEvent(input$file_wf_param, {
    Sys.sleep(3)  #wait until species data has rendered and then switch also helps with pre-rendering maps, etc.
    updateTabItems(session, inputId = "tabsetpan", selected = "wind_farm_panel")})

  #render the wind farm data to the tables on the wind farm data tab
  output$wind_farm_data1 <- DT::renderDataTable(
    wind_farm_df() %>%
      select(Run, 2:6, -contains("Op")),
    options = list(
      dom = 't',
      scrollX = TRUE
    ))
  
  output$wind_farm_data2 <- DT::renderDataTable(
    wind_farm_df() %>%
      select(Run, 7:10, -contains("Op")),
    options = list(
      dom = 't',
      scrollX = TRUE
    ))
  
  output$wind_farm_data3 <- DT::renderDataTable(
    wind_farm_df() %>%
      select(Run, 11:17, -contains("Op")),
    options = list(
      dom = 't',
      scrollX = TRUE
    ))
    
  #render the map with the lat/longs given in the study area map panel
  output$studymap <- renderLeaflet({
    studymap <- leaflet(options = leafletOptions(preferCanvas = T, tolerance = 1)) %>%
      addEsriBasemapLayer(esriBasemapLayers$Oceans, autoLabels = TRUE) %>% 
      addEsriFeatureLayer(
        #add BOEM renewable lease areas as a WFS
        url = "https://services1.arcgis.com/Hp6G80Pky0om7QvQ/ArcGIS/rest/services/BOEM_Wind_Planning_and_Lease_Areas/FeatureServer/0",
        weight = 1, fill=FALSE, color = "#808080", #gray
        layerId = "BOEM_wind_leases", 
        group = "BOEM wind leases") %>% 
      addEsriFeatureLayer(
        #add BOEM renewable lease areas as a WFS
        url = "https://services1.arcgis.com/Hp6G80Pky0om7QvQ/ArcGIS/rest/services/BOEM_Wind_Planning_and_Lease_Areas/FeatureServer/2",
        weight = 1, fill=FALSE, color = "#C0C0C0", #silver
        layerId = "BOEM_wpa", 
        group = "BOEM wind planning areas") %>% 
      #MOTUS antenna data
      addMarkers(
        data = wind_farm_df(),
        # data = react_latlon(),
        lat = ~ Latitude,
        lng = ~ Longitude,
        popup =
          paste0(
            "Run: ",
            wind_farm_df()$Run,
            "<br/>Number of turbines: ",
            wind_farm_df()$Num_Turbines,
            "<br/>Turbine Model (MW): ",
            wind_farm_df()$TurbineModel_MW,
            "<br/>Rotor radius (m): ",
            wind_farm_df()$RotorRadius_m,
            "<br/>Hub Height (m): ",
            wind_farm_df()$HubHeightAdd_m,
            "<br/>BladeWidth (m): ",
            wind_farm_df()$BladeWidth_m,
            "<br/>Pitch: ",
            wind_farm_df()$Pitch,
            "<br/>Wind farm width (km): ",
            wind_farm_df()$WFWidth_km
          ),
        group = "Wind farm"
      ) %>%
      setView(lat = mean(wind_farm_df()$Latitude), lng = mean(wind_farm_df()$Longitude), zoom = 6) %>%
      #Layers control
      addLayersControl(
        overlayGroups = c("Wind farm", "BOEM wind leases", "BOEM wind planning areas"),
        position = "topright", #"topleft",
        options = layersControlOptions(collapsed = TRUE)
      )
  })
  
#show the Wind Farm operational data as as table for QA/QC
  output$ops_data <-
    DT::renderDataTable(
      DT::datatable({
        ops_data_all <- data.frame()
        for (i in 1:nrow(wind_farm_df())) {
          # run <- row[["run"]]
          row <- wind_farm_df()[i,]
          ops_data <- row %>% 
            mutate(Var="MonthOp", Desc="Wind availability (maximum amount of time turbines can be operational/month)") %>% 
            select(Run, Var, Desc, matches("Op$")) %>% 
            rename_with(~ gsub("Op", "", .x)) #rename to month only  
          
          ops_mean_data <- row %>% 
            mutate(Var="MonthOpMean", Desc='Mean time that turbines will not be operational ("Down time").') %>% 
            select(Run, Var, Desc, matches("Mean$")) %>% 
            rename_with(~ gsub("OpMean", "", .x)) #rename to month only  
          
          ops_SD_data <- row %>% 
            mutate(Var="MonthOpSD", Desc="deviation of mean operational time") %>% 
            select(Run, Var, Desc, matches("OPSD$")) %>% 
            rename_with(~ gsub("OpSD", "", .x))
          
          # ops_data_run <- rbind(ops_data, ops_mean_data, ops_SD_data)
          ops_data_all <- rbind(ops_data_all, ops_data, ops_mean_data, ops_SD_data)
        }
        return(ops_data_all)
        },
        options = list(rownames = FALSE, pagelength=20, dom = 't')
        )
      )
  

  # data for species characteristics
  tablereact3 <- reactiveValues()
  tablereact3 <- eventReactive(c(input$file_spp_param, input$species_input), {
    if(!is.null(input$file_spp_param)){
      if(length(which(
        sapply(1:length(input$file_spp_param$datapath), function(x) length(suppressWarnings(read.table(input$file_spp_param[[x,"datapath"]], header=TRUE, sep = ","))[1,]))
        ==10))>0){
        suppressWarnings(read.table(
          input$file_spp_param[[which(sapply(1:length(input$file_spp_param$datapath), function(y)
            length(suppressWarnings(read.table(input$file_spp_param[[y,"datapath"]], header=TRUE, sep = ","))[1,]))
            ==10)[1],"datapath"]], header=TRUE, sep = ","))
      }else{
        read.csv("data/BirdData.csv", header = T)
      }
    }else{
      read.csv("data/BirdData.csv", header = T)
    }
  })
  
  species_data_react <- reactiveValues()
  species_data_react <- eventReactive(c(input$file_spp_param, input$species_input), {
    if(!is.null(input$file_spp_param)){
    }else{
      bird_data <- read.csv("data/BirdData.csv", header = T)
      species_data_row <- melt(bird_data[which(bird_data$Species==input$species_input), ], id.var=NULL)
      # colnames(species_data_row) <- c("values")
    }
    updateTabItems(session, inputId = "tabsetpan", selected = "species_panel")
    return(species_data_row)
  })
    
    
  output$species_data <-
    DT::renderDataTable(
      datatable(
        species_data_react(),
        selection = list(mode = "single", selected = 1),
        options = list(
          paging = FALSE,
          scrollY = "500px",
          searching = FALSE
        )
      )
    )
  
  # flight height distributions
  tablereact5 <- reactiveValues()
  tablereact5 <- eventReactive(c(input$file_spp_param, input$species_input), {
    if(!is.null(input$file_spp_param)){
      if(length(which(
        sapply(1:length(input$file_spp_param$datapath), function(x) length(suppressWarnings(read.table(input$file_spp_param[[x,"datapath"]], header=TRUE, sep = ","))[1,]))
        ==1002))>0){
        lapply(1:length(which(
          sapply(1:length(input$file_spp_param$datapath),
                 function(x) length(suppressWarnings(read.table(input$file_spp_param[[x,"datapath"]], header=TRUE, sep = ","))[1,]))
          == 1002)),
          function(x) suppressWarnings(read.table(input$file_spp_param[[which(
            sapply(1:length(input$file_spp_param$datapath), function(x) length(suppressWarnings(read.table(input$file_spp_param[[x,"datapath"]], header=TRUE, sep = ","))[1,]))
            ==1002)[x], "datapath"]], header=TRUE, sep = ","))
        )
      }else{
        lapply(1:length(speciesreact()), function(x) read.csv(paste("data/", speciesreact()[x],"_ht_dflt.csv", sep=''), header = T))
      }
    }else{
      lapply(1:length(speciesreact()), function(x) read.csv(paste("data/", speciesreact()[x],"_ht_dflt.csv", sep=''), header = T))
    }
  })
  
  flt_ht_data_react <- reactiveValues()
  flt_ht_data_react <- eventReactive(c(input$file_spp_param, input$species_input), {
    if(!is.null(input$file_spp_param)){
    }else{
      flt_ht_boot_table <- read.csv(paste0("data/", input$species_input,"_ht_dflt.csv"), header = T)
      n_cols <- ncol(flt_ht_boot_table)
      #summarize across all boot samples
      flt_ht_summary <- flt_ht_boot_table %>%
        group_by(Height_m) %>%
        rowwise() %>% 
        summarise(mean_prop = mean(c_across(3:n_cols-1)), min_prop = min(c_across(3:n_cols-1)), max_prop = max(c_across(3:n_cols-1)))
    }
    return(flt_ht_summary)
  })
  
  output$flt_ht_data <-
    DT::renderDataTable(
      datatable(
        # flt_ht_data_react()[,2:10],
        flt_ht_data_react(),
        selection = list(mode = "single", selected = 1),
        options = list(
          paging = FALSE,
          scrollY = "500px",
          searching = FALSE
        )
      )
    )
  
#flight height data plot
  output$flt_ht_plot <- renderPlot({
    ggplot(flt_ht_data_react()) +
      geom_pointrange(aes(x = Height_m, y = mean_prop, ymin = min_prop, ymax = max_prop)) +
      geom_point(aes(x = Height_m, y = mean_prop), col = "red") +
      xlab("Flight height (m)") +
      ylab("Proportion") +
      coord_flip() +
      theme_bw()
    
  })
  
  # count data
  tablereact7 <- reactiveValues()
  tablereact7 <- eventReactive(c(input$file_spp_param, input$species_input), {
    if(!is.null(input$file_spp_param)){
      if(length(which(
        sapply(1:length(input$file_spp_param$datapath), function(x) length(suppressWarnings(read.table(input$file_spp_param[[x,"datapath"]], header=TRUE, sep = ","))[1,]))
        ==25))>0){
        suppressWarnings(read.table(
          input$file_spp_param[[which(
            sapply(1:length(input$file_spp_param$datapath), function(y) length(suppressWarnings(read.table(input$file_spp_param[[y,"datapath"]], header=TRUE, sep = ","))[1,]))
            ==25)[1],"datapath"]], header=TRUE, sep = ","))
      }else{
        read.csv("data/CountData_motus.csv", header = T)
      }
    }else{
      read.csv("data/CountData_motus.csv", header = T)
    }
  })
  # movement data
  # option for user-provided movement files disabled to simplify input
  tablereact9 <- reactiveValues()
  tablereact9 <- eventReactive(c(input$species_input), {
    #tablereact9 <- eventReactive(c(input$file_spp_param, input$species_input), {
    #  if(!is.null(input$file_spp_param)){
    #    if(length(which(
    #      sapply(1:length(input$file_spp_param$datapath), function(x) length(suppressWarnings(read.table(input$file_spp_param[[x,"datapath"]], header=TRUE, sep = ","))[1,]))
    #      ==13))>0){
    #      lapply(1:length(which(
    #        sapply(1:length(input$file_spp_param$datapath), function(x) length(suppressWarnings(read.table(input$file_spp_param[[x,"datapath"]], header=TRUE, sep = ","))[1,]))
    #        ==13)),
    #        function(x) suppressWarnings(read.table(
    #          input$file_spp_param[[which(sapply(1:length(input$file_spp_param$datapath),
    #                                    function(x) length(suppressWarnings(read.table(input$file_spp_param[[x,"datapath"]], header=TRUE, sep = ","))[1,]))
    #                             ==13)[x], "datapath"]], header=TRUE, sep = ","))
    #      )
    #    }else{
    #      lapply(1:length(speciesreact()), function(x) read.csv(paste("data/MovementData_", speciesreact()[x],".csv", sep=''), header = T))
    #    }
    #  }else{
    lapply(1:length(speciesreact()), function(x) read.csv(paste("data/MovementData_", speciesreact()[x],".csv", sep=''), header = T))
    #  }
  })
  # indicator to let app know whether to use user-provided passage rate data (1) or built-in movement data (0)
  tablereact11 <- reactiveValues()
  tablereact11 <- eventReactive(c(input$file_spp_param, input$file_wf_param), {
    if(!is.null(input$file_spp_param)){
      if(length(which(
        sapply(1:length(input$file_spp_param$datapath), function(x) length(suppressWarnings(read.table(input$file_spp_param[[x,"datapath"]], header=TRUE, sep = ","))[1,]))
        ==25))>0){1}else{0}
    }else{0}
  })

  ### this block deals with making it possible to warn the user that their specified location is in a low confidence area
  movement_type <- reactiveValues()
  movement_type <- eventReactive(c(input$file_wf_param, input$file_spp_param), {
    cell_match <- read.csv("data/BOEM_XY_species_halfdeg.csv", header = T)
    turb_tab <- suppressWarnings(read.table(input$file_wf_param$datapath, header=TRUE, sep = ","))
    loc_match <- mat.or.vec(length(turb_tab[ ,1]) + 2, 1)
    # mute the following block when not accepting user-provided movement data
    #if statement to fill in first position
    #if(!is.null(input$file_spp_param)){
    #  if(length(which(
    #    sapply(1:length(input$file_spp_param$datapath), function(x) length(suppressWarnings(read.table(input$file_spp_param[[x,"datapath"]], header=TRUE, sep = ","))[1,]))
    #    ==13))>0){
    #    loc_match[1] <- 0
    # }else{
    #    loc_match[1] <- 1
    #  }
    #}else{
    loc_match[1] <- 1
    #}
    #if(!is.null(react_latlon()[1])){
    if(react_latlon()[1] != -9){
      loc_match_dists <- lapply(1:length(cell_match[ ,'lat']), function(x) sqrt((cell_match[x ,'lat'] -
                                                                                   turb_tab[1, "Latitude"])^2 +
                                                                                  (cell_match[x ,'lon'] - turb_tab[1, "Longitude"])^2))
      loc_match[2] <- which(unlist(loc_match_dists) == min(unlist(loc_match_dists)))[1]
      loc_match[3] <- cell_match[which(unlist(loc_match_dists) == min(unlist(loc_match_dists)))[1],'area']
    }
    print(paste("loc_match",loc_match))
    loc_match
  })


  # create a vector that stores values for lat and lon to use for identifying the location of simulated wind farm
  # and can indicate whether there are appropriate values for lat/lon
  react_latlon <- reactiveValues()
  react_latlon <- NULL
  react_latlon <- eventReactive(input$file_wf_param, {
    temp_tab <-  suppressWarnings(read.table(input$file_wf_param$datapath, header=TRUE, sep = ","))
    if(length(which(colnames(temp_tab)=="Latitude")) > 0&length(which(colnames(temp_tab) == "Longitude")) > 0){
      temp_lats <- temp_tab[,c("Latitude", "Longitude")]
      #check to see if multiple lat/long value pairs for inputs - not allowed in this version
      # browser()
      if(nrow(temp_lats)>1){
        if(nrow(unique(temp_lats)) > 1){
          # showModal(modalDialog(
          #   title = "Check values for Latitude and Longitude",
          #   footer = modalButton("OK"),
          #   paste("Latitude and/or Longitude are not the same in the different turbine option parameters.
          #     Please upload appropriate values with turbine data.")
          # ))
          temp_lat <- c(-9, -9)
        } else {
          temp_lat <- c(temp_tab[1, "Latitude"], temp_tab[1, "Longitude"])
          
        }
      } else {
        temp_lat <- c(temp_tab[1, "Latitude"], temp_tab[1, "Longitude"])
      }
      
    } else {
      temp_lat <- c(-9, -9)
    }
    
    if(length(which(temp_lat[1] > 45)) < 1&
       length(which(temp_lat[1] < 23)) < 1&
       length(which(temp_lat[2] > (-63))) < 1&
       length(which(temp_lat[2] < (-83))) < 1&
       temp_lat[1] != -9
    ){
      lat_check <- 1
    }else {
      lat_check <- 0

    }
    c(temp_lat, lat_check)
  })
  
  # matrix that can serve as an indicator for whether each selected species is in a low confidence area
  within_conf <- reactiveValues()
  within_conf <- eventReactive(req(!is.null(input$file_wf_param)&length(which(input$species_input!="Other"))>0&length(which(input$species_input=="Other"))==0), {
    #if(!is.null(react_latlon())){
    if(react_latlon()[1] != -9){
      # turb_tab <- suppressWarnings(read.table(input$file_wf_param$datapath, header=TRUE, sep = ","))  #not needed here?
      species_conf <- mat.or.vec(length(input$species_input), 1)
      for(s in 1:length(input$species_input)){
        species_conf[s] <- read.csv("data/BOEM_XY_species_halfdeg.csv", header = T)[movement_type()[2], input$species_input[s]]
      }
      #}
    }else{
      species_conf <- mat.or.vec(2, 1)
    }
    species_conf
  })

  # dialog box to check whether the lats and lons from the turbine data are reasonable values
  observeEvent(input$file_wf_param, {
    {if(length(which(react_latlon()[1] > 45)) > 0|
        length(which(react_latlon()[1] < 23)) > 0|
        length(which(react_latlon()[2] > (-63))) > 0|
        length(which(react_latlon()[2] < (-83))) > 0){
      showModal(modalDialog(
        title = "Check values for Latitude and Longitude",
        footer = modalButton("OK"),
        paste("Latitude and/or Longitude are not specified, not the same for different wind farm parameter runs (only one location allowed), or values fall outside the geographic extent of this tool.
              Please upload appropriate values with turbine data or consult documentation for more information on geographic scope.")
      ))
    }}
  })

  # dialog box to warn user that specified location is in a low confidence area for one or more of the specified species
  observeEvent(c(input$file_wf_param, input$species_input), {
    {if(sum(within_conf()) > 0&
        (length(which(react_latlon()[1] > 45)) < 1&
         length(which(react_latlon()[1] < 23)) < 1&
         length(which(react_latlon()[2] > (-63))) < 1&
         length(which(react_latlon()[2] < (-83))) < 1)
    ){
      showModal(modalDialog(
        title = "Proceed with caution",
        footer = modalButton("OK"),
        paste("Specified location is in an area with less confidence for", sep=""),
        paste(paste(SpeciesLabels[unlist(lapply(1:length(input$species_input[which(within_conf()==1)]), function(x) which(SpeciesLabels[,1]==input$species_input[which(within_conf()==1)][x]))), 2], collapse=", "), ".", sep="", collapse = ", ")
      ))
    }}
  })
  ##

  # dialog box that alerts the user that some data was uploaded, but not every data type was provided
  observeEvent(c(input$file_spp_param, input$species_input), {
    if(!is.null(input$file_spp_param)){
      if(length(which(input$species_input=="Other")) == 0&
         (length(which(sapply(1:length(input$file_spp_param$datapath), function(x) length(suppressWarnings(read.table(input$file_spp_param[[x,"datapath"]], header=TRUE, sep = ","))[1,]))
                       == 10)) == 0|
          length(which(sapply(1:length(input$file_spp_param$datapath), function(x) length(suppressWarnings(read.table(input$file_spp_param[[x,"datapath"]], header=TRUE, sep = ","))[1,]))
                       == 25)) == 0|
          length(which(sapply(1:length(input$file_spp_param$datapath), function(x) length(suppressWarnings(read.table(input$file_spp_param[[x,"datapath"]], header=TRUE, sep = ","))[1,]))
                       == 1002)) == 0)){
        showModal(modalDialog(
          title = "Some data are missing",
          footer = modalButton("OK"),
          paste("Species data, flight heights, or survey data are not provided; default values will be used for any datasets that are not provided.")
        ))
      }}
  })

  # dialog box for when user chooses "Other" species option
  observeEvent(c(input$file_spp_param, input$species_input), {
    {if(length(input$file_spp_param$datapath) < 3&length(which(input$species_input=="Other")) == 1&!is.null(input$file_spp_param)){
      showModal(modalDialog(
        title = "Cannot run CRM",
        footer = modalButton("OK"),
        paste("Please provide species data, flight heights, and movement data when choosing the 'Use your own species' option. See documentation ('Start here') for more information.")
      ))
    }}
  })

  # create reactive objects that will be used in the promise for the main computational script
  interruptor <- AsyncInterruptor$new()
  CRM_fun <- reactiveVal()
  running <- reactiveVal(FALSE)

  # primary function that called computational script (using a promise)
  observeEvent(input$run, {
    if(running())
      return(NULL)
    running(TRUE)
    #if(TRUE){
    movement_type2 <- movement_type()
    speciesreact2 <- speciesreact()
    radioreact2 <- radioreact()
    tablereact2 <- tablereact()
    tablereact4 <- tablereact3()
    tablereact6 <- tablereact5()
    tablereact8 <- tablereact7()
    #tablereact10 <- tablereact9()
    tablereact12 <- tablereact11()
    sliderreact2 <- sliderreact()
    progress <- AsyncProgress$new(message="Simulating collision risk")
    CRM_fun(NULL)
    fut <- future({
      stochasticBand(
        results_folder = "results",
        BirdData = tablereact4,
        TurbineData = tablereact2,
        CountData = tablereact8,
        movement_type = movement_type2,
        FlightData = tablereact6,
        iter = sliderreact2,
        CRSpecies = speciesreact2,
        LargeArrayCorrection = "yes",
        Options_select = radioreact2,
        progress = progress,
        interruptor = interruptor,
        survey_data = tablereact12,
        runlocal = FALSE
      )
    })
    fut %...>% CRM_fun()
    fut <- catch(fut,
                 function(e){
                   CRM_fun(NULL)
                   print(e$message)
                   showNotification(e$message)
                 })
    fut <- finally(fut, function(){
      progress$close()
      running(FALSE) # done with run
      # output$study_design_report_txt <- renderPrint(CRM_fun())
      updateTabItems(session, inputId = "tabsetpan", selected = "crm_results")
    })
    #}
    NULL
  })

  # box appears if main function is canceled
  observeEvent(input$cancel,{
    if(running())
      interruptor$interrupt("Canceled")
  })

  # after main function is run, but before sensitivity analyses are, create a .csv that alerts the user (will be included in download package if user downloads raw results)
  observeEvent(input$run,
               if(!is.null(CRM_fun())){
                 write.csv("Sensitivity analyses not run", file = paste0(tempdir(), "/", CRM_fun()[['CRSpecies']][1], "_", paste0("turbModel", CRM_fun()[['Turbines']][1]),"_sensitivity.csv"), row.names = FALSE)
                 
               })

  # create a reactive object for the sensitivity analyses
  GSA_fun <- eventReactive(
    input$runGSA, {
      GSA_approx(CRM_fun(), input$optionradio)
    })

  # write results of sensitivity analysis to .csv (to be downloaded if user chooses)
  observeEvent(input$runGSA, {
    for(i in 1:length(CRM_fun()[['CRSpecies']])){
      for(e in 1:length(CRM_fun()[['Turbines']])){
        write.csv(GSA_fun()[[CRM_fun()[['CRSpecies']][i]]][[CRM_fun()[['Turbines']][e]]], file = paste0(tempdir(), "/", CRM_fun()[['CRSpecies']][i], "_", paste0("turbModel", CRM_fun()[['Turbines']][e]),"_sensitivity.csv"), row.names = FALSE)
      }
    }
  })

  
  # update estimated run time when user chooses a new option
  observeEvent(req(input$optionradio), {output$message <- renderText({
    if((times*as.numeric(input$slider1))[as.numeric(input$optionradio)] > 90 & (times*as.numeric(input$slider1))[as.numeric(input$optionradio)] <= 3600){
      time_estimate <- round((times*as.numeric(input$slider1))[as.numeric(input$optionradio)]/60)
      unit_time <- "minutes"
    }
    if((times*as.numeric(input$slider1))[as.numeric(input$optionradio)] > 3600){
      time_estimate <- round((times*as.numeric(input$slider1))[as.numeric(input$optionradio)]/60/60)
      unit_time <- "hour(s)"
    }
    if((times*as.numeric(input$slider1))[as.numeric(input$optionradio)] <= 90){
      time_estimate <- (times*as.numeric(input$slider1))[as.numeric(input$optionradio)]
      unit_time <- "seconds"
    }
    paste("~", time_estimate, unit_time, "per species, per turbine model")
  })
  })

  # print message alerting when main script is run successfully
  observeEvent(input$run, {output$hack <- renderText({
    if(!is.null(CRM_fun()$monthCollsnReps_opt1)){
      isolate(paste(option_labels[as.numeric(input$optionradio)], " ran successfully.", sep=""))
    }
  })
  })

  # if checks for species or turbine data are triggered, dialog box informs user of which parameters need to be updated
  observeEvent({req(!is.null(CRM_fun()$TurbineError)|!is.null(CRM_fun()$BirdDataError)|!is.null(CRM_fun()$FHDError)|!is.null(CRM_fun()$CountsError)|!is.null(CRM_fun()$MovementError))}, {
    showModal(modalDialog(
      title = "Cannot run CRM",
      footer = modalButton("OK"),
      paste("Failed data checks for appropriate values. Please provide appropriate values for ", paste(c(CRM_fun()$TurbineError, CRM_fun()$BirdDataError, CRM_fun()$FHDError, CRM_fun()$CountsError, CRM_fun()$MovementError), collapse=", "), ".", sep="")
    ))
  })

  # dialog box to ensure that the files provided for each species match the species listed in the provided count and species data files
  observeEvent({req(!is.null(CRM_fun()$SpeciesCheck))}, {
    showModal(modalDialog(
      title = "Cannot run CRM because species do not match",
      footer = modalButton("OK"),
      paste("Species specified in flight heights or count data do not match the rows of the species data.")
    ))
  })


  # if all conditions are met (conditions dependent on whether user is using baked-in species or uploading their own data) render the "run CRM" button to allow user to run main script
  # could simplify using bothdataup()
  observeEvent(c(input$file_wf_param, input$file_spp_param, input$species_input), {
    if((length(input$file_wf_param$datapath) > 0&length(input$file_spp_param$datapath) > 0&length(input$species_input) > 0&react_latlon()[3]==1)|
       (length(input$file_wf_param$datapath) > 0&length(input$species_input) > 0&length(which(input$species_input=="Other")) == 0&react_latlon()[3] == 1))
    # browser()
    # if((length(input$file_wf_param$datapath) > 0&length(input$file_spp_param$datapath) > 0&length(input$species_input) > 0 & nrow(react_latlon())>0)|
    #    (length(input$file_wf_param$datapath) > 0&length(input$species_input) > 0&length(which(input$species_input=="Other")) == 0 & nrow(react_latlon())>0))
      {
      output$runui <- renderUI({
        div(
          actionButton("run", "Run CRM", style = "margin-left: 20px; margin-top: 0px; background-color: green; color: white; font-weight: bold;"),
          id="uiinput"
        )
      })
    }
  })

  # if conditions above are no longer met, remove "run CRM" button
  observeEvent(req((length(input$file_spp_param$datapath) < 3&length(which(input$species_input == "Other")) == 1&!is.null(input$file_spp_param))|
                     # length(input$species_input) == 0|nrow(react_latlon())==0), {
                       length(input$species_input) == 0|react_latlon()[3] == 0), {
                         
                       removeUI(
                         selector = "#uiinput"
                       )
                     })

  # remove cancel button according to conditions for "run CRM"
  # observeEvent(req((length(input$file_spp_param$datapath) < 3&length(which(input$species_input=="Other")) == 1&!is.null(input$file_spp_param))|length(input$species_input) == 0|nrow(react_latlon())==0), {
  observeEvent(req((length(input$file_spp_param$datapath) < 3&length(which(input$species_input=="Other")) == 1&!is.null(input$file_spp_param))|length(input$species_input) == 0|react_latlon()[3] == 0), {
      
    removeUI(
      selector = "#uiinput2"
    )
  })

  # render cancel button according to conditions for "run CRM"
  observeEvent({req((bothdataup()&length(input$species_input) > 0&react_latlon()[3] == 1)|
                      (length(which(input$species_input == "Other")) == 0&length(input$species_input) > 0&length(input$file_wf_param$datapath) > 0&react_latlon()[3] == 1))}, {
                        output$cancel <- renderUI({
                          div(actionButton("cancel", "Cancel", style = "margin-left: 20px; margin-top: 0px; background-color: red; color: white; font-weight: bold;"),
                              id="uiinput2")
                        })
                      })
  
  # observeEvent({req((bothdataup()&length(input$species_input) > 0&nrow(react_latlon())>0)|
  #                     (length(which(input$species_input == "Other")) == 0&length(input$species_input) > 0&length(input$file_wf_param$datapath) > 0&nrow(react_latlon())>0))}, {
  #                       output$cancel <- renderUI({
  #                         div(actionButton("cancel", "Cancel", style = "margin-left: 20px; margin-top: 0px; background-color: red; color: white; font-weight: bold;"),
  #                             id="uiinput2")
  #                       })
  #                     })
  
  
  # when results are available, render button for running sensitivity analyses
  observeEvent({req(!is.null(CRM_fun()$monthCollsnReps_opt1))}, {
    output$runGSA2 <- renderUI({
      actionButton("runGSA", "Run Sensitivity", 
                   style = "margin-left: 0px; margin-top: 0px; background-color: navy; color: white; font-weight: bold;")
    })
  })
  
  # when results are available, render button for downloading raw model run data
  observeEvent({req(!is.null(CRM_fun()$monthCollsnReps_opt1))}, {
    output$download_output <- renderUI({
      downloadButton('downloadDataRaw', 'Download model runs', 
                     style = "margin-left: 0px; margin-top: 0px; background-color: darkkhaki; color: white; font-weight: bold;")
    })
  })


  # if results are available, render button for generating report
  observeEvent({req(!is.null(CRM_fun()$monthCollsnReps_opt1))}, {
    output$genreport <- renderUI({
      downloadButton("report", "Generate report", 
                     style = "margin-left: 0px; margin-top: 0px; background-color: darkviolet; color: white; font-weight: bold;")
    })
  })
  

  # download handler for report using R Markdown
  output$report <- downloadHandler(
    # for PDF output, change this to "report.pdf"
    filename = paste0("SCRAM_report_", strftime(Sys.time(), "%Y%m%d_%H%M%S"), ".pdf"),
    content = function(file) {
      # copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "report_BRI.Rmd")
      file.copy("report_BRI.Rmd", tempReport, overwrite = TRUE)
      # file_create(tempReport)

      # set up parameters to pass to Rmd document
      # browser()
      params <- list(iterations = input$slider1, model_output = CRM_fun(), threshold = input$inputthreshold, option = input$optionradio, species_labels = SpeciesLabels)
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      # Below works for HTML files, not pdf
      # rmarkdown::render(tempReport, output_file = file,
      #                   params = params,
      #                   envir = new.env(parent = globalenv())
      # )
      # can't render to PDF - error with latexpdf. Found this solution: 
      # https://stackoverflow.com/questions/66056764/knitr-cannot-find-pdflatex-when-creating-pdf-from-shiny-app
      # "ou should NOT specify the output_file argument in render() Instead you need to rename the file AFTER rendering."
      
      out <- rmarkdown::render(tempReport,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
      file.rename(out, file)
    }
    # contentType = "application/pdf"
  )
  

  # download handler for example for turbine data
  output$downloadTurbineExample <- downloadHandler(
    # filename = "TurbineData_example.zip",
    filename = "TurbineData_inputs_example.zip",
    content = function(file) {
      file.copy("data/TurbineData_inputs_example.zip", file)
    }
  )

  # download handler for example for turbine data
  output$downloadDataX <- downloadHandler(
    filename = "URI_CRM_documentation.pdf",
    content = function(file) {
      file.copy("URI_CRM_documentation.pdf", file)
    }
  )

  # download handler for example for species data
  output$downloadSpeciesExample <- downloadHandler(
    filename = "SpeciesData.zip",
    content = function(file) {
      file.copy("data/SpeciesData.zip", file)
    }
  )

  # download handler for raw results download
  output$downloadDataRaw <-
    downloadHandler(
      filename = paste0('SCRAM_CRM_model_output_', strftime(Sys.time(), "%Y%m%d_%H%M%S"),'.zip'),
      content = function(fname) {
        tmpdir = tempdir()
        fnames4zip1 <- list()
        fnames4zip2 <- list()
        fnames4zip3 <- list()
        for(e in 1:length(CRM_fun()[['CRSpecies']])){
          for(w in 1:length(CRM_fun()[['Turbines']])){
            sindex <- CRM_fun()[['CRSpecies']][e]
            tindex <- paste0("turbModel", CRM_fun()[['Turbines']][w])
            write.csv(CRM_fun()[[as.numeric(input$optionradio)]][[sindex]][[tindex]], file = paste0(tmpdir, "/", sindex, "_", tindex,".csv"), row.names = FALSE)
            write.csv(cbind(CRM_fun()[["sampledParamsTurbine"]][[sindex]][[tindex]],
                            CRM_fun()[["sampledParamsBird"]][[sindex]][[tindex]]), file = paste0(tmpdir, "/", sindex, "_", tindex,"_params.csv"), row.names = FALSE)
            #write.csv("Sensitivity analyses not run", file = paste0(tmpdir, "/", CRM_fun()[['CRSpecies']][e], "_", paste0("turbModel", CRM_fun()[['Turbines']][w]),"_sensitivity.csv"), row.names = FALSE)
            fnames4zip1 <- c(fnames4zip1, paste0(tmpdir, "/", sindex, "_", tindex,".csv", sep=""))
            fnames4zip2 <- c(fnames4zip2, paste0(tmpdir, "/", sindex, "_", tindex,"_params.csv", sep=""))
            fnames4zip3 <- c(fnames4zip3, paste0(tmpdir, "/", CRM_fun()[['CRSpecies']][e], "_", paste0("turbModel", CRM_fun()[['Turbines']][w]),"_sensitivity.csv"))
          }}
        fnames4zip4 <- list()
        params <- list(iterations = input$slider1, model_output = CRM_fun(), threshold = input$inputthreshold, option = input$optionradio, species_labels = SpeciesLabels)
        save(params, file = file.path(tmpdir, paste0('SCRAM_CRM_model_output_', strftime(Sys.time(), "%Y%m%d_%H%M%S"),'.RData'))) 
        fnames4zip4 <- c(fnames4zip4, file.path(tmpdir, paste0('SCRAM_CRM_model_output_', strftime(Sys.time(), "%Y%m%d_%H%M%S"),'.RData')))
        
        utils::zip(zipfile=fname, files=unlist(c(fnames4zip1, fnames4zip2, fnames4zip3, fnames4zip4)), flags = "-r9Xj")
        #if(file.exists(paste0(fname, ".zip"))) {file.rename(paste0(fname, ".zip"), fname)}
      },
      contentType = "application/zip"
    )
  # end server function
  
}

# Run the application
shinyApp(ui = ui, server = server)
