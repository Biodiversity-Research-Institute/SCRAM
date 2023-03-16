#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# A. Gilbert
# Biodiversity Research Institute
# 276 Canco Rd
# Portland, ME 04103
# This tool was modified from one largely developed by Chris Field at the University of Rhode Island and depends on code
# from the Stochastic Band model
# created: 16 Feb 2022
# Modified 06 April 2022 - pre-render movement data for plotting and tables for PIPL, REKN, ROST; prerender summary flt ht for same
#   Add back probability prediction for exceeding collision threshold
#   Create probability of occurrence polygons layers for PIPL, REKN, ROST from baked data files for mapping
# 27 April 22 - recreated output report using DPLYR and ggplot to simplify
# 05 May 22 - fixed issues with report and output tables to tabs in results figure 
# 10 May 22 - 0.74.3 - table reorganization in output
# 12 May 22 - Fixed from issues and rewrote baked movemement files with NAs in months according to when we had movememnt, per Pam Loring
# 19 May 22 - Added download folder for automatic saving of results in case of long model runs, but muted as it only works with local file systems which doesn't means
#   only local file service on remote machines.
# 25 May 22 - update baked movement models
# 13 Jun 22 - fully update movement models
# 15 Jun 22 - remove other species input for later release
# 21 Jul 22 - 0.80 - Upate movement and flight height models, bug fixes and improvements based on reviews
# 20 Sep 22	- 0.81 - update species data 
# 21 Sep 22 also change to report total monthly collision and the prediction interval for that estimate.
# 22 Sep 22 - change def of hub height add to correct to air gap
# 03 Oct 22 - 0.90 - fix movement model - some errors fixed by EMA, slight changes to report and manual. 
# 07 Oct 22 - 0.91 - fix bug where prob. of exceedenc not updating on subsequent runs and figures don't update. Also fixed issue with report names
#  not updating on re-runs, or zip files. 
# 11 Oct 22 - 0.91.1 - fix histogram results and slight change to report output
# 22 Dec 22 - 1.0 - Changed species occup. model maps to show mean monthly, not mean cum. daily (over all months), 
#   also added last position models to compare to first position (of day) models
# 24 Feb 23 - 1.0.1 - fixed reporting issue and made change to be much more like the Band 2012 migration model
# 01 Mar 23 - 1.0.2 - add coastal flag as dialog box and report warning.
# 09 Mar 23 - 1.0.3 - removed coastal flag and modified the number of animals in a model cell by  multiplying by the proportion of transient animals
# to reduce the effect of coastal staging animals on risk.

source("helpers.R")
# SCRAM_version = "0.91 - Brachycarpus"
# SCRAM_version = "0.91.1 - Lyrical Brachycarpus"   #https://en.wikipedia.org/wiki/List_of_Atlantic_decapod_species
# SCRAM_version = "1.0.1 - Allegorical Adela" #https://www.butterfliesandmoths.org/taxonomy
# SCRAM_version = "1.0.2 - Bucolic Adela" 
SCRAM_version = "1.0.3 - Cathartic Adela" 

options(shiny.trace = F)

##################################################################################################################################################
##                                                                                                                                              ##
##                                                              DASHBOARD HEADER                                                                ##
##                                                                                                                                              ##
##################################################################################################################################################
ui <- dashboardPage(
  skin = "green",
  dashboardHeader(
    titleWidth = 400,

    tags$li(
      class = "dropdown",
      actionLink("appvrsn", label = tags$b(paste("Stochastic Collision Risk Assessment for Movement: v", SCRAM_version), style = "font-size: 14px")),
      style = "float: left"
    ),
    tags$li(
      class = "dropdown",
      a(id = "download_manual",
        icon('fa-solid fa-book', "fa-2x"),
        style = "padding-top: 10px; padding-bottom: 10px",
        target = '_blank',
        href = "SCRAM_manual_v0911_101722.pdf"),
      style = "float: left"
    ),
    tags$li(
      class = "dropdown",
      a(
        icon('github', "fa-2x"),
        href = 'https://github.com/Biodiversity-Research-Institute/SCRAM',
        style = "padding-top: 10px; padding-bottom: 10px; padding-left: 5px; padding-right: 5px",
        target = '_blank',
        id = "lbl_codeLink"
      ),
      style = "float: left"
    ),
    
    tags$li(
      class = "dropdown",
      a(
        icon('bug', "fa-2x"),
        href = 'https://github.com/Biodiversity-Research-Institute/SCRAM/issues',
        #exclamation-circle
        style = "padding-top: 10px; padding-bottom: 10px; padding-left: 5px; padding-right: 5px",
        target = '_blank',
        id = "lbl_issuesLink"
      ),
      style = "float: left"
    ),

    # tags$li(class = "dropdown", actionLink("saveInputs_btt", label = NULL, icon("save", "fa-2x", lib = "font-awesome"),
    #                                        style = "padding-top: 10px; padding-bottom: 10px")),
    # tags$li(class = "dropdown", actionLink("restoreInputs_btt", label = NULL, icon("window-restore", "fa-2x", lib = "font-awesome"),
    #                                        style = "padding-top: 10px; padding-bottom: 10px")),
    tags$li(
      class = "dropdown",
      a(
        img(src = "BRI_color_logo_no_words.png", height = "40px"),
        href = 'https://briwildlife.org',
        style = "padding-top: 5px; padding-bottom: 5px; padding-left: 5px;, padding-right: 0px; margin-right: -5px",
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
        style = "padding-top: 5px; padding-bottom: 5px; padding-left: 5px;, padding-right: 0px; margin-right: -15px",
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
        style = "padding-top: 5px; padding-bottom: 5px; padding-left: 5px;, padding-right: 0px; margin-right: -5px",
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
        style = "padding-top: 5px; padding-bottom: 5px; padding-left: 5px;, padding-right: 0px; margin-right: -5px",
        target = '_blank',
        id = "lbl_BOEMLogoLink"
      ),
      style = "float: right"
    )
  ), #dashboardHeader
  
  ##################################################################################################################################################
  ##                                                                                                                                              ##
  ##                                                              DASHBOARD SIDEBAR                                                               ##
  ##                                                                                                                                              ##
  ##################################################################################################################################################
  
  dashboardSidebar(
    collapsed = F,
    width=415,
    # title= 
  
    sidebarMenu(
      id = "sidebar",
      style = "overflow-y:scroll; max-height: 800px; position:relative;",
      tags$a(
        img(src = "SCRAM_logo_400px.png", alt="Stochastic Collision Risk Assessment for Movement", width = "400px", class="header_img"),
        href = 'https://briwildlife.org/SCRAM',
        style = "margin-bottom: 10px;",# padding-right:20px; margin-bottom: -10px; margin-top:-45px;",
        target = '_blank',
        id = "lbl_SCRAMLogoLink"
      ),
      
      h4("1) SCRAM run details:", style = "padding-left: 10px; margin-bottom: 0px"),
      textInput(inputId = "project_name", label = "Project name: ", value = "", width = "380px", placeholder = "Project"),
      div(style = "margin-top: -20px;"),  #reduce space between elements
      textInput(inputId = "modeler", label = "Name of person running SCRAM: ", value = "", width = "380px", placeholder = "Name"),
      
      conditionalPanel( 
        #show only when project names entered
        condition = ('input.project_name != "" & input.modeler != ""'),
        h4("2) Select the species or load species data:", style = "padding-left: 10px; margin-bottom: 0px"),
  
        ################### Input: Select the model type and species to model
        # radioButtons(inputId = "model_input",
        #              # label ="Select included species data or your own:",
        #              label ="Select Motus daily position modeled:",
        #              choices = c("First position" = "first", "Last position" = "last"),
        #              selected = "last"),
        # radioButtons(inputId = "model_input",
        #              # label ="Select included species data or your own:",
        #              label ="Select Motus daily position modeled:",
        #              choices = c("Mean occup." = "mean", "Max occup." = "max"),
        #              selected = "max"),
        
        # div(style = "margin-top: -20px;"),  #reduce space between elements
        
        radioButtons(inputId = "species_input",
                     # label ="Select included species data or your own:",
                     label ="Select included species:",
                     # choices = c("Piping Plover" = "Piping_Plover", "Red Knot" = "Red_Knot", "Roseate Tern" = "Roseate_Tern", "Common Tern" = "Common_Tern","Use your own species data" = "Other"),
                     choices = c("Piping Plover" = "Piping_Plover", "Red Knot" = "Red_Knot", "Roseate Tern" = "Roseate_Tern"),
                     selected = character(0)) #start with no items selected
        #Deactivated ability to add Other species for now. Mute below until can be fixed for next version
        # conditionalPanel(  #only expand if you selected other to hide the load button
        #   condition = "input.species_input == 'Other'",
        #   # downloadButton("downloadSpeciesExample", "Download example species input",
        #   #                style = "margin-left: 20px; margin-top: 0px; background-color: blue; color: white; font-weight: bold;"),
        #   fileInput("file_spp_param", "Upload species data and flight height distributions", accept = ".csv", multiple = TRUE, width = '90%', options(shiny.maxRequestSize = 25 * 1024^2))
        #   ),

      ),
      #################Enter wind farm parameters
      conditionalPanel( 
        #show only when species data have been inputted
        condition = 'input.species_input',
        h4("3) Load wind farm parameters:", style = "padding-left: 10px; margin-bottom: 0px"),
        fileInput("file_wf_param", "Upload wind farm data", accept = c('text/csv', 
                                                             'text/comma-separated-values,text/plain', 
                                                             '.csv'), width = '95%'),
      
        # div(style = "margin-top: -20px;"),  #reduce space between elements
        # 
        # #spacing between turbines to estimate total crm space for model
        # numericInput(
        #   inputId = "turbine_spacing",
        #   label = "Average spacing between turbines (nmi)",
        #   value = 1,
        #   min = 0,
        #   max = NA,
        #   step = 0.1,
        #   width = '75%'),
        
      ),
      
      #################Enter CRM options
      conditionalPanel( 
        #show only when wind farm data have been inputted
        condition = "output.fileUploaded",
        h4("4) Select CRM parameter options:", style = "padding-left: 10px; margin-bottom: 0px"),
        radioButtons("crm_option_radio", "Band (2012) equivalent CRM options:",
                     c("Option 1: faster approximation" = "1", "Option 3: slower but more precise" = "3")),
        div(style = "margin-top: -20px;"),  #reduce space between elements
        sliderInput("iter_slider", label = "Model iterations (rec. min. 1,000)", min = 100, 
                    max = 10000, value = 100, step=100, width = '95%'), 
        htmlOutput("iter_message", style = "margin-top: -10px; margin-left: 10px"),
        numericInput(
          inputId = "inputthreshold",
          label = "Annual collision threshold",
          value = 1, #round(1/35, 3),
          min = 0,
          max = NA,
          step = NA,
          width = '75%'),
        # hr(),
        h4("5) Run CRM:", style = "padding-left: 10px;"),
        # Below code to add folder selection for output, but on Shinyapps.io only accesses local directory
        # div(p("Select an output folder for saving model results:"), style = "padding-left: 12px;"),
        # shinyDirButton('folder', 'Select folder', 'Select an output folder for saving model results', 
        #                style = "background-color: orange; color: white; font-weight: bold; text-align: center; padding-left: 12px; margin-bottom: 15px;"),
        # div(verbatimTextOutput('dirpath', placeholder = T), style = "padding-left: 12px; padding-right: 30px; margin-bottom: 20px; "),
        
        fluidRow(column(6, 
                        uiOutput("runUI")), 
                 column(6, 
                        uiOutput("cancelUI"))),
        br()
      )
  )
  ),
  
##################################################################################################################################################
##                                                                                                                                              ##
##                                                              DASHBOARD BODY                                                                  ##
##                                                                                                                                              ##
##################################################################################################################################################

  dashboardBody(
    # tags$head(
    #   tags$link(rel = "stylesheet",
    #             type = "text/css",
    #             href = "www/style.css")
    # ),
    
    useShinyjs(),
    
        # adds a indicator on slow load
    add_busy_spinner(spin = "atom", 
                     color = "blue",
                     position = "top-left", 
                     margins = c("50%", "50%"), 
                     onstart = T),
    
    tabsetPanel(
      id = "tabsetpan",
      type = "tabs",
      selected = "start",
      tabPanel(
        "Start Here",
        value = "start",
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
              # Mute download buttons - for species data for now, don't allow uploading of custom species data
              # Also move manual to book button
              column(width = 4, 
                     htmlOutput("downloads", style = "margin-top: -14px"),
                     #        downloadButton("downloadDataX", "Manual",
                     #                       style = "margin-bottom: 8px; background-color: gray; color: black; font-weight: bold;"),
                     #        br(),
                     #        downloadButton("downloadSpeciesExample", "Example species input",
                     #                       style = "margin-bottom: 8px; background-color: blue; color: white; font-weight: bold;"),
                     #        br(),
                     downloadButton("downloadTurbineExample", "Example wind farm input",
                                    style = "margin-bottom: 8px; background-color: orange; color: white; font-weight: bold;"),
                     br(),
                     img(src = "CVOW_turbines.jpg", width = 200)
              )
            )
          ), 
          fluidRow(
            p("This tool was developed by Biodiversity Research Institute, The University of Rhode Island, and 
                    U.S. Fish and Wildlife Service with funding from the Bureau of Ocean Energy Management.", 
              style = "margin-left: 20px; margin-right: 20px; margin-top: -10px; color: steelblue; font-size: 14px")
          )
        )
      ), #tabpanel
      
      tabPanel(
        "Species Data",
        value = "species_panel",
        
        fluidRow(
          #show the species data prior to modeling for checks
          column(6,
                 fluidRow(
                   box(
                     title = "Species Data",
                     status = "primary",
                     solidHeader = TRUE,
                     width = 12,
                     dataTableOutput("species_data"),
                                     style = "height:577px; overflow-y: scroll;overflow-x: scroll;")
                 )
          ),
          #Show the flight height data raw and as figure to help to make sure user check these before running
          column(6,
                 fluidRow(
                   box(title = "Flight Height Data",
                       status = "primary",
                       solidHeader = TRUE,
                       width = 12,
                       tabsetPanel(
                         id = "tabsetpan",
                         type = "tabs",
                         selected = "flt_ht_plot",
                         tabPanel(title = "Plot", 
                                  value = "flt_ht_plot",
                                  plotOutput("flt_ht_plot"), 
                                  style = "height:420px; overflow-y: scroll;overflow-x: scroll;"),
                         tabPanel(title = "Data", 
                                  value = "flt_ht_data",
                                  dataTableOutput("flt_ht_data"))
                                  #style = "overflow-y: scroll;")
                         ), #tabsetPanel
                       column(3, h5("Flight heights displayed (m)", 
                                    style="margin-top:20px; margin-left:5px; margin-right:5px; margin-bottom:5px")),
                       column(9, sliderInput("slider_flt_ht", label = "", min = 0, max = 1000, value=c(0,1000),step=25, width = '100%')))
                   )
                 ) #flight height column
          ), #species data fluidRow
        fluidRow(
          #show the species count data prior to modeling for checks
          column(12,
                 fluidRow(
                   box(
                     title = "Species Count Data",
                     status = "primary",
                     solidHeader = TRUE,
                     width = 12,
                     dataTableOutput("count_data"),
                     htmlOutput("popn_notes", style = "margin-top: 0px; margin-left:20px; margin-right:20px; margin-bottom:10px; font-size: 12px")
                   )))
        )

      ), #tabpanel Species Data
      
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
          column(7,
                 fluidRow(
                   box(
                     title = "Wind Farm Data",
                     status = "success",
                     solidHeader = TRUE,
                     width = 12,
                     tabsetPanel(
                       tabPanel("Turbine Specs", dataTableOutput("wind_farm_data1"),
                                style = "height:500px; overflow-y: scroll;overflow-x: scroll;"),
                       tabPanel("Turbine Ops Data", dataTableOutput("ops_data"),
                                style = "height:500px; overflow-y: scroll;overflow-x: scroll;"))
                   )
                 )
          ),
          column(5,
                 fluidRow(
                   box(
                     title = "Study Location and Species Densities",
                     status = "primary",
                     solidHeader = TRUE,
                     width = 12,
                     leafletOutput("studymap", height = "535px", width = "100%")
                   )
                 ), 
                 fluidRow(
                   p("\u00B9Mean occup. prob.= mean monthly occupancy probability", 
                     style = "margin-left: 20px; margin-right: 20px; margin-top: -10px; color: steelblue; font-size: 12px"))
                 )
        )
      ), #tabpanel wind farm data
      
      #CRM results tab
      tabPanel("CRM Results", value = "crm_results",
               fluidRow(
                 box(
                   title = "Results",
                   status = "primary",
                   solidHeader = F,
                   collapsible = F,
                   width = 12,
                   textOutput("run_start_txt"),
                   textOutput("run_end_txt"),
                   textOutput("run_success_msg"), 
                   htmlOutput("prob_exceed_threshold_msg")
                 )
               ),
               fluidRow(
                 column(7,
                        h4("Output dashboard", style = " margin-top: -10px;"), 
                        uiOutput("plot_tabs"),
                        uiOutput("plot_results_caption")

                 ), 
                 #buttons for sensitivity Analysis, downloading output, and generating report
                 column(5,
                        h4("Next steps:", style = " margin-top: -10px;"), 
                        uiOutput("runGSA2"), 
                        br(),
                        uiOutput("download_output"), 
                        br(),
                        uiOutput("genreport")
                 )
               )
      )# tabpanel CRM results
    ) # tabsetpanel
  ) #dashboard body
) #dashboard page

verbose <- F

##################################################################################################################################################
##                                                                                                                                              ##
##                                                              Define server logic                                                             ##
##                                                                                                                                              ##
##################################################################################################################################################

server <- function(input, output, session) {

  # input for the model options
  crm_option_radio_react <- eventReactive(input$crm_option_radio, {input$crm_option_radio})
  
  output$fileUploaded  <- reactive({
    val <- !(is.null(input$file_wf_param))
  })
  outputOptions(output, 'fileUploaded', suspendWhenHidden=FALSE)

  # number of iterations
  # iter_slider_react <- reactiveVal()
  iter_slider_react <- eventReactive(input$iter_slider, {as.numeric(input$iter_slider)})
  
  # approximate the run time for 1 species and 1 turbine; reactive with user input for number of iterations
  approx_run_times <- c(0.1, 1.2, 0.8, 5.9, 1.9, 3.1)
  
  # update estimated run time when user chooses a new option
  observeEvent(req(input$crm_option_radio), {output$iter_message <- renderText({
    if((approx_run_times*as.numeric(input$iter_slider))[as.numeric(input$crm_option_radio)] > 90 & (approx_run_times*as.numeric(input$iter_slider))[as.numeric(input$crm_option_radio)] <= 3600){
      time_estimate <- round((approx_run_times*as.numeric(input$iter_slider))[as.numeric(input$crm_option_radio)]/60)
      unit_time <- "minutes"
    }
    if((approx_run_times*as.numeric(input$iter_slider))[as.numeric(input$crm_option_radio)] > 3600){
      time_estimate <- round((approx_run_times*as.numeric(input$iter_slider))[as.numeric(input$crm_option_radio)]/3600)
      unit_time <- "hour(s)"
    }
    if((approx_run_times*as.numeric(input$iter_slider))[as.numeric(input$crm_option_radio)] <= 90){
      time_estimate <- (approx_run_times*as.numeric(input$iter_slider))[as.numeric(input$crm_option_radio)]
      unit_time <- "seconds"
    }
    paste("~", time_estimate, unit_time, "per wind farm/turbine option")
  })
  })
  
  # load labels to display versions of species names without underscores
  SpeciesLabels <- read.table("data/SpeciesLabels.csv", sep =",")
  
  # Render and remove buttons -----------------------------------------------
  
  # if all conditions are met (conditions dependent on whether user is using baked-in species or uploading their own data) 
  # could simplify using bothdataup()
  observeEvent(c(input$file_wf_param, input$species_input), {
    # render the "run CRM" button to allow user to run main script
    output$runUI <- renderUI({
      div(style="display:inline-block; float:right; padding-right: 10px",
          actionButton("run", "Run CRM", style = "width: 100px; background-color: green; color: white; font-weight: bold;"),
          id="run_div")
    })
    # render cancel button according to conditions for "run CRM"
    output$cancelUI <- renderUI({
      div(style="display:inline-block; float:left; padding-left: 10px",
          actionButton("cancel", "Cancel", style = "width: 100px; background-color: red; color: white; font-weight: bold;"),
          id="cancel_div")
    })
  })
  
  # box appears if main function is canceled
  observeEvent(input$cancel,{
    if(running())interruptor$interrupt("Canceled")
  })
  
  # if conditions above are no longer met, remove "run CRM" button
  observeEvent(req((length(input$file_spp_param$datapath) < 3 & length(which(input$species_input == "Other")) == 1 & !is.null(input$file_spp_param))|
                     length(input$species_input) == 0 | nrow(isolate(windfarm_loc$cell_sf))==0), { 
                       removeUI(
                         selector = "#run_div")
                     })
  
  # remove cancel button according to conditions for "run CRM"
  observeEvent(req((length(input$file_spp_param$datapath) < 3 & length(which(input$species_input=="Other")) == 1 &
                      !is.null(input$file_spp_param))|length(input$species_input) == 0 | nrow(isolate(windfarm_loc$cell_sf))==0), { 
                        removeUI(
                          selector = "#cancel_div")
                      })
  

# Start Here tab ----------------------------------------------------------

  output$user_instructions <- renderText(
  "<h4>INSTRUCTIONS:</h4>
   <p>1) Enter the project name and person conducting the analysis. This will be saved in output. <br>
    2) Select the species of interest included with SCRAM. <br>
    3) Check the species data for expected values in the tables and figures in the 'Species Data' tab. If data values are not as
    expected, do NOT run SCRAM. These values are currently fixed and can't be changed. Future updates should allow custom data. <br>
    4) Download the example wind farm inuput data using the button to the right and either modify for your specific use
    or use it directly to demonstrate the use of the tool. <br>
    5) Upon proper loading of the wind farm data, the wind farm data tab will be shown.
    Check the wind farm data for correct values in the include maps and tables. 
    Correct any errors and reload as necessary. <br>
    6) Choose which version of the CRM to run.<br>
    7) Select the number of iterations (100-10,0000).<br>
    8) Set a threshold for the maximum acceptable number of collisions. <br>
    9) Run CRM. <br>
    10) Run sensitivity analyses (optional). <br>
    11) Generate summary report and/or download results for each iteration. <br>
    12) Check the CRM results.</p>")
  
  output$check_windfarm_instructions <- renderText(
    "<h4>INSTRUCTIONS:</h4>
     Check the wind farm data carefully below before running this tool to make sure it's correct.<br>
     Fix any data in your original data file and upload again."
  )
  
  output$downloads <- renderText("<h4>DOWNLOAD INPUT FILE(S):</h4>")
  

# Species data (parameters, flight height, popn data) --------------------

  #navigate to species tab once selected
  observeEvent(input$species_input, 
               updateTabItems(session, inputId = "tabsetpan", selected = "species_panel")
  )
  
  # data for species characteristics
  species_params <- eventReactive(c(input$file_spp_param, input$species_input), {
    if(!is.null(input$file_spp_param)){
      if(length(which(
        sapply(1:length(input$file_spp_param$datapath), function(x) length(suppressWarnings(read.table(input$file_spp_param[[x,"datapath"]], header=TRUE, sep = ","))[1,]))
        ==10))>0){
        suppressWarnings(read.table(
          input$file_spp_param[[which(sapply(1:length(input$file_spp_param$datapath), function(y)
            length(suppressWarnings(read.table(input$file_spp_param[[y,"datapath"]], header=TRUE, sep = ","))[1,]))
            ==10)[1],"datapath"]], header=TRUE, sep = ","))
      }else{
        read.csv("data/BirdData_020923.csv", header = T)
      }
    }else{
      read.csv("data/BirdData_020923.csv", header = T)
    }
  })
  
  species_input_react <- eventReactive(c(input$species_input,input$file_spp_param$datapath), {
    if(length(which(input$species_input=="Other"))==0){
      input$species_input
    }else{
      suppressWarnings(read.table(input$file_spp_param[[which(
        sapply(1:length(input$file_spp_param$datapath), function(x) length(suppressWarnings(read.table(input$file_spp_param[[x,"datapath"]], header=TRUE, sep = ","))[1,]))
        == 10)[1],"datapath"]], header=TRUE, sep = ","))[,1]
    }
  })  

  #create a parameter for migrant vs. resident for crm calc switch
  migrate_resident_react <- eventReactive(species_input_react(), {
    if (species_input_react() %in% c("Piping_Plover","Red_Knot", "Roseate_Tern")){
      return("migrant")
    } else {  
      return("resident")
    }
  })
  
  #model type inputs
  # model_input_react <- eventReactive(input$model_input, {as.character(input$model_input)})
  model_input_react <- eventReactive(migrate_resident_react(), {
    if (migrate_resident_react()=="migrant"){
      return("trunc") #"last" - seems to produce worse state estimates, use first daily position (trunc)
    } else {  #resident
      return("mean")
    }
  })
  
  output$species_data <-
    DT::renderDataTable({
      if(!is.null(input$file_spp_param)){
        #load only first row of data
        bird_data <- read.csv(input$file_spp_param$datapath, header = T)[1,]
        species_data_row <- as.data.frame(t(bird_data[which(bird_data$Species==input$species_input), ]))
        colnames(species_data_row) <- sub("_", " ", species_data_row[1,])
      }else{
        bird_data <- read.csv("data/BirdData_020923.csv", header = T)
        # species_data_row <- reshape2::melt(as.data.table(bird_data[which(bird_data$Species==input$species_input), ]), id.var=NULL)
        species_params_defs <- c("Parameter definitions",
                                 "Mean Proportion of birds that avoid turbines (0-1)", 
                                 "Standard deviation of the avoidance rate",
                                 "Mean body length of the target species (m)",
                                 "Standard deviation of the species body length (m)",
                                 "Mean species wingspan length (m)",
                                 "Standard deviation of the species wingspan length (m)",
                                 "Mean species flight speed (m/s)",
                                 "Standard deviation of the species flight speed (m/s)",
                                 "Mean proportion of upwind flight for the species (0-1)",
                                 "Standard deviation of the proportion of upwind flight",
                                 # "Nocturnal flight activity",
                                 # "Standard deviation of the nocturnal flight activity",
                                 "Flight type, either flapping or gliding")
        species_data_row <- as.data.frame(cbind(species_params_defs ,t(bird_data[which(bird_data$Species==input$species_input), ])))
        colnames(species_data_row) <- sub("_", " ", species_data_row[1,])
      }
      return(species_data_row[-1, , drop = FALSE])
    },
    # species_params(),
    # selection = list(mode = "single", selected = 1),
    options = list(dom = 't', 
                   paging = FALSE,
                   bSort=FALSE,
                   scrollY = "500px",
                   searching = FALSE))

# Species flight height data ----------------------------------------------

  # flight height distributions
  spp_flt_ht_react <- eventReactive(c(input$file_spp_param, input$species_input), {
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
        lapply(1:length(species_input_react()), function(x) read.csv(paste("data/", species_input_react()[x],"_ht_dflt_v2.csv", sep=''), header = T))
      }
    }else{
      lapply(1:length(species_input_react()), function(x) read.csv(paste("data/", species_input_react()[x],"_ht_dflt_v2.csv", sep=''), header = T))
    }
  })
  
  #Summary table of flight height data for plotting and tables
  #  Created these for default/included data to speed up process, and otherwise summarize loaded flight height data
  flt_ht_summary_react <- eventReactive(c(input$file_spp_param, input$species_input), {
    if(!is.null(input$file_spp_param)){
      flt_ht_boot_table <- spp_flt_ht_react()[[1]]
      n_cols <- ncol(flt_ht_boot_table)
      #summarize across all boot samples
      flt_ht_summary <- flt_ht_boot_table %>%
        dplyr::group_by(Height_m) %>%
        dplyr::rowwise() %>%
        dplyr::summarise(mean_prop = mean(c_across(3:n_cols-1)), min_prop = min(c_across(3:n_cols-1)), max_prop = max(c_across(3:n_cols-1)))
    } else {
      #return  pre-rendered flt height summaries depending on species
      # load(file=paste0("data/", input$species_input, "_ht_dflt_summary.RData"))
      load(file=paste0("data/", input$species_input, "_ht_dflt_v2_summary.RData"))
    }
    return(flt_ht_summary)
  })

  #output table showing the min, max, mean flight heights from the bootstrap tables
  output$flt_ht_data <-
      DT::renderDataTable(
      datatable(
        flt_ht_summary_react() %>% 
          filter(Height_m >= input$slider_flt_ht[1] & Height_m <= input$slider_flt_ht[2]),
        selection = list(mode = "single", selected = 1),
        options = list(
          paging = FALSE,
          scrollY = "500px",
          searching = FALSE
        )))
  
  #flight height data plot
  output$flt_ht_plot <- renderPlot({
    ggplot(flt_ht_summary_react()) +
      geom_pointrange(aes(x = Height_m, y = mean_prop, ymin = min_prop, ymax = max_prop)) +
      geom_point(aes(x = Height_m, y = mean_prop), col = "red") +
      xlim(input$slider_flt_ht[1], input$slider_flt_ht[2]) +
      xlab("Flight height (m)") +
      ylab("Proportion") +
      coord_flip() +
      theme_bw()

  })


# Species population data -------------------------------------------

  # count data
  popn_data_react <- eventReactive(c(input$file_spp_param, input$species_input), {
    if(!is.null(input$file_spp_param)){
      if(length(which(
        sapply(1:length(input$file_spp_param$datapath), function(x) length(suppressWarnings(read.table(input$file_spp_param[[x,"datapath"]], header=TRUE, sep = ","))[1,]))
        ==25))>0){
        suppressWarnings(read.table(
          input$file_spp_param[[which(
            sapply(1:length(input$file_spp_param$datapath), function(y) length(suppressWarnings(read.table(input$file_spp_param[[y,"datapath"]], header=TRUE, sep = ","))[1,]))
            ==25)[1],"datapath"]], header=TRUE, sep = ","))
      }else{
        read.csv("data/CountData_USFWS_20220919.csv", header = T)
      }
    }else{
      read.csv("data/CountData_USFWS_20220919.csv", header = T)
    }
  })
  
  #output of the population data for species baked into SCRAM
  output$count_data <-
    DT::renderDataTable({
      count_tbl <- popn_data_react()[which(popn_data_react()$Species==isolate(input$species_input)), ] %>%
        dplyr::select(-Species)
      return(count_tbl)},
      options = list(dom = 't', 
                     paging = FALSE,
                     bSort=FALSE, 
                     scrollX = TRUE
                     ),
      rownames= FALSE
    )
  
  #Notes about assumptions and limitations for the three species for which population info is provided.
  spp_popn_notes <- reactive({
    Species <- input$species_input
    if (Species=="Piping_Plover"){
      return(c("Entire Atlantic coast population could be present in area during months listed.",
      "Occurrence through October to include birds stopping over in mid-Atlantic (e.g. North Carolina). Number of birds still present in Atlantic likely lower.",     
      "Estimate of HY fledges, uses the 20-year (2002 - 2021) average productivity (unweighted)."))
    }
    else if (Species=="Red_Knot"){
      return(c("All pass through in spring - #s consistent w/Lyons et al super-population estimate for 2020 in DE Bay: 40,444 (95 perc. credible interval: 33,627–49,966).",
    "Winter population estimates represent the total # of adults and sub-adults (in general); they do not include hatch-year (HY) birds in the fall.",
    "Southern and northern wintering birds could be present during July - Sept.",
    "Only northern wintering birds could be present during Oct - Nov.",
    "Only southeast US and Caribbean birds could be present during Dec.",
    "Birds from western Gulf population are excluded from totals in Atlantic region due to lack of information on extent to which they use the Atlantic region.",
    "Numbers do not include HY birds in fall.",
    "Dec number coming from Lyons et al 2017. Just includes SE US Birds, not Caribbean.",
    "Issues with double counting addressed because birds may be present in different areas of Atlantic region for weeks to months."))
    }
    else if (Species=="Roseate_Tern"){
      return(c("Entire NW Atlantic pop could be present in area during months listed.",
    "Average of most recent (2018 and 2019) productivity data from three largest colonies (representing >90 perc. of population) representative of entire population.",
    "Fledging and post-breeding dispersal period occurs from July through Sept.",
    "Numbers of non-breeding adults are not included.",
    "Does not include non-breeding 1 and 2 year old birds that return but do not breed.",
    "From Gochfeld and Burger (2020): Northeastern birds first arrive at Nantucket and Martha's Vineyard, MA, in large flocks, then disperse north as well as west. They arrive 26 Apr-20 May at Bird I., MA (Nisbet 1980, Nisbet 1981b, Nisbet 1989b), slightly later at Falkner I., CT, and Great Gull I., NY.",
    "From Gochfeld and Burger (2020): Apparently all birds migrate directly from the staging area around Cape Cod across the w. North Atlantic to the West Indies (Nisbet 1984, C. Mostello). Very small numbers occur at sea off N. Carolina from late Aug to late Sep, with a peak in early Sep; the latest date was 28 Oct (D. Lee)."))
    } else {return("")} 
    
  })
  
  #create count notes for rendering.
  output$popn_notes <- renderText(
    c("<h5>Population data assumptions/limitations:</h5>",
    paste0(1:length(spp_popn_notes()),") ", spp_popn_notes(), " <br>"))
  )
  

# Wind farm data ----------------------------------------------------------

  observeEvent(input$file_wf_param, {
    Sys.sleep(2)  #wait until species data has rendered and then switch also helps with pre-rendering maps, etc.
    updateTabItems(session, inputId = "tabsetpan", selected = "wind_farm_panel")})
  
  load(file = file.path(data_dir, "BOEM_halfdeg_grid_sf.RData"))
  
  wind_farm_df <- eventReactive(input$file_wf_param, {
    infile <- input$file_wf_param
    if (is.null(infile)) {
      return(NULL)
    }
    suppressWarnings(read.csv(infile$datapath, header=TRUE))
  })
  
  #reactive value to hold the selected grid cell for calculating
  windfarm_loc <- reactiveValues(
    cell_sf = NULL,
    Latitude= NULL)

  #perform lat/long checks prior to creating spatial data and create a spatial sf object holding the wind farm location and data
  observeEvent(wind_farm_df(), {
    
    if(length(which(colnames(isolate(wind_farm_df()))=="Latitude")) > 0 & length(which(colnames(isolate(wind_farm_df())) == "Longitude")) > 0){
      windfarm_lats <- isolate(wind_farm_df())[,c("Latitude", "Longitude")]
      #check to see if multiple lat/long value pairs for inputs - not allowed in this version
      if(nrow(windfarm_lats) >= 1){
        if(nrow(unique(windfarm_lats)) >= 2){
          showModal(modalDialog(
            title = "Check values for Latitude and Longitude",
            footer = modalButton("OK"),
            paste("Latitude and/or Longitude are not the same in the different wind farm option parameters.
              Only one location is allowed per model run.
              Please upload wind farm options for the same location.")
          ))
        } else {
          windfarm_loc$Latitude = isolate(wind_farm_df())[1,"Latitude"]
          loc_sf <- sf::st_as_sf(isolate(wind_farm_df())[1,], coords=c("Longitude", "Latitude" ), crs = sf::st_crs(4326))
          # CF'original cell match code, which is confusing at best, replaced with spatial functions
          # Gets cell values from lat/longs provided to provide info on low confidence areas as well as grid cell area in sq. km 
          #intersect with the model grid polygon
          windfarm_loc$cell_sf <- BOEM_halfdeg_grid_sf[unlist(sf::st_intersects(loc_sf, BOEM_halfdeg_grid_sf)), ]
          
          # #flag with the location include cells with coast
          # #provide dialog box warning
          # if (windfarm_loc$cell_sf$cells_with_coast){
          #   showModal(modalDialog(
          #     title = "Coastal occupancy model warning",
          #     footer = modalButton("OK"),
          #     coastal_disclaimer
          #   ))
          # }
          
          if(nrow(windfarm_loc$cell_sf)==0){
            showModal(modalDialog(
              title = "Check values for Latitude and Longitude",
              footer = modalButton("OK"),
              paste("Latitude and/or Longitude values fall outside the geographic extent of this tool.
              Please upload appropriate values with wind farm data or consult documentation for more information on geographic scope.")
            ))
          } 
        } 
      } 
    } else {  #missing lat or long fields
      showModal(modalDialog(
        title = "Check wind farm data for Latitude and Longitude",
        footer = modalButton("OK"),
        paste("Latitude and/or Longitude are missing.
              Please upload appropriate values with wind farm data.")
      ))
    }
  })

  #render the wind farm data to the tables on the wind farm data tab
  output$wind_farm_data1 <- DT::renderDataTable({
    wf_t <- wind_farm_df() %>%
      dplyr::select(-contains("Op"))
    wf_t <- as.data.frame(t(wf_t))
    colnames(wf_t) <- paste("Run", wf_t[1, ])
    #add parameter defs
    wf1_params_defs <- c("Parameter definitions",
                         "The number of turbines in the wind farm array", 
                         "The turbine model used in the analysis",
                         "Number of blades for the turbine model",
                         "Rotor radius (hub to blade tip; m)",
                         "Standard deviation of rotor radius (m)",
                         "Air gap (m)",
                         "Standard deviation of air gap (m)",
                         "Chord width of blade (m)",
                         "Standard deviation of blade width (m)",
                         "Mean wind speed (m/s) between cut-in and cut-out speeds or wind speed rating of the turbine model if wind speed is unavailable",
                         "Standard deviation of wind speed or wind speed rating (m/s)",
                         "Pitch angle of blades (degrees relative to rotor plane)",
                         "Standard deviation of pitch angle of blades",
                         "Wind farm width (km)",
                         "Latitude (decimal degrees)",
                         "Longitude (decimal degrees)")
    wf_t <- cbind("Parameter definitions"=wf1_params_defs, wf_t)
    return(wf_t[-1,, drop = FALSE])
  },
  options = list(
    dom = 't',
    paging = FALSE,
    bSort=FALSE
  ))
  # wind farm parameters
  wind_farm_params_react <- eventReactive(input$file_wf_param, {
    suppressWarnings(read.table(input$file_wf_param$datapath, header=TRUE, sep = ","))
  })
  
  # wtg_spacing_react <- eventReactive(input$turbine_spacing, {
  #   input$turbine_spacing
  # })
  
  #show the Wind Farm operational data as as table for QA/QC
  output$ops_data <-
    DT::renderDataTable({
      ops_data <- wind_farm_df() %>% 
        dplyr::select(Run, matches("Op", ignore.case=F)) %>% 
        t()
      colnames(ops_data) <- paste("Run", ops_data[1, ])
      #add parameter defs
      op_defs <- c("defs", (rep(c("Wind availability (maximum amount of time turbines can be operational/month).",
                                  "Mean time that turbines will not be operational (“down time”)", 
                                  "Standard deviation of mean operational time"),12)))
      ops_data <- cbind("Parameter definitions"=op_defs, ops_data)
      return(ops_data[-1, , drop = FALSE])},
      options = list(dom = 't', 
                     paging = FALSE,
                     bSort=FALSE)
    )
  
  bladeIcon <- makeIcon(
    iconUrl = "www/outline_wind_power_black_36dp.png",
    iconWidth = 36, iconHeight = 36,
    iconAnchorX = 0, iconAnchorY = 36,
  )
  

  #add appropriate species model output to map in leaflet when species chosen
  #create the color palette functions for coloring
  spp_move_data <- eventReactive(species_input_react(), {
    species <- isolate(species_input_react())
    grid_layer <-  paste0(species, "_monthly_prob_BOEM_half_deg_", isolate(model_input_react()))
    load(file.path(data_dir, paste0(species, "_monthly_prob_BOEM_half_deg_", isolate(model_input_react()),".RData")))
    assign(grid_layer, spp_monthly_prob_BOEM_half_deg)
  })
  
  meanpal <- eventReactive(c(input$species_input, model_input_react()), {
    #issue with zero inflated bins - need to generate non-zero quants
    #changed from using cumulative daily to mean monthly (non-culative daily) occupancy
    non_zero_mean <- spp_move_data()$mean_daily_allmonths[spp_move_data()$mean_daily_allmonths>0]
    mean_bins <- c(0, quantile(non_zero_mean, probs=seq(0,1,1/7)))
    colorBin("YlOrRd", spp_move_data()$mean_daily_allmonths, bins = mean_bins)
  })
  
  CIpal <- eventReactive(c(input$species_input, model_input_react()), {
    #issue with zero inflated bins - need to generate non-zero quants
    non_zero_CIrange <- spp_move_data()$mean_CI_range_daily_allmonths[spp_move_data()$mean_CI_range_daily_allmonths>0]
    CI_bins <- c(0, quantile(non_zero_CIrange, probs=seq(0,1,1/7)))
    colorBin("Purples", spp_move_data()$mean_CI_range_daily_allmonths, bins = CI_bins)
  })
  
  #render the map with the lat/longs given in the study area map panel
  output$studymap <- renderLeaflet({
    leaflet(options = leafletOptions(preferCanvas = T, tolerance = 1)) %>%
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
      # MOTUS antenna data
      addMarkers(
        data = isolate(wind_farm_df()),
        lat = ~ Latitude,
        lng = ~ Longitude,
        icon = bladeIcon,
        popup =
          paste0(
            "Run: ",
            wind_farm_df()$Run,
            "<br/>Number of turbines: ",
            wind_farm_df()$Num_Turbines,
            "<br/>Turbine model: ",
            wind_farm_df()$TurbineModel,
            "<br/>Rotor radius (m): ",
            wind_farm_df()$RotorRadius_m,
            "<br/>Air gap (m): ",
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
      addPolygons(data=spp_move_data(), weight = 1, fillOpacity = 0.5, opacity = 1,
                  color = ~meanpal()(mean_daily_allmonths),
                  highlightOptions = highlightOptions(color = "white", weight = 2), 
                  label = ~formatC(mean_daily_allmonths, digits = 2, format = "g"),
                  group="Occup. prob.") %>%
      #custom legend formatting labels
      addLegend(data=spp_move_data(), pal = meanpal(), values = ~mean_daily_allmonths, 
                title = "\u00B9Mean occup. prob.", 
                labFormat = function(type, cuts, p) {
                  n = length(cuts)
                  paste0(formatC(cuts[-n], digits = 2, format = "g"), " &ndash; ", formatC(cuts[-1], digits = 2, format = "g"))
                },
                position = "bottomright", group="Occup. prob.") %>% 
      # addLegend(data=spp_move_data(), pal = CIpal(), values = ~CI_range, title = "\u00B2CI range occup. prob.",position = "bottomright", group="CI range") %>% 
      setView(lat = mean(wind_farm_df()$Latitude), lng = mean(wind_farm_df()$Longitude), zoom = 6) %>%
      #Layers control
      addLayersControl(
        overlayGroups = c("Wind farm", "BOEM wind leases", "BOEM wind planning areas", "Occup. prob."), #, "CI range"),
        position = "topright", #"topleft",
        options = layersControlOptions(collapsed = TRUE)
      ) #%>% 
    #hideGroup("CI range")
  })
  
  
# Additional data checks prior to running SCRAM --------------------------------------
  
  # an object that indexes whether at least one file has been uploaded for both species and turbine data
  bothdataup <- reactiveVal()
  bothdataup  <- eventReactive(c(input$"file_spp_param", input$"file_wf_param"), {length(input$file_spp_param$datapath) > 0 & length(input$file_wf_param$datapath) > 0})

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
  

  # Main crm execution code using Promise functions -------------------------
  
  # load scripts for the main collision risk computation and the global sensitivity analyses
  source("stochCRM_move_function_BRI.R")
  source("scripts/GSA.R")
  
  # create reactive objects that will be used in the promise for the main computational script
  interruptor <- AsyncInterruptor$new()
  CRM_fun <- reactiveVal()
  running <- reactiveVal(FALSE)
  run_times <- reactiveValues()
  filenames <- reactiveValues()

  # primary function that called computational script (using a promise)
  observeEvent(input$run, {
    if(running())
      return(NULL)
    running(TRUE)
    run_times$start <- Sys.time()
    output$run_start_txt <- renderText(paste0("The model run was started at: ", strftime(run_times$start, "%Y-%m-%d %H:%M:%S %Z", tz = "America/New_York")))

    progress <- AsyncProgress$new(message="Simulating collision risk")

    # Must set seed=T in future function for correct parallel application
    # https://www.r-bloggers.com/2020/09/future-1-19-1-making-sure-proper-random-numbers-are-produced-in-parallel-processing/

    CRM_fun(NULL)
    fut <- future({
      stochasticBand(
        results_folder = "results",
        BirdData = isolate(species_params()),
        TurbineData = isolate(wind_farm_params_react()),
        # WTGSpacing = isolate(wtg_spacing_react()),
        CountData = isolate(popn_data_react()),
        # movement_type = isolate(windfarm_loc$cell_sf),
        FlightData = isolate(spp_flt_ht_react()),
        iter = isolate(iter_slider_react()),
        ModelCell = isolate(windfarm_loc$cell_sf),
        ModelType = isolate(model_input_react()),
        CRSpecies = isolate(species_input_react()),
        migr_res = isolate(migrate_resident_react()),
        Latitude = isolate(windfarm_loc$Latitude),
        LargeArrayCorrection = "yes",
        Options_select = isolate(crm_option_radio_react()),
        progress = progress,
        interruptor = interruptor,
        survey_data = 0,
        runlocal = FALSE
      )
    }, seed = T)
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
      run_times$end <- Sys.time()
      output$run_end_txt <- renderText(paste0("The model run was completed at: ", strftime(run_times$end, "%Y-%m-%d %H:%M:%S %Z", tz = "America/New_York")))
      # run_elaps_time <- run_end_time - run_start_time
      updateTabItems(session, inputId = "tabsetpan", selected = "crm_results")
    })
    NULL
    
    #MUTED below -in order to step through CRM_fun you need to mute future promises above and unmute below - you can't step through 
    #code with future-promises, it doesn't work.
    
    # CRM_fun() <- stochasticBand(
    #   results_folder = "results",
    #   BirdData = isolate(species_params()),
    #   TurbineData = isolate(wind_farm_params_react()),
    #   # WTGSpacing = isolate(wtg_spacing_react()),
    #   CountData = isolate(popn_data_react()),
    #   movement_type = isolate(windfarm_loc$cell_sf),
    #   FlightData = isolate(spp_flt_ht_react()),
    #   iter = isolate(iter_slider_react()),
    #   ModelType = isolate(model_input_react()),
    #   CRSpecies = isolate(species_input_react()),
    #   migr_res = isolate(migrate_resident_react()),
    #   Latitude = isolate(windfarm_loc$Latitude),
    #   LargeArrayCorrection = "yes",
    #   Options_select = isolate(crm_option_radio_react()),
    #   progress = progress,
    #   interruptor = interruptor,
    #   survey_data = 0, #movement_provided_react()
    #   runlocal = FALSE
    # )
    # NULL
  })  #end observeEvent input$run


# Render results output tab -----------------------------------------------

  # rendertext to report the probability of collisions exceeding a user-specified threshold
  # Divide the total number of collision results exceeding the threshold, divided by the total number of runs
  prob_exceed_threshold <- eventReactive(CRM_fun(), {
    num_species <- length(isolate(CRM_fun()[['CRSpecies']]))
    num_turb_mods <- length(isolate(CRM_fun()[['Turbines']]))
    prob_threshold_list <- c()
    n <- 1
    for(q in 1:num_species) {
      for(i in 1:num_turb_mods) {
        threshold_text <- ""
        if(!is.null(CRM_fun()[['monthCollsnReps']])){
          #add species and turbine input variable to carry the probabilities for each run through
          threshold_text <- length(which(rowSums(CRM_fun()[['monthCollsnReps']][[CRM_fun()[['CRSpecies']][q]]][[i]], na.rm=TRUE) > isolate(input$inputthreshold)))/
            length(rowSums(CRM_fun()[['monthCollsnReps']][[CRM_fun()[['CRSpecies']][q]]][[i]]))
          
          if(threshold_text == 1){
            threshold_text <- paste(">", isolate(round(1 - 1/input$iter_slider, log10(input$iter_slider))), sep=" ")
          }
          else if(threshold_text == 0){
            threshold_text <- paste("<", isolate(round(((1/input$iter_slider)), log10(input$iter_slider))), sep=" ")
          }
          else {
            #add some formatting to other probabilities
            threshold_text <- formatC(threshold_text, digits = 3, format = "fg")
          }
          prob_threshold_list[n] <- paste0("Run ", n,": the probability of exceeding specified threshold (", isolate(input$inputthreshold),") is ", threshold_text, ".")
          n <-  n + 1
        }
      }
    }
    return(prob_threshold_list)
  })
  
  #now render in correct format to output to Shiny with newlines as needed.
  output$prob_exceed_threshold_msg <- renderUI({
    HTML(paste(prob_exceed_threshold(), collapse = " <br> "))
  })
  
  # after main function is run, but before sensitivity analyses are, create a .csv that alerts the user (will be included in download package if user downloads raw results)
  observeEvent(input$run,
               if(!is.null(CRM_fun())){
                 write.csv("Sensitivity analyses not run", file = paste0(tempdir(), "/", CRM_fun()[['CRSpecies']][1], "_", paste0("turbModel", CRM_fun()[['Turbines']][1]),"_sensitivity.csv"), row.names = FALSE)
               })
  
  # main plot for annual cumulative collision estimation
  results_plots <- eventReactive(CRM_fun(), {
    num_species <- length(CRM_fun()[['CRSpecies']])
    num_turb_mods <- length(CRM_fun()[['Turbines']])
    plot_list = list()
    n = 1
    nbins = 20
    for(q in 1:num_species) {
      for(i in 1:num_turb_mods) {
        if(!is.null(CRM_fun()[['monthCollsnReps']])){
          # ATG - an issue with plots not rendering in dynamic tabs; use local to get output correct
          # https://stackoverflow.com/questions/31993704/storing-ggplot-objects-in-a-list-from-within-loop-in-r
          plot_list[[n]] <- local({
            NA_index <- which(is.na(CRM_fun()[['monthCollsnReps']][[CRM_fun()[['CRSpecies']][q]]][[i]][1,]))
            outvector <- round(rowSums(CRM_fun()[['monthCollsnReps']][[CRM_fun()[['CRSpecies']][q]]][[i]], na.rm = TRUE), digits = 3)
            xmin <- round(min(outvector, na.rm=TRUE))
            xmax <- round(max(outvector, na.rm=TRUE))
            hist_outvector <- hist(outvector, breaks = nbins, plot = F)
            freq_outvector <- hist_outvector$counts
            bar_outvector <- as.data.frame(cbind(mids=hist_outvector$mids, density=freq_outvector/sum(freq_outvector)))
            
            if (sum(outvector, na.rm=T)==0){
              #no collisions provide a modified figure
              plot_xmin = 0
              plot_xmax = 10
              plot_ymin = 0
              plot_ymax = 10
              x_ann = 5
              y_ann = 5
              fig_text = "No collisions predicted"
              
            } else {
              plot_xmin = -1
              if (xmax + 2 < 10){
                plot_xmax = 10
              } else {plot_xmax = xmax + 2}
              plot_ymin = 0
              plot_ymax = max(freq_outvector)/sum(freq_outvector)*1.1  #calculate frequency of first element to set as max y
              x_ann = 0
              y_ann = 0
              fig_text = ""
            }
            
            #main plot title
            if(length(which(SpeciesLabels[,q] == CRM_fun()[['CRSpecies']][q]))>0){
              main_label <- paste0("Model option ", CRM_fun()$Options_select, " for species ",SpeciesLabels[SpeciesLabels[,1] == CRM_fun()[['CRSpecies']][q], 2], " (turbine model ", CRM_fun()[['Turbines']][i], ")")
            }else{
              main_label <- paste0("Model option ", CRM_fun()$Options_select, " for species ",CRM_fun()[['CRSpecies']][q],  " (turbine model ", CRM_fun()[['Turbines']][i], ")")
            }
            
            bold <- rep(2, 12)
            month_col <- rep("dark blue", 12)
            month_col[NA_index] <- rgb(0, 0, 0, 0.8)
            bold[NA_index] <- 1 # make bold those months with data
            p1 <- ggplot2::ggplot(bar_outvector, aes(x=mids, y=density)) + 
              #center on integers use binwidth = 1 and center = 0; but modified to make bar end at number for thresholding
              # stat_bin(aes(y = stat(count / sum(count))), col="darkgreen", fill="darkgreen", binwidth = 0.5, center = 0.5) +
              #change to geom_col to have more control over bars with addition of limits to 0 and above to threshold
              geom_col(fill="darkgreen") +
              annotate("rect", xmin=0, xmax=input$inputthreshold, ymin=0, ymax=plot_ymax, fill=rgb(1, 1, 1, 0.65), col=rgb(0, 0, 0, 0)) +
              ggtitle(main_label) +
              scale_x_continuous(breaks=scales::pretty_breaks()) +
              # xlim(c(plot_xmin, plot_xmax)) +
              # ylim(c(plot_ymin, plot_ymax)) +
              xlab("Total collisions per year over months highlighted below") +
              ylab("Frequency") +
              annotate("text", x=x_ann, y=y_ann, label = fig_text) +
              geom_vline(xintercept = input$inputthreshold, color = "red", linetype = "dashed") +
              annotate("text", x=input$inputthreshold, y=plot_ymax*0.5, col="red", label = "Input threshold", angle=90, vjust=-0.75) +
              theme_classic()
            
            p2 <- cowplot::ggdraw(cowplot::add_sub(p1, label = month.abb, x=seq(0.1,0.9,0.8/11), color = month_col, size = 10, fontface = bold))
            # Add plot to list to render in each tab
            plot(p2)
          })
          # names(plot_list[[n]]) <- main_label
          n = n + 1
        }
      }
    }
    return(plot_list)
  })
  
  # ATG - causing flashing in plot - figure out why that is.
  # Started happening with rebuild of baked movemement was changed
  # This has something to do with plots telling user they are updating see:
  # https://github.com/rstudio/shiny/issues/1591
  # Adding  tags$style(type="text/css", ".recalculating {opacity: 1.0;}" removes this feature
  output$plot_tabs = renderUI({
    nTabs = length(results_plots())
    if (nTabs>0){
      histTabs = lapply(1:nTabs, function(x) {
        tabPanel(paste('Run', x),
                 renderPlot(results_plots()[[x]]), height = 400, 
                 tags$style(type="text/css", ".recalculating {opacity: 1.0;}"))
      })
      do.call(tabsetPanel, histTabs)
    }
  })
  
  output$plot_results_caption <-
    renderUI(p("Figure 1: A histogram of the frequency of the total number of collisions per year.
                            The heights of the bars show the relative frequency of each value.
                            Months for which movement data were provided or available are shown in bold;
                            only bold months are shown in histogram of annual collisions.",
               style = "margin-top: 10px; margin-left: 5px; margin-right: 10px; font-size: 12px; font-style: italic;"))
  

  # create a reactive object for the sensitivity analyses
  GSA_fun <- eventReactive(
    input$runGSA, {
      GSA_approx(CRM_fun(), input$crm_option_radio)
    })

  # write results of sensitivity analysis to .csv (to be downloaded if user chooses)
  observeEvent(input$runGSA, {
    for(i in 1:length(CRM_fun()[['CRSpecies']])){
      for(e in 1:length(CRM_fun()[['Turbines']])){
        write.csv(GSA_fun()[[CRM_fun()[['CRSpecies']][i]]][[CRM_fun()[['Turbines']][e]]], file = paste0(tempdir(), "/SCRAM_", CRM_fun()[['CRSpecies']][i], "_", paste0("turbModel", CRM_fun()[['Turbines']][e]),"_sensitivity.csv"), row.names = FALSE)
      }
    }
  })

  option_labels <- c("Option 1: faster approximation", NA,"Option 3: slower but more precise")
  
  # dialog box for sensitivity analyses
  observeEvent(input$runGSA, {
    {showModal(modalDialog(
      title = "Sensitivity analysis ran successfully",
      footer = modalButton("OK"),
      paste("Global sensivity analysis for ", option_labels[as.numeric(input$crm_option_radio)], " ran successfully. Results ready to download.", sep="")
    ))}
  })
  
  
  # print message alerting when main script is run successfully
  observeEvent(input$run, {output$run_success_msg <- renderText({
    if(!is.null(CRM_fun()$monthCollsnReps)){
      isolate(paste(option_labels[as.numeric(input$crm_option_radio)], " ran successfully.", sep=""))
    }
  })
  })
  
  # when results are available, render button for running sensitivity analyses
  observeEvent({req(!is.null(CRM_fun()$monthCollsnReps))}, {
    output$runGSA2 <- renderUI({
      actionButton("runGSA", HTML("Run sensitivity <br/> analysis"), 
                   style = "width: 150px; margin-left: 0px; margin-top: 0px; background-color: navy; color: white; font-weight: bold;")
    })
  })
  
  # when results are available, render button for downloading raw model run data
  observeEvent({req(!is.null(CRM_fun()$monthCollsnReps))}, {
    output$download_output <- renderUI({
      downloadButton("downloadDataRaw", HTML("Download model <br/> results"), 
                     style = "width: 150px; margin-left: 0px; margin-top: 0px; background-color: darkkhaki; color: white; font-weight: bold;")
    })
  })

  # if results are available, render button for generating report
  observeEvent({req(!is.null(CRM_fun()$monthCollsnReps))}, {
    output$genreport <- renderUI({
      downloadButton("report", HTML("Generate output <br/> report"), 
                     style = "width: 150px; margin-left: 0px; margin-top: 0px; background-color: darkviolet; color: white; font-weight: bold;")
    })
  })
  

# SCRAM model outputs -----------------------------------------------------

  # download handler for report using R Markdown
  output$report <- downloadHandler(
    # for PDF output, change this to "report.pdf"
    # must use function() of else the report name will not change on re-run
    filename = function() {paste0("SCRAM_report_", strftime(run_times$end, "%Y%m%d_%H%M%S"), ".pdf")},
    content = function(file) {
      # copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "report_BRI_v3.Rmd")
      img1 <- file.path(tempdir(), "SCRAM_logo_2_4inch.jpg")
      img2 <- file.path(tempdir(), "BRI_color_logo_no_words.png")
      img3 <- file.path(tempdir(), "URI.png")
      img4 <- file.path(tempdir(), "USFWS.png")
      img5 <- file.path(tempdir(), "BOEM.png")
      
      file.copy("scripts/report_BRI_v3.Rmd", tempReport, overwrite = TRUE)
      # need to copy images to temp dir otherwise can't be found 
      # see: (https://stackoverflow.com/questions/35800883/using-image-in-r-markdown-report-downloaded-from-shiny-app?rq=1)
      file.copy("www/SCRAM_logo_2_4inch.jpg", img1, overwrite = TRUE)
      file.copy("www/BRI_color_logo_no_words.png", img2, overwrite = TRUE)
      file.copy("www/URI.png", img3, overwrite = TRUE)
      file.copy("www/USFWS.png", img4, overwrite = TRUE)
      file.copy("www/BOEM.png", img5, overwrite = TRUE)

      # set up parameters to pass to Rmd document
      params <- list(SCRAM_version = SCRAM_version, project = input$project_name, modeler = input$modeler, 
                     run_start_time = isolate(run_times$start), run_end_time = isolate(run_times$end), 
                     iterations = input$iter_slider, model_cell = isolate(windfarm_loc$cell_sf), model_type = isolate(model_input_react()), 
                     model_output = isolate(CRM_fun()), spp_move_data_summary = isolate(spp_move_data()),
                     threshold = input$inputthreshold, prob_exceed = isolate(prob_exceed_threshold()), 
                     option = input$crm_option_radio, species_labels = SpeciesLabels, 
                     species_popn_data = popn_data_react()[which(popn_data_react()$Species==isolate(input$species_input)),], 
                     species_popn_assumptions = spp_popn_notes())
      
      # can't render to PDF - error with latexpdf. Found this solution: 
      # https://stackoverflow.com/questions/66056764/knitr-cannot-find-pdflatex-when-creating-pdf-from-shiny-app
      # "You should NOT specify the output_file argument in render() Instead you need to rename the file AFTER rendering."
      
      out <- rmarkdown::render(tempReport,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
      file.rename(out, file)
    }
    # contentType = "application/pdf"
  )
  
  # Below code needed once we implement other species data inputs
  # download handler for example for turbine data
  output$downloadTurbineExample <- downloadHandler(
    # filename = "TurbineData_example.zip",
    filename = "TurbineData_inputs_example.zip",
    content = function(file) {
      file.copy("data/TurbineData_inputs_example.zip", file)
    }
  )

  # download handler for raw results download
  output$downloadDataRaw <-
    downloadHandler(
      # must use function() of else the zip name will not change on re-run
      filename = function() {paste0('SCRAM_model_output_', strftime(run_times$end, "%Y%m%d_%H%M%S"),'.zip')},
      content = function(fname) {
        tmpdir = tempdir()
        fnames4zip1 <- list()
        model_type = isolate(model_input_react())
        for(e in 1:length(CRM_fun()[['CRSpecies']])){
          for(w in 1:length(CRM_fun()[['Turbines']])){
            sindex <- CRM_fun()[['CRSpecies']][e]
            tindex <- paste0("turbModel", CRM_fun()[['Turbines']][w])
            pred_monthly_coll <- CRM_fun()[["monthCollsnReps"]][[sindex]][[tindex]]
            colnames(pred_monthly_coll) <- paste0("crm_pred_", month.abb)
            pred_monthly_coll <- cbind(run=1:isolate(iter_slider_react()), pred_monthly_coll)
            write.csv(pred_monthly_coll, file = paste0(tmpdir, "/SCRAM_crm_pred_monthly_", sindex, "_", tindex,".csv"), row.names = FALSE)
            pred_daily_coll <- CRM_fun()[["dailyCollsnReps"]][[sindex]][[tindex]]
            colnames(pred_daily_coll) <- paste0("crm_pred_", month.abb)
            pred_daily_coll <- cbind(run=1:isolate(iter_slider_react()), pred_daily_coll)
            write.csv(pred_monthly_coll, file = paste0(tmpdir, "/SCRAM_crm_pred_daily_", sindex, "_", tindex,".csv"), row.names = FALSE)
            
            write.csv(popn_data_react()[which(popn_data_react()$Species==sindex),], file = paste0(tmpdir, "/SCRAM_", sindex, "_species_popn_data.csv"), row.names = FALSE)
            write.csv(cbind(CRM_fun()[["sampledParamsTurbine"]][[sindex]][[tindex]],
                            CRM_fun()[["sampledParamsBird"]][[sindex]][[tindex]]), file = paste0(tmpdir, "/SCRAM_", sindex, "_", tindex,"_params.csv"), row.names = FALSE)
            write.csv(CRM_fun()[["num_birds_cell_perday"]][[sindex]][[tindex]], file = paste0(tmpdir, "/SCRAM_", sindex, "_", tindex,"_num_birds_cell_perday.csv"), row.names = FALSE)
            write.csv(CRM_fun()[["num_birds_WF_perday"]][[sindex]][[tindex]], file = paste0(tmpdir, "/SCRAM_", sindex, "_", tindex,"_num_birds_WF_perday.csv"), row.names = FALSE)
            write.csv(CRM_fun()[["movement.boot.sample_list"]][[sindex]], file = paste0(tmpdir, "/SCRAM_", sindex,"_movement.boot.sample.csv"), row.names = FALSE)
            
            #check below - some of this may be overwritten with the multiple species/turbines types - may need to pull some of this out of the loop
            #add count data by grid as well below
            params <- list(SCRAM_version = SCRAM_version, project = input$project_name, modeler = input$modeler, 
                           run_start_time = isolate(run_times$start),  run_end_time = isolate(run_times$end), 
                           prob_exceed = isolate(prob_exceed_threshold()), iterations = isolate(input$iter_slider), 
                           model_type = model_input_react(), model_output = CRM_fun(), spp_move_data_summary = isolate(spp_move_data()),
                           threshold = input$inputthreshold, option = input$crm_option_radio, 
                           species_labels = SpeciesLabels, species_popn_data = popn_data_react()[which(popn_data_react()$Species==sindex),], 
                           species_popn_assumptions = spp_popn_notes())
            save(params, file = file.path(tmpdir, paste0('SCRAM_model_output_', strftime(isolate(run_times$end), "%Y%m%d_%H%M%S"),'.RData'))) 
            file.copy(input$file_wf_param$datapath, to = file.path(tmpdir, input$file_wf_param$name))
            fnames4zip1 <- c(fnames4zip1, file.path(tmpdir, input$file_wf_param$name))
            
            fnames4zip1 <- c(fnames4zip1, paste0(tmpdir, "/SCRAM_crm_pred_monthly_", sindex, "_", tindex,".csv"))
            fnames4zip1 <- c(fnames4zip1, paste0(tmpdir, "/SCRAM_crm_pred_daily_", sindex, "_", tindex,".csv"))
            fnames4zip1 <- c(fnames4zip1, paste0(tmpdir, "/SCRAM_", sindex, "_species_popn_data.csv"))
            fnames4zip1 <- c(fnames4zip1, paste0(tmpdir, "/SCRAM_", sindex, "_", tindex,"_params.csv"))
            fnames4zip1 <- c(fnames4zip1, paste0(tmpdir, "/SCRAM_", sindex, "_", tindex, "_num_birds_cell_perday.csv"))
            fnames4zip1 <- c(fnames4zip1, paste0(tmpdir, "/SCRAM_", sindex, "_", tindex, "_num_birds_WF_perday.csv"))
            fnames4zip1 <- c(fnames4zip1, paste0(tmpdir, "/SCRAM_", sindex, "_movement.boot.sample.csv"))
            fnames4zip1 <- c(fnames4zip1, paste0(tmpdir, "/SCRAM_", sindex, "_", tindex,"_sensitivity.csv"))
            fnames4zip1 <- c(fnames4zip1, paste0("data/", sindex, "_ht_dflt_v2.csv"))
            # if(isolate(input$model_input)=="first"){
            #   fnames4zip1 <- c(fnames4zip1, paste0("data/movements/",sindex, "_movements_trunc.zip"))
            # } else {
            #   fnames4zip1 <- c(fnames4zip1, paste0("data/movements/",sindex, "_movements_last.zip"))
            # }
            fnames4zip1 <- c(fnames4zip1, paste0("data/movements/",sindex, "_movements_", isolate(model_input_react()), ".zip"))
            fnames4zip1 <- c(fnames4zip1, file.path(tmpdir, paste0('SCRAM_model_output_', strftime(isolate(run_times$end), "%Y%m%d_%H%M%S"),'.RData')))
          }}
        # write.csv(CRM_fun()[["resultsSummary"]], file = paste0(tmpdir, "/SCRAM_results_summary.csv"), row.names = FALSE)
        # fnames4zip1 <- c(fnames4zip1, paste0(tmpdir, "/SCRAM_results_summary.csv"))
        
        utils::zip(zipfile=fname, files=unlist(c(fnames4zip1)), flags = "-r9Xj")
        #if(file.exists(paste0(fname, ".zip"))) {file.rename(paste0(fname, ".zip"), fname)}
      },
      contentType = "application/zip"
    )
  # end server function
  
}

# Run the application
shinyApp(ui = ui, server = server)


# EXTRA CODE --------------------------------------------------------------


# option for user-provided movement files disabled to simplify input
# tablereact9 <- reactiveValues()
# tablereact9 <- eventReactive(c(input$species_input), {
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
#      lapply(1:length(species_input_react()), function(x) read.csv(paste("data/MovementData_", species_input_react()[x],".csv", sep=''), header = T))
#    }
#  }else{
# lapply(1:length(species_input_react()), function(x) read.csv(paste("data/MovementData_", species_input_react()[x],".csv", sep=''), header = T))
#  }
# })



# indicator to let app know whether to use user-provided passage rate data (1) or built-in movement data (0)
# movement_provided_react <- reactiveValues()
# movement_provided_react <- eventReactive(c(input$file_spp_param, input$file_wf_param), {
#   if(!is.null(input$file_spp_param)){
#     if(length(which(
#       sapply(1:length(input$file_spp_param$datapath), function(x) length(suppressWarnings(read.table(input$file_spp_param[[x,"datapath"]], header=TRUE, sep = ","))[1,]))
#       ==25))>0){1}else{0}
#   }else{0}
# })
