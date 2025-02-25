ui <- dashboardPage(
  #### Header ####
  dashboardHeader(
    title = glue::glue(
      "{get_translation('title_bezos')} {country}"
    ),
    titleWidth = 650
  ),
  
  #### Sideboard ####
  dashboardSidebar(disable = TRUE),
  
  #### Body ####
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    tags$style(
      # Needed to overwrite dataTable CSS for paginators
      HTML(
        ".dataTables_wrapper .dataTables_paginate .paginate_button {
           box-sizing:border-box;
           display:inline-block;
           min-width:1.5em;
           padding:.5em 1em;
           margin-left:2px;
           text-align:center;
           text-decoration:none !important;
           cursor:pointer;
           *cursor:hand;
           color:#ffffff90 !important;
           border:1px solid transparent;
           border-radius:2px !important;
           background: #343A40 !important
           }"
      )
    ),
    tags$style(
      HTML(
        ".dataTables_wrapper .dataTables_paginate .paginate_button.disabled,
           .dataTables_wrapper .dataTables_paginate .paginate_button.disabled:hover,
           .dataTables_wrapper .dataTables_paginate .paginate_button.disabled:active {
           cursor:default;
           color:#ffffff90 !important;
           border: none !important;
           background: #343A40 !important;
           box-shadow:none
           }"
      )
    ),
    tags$style(
      HTML(
        ".dataTables_wrapper .dataTables_paginate .paginate_button.current,
           .dataTables_wrapper .dataTables_paginate .paginate_button.current:hover {
           color:#ffffff !important;
           border:1px solid #83E8F0 !important;
           background-color: #343A40 !important;
           }"
      )
    ),
    # Javascript to enable nice scrolling on weights table if it overflows
    tags$style(
      HTML(
        "#scrollable-content {
          max-height: 500px;
          overflow-y: auto;
          }"
      )
    ),
    tags$script(
      HTML(
        "$(document).on('shiny:connected', function() {
          $('#scrollable-content').on('wheel', function(e) {
          var delta = e.originalEvent.deltaY;
          this.scrollTop += delta;
          e.preventDefault();
          });
          });"
      )
    ),
    
    # Boxes need to be put in a row (or column)
    fluidRow(column(
      width = 3,
      box(
        title = get_translation("subtitle"),
        # The id lets us use input$tabset1 on the server to find the current tab
        id = "in",
        width = NULL,
        height = "100%",
        actionButton("mrun",
                     HTML(
                       "<h4>",
                       get_translation("button1"),
                       "</h4>"
                     )),
        helpText("================================="),
        helpText(
          HTML(
            "<h4><strong>",
            get_translation("global_par"),
            ":</strong></h4>"
          )
        ),
        checkboxInput(
          "multipri",
          get_translation("multipr"),
          FALSE
        ),
        selectInput(
          "protected",
          get_translation("protect_lock_per_bezos"),
          prot_lst
        ),
        # Update if other areas need to be locked in
        tags$hr(),
        helpText(HTML(
          get_translation("help_value_bezos")
        )),
        numericInput(
          "zone_1_target",
          get_translation("tar_prot"),
          protect_budget,
          min = 0,
          max = 100,
          step = 0.01
        ),
        numericInput(
          "zone_2_target",
          get_translation("tar_rest"),
          restore_budget,
          min = 0,
          max = 100,
          step = 0.01
        ),
        numericInput(
          "zone_3_target",
          get_translation("tar_man"),
          manage_budget,
          min = 0,
          max = 100,
          step = 0.01
        ),
        tags$hr(),
        helpText(HTML(
          get_translation("blm_help")
        )),
        numericInput(
          "blm",
          get_translation("blm"),
          0,
          min = 0,
          max = 1e10,
          step = 0.1
        )
      )
    ),
    column(
      width = 9,
      tabBox(
        title = "",
        # The id lets us use input$tabset1 on the server to find the current tab
        id = "out",
        width = NULL,
        height = "100%",
        tabPanel(
          get_translation("tab_wgt"),
          id = "rhweights",
          div(
            id = "scrollable-content",  # Apply scrollable ID to capture in JavaScript
            style = "max-height: 740px; padding: 10px;",
          helpText(get_translation("wgt_help")),
          rHandsontableOutput("hot_wgt")
          )
        ),
        tabPanel(
          get_translation("tab_input"),
          leafletOutput("InMap", height = 700)
        ),
        tabPanel(
          get_translation("tab_res"),
          helpText(HTML(
            "<h4>",
            get_translation("res_tab"),
            "</h4>"
          )),
          helpText(HTML(
            get_translation("res_tab_help")
          )),
          DT::dataTableOutput("summary"),
          helpText(HTML("<br>")),
          uiOutput("conditional_plot"),
          helpText(HTML("<br>")),
          helpText(HTML(
            "<h4>",
            get_translation("res_down"),
            "</h4>"
          )),
          downloadButton(
            "downloadSHP",
            label = get_translation("d_load")
          ),
          helpText(HTML("<br>")),
          helpText(HTML(
            "<h4>",
            get_translation("res_tab_down"),
            "</h4>"
          )),
          downloadButton(
            "download_ssoln_xlsx",
            label = glue(
              "{get_translation('d_load')} {get_translation('summary')} (Excel)"
            )
          ),
          downloadButton(
            "download_params_csv",
            label = glue(
              "{get_translation('d_load')} {get_translation('param')} (CSV)"
            )
          ),
          helpText(HTML("<br>"))
        ),
        tabPanel(
          get_translation("tab_map"),
          leafletOutput("cadMap", height = 700)
        )
      )
    ))
  )
)
