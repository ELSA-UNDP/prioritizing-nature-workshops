# ELSA Shiny App Server
server <- function(input, output, session) {
  # # Authentication module ####
  # auth <- callModule(
  #   module = auth_server,
  #   id = "auth",
  #   check_credentials = check_credentials(credentials)
  # )

  # Define the helper function at a global level, accessible throughout the server function
  reset_and_enforce_min <- function(input_id, temp_reset_value, min_value) {
    # Reset the input to temporary reset value to clear any previous state
    updateNumericInput(session, input_id, min = 0, value = temp_reset_value)
    # Set the actual min value after the reset
    updateNumericInput(session, input_id, min = min_value, value = max(input[[input_id]], min_value))
  }

  # Monitor the lock-in option and update the min budget values dynamically
  observeEvent(input$protected, {
    selected_value <- input$protected

    # Reset and enforce min values based on the selected protection
    reset_and_enforce_min("zone_1_target", temp_reset_value = -1, min_value = 0)
    reset_and_enforce_min("zone_2_target", temp_reset_value = -1, min_value = 0)
    reset_and_enforce_min("zone_3_target", temp_reset_value = -1, min_value = 0)

    # Dynamically set min values based on input$protected
    isolate({
      if (selected_value == prot_lst[1]) {
        reset_and_enforce_min("zone_1_target", temp_reset_value = 0, min_value = default_protect_min_budget)
        reset_and_enforce_min("zone_2_target", temp_reset_value = 0, min_value = default_restore_min_budget)
        reset_and_enforce_min("zone_3_target", temp_reset_value = 0, min_value = 0)
      } else if (selected_value == prot_lst[2]) {
        reset_and_enforce_min("zone_1_target", temp_reset_value = 0, min_value = paoecm_lockin_min_budget)
        reset_and_enforce_min("zone_2_target", temp_reset_value = 0, min_value = default_restore_min_budget)
        reset_and_enforce_min("zone_3_target", temp_reset_value = 0, min_value = 0)
      } else if (selected_value == prot_lst[3]) {
        reset_and_enforce_min("zone_1_target", temp_reset_value = 0, min_value = default_protect_min_budget)
        reset_and_enforce_min("zone_2_target", temp_reset_value = 0, min_value = restore_snap_lockin_min_budget)
        reset_and_enforce_min("zone_3_target", temp_reset_value = 0, min_value = 0)
      } else if (selected_value == prot_lst[4]) {
        reset_and_enforce_min("zone_1_target", temp_reset_value = 0, min_value = paoecm_lockin_min_budget)
        reset_and_enforce_min("zone_2_target", temp_reset_value = 0, min_value = restore_snap_lockin_min_budget)
        reset_and_enforce_min("zone_3_target", temp_reset_value = 0, min_value = 0)
      } else if (selected_value == prot_lst[5]) {
        reset_and_enforce_min("zone_1_target", temp_reset_value = 0, min_value = default_protect_min_budget)
        reset_and_enforce_min("zone_2_target", temp_reset_value = 0, min_value = default_restore_min_budget)
        reset_and_enforce_min("zone_3_target", temp_reset_value = 0, min_value = 0)
      } else {
        reset_and_enforce_min("zone_1_target", temp_reset_value = 0, min_value = default_protect_min_budget)
        reset_and_enforce_min("zone_2_target", temp_reset_value = 0, min_value = default_restore_min_budget)
        reset_and_enforce_min("zone_3_target", temp_reset_value = 0, min_value = 0)
      }
    })
  })

  # Monitor the individual target inputs and dynamically enforce minimum values based on input$protected
  observeEvent(input$zone_1_target, {
    selected_value <- input$protected
    if (selected_value == prot_lst[1] || selected_value == prot_lst[3] || selected_value == prot_lst[5]) {
      reset_and_enforce_min("zone_1_target", temp_reset_value = input$zone_1_target, min_value = default_protect_min_budget)
    } else if (selected_value == prot_lst[2] || selected_value == prot_lst[4]) {
      reset_and_enforce_min("zone_1_target", temp_reset_value = input$zone_1_target, min_value = paoecm_lockin_min_budget)
    }
  })

  observeEvent(input$zone_2_target, {
    selected_value <- input$protected
    if (selected_value == prot_lst[1] || selected_value == prot_lst[2] || selected_value == prot_lst[5]) {
      reset_and_enforce_min("zone_2_target", temp_reset_value = input$zone_2_target, min_value = default_restore_min_budget)
    } else if (selected_value == prot_lst[3] || selected_value == prot_lst[4]) {
      reset_and_enforce_min("zone_2_target", temp_reset_value = input$zone_2_target, min_value = restore_snap_lockin_min_budget)
    }
  })

  # No observeEvent() required for zone 3 budget

  output$res_auth <- renderPrint({
    reactiveValuesToList(auth)
  })

  session$onSessionEnded(function() {
    stopApp()
  })

  values <- reactiveValues(hot_wgt = wgts, hot_imp = impacts)

  calc <- reactive({
    # Load initial values ####
    df1 <- values[["hot_wgt"]]
    df3 <- values[["hot_imp"]]

    list(wgts = df1, impacts = df3)
  })

  #### Edit Weights ####
  # Reactive buffer to temporarily store input changes
  hot_wgt_buffer <- reactiveVal(NULL)
  
  # Observe changes to the rHandsontable input and update the buffer
  observeEvent(input$hot_wgt, {
    req(input$hot_wgt)  # Ensure the input exists before processing
    updated_df <- hot_to_r(input$hot_wgt)
    
    # Prevent adding extra rows (limit to the original size of wgts)
    if (nrow(updated_df) > nrow(wgts)) {
      updated_df <- updated_df[1:nrow(wgts), ]
    }
    
    # Only update the buffer if the data has changed
    if (!identical(updated_df, hot_wgt_buffer())) {
      hot_wgt_buffer(updated_df)
    }
  }, ignoreInit = TRUE)  # Ignore initial triggers when the app starts
  
  # Commit changes from the buffer to the main reactive value
  observeEvent(hot_wgt_buffer(), {
    req(hot_wgt_buffer())  # Ensure the buffer has data
    isolate({
      # Only update values if the buffer differs from current values
      if (!identical(hot_wgt_buffer(), values[["hot_wgt"]])) {
        values[["hot_wgt"]] <- hot_wgt_buffer()
      }
    })
  })
  
  # Render the rHandsontable
  output$hot_wgt <- renderRHandsontable({
    # Use the current reactive values or the default data
    DF <- isolate(values[["hot_wgt"]]) %||% wgts
    
    rhandsontable(
      DF[, c("name", "theme", "weight", "policy", "feature")],
      readOnly = TRUE,
      colHeaders = c(
        purrr::map_chr(c("data", "theme", "weight", "policy"), get_translation),
        "feature"
      )
    ) |> 
      hot_table(highlightCol = TRUE, highlightRow = TRUE) |> 
      hot_col(
        get_translation("weight"),
        readOnly = FALSE
      ) |> 
      hot_col(col = "feature", colWidths = 0.1) |> # Small width hides column
      hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
  })

  #### Gurobi reactive ####
  my.data <- shiny::reactive({
    # Don't do anything until after the first button push.
    input$mrun
    # Note that just by virtue of checking the value of input$recalcButton,
    # we're now going to get called whenever it is pushed.
    if (input$mrun == 0) {
      return(NULL)
    }

    return(isolate({
      weights.temp <- calc()$wgts
      # Adjust UI weights with pre-calibrated weights
      weights.temp$weight <- weights.temp$weight * wgta

      impacts.temp <- impacts

      progress <- Progress$new(session)

      progress$set(
        message = get_translation("setup"),
        detail = get_translation("be_patient"),
        value = 0.01
      )
      
      pu_temp <- pu_all[["area"]][[input$protected]]
      
      #### Create Problem ####
      prob.ta <-
        prioritizr::problem(pu_temp, zns, run_checks = FALSE) |>
        prioritizr::add_gurobi_solver(gap = 0.05, threads = 8)
      
      #### Available ####
      if (input$protected == "avail") {
        prob.ta <- define_problem_avail(
          input = input,
          prob.ta = prob.ta,
          pu_temp = pu_temp)
        }
      
      #### OECM Lock-in ####
      if (input$protected == "locked") {
        prob.ta <- define_problem_oecmLI(
          input = input,
          prob.ta = prob.ta,
          pu_temp = pu_temp,
          new_protect_zone = TRUE
          )
        }
      
      #### Restore Lock-in (interpreted as Degraded PA lock-in) ####
      if (input$protected == "restore") {
        prob.ta <- define_problem_restoreLI(
          input = input,
          prob.ta = prob.ta,
          pu_temp = pu_temp)
        }
      
      #### OECM and Restore Lock-in (interpreted as OECM and Degraded PA lock-in) ####
      if (input$protected == "pa_restore") {
        prob.ta <- define_problem_oecmRestoreLI(
          input = input,
          prob.ta = prob.ta,
          pu_temp = pu_temp,
          new_protect_zone = TRUE
          )
        }
      
      #### Boundary Penalty Factor ####
      if (input$blm > 0) {
        prob.ta <- prob.ta |>
          prioritizr::add_boundary_penalties(penalty = input$blm / 10000)
      }
      
      if (input$multipri) {
        progress$set(
          message = get_translation("calc"),
          detail = paste(
            get_translation("calc"),
            sprintf("1/%s", (1 + nrow(theme_tbl)))
          ),
          value = round(1 / (2 + nrow(theme_tbl)), 1)
        )
      } else {
        progress$set(
          message = get_translation("calc"),
          detail = get_translation("run"),
          value = 0.5
          )
        }
      
      #### Add weights ####
      prob.all <- prob.ta |>
        prioritizr::add_feature_weights(as.matrix(matrix(
          rep(weights.temp$weight, length(zns)),
          ncol = length(zns),
          nrow = terra::nlyr(feat_stack)
        )))
      
      #### Solve ####
      elsa_result <- solve.ConservationProblem(prob.all, force = TRUE)
      
      # Convert to categorical raster only one time
      elsa_raster <- make_elsa_categorical_raster(elsa_result)
      
      #### Get feature representation ####
      feature_rep <- calc_feature_representation(
        prob.all = prob.all,
        elsa_result = elsa_result,
        elsa_raster = elsa_raster,
        feat_df = feat_df
      )
      
      # Get results based on multiple themes (biodiversity, climate change mitigation, human well-being)
      elsa_result_multi <- feature_rep_multi <- list()
      
      ##### Solve Multi-theme ####
      if (input$multipri) {
        for (ii in 1:nrow(theme_tbl)) {
          progress$set(
            message = get_translation("calc"),
            detail = paste(
              get_translation("calc"),
              sprintf("%s/%s", 1 + ii, (1 + nrow(theme_tbl)))
            ),
            value = round((1 + ii) / (2 + nrow(theme_tbl)), 1)
          )
          
          wgt.tmp <- weights.temp
          
          wgt.tmp$weight[names(feat_stack) %nin% theme_tbl$names[[ii]]] <- 0
          
          prob.tmp <- prob.ta |>
            prioritizr::add_feature_weights(as.matrix(matrix(
              rep(wgt.tmp$weight, length(zns)),
              ncol = length(zns),
              nrow = terra::nlyr(feat_stack)
            )))
          
          #### ELSA Result ####
          elsa_result_multi[[ii]] <- solve.ConservationProblem(prob.tmp, force = TRUE)
          
          # Convert to categorical raster and add to existing ELSA raster - so makes a stacked raster
          elsa_raster <- c(elsa_raster,
                           make_elsa_categorical_raster(elsa_result_multi[[ii]]))
          
          # Combine the overall totals with the relative values
          feature_rep_multi[[ii]] <- calc_feature_representation(
            prob.all = prob.all,
            elsa_result = elsa_result_multi[[ii]],
            elsa_raster = elsa_raster[[ii + 1]],
            feat_df = feat_df
          )
          
          rm(wgt.tmp, prob.tmp)
        }
      }
      
      #### Calculate representation summaries ####
      progress$set(
        message = get_translation("post"),
        detail = get_translation("post_help"),
        value = 0.9
      )
      
      ##### Calculate ELSA Trade-off from Multi-theme prioritisations ####
      if (input$multipri) {
        feature_rep_tabl_multi <- purrr::reduce(seq_along(feature_rep_multi), function(x, i) {
          df <- feature_rep_multi[[i]][[2]][, c(1, 3)] # Extract column 1 (ID) and column 3 (relative_held_overall)
          colnames(df)[2] <- themes[[i]] # Rename extracted column using themes list
          dplyr::left_join(x, df, by = setNames(names(x)[1], names(df)[1])) # Join with the previous table
        }, .init = feature_rep[[2]][, c(1, 3)]) |> # Initialize with first feature table selecting col 1 and 3
          dplyr::rename(ELSA = 2) |> # Rename second column (relative_held_overall) to "ELSA"
          dplyr::rowwise() |>
          dplyr::mutate(elsa_tradeoff = round(ELSA / max(c_across(-c(1:2))) * 100, 1)) |> # Calculate tradeoff (as %s)
          dplyr::rename(
            "{get_translation('elsa_tradeoff')}" := elsa_tradeoff
          )
      }
      
      feature_rep_tabl <- feature_rep[[2]]
      
      rlist <- list(elsa_raster = elsa_raster, feature_rep_tabl = feature_rep_tabl)
      
      if (input$multipri) {
        rlist$feature_rep_tabl_multi <- feature_rep_tabl_multi
      }
      
      progress$set(value = 1)

      progress$close()

      return(rlist)
    }))
  })
      
  observe({
    my.data()
  })

  # Maps ####
  output$InMap <- renderLeaflet({
    #### Leaflet input datasets map ####
    progress <- Progress$new(session)

    progress$set(
      message = get_translation("gen_map"),
      detail = get_translation("be_patient"),
      value = 0.5
    )

    outl <- make_leaflet_input(
      layers = feat_stack,
      labels = feat_df$label
    )

    progress$set(value = 1)

    progress$close()

    outl
  })

  #### Input Datasets Maps ####
  output$cadMap <- renderLeaflet({
    if (input$mrun == 0) {
      progress <- Progress$new(session)

      progress$set(
        message = get_translation("gen_map"),
        detail = get_translation("be_patient"),
        value = 0.5
      )

      weights.temp <- calc()$wgts

      heatm_lst <- list()

      for (ii in 1:nrow(theme_tbl))
      {
        if (terra::nlyr(theme_tbl$layers[[ii]]) > 1) {
          heatm_lst[[ii]] <-
            terra::app(theme_tbl$layers[[ii]] * weights.temp$weight[weights.temp$feature %in% theme_tbl$names[[ii]]],
              sum,
              na.rm = TRUE
            )
        } else {
          heatm_lst[[ii]] <- theme_tbl$layers[[ii]] *
            weights.temp$weight[weights.temp$feature %in% theme_tbl$names[[ii]]]
        }

        heatm_lst[[ii]] <-
          heatm_lst[[ii]] / terra::global(heatm_lst[[ii]], max, na.rm = TRUE)$max * pu
      }

      elsa_hm <-
        terra::app(feat_stack * weights.temp$weight, sum, na.rm = TRUE)

      elsa_hm <-
        elsa_hm / terra::global(elsa_hm, max, na.rm = TRUE)$max * pu

      #### No Run Maps ####
      outl <- make_leaflet_elsa_0(
        elsa_hm = elsa_hm,
        heatm_lst = heatm_lst,
        theme_tbl = theme_tbl
      )

      progress$set(value = 1)
      progress$close()

      outl
    } else {
      progress <- Progress$new(session)
      progress$set(
        message = get_translation("gen_map"),
        detail = get_translation("be_patient"),
        value = 0.5
      )

      weights.temp <- calc()$wgts

      heatm_lst <- list()

      for (ii in 1:nrow(theme_tbl))
      {
        if (terra::nlyr(theme_tbl$layers[[ii]]) > 1) {
          heatm_lst[[ii]] <-
            terra::app(theme_tbl$layers[[ii]] * weights.temp$weight[weights.temp$feature %in% theme_tbl$names[[ii]]],
              sum,
              na.rm = TRUE
            )
        } else {
          heatm_lst[[ii]] <- theme_tbl$layers[[ii]] *
            weights.temp$weight[weights.temp$feature %in% theme_tbl$names[[ii]]]
        }

        heatm_lst[[ii]] <-
          heatm_lst[[ii]] / terra::global(heatm_lst[[ii]], max, na.rm = TRUE)$max * pu
      }

      elsa_hm <-
        terra::app(feat_stack * weights.temp$weight, sum, na.rm = TRUE)

      elsa_hm <-
        elsa_hm / terra::global(elsa_hm, max, na.rm = TRUE)$max * pu

      #### Prioritisation Maps ####
      outl <- make_leaflet_elsa_1(
        multi_theme = input$multipri,
        elsa_hm = elsa_hm,
        heatm_lst = heatm_lst,
        rast = my.data()$elsa_raster,
        theme_tbl = theme_tbl
      )

      progress$set(value = 1)
      progress$close()

      outl
    }
  })

  # Summary Table + Download Results raster ####
  output$summary <- DT::renderDataTable(
    if (!input$multipri) {
      my.data()$feature_rep_tabl
    }
    else {
      my.data()$feature_rep_tabl_multi
    }, 
    options = list(
      dom = "tipr",
      autoWidth = TRUE,
      pageLength = 9
    ))
  
  # Representation figure - uses the ELSA output only, not theme specific outputs.
  # output$elsa_representation_plot <- shiny::renderPlot(
  #   make_elsa_representation_plot(my.data()$feature_rep_tabl, input)
  # )
  output$conditional_plot <- renderUI({
    req(input$mrun > 0) # Only proceed if something has been run
    
    if (input$multipri) {
      # Display a message if multiple scenarios are selected
      tagList(
        h4(get_translation("rep_plot_h4_bezos")),
        p(get_translation("rep_plot_p_bezos"))
      )
    } else {
      # Render the plot if there's a single scenario
      shiny::plotOutput("elsa_representation_plot", width = "90%")
    }
  })

  # Define the actual plot rendering based on single scenario data
  output$elsa_representation_plot <- shiny::renderPlot({
    # Only proceed if feature_rep_tabl is not a list, i.e., single scenario
    if (!input$multipri) {
      make_elsa_representation_plot(my.data()$feature_rep_tabl, input)
    } else {
      get_translation("rep_plot_txt")
    }
  })

  output$downloadSHP <- downloadHandler(
    filename = function() {
      glue::glue("ELSA_layers_{Sys.Date()}.zip")
    },
    content = function(file) {
      owd <- setwd(tempdir())
      on.exit(setwd(owd))

      #### Delete geotiffs before prepping new tifs ####
      list.files(pattern = "*\\.(tif|xml)$") |>
        file.remove()

      files <- NULL

      progress <- Progress$new(session)
      progress$set(
        message = get_translation("prep_raster"),
        detail = get_translation("be_patient"),
        value = 0.5
      )

      weights.temp <- calc()$wgts
      
      #### Create/Write Rasters ####
      if (input$multipri) {
        elsa_raster <- my.data()$elsa_raster
        
        for (i in 1:nlyr(elsa_raster)) {
          # Set active category for each layer to label
          activeCat(elsa_raster[[i]]) <- 3
        }
        
        layer_names <- c("ELSA", theme_tbl$theme)
        
        elsa_raster |>
          terra::writeRaster(
            c(glue::glue("{layer_names[1]}_{Sys.Date()}.tif"),
              glue::glue("ELSA_{gsub(' ', '_', layer_names[-1])}_{Sys.Date()}.tif")),
            gdal = c(
              "COMPRESS=DEFLATE",
              "NUM_THREADS=ALL_CPUS",
              "OVERVIEWS=NONE"
            ),
            overwrite = TRUE,
            filetype = "COG",
            datatype = "INT1U",
            NAflag = 255
          )
      } else {
        elsa_raster <- my.data()$elsa_raster
        
        activeCat(elsa_raster) <- 3 # Set active category to the action label in the langauge used
        
        names(elsa_raster) <- glue("ELSA {get_translation('action')}")
        
        elsa_raster |>
          terra::writeRaster(
            glue::glue("ELSA_{Sys.Date()}.tif"),
            gdal = c(
              "COMPRESS=DEFLATE",
              "NUM_THREADS=ALL_CPUS",
              "TILED=YES",
              "OVERVIEWS=NONE"
            ),
            overwrite = TRUE,
            datatype = "INT1U",
            NAflag = 255
          )
      } 
      
      #### Heatmaps ####
      elsa_hm <-
        terra::app(feat_stack * weights.temp$weight, sum, na.rm = TRUE)

      elsa_hm <-
        terra::ifel(
          max(elsa_hm) > 0,
          elsa_hm / terra::global(elsa_hm, max, na.rm = TRUE)$max * pu,
          elsa_hm
        )

      names(elsa_hm) <- "ELSA heatmap"
      
      elsa_hm |>
        terra::classify(cbind(NA, -9999)) |>
        terra::writeRaster(
          glue::glue("ELSA_HM_{Sys.Date()}.tif"),
          gdal = c(
            "COMPRESS=DEFLATE",
            "PREDICTOR=3",
            "NUM_THREADS=ALL_CPUS",
            "OVERVIEWS=NONE"
          ),
          overwrite = TRUE,
          datatype = "FLT4S",
          filetype = "COG",
          NAflag = -9999
        )

      theme_hm <- list()

      for (ii in 1:nrow(theme_tbl))
      {
        if (terra::nlyr(theme_tbl$layers[[ii]]) > 1) {
          theme_hm[[ii]] <-
            terra::app(theme_tbl$layers[[ii]] * weights.temp$weight[weights.temp$feature %in% theme_tbl$names[[ii]]], sum, na.rm = TRUE)
        } else {
          theme_hm[[ii]] <-
            theme_tbl$layers[[ii]] * weights.temp$weight[weights.temp$feature %in% theme_tbl$names[[ii]]]
        }

        theme_hm[[ii]] <-
          terra::ifel(
            max(theme_hm[[ii]]) > 0,
            theme_hm[[ii]] / terra::global(theme_hm[[ii]], max, na.rm = TRUE)$max * pu,
            theme_hm[[ii]]
          )

        names(theme_hm[[ii]]) <-
          glue::glue("{theme_tbl$theme[ii]} heatmap")
        
        theme_hm[[ii]] |>
          terra::classify(cbind(NA, -9999)) |>
          terra::writeRaster(
            glue::glue("{theme_tbl$theme[ii]}_HM_{Sys.Date()}.tif") |> gsub("/", "-", .) |> gsub(" ", "_", .),
            gdal = c(
              "COMPRESS=DEFLATE",
              "PREDICTOR=3",
              "NUM_THREADS=ALL_CPUS",
              "OVERVIEWS=NONE"
            ),
            overwrite = TRUE,
            datatype = "FLT4S",
            filetype = "COG",
            NAflag = -9999
          )
      }
      
      files <- list.files(pattern = "*\\.(tif|xml)$")

      files <- files[!grepl("spat", files)]

      progress$set(value = 1)

      progress$close()

      # Create the zip file ####
      zip::zip(file, files)
    }
  )

  output$download_ssoln_xlsx <- downloadHandler(
    filename = function() {
      glue::glue("ELSA_summary_results_{Sys.Date()}.xlsx")
    },
    content = function(file)
    {
      if (input$multipri) {
        writexl::write_xlsx(my.data()$feature_rep_tabl_multi, file)
      } else {
        writexl::write_xlsx(my.data()$feature_rep_tabl, file)
      }
    }
  )

  output$download_params_csv <- downloadHandler(
    filename = function() {
      glue::glue("ELSA_model_parameters_{Sys.Date()}.csv")
    },
    content = function(file) {
      tidyr::pivot_longer(
        data = tibble(
          `Multi-theme prioritisation` = input$multipri,
          `Protected areas lock-in` = input$protected,
          `Protect budget` = input$zone_1_target,
          `Restore budget` = input$zone_2_target,
          `Manage budget` = input$zone_3_target,
          `Protect/Restore Zone` = "No budget allocation",
          `Boundary Penalty Factor` = input$blm,
        ),
        everything(),
        names_to = "Parameter",
        values_to = "Value",
        values_transform = list(Value = as.character)
      ) |> 
        readr::write_csv(file, col_names = TRUE)
    }
  )
}
