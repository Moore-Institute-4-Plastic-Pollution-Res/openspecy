function(input, output, session) {
  #Setup ----
  options(shiny.maxRequestSize = 10000 * 1024 ^ 2)
  
  #URL Query
  # observeEvent(session$clientData$url_search, {
  #     query <- parseQueryString(session$clientData$url_search)
  #
  #     for (i in 1:(length(reactiveValuesToList(input)))) {
  #         nameval = names(reactiveValuesToList(input)[i])
  #         valuetoupdate = query[[nameval]]
  #
  #         if (!is.null(query[[nameval]])) {
  #             if (is.na(as.numeric(valuetoupdate))) {
  #                 updateTextInput(session, nameval, value = valuetoupdate)
  #             }
  #             else {
  #                 updateTextInput(session, nameval, value = as.numeric(valuetoupdate))
  #             }
  #         }
  #
  #     }
  #
  # })
  
  #create a random session id
  session_id <- digest(runif(10))
  
  # Loading overlay
  load_data()
  hide(id = "loading_overlay",
       anim = TRUE,
       animType = "fade")
  show("app_content")
  
  preprocessed <- reactiveValues(data = NULL)
  data_click <- reactiveValues(data = NULL)
  
  
  #Read Data ----
  #Sending data to a remote repo.
  observeEvent(input$file, {
    # Read in data when uploaded based on the file type
    req(input$file)
    data_click$data <- 1
    preprocessed$data <- NULL
    
    if (!all(
      grepl(
        "(\\.tsv$)|(\\.dat$)|(\\.hdr$)|(\\.json$)|(\\.rds$)|(\\.yml$)|(\\.csv$)|(\\.asp$)|(\\.spa$)|(\\.spc$)|(\\.jdx$)|(\\.dx$)|(\\.RData$)|(\\.zip$)|(\\.[0-9]$)",
        ignore.case = T,
        as.character(input$file$datapath)
      )
    )) {
      show_alert(
        title = "Data type not supported!",
        text = paste0(
          "Uploaded data type is not currently supported; please
                      check tooltips and 'About' tab for details."
        ),
        type = "warning"
      )
      return(NULL)
    }
    
    withProgress(message = "Reading data", value = 2 / 3, {
      rout <- tryCatch(
        expr = {
          read_any(file = as.character(input$file$datapath)) |>
            c_spec(range = "common", res = if (input$conform_decision) {
              input$conform_res
            } else{
              8
            }) |>
            manage_na(ig = c(NA, 0), type = "remove")
        },
        error = function(e) {
          class(e$message) <- "simpleWarning"
          e$message
        }#,
        #warning = function(w){
        #class(w$message) <- "simpleWarning"
        #    w$message
        #}
      )
      #print(rout)
      
      if (!inherits(rout, "simpleWarning") &&
          all(!grepl("(\\.hdr$)|(\\.dat$)|(\\.zip$)", input$file$datapath))) {
        rout$metadata$file_name <- input$file$name
      }
      
      if (!inherits(rout, "simpleWarning")) {
        checkit <- tryCatch(
          expr = {
            check_OpenSpecy(rout)
          },
          error = function(e) {
            class(e$message) <- "simpleWarning"
            e$message
          },
          warning = function(w) {
            class(w$message) <- "simpleWarning"
            w$message
          }
        )
      }
      else{
        checkit <- NA
      }
      #print(checkit)
      if (inherits(rout, "simpleWarning") |
          inherits(checkit, "simpleWarning")) {
        show_alert(
          title = "Something went wrong with reading the data :-(",
          text =  paste0(
            if (inherits(rout, "simpleWarning")) {
              paste0("There was an error during data loading that said ",
                     rout,
                     ".")
            } else{
              ""
            },
            if (inherits(checkit, "simpleWarning")) {
              paste0(" There was an error during data checking that said ",
                     checkit,
                     ".")
            } else{
              ""
            },
            ". If you uploaded a text/csv file, make sure that the columns are numeric and named 'wavenumber' and 'intensity'."
          ),
          type =  "error"
        )
        reset("file")
        preprocessed$data <- NULL
      }
      
      else {
        preprocessed$data <- rout
        #print(preprocessed$data)
      }
    })
  })
  
  #The matching library to use.
  libraryR <- reactive({
    req(input$active_identification)
    if (input$id_strategy == "deriv" & input$lib_type == "medoid") {
      if (file.exists("data/medoid_derivative.rds")) {
        library <- read_any("data/medoid_derivative.rds")
      }
      else{
        get_lib("medoid_derivative", mode = "w", path = "data/", aws = TRUE)
        library <- read_any("data/medoid_derivative.rds")
      }
      #return(library)
    }
    else if (input$id_strategy == "nobaseline" &
             input$lib_type == "medoid") {
      if (file.exists("data/medoid_nobaseline.rds")) {
        library <- read_any("data/medoid_nobaseline.rds")
      }
      else{
        get_lib("medoid_nobaseline", mode = "w", path = "data/", aws = TRUE)
        library <- read_any("data/medoid_nobaseline.rds")
      }
    }
    else if (input$id_strategy == "deriv" &
             input$lib_type == "model") {
      if (file.exists("data/model_derivative.rds")) {
        library <- read_any("data/model_derivative.rds")
      }
      else{
        get_lib("model_derivative", mode = "w", path = "data/", aws = TRUE)
        library <- read_any("data/model_derivative.rds")
      }
      return(library)
    }
    else if (input$id_strategy == "nobaseline" &
             input$lib_type == "model") {
      if (file.exists("data/model_nobaseline.rds")) {
        library <- read_any("data/model_nobaseline.rds")
      }
      else{
        get_lib("model_nobaseline", mode = "w", path = "data/", aws = TRUE)
        library <- read_any("data/model_nobaseline.rds")
      }
      return(library)
    }
    else if (grepl("nobaseline$", input$id_strategy)) {
      if (file.exists("data/nobaseline.rds")) {
        library <- read_any("data/nobaseline.rds")
      }
      else{
        get_lib("nobaseline", mode = "w", path = "data/", aws = TRUE)
        library <- read_any("data/nobaseline.rds")
      }
    }
    else if (grepl("deriv$", input$id_strategy)) {
      if (file.exists("data/derivative.rds")) {
        library <- read_any("data/derivative.rds")
      }
      else{
        get_lib("derivative",mode = "w", path = "data/", aws = TRUE)
        library <- read_any("data/derivative.rds")
      }
    }
    if (grepl("^both", input$id_spec_type)) {
      library
    }
    else if (grepl("^ftir", input$id_spec_type)) {
      filter_spec(library, logic = library$metadata$spectrum_type == "ftir")
    }
    else if (grepl("^raman", input$id_spec_type)) {
      filter_spec(library, logic = library$metadata$spectrum_type == "raman")
    }
  })
  
  # Corrects spectral intensity units using the user specified correction
  
  # Redirecting preprocessed data to be a reactive variable. Not totally sure why this is happening in addition to the other.
  data <- reactive({
    req(input$file)
    preprocessed$data
  })
  
  #Preprocess ----
  
  # All cleaning of the data happens here. Range selection, Smoothing, and Baseline removing
  baseline_data <- reactive({
    req(!is.null(preprocessed$data))
    req(input$active_preprocessing)
    processed = process_spec(
      x = data(),
      active = input$active_preprocessing,
      adj_intens = input$intensity_decision,
      adj_intens_args = list(type = input$intensity_corr),
      conform_spec = input$conform_decision,
      conform_spec_args = list(
        range = NULL,
        res = input$conform_res,
        type = input$conform_selection
      ),
      restrict_range = input$range_decision,
      restrict_range_args = list(min = input$MinRange, max = input$MaxRange),
      flatten_range = input$co2_decision,
      flatten_range_args = list(min = input$MinFlat, max = input$MaxFlat),
      subtr_baseline = input$baseline_decision,
      subtr_baseline_args = list(
        type = "polynomial",
        degree = input$baseline,
        raw = FALSE,
        refit_at_end = input$refit,
        iterations = input$iterations,
        baseline = NULL
      ),
      smooth_intens = input$smooth_decision,
      smooth_intens_args = list(
        polynomial = input$smoother,
        window = calc_window_points(if (input$conform_decision) {
          seq(100, 4000, by = input$conform_res)
        }
        else{
          data()
        }, input$smoother_window),
        derivative = input$derivative_order,
        abs = input$derivative_abs
      ),
      make_rel = input$make_rel_decision
    )
    
    if (input$spatial_decision) {
      processed = spatial_smooth(processed,
                                 sigma = c(input$sigma, input$sigma, input$sigma))
    }
    processed
  })
  
  
  # Choose which spectra to use for matching and plotting.
  DataR <- reactive({
    req(!is.null(preprocessed$data))
    if (input$active_preprocessing) {
      baseline_data()
    }
    else {
      data()
    }
  })
  
  #The data to use in the plot.
  # change 1----
  DataR_plot <- reactive({
    if (isTruthy(DataR())) {
      filter_spec(DataR(), logic = 1:ncol(DataR()$spectra) == data_click$data)
    }
    else {
      list(wavenumber = numeric(),
           spectra = data.table(empty = numeric()))
    }
  })
  
  # SNR ----
  #The signal to noise ratio
  signal_to_noise <- reactive({
    req(!is.null(preprocessed$data))
    sig_noise(
      x = DataR(),
      step = 10,
      metric = input$signal_selection,
      abs = F
    )
  })
  
  
  MinSNR <- reactive({
    req(!is.null(preprocessed$data))
    if (!input$threshold_decision) {
      -Inf
    }
    else{
      input$MinSNR
    }
  })
  
  # change 2 ----
  particles_logi <- reactive({
    if (input$collapse_log_type == "Thresholds") {
      if (input$active_identification &
          input$threshold_decision & input$cor_threshold_decision) {
        return(signal_to_noise() > MinSNR() & max_cor() > MinCor())
      }
      if (input$threshold_decision) {
        return(signal_to_noise() > MinSNR())
      }
      if (input$active_identification &
          input$cor_threshold_decision) {
        return(max_cor() > MinCor())
      }
    }
    if (input$collapse_log_type == "Identities") {
      return(max_cor_identity())
    }
    if (input$collapse_log_type == "Both") {
      background_fill <- max_cor_identity()
      if (input$active_identification &
          input$threshold_decision & input$cor_threshold_decision) {
        background_fill[!(signal_to_noise() > MinSNR() &
                            max_cor() > MinCor())] <- "background"
        return(background_fill)
      }
      if (input$threshold_decision) {
        background_fill[!(signal_to_noise() > MinSNR())] <- "background"
        return(background_fill)
      }
      if (input$active_identification &
          input$cor_threshold_decision) {
        background_fill[!(max_cor() > MinCor())] <- "background"
        return(background_fill)
      }
    }
    return(NULL)
  })
  
  
  #Identification ----
  output$correlation_head <- renderUI({
    req(!is.null(preprocessed$data))
    req((input$threshold_decision | input$cor_threshold_decision))
    good_cor <- max_cor()[[data_click$data]] > MinCor() &
      signal_to_noise()[[data_click$data]] > MinSNR()
    good_sig <- signal_to_noise()[[data_click$data]] > MinSNR()
    good_match <- good_cor & good_sig
    
    boxLabel(
      text = if (input$cor_threshold_decision &
                 input$threshold_decision &
                 input$active_identification) {
        "Match"
      } else if (input$cor_threshold_decision &
                 input$active_identification) {
        "Cor"
      } else if (input$threshold_decision) {
        "SNR"
      } else{
        ""
      },
      status = if (input$cor_threshold_decision &
                   input$threshold_decision & input$active_identification) {
        if (good_match) {
          "success"
        }
        else{
          "error"
        }
      }
      else if (input$cor_threshold_decision &
               input$active_identification) {
        if (good_cor) {
          "success"
        }
        else {
          "error"
        }
      }
      else if (input$threshold_decision) {
        if (good_sig) {
          "success"
        }
        else{
          "error"
        }
      }
      else{
        NULL
      },
      tooltip = "This tells you whether the signal to noise ratio or the match observed is above or below the thresholds."
    )
  })
  
  #Check that correct logic exists for matching.
  # change 3 ----
  observe({
    if (!is.null(preprocessed$data) &
        input$id_strategy == "deriv" & input$active_identification) {
      if (!input$active_preprocessing |
          !input$smooth_decision |
          input$smoother != 3 |
          input$derivative_order != 1 |
          input$smoother_window != 90 | !input$derivative_abs) {
        show_alert(
          title = "Best practice not followed!",
          text = paste0(
            "If you are using the derivative library or model the typical best practice is to preprocess the spectra with ",
            "Smoothing/Derivative turned on, the Polynomial set to 3, the Derivative Order set to 1, the Wavenumber Window set to 90 ",
            "and the Absolute Value turned on because that is the way the library was created. You could be doing something special like uploading already processed spectra and if so feel free to ignore this warning."
          ),
          type = "warning"
        )
      }
    }
    if (!is.null(preprocessed$data) &
        input$id_strategy == "nobaseline" & input$active_identification) {
      if (!input$active_preprocessing |
          !input$baseline_decision |
          (input$smooth_decision &
           (input$derivative_order != 0 | input$derivative_abs))) {
        show_alert(
          title = "Best practice not followed!",
          text = paste0(
            "If you are using the no baseline library or model the typical best practice is to preprocess the spectra with ",
            "Baseline Correction turned on and setting Derivative Order to 0 and turning off Absolute Value if using Smoothing/Derivative. ",
            "because that is the way the library was created. You could be doing something special like uploading already processed spectra and if so feel free to ignore this warning."
          ),
          type = "warning"
        )
      }
    }
    
  })
  
  #The correlation matrix between the unknowns and the library.
  correlation <- reactive({
    req(!is.null(preprocessed$data))
    req(input$active_identification)
    req(!grepl("^model$", input$lib_type))
    withProgress(message = 'Analyzing Spectrum', value = 1 / 3, {
      cor_spec(
        x = DataR(),
        library = libraryR(),
        conform = T,
        type = "roll"
      )
    })
  })
  
  #The output from the AI classification algorithm.
  ai_output <- reactive({
    #tested working.
    req(!is.null(preprocessed$data))
    req(input$active_identification)
    req(grepl("^model$", input$lib_type))
    
    #rn <- runif(n = length(unique(libraryR()$all_variables)))
    mean <- rep.int(mean(unlist(DataR()$spectra)), times = length(unique(libraryR()$all_variables)))
    
    fill <- as_OpenSpecy(as.numeric(unique(libraryR()$all_variables)), spectra = data.frame(mean))
    
    data <- conform_spec(DataR(), range = fill$wavenumber, res = NULL)
    
    match_spec(data,
               library = libraryR(),
               na.rm = T,
               fill = fill)
  })
  
  #The maximum correlation or AI value.
  max_cor <- reactive({
    req(!is.null(preprocessed$data))
    #req(input$active_identification)
    if (isTruthy(input$active_identification)) {
      if (!grepl("^model$", input$lib_type)) {
        max_cor_named(correlation())
      }
      else {
        ai <- signif(ai_output()[["value"]], 2)
        names(ai) <- ai_output()[["name"]]
        ai
      }
    }
    else{
      NULL
    }
  })
  
  #The maximum correlation or AI value.
  max_cor_identity <- reactive({
    req(!is.null(preprocessed$data))
    if (isTruthy(input$active_identification)) {
      if (!grepl("^model$", input$lib_type)) {
        fifelse(
          max_cor() < MinCor(),
          rep.int("Unknown", length(max_cor())),
          libraryR()$metadata$material_class[match(names(max_cor()), libraryR()$metadata$sample_name)]
        )
      }
      else{
        fifelse(max_cor() < MinCor(),
                rep.int("Unknown", length(max_cor())),
                names(max_cor()))
      }
    }
    # change 4 ----
    if (isTruthy(input$active_identification)) {
      if (!grepl("^model$", input$lib_type)) {
        fifelse(
          max_cor() < MinCor(),
          rep.int("Unknown", length(max_cor())),
          libraryR()$metadata$material_class[match(names(max_cor()), libraryR()$metadata$sample_name)]
        )
      }
      else{
        fifelse(max_cor() < MinCor(),
                rep.int("Unknown", length(max_cor())),
                names(max_cor()))
      }
    }
    else{
      NULL
    }
  })
  
  MinCor <- reactive({
    req(!is.null(preprocessed$data))
    if (!input$cor_threshold_decision) {
      -Inf
    }
    else{
      input$MinCor
    }
  })
  
  output$cor_plot <- renderPlot({
    req(!is.null(preprocessed$data))
    ggplot() +
      geom_histogram(aes(x = max_cor()), fill = "white") +
      scale_x_continuous(trans =  scales::modulus_trans(p = 0, offset = 1)) +
      geom_vline(xintercept = MinCor(), color = "red") +
      theme_black_minimal() +
      labs(x = "Correlation")
  })
  

  #Metadata for all the matches for a single unknown spectrum
  matches_to_single <- reactive({
    req(input$active_identification)
    if (is.null(preprocessed$data)) {
      libraryR()$metadata %>%
        mutate("match_val" = NA)
    }
    else if (grepl("^model$", input$lib_type)) {
      data.table(
        object_id = names(DataR()$spectra),
        material_class = max_cor_identity(),
        match_val = ai_output()$value
      )
    }
    else{
      data.table(
        object_id = names(DataR()$spectra)[data_click$data],
        sample_name = names(libraryR()$spectra),
        match_val = c(correlation()[, data_click$data])
      )[order(-match_val), ] %>%
        left_join(libraryR()$metadata, by = c("sample_name")) %>%
        mutate(match_val = signif(match_val, 2)) %>%
        {
          if (input$cor_threshold_decision) {
            mutate(.,
                   name = ifelse(
                     match_val < input$MinCor,
                     rep.int("Unknown", nrow(.)),
                     material_class
                   ))
          } else{
            .
          }
        }
      
    }
  })
  
  #Spectral data for the selected match.
  match_selected <- reactive({
    # Default to first row if not yet clicked
    #req(input$file)
    #req(input$active_identification)
    req(!grepl("^model$", input$lib_type))
    if (!input$active_identification) {
      as_OpenSpecy(x = numeric(), spectra = data.table(empty = numeric()))
    }
    else{
      #need to make reactive
      id_select <-  ifelse(
        is.null(input$event_rows_selected),
        matches_to_single()[[1, "sample_name"]],
        matches_to_single()[[input$event_rows_selected, "sample_name"]]
      )#"00087f78d45c571524fce483ef10752e"	#matches_to_single[[1,column_name]]
      
      # Get data from filter_spec
      filter_spec(libraryR(), logic = id_select)
    }
  })
  
  #All matches table for the current selection
  top_matches <- reactive({
    #req(input$file)
    req(input$active_identification)
    req(!grepl("^model$", input$lib_type))
    if (is.null(preprocessed$data)) {
      matches_to_single() %>%
        dplyr::select("material_class",
                      "spectrum_identity",
                      "organization",
                      "sample_name")
    }
    else{
      matches_to_single() %>%
        dplyr::select(
          "match_val",
          "material_class",
          "spectrum_identity",
          "organization",
          "sample_name"
        )
    }
  })
  
  #Create the data table that goes below the plot which provides extra metadata.
  match_metadata <- reactive({
    req(!is.null(preprocessed$data))
    if (input$active_identification &
        !grepl("^model$", input$lib_type)) {
      selected_match <- matches_to_single()[input$event_rows_selected, ]
      dataR_metadata <- DataR()$metadata
      dataR_metadata$signal_to_noise <- signal_to_noise()
      setkey(dataR_metadata, col_id)
      setkey(selected_match, object_id)
      
      result <- dataR_metadata[selected_match, on = c(col_id = "object_id")]
      result <- result[, !sapply(result, OpenSpecy::is_empty_vector), with = FALSE] %>%
        select(
          file_name,
          col_id,
          material_class,
          spectrum_identity,
          match_val,
          signal_to_noise,
          everything()
        )
      result
    }
    else if (input$active_identification &
             grepl("^model$", input$lib_type)) {
      result <- bind_cols(DataR()$metadata[data_click$data, ], matches_to_single()[data_click$data, ])
      result$signal_to_noise <- signal_to_noise()[data_click$data]
      result <- result[, !sapply(result, OpenSpecy::is_empty_vector), with = FALSE] %>%
        mutate(match_val = signif(match_val, 2)) %>%
        select(file_name,
               col_id,
               material_class,
               match_val,
               signal_to_noise,
               everything())
      result
    }
    else{
      DataR()$metadata[data_click$data, ] %>%
        .[, !sapply(., OpenSpecy::is_empty_vector), with = F]
    }
  })
  
  # Display ----
  
  #Histogram of SNR
  output$snr_plot <- renderPlot({
    req(!is.null(preprocessed$data))
    ggplot() +
      geom_histogram(aes(x = signal_to_noise()), fill = "white") +
      scale_x_continuous(trans =  scales::modulus_trans(p = 0, offset = 1)) +
      geom_vline(xintercept = MinSNR(), color = "red") +
      theme_black_minimal() +
      labs(x = "Signal/Noise")
  })
  
  #Table of metadata for the selected library value
  output$eventmetadata <- DT::renderDataTable({
    req(!is.null(preprocessed$data))
    #req(!grepl("^ai$", input$id_strategy))
    datatable(
      match_metadata(),
      escape = FALSE,
      options = list(
        dom = 't',
        bSort = F,
        scrollX = TRUE,
        lengthChange = FALSE,
        info = FALSE
      ),
      rownames = FALSE,
      style = 'bootstrap',
      caption = "Selection Metadata",
      selection = list(mode = 'none')
    )
  })
  
  # Create the data tables for all matches
  output$event <- DT::renderDataTable({
    req(input$active_identification)
    req(!grepl("^model$", input$lib_type))
    datatable(
      top_matches() %>%
        mutate(
          organization = as.factor(organization),
          material_class = as.factor(material_class)
        ),
      options = list(
        searchHighlight = TRUE,
        scrollX = TRUE,
        sDom  = '<"top">lrt<"bottom">ip',
        lengthChange = FALSE,
        pageLength = 5
      ),
      rownames = FALSE,
      filter = "top",
      caption = "Selectable Matches",
      style = "bootstrap",
      selection = list(mode = "single", selected = c(1))
    )
  })
  
  # Progress Bars
  output$choice_names <- renderUI({
    req(ncol(preprocessed$data$spectra) > 1)
    #req(input$threshold_decision | input$active_identification)
    choice_names = c(
      if (input$active_identification)
        "Match Name"
      else
        NA,
      if (input$active_identification &
          input$lib_type != "model")
        "Match ID"
      else
        NA,
      if (!is.null(max_cor()))
        "Match Value"
      else
        NA,
      if (!is.null(signal_to_noise()))
        "Signal/Noise"
      else
        NA,
      if (isTruthy(particles_logi()))
        "Feature ID"
      else
        NA
    )
    choice_names = choice_names[!is.na(choice_names)]
    tagList(fluidRow(column(
      6,
      selectInput(
        inputId = "map_color",
        label = "Map Color",
        choices = choice_names
      )
    )))
  })
  
  output$progress_bars <- renderUI({
    req(ncol(preprocessed$data$spectra) > 1)
    req(
      input$threshold_decision |
        (
          input$cor_threshold_decision & input$active_identification
        )
    )
    tagList(box(
      title = "Summary",
      maximizable = T,
      width = 12,
      fluidRow(
        column(
          4,
          if (input$threshold_decision)
            shinyWidgets::progressBar(
              id = "signal_progress",
              value = sum(signal_to_noise() > MinSNR()) / length(signal_to_noise()) * 100,
              status = "success",
              title = "Good Signal",
              display_pct = TRUE
            )
          else
            NULL
        ),
        column(
          4,
          if (input$cor_threshold_decision &
              input$active_identification)
            shinyWidgets::progressBar(
              id = "correlation_progress",
              value = sum(max_cor() > MinCor()) / length(max_cor()) * 100,
              status = "success",
              title = "Good Match Values",
              display_pct = TRUE
            )
          else
            NULL
        ),
        column(
          4,
          if (input$cor_threshold_decision &
              input$active_identification & input$threshold_decision)
            shinyWidgets::progressBar(
              id = "match_progress",
              value = sum(signal_to_noise() > MinSNR() &
                            max_cor() > MinCor()) / length(signal_to_noise()) * 100,
              status = "success",
              title = "Good Identifications",
              display_pct = TRUE
            )
          else
            NULL
        )
      ),
      fluidRow(column(
        6, plotOutput("particle_plot", height = "25vh")
      ), column(
        6, plotOutput("material_plot", height = "25vh")
      ))
    ))
  })
  
  output$MyPlotC <- renderPlotly({
    #req(input$id_strategy == "correlation")
    #req(preprocessed$data)
    plotly_spec(
      x = if (!is.null(preprocessed$data)) {
        DataR_plot()
      } else{
        match_selected()
      },
      x2 = if (!is.null(preprocessed$data) &
               !grepl("^model$", input$lib_type)) {
        match_selected()
      } else{
        NULL
      },
      make_rel = input$make_rel_decision,
      source = "B"
    ) %>%
      config(modeBarButtonsToAdd = list("drawopenpath", "eraseshape"))
  })
  
  #Heatmap ----
  #Display the map or batch data in a selectable heatmap.
  output$heatmap <- renderPlotly({
    req(!is.null(preprocessed$data))
    req(ncol(preprocessed$data$spectra) > 1)
    #req(input$map_color)
    if (isTruthy(particles_logi())) {
      test = def_features(DataR(), features = particles_logi())
    }
    else{
      test = DataR()
    }
    
    heatmap_spec(
      x = test,
      z = if (!is.null(max_cor()) &
              !isTruthy(input$map_color)) {
        signif(max_cor(), 2)
      }
      else if (!is.null(signal_to_noise()) &
               !isTruthy(input$map_color)) {
        signif(signal_to_noise(), 2)
      }
      else if (!is.null(max_cor()) &
               input$map_color == "Match ID") {
        names(max_cor())
      }
      else if (!is.null(max_cor()) &
               input$map_color == "Match Value") {
        signif(max_cor(), 2)
      }
      else if (!is.null(signal_to_noise()) &
               input$map_color == "Signal/Noise") {
        signif(signal_to_noise(), 2)
      }
      else if (!is.null(max_cor()) &
               input$map_color == "Match Name") {
        max_cor_identity()
      }
      else if (isTruthy(particles_logi()) &
               input$map_color == "Feature ID") {
        test$metadata$feature_id
      }
      else{
        NULL
      },
      sn = signif(signal_to_noise(), 2),
      cor = if (is.null(max_cor())) {
        max_cor()
      } else{
        signif(max_cor(), 2)
      },
      min_sn = MinSNR(),
      min_cor = MinCor(),
      select = data_click$data,
      source = "heat_plot"
    ) %>%
      event_register(event = "plotly_click")
  })
  
  thresholded_particles <- reactive({
    req(input$collapse_decision)
    collapse_fun <- function(x, type = input$collapse_type) {
      switch(
        type,
        "Mean" = mean(x),
        "Median" = median(x),
        "Geometric Mean" = exp(mean(log(x)))
      )
    }
    collapse_spec(def_features(DataR(), features = particles_logi()), fun = collapse_fun) %>%
      filter_spec(., logic = .$metadata$feature_id != "-88")
  })
  
  #Summary Plots ----
  output$particle_plot <- renderPlot({
    req(!is.null(preprocessed$data))
    req(thresholded_particles()$metadata$area)
    ggplot() +
      geom_histogram(aes(x = sqrt(
        thresholded_particles()$metadata$area
      )), fill = "white") +
      theme_black_minimal(base_size = 15) +
      labs(x = "Nominal Particle Size (√area)", y = "Count")
  })
  
  output$material_plot <- renderPlot({
    req(!is.null(preprocessed$data))
    req(max_cor_identity())
    if (isTruthy(thresholded_particles())) {
      if (all(grepl(
        "_[0-9]+",
        thresholded_particles()$metadata$feature_id
      ))) {
        match_names <- gsub("_[0-9]+",
                            "",
                            thresholded_particles()$metadata$feature_id)
      }
    }  else {
      match_names <- max_cor_identity()
    }
    
    ggplot() +
      geom_bar(aes(y = match_names, fill = match_names)) +
      theme_black_minimal(base_size = 15) +
      theme(legend.position = "none") +
      labs(x = "Count", y = "Material Class")
  })
  
  
  # Data Download options ----
  # Progress Bars
  output$download_ui <- renderUI({
    choice_names = c(
      "Test Data",
      "Test Map",
      if (isTruthy(ncol(preprocessed$data$spectra) >= 1))
        "Your Spectra"
      else
        NA,
      if (input$active_identification)
        c("Library Spectra", "Top Matches")
      else
        NA,
      if (input$collapse_decision)
        "Thresholded Particles"
      else
        NA
    )
    
    choice_names = choice_names[!is.na(choice_names)]
    tagList(
      selectInput(
        inputId = "download_selection",
        label = downloadButton("download_data", style = "background-color: rgb(0,0,0); color: rgb(255,255,255);"),
        choices = choice_names
      ) %>%
        popover(
          title = "Options for downloading spectra and metadata from the analysis.
                                          Test Data is a Raman HDPE spectrum in csv format. Test Map is an FTIR ENVI file of a CA particle.
                                          Your Spectra will download your data with whatever processing options are active. Library Spectra
                                          will download the current library selected. Top Matches downloads the top identifications in the
                                          active analysis. All Matches will download all identifications with the library. Thresholded Particles will download a version of your spectra using the active
                                          thresholds selected to infer where particles are in spectral maps, particle spectra are collapsed
                                          to their medians and locations to their centroids.",
          content = "Download Options",
          placement = "left"
        )
    )
  })
  
  output$top_n <- renderUI({
    req(ncol(preprocessed$data$spectra) >= 1)
    req(input$active_identification)
    req(input$download_selection == "Top Matches")
    req(!grepl("^model$", input$lib_type))
    tagList(
      numericInput(
        "top_n_input",
        "Top N",
        value = 1,
        min = 1,
        max = ncol(libraryR()$spectra),
        step = 1
      ),
      selectInput(
        inputId = "columns_selected",
        label = "Columns to save",
        choices = c("Simple", "All")
      )
    )
  })
  output$download_data <- downloadHandler(
    filename = function() {
      if (input$download_selection == "Test Map") {
        paste0(input$download_selection, human_ts(), ".zip")
      } else{
        paste0(input$download_selection, human_ts(), ".csv")
      }
    },
    content = function(file) {
      if (input$download_selection == "Test Data") {
        fwrite(testdata, file)
      }
      if (input$download_selection == "Test Map") {
        file.copy(read_extdata("CA_tiny_map.zip"), file)
      }
      if (input$download_selection == "Your Spectra") {
        your_spec <- DataR()
        your_spec$metadata$signal_to_noise <- signal_to_noise()
        write_spec(your_spec, file)
      }
      if (input$download_selection == "Library Spectra") {
        write_spec(libraryR(), file)
      }
      if (input$download_selection == "Top Matches") {
        if (!grepl("^model$", input$lib_type)) {
          dataR_metadata <- data.table(
            match_threshold = MinCor(),
            signal_to_noise = signal_to_noise(),
            signal_threshold = MinSNR(),
            good_signal = signal_to_noise() > MinSNR()
          ) %>%
            bind_cols(DataR()$metadata)
          
          all_matches <- reshape2::melt(correlation()) %>%
            as.data.table() %>%
            left_join(libraryR()$metadata %>% select(-any_of(c(
              "col_id", "file_name"
            ))),
            by = c("Var1" = "sample_name")) %>%
            left_join(dataR_metadata, by = c("Var2" = "col_id")) %>%
            rename(
              "sample_name" = "Var1",
              "col_id" = "Var2",
              "match_val" = "value"
            ) %>%
            mutate(
              good_match_vals = match_val > match_threshold,
              good_matches = match_val > match_threshold &
                signal_to_noise > signal_threshold
            ) %>%
            .[, !sapply(., OpenSpecy::is_empty_vector), with = F] %>%
            select(
              file_name,
              col_id,
              material_class,
              spectrum_identity,
              match_val,
              signal_to_noise,
              everything()
            ) %>%
            .[order(-match_val), .SD[1:input$top_n_input], by = col_id] %>%
            {
              if (grepl("Simple", input$columns_selected)) {
                select(.,
                       file_name,
                       col_id,
                       material_class,
                       match_val,
                       signal_to_noise)
              } else{
                .
              }
            } %>%
            mutate(material_class = ifelse(
              match_val < MinCor(),
              rep.int("Unknown", nrow(.)),
              material_class
            ))
          
          fwrite(all_matches, file)
        }
        else{
          result <- bind_cols(DataR()$metadata, matches_to_single())
          result$signal_to_noise <- signal_to_noise()
          result <- result[, !sapply(result, OpenSpecy::is_empty_vector), with = FALSE] %>%
            select(file_name,
                   col_id,
                   material_class,
                   match_val,
                   signal_to_noise,
                   everything()) %>%
            mutate(material_class = ifelse(
              match_val < MinCor(),
              rep.int("Unknown", nrow(.)),
              material_class
            ))
          
          fwrite(result, file)
        }
      }
      if (input$download_selection == "Thresholded Particles") {
        write_spec(thresholded_particles(), file = file)
      }
    }
  )
  
  # Hide functions or objects when the shouldn't exist.
  observe({
    toggle(id = "heatmap",
           condition = !is.null(preprocessed$data))
    toggle(id = "placeholder1", condition = is.null(preprocessed$data))
    if (!is.null(preprocessed$data)) {
      toggle(id = "heatmap",
             condition = ncol(preprocessed$data$spectra) > 1)
    }
    if (is.null(event_data("plotly_click", source = "heat_plot"))) {
      data_click$data <- 1
    }
    else{
      data_click$data <- event_data("plotly_click", source = "heat_plot")[["pointNumber"]] + 1
    }
  })
  
  # #Google translate.
  # output$translate <- renderUI({
  #   if(translate & curl::has_internet()) {
  #     includeHTML("www/googletranslate.html")
  #   }
  # })
  
  # Log events ----
  
  user_metadata <- reactive({
    list(
      #user_name = input$fingerprint,
      time = human_ts(),
      session_name = session_id,
      data_id = digest::digest(preprocessed$data, algo = "md5"),
      active = input$active_preprocessing,
      adj_intens = input$intensity_decision,
      type = input$intensity_corr,
      restric_range = input$range_decision,
      restric_range_min = input$MinRange,
      restric_range_max = input$MaxRange,
      flatten_range = input$co2_decision,
      flatten_range_min = input$MinFlat,
      flatten_range_max = input$MaxFlat,
      baseline_decision = input$baseline_decision,
      subtr_baseline = input$baseline,
      smooth_intens = input$smooth_decision,
      polynomial = input$smoother,
      window = input$smoother_window,
      derivative = input$derivative_order,
      abs = input$derivative_abs,
      download_selection = input$download_selection,
      id_strategy = input$id_strategy,
      cor_threshold_decision = input$cor_threshold_decision,
      min_cor = input$MinCor,
      threshold_decision = input$threshold_decision,
      min_sn = input$MinSNR,
      signal_selection = input$signal_selection
    )
  })
  
  # observe({
  #   req(!is.null(preprocessed$data))
  #       loggit("INFO", "trigger",
  #              user_metadata())
  # })
  
  #output$event_test <- renderPrint({
  #    list(
  #        conform_spec = input$conform_decision,
  #        conform_args = list(range = NULL, res = input$conform_res, type = input$conform_selection)
  #    )
  #})
  
}
