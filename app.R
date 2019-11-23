source("global.R")

ui <-
  fluidPage(
    fluidRow(
      div(
        style = "display: inline-block; vertical-align: top",
        textInput("tokenField", label = NULL, value = "",
                  placeholder = "Token in agriculture production dataset")
      ),
      div(
        style = "display: inline-block; vertical-align: top",
        actionButton("dummy", "Go!") # really does nothing
      ),
      div(
        style = "display: inline-block; vertical-align: top",
        actionButton("clear_row", "Clear row",
                     style = "background-color: tomato;")
      ),
      div(
        style = "display: inline-block; vertical-align: top",
        actionButton("save_current", "Save current data",
                     style = "background-color: yellow;")
      ),
      div(
        style = "display: inline-block; vertical-align: top",
        actionButton("not_outlier", "Looks fine",
                     style = "background-color: green; color: white; font-weight: bold;")
      ),
      # https://shiny.rstudio.com/articles/generating-reports.html
      div(
        style = "display: inline-block; vertical-align: top",
        actionButton("report", "Generate report")
      )
    ),
    fluidRow(
      verbatimTextOutput("info_country")
    ),
    tabsetPanel(
      id = "maincontainer",
      tabPanel(
        "Outliers",
        value = "myoutliers",
        uiOutput("helpoutliers"),
        DT::dataTableOutput("maintable")
      ),
      tabPanel(
        "Plots",
        value = "myplots",
        plotOutput("prodplot"),
        plotOutput("yieldplot"),
        plotOutput("areaplot")
      ),
      tabPanel(
        "Data",
        value = "mydata",
        rHandsontableOutput("hot")
      ),
      tabPanel('Help', 
        HTML(markdown::markdownToHTML(HELP_FILE, fragment.only = TRUE))
      )
    )
  )

server <- function(input, output, session) {

  values <-
    reactiveValues(
      mydta = NULL,
      outliers_info = NULL,
      country = NULL,
      recomputed = NULL,
      modif = NULL,
      flags = NULL,
      file_cache = NA_character_,
      session_info = NULL
    )

  output$info_country <-
    renderText({
      paste("country:", values$country, " / item:", values$item)
    })

  TokenValidator <- eventReactive(input$tokenField, {
    validate({
      need(
        grepl(
          sprintf("%1$s{8}-%1$s{4}-%1$s{4}-%1$s{4}-%1$s{11}", "[a-f0-9]"),
          input$tokenField),
        "Please supply a valid token")
    })
    return("")
  })

  output$tokenValidator <- renderText(TokenValidator())

  swsData <- reactive({

    TokenValidator()


    SetClientFiles(CONFIG_CERTIFICATES)

    # QA
    validate(need(try(GetTestEnvironment(SERVER, input$tokenField)),
                  "Could not connect to the SWS"))

    validate(need(try(!is.null(swsContext.baseRestUrl)),
                  "The token is invalid"))

    dataReadProgress <- Progress$new(session, min = 0, max = 100)

    dataReadProgress$set(value = 100, message = "Loading your data from the SWS")

    #on.exit(dataReadProgress$close())

    flagValidTable <- ReadDatatable("valid_flags")

    flagValidTable[, Valid := NULL]

    values$flags <- flagValidTable

    values$session_info <- sapply(ls(pattern = "swsContext", pos = 1), get)

    #DIM_GEO <- ref_m49[id == input$sel_m49]$code
    DIM_GEO <- swsContext.datasets[[1]]@dimensions$geographicAreaM49@keys

    if (length(DIM_GEO) > 1) {
      stop("Only one country")
    }

    values$country <- ref_m49[code == DIM_GEO]$id

    #DIM_ITEM <- ref_cpc$code
    #DIM_ITEM <- c("0111", "0112", "0113", "0115")
    #DIM_ITEM <- sort(unique(ref_cpc[id == input$sel_cpc]$code))
    DIM_ITEM <- sort(unique(ref_cpc$code))

    # XXX: no 01919.90 and other on LIVE
    DIM_ITEM <- DIM_ITEM[!(DIM_ITEM %in% c("01919.90", "21111.01b", "2153"))]
    DIM_ITEM <- DIM_ITEM[!grepl("[ib]$", DIM_ITEM)]
    DIM_ITEM <- DIM_ITEM[!grepl("^21", DIM_ITEM)] # maybe just some of these

    key_prod <-
      DatasetKey(
        domain = "aproduction",
        dataset = "aproduction",
        dimensions =
          list(
            geographicAreaM49 = Dimension(name = "geographicAreaM49", keys = DIM_GEO),
            measuredElement   = Dimension(name = "measuredElement",   keys = DIM_ELEM_PROD),
            measuredItemCPC   = Dimension(name = "measuredItemCPC",   keys = DIM_ITEM),
            timePointYears    = Dimension(name = "timePointYears",    keys = DIM_TIME)
          )
      )

    data_prod_sws <-
      tryCatch(
        GetData(key_prod),
        error = function(e) stop(safeError("Something went wrong when downloading PRODUCTION data."))
      )

    values$file_cache <- file.path(SESSIONS_DIR, paste0(input$tokenField, ".rds"))

    if (file.exists(values$file_cache)) {
      cache <- readRDS(values$file_cache)
    } else {
      cache <- list(crops = list(), livestok = list(), milk = list(), egges = list())
      cache$crops$original_data <- data_prod_sws
      cache$session_info <- values$session_info
      saveRDS(cache, values$file_cache)
    }

    data_prod_sws <-
      merge(
        data_prod_sws,
        flagValidTable,
        by = c("flagObservationStatus", "flagMethod")
      )

    setcolorder(
      data_prod_sws,
      c(setdiff(names(data_prod_sws), names(flagValidTable)), names(flagValidTable))
    )

    # Recalculate identities
    data_prod_complete <-
      CJ(
        geographicAreaM49 = unique(data_prod_sws$geographicAreaM49),
        measuredElement   = unique(data_prod_sws$measuredElement),
        measuredItemCPC   = unique(data_prod_sws$measuredItemCPC),
        timePointYears    = unique(data_prod_sws$timePointYears)
      )

    data_prod_sws <-
      data_prod_sws[
        data_prod_complete,
        on = c("geographicAreaM49", "measuredElement", "measuredItemCPC", "timePointYears")
      ]

    data_prod_sws[,
      keep := (sum(!is.na(Value)) >= 2),
      by = c("geographicAreaM49", "measuredItemCPC", "timePointYears")
    ]

    data_prod_sws <- data_prod_sws[keep == TRUE | !is.na(Value)]

    data_prod_sws[, keep := NULL]

    data_prod_sws[is.na(Protected), Protected := FALSE]

    data_prod_sws[,
      to_compute := sum(!is.na(Value)) >= 2 & any(Protected == FALSE),
      by = c("geographicAreaM49", "measuredItemCPC", "timePointYears")
    ]

    data_prod_sws[
      to_compute == TRUE,
      recomputed := compute_remaining_element(.SD),
      by = c("geographicAreaM49", "measuredItemCPC", "timePointYears")
    ]

    # SWS keeps only 6 digits
    data_prod_sws[, recomputed := round(recomputed, 6)]

    data_prod_sws[dplyr::near(recomputed, Value), recomputed := NA_real_]

    #d[Value != recomputed]

    data_prod_sws[, to_compute := FALSE]

    data_prod_sws[,
      to_compute := any(!is.na(recomputed)),
      by = c("geographicAreaM49", "measuredItemCPC", "timePointYears")
    ]

    data_prod_sws[
      to_compute == TRUE,
      new_flag :=
        aggregateObservationFlag(flagObservationStatus[is.na(recomputed)]),
      by = c("geographicAreaM49", "measuredItemCPC", "timePointYears")
    ]

    data_prod_sws[
      !is.na(recomputed),
      `:=`(
        Value = recomputed,
        flagObservationStatus = new_flag,
        flagMethod = "i"
      )
    ]

    d_recomputed <-
      data_prod_sws[
        !is.na(recomputed),
        c("geographicAreaM49", "measuredElement", "measuredItemCPC",
          "timePointYears", "Value", "flagObservationStatus", "flagMethod"),
        with = FALSE
      ]

    dataReadProgress_save <- Progress$new(session, min = 0, max = 100)

    dataReadProgress_save$set(value = 100, message = "Sending recomputed data to SWS")

    on.exit({
      dataReadProgress_save$close()
      dataReadProgress$close()
    })

    metad <- copy(d_recomputed)

    metad[, c("Value", "flagObservationStatus", "flagMethod") := NULL]

    metad[,
      `:=`(
        Metadata          = "GENERAL",
        Metadata_Element  = "COMMENT",
        Metadata_Language = "en",
        Metadata_Value    = "Recalculated during production validation"
      )
    ]

    SaveData(
      domain = "agriculture",
      dataset = "aproduction",
      data = d_recomputed,
      metadata = metad,
      waitTimeout = 180000
    )

    values$recomputed <-
      d_recomputed[,
        c("geographicAreaM49", "measuredElement",
          "measuredItemCPC", "timePointYears"),
        with = FALSE
      ]

    data_prod_sws[, c("Protected", "to_compute", "recomputed", "new_flag") := NULL]


    # / Recalculate identities

    data_prod_local <- readRDS(file.path(LOCAL_PROD_FILES, paste0(DIM_GEO, ".rds")))

    data_prod <- rbind(data_prod_local[timePointYears < DIM_TIME[1]], data_prod_sws)

    data_prod[measuredElement == "5510", measuredElement := "production"]
    data_prod[measuredElement == "5312", measuredElement := "area"]
    data_prod[measuredElement == "5421", measuredElement := "yield"]

    key_trade <-
      DatasetKey(
        domain = "trade",
        dataset = "total_trade_cpc_m49",
        dimensions =
          list(
            geographicAreaM49    = Dimension(name = "geographicAreaM49",    keys = DIM_GEO),
            measuredElementTrade = Dimension(name = "measuredElementTrade", keys = DIM_ELEM_TRADE),
            measuredItemCPC      = Dimension(name = "measuredItemCPC",      keys = DIM_ITEM),
            timePointYears       = Dimension(name = "timePointYears",       keys = DIM_TIME)
          )
      )

    data_trade_sws <-
      tryCatch(
        GetData(key_trade),
        error = function(e) stop(safeError("Something went wrong when downloading TRADE data."))
      )

    data_trade_local <- readRDS(file.path(LOCAL_TRADE_FILES, paste0(DIM_GEO, ".rds")))

    data_trade_local <- data_trade_local[measuredElementTrade %in% DIM_ELEM_TRADE]

    data_trade <- rbind(data_trade_local[timePointYears < DIM_TIME[1]], data_trade_sws)

    setnames(data_trade, "measuredElementTrade", "measuredElement")

    data_trade[measuredElement == "5610", measuredElement := "imports"]
    data_trade[measuredElement == "5910", measuredElement := "exports"]

    d <- rbind(data_prod, data_trade)

    d <- merge(d, flagValidTable, by = c("flagObservationStatus", "flagMethod"))

    setcolorder(d, c(setdiff(names(d), names(flagValidTable)), names(flagValidTable)))

    setorderv(d, c("geographicAreaM49", "measuredItemCPC", "measuredElement", "timePointYears"))

    return(d[])
  })

  output$yieldplot <-
    renderPlot({
      item <- values$mydta[input$maintable_rows_selected, measuredItemCPC]
      d <- swsData()[measuredItemCPC == item & measuredElement == "yield"]

      if (nrow(d) > 0) {
        d[,
          MeanOld := mean(Value[timePointYears %in% 2013:2015], na.rm = TRUE),
          by = c("geographicAreaM49", "measuredElement", "measuredItemCPC")
        ]

        d[timePointYears >= 2016, upper := MeanOld * (1 + THRESHOLD_PA)]
        d[timePointYears >= 2016, lower := MeanOld * (1 - THRESHOLD_PA)]
        d <- d[data.table(timePointYears = as.character(1960:2018)), on = "timePointYears"]
        d <- d[timePointYears >= 1990]
        ggplot(d, aes(x = timePointYears, group = measuredElement, color = measuredElement)) +
         geom_line(aes(y = Value), size = 2) +
         scale_size(range = c(1, 2), guide = FALSE) +
         geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1) +
         #geom_smooth(linetype = 2) +
         ggtitle(paste("YIELD", item))
      } else {
        NULL
      }
    })

  output$prodplot <-
    renderPlot({
      item <- values$mydta[input$maintable_rows_selected, measuredItemCPC]
      d <- swsData()[measuredItemCPC == item]

      d[
        measuredElement == "production",
        MeanOld := mean(Value[timePointYears %in% 2013:2015], na.rm = TRUE),
        by = c("geographicAreaM49", "measuredElement", "measuredItemCPC")
      ]

      d[timePointYears >= 2016, upper := MeanOld * (1 + THRESHOLD_PA)]
      d[timePointYears >= 2016, lower := MeanOld * (1 - THRESHOLD_PA)]
      d <- d[measuredElement %in% c("production", "imports", "exports")]
      d[, plotsize := ifelse(measuredElement == "production", 2, 1)]
      d[, measuredElement := factor(measuredElement, levels = c("production", "imports", "exports"))]
      d <- d[data.table(timePointYears = as.character(1960:2018)), on = "timePointYears"]
      d <- d[timePointYears >= 1990]
      ggplot(d, aes(x = timePointYears, group = measuredElement, color = measuredElement)) +
       geom_line(aes(y = Value, size = plotsize)) +
       scale_size(range = c(1, 2), guide = FALSE) +
       geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1) +
       #geom_smooth(linetype = 2) +
       ggtitle(paste("PRODUCTION", item))
    })

  output$hot <-
    renderRHandsontable({
      rhandsontable(values$current_data, height = 500, col_highlight = 0,
                    row_highlight = values$modif, selectCallback = TRUE,
                    readOnly = FALSE) %>%
        hot_col("modified", colWidths = 0.1) %>%
        hot_col("Value", renderer = "
            function(instance, td, row) {
                Handsontable.renderers.NumericRenderer.apply(this, arguments);
                if (instance.params) {
                    hrows = instance.params.row_highlight
                    hrows = hrows instanceof Array ? hrows : [hrows]
                }
                if (instance.params && hrows.includes(row)) td.style.background = 'red';
            }")
    })


  output$areaplot <-
    renderPlot({
      item <- values$mydta[input$maintable_rows_selected, measuredItemCPC]

      d <- swsData()[measuredItemCPC == item & measuredElement == "area"]

      if (nrow(d) > 0) {
        d[,
          MeanOld := mean(Value[timePointYears %in% 2013:2015], na.rm = TRUE),
          by = c("geographicAreaM49", "measuredElement", "measuredItemCPC")
        ]

        d[timePointYears >= 2016, upper := MeanOld * (1 + THRESHOLD_PA)]
        d[timePointYears >= 2016, lower := MeanOld * (1 - THRESHOLD_PA)]
        d <- d[data.table(timePointYears = as.character(1960:2018)), on = "timePointYears"]
        d <- d[timePointYears >= 1990]
        ggplot(d, aes(x = timePointYears, group = measuredElement, color = measuredElement)) +
         geom_line(aes(y = Value), size = 2) +
         scale_size(range = c(1, 2), guide = FALSE) +
         geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1) +
         #geom_smooth(linetype = 2) +
         ggtitle(paste("AREA", item))
      } else {
        NULL
      }
    })

  output$maintable <-
    DT::renderDataTable({
      d <- swsData()

      # Update on this events
      input$not_outlier
      input$save_current

      d_complete <-
        CJ(
          geographicAreaM49 = unique(d$geographicAreaM49),
          measuredElement   = unique(d$measuredElement),
          measuredItemCPC   = unique(d$measuredItemCPC),
          timePointYears    = unique(d$timePointYears)
        )

      d <- d[d_complete,
             on = c("geographicAreaM49", "measuredElement",
                    "measuredItemCPC", "timePointYears")]

      d[,
        MeanOld := mean(Value[timePointYears %in% 2013:2015], na.rm = TRUE),
        by = c('geographicAreaM49', 'measuredItemCPC', 'measuredElement')
      ]

      d[, ratio := Value / MeanOld]

      d[, outlier := FALSE]

      d[
        Protected == TRUE &
          timePointYears >= 2016 &
          !is.na(ratio) &
          measuredElement %in% c("production", "area"),
        outlier := abs(ratio - 1) > THRESHOLD_PA
      ]

      d[
        Protected == TRUE &
          timePointYears >= 2016 &
          !is.na(ratio) &
          measuredElement == "yield",
        outlier := abs(ratio - 1) > THRESHOLD_Y
      ]

      cache <- readRDS(values$file_cache)

      if (!("original_outliers" %in% names(cache$crops))) {
        cache$crops$original_outliers <-
          d[outlier == TRUE][, -"outlier", with = FALSE]

        saveRDS(cache, values$file_cache)
      }

      if (!("fixed_outliers" %in% names(cache$crops))) {
        cache$crops$fixed_outliers <-
          data.table(
            geographicAreaM49 = character(),
            measuredItemCPC = character(),
            status = character()
          )

        saveRDS(cache, values$file_cache)
      }


      d_unique <-
        unique(
          d[outlier == TRUE, .(geographicAreaM49, measuredItemCPC, measuredElement, outlier)]
        )

      values$outliers_info <-
        list(
          items      = length(d_unique[, unique(measuredItemCPC)]),
          total      = nrow(d_unique),
          area       = nrow(d_unique[measuredElement == "area"]),
          production = nrow(d_unique[measuredElement == "production"]),
          yield      = nrow(d_unique[measuredElement == "yield"])
        )

      d_out <-
        dcast.data.table(
          d_unique,
          geographicAreaM49 + measuredItemCPC ~ measuredElement,
          value.var = 'outlier'
        )

      d_out[!cache$crops$fixed_outliers, on = c("geographicAreaM49", "measuredItemCPC")]

      avg_value <-
        unique(
          d[outlier == TRUE][, .(geographicAreaM49, measuredItemCPC, MeanOld)],
          by = c("geographicAreaM49", "measuredItemCPC")
        )

      d_table <-
        merge(d_out, avg_value, by = c("geographicAreaM49", "measuredItemCPC"))

      # remove already fixed outliers
      d_table <-
        d_table[
          !unique(cache$crops$fixed_outliers[, .(geographicAreaM49, measuredItemCPC)]),
          on = c("geographicAreaM49", "measuredItemCPC")
        ]

      d_table[, geographicAreaM49 := NULL]

      d_table <-
        merge(
          d_table,
          ref_cpc[, .(measuredItemCPC = code, description)],
          by = "measuredItemCPC",
          all.x = TRUE
        )

      setorderv(d_table, "MeanOld", order = -1)

      d_table[, status := factor(NA_character_, levels = c("NO", "YES"))]

      values$mydta <- d_table

      setnames(d_table, "MeanOld", "avg_prod_2013_2015")

      myd <-
        DT::datatable(
          d_table, rownames = FALSE, selection = 'single',
          callback = DT::JS('$("tbody").on("click.dt", "tr", function() {tabs = $("li"); $(tabs[0]).click()});')
        )

      myd <-
        DT::formatCurrency(myd, "avg_prod_2013_2015", digits = 1, currency = '')

      return(myd)
    })

  output$helpoutliers <-
    renderUI({
      req(input$tokenField)

      out_msg <-
        sprintf(
          "There are outliers in %s items and %s elements (of which:
            %s in area, %s in production, %s in yield).",
          values$outliers_info$items,
          values$outliers_info$total,
          values$outliers_info$area,
          values$outliers_info$production,
          values$outliers_info$yield
        )

      div(style = "font-weight: bold; background-color: lightyellow;",
        div(out_msg),
        div(style = "color: red;", "Choose one row below (i.e., click on a row)"),
        br()
      )
    })

  # https://shiny.rstudio.com/articles/generating-reports.html
  output$generate_report <-
    downloadHandler(
      filename = "report.docx",
      content = function(file) {
        cache <- readRDS(values$file_cache)
        # Copy the report file to a temporary directory before processing it, in
        # case we don't have write permissions to the current working dir (which
        # can happen when deployed).
        tempReport <- file.path(tempdir(), "report.Rmd")
        file.copy(REPORT_TEMPLATE, tempReport, overwrite = TRUE)

        ## Set up parameters to pass to Rmd document
        params <- list(results = cache)

        # Knit the document, passing in the `params` list, and eval it in a
        # child of the global environment (this isolates the code in the document
        # from the code in this app).
        rmarkdown::render(
          tempReport,
          output_file = file,
          params = params,
          envir = new.env(parent = globalenv())
        )

        if (REMOTE == TRUE) {
          send_mail(
            from = "do-not-reply@fao.org",
            to = cache$session_info$swsContext.userEmail,
            subject = "Production validation report",
            body = c("Production validation report attached.", file)
          )
        }

        removeModal()
      }
    )

  # OBSERVERS

  observeEvent(input$report, {
    cache <- readRDS(values$file_cache)
    tmp <-
      merge(
        unique(cache$crops$original_outliers[, c("geographicAreaM49", "measuredItemCPC"), with = FALSE]),
        cache$crops$fixed_outliers,
        by = c("geographicAreaM49", "measuredItemCPC"),
        all.x = TRUE
      )

    if (anyNA(tmp$status) == TRUE) {
      showModal(
        modalDialog(
          title = "Important message",
          'You need to validate all items. When you do so, no data will be shown in the "Outliers" tab.'
        )
      )
    } else {
      showModal(
        modalDialog(
          title = "Confirm report",
          "Do you want to generate the report now?",
          footer = tagList(
            modalButton("Cancel"),
            downloadButton("generate_report", "OK")
          )
        )
      )
    }
  })

  observeEvent(
    input$maintable_rows_selected,
    {
      values$item <-
        values$mydta[
          input$maintable_rows_selected, paste(measuredItemCPC, description, sep = " - ")
        ]

      values$modif <- NULL

      values$current_data <-
        swsData()[
          timePointYears >= 2016 &
            measuredElement %in% c("area", "production", "yield") &
            measuredItemCPC == strsplit(values$item, split = " - ")[[1]][1]
        ][, -"Protected", with = FALSE]

      values$current_data[,
        flagMethod :=
          factor(flagMethod, levels = unique(values$flags$flagMethod))
      ]

      values$current_data[,
        flagObservationStatus :=
          factor(flagObservationStatus,
                 levels = unique(values$flags$flagObservationStatus))
      ]

      # Keep track of modifications
      values$current_data[, modified := FALSE]

      updateTabsetPanel(session, "maincontainer", selected = "myplots")
    }
  )

  observeEvent(
    input$save_current,
    {
      dat <- values$current_data
      setDT(dat)

      dat <- dat[modified == TRUE]

      dat[, modified := NULL]

      dat[, flagObservationStatus := as.character(flagObservationStatus)]
      dat[, flagMethod := as.character(flagMethod)]
      dat[measuredElement == "production", measuredElement := "5510"]
      dat[measuredElement == "area", measuredElement := "5312"]
      dat[measuredElement == "yield", measuredElement := "5421"]

      metad <- copy(dat)

      metad[, c("Value", "flagObservationStatus", "flagMethod") := NULL]

      metad[,
        `:=`(
          Metadata          = "GENERAL",
          Metadata_Element  = "COMMENT",
          Metadata_Language = "en",
          Metadata_Value    = "Production validation"
        )
      ]

      dataReadProgress <- Progress$new(session, min = 0, max = 100)

      dataReadProgress$set(value = 100, message = "Sending data to SWS")

      on.exit(dataReadProgress$close())

      SaveData(
        domain = "agriculture",
        dataset = "aproduction",
        data = dat,
        metadata = metad,
        waitTimeout = 180000
      )

      # TODO: do a function as this is done also for `not_outlier`
      cache <- readRDS(values$file_cache)
      dat <-
        data.table(
          geographicAreaM49 = strsplit(values$country, split = " - ")[[1]][1],
          measuredItemCPC = strsplit(values$item, split = " - ")[[1]][1],
          status = "action"
        )
      cache$crops$fixed_outliers <- rbind(cache$crops$fixed_outliers, dat)
      cache$crops$fixed_outliers <- unique(cache$crops$fixed_outliers)
      saveRDS(cache, values$file_cache)

      # XXX: what if by mistake is both `action` and `pass`?

      updateTabsetPanel(session, "maincontainer", selected = "myoutliers")
    }
  )

  observeEvent(
    input$clear_row,
    {
      values$current_data <- as.data.frame(hot_to_r(input$hot))
      rowid <- input$hot_select$select$r
      values$current_data$modified[rowid] <- TRUE
      values$current_data$Value[rowid] <- NA_real_
      values$current_data$flagObservationStatus[rowid] <- NA_character_
      values$current_data$flagMethod[rowid] <- NA_character_
    }
  )

  observeEvent(
    input$not_outlier,
    {
      cache <- readRDS(values$file_cache)
      tmp <-
        data.table(
          geographicAreaM49 = strsplit(values$country, split = " - ")[[1]][1],
          measuredItemCPC = strsplit(values$item, split = " - ")[[1]][1],
          status = "pass"
        )
      cache$crops$fixed_outliers <- rbind(cache$crops$fixed_outliers, tmp)
      cache$crops$fixed_outliers <- unique(cache$crops$fixed_outliers)
      saveRDS(cache, values$file_cache)

      updateTabsetPanel(session, "maincontainer", selected = "myoutliers")
    }
  )

  observeEvent(
    input$hot$changes$changes[[1]][[1]],
    {
      rowid <- input$hot$changes$changes[[1]][[1]]
      values$modif <- append(values$modif, rowid)
      values$current_data <- as.data.frame(hot_to_r(input$hot))
      values$current_data$modified[rowid + 1] <- TRUE
    }
  )

  # / OBSERVERS
}

shinyApp(ui = ui, server = server)
